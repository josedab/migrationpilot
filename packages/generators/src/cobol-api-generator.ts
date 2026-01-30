/**
 * COBOL-to-API Generator
 * 
 * Generates REST/GraphQL APIs that wrap legacy COBOL programs,
 * enabling gradual modernization without full code translation.
 */

import type { BusinessRule, DataStructure, Procedure } from '@migrationpilot/core';

export interface APIEndpoint {
  path: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE';
  summary: string;
  description: string;
  operationId: string;
  requestBody?: {
    contentType: string;
    schema: JSONSchema;
  };
  responseSchema: JSONSchema;
  cobolProgram: string;
  cobolProcedure?: string;
  parameterMapping: ParameterMapping[];
  responseMapping: ResponseMapping[];
}

export interface JSONSchema {
  type: 'object' | 'array' | 'string' | 'number' | 'integer' | 'boolean';
  properties?: Record<string, JSONSchema>;
  items?: JSONSchema;
  required?: string[];
  format?: string;
  description?: string;
  example?: unknown;
}

export interface ParameterMapping {
  apiField: string;
  cobolField: string;
  transformation?: string;
}

export interface ResponseMapping {
  cobolField: string;
  apiField: string;
  transformation?: string;
}

export interface GeneratedAPI {
  openApiSpec: object;
  endpoints: APIEndpoint[];
  wrapperCode: {
    language: string;
    files: Array<{ path: string; content: string }>;
  };
  dockerConfig?: string;
  documentation: string;
}

export interface APIGeneratorConfig {
  apiStyle: 'rest' | 'graphql' | 'both';
  baseUrl: string;
  version: string;
  authentication: 'none' | 'api-key' | 'jwt' | 'oauth2';
  includeHealthCheck: boolean;
  includeMetrics: boolean;
  codeLanguage: 'java' | 'python' | 'nodejs' | 'go';
  cobolRuntime: 'gnucobol' | 'microfocus' | 'ibm';
  includeDockerfile: boolean;
}

export class CobolApiGenerator {
  private defaultConfig: APIGeneratorConfig = {
    apiStyle: 'rest',
    baseUrl: '/api/v1',
    version: '1.0.0',
    authentication: 'api-key',
    includeHealthCheck: true,
    includeMetrics: true,
    codeLanguage: 'nodejs',
    cobolRuntime: 'gnucobol',
    includeDockerfile: true,
  };

  /**
   * Generate API from COBOL program analysis
   */
  generate(
    programName: string,
    procedures: Procedure[],
    dataStructures: DataStructure[],
    businessRules: BusinessRule[],
    config: Partial<APIGeneratorConfig> = {}
  ): GeneratedAPI {
    const cfg = { ...this.defaultConfig, ...config };
    
    // Generate endpoints from procedures
    const endpoints = this.generateEndpoints(programName, procedures, dataStructures, businessRules, cfg);
    
    // Generate OpenAPI spec
    const openApiSpec = this.generateOpenAPISpec(endpoints, cfg);
    
    // Generate wrapper code
    const wrapperCode = this.generateWrapperCode(endpoints, cfg);
    
    // Generate Docker config if requested
    const dockerConfig = cfg.includeDockerfile 
      ? this.generateDockerfile(cfg)
      : undefined;
    
    // Generate documentation
    const documentation = this.generateDocumentation(endpoints, cfg);
    
    return {
      openApiSpec,
      endpoints,
      wrapperCode,
      dockerConfig,
      documentation,
    };
  }

  private generateEndpoints(
    programName: string,
    procedures: Procedure[],
    dataStructures: DataStructure[],
    businessRules: BusinessRule[],
    config: APIGeneratorConfig
  ): APIEndpoint[] {
    const endpoints: APIEndpoint[] = [];
    
    // Generate endpoint for each procedure
    for (const proc of procedures) {
      if (proc.type === 'program' || proc.type === 'section' || proc.type === 'paragraph') {
        const endpoint = this.procedureToEndpoint(proc, programName, dataStructures, businessRules, config);
        endpoints.push(endpoint);
      }
    }
    
    // Add health check endpoint
    if (config.includeHealthCheck) {
      endpoints.push({
        path: `${config.baseUrl}/health`,
        method: 'GET',
        summary: 'Health check',
        description: 'Check if the COBOL wrapper service is healthy',
        operationId: 'healthCheck',
        responseSchema: {
          type: 'object',
          properties: {
            status: { type: 'string', description: 'Service status' },
            cobolStatus: { type: 'string', description: 'COBOL runtime status' },
            timestamp: { type: 'string', format: 'date-time' },
          },
        },
        cobolProgram: '',
        parameterMapping: [],
        responseMapping: [],
      });
    }
    
    return endpoints;
  }

  private procedureToEndpoint(
    proc: Procedure,
    programName: string,
    dataStructures: DataStructure[],
    businessRules: BusinessRule[],
    config: APIGeneratorConfig
  ): APIEndpoint {
    // Convert COBOL naming to REST-friendly names
    const endpointName = this.toKebabCase(proc.name);
    const operationId = this.toCamelCase(proc.name);
    
    // Determine HTTP method based on procedure characteristics
    const method = this.determineHttpMethod(proc, businessRules);
    
    // Generate request schema from parameters
    const requestSchema = this.generateRequestSchema(proc, dataStructures);
    
    // Generate response schema from return values
    const responseSchema = this.generateResponseSchema(proc, dataStructures);
    
    // Create parameter mappings
    const parameterMapping = proc.parameters.map(p => ({
      apiField: this.toCamelCase(p.name),
      cobolField: p.name,
      transformation: this.getTransformation(p.type),
    }));
    
    // Create response mappings
    const responseMapping = proc.localVariables
      .filter(v => v.name.includes('OUT') || v.name.includes('RESULT'))
      .map(v => ({
        cobolField: v.name,
        apiField: this.toCamelCase(v.name),
        transformation: this.getTransformation(v.type),
      }));
    
    return {
      path: `${config.baseUrl}/${endpointName}`,
      method,
      summary: `Execute ${proc.name}`,
      description: this.generateDescription(proc, businessRules),
      operationId,
      requestBody: requestSchema ? {
        contentType: 'application/json',
        schema: requestSchema,
      } : undefined,
      responseSchema,
      cobolProgram: programName,
      cobolProcedure: proc.name,
      parameterMapping,
      responseMapping,
    };
  }

  private generateOpenAPISpec(endpoints: APIEndpoint[], config: APIGeneratorConfig): object {
    const paths: Record<string, Record<string, object>> = {};
    
    for (const endpoint of endpoints) {
      if (!paths[endpoint.path]) {
        paths[endpoint.path] = {};
      }
      
      const operation: Record<string, unknown> = {
        summary: endpoint.summary,
        description: endpoint.description,
        operationId: endpoint.operationId,
        responses: {
          '200': {
            description: 'Successful response',
            content: {
              'application/json': {
                schema: endpoint.responseSchema,
              },
            },
          },
          '400': {
            description: 'Bad request',
          },
          '500': {
            description: 'Internal server error',
          },
        },
      };
      
      if (endpoint.requestBody) {
        operation.requestBody = {
          required: true,
          content: {
            [endpoint.requestBody.contentType]: {
              schema: endpoint.requestBody.schema,
            },
          },
        };
      }
      
      paths[endpoint.path]![endpoint.method.toLowerCase()] = operation;
    }
    
    return {
      openapi: '3.0.3',
      info: {
        title: 'COBOL Legacy API',
        description: 'API wrapper for legacy COBOL programs',
        version: config.version,
      },
      servers: [
        { url: config.baseUrl },
      ],
      paths,
      components: {
        securitySchemes: this.getSecuritySchemes(config),
      },
    };
  }

  private generateWrapperCode(
    endpoints: APIEndpoint[],
    config: APIGeneratorConfig
  ): { language: string; files: Array<{ path: string; content: string }> } {
    switch (config.codeLanguage) {
      case 'nodejs':
        return this.generateNodeJSWrapper(endpoints, config);
      case 'python':
        return this.generatePythonWrapper(endpoints, config);
      case 'java':
        return this.generateJavaWrapper(endpoints, config);
      default:
        return this.generateNodeJSWrapper(endpoints, config);
    }
  }

  private generateNodeJSWrapper(
    endpoints: APIEndpoint[],
    config: APIGeneratorConfig
  ): { language: string; files: Array<{ path: string; content: string }> } {
    const files: Array<{ path: string; content: string }> = [];
    
    // Main server file
    const serverCode = `/**
 * COBOL API Wrapper Server
 * Auto-generated by MigrationPilot
 */

import express from 'express';
import { execSync } from 'child_process';
import cors from 'cors';

const app = express();
app.use(express.json());
app.use(cors());

// COBOL execution helper
function executeCobol(program, procedure, inputs) {
  try {
    // Convert inputs to COBOL format
    const inputFile = '/tmp/cobol_input_' + Date.now() + '.dat';
    const outputFile = '/tmp/cobol_output_' + Date.now() + '.dat';
    
    // Write input data
    require('fs').writeFileSync(inputFile, formatCobolInput(inputs));
    
    // Execute COBOL program
    const cmd = \`cobc -x -o /tmp/\${program} /opt/cobol/\${program}.cbl && /tmp/\${program}\`;
    execSync(cmd, {
      env: { ...process.env, COBOL_INPUT: inputFile, COBOL_OUTPUT: outputFile },
      timeout: 30000,
    });
    
    // Read output
    const output = require('fs').readFileSync(outputFile, 'utf8');
    return parseCobolOutput(output);
  } catch (error) {
    console.error('COBOL execution error:', error);
    throw error;
  }
}

function formatCobolInput(inputs) {
  // Convert JSON to fixed-width COBOL format
  return Object.entries(inputs)
    .map(([key, value]) => String(value).padEnd(80))
    .join('');
}

function parseCobolOutput(output) {
  // Parse fixed-width COBOL output to JSON
  // This is a simplified implementation
  return { result: output.trim() };
}

${endpoints.filter(e => e.cobolProgram).map(e => `
// ${e.summary}
app.${e.method.toLowerCase()}('${e.path}', async (req, res) => {
  try {
    const inputs = ${e.method === 'GET' ? 'req.query' : 'req.body'};
    const result = executeCobol('${e.cobolProgram}', '${e.cobolProcedure || ''}', inputs);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});
`).join('\n')}

// Health check
app.get('${config.baseUrl}/health', (req, res) => {
  res.json({
    status: 'healthy',
    cobolStatus: 'available',
    timestamp: new Date().toISOString(),
  });
});

const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(\`COBOL API Wrapper listening on port \${port}\`);
});
`;

    files.push({ path: 'src/server.js', content: serverCode });
    
    // Package.json
    const packageJson = `{
  "name": "cobol-api-wrapper",
  "version": "${config.version}",
  "description": "API wrapper for legacy COBOL programs",
  "main": "src/server.js",
  "scripts": {
    "start": "node src/server.js",
    "dev": "nodemon src/server.js"
  },
  "dependencies": {
    "express": "^4.18.2",
    "cors": "^2.8.5"
  },
  "devDependencies": {
    "nodemon": "^3.0.0"
  }
}`;
    files.push({ path: 'package.json', content: packageJson });
    
    return { language: 'nodejs', files };
  }

  private generatePythonWrapper(
    endpoints: APIEndpoint[],
    config: APIGeneratorConfig
  ): { language: string; files: Array<{ path: string; content: string }> } {
    const files: Array<{ path: string; content: string }> = [];
    
    const serverCode = `"""
COBOL API Wrapper Server
Auto-generated by MigrationPilot
"""

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import subprocess
import tempfile
import os

app = FastAPI(
    title="COBOL Legacy API",
    version="${config.version}"
)

def execute_cobol(program: str, procedure: str, inputs: dict) -> dict:
    """Execute a COBOL program and return results."""
    try:
        # Create temp files for I/O
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            input_file = f.name
            f.write(format_cobol_input(inputs))
        
        output_file = tempfile.mktemp(suffix='.dat')
        
        # Execute COBOL
        env = os.environ.copy()
        env['COBOL_INPUT'] = input_file
        env['COBOL_OUTPUT'] = output_file
        
        result = subprocess.run(
            ['cobc', '-x', '-o', f'/tmp/{program}', f'/opt/cobol/{program}.cbl'],
            env=env,
            capture_output=True,
            timeout=30
        )
        
        if result.returncode != 0:
            raise Exception(f"COBOL compilation failed: {result.stderr.decode()}")
        
        subprocess.run([f'/tmp/{program}'], env=env, timeout=30)
        
        # Read output
        with open(output_file, 'r') as f:
            output = f.read()
        
        return parse_cobol_output(output)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

def format_cobol_input(inputs: dict) -> str:
    return ''.join(str(v).ljust(80) for v in inputs.values())

def parse_cobol_output(output: str) -> dict:
    return {"result": output.strip()}

${endpoints.filter(e => e.cobolProgram).map(e => `
@app.${e.method.toLowerCase()}("${e.path}")
async def ${e.operationId}(${e.method === 'GET' ? '' : 'data: dict'}):
    """${e.summary}"""
    inputs = ${e.method === 'GET' ? '{}' : 'data'}
    return execute_cobol("${e.cobolProgram}", "${e.cobolProcedure || ''}", inputs)
`).join('\n')}

@app.get("${config.baseUrl}/health")
async def health_check():
    return {
        "status": "healthy",
        "cobol_status": "available",
        "timestamp": __import__('datetime').datetime.utcnow().isoformat()
    }
`;

    files.push({ path: 'main.py', content: serverCode });
    
    const requirements = `fastapi==0.104.0
uvicorn==0.24.0
pydantic==2.5.0`;
    files.push({ path: 'requirements.txt', content: requirements });
    
    return { language: 'python', files };
  }

  private generateJavaWrapper(
    endpoints: APIEndpoint[],
    config: APIGeneratorConfig
  ): { language: string; files: Array<{ path: string; content: string }> } {
    // Simplified Java wrapper
    const files: Array<{ path: string; content: string }> = [];
    
    const controllerCode = `package com.migrationpilot.cobolapi;

import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import java.util.Map;

@RestController
@RequestMapping("${config.baseUrl}")
public class CobolApiController {

    private final CobolExecutor cobolExecutor;

    public CobolApiController(CobolExecutor cobolExecutor) {
        this.cobolExecutor = cobolExecutor;
    }

${endpoints.filter(e => e.cobolProgram).map(e => `
    @${e.method === 'GET' ? 'GetMapping' : 'PostMapping'}("/${this.toKebabCase(e.operationId)}")
    public ResponseEntity<Map<String, Object>> ${e.operationId}(${e.method === 'GET' ? '' : '@RequestBody Map<String, Object> request'}) {
        try {
            Map<String, Object> result = cobolExecutor.execute(
                "${e.cobolProgram}",
                "${e.cobolProcedure || ''}",
                ${e.method === 'GET' ? 'Map.of()' : 'request'}
            );
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(Map.of("error", e.getMessage()));
        }
    }
`).join('\n')}

    @GetMapping("/health")
    public ResponseEntity<Map<String, Object>> healthCheck() {
        return ResponseEntity.ok(Map.of(
            "status", "healthy",
            "cobolStatus", "available",
            "timestamp", java.time.Instant.now().toString()
        ));
    }
}`;

    files.push({ path: 'src/main/java/com/migrationpilot/cobolapi/CobolApiController.java', content: controllerCode });
    
    return { language: 'java', files };
  }

  private generateDockerfile(config: APIGeneratorConfig): string {
    const runtimeImage = config.cobolRuntime === 'gnucobol' 
      ? 'gnucobol/gnucobol:latest'
      : 'cobol-runtime:latest';

    if (config.codeLanguage === 'nodejs') {
      return `FROM node:20-alpine AS builder
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production

FROM ${runtimeImage}
WORKDIR /app
COPY --from=builder /app/node_modules ./node_modules
COPY src/ ./src/
COPY cobol/ /opt/cobol/

ENV PORT=3000
EXPOSE 3000

CMD ["node", "src/server.js"]
`;
    }

    if (config.codeLanguage === 'python') {
      return `FROM ${runtimeImage}
WORKDIR /app

RUN apt-get update && apt-get install -y python3 python3-pip

COPY requirements.txt .
RUN pip3 install --no-cache-dir -r requirements.txt

COPY main.py .
COPY cobol/ /opt/cobol/

ENV PORT=8000
EXPOSE 8000

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
`;
    }

    return `# Dockerfile for ${config.codeLanguage}
# TODO: Implement for ${config.codeLanguage}
`;
  }

  private generateDocumentation(endpoints: APIEndpoint[], config: APIGeneratorConfig): string {
    return `# COBOL API Wrapper Documentation

## Overview
This API provides a modern REST interface to legacy COBOL programs.

## Base URL
\`${config.baseUrl}\`

## Authentication
${config.authentication === 'api-key' ? 'API Key authentication via X-API-Key header' : config.authentication}

## Endpoints

${endpoints.map(e => `### ${e.method} ${e.path}

**Summary:** ${e.summary}

${e.description}

**COBOL Program:** ${e.cobolProgram || 'N/A'}
**COBOL Procedure:** ${e.cobolProcedure || 'N/A'}

${e.requestBody ? `**Request Body:**
\`\`\`json
${JSON.stringify(e.requestBody.schema, null, 2)}
\`\`\`` : ''}

**Response:**
\`\`\`json
${JSON.stringify(e.responseSchema, null, 2)}
\`\`\`

---
`).join('\n')}

## Error Handling
All endpoints return standard HTTP status codes:
- 200: Success
- 400: Bad Request
- 500: Internal Server Error (COBOL execution failure)

## Rate Limits
Default: 100 requests per minute per API key.
`;
  }

  // Helper methods

  private determineHttpMethod(proc: Procedure, _businessRules: BusinessRule[]): 'GET' | 'POST' | 'PUT' | 'DELETE' {
    const name = proc.name.toUpperCase();
    
    if (name.includes('READ') || name.includes('GET') || name.includes('DISPLAY') || name.includes('INQUIRY')) {
      return 'GET';
    }
    if (name.includes('UPDATE') || name.includes('MODIFY') || name.includes('CHANGE')) {
      return 'PUT';
    }
    if (name.includes('DELETE') || name.includes('REMOVE')) {
      return 'DELETE';
    }
    
    return 'POST';
  }

  private generateRequestSchema(proc: Procedure, _dataStructures: DataStructure[]): JSONSchema | null {
    if (proc.parameters.length === 0) return null;
    
    const properties: Record<string, JSONSchema> = {};
    const required: string[] = [];
    
    for (const param of proc.parameters) {
      const fieldName = this.toCamelCase(param.name);
      properties[fieldName] = {
        type: this.mapDataType(param.type),
        description: `COBOL field: ${param.name}`,
      };
      if (param.direction !== 'out') {
        required.push(fieldName);
      }
    }
    
    return {
      type: 'object',
      properties,
      required: required.length > 0 ? required : undefined,
    };
  }

  private generateResponseSchema(proc: Procedure, _dataStructures: DataStructure[]): JSONSchema {
    const properties: Record<string, JSONSchema> = {};
    
    // Look for output parameters
    for (const param of proc.parameters) {
      if (param.direction === 'out' || param.direction === 'inout') {
        properties[this.toCamelCase(param.name)] = {
          type: this.mapDataType(param.type),
        };
      }
    }
    
    // Add return status
    properties.status = { type: 'string' };
    properties.message = { type: 'string' };
    
    return {
      type: 'object',
      properties,
    };
  }

  private generateDescription(proc: Procedure, businessRules: BusinessRule[]): string {
    // Find related business rules
    const relatedRules = businessRules.filter(r => 
      r.sourceLines[0] >= (proc.location.startLine || 0) &&
      r.sourceLines[1] <= (proc.location.endLine || 0)
    );
    
    let desc = `Executes the ${proc.name} procedure from the COBOL program.`;
    
    if (relatedRules.length > 0) {
      desc += `\n\nImplements the following business rules:\n`;
      desc += relatedRules.map(r => `- ${r.name}: ${r.description}`).join('\n');
    }
    
    return desc;
  }

  private mapDataType(type: string): 'string' | 'number' | 'integer' | 'boolean' {
    switch (type) {
      case 'integer':
        return 'integer';
      case 'decimal':
      case 'number':
        return 'number';
      case 'boolean':
        return 'boolean';
      default:
        return 'string';
    }
  }

  private getTransformation(type: string): string | undefined {
    switch (type) {
      case 'decimal':
        return 'parseFloat';
      case 'integer':
        return 'parseInt';
      case 'date':
        return 'parseDate';
      default:
        return undefined;
    }
  }

  private getSecuritySchemes(config: APIGeneratorConfig): object {
    switch (config.authentication) {
      case 'api-key':
        return {
          apiKey: {
            type: 'apiKey',
            in: 'header',
            name: 'X-API-Key',
          },
        };
      case 'jwt':
        return {
          bearerAuth: {
            type: 'http',
            scheme: 'bearer',
            bearerFormat: 'JWT',
          },
        };
      default:
        return {};
    }
  }

  private toKebabCase(str: string): string {
    return str
      .replace(/([a-z])([A-Z])/g, '$1-$2')
      .replace(/[_\s]+/g, '-')
      .toLowerCase();
  }

  private toCamelCase(str: string): string {
    return str
      .toLowerCase()
      .replace(/[_-]+(.)/g, (_, c) => c.toUpperCase());
  }
}

/**
 * Architect Agent Tools
 */

import type { AgentTool } from '@migrationpilot/core';

/**
 * Design service boundaries
 */
export const designServicesTool: AgentTool = {
  name: 'design_services',
  description: 'Design service boundaries based on business capabilities and domain-driven design principles.',
  parameters: {
    business_rules: {
      type: 'array',
      description: 'List of business rules to organize into services',
      required: true,
    },
    pattern: {
      type: 'string',
      description: 'Architecture pattern to apply',
      required: true,
      enum: ['microservices', 'modular-monolith', 'layered', 'hexagonal'],
    },
  },
  execute: async (args) => {
    const businessRules = args.business_rules as string[];
    const pattern = args.pattern as string;
    
    // Group rules by domain
    const domains = groupByDomain(businessRules);
    
    return {
      pattern,
      domains,
      recommendedServices: Object.keys(domains).map(domain => ({
        name: `${domain}Service`,
        rules: domains[domain],
      })),
    };
  },
};

/**
 * Map data structures to modern models
 */
export const mapDataModelsTool: AgentTool = {
  name: 'map_data_models',
  description: 'Transform legacy data structures into modern data models with proper typing.',
  parameters: {
    structures: {
      type: 'array',
      description: 'Legacy data structures to transform',
      required: true,
    },
    target_language: {
      type: 'string',
      description: 'Target programming language',
      required: true,
    },
  },
  execute: async (args) => {
    const structures = args.structures as Array<{ name: string; type: string; picture?: string }>;
    const targetLanguage = args.target_language as string;
    
    const models = structures.map(s => ({
      originalName: s.name,
      modernName: toModernName(s.name),
      originalType: s.type,
      modernType: mapType(s.type, s.picture, targetLanguage),
    }));
    
    return { models };
  },
};

/**
 * Generate API specification
 */
export const generateApiSpecTool: AgentTool = {
  name: 'generate_api_spec',
  description: 'Generate OpenAPI specification for a service.',
  parameters: {
    service_name: {
      type: 'string',
      description: 'Name of the service',
      required: true,
    },
    operations: {
      type: 'array',
      description: 'List of operations to expose as API endpoints',
      required: true,
    },
  },
  execute: async (args) => {
    const serviceName = args.service_name as string;
    const operations = args.operations as Array<{ name: string; type: string }>;
    
    const spec = {
      openapi: '3.0.0',
      info: {
        title: `${serviceName} API`,
        version: '1.0.0',
      },
      paths: {} as Record<string, unknown>,
    };
    
    for (const op of operations) {
      const path = `/${toKebabCase(serviceName)}/${toKebabCase(op.name)}`;
      spec.paths[path] = {
        [op.type === 'query' ? 'get' : 'post']: {
          operationId: op.name,
          summary: `Execute ${op.name}`,
          responses: {
            '200': { description: 'Success' },
          },
        },
      };
    }
    
    return spec;
  },
};

/**
 * Design database schema
 */
export const designSchemaTool: AgentTool = {
  name: 'design_schema',
  description: 'Design relational database schema from data models.',
  parameters: {
    models: {
      type: 'array',
      description: 'Data models to create tables for',
      required: true,
    },
    database_type: {
      type: 'string',
      description: 'Target database type',
      required: true,
      enum: ['postgresql', 'mysql', 'sqlserver'],
    },
  },
  execute: async (args) => {
    const models = args.models as Array<{ name: string; fields: Array<{ name: string; type: string }> }>;
    const dbType = args.database_type as string;
    
    const tables = models.map(model => ({
      name: toSnakeCase(model.name),
      columns: [
        { name: 'id', type: 'UUID', nullable: false, primaryKey: true },
        ...model.fields.map(f => ({
          name: toSnakeCase(f.name),
          type: mapToDbType(f.type, dbType),
          nullable: true,
        })),
        { name: 'created_at', type: 'TIMESTAMP', nullable: false },
        { name: 'updated_at', type: 'TIMESTAMP', nullable: false },
      ],
    }));
    
    return { tables };
  },
};

/**
 * Plan migration phases
 */
export const planMigrationTool: AgentTool = {
  name: 'plan_migration',
  description: 'Create a phased migration plan with dependencies and validation criteria.',
  parameters: {
    services: {
      type: 'array',
      description: 'Services to migrate',
      required: true,
    },
    approach: {
      type: 'string',
      description: 'Migration approach',
      required: true,
      enum: ['big-bang', 'strangler-fig', 'parallel-run'],
    },
  },
  execute: async (args) => {
    const services = args.services as Array<{ name: string; dependencies: string[] }>;
    const approach = args.approach as string;
    
    // Topological sort for dependency ordering
    const phases = topologicalSort(services);
    
    return {
      approach,
      phases: phases.map((phase, i) => ({
        phase: i + 1,
        services: phase,
        description: `Migrate ${phase.join(', ')}`,
      })),
    };
  },
};

export const architectTools: AgentTool[] = [
  designServicesTool,
  mapDataModelsTool,
  generateApiSpecTool,
  designSchemaTool,
  planMigrationTool,
];

// Helper functions

function groupByDomain(rules: string[]): Record<string, string[]> {
  const domains: Record<string, string[]> = {};
  
  for (const rule of rules) {
    // Simple heuristic: use first word as domain
    const domain = rule.split(/[_\s-]/)[0]?.toLowerCase() || 'core';
    if (!domains[domain]) {
      domains[domain] = [];
    }
    domains[domain].push(rule);
  }
  
  return domains;
}

function toModernName(name: string): string {
  return name
    .replace(/^WS-|^LS-|^WK-/, '')
    .split(/[-_]/)
    .map((part, i) => i === 0 ? part.toLowerCase() : part.charAt(0).toUpperCase() + part.slice(1).toLowerCase())
    .join('');
}

function toKebabCase(str: string): string {
  return str
    .replace(/([a-z])([A-Z])/g, '$1-$2')
    .replace(/[\s_]+/g, '-')
    .toLowerCase();
}

function toSnakeCase(str: string): string {
  return str
    .replace(/([a-z])([A-Z])/g, '$1_$2')
    .replace(/[\s-]+/g, '_')
    .toLowerCase();
}

function mapType(type: string, _picture: string | undefined, targetLanguage: string): string {
  const typeMap: Record<string, Record<string, string>> = {
    java: {
      string: 'String',
      integer: 'Integer',
      decimal: 'BigDecimal',
      date: 'LocalDate',
      boolean: 'Boolean',
    },
    python: {
      string: 'str',
      integer: 'int',
      decimal: 'Decimal',
      date: 'date',
      boolean: 'bool',
    },
    typescript: {
      string: 'string',
      integer: 'number',
      decimal: 'number',
      date: 'Date',
      boolean: 'boolean',
    },
  };
  
  return typeMap[targetLanguage]?.[type] || 'any';
}

function mapToDbType(type: string, dbType: string): string {
  const typeMap: Record<string, Record<string, string>> = {
    postgresql: {
      string: 'VARCHAR(255)',
      integer: 'INTEGER',
      decimal: 'DECIMAL(18,2)',
      date: 'DATE',
      boolean: 'BOOLEAN',
    },
    mysql: {
      string: 'VARCHAR(255)',
      integer: 'INT',
      decimal: 'DECIMAL(18,2)',
      date: 'DATE',
      boolean: 'TINYINT(1)',
    },
  };
  
  return typeMap[dbType]?.[type] || 'TEXT';
}

function topologicalSort(services: Array<{ name: string; dependencies: string[] }>): string[][] {
  const phases: string[][] = [];
  const remaining = new Set(services.map(s => s.name));
  const completed = new Set<string>();
  
  while (remaining.size > 0) {
    const phase: string[] = [];
    
    for (const service of services) {
      if (!remaining.has(service.name)) continue;
      
      const depsComplete = service.dependencies.every(d => completed.has(d) || !remaining.has(d));
      if (depsComplete) {
        phase.push(service.name);
      }
    }
    
    if (phase.length === 0) {
      // Circular dependency, just take remaining
      phase.push(...remaining);
    }
    
    for (const name of phase) {
      remaining.delete(name);
      completed.add(name);
    }
    
    phases.push(phase);
  }
  
  return phases;
}

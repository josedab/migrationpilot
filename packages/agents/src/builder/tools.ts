/**
 * Builder Agent Tools
 */

import type { AgentTool } from '@migrationpilot/core';

/**
 * Generate source code file
 */
export const generateCodeTool: AgentTool = {
  name: 'generate_code',
  description: 'Generate source code in the target language following best practices.',
  parameters: {
    language: {
      type: 'string',
      description: 'Target programming language',
      required: true,
      enum: ['java', 'python', 'typescript', 'go', 'csharp'],
    },
    file_type: {
      type: 'string',
      description: 'Type of file to generate',
      required: true,
      enum: ['class', 'interface', 'function', 'module', 'test'],
    },
    name: {
      type: 'string',
      description: 'Name of the class/function/module',
      required: true,
    },
    specification: {
      type: 'object',
      description: 'Specification including methods, fields, etc.',
      required: true,
    },
  },
  execute: async (args) => {
    const language = args.language as string;
    const fileType = args.file_type as string;
    const name = args.name as string;
    const spec = args.specification as Record<string, unknown>;
    
    // Generate code based on language templates
    const code = generateCodeFromSpec(language, fileType, name, spec);
    
    return {
      filename: getFilename(language, name, fileType),
      content: code,
      language,
    };
  },
};

/**
 * Generate unit tests
 */
export const generateTestsTool: AgentTool = {
  name: 'generate_tests',
  description: 'Generate unit tests for a given source code.',
  parameters: {
    language: {
      type: 'string',
      description: 'Programming language',
      required: true,
    },
    source_code: {
      type: 'string',
      description: 'Source code to generate tests for',
      required: true,
    },
    test_framework: {
      type: 'string',
      description: 'Test framework to use',
      required: false,
    },
  },
  execute: async (args) => {
    const language = args.language as string;
    const sourceCode = args.source_code as string;
    const framework = args.test_framework as string || getDefaultTestFramework(language);
    
    // Extract testable elements from source
    const testCases = extractTestCases(sourceCode, language);
    
    return {
      testFramework: framework,
      testCases,
    };
  },
};

/**
 * Generate documentation
 */
export const generateDocsTool: AgentTool = {
  name: 'generate_documentation',
  description: 'Generate documentation for code including API docs and comments.',
  parameters: {
    code: {
      type: 'string',
      description: 'Source code to document',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Documentation format',
      required: true,
      enum: ['markdown', 'jsdoc', 'javadoc', 'docstring', 'openapi'],
    },
  },
  execute: async (args) => {
    const code = args.code as string;
    const format = args.format as string;
    
    return {
      format,
      documentation: generateDocumentation(code, format),
    };
  },
};

/**
 * Format and lint code
 */
export const formatCodeTool: AgentTool = {
  name: 'format_code',
  description: 'Format code according to language conventions and style guides.',
  parameters: {
    code: {
      type: 'string',
      description: 'Code to format',
      required: true,
    },
    language: {
      type: 'string',
      description: 'Programming language',
      required: true,
    },
  },
  execute: async (args) => {
    const code = args.code as string;
    const language = args.language as string;
    
    // In real implementation, would use actual formatters
    return {
      formattedCode: code.trim(),
      language,
    };
  },
};

/**
 * Generate database migration
 */
export const generateMigrationTool: AgentTool = {
  name: 'generate_migration',
  description: 'Generate database migration scripts.',
  parameters: {
    operation: {
      type: 'string',
      description: 'Type of migration operation',
      required: true,
      enum: ['create_table', 'alter_table', 'create_index', 'drop_table'],
    },
    table_name: {
      type: 'string',
      description: 'Name of the table',
      required: true,
    },
    columns: {
      type: 'array',
      description: 'Column definitions',
      required: false,
    },
    database: {
      type: 'string',
      description: 'Target database type',
      required: true,
      enum: ['postgresql', 'mysql', 'sqlserver'],
    },
  },
  execute: async (args) => {
    const operation = args.operation as string;
    const tableName = args.table_name as string;
    const columns = args.columns as Array<{ name: string; type: string }> | undefined;
    const database = args.database as string;
    
    const migration = generateMigrationSQL(operation, tableName, columns || [], database);
    
    return {
      up: migration.up,
      down: migration.down,
      filename: `${Date.now()}_${operation}_${tableName}.sql`,
    };
  },
};

export const builderTools: AgentTool[] = [
  generateCodeTool,
  generateTestsTool,
  generateDocsTool,
  formatCodeTool,
  generateMigrationTool,
];

// Helper functions

function generateCodeFromSpec(
  language: string,
  fileType: string,
  name: string,
  spec: Record<string, unknown>
): string {
  const templates: Record<string, Record<string, (n: string, s: Record<string, unknown>) => string>> = {
    java: {
      class: (n, s) => `
package com.migrationpilot.generated;

/**
 * ${s.description || 'Generated class'}
 */
public class ${n} {
    // TODO: Implement based on spec
}
`.trim(),
      interface: (n, _s) => `
package com.migrationpilot.generated;

public interface ${n} {
    // TODO: Define methods
}
`.trim(),
    },
    python: {
      class: (n, s) => `
"""${s.description || 'Generated class'}"""

class ${n}:
    """${s.description || ''}"""
    
    def __init__(self):
        pass
`.trim(),
      function: (n, s) => `
def ${toSnakeCase(n)}():
    """${s.description || ''}"""
    pass
`.trim(),
    },
    typescript: {
      class: (n, s) => `
/**
 * ${s.description || 'Generated class'}
 */
export class ${n} {
  constructor() {}
}
`.trim(),
      interface: (n, s) => `
/**
 * ${s.description || 'Generated interface'}
 */
export interface ${n} {
  // TODO: Define properties
}
`.trim(),
    },
  };
  
  const languageTemplates = templates[language] || templates.typescript!;
  const template = languageTemplates[fileType] || languageTemplates.class!;
  
  return template(name, spec);
}

function getFilename(language: string, name: string, fileType: string): string {
  const extensions: Record<string, string> = {
    java: '.java',
    python: '.py',
    typescript: '.ts',
    go: '.go',
    csharp: '.cs',
  };
  
  const ext = extensions[language] || '.txt';
  const suffix = fileType === 'test' ? '.test' : '';
  
  if (language === 'python') {
    return `${toSnakeCase(name)}${suffix}${ext}`;
  }
  
  return `${name}${suffix}${ext}`;
}

function getDefaultTestFramework(language: string): string {
  const frameworks: Record<string, string> = {
    java: 'junit5',
    python: 'pytest',
    typescript: 'jest',
    go: 'testing',
    csharp: 'xunit',
  };
  
  return frameworks[language] || 'generic';
}

function extractTestCases(sourceCode: string, language: string): Array<{ name: string; type: string }> {
  const testCases: Array<{ name: string; type: string }> = [];
  
  // Extract function/method names for test generation
  const patterns: Record<string, RegExp> = {
    java: /(?:public|private|protected)\s+\w+\s+(\w+)\s*\(/g,
    python: /def\s+(\w+)\s*\(/g,
    typescript: /(?:async\s+)?(?:function\s+)?(\w+)\s*\(/g,
  };
  
  const pattern = patterns[language] || patterns.typescript!;
  let match;
  
  while ((match = pattern.exec(sourceCode)) !== null) {
    if (!match[1]!.startsWith('_') && match[1] !== 'constructor') {
      testCases.push({
        name: match[1]!,
        type: 'unit',
      });
    }
  }
  
  return testCases;
}

function generateDocumentation(code: string, format: string): string {
  switch (format) {
    case 'markdown':
      return `# API Documentation\n\n\`\`\`\n${code}\n\`\`\``;
    case 'jsdoc':
      return `/**\n * Generated documentation\n */`;
    case 'javadoc':
      return `/**\n * Generated documentation\n */`;
    case 'docstring':
      return `"""Generated documentation"""`;
    default:
      return code;
  }
}

function generateMigrationSQL(
  operation: string,
  tableName: string,
  columns: Array<{ name: string; type: string }>,
  _database: string
): { up: string; down: string } {
  switch (operation) {
    case 'create_table':
      const columnDefs = columns.map(c => `    ${c.name} ${c.type}`).join(',\n');
      return {
        up: `CREATE TABLE ${tableName} (\n    id UUID PRIMARY KEY,\n${columnDefs},\n    created_at TIMESTAMP NOT NULL DEFAULT NOW(),\n    updated_at TIMESTAMP NOT NULL DEFAULT NOW()\n);`,
        down: `DROP TABLE IF EXISTS ${tableName};`,
      };
    case 'drop_table':
      return {
        up: `DROP TABLE IF EXISTS ${tableName};`,
        down: `-- Manual recreation required`,
      };
    default:
      return { up: '', down: '' };
  }
}

function toSnakeCase(str: string): string {
  return str
    .replace(/([a-z])([A-Z])/g, '$1_$2')
    .replace(/[\s-]+/g, '_')
    .toLowerCase();
}

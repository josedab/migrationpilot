/**
 * Archeologist Agent Tools
 * 
 * Tools for code analysis and business rule extraction
 */

import type { AgentTool } from '@migrationpilot/core';

/**
 * Parse COBOL source code structure
 */
export const parseCobolTool: AgentTool = {
  name: 'parse_cobol',
  description: 'Parse COBOL source code and return its structural components including divisions, sections, paragraphs, and data items.',
  parameters: {
    source_code: {
      type: 'string',
      description: 'The COBOL source code to parse',
      required: true,
    },
    include_copybooks: {
      type: 'boolean',
      description: 'Whether to attempt to resolve COPY statements',
      required: false,
    },
  },
  execute: async (args) => {
    const sourceCode = args.source_code as string;
    
    // Basic COBOL structure detection
    const structure = {
      identificationDivision: extractDivision(sourceCode, 'IDENTIFICATION'),
      environmentDivision: extractDivision(sourceCode, 'ENVIRONMENT'),
      dataDivision: extractDivision(sourceCode, 'DATA'),
      procedureDivision: extractDivision(sourceCode, 'PROCEDURE'),
      copybooks: extractCopybooks(sourceCode),
      paragraphs: extractParagraphs(sourceCode),
    };

    return structure;
  },
};

/**
 * Extract business rules from code patterns
 */
export const extractRulesTool: AgentTool = {
  name: 'extract_rules',
  description: 'Identify and extract business rules from code patterns like calculations, validations, and decisions.',
  parameters: {
    source_code: {
      type: 'string',
      description: 'The source code to analyze',
      required: true,
    },
    language: {
      type: 'string',
      description: 'The programming language (cobol, fortran, vb6, java)',
      required: true,
      enum: ['cobol', 'fortran', 'vb6', 'java'],
    },
    rule_types: {
      type: 'array',
      description: 'Types of rules to extract (calculation, validation, decision, transformation)',
      required: false,
    },
  },
  execute: async (args) => {
    const sourceCode = args.source_code as string;
    const language = args.language as string;
    
    const patterns = {
      calculations: findCalculations(sourceCode, language),
      validations: findValidations(sourceCode, language),
      decisions: findDecisions(sourceCode, language),
      transformations: findTransformations(sourceCode, language),
    };

    return patterns;
  },
};

/**
 * Analyze data flow through the program
 */
export const analyzeDataFlowTool: AgentTool = {
  name: 'analyze_data_flow',
  description: 'Trace how data flows through the program, identifying inputs, transformations, and outputs.',
  parameters: {
    source_code: {
      type: 'string',
      description: 'The source code to analyze',
      required: true,
    },
    variable_name: {
      type: 'string',
      description: 'Optional: specific variable to trace',
      required: false,
    },
  },
  execute: async (args) => {
    const sourceCode = args.source_code as string;
    const variableName = args.variable_name as string | undefined;
    
    // Simplified data flow analysis
    const dataFlow = {
      inputs: findInputs(sourceCode),
      outputs: findOutputs(sourceCode),
      transformations: findDataTransformations(sourceCode),
      variableUsage: variableName ? traceVariable(sourceCode, variableName) : null,
    };

    return dataFlow;
  },
};

/**
 * Identify external dependencies
 */
export const findDependenciesTool: AgentTool = {
  name: 'find_dependencies',
  description: 'Identify external dependencies including file I/O, database access, and external program calls.',
  parameters: {
    source_code: {
      type: 'string',
      description: 'The source code to analyze',
      required: true,
    },
    language: {
      type: 'string',
      description: 'The programming language',
      required: true,
    },
  },
  execute: async (args) => {
    const sourceCode = args.source_code as string;
    const language = args.language as string;
    
    const dependencies = {
      fileOperations: findFileOperations(sourceCode, language),
      databaseCalls: findDatabaseCalls(sourceCode, language),
      externalCalls: findExternalCalls(sourceCode, language),
      copybooks: language === 'cobol' ? extractCopybooks(sourceCode) : [],
    };

    return dependencies;
  },
};

/**
 * Calculate complexity metrics
 */
export const calculateComplexityTool: AgentTool = {
  name: 'calculate_complexity',
  description: 'Calculate code complexity metrics including cyclomatic complexity, nesting depth, and other quality indicators.',
  parameters: {
    source_code: {
      type: 'string',
      description: 'The source code to analyze',
      required: true,
    },
  },
  execute: async (args) => {
    const sourceCode = args.source_code as string;
    const lines = sourceCode.split('\n');
    
    const metrics = {
      linesOfCode: lines.length,
      nonEmptyLines: lines.filter(l => l.trim().length > 0).length,
      commentLines: countCommentLines(sourceCode),
      cyclomaticComplexity: estimateCyclomaticComplexity(sourceCode),
      maxNestingDepth: calculateMaxNesting(sourceCode),
    };

    return metrics;
  },
};

// Export all tools
export const archeologyTools: AgentTool[] = [
  parseCobolTool,
  extractRulesTool,
  analyzeDataFlowTool,
  findDependenciesTool,
  calculateComplexityTool,
];

// ============================================================================
// Helper Functions
// ============================================================================

function extractDivision(source: string, divisionName: string): string | null {
  const regex = new RegExp(
    `${divisionName}\\s+DIVISION[\\s\\S]*?(?=(?:IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\\s+DIVISION|$)`,
    'i'
  );
  const match = source.match(regex);
  return match ? match[0].trim() : null;
}

function extractCopybooks(source: string): string[] {
  const copyRegex = /COPY\s+['"]?(\w+)['"]?/gi;
  const matches: string[] = [];
  let match;
  while ((match = copyRegex.exec(source)) !== null) {
    matches.push(match[1]!);
  }
  return matches;
}

function extractParagraphs(source: string): Array<{ name: string; line: number }> {
  const paragraphs: Array<{ name: string; line: number }> = [];
  const lines = source.split('\n');
  
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    // COBOL paragraph: starts in column 8, ends with period
    const match = line.match(/^\s{7}(\w[\w-]*)\s*\./);
    if (match) {
      paragraphs.push({ name: match[1]!, line: i + 1 });
    }
  }
  
  return paragraphs;
}

function findCalculations(source: string, language: string): Array<{ line: number; code: string }> {
  const results: Array<{ line: number; code: string }> = [];
  const lines = source.split('\n');
  
  const patterns: Record<string, RegExp> = {
    cobol: /COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE/i,
    fortran: /^\s*\w+\s*=\s*[^=]/,
    vb6: /^\s*\w+\s*=\s*[^=]/,
    java: /^\s*\w+\s*=\s*[^=]/,
  };
  
  const pattern = patterns[language] ?? patterns.cobol;
  
  lines.forEach((line, i) => {
    if (pattern?.test(line)) {
      results.push({ line: i + 1, code: line.trim() });
    }
  });
  
  return results;
}

function findValidations(source: string, language: string): Array<{ line: number; code: string }> {
  const results: Array<{ line: number; code: string }> = [];
  const lines = source.split('\n');
  
  const patterns: Record<string, RegExp> = {
    cobol: /IF\s+.*\s+(NOT\s+)?(NUMERIC|ALPHABETIC|POSITIVE|NEGATIVE|ZERO)/i,
    fortran: /IF\s*\(.*(\.LT\.|\.GT\.|\.EQ\.|\.NE\.|\.LE\.|\.GE\.)/i,
    vb6: /If\s+.*\s+(Is|Like|<>|=)/i,
    java: /if\s*\(.*[<>=!]/,
  };
  
  const pattern = patterns[language] ?? patterns.cobol;
  
  lines.forEach((line, i) => {
    if (pattern?.test(line)) {
      results.push({ line: i + 1, code: line.trim() });
    }
  });
  
  return results;
}

function findDecisions(source: string, language: string): Array<{ line: number; code: string }> {
  const results: Array<{ line: number; code: string }> = [];
  const lines = source.split('\n');
  
  const patterns: Record<string, RegExp> = {
    cobol: /IF\s+|EVALUATE\s+|WHEN\s+/i,
    fortran: /IF\s*\(|SELECT\s+CASE/i,
    vb6: /If\s+|Select\s+Case/i,
    java: /if\s*\(|switch\s*\(/,
  };
  
  const pattern = patterns[language] ?? patterns.cobol;
  
  lines.forEach((line, i) => {
    if (pattern?.test(line)) {
      results.push({ line: i + 1, code: line.trim() });
    }
  });
  
  return results;
}

function findTransformations(source: string, language: string): Array<{ line: number; code: string }> {
  const results: Array<{ line: number; code: string }> = [];
  const lines = source.split('\n');
  
  const patterns: Record<string, RegExp> = {
    cobol: /MOVE\s+|STRING\s+|UNSTRING\s+|INSPECT\s+/i,
    fortran: /READ\s*\(|WRITE\s*\(/i,
    vb6: /CStr|CInt|Format|Mid|Left|Right/i,
    java: /toString\(|parseInt\(|format\(/,
  };
  
  const pattern = patterns[language] ?? patterns.cobol;
  
  lines.forEach((line, i) => {
    if (pattern?.test(line)) {
      results.push({ line: i + 1, code: line.trim() });
    }
  });
  
  return results;
}

function findInputs(source: string): string[] {
  const inputs: string[] = [];
  
  // COBOL ACCEPT
  const acceptMatches = source.matchAll(/ACCEPT\s+(\w+)/gi);
  for (const match of acceptMatches) {
    inputs.push(match[1]!);
  }
  
  // COBOL READ
  const readMatches = source.matchAll(/READ\s+(\w+)/gi);
  for (const match of readMatches) {
    inputs.push(match[1]!);
  }
  
  return [...new Set(inputs)];
}

function findOutputs(source: string): string[] {
  const outputs: string[] = [];
  
  // COBOL DISPLAY
  const displayMatches = source.matchAll(/DISPLAY\s+(\w+)/gi);
  for (const match of displayMatches) {
    outputs.push(match[1]!);
  }
  
  // COBOL WRITE
  const writeMatches = source.matchAll(/WRITE\s+(\w+)/gi);
  for (const match of writeMatches) {
    outputs.push(match[1]!);
  }
  
  return [...new Set(outputs)];
}

function findDataTransformations(source: string): Array<{ from: string; to: string; line: number }> {
  const transformations: Array<{ from: string; to: string; line: number }> = [];
  const lines = source.split('\n');
  
  lines.forEach((line, i) => {
    // COBOL MOVE
    const moveMatch = line.match(/MOVE\s+(\S+)\s+TO\s+(\S+)/i);
    if (moveMatch) {
      transformations.push({
        from: moveMatch[1]!,
        to: moveMatch[2]!,
        line: i + 1,
      });
    }
  });
  
  return transformations;
}

function traceVariable(source: string, variableName: string): {
  definitions: number[];
  uses: number[];
  modifications: number[];
} {
  const result = { definitions: [] as number[], uses: [] as number[], modifications: [] as number[] };
  const lines = source.split('\n');
  const varRegex = new RegExp(`\\b${variableName}\\b`, 'gi');
  
  lines.forEach((line, i) => {
    if (varRegex.test(line)) {
      if (/^\s*\d{2}\s+/.test(line)) {
        result.definitions.push(i + 1);
      } else if (/MOVE\s+.*\s+TO\s+/i.test(line) && new RegExp(`TO\\s+${variableName}`, 'i').test(line)) {
        result.modifications.push(i + 1);
      } else {
        result.uses.push(i + 1);
      }
    }
  });
  
  return result;
}

function findFileOperations(source: string, language: string): Array<{ file: string; operation: string; line: number }> {
  const operations: Array<{ file: string; operation: string; line: number }> = [];
  const lines = source.split('\n');
  
  if (language === 'cobol') {
    lines.forEach((line, i) => {
      const openMatch = line.match(/OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+(\w+)/i);
      if (openMatch) {
        operations.push({ file: openMatch[2]!, operation: openMatch[1]!, line: i + 1 });
      }
      
      const readMatch = line.match(/READ\s+(\w+)/i);
      if (readMatch) {
        operations.push({ file: readMatch[1]!, operation: 'READ', line: i + 1 });
      }
      
      const writeMatch = line.match(/WRITE\s+(\w+)/i);
      if (writeMatch) {
        operations.push({ file: writeMatch[1]!, operation: 'WRITE', line: i + 1 });
      }
    });
  }
  
  return operations;
}

function findDatabaseCalls(source: string, language: string): Array<{ type: string; line: number }> {
  const calls: Array<{ type: string; line: number }> = [];
  const lines = source.split('\n');
  
  if (language === 'cobol') {
    lines.forEach((line, i) => {
      if (/EXEC\s+SQL/i.test(line)) {
        const type = line.match(/SELECT|INSERT|UPDATE|DELETE|CALL/i)?.[0] || 'SQL';
        calls.push({ type: type.toUpperCase(), line: i + 1 });
      }
    });
  }
  
  return calls;
}

function findExternalCalls(source: string, language: string): Array<{ name: string; line: number }> {
  const calls: Array<{ name: string; line: number }> = [];
  const lines = source.split('\n');
  
  if (language === 'cobol') {
    lines.forEach((line, i) => {
      const callMatch = line.match(/CALL\s+['"]?(\w+)['"]?/i);
      if (callMatch) {
        calls.push({ name: callMatch[1]!, line: i + 1 });
      }
    });
  }
  
  return calls;
}

function countCommentLines(source: string): number {
  const lines = source.split('\n');
  return lines.filter(line => {
    // COBOL comment (asterisk in column 7)
    if (line.length >= 7 && line[6] === '*') return true;
    // Fortran comment
    if (/^[cC*!]/i.test(line)) return true;
    // VB/Java comments
    if (/^\s*(\'|REM|\/\/|\/\*|\*)/i.test(line)) return true;
    return false;
  }).length;
}

function estimateCyclomaticComplexity(source: string): number {
  let complexity = 1;
  
  // Count decision points
  const decisionPatterns = [
    /\bIF\b/gi,
    /\bELSE\b/gi,
    /\bWHEN\b/gi,
    /\bWHILE\b/gi,
    /\bUNTIL\b/gi,
    /\bFOR\b/gi,
    /\bCASE\b/gi,
    /\bAND\b/gi,
    /\bOR\b/gi,
  ];
  
  for (const pattern of decisionPatterns) {
    const matches = source.match(pattern);
    if (matches) {
      complexity += matches.length;
    }
  }
  
  return complexity;
}

function calculateMaxNesting(source: string): number {
  const lines = source.split('\n');
  let currentDepth = 0;
  let maxDepth = 0;
  
  for (const line of lines) {
    // Increase depth for control structures
    if (/\b(IF|EVALUATE|PERFORM\s+UNTIL|PERFORM\s+VARYING)\b/i.test(line)) {
      currentDepth++;
      maxDepth = Math.max(maxDepth, currentDepth);
    }
    // Decrease depth for end statements
    if (/\b(END-IF|END-EVALUATE|END-PERFORM)\b/i.test(line)) {
      currentDepth = Math.max(0, currentDepth - 1);
    }
  }
  
  return maxDepth;
}

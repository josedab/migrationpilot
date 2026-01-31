/**
 * Relationship Analyzer
 * 
 * AI-powered inference of relationships between legacy tables
 * using naming conventions, data patterns, and code analysis.
 */

import type {
  LegacySchema,
  LegacyTable,
  LegacyColumn,
  InferredRelationship,
  RelationshipEvidence,
} from '../types.js';

export interface RelationshipAnalyzerConfig {
  minConfidenceThreshold: number;
  enableNamingAnalysis: boolean;
  enableDataAnalysis: boolean;
  enableCodeAnalysis: boolean;
  namingPatterns?: NamingPattern[];
}

interface NamingPattern {
  pattern: RegExp;
  extractForeignTable: (match: RegExpMatchArray) => string;
  extractForeignColumn: (match: RegExpMatchArray) => string;
  confidence: number;
}

const DEFAULT_NAMING_PATTERNS: NamingPattern[] = [
  // Pattern: customer_id -> customers.id
  {
    pattern: /^(\w+)_id$/i,
    extractForeignTable: (m) => (m[1] || '') + 's',
    extractForeignColumn: () => 'id',
    confidence: 0.7,
  },
  // Pattern: cust_id -> customer.id (abbreviation)
  {
    pattern: /^(\w{3,4})_id$/i,
    extractForeignTable: (m) => m[1] || '',
    extractForeignColumn: () => 'id',
    confidence: 0.5,
  },
  // Pattern: fk_customer -> customer.id
  {
    pattern: /^fk_(\w+)$/i,
    extractForeignTable: (m) => m[1] || '',
    extractForeignColumn: () => 'id',
    confidence: 0.8,
  },
  // Pattern: customer_code -> customers.code
  {
    pattern: /^(\w+)_(code|key|num|number)$/i,
    extractForeignTable: (m) => (m[1] || '') + 's',
    extractForeignColumn: (m) => m[2] || 'id',
    confidence: 0.6,
  },
];

export class RelationshipAnalyzer {
  private config: RelationshipAnalyzerConfig;
  private namingPatterns: NamingPattern[];

  constructor(config?: Partial<RelationshipAnalyzerConfig>) {
    this.config = {
      minConfidenceThreshold: 0.5,
      enableNamingAnalysis: true,
      enableDataAnalysis: true,
      enableCodeAnalysis: true,
      ...config,
    };
    this.namingPatterns = config?.namingPatterns || DEFAULT_NAMING_PATTERNS;
  }

  /**
   * Analyze schema and infer relationships
   */
  analyzeRelationships(schema: LegacySchema): InferredRelationship[] {
    const relationships: InferredRelationship[] = [];
    const tableMap = new Map(schema.tables.map(t => [t.name.toLowerCase(), t]));

    for (const table of schema.tables) {
      // Analyze each column for potential foreign keys
      for (const column of table.columns) {
        const inferred = this.analyzeColumn(table, column, tableMap);
        if (inferred && inferred.confidence >= this.config.minConfidenceThreshold) {
          relationships.push(inferred);
        }
      }

      // Analyze explicit foreign keys (if any)
      for (const fk of table.foreignKeys || []) {
        relationships.push({
          id: `explicit_${table.name}_${fk.name}`,
          sourceTable: table.name,
          sourceColumns: fk.columns,
          targetTable: fk.referencedTable,
          targetColumns: fk.referencedColumns,
          confidence: 1.0,
          evidence: [{
            type: 'explicit-definition',
            description: `Explicit FK constraint: ${fk.name}`,
            confidence: 1.0,
          }],
          inferenceMethod: 'explicit-fk',
          status: 'confirmed',
        });
      }
    }

    // Deduplicate and merge evidence
    return this.mergeRelationships(relationships);
  }

  private analyzeColumn(
    table: LegacyTable,
    column: LegacyColumn,
    tableMap: Map<string, LegacyTable>
  ): InferredRelationship | null {
    const evidence: RelationshipEvidence[] = [];
    let targetTable: string | null = null;
    let targetColumn: string | null = null;

    // Naming convention analysis
    if (this.config.enableNamingAnalysis) {
      const namingResult = this.analyzeNamingConvention(column, tableMap);
      if (namingResult) {
        evidence.push(namingResult.evidence);
        targetTable = namingResult.targetTable;
        targetColumn = namingResult.targetColumn;
      }
    }

    // Skip if no relationship found
    if (!targetTable || evidence.length === 0) {
      return null;
    }

    // Calculate aggregate confidence
    const confidence = this.calculateConfidence(evidence);

    return {
      id: `inferred_${table.name}_${column.name}_${targetTable}`,
      sourceTable: table.name,
      sourceColumns: [column.name],
      targetTable,
      targetColumns: [targetColumn || 'id'],
      confidence,
      evidence,
      inferenceMethod: 'naming-convention',
      status: 'suggested',
    };
  }

  private analyzeNamingConvention(
    column: LegacyColumn,
    tableMap: Map<string, LegacyTable>
  ): { targetTable: string; targetColumn: string; evidence: RelationshipEvidence } | null {
    const columnName = column.name.toLowerCase();

    for (const pattern of this.namingPatterns) {
      const match = columnName.match(pattern.pattern);
      if (!match) continue;

      const potentialTable = pattern.extractForeignTable(match).toLowerCase();
      const potentialColumn = pattern.extractForeignColumn(match).toLowerCase();

      // Check if target table exists
      const variants = [
        potentialTable,
        potentialTable.replace(/s$/, ''),
        potentialTable + 's',
        this.expandAbbreviation(potentialTable),
      ];

      for (const variant of variants) {
        const targetTable = tableMap.get(variant);
        if (targetTable) {
          // Verify target column exists
          const targetCol = targetTable.columns.find(
            c => c.name.toLowerCase() === potentialColumn ||
                 c.name.toLowerCase() === 'id' ||
                 targetTable.primaryKey?.includes(c.name)
          );

          if (targetCol) {
            // Verify data type compatibility
            const typeCompatible = this.checkTypeCompatibility(column, targetCol);
            const adjustedConfidence = typeCompatible 
              ? pattern.confidence 
              : pattern.confidence * 0.5;

            return {
              targetTable: targetTable.name,
              targetColumn: targetCol.name,
              evidence: {
                type: 'name-match',
                description: `Column "${column.name}" matches pattern for FK to "${targetTable.name}.${targetCol.name}"`,
                confidence: adjustedConfidence,
                details: {
                  pattern: pattern.pattern.toString(),
                  typeCompatible,
                },
              },
            };
          }
        }
      }
    }

    return null;
  }

  private expandAbbreviation(abbrev: string): string {
    const abbreviations: Record<string, string> = {
      'cust': 'customer',
      'cus': 'customer',
      'emp': 'employee',
      'prod': 'product',
      'prd': 'product',
      'ord': 'order',
      'inv': 'invoice',
      'acc': 'account',
      'acct': 'account',
      'trx': 'transaction',
      'txn': 'transaction',
      'dept': 'department',
      'addr': 'address',
      'cat': 'category',
    };
    return abbreviations[abbrev] || abbrev;
  }

  private checkTypeCompatibility(source: LegacyColumn, target: LegacyColumn): boolean {
    const sourceBase = this.getBaseType(source.dataType);
    const targetBase = this.getBaseType(target.dataType);
    
    // Exact match
    if (sourceBase === targetBase) return true;
    
    // Numeric compatibility
    const numericTypes = ['integer', 'bigint', 'smallint', 'decimal', 'numeric'];
    if (numericTypes.includes(sourceBase) && numericTypes.includes(targetBase)) {
      return true;
    }
    
    // String compatibility
    const stringTypes = ['varchar', 'char', 'text', 'string'];
    if (stringTypes.includes(sourceBase) && stringTypes.includes(targetBase)) {
      return true;
    }

    return false;
  }

  private getBaseType(dataType: string): string {
    return dataType.toLowerCase().replace(/\(.*\)/, '').trim();
  }

  private calculateConfidence(evidence: RelationshipEvidence[]): number {
    if (evidence.length === 0) return 0;
    
    // Weighted average with diminishing returns for multiple evidence
    let totalWeight = 0;
    let weightedSum = 0;
    
    for (let i = 0; i < evidence.length; i++) {
      const weight = 1 / (i + 1); // Diminishing weights
      const ev = evidence[i];
      if (ev) {
        weightedSum += ev.confidence * weight;
        totalWeight += weight;
      }
    }
    
    return Math.min(1, weightedSum / totalWeight);
  }

  private mergeRelationships(relationships: InferredRelationship[]): InferredRelationship[] {
    const merged = new Map<string, InferredRelationship>();
    
    for (const rel of relationships) {
      const key = `${rel.sourceTable}_${rel.sourceColumns.join(',')}_${rel.targetTable}_${rel.targetColumns.join(',')}`;
      
      if (merged.has(key)) {
        const existing = merged.get(key)!;
        existing.evidence.push(...rel.evidence);
        existing.confidence = this.calculateConfidence(existing.evidence);
      } else {
        merged.set(key, { ...rel });
      }
    }
    
    return Array.from(merged.values())
      .sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Analyze relationships from application code (COBOL joins, etc.)
   */
  analyzeFromCode(
    _schema: LegacySchema,
    codeAnalysis: CodeJoinAnalysis[]
  ): InferredRelationship[] {
    const relationships: InferredRelationship[] = [];

    for (const join of codeAnalysis) {
      relationships.push({
        id: `code_${join.sourceTable}_${join.targetTable}`,
        sourceTable: join.sourceTable,
        sourceColumns: join.sourceColumns,
        targetTable: join.targetTable,
        targetColumns: join.targetColumns,
        confidence: join.frequency > 10 ? 0.9 : 0.7,
        evidence: [{
          type: 'code-join',
          description: `Found ${join.frequency} join operations in code`,
          confidence: join.frequency > 10 ? 0.9 : 0.7,
          details: {
            locations: join.locations,
            frequency: join.frequency,
          },
        }],
        inferenceMethod: 'code-analysis',
        status: 'suggested',
      });
    }

    return relationships;
  }
}

export interface CodeJoinAnalysis {
  sourceTable: string;
  sourceColumns: string[];
  targetTable: string;
  targetColumns: string[];
  frequency: number;
  locations: string[];
}

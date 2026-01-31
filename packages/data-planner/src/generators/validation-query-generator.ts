/**
 * Validation Query Generator
 * 
 * Generates validation queries to verify data migration accuracy.
 */

import type {
  LegacySchema,
  TargetSchema,
  TableMigrationPlan,
  ValidationQuery,
  InferredRelationship,
  TargetDatabaseType,
} from '../types.js';

export interface ValidationGeneratorConfig {
  targetDatabase: TargetDatabaseType;
  sampleSize: number;
  checksumMethod: 'md5' | 'sha256' | 'crc32' | 'sum';
  includeDataSampling: boolean;
  includeReferentialChecks: boolean;
}

export class ValidationQueryGenerator {
  private config: ValidationGeneratorConfig;

  constructor(config: ValidationGeneratorConfig) {
    this.config = config;
  }

  /**
   * Generate all validation queries for a migration
   */
  generateAll(
    source: LegacySchema,
    target: TargetSchema,
    mappings: TableMigrationPlan[],
    relationships: InferredRelationship[]
  ): {
    preValidation: ValidationQuery[];
    postValidation: ValidationQuery[];
    continuousValidation: ValidationQuery[];
  } {
    const preValidation: ValidationQuery[] = [];
    const postValidation: ValidationQuery[] = [];
    const continuousValidation: ValidationQuery[] = [];

    // Pre-validation: source data quality
    for (const mapping of mappings) {
      preValidation.push(...this.generatePreValidation(mapping, source));
    }

    // Post-validation: data integrity
    for (const mapping of mappings) {
      postValidation.push(...this.generatePostValidation(mapping, source, target));
    }

    // Referential integrity
    if (this.config.includeReferentialChecks) {
      postValidation.push(...this.generateReferentialValidation(relationships, mappings));
    }

    // Continuous validation for ongoing monitoring
    continuousValidation.push(...this.generateContinuousValidation(mappings));

    return { preValidation, postValidation, continuousValidation };
  }

  /**
   * Generate pre-migration validation queries
   */
  private generatePreValidation(
    mapping: TableMigrationPlan,
    source: LegacySchema
  ): ValidationQuery[] {
    const queries: ValidationQuery[] = [];

    // Source row count
    queries.push({
      id: `pre_count_${mapping.sourceTable}`,
      name: `Pre-Migration Row Count - ${mapping.sourceTable}`,
      description: 'Record source row count before migration',
      type: 'row-count',
      sourceQuery: `SELECT COUNT(*) as row_count FROM ${mapping.sourceTable}`,
      targetQuery: '', // No target yet
      tolerance: 0,
      critical: true,
    });

    // Null value analysis
    const sourceTable = source.tables.find(t => t.name === mapping.sourceTable);
    if (sourceTable) {
      const nullCheckCols = sourceTable.columns
        .filter(c => !c.nullable)
        .slice(0, 5) // Limit to first 5 non-nullable columns
        .map(c => c.name);

      if (nullCheckCols.length > 0) {
        queries.push({
          id: `pre_null_${mapping.sourceTable}`,
          name: `Pre-Migration Null Check - ${mapping.sourceTable}`,
          description: 'Verify non-nullable columns have no nulls',
          type: 'custom',
          sourceQuery: `
            SELECT 
              ${nullCheckCols.map(c => `SUM(CASE WHEN ${c} IS NULL THEN 1 ELSE 0 END) as ${c}_nulls`).join(',\n              ')}
            FROM ${mapping.sourceTable}
          `,
          targetQuery: '',
          tolerance: 0,
          critical: true,
        });
      }
    }

    return queries;
  }

  /**
   * Generate post-migration validation queries
   */
  private generatePostValidation(
    mapping: TableMigrationPlan,
    source: LegacySchema,
    _target: TargetSchema
  ): ValidationQuery[] {
    const queries: ValidationQuery[] = [];

    // Row count comparison
    queries.push({
      id: `post_count_${mapping.targetTable}`,
      name: `Row Count Validation - ${mapping.targetTable}`,
      description: 'Verify target has same row count as source',
      type: 'row-count',
      sourceQuery: `SELECT COUNT(*) as row_count FROM ${mapping.sourceTable}`,
      targetQuery: `SELECT COUNT(*) as row_count FROM ${mapping.targetTable}`,
      tolerance: 0,
      critical: true,
    });

    // Checksum validation
    const checksumColumns = this.getChecksumColumns(mapping, source);
    if (checksumColumns.length > 0) {
      queries.push({
        id: `post_checksum_${mapping.targetTable}`,
        name: `Checksum Validation - ${mapping.targetTable}`,
        description: 'Verify data integrity via column checksums',
        type: 'checksum',
        sourceQuery: this.generateChecksumQuery(
          mapping.sourceTable,
          checksumColumns,
          'source'
        ),
        targetQuery: this.generateChecksumQuery(
          mapping.targetTable,
          checksumColumns.map(c => {
            const colMapping = mapping.columnMappings.find(m => m.sourceColumn === c);
            return colMapping?.targetColumn || c;
          }),
          'target'
        ),
        tolerance: 0,
        critical: true,
      });
    }

    // Sample validation
    if (this.config.includeDataSampling) {
      queries.push({
        id: `post_sample_${mapping.targetTable}`,
        name: `Sample Validation - ${mapping.targetTable}`,
        description: `Verify ${this.config.sampleSize} random records match`,
        type: 'sample',
        sourceQuery: this.generateSampleQuery(mapping, 'source'),
        targetQuery: this.generateSampleQuery(mapping, 'target'),
        tolerance: 0,
        critical: false,
      });
    }

    // Min/Max validation for numeric columns
    const numericColumns = this.getNumericColumns(mapping, source);
    if (numericColumns.length > 0) {
      queries.push({
        id: `post_minmax_${mapping.targetTable}`,
        name: `Min/Max Validation - ${mapping.targetTable}`,
        description: 'Verify numeric column ranges match',
        type: 'custom',
        sourceQuery: this.generateMinMaxQuery(mapping.sourceTable, numericColumns),
        targetQuery: this.generateMinMaxQuery(
          mapping.targetTable,
          numericColumns.map(c => {
            const colMapping = mapping.columnMappings.find(m => m.sourceColumn === c);
            return colMapping?.targetColumn || c;
          })
        ),
        tolerance: 0.001, // Allow small rounding differences
        critical: false,
      });
    }

    return queries;
  }

  /**
   * Generate referential integrity validation
   */
  private generateReferentialValidation(
    relationships: InferredRelationship[],
    mappings: TableMigrationPlan[]
  ): ValidationQuery[] {
    const queries: ValidationQuery[] = [];

    for (const rel of relationships.filter(r => r.status !== 'rejected')) {
      const sourceMapping = mappings.find(m => m.sourceTable === rel.sourceTable);
      const targetMapping = mappings.find(m => m.sourceTable === rel.targetTable);

      if (!sourceMapping || !targetMapping) continue;

      const targetSourceCols = rel.sourceColumns.map(c => {
        const colMapping = sourceMapping.columnMappings.find(m => m.sourceColumn === c);
        return colMapping?.targetColumn || c;
      });

      const targetRefCols = rel.targetColumns.map(c => {
        const colMapping = targetMapping.columnMappings.find(m => m.sourceColumn === c);
        return colMapping?.targetColumn || c;
      });

      queries.push({
        id: `ref_${sourceMapping.targetTable}_${targetMapping.targetTable}`,
        name: `Referential Integrity - ${sourceMapping.targetTable} -> ${targetMapping.targetTable}`,
        description: 'Verify no orphan records after migration',
        type: 'referential',
        sourceQuery: this.generateOrphanCheckQuery(
          sourceMapping.targetTable,
          targetSourceCols,
          targetMapping.targetTable,
          targetRefCols
        ),
        targetQuery: 'SELECT 0 as orphan_count', // Expected: 0 orphans
        tolerance: 0,
        critical: rel.confidence > 0.8,
      });
    }

    return queries;
  }

  /**
   * Generate continuous validation queries
   */
  private generateContinuousValidation(mappings: TableMigrationPlan[]): ValidationQuery[] {
    return mappings.map(mapping => ({
      id: `continuous_${mapping.targetTable}`,
      name: `Continuous Monitoring - ${mapping.targetTable}`,
      description: 'Monitor for data drift post-migration',
      type: 'row-count',
      sourceQuery: `SELECT COUNT(*) as row_count FROM ${mapping.sourceTable}`,
      targetQuery: `SELECT COUNT(*) as row_count FROM ${mapping.targetTable}`,
      tolerance: 0.01, // 1% tolerance for ongoing sync
      critical: false,
    }));
  }

  private getChecksumColumns(mapping: TableMigrationPlan, source: LegacySchema): string[] {
    const sourceTable = source.tables.find(t => t.name === mapping.sourceTable);
    if (!sourceTable) return [];

    return sourceTable.columns
      .filter(c => 
        c.dataType.includes('char') || 
        c.dataType.includes('int') ||
        c.dataType.includes('decimal')
      )
      .slice(0, 10) // Limit columns for performance
      .map(c => c.name);
  }

  private getNumericColumns(mapping: TableMigrationPlan, source: LegacySchema): string[] {
    const sourceTable = source.tables.find(t => t.name === mapping.sourceTable);
    if (!sourceTable) return [];

    return sourceTable.columns
      .filter(c => 
        c.dataType.includes('int') ||
        c.dataType.includes('decimal') ||
        c.dataType.includes('numeric') ||
        c.dataType.includes('float')
      )
      .slice(0, 5)
      .map(c => c.name);
  }

  private generateChecksumQuery(
    table: string,
    columns: string[],
    _side: 'source' | 'target'
  ): string {
    const checksumExpr = this.getChecksumExpression(columns);
    return `
      SELECT 
        COUNT(*) as row_count,
        ${checksumExpr} as data_checksum
      FROM ${table}
    `;
  }

  private getChecksumExpression(columns: string[]): string {
    const concatExpr = columns.map(c => `COALESCE(CAST(${c} AS VARCHAR), '')`).join(" || '|' || ");
    
    switch (this.config.checksumMethod) {
      case 'md5':
        return `SUM(CAST(MD5(${concatExpr}) AS BIGINT))`;
      case 'sha256':
        return `SUM(CAST(SHA256(${concatExpr}) AS BIGINT))`;
      case 'crc32':
        return `SUM(CRC32(${concatExpr}))`;
      default:
        return `SUM(LENGTH(${concatExpr}))`;
    }
  }

  private generateSampleQuery(mapping: TableMigrationPlan, side: 'source' | 'target'): string {
    const table = side === 'source' ? mapping.sourceTable : mapping.targetTable;
    const columns = mapping.columnMappings.map(c => 
      side === 'source' ? c.sourceColumn : c.targetColumn
    );

    return `
      SELECT ${columns.join(', ')}
      FROM ${table}
      ORDER BY RANDOM()
      LIMIT ${this.config.sampleSize}
    `;
  }

  private generateMinMaxQuery(table: string, columns: string[]): string {
    const aggs = columns.flatMap(c => [
      `MIN(${c}) as ${c}_min`,
      `MAX(${c}) as ${c}_max`,
      `AVG(${c}) as ${c}_avg`,
    ]);

    return `SELECT ${aggs.join(', ')} FROM ${table}`;
  }

  private generateOrphanCheckQuery(
    childTable: string,
    childColumns: string[],
    parentTable: string,
    parentColumns: string[]
  ): string {
    const joinConditions = childColumns.map((c, i) => 
      `c.${c} = p.${parentColumns[i]}`
    ).join(' AND ');

    const nullChecks = childColumns.map(c => `c.${c} IS NOT NULL`).join(' AND ');

    return `
      SELECT COUNT(*) as orphan_count
      FROM ${childTable} c
      WHERE ${nullChecks}
        AND NOT EXISTS (
          SELECT 1 FROM ${parentTable} p
          WHERE ${joinConditions}
        )
    `;
  }

  /**
   * Generate validation report template
   */
  generateReportTemplate(): string {
    return `
# Data Migration Validation Report

## Execution Summary
- **Migration Date**: {{migration_date}}
- **Source System**: {{source_system}}
- **Target System**: {{target_system}}
- **Total Tables**: {{total_tables}}

## Pre-Validation Results
| Check | Table | Status | Details |
|-------|-------|--------|---------|
{{#each pre_validation}}
| {{name}} | {{table}} | {{status}} | {{details}} |
{{/each}}

## Post-Validation Results
| Check | Table | Source | Target | Match | Critical |
|-------|-------|--------|--------|-------|----------|
{{#each post_validation}}
| {{name}} | {{table}} | {{source_value}} | {{target_value}} | {{match}} | {{critical}} |
{{/each}}

## Referential Integrity
| Relationship | Orphan Count | Status |
|--------------|--------------|--------|
{{#each referential}}
| {{relationship}} | {{orphan_count}} | {{status}} |
{{/each}}

## Summary
- **Passed Checks**: {{passed_count}}
- **Failed Checks**: {{failed_count}}
- **Overall Status**: {{overall_status}}

## Recommendations
{{#each recommendations}}
- {{this}}
{{/each}}
    `.trim();
  }
}

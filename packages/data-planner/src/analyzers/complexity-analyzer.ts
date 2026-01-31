/**
 * Migration Complexity Analyzer
 * 
 * Analyzes legacy schema to estimate migration complexity and effort.
 */

import type {
  LegacySchema,
  LegacyTable,
  MigrationEstimates,
  TableEstimate,
  InferredRelationship,
} from '../types.js';

export interface ComplexityConfig {
  rowsPerMinute: number;
  baseOverheadMinutes: number;
  complexityMultipliers: {
    transformations: number;
    relationships: number;
    largeBlobs: number;
    redefines: number;
  };
}

const DEFAULT_CONFIG: ComplexityConfig = {
  rowsPerMinute: 100000,
  baseOverheadMinutes: 30,
  complexityMultipliers: {
    transformations: 1.5,
    relationships: 1.2,
    largeBlobs: 2.0,
    redefines: 1.8,
  },
};

export class MigrationComplexityAnalyzer {
  private config: ComplexityConfig;

  constructor(config?: Partial<ComplexityConfig>) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  /**
   * Analyze schema and estimate migration complexity
   */
  analyze(
    schema: LegacySchema,
    relationships: InferredRelationship[],
    statistics?: Map<string, { rowCount: number; sizeBytes: number }>
  ): MigrationEstimates {
    const tableEstimates: TableEstimate[] = [];
    let totalRows = 0;
    let totalSizeGB = 0;

    for (const table of schema.tables) {
      const stats = statistics?.get(table.name);
      const estimate = this.estimateTable(table, relationships, stats);
      tableEstimates.push(estimate);
      totalRows += estimate.rows;
      totalSizeGB += estimate.sizeGB;
    }

    // Calculate total duration
    const baseDuration = totalRows / this.config.rowsPerMinute;
    const complexityMultiplier = this.calculateOverallComplexityMultiplier(tableEstimates);
    const estimatedDurationMinutes = Math.ceil(
      this.config.baseOverheadMinutes + (baseDuration * complexityMultiplier)
    );

    // Calculate scores
    const complexityScore = this.calculateComplexityScore(tableEstimates);
    const riskScore = this.calculateRiskScore(schema, relationships, tableEstimates);

    return {
      totalRows,
      totalSizeGB,
      estimatedDurationMinutes,
      complexityScore,
      riskScore,
      tableEstimates,
    };
  }

  private estimateTable(
    table: LegacyTable,
    relationships: InferredRelationship[],
    stats?: { rowCount: number; sizeBytes: number }
  ): TableEstimate {
    const rows = stats?.rowCount || table.rowCount || 0;
    const sizeGB = stats?.sizeBytes 
      ? stats.sizeBytes / (1024 * 1024 * 1024) 
      : (table.sizeBytes || 0) / (1024 * 1024 * 1024);

    const risks: string[] = [];
    let complexityFactors = 0;

    // Analyze column complexity
    for (const column of table.columns) {
      // Check for complex data types
      if (column.computed) {
        complexityFactors++;
        risks.push(`Computed column: ${column.name}`);
      }
      if (column.dataType.includes('blob') || column.dataType.includes('clob')) {
        complexityFactors += 2;
        risks.push(`Large object column: ${column.name}`);
      }
      if (column.nativeType?.includes('PACKED') || column.nativeType?.includes('COMP-3')) {
        complexityFactors++;
      }
    }

    // Check for relationships
    const tableRelationships = relationships.filter(
      r => r.sourceTable === table.name || r.targetTable === table.name
    );
    if (tableRelationships.length > 5) {
      complexityFactors += 2;
      risks.push(`High number of relationships: ${tableRelationships.length}`);
    }

    // Check for missing primary key
    if (!table.primaryKey || table.primaryKey.length === 0) {
      complexityFactors++;
      risks.push('No primary key defined');
    }

    // Calculate complexity level
    const complexity: 'low' | 'medium' | 'high' = 
      complexityFactors >= 4 ? 'high' :
      complexityFactors >= 2 ? 'medium' : 'low';

    // Estimate duration
    const baseMinutes = rows / this.config.rowsPerMinute;
    const multiplier = complexity === 'high' ? 2 : complexity === 'medium' ? 1.5 : 1;
    const estimatedMinutes = Math.ceil(baseMinutes * multiplier) + 5; // +5 min overhead per table

    return {
      table: table.name,
      rows,
      sizeGB,
      complexity,
      estimatedMinutes,
      risks,
    };
  }

  private calculateOverallComplexityMultiplier(estimates: TableEstimate[]): number {
    const highComplexity = estimates.filter(e => e.complexity === 'high').length;
    const mediumComplexity = estimates.filter(e => e.complexity === 'medium').length;
    const total = estimates.length || 1;

    return 1 + (highComplexity * 0.3 + mediumComplexity * 0.1) / total;
  }

  private calculateComplexityScore(estimates: TableEstimate[]): number {
    if (estimates.length === 0) return 0;

    const weights = { low: 1, medium: 2, high: 3 };
    const totalWeight = estimates.reduce((sum, e) => sum + weights[e.complexity], 0);
    const maxWeight = estimates.length * 3;

    return Math.round((totalWeight / maxWeight) * 10);
  }

  private calculateRiskScore(
    schema: LegacySchema,
    relationships: InferredRelationship[],
    estimates: TableEstimate[]
  ): number {
    let riskPoints = 0;
    const maxRisk = 10;

    // Risk from unconfirmed relationships
    const unconfirmedRels = relationships.filter(r => r.status === 'suggested').length;
    riskPoints += Math.min(3, unconfirmedRels * 0.2);

    // Risk from low-confidence relationships
    const lowConfidenceRels = relationships.filter(r => r.confidence < 0.7).length;
    riskPoints += Math.min(2, lowConfidenceRels * 0.3);

    // Risk from high complexity tables
    const highComplexityTables = estimates.filter(e => e.complexity === 'high').length;
    riskPoints += Math.min(3, highComplexityTables * 0.5);

    // Risk from large data volume
    const totalRows = estimates.reduce((sum, e) => sum + e.rows, 0);
    if (totalRows > 100_000_000) riskPoints += 2;
    else if (totalRows > 10_000_000) riskPoints += 1;

    // Risk from missing primary keys
    const missingPK = schema.tables.filter(t => !t.primaryKey || t.primaryKey.length === 0).length;
    riskPoints += Math.min(2, missingPK * 0.5);

    return Math.min(maxRisk, Math.round(riskPoints));
  }

  /**
   * Generate effort breakdown report
   */
  generateEffortReport(estimates: MigrationEstimates): string {
    const lines: string[] = [
      '# Data Migration Effort Estimate',
      '',
      '## Summary',
      `- Total Tables: ${estimates.tableEstimates.length}`,
      `- Total Rows: ${estimates.totalRows.toLocaleString()}`,
      `- Total Size: ${estimates.totalSizeGB.toFixed(2)} GB`,
      `- Estimated Duration: ${this.formatDuration(estimates.estimatedDurationMinutes)}`,
      `- Complexity Score: ${estimates.complexityScore}/10`,
      `- Risk Score: ${estimates.riskScore}/10`,
      '',
      '## Complexity Distribution',
      `- Low: ${estimates.tableEstimates.filter(e => e.complexity === 'low').length} tables`,
      `- Medium: ${estimates.tableEstimates.filter(e => e.complexity === 'medium').length} tables`,
      `- High: ${estimates.tableEstimates.filter(e => e.complexity === 'high').length} tables`,
      '',
      '## Table Details',
      '',
      '| Table | Rows | Size | Complexity | Est. Time | Risks |',
      '|-------|------|------|------------|-----------|-------|',
    ];

    for (const table of estimates.tableEstimates.sort((a, b) => b.rows - a.rows)) {
      lines.push(
        `| ${table.table} | ${table.rows.toLocaleString()} | ${table.sizeGB.toFixed(2)} GB | ${table.complexity} | ${this.formatDuration(table.estimatedMinutes)} | ${table.risks.length > 0 ? table.risks.join('; ') : '-'} |`
      );
    }

    return lines.join('\n');
  }

  private formatDuration(minutes: number): string {
    if (minutes < 60) return `${minutes} min`;
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    if (hours < 24) return `${hours}h ${mins}m`;
    const days = Math.floor(hours / 24);
    const hrs = hours % 24;
    return `${days}d ${hrs}h`;
  }
}

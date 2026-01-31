/**
 * Data Quality Analyzer
 * 
 * Analyzes legacy data for quality issues that may impact migration.
 */

import type {
  LegacySchema,
  LegacyTable,
  DataQualityReport,
  TableQualityReport,
  ColumnQualityReport,
  DataQualityIssue,
  PatternMatch,
} from '../types.js';

export interface DataQualityConfig {
  sampleSize?: number;
  nullThreshold?: number;
  duplicateThreshold?: number;
  patternDetection?: boolean;
}

export interface DataSample {
  table: string;
  column: string;
  values: unknown[];
  totalRows: number;
}

export class DataQualityAnalyzer {
  private config: DataQualityConfig;

  constructor(config: DataQualityConfig = {}) {
    this.config = {
      sampleSize: 10000,
      nullThreshold: 0.1,
      duplicateThreshold: 0.5,
      patternDetection: true,
      ...config,
    };
  }

  /**
   * Generate SQL queries for data quality analysis
   */
  generateQualityQueries(schema: LegacySchema): Map<string, string[]> {
    const queries = new Map<string, string[]>();

    for (const table of schema.tables) {
      const tableQueries: string[] = [];

      // Row count
      tableQueries.push(`SELECT COUNT(*) AS row_count FROM ${table.name}`);

      // Null analysis per column
      for (const column of table.columns) {
        tableQueries.push(`
          SELECT 
            '${column.name}' AS column_name,
            COUNT(*) AS total_rows,
            SUM(CASE WHEN ${column.name} IS NULL THEN 1 ELSE 0 END) AS null_count,
            COUNT(DISTINCT ${column.name}) AS distinct_count
          FROM ${table.name}
        `);
      }

      // Duplicate analysis for potential keys
      const potentialKeys = table.columns.filter(c => 
        c.name.toLowerCase().includes('id') ||
        c.name.toLowerCase().includes('key') ||
        c.name.toLowerCase().includes('code')
      );

      for (const keyCol of potentialKeys) {
        tableQueries.push(`
          SELECT 
            ${keyCol.name},
            COUNT(*) AS occurrence_count
          FROM ${table.name}
          GROUP BY ${keyCol.name}
          HAVING COUNT(*) > 1
          ORDER BY COUNT(*) DESC
          FETCH FIRST 100 ROWS ONLY
        `);
      }

      queries.set(table.name, tableQueries);
    }

    return queries;
  }

  /**
   * Analyze data samples and generate quality report
   */
  analyzeDataSamples(
    schema: LegacySchema,
    samples: DataSample[]
  ): DataQualityReport {
    const tableReports: TableQualityReport[] = [];
    const issues: DataQualityIssue[] = [];

    // Group samples by table
    const samplesByTable = new Map<string, DataSample[]>();
    for (const sample of samples) {
      if (!samplesByTable.has(sample.table)) {
        samplesByTable.set(sample.table, []);
      }
      samplesByTable.get(sample.table)!.push(sample);
    }

    // Analyze each table
    for (const table of schema.tables) {
      const tableSamples = samplesByTable.get(table.name) || [];
      const columnReports: ColumnQualityReport[] = [];

      for (const column of table.columns) {
        const sample = tableSamples.find(s => s.column === column.name);
        const report = this.analyzeColumn(table, column, sample);
        columnReports.push(report);

        // Generate issues
        issues.push(...this.generateColumnIssues(table.name, column.name, report));
      }

      const tableScore = this.calculateTableScore(columnReports);
      tableReports.push({
        table: table.name,
        rowCount: tableSamples[0]?.totalRows || 0,
        columnReports,
        score: tableScore,
      });
    }

    const overallScore = tableReports.length > 0
      ? tableReports.reduce((sum, t) => sum + t.score, 0) / tableReports.length
      : 0;

    return {
      sourceId: schema.sourceId,
      analyzedAt: new Date(),
      tables: tableReports,
      overallScore,
      issues: issues.sort((a, b) => {
        const severityOrder = { critical: 0, warning: 1, info: 2 };
        return severityOrder[a.severity] - severityOrder[b.severity];
      }),
    };
  }

  private analyzeColumn(
    _table: LegacyTable,
    column: { name: string; nullable: boolean },
    sample?: DataSample
  ): ColumnQualityReport {
    if (!sample || sample.values.length === 0) {
      return {
        column: column.name,
        nullRate: 0,
        uniqueRate: 0,
      };
    }

    const values = sample.values;
    const total = values.length;

    // Null rate
    const nullCount = values.filter(v => v === null || v === undefined || v === '').length;
    const nullRate = nullCount / total;

    // Unique rate
    const uniqueValues = new Set(values.map(v => String(v)));
    const uniqueRate = uniqueValues.size / total;

    // Pattern detection
    const patternMatches = this.config.patternDetection
      ? this.detectPatterns(values)
      : undefined;

    return {
      column: column.name,
      nullRate,
      uniqueRate,
      patternMatches,
    };
  }

  private detectPatterns(values: unknown[]): PatternMatch[] {
    const stringValues = values
      .filter(v => v !== null && v !== undefined)
      .map(v => String(v));

    if (stringValues.length === 0) return [];

    const patterns: { pattern: string; regex: RegExp; examples: string[] }[] = [
      { pattern: 'Email', regex: /^[\w.-]+@[\w.-]+\.\w+$/, examples: [] },
      { pattern: 'Phone', regex: /^[\d\s\-()]+$/, examples: [] },
      { pattern: 'Date (YYYYMMDD)', regex: /^\d{8}$/, examples: [] },
      { pattern: 'Date (YYYY-MM-DD)', regex: /^\d{4}-\d{2}-\d{2}$/, examples: [] },
      { pattern: 'SSN', regex: /^\d{3}-?\d{2}-?\d{4}$/, examples: [] },
      { pattern: 'ZIP Code', regex: /^\d{5}(-\d{4})?$/, examples: [] },
      { pattern: 'Numeric', regex: /^-?\d+\.?\d*$/, examples: [] },
      { pattern: 'Alpha', regex: /^[A-Za-z]+$/, examples: [] },
      { pattern: 'Alphanumeric', regex: /^[A-Za-z0-9]+$/, examples: [] },
    ];

    const matches: PatternMatch[] = [];

    for (const { pattern, regex, examples } of patterns) {
      let matchCount = 0;
      
      for (const value of stringValues) {
        if (regex.test(value)) {
          matchCount++;
          if (examples.length < 3) {
            examples.push(value);
          }
        }
      }

      const matchRate = matchCount / stringValues.length;
      if (matchRate > 0.5) {
        matches.push({
          pattern,
          matchRate,
          examples,
        });
      }
    }

    return matches.sort((a, b) => b.matchRate - a.matchRate);
  }

  private generateColumnIssues(
    tableName: string,
    columnName: string,
    report: ColumnQualityReport
  ): DataQualityIssue[] {
    const issues: DataQualityIssue[] = [];

    // High null rate
    if (report.nullRate > this.config.nullThreshold!) {
      issues.push({
        severity: report.nullRate > 0.5 ? 'critical' : 'warning',
        category: 'null-values',
        table: tableName,
        column: columnName,
        description: `High null rate: ${(report.nullRate * 100).toFixed(1)}% of values are null`,
        recommendation: 'Consider adding default value or reviewing data extraction',
      });
    }

    // Very low uniqueness (potential duplicate data)
    if (report.uniqueRate < 0.01 && report.uniqueRate > 0) {
      issues.push({
        severity: 'info',
        category: 'duplicates',
        table: tableName,
        column: columnName,
        description: `Low cardinality: only ${(report.uniqueRate * 100).toFixed(2)}% unique values`,
        recommendation: 'Consider using an enum or lookup table',
      });
    }

    return issues;
  }

  private calculateTableScore(columnReports: ColumnQualityReport[]): number {
    if (columnReports.length === 0) return 100;

    let totalDeductions = 0;

    for (const report of columnReports) {
      // Deduct for high null rates
      if (report.nullRate > this.config.nullThreshold!) {
        totalDeductions += (report.nullRate - this.config.nullThreshold!) * 50;
      }
    }

    return Math.max(0, 100 - totalDeductions);
  }

  /**
   * Generate referential integrity check queries
   */
  generateReferentialIntegrityQueries(
    _schema: LegacySchema,
    relationships: { sourceTable: string; sourceColumns: string[]; targetTable: string; targetColumns: string[] }[]
  ): string[] {
    const queries: string[] = [];

    for (const rel of relationships) {
      const sourceColList = rel.sourceColumns.join(', ');
      const targetColList = rel.targetColumns.join(', ');

      queries.push(`
        -- Orphan records in ${rel.sourceTable} referencing ${rel.targetTable}
        SELECT COUNT(*) AS orphan_count,
               'Orphan records: ${rel.sourceTable}.${sourceColList} -> ${rel.targetTable}.${targetColList}' AS description
        FROM ${rel.sourceTable} s
        WHERE NOT EXISTS (
          SELECT 1 FROM ${rel.targetTable} t
          WHERE ${rel.sourceColumns.map((sc, i) => `s.${sc} = t.${rel.targetColumns[i]}`).join(' AND ')}
        )
        AND ${rel.sourceColumns.map(sc => `s.${sc} IS NOT NULL`).join(' AND ')}
      `);
    }

    return queries;
  }
}

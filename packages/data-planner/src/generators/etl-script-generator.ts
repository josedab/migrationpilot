/**
 * ETL Script Generator
 * 
 * Generates ETL scripts for data migration in various formats.
 */

import type {
  TableMigrationPlan,
  ETLScript,
  ETLPipeline,
  TargetDatabaseType,
  ColumnMigrationPlan,
} from '../types.js';

export interface ETLGeneratorConfig {
  outputFormat: 'sql' | 'python' | 'spark' | 'dbt';
  targetDatabase: TargetDatabaseType;
  includeErrorHandling: boolean;
  includeLogging: boolean;
  batchSize: number;
}

export class ETLScriptGenerator {
  private config: ETLGeneratorConfig;

  constructor(config: ETLGeneratorConfig) {
    this.config = config;
  }

  /**
   * Generate ETL scripts for all table mappings
   */
  generatePipeline(
    mappings: TableMigrationPlan[],
    pipelineName: string
  ): ETLPipeline {
    const scripts: ETLScript[] = mappings.map(mapping => 
      this.generateScript(mapping)
    );

    return {
      id: `pipeline_${Date.now()}`,
      name: pipelineName,
      scripts,
      notifications: [
        { type: 'email', events: ['failure'], recipients: [] },
      ],
    };
  }

  /**
   * Generate ETL script for a single table
   */
  generateScript(mapping: TableMigrationPlan): ETLScript {
    let content: string;

    switch (this.config.outputFormat) {
      case 'python':
        content = this.generatePythonScript(mapping);
        break;
      case 'spark':
        content = this.generateSparkScript(mapping);
        break;
      case 'dbt':
        content = this.generateDBTModel(mapping);
        break;
      default:
        content = this.generateSQLScript(mapping);
    }

    return {
      id: `etl_${mapping.sourceTable}`,
      name: `ETL - ${mapping.sourceTable} to ${mapping.targetTable}`,
      language: this.config.outputFormat,
      content,
      sourceTable: mapping.sourceTable,
      targetTable: mapping.targetTable,
      dependencies: mapping.dependencies.map(dep => `etl_${dep}`),
      estimatedDuration: 5,
    };
  }

  private generateSQLScript(mapping: TableMigrationPlan): string {
    const selectColumns = mapping.columnMappings.map(col => {
      if (col.transformation) {
        return this.generateSQLTransformation(col);
      }
      return col.sourceColumn;
    });

    const insertColumns = mapping.columnMappings.map(col => col.targetColumn);

    const lines: string[] = [
      `-- ETL Script: ${mapping.sourceTable} -> ${mapping.targetTable}`,
      `-- Generated: ${new Date().toISOString()}`,
      '',
    ];

    if (this.config.includeLogging) {
      lines.push(
        `-- Log start`,
        `INSERT INTO etl_log (table_name, status, started_at)`,
        `VALUES ('${mapping.targetTable}', 'RUNNING', CURRENT_TIMESTAMP);`,
        ''
      );
    }

    if (this.config.includeErrorHandling) {
      lines.push('BEGIN TRANSACTION;', '');
    }

    // Truncate target (optional)
    lines.push(`-- Clear target table`, `TRUNCATE TABLE ${mapping.targetTable};`, '');

    // Insert with batching
    if (this.config.batchSize > 0) {
      lines.push(
        `-- Batch insert (${this.config.batchSize} rows per batch)`,
        `INSERT INTO ${mapping.targetTable} (${insertColumns.join(', ')})`,
        `SELECT ${selectColumns.join(', ')}`,
        `FROM ${mapping.sourceTable}`,
        mapping.filterCondition ? `WHERE ${mapping.filterCondition}` : '',
        mapping.orderBy?.length ? `ORDER BY ${mapping.orderBy.join(', ')}` : '',
        `;`
      );
    } else {
      lines.push(
        `INSERT INTO ${mapping.targetTable} (${insertColumns.join(', ')})`,
        `SELECT ${selectColumns.join(', ')}`,
        `FROM ${mapping.sourceTable}`,
        mapping.filterCondition ? `WHERE ${mapping.filterCondition}` : '',
        `;`
      );
    }

    if (this.config.includeErrorHandling) {
      lines.push(
        '',
        'COMMIT;',
        '',
        '-- Error handling would be added in stored procedure'
      );
    }

    if (this.config.includeLogging) {
      lines.push(
        '',
        `-- Log completion`,
        `UPDATE etl_log`,
        `SET status = 'COMPLETED', completed_at = CURRENT_TIMESTAMP,`,
        `    rows_processed = (SELECT COUNT(*) FROM ${mapping.targetTable})`,
        `WHERE table_name = '${mapping.targetTable}' AND status = 'RUNNING';`
      );
    }

    return lines.filter(l => l !== '').join('\n');
  }

  private generateSQLTransformation(col: ColumnMigrationPlan): string {
    if (!col.transformation) return col.sourceColumn;

    switch (col.transformation.type) {
      case 'cast':
        return `CAST(${col.sourceColumn} AS ${col.transformation.expression}) AS ${col.targetColumn}`;
      case 'substring':
        return `SUBSTRING(${col.sourceColumn}, ${col.transformation.parameters?.start || 1}, ${col.transformation.parameters?.length || 100}) AS ${col.targetColumn}`;
      case 'concat':
        return col.transformation.expression || col.sourceColumn;
      case 'custom':
        return `${col.transformation.expression} AS ${col.targetColumn}`;
      default:
        return col.sourceColumn;
    }
  }

  private generatePythonScript(mapping: TableMigrationPlan): string {
    const lines: string[] = [
      `"""`,
      `ETL Script: ${mapping.sourceTable} -> ${mapping.targetTable}`,
      `Generated: ${new Date().toISOString()}`,
      `"""`,
      '',
      'import pandas as pd',
      'from sqlalchemy import create_engine',
      'import logging',
      '',
      'logging.basicConfig(level=logging.INFO)',
      'logger = logging.getLogger(__name__)',
      '',
      '',
      `def transform_${mapping.sourceTable.toLowerCase()}(df: pd.DataFrame) -> pd.DataFrame:`,
      '    """Apply transformations to source data."""',
      '    result = df.copy()',
      '',
    ];

    // Add transformations
    for (const col of mapping.columnMappings) {
      if (col.transformation) {
        lines.push(`    # Transform ${col.sourceColumn} -> ${col.targetColumn}`);
        lines.push(this.generatePythonTransformation(col));
      } else if (col.sourceColumn !== col.targetColumn) {
        lines.push(`    result = result.rename(columns={'${col.sourceColumn}': '${col.targetColumn}'})`);
      }
    }

    lines.push(
      '',
      '    return result',
      '',
      '',
      `def migrate_${mapping.sourceTable.toLowerCase()}(source_engine, target_engine, batch_size: int = ${this.config.batchSize}):`,
      '    """Migrate data from source to target."""',
      `    logger.info("Starting migration: ${mapping.sourceTable} -> ${mapping.targetTable}")`,
      '',
      '    try:',
      `        # Read source data`,
      `        query = "SELECT * FROM ${mapping.sourceTable}"`,
      mapping.filterCondition ? `        query += " WHERE ${mapping.filterCondition}"` : '',
      '        ',
      '        total_rows = 0',
      '        for chunk in pd.read_sql(query, source_engine, chunksize=batch_size):',
      '            # Apply transformations',
      `            transformed = transform_${mapping.sourceTable.toLowerCase()}(chunk)`,
      '            ',
      '            # Write to target',
      `            transformed.to_sql('${mapping.targetTable}', target_engine, `,
      `                              if_exists='append', index=False)`,
      '            total_rows += len(chunk)',
      '            logger.info(f"Processed {total_rows} rows")',
      '        ',
      '        logger.info(f"Migration complete: {total_rows} total rows")',
      '        return total_rows',
      '        ',
      '    except Exception as e:',
      '        logger.error(f"Migration failed: {e}")',
      '        raise',
      '',
      '',
      'if __name__ == "__main__":',
      '    # Configure connections',
      '    source_engine = create_engine("source_connection_string")',
      '    target_engine = create_engine("target_connection_string")',
      '    ',
      `    migrate_${mapping.sourceTable.toLowerCase()}(source_engine, target_engine)`,
    );

    return lines.join('\n');
  }

  private generatePythonTransformation(col: ColumnMigrationPlan): string {
    if (!col.transformation) return '';

    switch (col.transformation.type) {
      case 'cast':
        return `    result['${col.targetColumn}'] = result['${col.sourceColumn}'].astype('${col.transformation.expression}')`;
      case 'substring': {
        const start = Number(col.transformation.parameters?.start || 0);
        const length = Number(col.transformation.parameters?.length || 100);
        const end = start + length;
        return `    result['${col.targetColumn}'] = result['${col.sourceColumn}'].str[${start}:${end}]`;
      }
      case 'custom':
        if (col.transformation.parameters?.sourceType === 'packed-decimal') {
          return `    result['${col.targetColumn}'] = result['${col.sourceColumn}'].apply(unpack_decimal)`;
        }
        return `    result['${col.targetColumn}'] = result['${col.sourceColumn}']  # Custom transformation needed`;
      default:
        return `    result['${col.targetColumn}'] = result['${col.sourceColumn}']`;
    }
  }

  private generateSparkScript(mapping: TableMigrationPlan): string {
    const lines: string[] = [
      `# ETL Script: ${mapping.sourceTable} -> ${mapping.targetTable}`,
      `# Generated: ${new Date().toISOString()}`,
      '',
      'from pyspark.sql import SparkSession',
      'from pyspark.sql.functions import col, when, lit, trim, upper',
      '',
      'spark = SparkSession.builder \\',
      `    .appName("ETL_${mapping.sourceTable}") \\`,
      '    .getOrCreate()',
      '',
      '# Read source data',
      `source_df = spark.read \\`,
      '    .format("jdbc") \\',
      '    .option("url", "jdbc:db2://source_host:50000/source_db") \\',
      `    .option("dbtable", "${mapping.sourceTable}") \\`,
      '    .option("user", "user") \\',
      '    .option("password", "password") \\',
      '    .load()',
      '',
    ];

    // Add filter if specified
    if (mapping.filterCondition) {
      lines.push(`source_df = source_df.filter("${mapping.filterCondition}")`);
      lines.push('');
    }

    lines.push('# Apply transformations');
    lines.push('transformed_df = source_df');

    for (const col of mapping.columnMappings) {
      if (col.transformation) {
        lines.push(this.generateSparkTransformation(col));
      } else if (col.sourceColumn !== col.targetColumn) {
        lines.push(`transformed_df = transformed_df.withColumnRenamed("${col.sourceColumn}", "${col.targetColumn}")`);
      }
    }

    lines.push(
      '',
      '# Write to target',
      'transformed_df.write \\',
      '    .format("jdbc") \\',
      '    .option("url", "jdbc:postgresql://target_host:5432/target_db") \\',
      `    .option("dbtable", "${mapping.targetTable}") \\`,
      '    .option("user", "user") \\',
      '    .option("password", "password") \\',
      '    .mode("append") \\',
      '    .save()',
      '',
      'print(f"Migrated {transformed_df.count()} rows")',
      'spark.stop()',
    );

    return lines.join('\n');
  }

  private generateSparkTransformation(col: ColumnMigrationPlan): string {
    if (!col.transformation) return '';

    switch (col.transformation.type) {
      case 'cast':
        return `transformed_df = transformed_df.withColumn("${col.targetColumn}", col("${col.sourceColumn}").cast("${col.transformation.expression}"))`;
      case 'substring':
        const start = col.transformation.parameters?.start || 1;
        const length = col.transformation.parameters?.length || 100;
        return `transformed_df = transformed_df.withColumn("${col.targetColumn}", col("${col.sourceColumn}").substr(${start}, ${length}))`;
      case 'custom':
        return `transformed_df = transformed_df.withColumn("${col.targetColumn}", col("${col.sourceColumn}"))  # Custom UDF needed`;
      default:
        return `transformed_df = transformed_df.withColumn("${col.targetColumn}", col("${col.sourceColumn}"))`;
    }
  }

  private generateDBTModel(mapping: TableMigrationPlan): string {
    const selectColumns = mapping.columnMappings.map(col => {
      if (col.transformation) {
        return this.generateDBTTransformation(col);
      }
      if (col.sourceColumn !== col.targetColumn) {
        return `    ${col.sourceColumn} as ${col.targetColumn}`;
      }
      return `    ${col.sourceColumn}`;
    });

    const lines: string[] = [
      `-- models/${mapping.targetTable}.sql`,
      `-- ETL Model: ${mapping.sourceTable} -> ${mapping.targetTable}`,
      `-- Generated: ${new Date().toISOString()}`,
      '',
      '{{',
      `  config(`,
      `    materialized='incremental',`,
      `    unique_key='id',`,
      `    schema='target_schema'`,
      '  )',
      '}}',
      '',
      `with source as (`,
      `    select * from {{ source('legacy', '${mapping.sourceTable}') }}`,
      mapping.filterCondition ? `    where ${mapping.filterCondition}` : '',
      '),',
      '',
      'transformed as (',
      '    select',
      selectColumns.join(',\n'),
      '    from source',
      ')',
      '',
      'select * from transformed',
    ];

    return lines.filter(l => l !== '').join('\n');
  }

  private generateDBTTransformation(col: ColumnMigrationPlan): string {
    if (!col.transformation) {
      return `    ${col.sourceColumn}`;
    }

    switch (col.transformation.type) {
      case 'cast':
        return `    cast(${col.sourceColumn} as ${col.transformation.expression}) as ${col.targetColumn}`;
      case 'substring':
        const start = col.transformation.parameters?.start || 1;
        const length = col.transformation.parameters?.length || 100;
        return `    substr(${col.sourceColumn}, ${start}, ${length}) as ${col.targetColumn}`;
      case 'custom':
        return `    ${col.transformation.expression || col.sourceColumn} as ${col.targetColumn}`;
      default:
        return `    ${col.sourceColumn} as ${col.targetColumn}`;
    }
  }
}

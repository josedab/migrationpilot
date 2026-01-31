/**
 * Migration Plan Generator
 * 
 * Generates comprehensive data migration plans including
 * target schema design, mappings, and execution strategy.
 */

import type {
  LegacySchema,
  LegacyTable,
  LegacyColumn,
  TargetSchema,
  TargetTable,
  TargetColumn,
  TargetDatabaseType,
  DataMigrationPlan,
  TableMigrationPlan,
  ColumnMigrationPlan,
  InferredRelationship,
  MigrationStrategy,
  MigrationPhase,
  ValidationQuery,
  ColumnTransformation,
} from '../types.js';

export interface MigrationPlanConfig {
  targetDatabase: TargetDatabaseType;
  schemaName?: string;
  namingConvention: 'snake_case' | 'camelCase' | 'PascalCase';
  preserveNullability: boolean;
  addAuditColumns: boolean;
  generateIndexes: boolean;
  batchSize: number;
  parallelism: number;
}

const TYPE_MAPPINGS: Record<TargetDatabaseType, Record<string, string>> = {
  postgresql: {
    'integer': 'INTEGER',
    'bigint': 'BIGINT',
    'smallint': 'SMALLINT',
    'decimal': 'DECIMAL',
    'numeric': 'NUMERIC',
    'float': 'REAL',
    'double': 'DOUBLE PRECISION',
    'char': 'CHAR',
    'varchar': 'VARCHAR',
    'text': 'TEXT',
    'date': 'DATE',
    'time': 'TIME',
    'timestamp': 'TIMESTAMP',
    'boolean': 'BOOLEAN',
    'bytea': 'BYTEA',
    'blob': 'BYTEA',
    'clob': 'TEXT',
  },
  mysql: {
    'integer': 'INT',
    'bigint': 'BIGINT',
    'smallint': 'SMALLINT',
    'decimal': 'DECIMAL',
    'numeric': 'DECIMAL',
    'float': 'FLOAT',
    'double': 'DOUBLE',
    'char': 'CHAR',
    'varchar': 'VARCHAR',
    'text': 'TEXT',
    'date': 'DATE',
    'time': 'TIME',
    'timestamp': 'TIMESTAMP',
    'boolean': 'TINYINT(1)',
    'bytea': 'BLOB',
    'blob': 'LONGBLOB',
    'clob': 'LONGTEXT',
  },
  oracle: {
    'integer': 'NUMBER(10)',
    'bigint': 'NUMBER(19)',
    'smallint': 'NUMBER(5)',
    'decimal': 'NUMBER',
    'numeric': 'NUMBER',
    'float': 'FLOAT',
    'double': 'BINARY_DOUBLE',
    'char': 'CHAR',
    'varchar': 'VARCHAR2',
    'text': 'CLOB',
    'date': 'DATE',
    'time': 'TIMESTAMP',
    'timestamp': 'TIMESTAMP',
    'boolean': 'NUMBER(1)',
    'bytea': 'BLOB',
    'blob': 'BLOB',
    'clob': 'CLOB',
  },
  sqlserver: {
    'integer': 'INT',
    'bigint': 'BIGINT',
    'smallint': 'SMALLINT',
    'decimal': 'DECIMAL',
    'numeric': 'NUMERIC',
    'float': 'REAL',
    'double': 'FLOAT',
    'char': 'CHAR',
    'varchar': 'VARCHAR',
    'text': 'NVARCHAR(MAX)',
    'date': 'DATE',
    'time': 'TIME',
    'timestamp': 'DATETIME2',
    'boolean': 'BIT',
    'bytea': 'VARBINARY(MAX)',
    'blob': 'VARBINARY(MAX)',
    'clob': 'NVARCHAR(MAX)',
  },
  mongodb: {
    'integer': 'int',
    'bigint': 'long',
    'decimal': 'decimal',
    'varchar': 'string',
    'text': 'string',
    'date': 'date',
    'timestamp': 'date',
    'boolean': 'bool',
    'bytea': 'binData',
  },
  dynamodb: {
    'integer': 'N',
    'bigint': 'N',
    'decimal': 'N',
    'varchar': 'S',
    'text': 'S',
    'date': 'S',
    'timestamp': 'S',
    'boolean': 'BOOL',
    'bytea': 'B',
  },
};

export class MigrationPlanGenerator {
  private config: MigrationPlanConfig;

  constructor(config: MigrationPlanConfig) {
    this.config = config;
  }

  /**
   * Generate complete migration plan
   */
  generatePlan(
    source: LegacySchema,
    relationships: InferredRelationship[],
    projectId: string,
    userId: string
  ): DataMigrationPlan {
    // Generate target schema
    const target = this.generateTargetSchema(source, relationships);

    // Generate table mappings
    const tableMappings = this.generateTableMappings(source, target, relationships);

    // Determine migration strategy
    const strategy = this.generateStrategy(tableMappings, relationships);

    // Generate validation queries
    const validationQueries = this.generateValidationQueries(source, target, tableMappings);

    return {
      id: `plan_${Date.now()}`,
      projectId,
      name: `Migration Plan - ${source.sourceId}`,
      description: `Data migration from ${source.sourceType} to ${this.config.targetDatabase}`,
      source,
      target,
      tableMappings,
      inferredRelationships: relationships,
      strategy,
      validationQueries,
      estimates: {
        totalRows: 0,
        totalSizeGB: 0,
        estimatedDurationMinutes: 0,
        complexityScore: 0,
        riskScore: 0,
        tableEstimates: [],
      },
      status: 'draft',
      createdAt: new Date(),
      updatedAt: new Date(),
      createdBy: userId,
    };
  }

  /**
   * Generate target schema from source
   */
  generateTargetSchema(
    source: LegacySchema,
    relationships: InferredRelationship[]
  ): TargetSchema {
    const tables: TargetTable[] = source.tables.map(sourceTable => 
      this.generateTargetTable(sourceTable, relationships)
    );

    return {
      targetType: this.config.targetDatabase,
      tables,
      generatedAt: new Date(),
    };
  }

  private generateTargetTable(
    source: LegacyTable,
    relationships: InferredRelationship[]
  ): TargetTable {
    const columns: TargetColumn[] = source.columns.map(col => 
      this.generateTargetColumn(col, source.name)
    );

    // Add audit columns if configured
    if (this.config.addAuditColumns) {
      columns.push(
        {
          name: 'created_at',
          dataType: this.mapDataType('timestamp'),
          nullable: false,
          defaultValue: 'CURRENT_TIMESTAMP',
          sourceMapping: { sourceColumn: '', sourceTable: source.name },
        },
        {
          name: 'updated_at',
          dataType: this.mapDataType('timestamp'),
          nullable: false,
          defaultValue: 'CURRENT_TIMESTAMP',
          sourceMapping: { sourceColumn: '', sourceTable: source.name },
        }
      );
    }

    // Determine primary key
    let primaryKey = source.primaryKey || [];
    if (primaryKey.length === 0) {
      // Generate synthetic primary key
      const idColumn: TargetColumn = {
        name: 'id',
        dataType: this.mapDataType('bigint'),
        nullable: false,
        generated: { type: 'identity' },
        sourceMapping: { sourceColumn: '', sourceTable: source.name },
      };
      columns.unshift(idColumn);
      primaryKey = ['id'];
    }

    // Generate indexes
    const indexes = this.config.generateIndexes
      ? this.generateIndexes(source, relationships)
      : [];

    return {
      name: this.transformName(source.name),
      schema: this.config.schemaName,
      columns,
      primaryKey: primaryKey.map(k => this.transformName(k)),
      foreignKeys: source.foreignKeys.map(fk => ({
        ...fk,
        name: this.transformName(fk.name),
        columns: fk.columns.map(c => this.transformName(c)),
        referencedTable: this.transformName(fk.referencedTable),
        referencedColumns: fk.referencedColumns.map(c => this.transformName(c)),
      })),
      indexes: indexes.map(idx => ({
        ...idx,
        name: this.transformName(idx.name),
        columns: idx.columns.map(c => this.transformName(c)),
      })),
      sourceMapping: {
        sourceTable: source.name,
        sourceType: 'db2',
        transformationType: 'direct',
      },
    };
  }

  private generateTargetColumn(source: LegacyColumn, tableName: string): TargetColumn {
    const targetType = this.mapDataType(source.dataType);
    const transformation = this.determineTransformation(source);

    return {
      name: this.transformName(source.name),
      dataType: targetType,
      nullable: this.config.preserveNullability ? source.nullable : true,
      defaultValue: source.defaultValue,
      generated: source.computed ? { type: 'computed', expression: source.computeExpression } : undefined,
      sourceMapping: {
        sourceColumn: source.name,
        sourceTable: tableName,
        transformation,
      },
    };
  }

  private mapDataType(sourceType: string): string {
    const baseType = sourceType.toLowerCase().replace(/\(.*\)/, '').trim();
    const typeMap = TYPE_MAPPINGS[this.config.targetDatabase];
    
    // Check for exact match
    if (typeMap[baseType]) {
      // Preserve length/precision if present
      const match = sourceType.match(/\(([^)]+)\)/);
      if (match && !['text', 'blob', 'clob'].includes(baseType)) {
        return `${typeMap[baseType]}(${match[1]})`;
      }
      return typeMap[baseType];
    }

    // Default handling
    return typeMap['varchar'] + '(255)';
  }

  private determineTransformation(column: LegacyColumn): ColumnTransformation | undefined {
    const nativeType = column.nativeType?.toUpperCase() || '';

    // Packed decimal conversion
    if (nativeType.includes('PACKED') || nativeType.includes('COMP-3')) {
      return {
        type: 'custom',
        expression: `UNPACK_DECIMAL(${column.name})`,
        parameters: { sourceType: 'packed-decimal' },
      };
    }

    // Date format conversion
    if (column.dataType === 'date' && column.length === 8) {
      return {
        type: 'custom',
        expression: `TO_DATE(${column.name}, 'YYYYMMDD')`,
        parameters: { sourceFormat: 'YYYYMMDD' },
      };
    }

    // EBCDIC to ASCII conversion (mainframe)
    if (nativeType.includes('CHAR') && column.nativeType?.includes('EBCDIC')) {
      return {
        type: 'custom',
        expression: `CONVERT_EBCDIC(${column.name})`,
        parameters: { encoding: 'EBCDIC' },
      };
    }

    return undefined;
  }

  private generateIndexes(
    source: LegacyTable,
    relationships: InferredRelationship[]
  ): { name: string; columns: string[]; unique: boolean; type: 'btree' }[] {
    const indexes: { name: string; columns: string[]; unique: boolean; type: 'btree' }[] = [];

    // Keep existing indexes
    for (const idx of source.indexes) {
      indexes.push({
        name: idx.name,
        columns: idx.columns,
        unique: idx.unique,
        type: 'btree',
      });
    }

    // Add indexes for foreign key columns
    const tableRels = relationships.filter(r => r.sourceTable === source.name);
    for (const rel of tableRels) {
      const indexExists = indexes.some(idx => 
        idx.columns.length === rel.sourceColumns.length &&
        idx.columns.every((c, i) => c === rel.sourceColumns[i])
      );
      if (!indexExists) {
        indexes.push({
          name: `idx_${source.name}_${rel.sourceColumns.join('_')}`,
          columns: rel.sourceColumns,
          unique: false,
          type: 'btree',
        });
      }
    }

    return indexes;
  }

  private transformName(name: string): string {
    const cleaned = name.toLowerCase().replace(/[^a-z0-9]/g, '_');
    
    switch (this.config.namingConvention) {
      case 'camelCase':
        return cleaned.replace(/_([a-z])/g, (_, c) => c.toUpperCase());
      case 'PascalCase':
        return cleaned.replace(/(^|_)([a-z])/g, (_, __, c) => c.toUpperCase());
      default:
        return cleaned;
    }
  }

  /**
   * Generate table migration plans
   */
  private generateTableMappings(
    source: LegacySchema,
    target: TargetSchema,
    relationships: InferredRelationship[]
  ): TableMigrationPlan[] {
    const plans: TableMigrationPlan[] = [];
    const dependencyGraph = this.buildDependencyGraph(source.tables, relationships);

    // Sort tables by dependency order
    const sortedTables = this.topologicalSort(source.tables, dependencyGraph);

    for (let i = 0; i < sortedTables.length; i++) {
      const sourceTable = sortedTables[i];
      if (!sourceTable) continue;
      
      const targetTable = target.tables.find(t => 
        t.sourceMapping.sourceTable === sourceTable.name
      );

      if (!targetTable) continue;

      const columnMappings: ColumnMigrationPlan[] = sourceTable.columns.map(col => ({
        sourceColumn: col.name,
        targetColumn: this.transformName(col.name),
        transformation: this.determineTransformation(col) || null,
        nullHandling: col.nullable ? 'preserve' : 'error',
      }));

      // Determine dependencies
      const tableDeps = relationships
        .filter(r => r.sourceTable === sourceTable.name && r.status !== 'rejected')
        .map(r => r.targetTable);

      plans.push({
        sourceTable: sourceTable.name,
        targetTable: targetTable.name,
        columnMappings,
        batchSize: this.config.batchSize,
        parallelism: this.config.parallelism,
        dependencies: tableDeps,
        priority: sortedTables.length - i,
      });
    }

    return plans;
  }

  private buildDependencyGraph(
    tables: LegacyTable[],
    relationships: InferredRelationship[]
  ): Map<string, Set<string>> {
    const graph = new Map<string, Set<string>>();
    
    for (const table of tables) {
      graph.set(table.name, new Set());
    }

    for (const rel of relationships) {
      if (rel.status !== 'rejected') {
        graph.get(rel.sourceTable)?.add(rel.targetTable);
      }
    }

    return graph;
  }

  private topologicalSort(
    tables: LegacyTable[],
    dependencies: Map<string, Set<string>>
  ): LegacyTable[] {
    const result: LegacyTable[] = [];
    const visited = new Set<string>();
    const visiting = new Set<string>();

    const visit = (tableName: string): void => {
      if (visited.has(tableName)) return;
      if (visiting.has(tableName)) {
        // Circular dependency - just add it
        return;
      }

      visiting.add(tableName);
      const deps = dependencies.get(tableName) || new Set();
      for (const dep of deps) {
        visit(dep);
      }
      visiting.delete(tableName);
      visited.add(tableName);

      const table = tables.find(t => t.name === tableName);
      if (table) result.push(table);
    };

    for (const table of tables) {
      visit(table.name);
    }

    return result;
  }

  /**
   * Generate migration strategy
   */
  private generateStrategy(
    tableMappings: TableMigrationPlan[],
    relationships: InferredRelationship[]
  ): MigrationStrategy {
    // Determine approach based on relationships and table count
    const hasCircularDeps = this.detectCircularDependencies(relationships);
    const approach = hasCircularDeps ? 'incremental' : 'big-bang';

    // Create phases
    const phases = this.createPhases(tableMappings, relationships);

    return {
      approach,
      phases,
      rollbackPlan: {
        strategy: 'full-restore',
        maxRollbackTime: 60,
        procedures: phases.map(phase => ({
          phase: phase.id,
          steps: [
            `Truncate tables: ${phase.tables.join(', ')}`,
            'Restore from backup',
            'Verify row counts',
          ],
          estimatedTime: 15,
        })),
      },
      dataValidation: {
        preValidation: [],
        postValidation: [],
        continuousValidation: [],
      },
    };
  }

  private detectCircularDependencies(relationships: InferredRelationship[]): boolean {
    const graph = new Map<string, Set<string>>();
    
    for (const rel of relationships) {
      if (!graph.has(rel.sourceTable)) {
        graph.set(rel.sourceTable, new Set());
      }
      graph.get(rel.sourceTable)!.add(rel.targetTable);
    }

    const visited = new Set<string>();
    const recursionStack = new Set<string>();

    const hasCycle = (node: string): boolean => {
      if (recursionStack.has(node)) return true;
      if (visited.has(node)) return false;

      visited.add(node);
      recursionStack.add(node);

      for (const neighbor of graph.get(node) || []) {
        if (hasCycle(neighbor)) return true;
      }

      recursionStack.delete(node);
      return false;
    };

    for (const node of graph.keys()) {
      if (hasCycle(node)) return true;
    }

    return false;
  }

  private createPhases(
    tableMappings: TableMigrationPlan[],
    _relationships: InferredRelationship[]
  ): MigrationPhase[] {
    const phases: MigrationPhase[] = [];
    const processed = new Set<string>();

    // Group tables by dependency level
    let phaseNum = 1;
    let remaining = [...tableMappings];

    while (remaining.length > 0) {
      const canProcess = remaining.filter(tm => 
        tm.dependencies.every(dep => processed.has(dep) || !remaining.some(r => r.sourceTable === dep))
      );

      if (canProcess.length === 0) {
        // Force process remaining (circular deps)
        canProcess.push(...remaining);
      }

      const phaseTables = canProcess.map(tm => tm.sourceTable);
      phases.push({
        id: `phase_${phaseNum}`,
        name: `Phase ${phaseNum}: ${phaseTables.length} tables`,
        description: `Migrate tables: ${phaseTables.slice(0, 5).join(', ')}${phaseTables.length > 5 ? '...' : ''}`,
        tables: phaseTables,
        dependencies: phaseNum > 1 ? [`phase_${phaseNum - 1}`] : [],
        estimatedDuration: canProcess.reduce((sum, _tm) => sum + 5, 0),
        checkpoints: [
          { name: 'Row count validation', type: 'row-count' },
        ],
      });

      for (const table of phaseTables) {
        processed.add(table);
      }
      remaining = remaining.filter(tm => !phaseTables.includes(tm.sourceTable));
      phaseNum++;
    }

    return phases;
  }

  /**
   * Generate validation queries
   */
  private generateValidationQueries(
    source: LegacySchema,
    _target: TargetSchema,
    tableMappings: TableMigrationPlan[]
  ): ValidationQuery[] {
    const queries: ValidationQuery[] = [];

    for (const mapping of tableMappings) {
      // Row count validation
      queries.push({
        id: `rowcount_${mapping.sourceTable}`,
        name: `Row Count - ${mapping.sourceTable}`,
        description: `Validate row counts match between source and target`,
        type: 'row-count',
        sourceQuery: `SELECT COUNT(*) FROM ${mapping.sourceTable}`,
        targetQuery: `SELECT COUNT(*) FROM ${mapping.targetTable}`,
        tolerance: 0,
        critical: true,
      });

      // Checksum validation for key columns
      const sourceTable = source.tables.find(t => t.name === mapping.sourceTable);
      if (sourceTable?.primaryKey && sourceTable.primaryKey.length > 0) {
        const pkCols = sourceTable.primaryKey.join(', ');
        queries.push({
          id: `checksum_${mapping.sourceTable}`,
          name: `Checksum - ${mapping.sourceTable}`,
          description: `Validate data integrity via checksum`,
          type: 'checksum',
          sourceQuery: `SELECT SUM(HASH(${pkCols})) FROM ${mapping.sourceTable}`,
          targetQuery: `SELECT SUM(HASH(${pkCols})) FROM ${mapping.targetTable}`,
          tolerance: 0,
          critical: true,
        });
      }
    }

    return queries;
  }

  /**
   * Generate DDL for target schema
   */
  generateDDL(target: TargetSchema): string {
    const statements: string[] = [];

    // Create schema if specified
    if (this.config.schemaName) {
      statements.push(`CREATE SCHEMA IF NOT EXISTS ${this.config.schemaName};`);
      statements.push('');
    }

    // Create tables
    for (const table of target.tables) {
      statements.push(this.generateTableDDL(table));
      statements.push('');
    }

    // Create foreign key constraints
    for (const table of target.tables) {
      for (const fk of table.foreignKeys) {
        statements.push(this.generateForeignKeyDDL(table.name, fk));
      }
    }

    // Create indexes
    for (const table of target.tables) {
      for (const idx of table.indexes) {
        statements.push(this.generateIndexDDL(table.name, idx));
      }
    }

    return statements.join('\n');
  }

  private generateTableDDL(table: TargetTable): string {
    const schema = table.schema ? `${table.schema}.` : '';
    const columns = table.columns.map(col => {
      let def = `  ${col.name} ${col.dataType}`;
      if (!col.nullable) def += ' NOT NULL';
      if (col.defaultValue) def += ` DEFAULT ${col.defaultValue}`;
      if (col.generated?.type === 'identity') {
        def += ' GENERATED ALWAYS AS IDENTITY';
      }
      return def;
    });

    const pk = table.primaryKey.length > 0
      ? `  PRIMARY KEY (${table.primaryKey.join(', ')})`
      : null;

    const allDefs = pk ? [...columns, pk] : columns;

    return `CREATE TABLE ${schema}${table.name} (\n${allDefs.join(',\n')}\n);`;
  }

  private generateForeignKeyDDL(
    tableName: string,
    fk: { name: string; columns: string[]; referencedTable: string; referencedColumns: string[] }
  ): string {
    return `ALTER TABLE ${tableName} ADD CONSTRAINT ${fk.name} ` +
      `FOREIGN KEY (${fk.columns.join(', ')}) ` +
      `REFERENCES ${fk.referencedTable} (${fk.referencedColumns.join(', ')});`;
  }

  private generateIndexDDL(
    tableName: string,
    idx: { name: string; columns: string[]; unique: boolean }
  ): string {
    const unique = idx.unique ? 'UNIQUE ' : '';
    return `CREATE ${unique}INDEX ${idx.name} ON ${tableName} (${idx.columns.join(', ')});`;
  }
}

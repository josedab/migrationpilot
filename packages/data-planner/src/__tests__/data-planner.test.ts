/**
 * Tests for Data Migration Planner
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  DataMigrationPlanner,
  DB2SchemaExtractor,
  COPYBOOKSchemaExtractor,
  RelationshipAnalyzer,
  MigrationPlanGenerator,
} from '../index.js';
import type { LegacySchema, LegacyTable } from '../types.js';

describe('COPYBOOKSchemaExtractor', () => {
  const extractor = new COPYBOOKSchemaExtractor();

  it('should parse simple copybook', () => {
    const copybook = `
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID        PIC 9(10).
         05 CUSTOMER-NAME      PIC X(50).
         05 CUSTOMER-BALANCE   PIC S9(9)V99 COMP-3.
    `;

    const result = extractor.extract(copybook, 'CUSTOMER');
    
    expect(result.name).toBe('CUSTOMER');
    expect(result.fields).toHaveLength(4); // Including the 01 level
    expect(result.fields[1].name).toBe('customer_id');
    expect(result.fields[1].mappedType).toBe('integer');
    expect(result.fields[2].name).toBe('customer_name');
    expect(result.fields[2].mappedType).toBe('string');
    expect(result.fields[3].mappedType).toBe('decimal');
  });

  it('should handle OCCURS clause', () => {
    const copybook = `
      01 ORDER-RECORD.
         05 ORDER-ID           PIC 9(10).
         05 LINE-ITEMS OCCURS 10 TIMES.
            10 ITEM-CODE       PIC X(20).
            10 QUANTITY        PIC 9(5).
    `;

    const result = extractor.extract(copybook, 'ORDER');
    
    expect(result.fields.some(f => f.occurs?.max === 10)).toBe(true);
  });

  it('should calculate correct record length', () => {
    const copybook = `
      01 SIMPLE-RECORD.
         05 FIELD-A    PIC X(10).
         05 FIELD-B    PIC 9(5).
    `;

    const result = extractor.extract(copybook, 'SIMPLE');
    
    expect(result.recordLength).toBe(15);
  });
});

describe('RelationshipAnalyzer', () => {
  const analyzer = new RelationshipAnalyzer({ minConfidenceThreshold: 0.3 });

  const mockSchema: LegacySchema = {
    sourceId: 'test',
    sourceType: 'db2',
    tables: [
      {
        name: 'customers',
        type: 'table',
        columns: [
          { name: 'id', dataType: 'integer', nativeType: 'INTEGER', nullable: false },
          { name: 'name', dataType: 'varchar(100)', nativeType: 'VARCHAR', nullable: false },
        ],
        primaryKey: ['id'],
        foreignKeys: [],
        indexes: [],
      },
      {
        name: 'orders',
        type: 'table',
        columns: [
          { name: 'id', dataType: 'integer', nativeType: 'INTEGER', nullable: false },
          { name: 'customer_id', dataType: 'integer', nativeType: 'INTEGER', nullable: false },
          { name: 'amount', dataType: 'decimal(10,2)', nativeType: 'DECIMAL', nullable: false },
        ],
        primaryKey: ['id'],
        foreignKeys: [],
        indexes: [],
      },
    ],
    extractedAt: new Date(),
  };

  it('should infer relationship from naming convention', () => {
    const relationships = analyzer.analyzeRelationships(mockSchema);
    
    expect(relationships.length).toBeGreaterThan(0);
    const custRel = relationships.find(r => 
      r.sourceTable === 'orders' && r.targetTable === 'customers'
    );
    expect(custRel).toBeDefined();
    expect(custRel?.sourceColumns).toContain('customer_id');
  });

  it('should assign confidence based on evidence', () => {
    const relationships = analyzer.analyzeRelationships(mockSchema);
    
    for (const rel of relationships) {
      expect(rel.confidence).toBeGreaterThan(0);
      expect(rel.confidence).toBeLessThanOrEqual(1);
      expect(rel.evidence.length).toBeGreaterThan(0);
    }
  });
});

describe('MigrationPlanGenerator', () => {
  const generator = new MigrationPlanGenerator({
    targetDatabase: 'postgresql',
    namingConvention: 'snake_case',
    preserveNullability: true,
    addAuditColumns: true,
    generateIndexes: true,
    batchSize: 10000,
    parallelism: 4,
  });

  const mockSchema: LegacySchema = {
    sourceId: 'test',
    sourceType: 'db2',
    tables: [
      {
        name: 'CUSTOMER',
        type: 'table',
        columns: [
          { name: 'CUST_ID', dataType: 'integer', nativeType: 'INTEGER', nullable: false },
          { name: 'CUST_NAME', dataType: 'varchar(100)', nativeType: 'VARCHAR', nullable: false },
          { name: 'CREATE_DT', dataType: 'date', nativeType: 'DATE', nullable: true },
        ],
        primaryKey: ['CUST_ID'],
        foreignKeys: [],
        indexes: [],
      },
    ],
    extractedAt: new Date(),
  };

  it('should generate target schema with snake_case names', () => {
    const target = generator.generateTargetSchema(mockSchema, []);
    
    expect(target.tables[0].name).toBe('customer');
    expect(target.tables[0].columns.some(c => c.name === 'cust_id')).toBe(true);
    expect(target.tables[0].columns.some(c => c.name === 'cust_name')).toBe(true);
  });

  it('should add audit columns when configured', () => {
    const target = generator.generateTargetSchema(mockSchema, []);
    
    expect(target.tables[0].columns.some(c => c.name === 'created_at')).toBe(true);
    expect(target.tables[0].columns.some(c => c.name === 'updated_at')).toBe(true);
  });

  it('should generate DDL', () => {
    const target = generator.generateTargetSchema(mockSchema, []);
    const ddl = generator.generateDDL(target);
    
    expect(ddl).toContain('CREATE TABLE');
    expect(ddl).toContain('customer');
    expect(ddl).toContain('PRIMARY KEY');
  });
});

describe('DataMigrationPlanner', () => {
  const planner = new DataMigrationPlanner({
    targetDatabase: 'postgresql',
    namingConvention: 'snake_case',
  });

  it('should generate effort report', () => {
    const estimates = {
      totalRows: 1000000,
      totalSizeGB: 5.5,
      estimatedDurationMinutes: 120,
      complexityScore: 5,
      riskScore: 3,
      tableEstimates: [
        {
          table: 'customers',
          rows: 500000,
          sizeGB: 2.5,
          complexity: 'medium' as const,
          estimatedMinutes: 60,
          risks: ['High null rate in email column'],
        },
        {
          table: 'orders',
          rows: 500000,
          sizeGB: 3.0,
          complexity: 'low' as const,
          estimatedMinutes: 60,
          risks: [],
        },
      ],
    };

    const report = planner.generateEffortReport(estimates);
    
    expect(report).toContain('Data Migration Effort Estimate');
    expect(report).toContain('1,000,000');
    expect(report).toContain('5.50 GB');
  });
});

describe('DB2SchemaExtractor', () => {
  it('should generate catalog queries', () => {
    const extractor = new DB2SchemaExtractor({
      connection: { database: 'TESTDB' },
      schemas: ['SCHEMA1', 'SCHEMA2'],
    });

    const queries = extractor.getCatalogQueries();
    
    expect(queries.tables).toContain('SYSCAT.TABLES');
    expect(queries.columns).toContain('SYSCAT.COLUMNS');
    expect(queries.primaryKeys).toContain('SYSCAT.KEYCOLUSE');
    expect(queries.foreignKeys).toContain('SYSCAT.REFERENCES');
  });
});

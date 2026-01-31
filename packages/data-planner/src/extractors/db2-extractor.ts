/**
 * DB2 Schema Extractor
 * 
 * Extracts schema information from DB2 catalog tables.
 */

import type {
  LegacySchema,
  LegacyTable,
  ForeignKeyDefinition,
  IndexDefinition,
  ConnectionInfo,
} from '../types.js';

export interface DB2ExtractorConfig {
  connection: ConnectionInfo;
  schemas?: string[];
  includeTables?: string[];
  excludeTables?: string[];
  includeViews?: boolean;
  includeSynonyms?: boolean;
}

export interface DB2CatalogQueries {
  tables: string;
  columns: string;
  primaryKeys: string;
  foreignKeys: string;
  indexes: string;
  statistics: string;
}

export class DB2SchemaExtractor {
  private config: DB2ExtractorConfig;

  constructor(config: DB2ExtractorConfig) {
    this.config = config;
  }

  /**
   * Generate SQL queries to extract schema from DB2 catalog
   */
  getCatalogQueries(): DB2CatalogQueries {
    const schemaFilter = this.config.schemas?.length
      ? `TABSCHEMA IN (${this.config.schemas.map(s => `'${s}'`).join(', ')})`
      : '1=1';

    return {
      tables: `
        SELECT 
          TABSCHEMA,
          TABNAME,
          TYPE,
          STATUS,
          REMARKS,
          CARD AS ROW_COUNT,
          NPAGES * 4096 AS SIZE_BYTES
        FROM SYSCAT.TABLES
        WHERE ${schemaFilter}
          AND TYPE IN ('T', 'V'${this.config.includeSynonyms ? ", 'A'" : ''})
        ORDER BY TABSCHEMA, TABNAME
      `,

      columns: `
        SELECT 
          TABSCHEMA,
          TABNAME,
          COLNAME,
          COLNO,
          TYPENAME,
          LENGTH,
          SCALE,
          NULLS,
          DEFAULT,
          GENERATED,
          TEXT AS COMPUTED_EXPR,
          REMARKS
        FROM SYSCAT.COLUMNS
        WHERE ${schemaFilter}
        ORDER BY TABSCHEMA, TABNAME, COLNO
      `,

      primaryKeys: `
        SELECT 
          TABSCHEMA,
          TABNAME,
          CONSTNAME,
          COLNAME,
          COLSEQ
        FROM SYSCAT.KEYCOLUSE K
        JOIN SYSCAT.TABCONST C 
          ON K.CONSTNAME = C.CONSTNAME AND K.TABSCHEMA = C.TABSCHEMA
        WHERE C.TYPE = 'P' AND ${schemaFilter.replace('TABSCHEMA', 'K.TABSCHEMA')}
        ORDER BY TABSCHEMA, TABNAME, COLSEQ
      `,

      foreignKeys: `
        SELECT 
          R.TABSCHEMA,
          R.TABNAME,
          R.CONSTNAME,
          R.REFTABSCHEMA,
          R.REFTABNAME,
          R.DELETERULE,
          R.UPDATERULE,
          FK.COLNAME AS FK_COLUMN,
          FK.COLSEQ,
          PK.COLNAME AS PK_COLUMN
        FROM SYSCAT.REFERENCES R
        JOIN SYSCAT.KEYCOLUSE FK 
          ON R.CONSTNAME = FK.CONSTNAME AND R.TABSCHEMA = FK.TABSCHEMA
        JOIN SYSCAT.KEYCOLUSE PK 
          ON R.REFKEYNAME = PK.CONSTNAME AND R.REFTABSCHEMA = PK.TABSCHEMA
          AND FK.COLSEQ = PK.COLSEQ
        WHERE ${schemaFilter.replace('TABSCHEMA', 'R.TABSCHEMA')}
        ORDER BY R.TABSCHEMA, R.TABNAME, R.CONSTNAME, FK.COLSEQ
      `,

      indexes: `
        SELECT 
          I.TABSCHEMA,
          I.TABNAME,
          I.INDNAME,
          I.UNIQUERULE,
          I.INDEXTYPE,
          I.CLUSTERING,
          IC.COLNAME,
          IC.COLSEQ
        FROM SYSCAT.INDEXES I
        JOIN SYSCAT.INDEXCOLUSE IC 
          ON I.INDSCHEMA = IC.INDSCHEMA AND I.INDNAME = IC.INDNAME
        WHERE ${schemaFilter.replace('TABSCHEMA', 'I.TABSCHEMA')}
        ORDER BY I.TABSCHEMA, I.TABNAME, I.INDNAME, IC.COLSEQ
      `,

      statistics: `
        SELECT 
          TABSCHEMA,
          TABNAME,
          CARD,
          NPAGES,
          FPAGES,
          OVERFLOW,
          STATS_TIME
        FROM SYSCAT.TABLES
        WHERE ${schemaFilter} AND TYPE = 'T'
      `,
    };
  }

  /**
   * Parse DB2 catalog query results into schema structure
   */
  parseResults(results: {
    tables: DB2TableRow[];
    columns: DB2ColumnRow[];
    primaryKeys: DB2PKRow[];
    foreignKeys: DB2FKRow[];
    indexes: DB2IndexRow[];
  }): LegacySchema {
    const tableMap = new Map<string, LegacyTable>();

    // Build tables
    for (const row of results.tables) {
      const key = `${row.TABSCHEMA}.${row.TABNAME}`;
      tableMap.set(key, {
        name: row.TABNAME,
        schema: row.TABSCHEMA,
        type: this.mapTableType(row.TYPE),
        columns: [],
        primaryKey: undefined,
        foreignKeys: [],
        indexes: [],
        rowCount: row.ROW_COUNT,
        sizeBytes: row.SIZE_BYTES,
        metadata: { remarks: row.REMARKS, status: row.STATUS },
      });
    }

    // Add columns
    for (const row of results.columns) {
      const key = `${row.TABSCHEMA}.${row.TABNAME}`;
      const table = tableMap.get(key);
      if (table) {
        table.columns.push({
          name: row.COLNAME,
          dataType: this.mapDB2Type(row.TYPENAME, row.LENGTH, row.SCALE),
          nativeType: row.TYPENAME,
          length: row.LENGTH,
          scale: row.SCALE,
          nullable: row.NULLS === 'Y',
          defaultValue: row.DEFAULT,
          computed: row.GENERATED !== ' ',
          computeExpression: row.COMPUTED_EXPR,
          description: row.REMARKS,
        });
      }
    }

    // Add primary keys
    const pkMap = new Map<string, string[]>();
    for (const row of results.primaryKeys) {
      const key = `${row.TABSCHEMA}.${row.TABNAME}`;
      if (!pkMap.has(key)) {
        pkMap.set(key, []);
      }
      pkMap.get(key)![row.COLSEQ - 1] = row.COLNAME;
    }
    for (const [key, columns] of pkMap) {
      const table = tableMap.get(key);
      if (table) {
        table.primaryKey = columns.filter(Boolean);
      }
    }

    // Add foreign keys
    const fkMap = new Map<string, Map<string, ForeignKeyDefinition>>();
    for (const row of results.foreignKeys) {
      const tableKey = `${row.TABSCHEMA}.${row.TABNAME}`;
      if (!fkMap.has(tableKey)) {
        fkMap.set(tableKey, new Map());
      }
      const tableFks = fkMap.get(tableKey)!;
      
      if (!tableFks.has(row.CONSTNAME)) {
        tableFks.set(row.CONSTNAME, {
          name: row.CONSTNAME,
          columns: [],
          referencedTable: `${row.REFTABSCHEMA}.${row.REFTABNAME}`,
          referencedColumns: [],
          onDelete: this.mapDeleteRule(row.DELETERULE),
          onUpdate: this.mapDeleteRule(row.UPDATERULE),
        });
      }
      
      const fk = tableFks.get(row.CONSTNAME)!;
      fk.columns[row.COLSEQ - 1] = row.FK_COLUMN;
      fk.referencedColumns[row.COLSEQ - 1] = row.PK_COLUMN;
    }
    
    for (const [tableKey, fks] of fkMap) {
      const table = tableMap.get(tableKey);
      if (table) {
        table.foreignKeys = Array.from(fks.values()).map(fk => ({
          ...fk,
          columns: fk.columns.filter(Boolean),
          referencedColumns: fk.referencedColumns.filter(Boolean),
        }));
      }
    }

    // Add indexes
    const indexMap = new Map<string, Map<string, IndexDefinition>>();
    for (const row of results.indexes) {
      const tableKey = `${row.TABSCHEMA}.${row.TABNAME}`;
      if (!indexMap.has(tableKey)) {
        indexMap.set(tableKey, new Map());
      }
      const tableIndexes = indexMap.get(tableKey)!;
      
      if (!tableIndexes.has(row.INDNAME)) {
        tableIndexes.set(row.INDNAME, {
          name: row.INDNAME,
          columns: [],
          unique: row.UNIQUERULE === 'U' || row.UNIQUERULE === 'P',
          type: row.CLUSTERING === 'Y' ? 'clustered' : 'btree',
        });
      }
      
      tableIndexes.get(row.INDNAME)!.columns[row.COLSEQ - 1] = row.COLNAME;
    }
    
    for (const [tableKey, indexes] of indexMap) {
      const table = tableMap.get(tableKey);
      if (table) {
        table.indexes = Array.from(indexes.values()).map(idx => ({
          ...idx,
          columns: idx.columns.filter(Boolean),
        }));
      }
    }

    return {
      sourceId: this.config.connection.database || 'db2',
      sourceType: 'db2',
      tables: Array.from(tableMap.values()),
      extractedAt: new Date(),
    };
  }

  private mapTableType(type: string): 'table' | 'view' | 'synonym' | 'alias' {
    switch (type) {
      case 'T': return 'table';
      case 'V': return 'view';
      case 'A': return 'alias';
      default: return 'table';
    }
  }

  private mapDB2Type(typename: string, length?: number, scale?: number): string {
    const baseType = typename.trim().toUpperCase();
    switch (baseType) {
      case 'INTEGER':
      case 'INT':
        return 'integer';
      case 'SMALLINT':
        return 'smallint';
      case 'BIGINT':
        return 'bigint';
      case 'DECIMAL':
      case 'NUMERIC':
        return `decimal(${length || 18}, ${scale || 0})`;
      case 'REAL':
      case 'FLOAT':
        return 'float';
      case 'DOUBLE':
        return 'double';
      case 'CHAR':
      case 'CHARACTER':
        return `char(${length || 1})`;
      case 'VARCHAR':
        return `varchar(${length || 255})`;
      case 'CLOB':
        return 'text';
      case 'BLOB':
        return 'bytea';
      case 'DATE':
        return 'date';
      case 'TIME':
        return 'time';
      case 'TIMESTAMP':
        return 'timestamp';
      default:
        return baseType.toLowerCase();
    }
  }

  private mapDeleteRule(rule: string): 'cascade' | 'set-null' | 'restrict' | 'no-action' {
    switch (rule) {
      case 'C': return 'cascade';
      case 'N': return 'set-null';
      case 'R': return 'restrict';
      default: return 'no-action';
    }
  }
}

// Row types from DB2 catalog
interface DB2TableRow {
  TABSCHEMA: string;
  TABNAME: string;
  TYPE: string;
  STATUS: string;
  REMARKS?: string;
  ROW_COUNT?: number;
  SIZE_BYTES?: number;
}

interface DB2ColumnRow {
  TABSCHEMA: string;
  TABNAME: string;
  COLNAME: string;
  COLNO: number;
  TYPENAME: string;
  LENGTH?: number;
  SCALE?: number;
  NULLS: string;
  DEFAULT?: string;
  GENERATED: string;
  COMPUTED_EXPR?: string;
  REMARKS?: string;
}

interface DB2PKRow {
  TABSCHEMA: string;
  TABNAME: string;
  CONSTNAME: string;
  COLNAME: string;
  COLSEQ: number;
}

interface DB2FKRow {
  TABSCHEMA: string;
  TABNAME: string;
  CONSTNAME: string;
  REFTABSCHEMA: string;
  REFTABNAME: string;
  DELETERULE: string;
  UPDATERULE: string;
  FK_COLUMN: string;
  COLSEQ: number;
  PK_COLUMN: string;
}

interface DB2IndexRow {
  TABSCHEMA: string;
  TABNAME: string;
  INDNAME: string;
  UNIQUERULE: string;
  INDEXTYPE: string;
  CLUSTERING: string;
  COLNAME: string;
  COLSEQ: number;
}

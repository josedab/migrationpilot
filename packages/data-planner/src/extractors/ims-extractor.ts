/**
 * IMS Schema Extractor
 * 
 * Extracts schema information from IMS DBD (Database Descriptor) 
 * and PSB (Program Specification Block) definitions.
 */

import type {
  LegacySchema,
  IMSSegment,
  IMSField,
} from '../types.js';

export interface IMSExtractorConfig {
  dbdSource?: string;
  psbSource?: string;
  copybookSources?: Map<string, string>;
}

interface ParsedDBD {
  name: string;
  access: string;
  segments: IMSSegment[];
}

export class IMSSchemaExtractor {
  constructor(_config: IMSExtractorConfig) {
    // Config stored for future use
  }

  /**
   * Extract IMS schema from DBD source
   */
  extractFromDBD(dbdContent: string): LegacySchema {
    const dbd = this.parseDBD(dbdContent);
    
    return {
      sourceId: dbd.name,
      sourceType: 'ims',
      tables: [],
      segments: dbd.segments,
      extractedAt: new Date(),
    };
  }

  private parseDBD(content: string): ParsedDBD {
    const lines = content.split('\n').map(l => l.trim()).filter(l => l && !l.startsWith('*'));
    
    let dbdName = '';
    let accessMethod = '';
    const segments: IMSSegment[] = [];
    let currentSegment: IMSSegment | null = null;
    
    for (const line of lines) {
      const dbdMatch = line.match(/DBD\s+NAME=(\w+)/i);
      if (dbdMatch && dbdMatch[1]) {
        dbdName = dbdMatch[1];
        const accessMatch = line.match(/ACCESS=(\w+)/i);
        if (accessMatch && accessMatch[1]) {
          accessMethod = accessMatch[1];
        }
        continue;
      }

      const segmMatch = line.match(/SEGM\s+NAME=(\w+)/i);
      if (segmMatch && segmMatch[1]) {
        if (currentSegment) {
          segments.push(currentSegment);
        }
        
        const parentMatch = line.match(/PARENT=(?:\((\w+)\)|(\w+))/i);
        
        currentSegment = {
          name: segmMatch[1],
          parentSegment: parentMatch ? (parentMatch[1] || parentMatch[2]) : undefined,
          fields: [],
          sequenceField: this.extractSequenceField(line),
        };
        continue;
      }

      const fieldMatch = line.match(/FIELD\s+NAME=\((\w+),SEQ(?:,U)?\)/i) ||
                        line.match(/FIELD\s+NAME=(\w+)/i);
      if (fieldMatch && fieldMatch[1] && currentSegment) {
        const startMatch = line.match(/START=(\d+)/i);
        const bytesMatch = line.match(/BYTES=(\d+)/i);
        const typeMatch = line.match(/TYPE=(C|P|X|F|H)/i);
        
        currentSegment.fields.push({
          name: fieldMatch[1],
          startPosition: startMatch && startMatch[1] ? parseInt(startMatch[1], 10) : 1,
          length: bytesMatch && bytesMatch[1] ? parseInt(bytesMatch[1], 10) : 0,
          dataType: this.mapIMSType(typeMatch?.[1]),
          key: line.includes(',SEQ') || line.includes(',U'),
        });
      }
    }

    if (currentSegment) {
      segments.push(currentSegment);
    }

    // Rebuild hierarchy
    this.buildHierarchy(segments);

    return {
      name: dbdName,
      access: accessMethod,
      segments,
    };
  }

  private extractSequenceField(line: string): string | undefined {
    const seqMatch = line.match(/FIELD\s+NAME=\((\w+),SEQ/i);
    return seqMatch ? seqMatch[1] : undefined;
  }

  private mapIMSType(type?: string): 'char' | 'packed' | 'zoned' | 'binary' | 'float' {
    switch (type?.toUpperCase()) {
      case 'P': return 'packed';
      case 'X': return 'binary';
      case 'F': return 'float';
      case 'H': return 'binary';
      default: return 'char';
    }
  }

  private buildHierarchy(segments: IMSSegment[]): void {
    const segmentMap = new Map(segments.map(s => [s.name, s]));
    
    for (const segment of segments) {
      if (segment.parentSegment) {
        const parent = segmentMap.get(segment.parentSegment);
        if (parent && !segment.pointerType) {
          segment.pointerType = 'hierarchical';
        }
      }
    }
  }

  /**
   * Convert IMS segments to relational tables for migration planning
   */
  convertToRelational(schema: LegacySchema): LegacySchema {
    if (!schema.segments) {
      return schema;
    }

    const tables = schema.segments.map(segment => ({
      name: segment.name.toLowerCase(),
      type: 'table' as const,
      columns: [
        // Add synthetic key for root segments
        ...(segment.parentSegment ? [] : [{
          name: `${segment.name.toLowerCase()}_id`,
          dataType: 'bigint',
          nativeType: 'SEQUENCE',
          nullable: false,
        }]),
        // Add foreign key to parent
        ...(segment.parentSegment ? [{
          name: `${segment.parentSegment.toLowerCase()}_id`,
          dataType: 'bigint',
          nativeType: 'FK',
          nullable: false,
        }] : []),
        // Map segment fields
        ...segment.fields.map(field => ({
          name: field.name.toLowerCase(),
          dataType: this.mapIMSToSQLType(field),
          nativeType: `IMS_${field.dataType.toUpperCase()}`,
          length: field.length,
          nullable: !field.key,
        })),
      ],
      primaryKey: segment.parentSegment 
        ? undefined 
        : [`${segment.name.toLowerCase()}_id`],
      foreignKeys: segment.parentSegment 
        ? [{
            name: `fk_${segment.name.toLowerCase()}_${segment.parentSegment.toLowerCase()}`,
            columns: [`${segment.parentSegment.toLowerCase()}_id`],
            referencedTable: segment.parentSegment.toLowerCase(),
            referencedColumns: [`${segment.parentSegment.toLowerCase()}_id`],
          }]
        : [],
      indexes: segment.fields
        .filter(f => f.key)
        .map(f => ({
          name: `idx_${segment.name.toLowerCase()}_${f.name.toLowerCase()}`,
          columns: [f.name.toLowerCase()],
          unique: true,
          type: 'btree' as const,
        })),
    }));

    return {
      ...schema,
      tables,
    };
  }

  private mapIMSToSQLType(field: IMSField): string {
    switch (field.dataType) {
      case 'packed':
        return `decimal(${Math.floor(field.length * 2) - 1}, ${field.scale || 0})`;
      case 'zoned':
        return `decimal(${field.length}, ${field.scale || 0})`;
      case 'binary':
        return field.length <= 4 ? 'integer' : 'bigint';
      case 'float':
        return 'double precision';
      default:
        return `varchar(${field.length})`;
    }
  }
}

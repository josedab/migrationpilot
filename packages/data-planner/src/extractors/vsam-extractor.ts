/**
 * VSAM Schema Extractor
 * 
 * Extracts schema information from VSAM cluster definitions 
 * and associated COBOL COPYBOOK structures.
 */

import type {
  LegacySchema,
  VSAMCluster,
  CopybookDefinition,
} from '../types.js';

export interface VSAMExtractorConfig {
  clusterDefinitions?: string[];
  copybookSources?: Map<string, string>;
}

export class VSAMSchemaExtractor {
  constructor(_config: VSAMExtractorConfig) {
    // Config stored for future use
  }

  /**
   * Extract VSAM schema from IDCAMS DEFINE CLUSTER output
   */
  extractFromIDCAMS(idcamsOutput: string): VSAMCluster[] {
    const clusters: VSAMCluster[] = [];
    const clusterBlocks = this.splitClusterDefinitions(idcamsOutput);

    for (const block of clusterBlocks) {
      const cluster = this.parseClusterDefinition(block);
      if (cluster) {
        clusters.push(cluster);
      }
    }

    return clusters;
  }

  private splitClusterDefinitions(output: string): string[] {
    const blocks: string[] = [];
    const lines = output.split('\n');
    let currentBlock: string[] = [];

    for (const line of lines) {
      if (line.includes('DEFINE CLUSTER') || line.includes('CLUSTER -')) {
        if (currentBlock.length > 0) {
          blocks.push(currentBlock.join('\n'));
        }
        currentBlock = [line];
      } else if (currentBlock.length > 0) {
        currentBlock.push(line);
      }
    }

    if (currentBlock.length > 0) {
      blocks.push(currentBlock.join('\n'));
    }

    return blocks;
  }

  private parseClusterDefinition(block: string): VSAMCluster | null {
    const nameMatch = block.match(/NAME\s*\(\s*(\S+)\s*\)/i) ||
                     block.match(/CLUSTER\s+-?\s*\(\s*NAME\s*\(\s*(\S+)/i);
    
    if (!nameMatch || !nameMatch[1]) return null;

    const dataMatch = block.match(/DATA\s*-?\s*\(\s*NAME\s*\(\s*(\S+)/i);
    const indexMatch = block.match(/INDEX\s*-?\s*\(\s*NAME\s*\(\s*(\S+)/i);
    
    const recordSizeMatch = block.match(/RECORDSIZE\s*\(\s*(\d+)\s+(\d+)\s*\)/i) ||
                           block.match(/RECSZ\s*\(\s*(\d+)\s+(\d+)\s*\)/i);
    
    const keyMatch = block.match(/KEYS\s*\(\s*(\d+)\s+(\d+)\s*\)/i);
    const ciSizeMatch = block.match(/CISZ\s*\(\s*(\d+)\s*\)/i) ||
                       block.match(/CONTROLINTERVALSIZE\s*\(\s*(\d+)\s*\)/i);
    const freespaceMatch = block.match(/FREESPACE\s*\(\s*(\d+)\s+(\d+)\s*\)/i);

    const type = this.determineClusterType(block);
    const recMin = recordSizeMatch?.[1] ? parseInt(recordSizeMatch[1], 10) : 0;
    const recMax = recordSizeMatch?.[2] ? parseInt(recordSizeMatch[2], 10) : 0;
    const avgRecSize = recordSizeMatch ? Math.floor((recMin + recMax) / 2) : 0;

    return {
      name: nameMatch[1],
      type,
      dataComponent: dataMatch?.[1] || `${nameMatch[1]}.DATA`,
      indexComponent: type === 'ksds' ? (indexMatch?.[1] || `${nameMatch[1]}.INDEX`) : undefined,
      recordSize: {
        min: recMin,
        max: recMax,
        average: avgRecSize,
      },
      keyPosition: keyMatch?.[2] ? parseInt(keyMatch[2], 10) : undefined,
      keyLength: keyMatch?.[1] ? parseInt(keyMatch[1], 10) : undefined,
      ciSize: ciSizeMatch?.[1] ? parseInt(ciSizeMatch[1], 10) : 4096,
      freespace: freespaceMatch?.[1] && freespaceMatch?.[2] ? {
        ci: parseInt(freespaceMatch[1], 10),
        ca: parseInt(freespaceMatch[2], 10),
      } : undefined,
    };
  }

  private determineClusterType(block: string): 'ksds' | 'esds' | 'rrds' | 'lds' {
    const upperBlock = block.toUpperCase();
    if (upperBlock.includes('INDEXED') || upperBlock.includes('KEYS(')) {
      return 'ksds';
    }
    if (upperBlock.includes('NONINDEXED') || upperBlock.includes('ESDS')) {
      return 'esds';
    }
    if (upperBlock.includes('NUMBERED') || upperBlock.includes('RRDS')) {
      return 'rrds';
    }
    if (upperBlock.includes('LINEAR') || upperBlock.includes('LDS')) {
      return 'lds';
    }
    return 'ksds'; // Default
  }

  /**
   * Build complete schema from VSAM clusters and copybooks
   */
  buildSchema(
    clusters: VSAMCluster[], 
    copybooks?: Map<string, CopybookDefinition>
  ): LegacySchema {
    // Link copybooks to clusters
    for (const cluster of clusters) {
      if (copybooks && cluster.copybook) {
        const cb = copybooks.get(cluster.copybook);
        if (cb) {
          cluster.copybook = cb.name;
        }
      }
    }

    // Convert to relational tables
    const tables = this.convertClustersToTables(clusters, copybooks);

    return {
      sourceId: 'vsam',
      sourceType: 'vsam',
      tables,
      clusters,
      copybooks: copybooks ? Array.from(copybooks.values()) : undefined,
      extractedAt: new Date(),
    };
  }

  private convertClustersToTables(
    clusters: VSAMCluster[],
    copybooks?: Map<string, CopybookDefinition>
  ): { name: string; type: 'table'; columns: { name: string; dataType: string; nativeType: string; length?: number; nullable: boolean; description?: string }[]; primaryKey?: string[]; foreignKeys: never[]; indexes: { name: string; columns: string[]; unique: boolean; type: 'btree' }[]; metadata: Record<string, unknown> }[] {
    return clusters.map(cluster => {
      const copybook = cluster.copybook && copybooks?.get(cluster.copybook);
      
      const columns: { name: string; dataType: string; nativeType: string; length?: number; nullable: boolean; description?: string }[] = copybook 
        ? copybook.fields
            .filter(f => f.level !== 88) // Exclude 88-level conditions
            .filter(f => !f.redefines) // Exclude redefines for now
            .map(field => ({
              name: field.name.toLowerCase().replace(/-/g, '_'),
              dataType: this.mapCopybookType(field),
              nativeType: field.picture || 'GROUP',
              length: field.length,
              nullable: true,
              description: field.usage,
            }))
        : [{
            name: 'record_data',
            dataType: `varchar(${cluster.recordSize.max})`,
            nativeType: 'RAW',
            length: cluster.recordSize.max,
            nullable: false,
            description: undefined,
          }];

      // Add key column if KSDS
      if (cluster.type === 'ksds' && cluster.keyPosition !== undefined) {
        columns.unshift({
          name: 'vsam_key',
          dataType: `varchar(${cluster.keyLength || 10})`,
          nativeType: 'KEY',
          length: cluster.keyLength,
          nullable: false,
          description: undefined,
        });
      }

      // Add RRN for RRDS
      if (cluster.type === 'rrds') {
        columns.unshift({
          name: 'rrn',
          dataType: 'bigint',
          nativeType: 'RRN',
          length: 8,
          nullable: false,
          description: undefined,
        });
      }

      return {
        name: this.sanitizeTableName(cluster.name),
        type: 'table' as const,
        columns,
        primaryKey: cluster.type === 'ksds' ? ['vsam_key'] : 
                   cluster.type === 'rrds' ? ['rrn'] : undefined,
        foreignKeys: [],
        indexes: cluster.type === 'ksds' ? [{
          name: `idx_${this.sanitizeTableName(cluster.name)}_key`,
          columns: ['vsam_key'],
          unique: true,
          type: 'btree' as const,
        }] : [],
        metadata: {
          vsamType: cluster.type,
          originalName: cluster.name,
          recordSize: cluster.recordSize,
        },
      };
    });
  }

  private mapCopybookType(field: { picture?: string; mappedType: string; length: number }): string {
    switch (field.mappedType) {
      case 'integer':
        return field.length <= 4 ? 'integer' : 
               field.length <= 9 ? 'bigint' : 
               `decimal(${field.length}, 0)`;
      case 'decimal':
        return `decimal(${field.length}, 2)`;
      case 'date':
        return 'date';
      case 'binary':
        return 'bytea';
      case 'group':
        return 'jsonb';
      default:
        return `varchar(${field.length})`;
    }
  }

  private sanitizeTableName(name: string): string {
    return name
      .toLowerCase()
      .replace(/[^a-z0-9]/g, '_')
      .replace(/^_+|_+$/g, '')
      .replace(/_+/g, '_');
  }
}

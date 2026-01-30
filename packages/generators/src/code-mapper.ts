/**
 * Code Mapper
 * 
 * Tracks line-by-line mappings between legacy and modern code
 */

import type { 
  CodeMappingDocument, 
  FileMappingEntry, 
} from './types.js';

export interface MappingEntry {
  sourceFile: string;
  sourceLine: number;
  sourceColumn?: number;
  targetFile: string;
  targetLine: number;
  targetColumn?: number;
  type: MappingType;
  description?: string;
}

export type MappingType = 
  | 'direct'      // Direct translation
  | 'split'       // One line became multiple
  | 'merge'       // Multiple lines became one
  | 'refactor'    // Logic restructured
  | 'generated'   // New code with no direct source
  | 'deleted';    // Source code not migrated

export interface MappingStatistics {
  totalSourceLines: number;
  totalTargetLines: number;
  directMappings: number;
  splitMappings: number;
  mergeMappings: number;
  refactoredLines: number;
  generatedLines: number;
  deletedLines: number;
  coveragePercent: number;
}

export class CodeMapper {
  private mappings: MappingEntry[] = [];
  private sourceFiles: Map<string, string[]> = new Map();
  private targetFiles: Map<string, string[]> = new Map();

  /**
   * Add a mapping entry
   */
  addMapping(entry: MappingEntry): void {
    this.mappings.push(entry);
  }

  /**
   * Add multiple mappings
   */
  addMappings(entries: MappingEntry[]): void {
    this.mappings.push(...entries);
  }

  /**
   * Register source file content
   */
  registerSourceFile(path: string, content: string): void {
    this.sourceFiles.set(path, content.split('\n'));
  }

  /**
   * Register target file content
   */
  registerTargetFile(path: string, content: string): void {
    this.targetFiles.set(path, content.split('\n'));
  }

  /**
   * Get all mappings for a source line
   */
  getMappingsForSourceLine(file: string, line: number): MappingEntry[] {
    return this.mappings.filter(
      m => m.sourceFile === file && m.sourceLine === line
    );
  }

  /**
   * Get all mappings for a target line
   */
  getMappingsForTargetLine(file: string, line: number): MappingEntry[] {
    return this.mappings.filter(
      m => m.targetFile === file && m.targetLine === line
    );
  }

  /**
   * Find source for a target location
   */
  findSourceForTarget(targetFile: string, targetLine: number): {
    sourceFile: string;
    sourceLine: number;
    code?: string;
  } | null {
    const mapping = this.mappings.find(
      m => m.targetFile === targetFile && m.targetLine === targetLine
    );
    
    if (!mapping) return null;
    
    const sourceLines = this.sourceFiles.get(mapping.sourceFile);
    return {
      sourceFile: mapping.sourceFile,
      sourceLine: mapping.sourceLine,
      code: sourceLines?.[mapping.sourceLine - 1],
    };
  }

  /**
   * Find target for a source location
   */
  findTargetForSource(sourceFile: string, sourceLine: number): {
    targetFile: string;
    targetLine: number;
    code?: string;
  }[] {
    const mappings = this.mappings.filter(
      m => m.sourceFile === sourceFile && m.sourceLine === sourceLine
    );
    
    return mappings.map(m => {
      const targetLines = this.targetFiles.get(m.targetFile);
      return {
        targetFile: m.targetFile,
        targetLine: m.targetLine,
        code: targetLines?.[m.targetLine - 1],
      };
    });
  }

  /**
   * Get mapping statistics
   */
  getStatistics(): MappingStatistics {
    let totalSourceLines = 0;
    let totalTargetLines = 0;
    
    for (const lines of this.sourceFiles.values()) {
      totalSourceLines += lines.length;
    }
    
    for (const lines of this.targetFiles.values()) {
      totalTargetLines += lines.length;
    }
    
    const byType = {
      direct: 0,
      split: 0,
      merge: 0,
      refactor: 0,
      generated: 0,
      deleted: 0,
    };
    
    for (const mapping of this.mappings) {
      byType[mapping.type]++;
    }
    
    const mappedSourceLines = new Set(
      this.mappings
        .filter(m => m.type !== 'generated')
        .map(m => `${m.sourceFile}:${m.sourceLine}`)
    ).size;
    
    return {
      totalSourceLines,
      totalTargetLines,
      directMappings: byType.direct,
      splitMappings: byType.split,
      mergeMappings: byType.merge,
      refactoredLines: byType.refactor,
      generatedLines: byType.generated,
      deletedLines: byType.deleted,
      coveragePercent: totalSourceLines > 0 
        ? (mappedSourceLines / totalSourceLines) * 100 
        : 0,
    };
  }

  /**
   * Export as CodeMappingDocument
   */
  toDocument(sourceLanguage: string, targetLanguage: string): CodeMappingDocument {
    // Group by file pairs
    const fileMappings = new Map<string, Set<string>>();
    
    for (const mapping of this.mappings) {
      const key = mapping.sourceFile;
      if (!fileMappings.has(key)) {
        fileMappings.set(key, new Set());
      }
      fileMappings.get(key)!.add(mapping.targetFile);
    }
    
    const mappings: FileMappingEntry[] = [];
    for (const [sourceFile, targetFiles] of fileMappings) {
      mappings.push({
        sourceFile,
        targetFiles: Array.from(targetFiles),
        description: `Migration of ${sourceFile}`,
      });
    }
    
    return {
      version: '1.0',
      sourceLanguage,
      targetLanguage,
      mappings,
      ruleMappings: [],
    };
  }

  /**
   * Export as source map (for debugging)
   */
  toSourceMap(): SourceMapV3 {
    const sourceMap: SourceMapV3 = {
      version: 3,
      sources: Array.from(this.sourceFiles.keys()),
      names: [],
      mappings: '',
      sourcesContent: Array.from(this.sourceFiles.values()).map(lines => lines.join('\n')),
    };
    
    // Generate VLQ-encoded mappings
    // (simplified - production would use proper source-map library)
    const segments: string[] = [];
    
    const sortedMappings = [...this.mappings].sort((a, b) => {
      if (a.targetFile !== b.targetFile) return a.targetFile.localeCompare(b.targetFile);
      return a.targetLine - b.targetLine;
    });
    
    for (const mapping of sortedMappings) {
      const sourceIndex = sourceMap.sources.indexOf(mapping.sourceFile);
      if (sourceIndex === -1) continue;
      
      // Simplified VLQ encoding (placeholder)
      segments.push(`${mapping.targetLine},${sourceIndex},${mapping.sourceLine}`);
    }
    
    sourceMap.mappings = segments.join(';');
    
    return sourceMap;
  }

  /**
   * Generate HTML visualization
   */
  toHTML(): string {
    const stats = this.getStatistics();
    
    return `<!DOCTYPE html>
<html>
<head>
  <title>Code Mapping Visualization</title>
  <style>
    body { font-family: system-ui, sans-serif; margin: 20px; }
    .stats { background: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 20px; }
    .mapping-table { width: 100%; border-collapse: collapse; }
    .mapping-table th, .mapping-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    .mapping-table th { background: #4a90d9; color: white; }
    .type-direct { background: #d4edda; }
    .type-split { background: #fff3cd; }
    .type-merge { background: #d1ecf1; }
    .type-refactor { background: #f8d7da; }
    .type-generated { background: #e2e3e5; }
  </style>
</head>
<body>
  <h1>Code Mapping Report</h1>
  
  <div class="stats">
    <h2>Statistics</h2>
    <p><strong>Source Lines:</strong> ${stats.totalSourceLines}</p>
    <p><strong>Target Lines:</strong> ${stats.totalTargetLines}</p>
    <p><strong>Coverage:</strong> ${stats.coveragePercent.toFixed(1)}%</p>
    <p><strong>Direct Mappings:</strong> ${stats.directMappings}</p>
    <p><strong>Split:</strong> ${stats.splitMappings} | <strong>Merge:</strong> ${stats.mergeMappings}</p>
    <p><strong>Refactored:</strong> ${stats.refactoredLines} | <strong>Generated:</strong> ${stats.generatedLines}</p>
  </div>
  
  <h2>Mappings</h2>
  <table class="mapping-table">
    <thead>
      <tr>
        <th>Source File</th>
        <th>Source Line</th>
        <th>Target File</th>
        <th>Target Line</th>
        <th>Type</th>
        <th>Description</th>
      </tr>
    </thead>
    <tbody>
      ${this.mappings.map(m => `
        <tr class="type-${m.type}">
          <td>${m.sourceFile}</td>
          <td>${m.sourceLine}</td>
          <td>${m.targetFile}</td>
          <td>${m.targetLine}</td>
          <td>${m.type}</td>
          <td>${m.description || ''}</td>
        </tr>
      `).join('')}
    </tbody>
  </table>
</body>
</html>`;
  }
}

interface SourceMapV3 {
  version: 3;
  sources: string[];
  names: string[];
  mappings: string;
  sourcesContent?: string[];
}

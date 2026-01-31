/**
 * Confidence Visualization for Code Analysis
 * 
 * Provides visual highlighting for code with confidence scores
 * to help developers understand AI analysis certainty levels.
 */

import type { Range } from '../index.js';

// Re-define Highlight locally to include 'kind' property for copilot
export interface ConfidenceHighlightBase {
  range: Range;
  kind: 'rule' | 'info' | 'suggestion' | 'warning' | 'error';
  message?: string;
}

// ============================================================================
// Confidence Types
// ============================================================================

export type ConfidenceLevel = 'high' | 'medium' | 'low' | 'unknown';

export interface ConfidenceRange {
  range: Range;
  confidence: number;
  level: ConfidenceLevel;
  category: ConfidenceCategory;
  explanation?: string;
}

export type ConfidenceCategory = 
  | 'rule-extraction'
  | 'type-inference'
  | 'relationship-detection'
  | 'migration-suggestion'
  | 'test-generation'
  | 'documentation';

export interface ConfidenceHighlight extends ConfidenceHighlightBase {
  confidenceLevel: ConfidenceLevel;
  confidenceScore: number;
  tooltip: string;
}

export interface ConfidenceConfig {
  thresholds: {
    high: number;   // >= this is high confidence
    medium: number; // >= this is medium confidence
    // Below medium is low confidence
  };
  colors: {
    high: string;
    medium: string;
    low: string;
    unknown: string;
  };
  showScores: boolean;
  showTooltips: boolean;
}

// ============================================================================
// Default Configuration
// ============================================================================

export const DEFAULT_CONFIDENCE_CONFIG: ConfidenceConfig = {
  thresholds: {
    high: 0.85,
    medium: 0.6,
  },
  colors: {
    high: '#22c55e',      // green-500
    medium: '#f59e0b',    // amber-500
    low: '#ef4444',       // red-500
    unknown: '#6b7280',   // gray-500
  },
  showScores: true,
  showTooltips: true,
};

// ============================================================================
// Confidence Visualizer
// ============================================================================

export class ConfidenceVisualizer {
  private config: ConfidenceConfig;
  private confidenceRanges: Map<string, ConfidenceRange[]> = new Map();

  constructor(config: Partial<ConfidenceConfig> = {}) {
    this.config = {
      ...DEFAULT_CONFIDENCE_CONFIG,
      ...config,
      thresholds: {
        ...DEFAULT_CONFIDENCE_CONFIG.thresholds,
        ...config.thresholds,
      },
      colors: {
        ...DEFAULT_CONFIDENCE_CONFIG.colors,
        ...config.colors,
      },
    };
  }

  /**
   * Classify a confidence score into a level
   */
  classifyConfidence(score: number): ConfidenceLevel {
    if (score >= this.config.thresholds.high) return 'high';
    if (score >= this.config.thresholds.medium) return 'medium';
    if (score > 0) return 'low';
    return 'unknown';
  }

  /**
   * Get color for a confidence level
   */
  getColor(level: ConfidenceLevel): string {
    return this.config.colors[level];
  }

  /**
   * Add confidence ranges for a document
   */
  setConfidenceRanges(documentUri: string, ranges: ConfidenceRange[]): void {
    this.confidenceRanges.set(documentUri, ranges);
  }

  /**
   * Get all confidence ranges for a document
   */
  getConfidenceRanges(documentUri: string): ConfidenceRange[] {
    return this.confidenceRanges.get(documentUri) || [];
  }

  /**
   * Get confidence at a specific position
   */
  getConfidenceAtPosition(
    documentUri: string,
    line: number,
    column: number
  ): ConfidenceRange | undefined {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    
    return ranges.find(r => 
      this.isPositionInRange(line, column, r.range)
    );
  }

  /**
   * Generate highlights for IDE decoration
   */
  generateHighlights(documentUri: string): ConfidenceHighlight[] {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    
    return ranges.map(r => ({
      range: r.range,
      kind: this.mapCategoryToHighlightKind(r.category),
      message: this.buildTooltipMessage(r),
      confidenceLevel: r.level,
      confidenceScore: r.confidence,
      tooltip: this.buildTooltipMessage(r),
    }));
  }

  /**
   * Generate inline decorations (e.g., confidence badges)
   */
  generateInlineDecorations(documentUri: string): InlineDecoration[] {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    
    return ranges.map(r => ({
      range: r.range,
      contentText: this.config.showScores 
        ? `${Math.round(r.confidence * 100)}%`
        : this.getConfidenceEmoji(r.level),
      color: this.getColor(r.level),
      hoverMessage: this.buildTooltipMessage(r),
    }));
  }

  /**
   * Get summary statistics for a document
   */
  getSummaryStats(documentUri: string): ConfidenceSummary {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    
    const byLevel = {
      high: 0,
      medium: 0,
      low: 0,
      unknown: 0,
    };

    const byCategory: Record<ConfidenceCategory, number> = {
      'rule-extraction': 0,
      'type-inference': 0,
      'relationship-detection': 0,
      'migration-suggestion': 0,
      'test-generation': 0,
      'documentation': 0,
    };

    let totalScore = 0;

    for (const range of ranges) {
      byLevel[range.level]++;
      byCategory[range.category]++;
      totalScore += range.confidence;
    }

    return {
      totalRanges: ranges.length,
      averageConfidence: ranges.length > 0 ? totalScore / ranges.length : 0,
      byLevel,
      byCategory,
    };
  }

  /**
   * Filter ranges by minimum confidence
   */
  filterByConfidence(
    documentUri: string,
    minConfidence: number
  ): ConfidenceRange[] {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    return ranges.filter(r => r.confidence >= minConfidence);
  }

  /**
   * Get ranges that need human review (low confidence)
   */
  getRangesNeedingReview(documentUri: string): ConfidenceRange[] {
    const ranges = this.confidenceRanges.get(documentUri) || [];
    return ranges.filter(r => 
      r.level === 'low' || r.level === 'unknown'
    );
  }

  /**
   * Clear confidence data for a document
   */
  clearDocument(documentUri: string): void {
    this.confidenceRanges.delete(documentUri);
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private isPositionInRange(
    line: number,
    column: number,
    range: Range
  ): boolean {
    if (line < range.startLine || line > range.endLine) return false;
    if (line === range.startLine && column < range.startColumn) return false;
    if (line === range.endLine && column > range.endColumn) return false;
    return true;
  }

  private mapCategoryToHighlightKind(category: ConfidenceCategory): ConfidenceHighlightBase['kind'] {
    const mapping: Record<ConfidenceCategory, ConfidenceHighlightBase['kind']> = {
      'rule-extraction': 'rule',
      'type-inference': 'info',
      'relationship-detection': 'info',
      'migration-suggestion': 'suggestion',
      'test-generation': 'info',
      'documentation': 'info',
    };
    return mapping[category];
  }

  private buildTooltipMessage(range: ConfidenceRange): string {
    const parts: string[] = [];
    
    // Confidence indicator
    const emoji = this.getConfidenceEmoji(range.level);
    const percentage = Math.round(range.confidence * 100);
    parts.push(`${emoji} **${range.level.toUpperCase()}** confidence (${percentage}%)`);
    
    // Category
    parts.push(`Category: ${this.formatCategory(range.category)}`);
    
    // Explanation if available
    if (range.explanation) {
      parts.push(`\n${range.explanation}`);
    }
    
    // Advice based on level
    if (range.level === 'low') {
      parts.push('\n⚠️ *Recommend manual review*');
    } else if (range.level === 'unknown') {
      parts.push('\n❓ *Unable to determine confidence - needs investigation*');
    }
    
    return parts.join('\n');
  }

  private getConfidenceEmoji(level: ConfidenceLevel): string {
    const emojis: Record<ConfidenceLevel, string> = {
      high: '✅',
      medium: '⚠️',
      low: '🔴',
      unknown: '❓',
    };
    return emojis[level];
  }

  private formatCategory(category: ConfidenceCategory): string {
    const labels: Record<ConfidenceCategory, string> = {
      'rule-extraction': 'Business Rule Extraction',
      'type-inference': 'Type Inference',
      'relationship-detection': 'Relationship Detection',
      'migration-suggestion': 'Migration Suggestion',
      'test-generation': 'Test Generation',
      'documentation': 'Documentation',
    };
    return labels[category];
  }
}

// ============================================================================
// Supporting Types
// ============================================================================

export interface InlineDecoration {
  range: Range;
  contentText: string;
  color: string;
  hoverMessage: string;
}

export interface ConfidenceSummary {
  totalRanges: number;
  averageConfidence: number;
  byLevel: Record<ConfidenceLevel, number>;
  byCategory: Record<ConfidenceCategory, number>;
}

// ============================================================================
// Factory Function
// ============================================================================

export function createConfidenceVisualizer(
  config?: Partial<ConfidenceConfig>
): ConfidenceVisualizer {
  return new ConfidenceVisualizer(config);
}

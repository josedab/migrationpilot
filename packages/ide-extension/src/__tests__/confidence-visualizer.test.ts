import { describe, it, expect, beforeEach } from 'vitest';
import {
  ConfidenceVisualizer,
  createConfidenceVisualizer,
  DEFAULT_CONFIDENCE_CONFIG,
  type ConfidenceRange,
} from '../copilot/confidence-visualizer.js';

describe('ConfidenceVisualizer', () => {
  let visualizer: ConfidenceVisualizer;

  beforeEach(() => {
    visualizer = createConfidenceVisualizer();
  });

  describe('classifyConfidence', () => {
    it('should classify high confidence (>= 0.85)', () => {
      expect(visualizer.classifyConfidence(0.85)).toBe('high');
      expect(visualizer.classifyConfidence(0.9)).toBe('high');
      expect(visualizer.classifyConfidence(1.0)).toBe('high');
    });

    it('should classify medium confidence (>= 0.6, < 0.85)', () => {
      expect(visualizer.classifyConfidence(0.6)).toBe('medium');
      expect(visualizer.classifyConfidence(0.7)).toBe('medium');
      expect(visualizer.classifyConfidence(0.84)).toBe('medium');
    });

    it('should classify low confidence (> 0, < 0.6)', () => {
      expect(visualizer.classifyConfidence(0.1)).toBe('low');
      expect(visualizer.classifyConfidence(0.5)).toBe('low');
      expect(visualizer.classifyConfidence(0.59)).toBe('low');
    });

    it('should classify unknown confidence (0)', () => {
      expect(visualizer.classifyConfidence(0)).toBe('unknown');
    });
  });

  describe('getColor', () => {
    it('should return correct colors for each level', () => {
      expect(visualizer.getColor('high')).toBe(DEFAULT_CONFIDENCE_CONFIG.colors.high);
      expect(visualizer.getColor('medium')).toBe(DEFAULT_CONFIDENCE_CONFIG.colors.medium);
      expect(visualizer.getColor('low')).toBe(DEFAULT_CONFIDENCE_CONFIG.colors.low);
      expect(visualizer.getColor('unknown')).toBe(DEFAULT_CONFIDENCE_CONFIG.colors.unknown);
    });
  });

  describe('confidence ranges', () => {
    const testRanges: ConfidenceRange[] = [
      {
        range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
        confidence: 0.9,
        level: 'high',
        category: 'rule-extraction',
        explanation: 'Business calculation detected',
      },
      {
        range: { startLine: 5, startColumn: 1, endLine: 8, endColumn: 10 },
        confidence: 0.65,
        level: 'medium',
        category: 'migration-suggestion',
      },
      {
        range: { startLine: 10, startColumn: 1, endLine: 10, endColumn: 30 },
        confidence: 0.4,
        level: 'low',
        category: 'type-inference',
      },
    ];

    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', testRanges);
    });

    it('should store and retrieve confidence ranges', () => {
      const ranges = visualizer.getConfidenceRanges('file:///test.cbl');
      expect(ranges).toEqual(testRanges);
    });

    it('should return empty array for unknown document', () => {
      const ranges = visualizer.getConfidenceRanges('file:///unknown.cbl');
      expect(ranges).toEqual([]);
    });

    it('should find confidence at specific position', () => {
      const result = visualizer.getConfidenceAtPosition('file:///test.cbl', 1, 25);
      expect(result).toBeDefined();
      expect(result?.level).toBe('high');
    });

    it('should return undefined for position outside ranges', () => {
      const result = visualizer.getConfidenceAtPosition('file:///test.cbl', 15, 1);
      expect(result).toBeUndefined();
    });
  });

  describe('generateHighlights', () => {
    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
      ]);
    });

    it('should generate highlights with confidence info', () => {
      const highlights = visualizer.generateHighlights('file:///test.cbl');
      
      expect(highlights.length).toBe(1);
      expect(highlights[0]?.confidenceLevel).toBe('high');
      expect(highlights[0]?.confidenceScore).toBe(0.9);
      expect(highlights[0]?.tooltip).toContain('HIGH');
    });
  });

  describe('generateInlineDecorations', () => {
    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
      ]);
    });

    it('should generate inline decorations with percentage', () => {
      const decorations = visualizer.generateInlineDecorations('file:///test.cbl');
      
      expect(decorations.length).toBe(1);
      expect(decorations[0]?.contentText).toBe('90%');
    });
  });

  describe('getSummaryStats', () => {
    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
        {
          range: { startLine: 5, startColumn: 1, endLine: 5, endColumn: 30 },
          confidence: 0.7,
          level: 'medium',
          category: 'rule-extraction',
        },
        {
          range: { startLine: 10, startColumn: 1, endLine: 10, endColumn: 20 },
          confidence: 0.4,
          level: 'low',
          category: 'migration-suggestion',
        },
      ]);
    });

    it('should calculate summary statistics', () => {
      const stats = visualizer.getSummaryStats('file:///test.cbl');
      
      expect(stats.totalRanges).toBe(3);
      expect(stats.averageConfidence).toBeCloseTo(0.667, 2);
      expect(stats.byLevel.high).toBe(1);
      expect(stats.byLevel.medium).toBe(1);
      expect(stats.byLevel.low).toBe(1);
      expect(stats.byCategory['rule-extraction']).toBe(2);
      expect(stats.byCategory['migration-suggestion']).toBe(1);
    });
  });

  describe('filterByConfidence', () => {
    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
        {
          range: { startLine: 5, startColumn: 1, endLine: 5, endColumn: 30 },
          confidence: 0.5,
          level: 'low',
          category: 'migration-suggestion',
        },
      ]);
    });

    it('should filter ranges by minimum confidence', () => {
      const filtered = visualizer.filterByConfidence('file:///test.cbl', 0.8);
      
      expect(filtered.length).toBe(1);
      expect(filtered[0]?.confidence).toBe(0.9);
    });
  });

  describe('getRangesNeedingReview', () => {
    beforeEach(() => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
        {
          range: { startLine: 5, startColumn: 1, endLine: 5, endColumn: 30 },
          confidence: 0.4,
          level: 'low',
          category: 'migration-suggestion',
        },
        {
          range: { startLine: 10, startColumn: 1, endLine: 10, endColumn: 20 },
          confidence: 0,
          level: 'unknown',
          category: 'type-inference',
        },
      ]);
    });

    it('should return low and unknown confidence ranges', () => {
      const needsReview = visualizer.getRangesNeedingReview('file:///test.cbl');
      
      expect(needsReview.length).toBe(2);
      expect(needsReview.every(r => r.level === 'low' || r.level === 'unknown')).toBe(true);
    });
  });

  describe('clearDocument', () => {
    it('should clear confidence data for a document', () => {
      visualizer.setConfidenceRanges('file:///test.cbl', [
        {
          range: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 50 },
          confidence: 0.9,
          level: 'high',
          category: 'rule-extraction',
        },
      ]);

      visualizer.clearDocument('file:///test.cbl');
      
      const ranges = visualizer.getConfidenceRanges('file:///test.cbl');
      expect(ranges).toEqual([]);
    });
  });

  describe('custom configuration', () => {
    it('should allow custom thresholds', () => {
      const customVisualizer = createConfidenceVisualizer({
        thresholds: {
          high: 0.95,
          medium: 0.7,
        },
      });

      expect(customVisualizer.classifyConfidence(0.9)).toBe('medium');
      expect(customVisualizer.classifyConfidence(0.95)).toBe('high');
      expect(customVisualizer.classifyConfidence(0.65)).toBe('low');
    });

    it('should allow custom colors', () => {
      const customVisualizer = createConfidenceVisualizer({
        colors: {
          high: '#00ff00',
          medium: '#ffff00',
          low: '#ff0000',
          unknown: '#808080',
        },
      });

      expect(customVisualizer.getColor('high')).toBe('#00ff00');
    });
  });
});

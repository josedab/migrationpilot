/**
 * Core Utilities Tests
 */

import { describe, it, expect } from 'vitest';
import { 
  generateId, 
  hashContent,
  countLines,
  truncate,
  deepMerge,
  chunk,
  formatConfidence,
  sleep,
} from '../utils/index.js';

describe('generateId', () => {
  it('should generate unique IDs', () => {
    const id1 = generateId();
    const id2 = generateId();
    
    expect(id1).toMatch(/^[a-z0-9]+-[a-z0-9]+$/);
    expect(id2).toMatch(/^[a-z0-9]+-[a-z0-9]+$/);
    expect(id1).not.toBe(id2);
  });
});

describe('hashContent', () => {
  it('should calculate consistent hash for same content', async () => {
    const content = 'Hello, World!';
    const hash1 = await hashContent(content);
    const hash2 = await hashContent(content);
    
    expect(hash1).toBe(hash2);
  });

  it('should produce different hashes for different content', async () => {
    const hash1 = await hashContent('content1');
    const hash2 = await hashContent('content2');
    
    expect(hash1).not.toBe(hash2);
  });
});

describe('countLines', () => {
  it('should count lines correctly', () => {
    expect(countLines('')).toBe(1);
    expect(countLines('line1')).toBe(1);
    expect(countLines('line1\nline2')).toBe(2);
    expect(countLines('line1\nline2\nline3')).toBe(3);
  });
});

describe('truncate', () => {
  it('should truncate long strings', () => {
    const long = 'This is a very long string that needs truncation';
    expect(truncate(long, 20)).toBe('This is a very lo...');
  });

  it('should not truncate short strings', () => {
    const short = 'Short';
    expect(truncate(short, 20)).toBe('Short');
  });
});

describe('formatConfidence', () => {
  it('should format confidence as percentage', () => {
    expect(formatConfidence(0.95)).toBe('95.0%');
    expect(formatConfidence(0.123)).toBe('12.3%');
    expect(formatConfidence(1)).toBe('100.0%');
  });
});

describe('chunk', () => {
  it('should chunk arrays correctly', () => {
    expect(chunk([1, 2, 3, 4, 5], 2)).toEqual([[1, 2], [3, 4], [5]]);
    expect(chunk([1, 2, 3], 3)).toEqual([[1, 2, 3]]);
    expect(chunk([], 2)).toEqual([]);
  });
});

describe('deepMerge', () => {
  it('should merge objects deeply', () => {
    const target = { a: 1, b: { c: 2 } };
    const source = { b: { c: 3 }, e: 4 } as Partial<typeof target & { e: number }>;
    
    const result = deepMerge(target, source as Partial<typeof target>);
    
    expect(result.a).toBe(1);
    expect(result.b.c).toBe(3);
  });

  it('should handle arrays by replacing', () => {
    const target = { a: [1, 2] };
    const source = { a: [3, 4] };
    
    const result = deepMerge(target, source);
    
    expect(result.a).toEqual([3, 4]);
  });
});

describe('sleep', () => {
  it('should wait for specified time', async () => {
    const start = Date.now();
    await sleep(50);
    const elapsed = Date.now() - start;
    
    expect(elapsed).toBeGreaterThanOrEqual(40);
  });
});

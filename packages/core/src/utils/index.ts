/**
 * Core utility functions
 */

import type { SourceLanguage, TargetLanguage, DataType } from '../types/index.js';

/**
 * Generate a unique ID
 */
export function generateId(): string {
  return `${Date.now().toString(36)}-${Math.random().toString(36).substring(2, 9)}`;
}

/**
 * Calculate hash of content for deduplication
 */
export async function hashContent(content: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(content);
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
}

/**
 * Count lines in content
 */
export function countLines(content: string): number {
  return content.split('\n').length;
}

/**
 * Get file extension for source language
 */
export function getSourceExtension(language: SourceLanguage): string[] {
  const extensions: Record<SourceLanguage, string[]> = {
    cobol: ['.cbl', '.cob', '.cpy', '.ccp'],
    fortran: ['.f', '.f77', '.f90', '.f95', '.for'],
    vb6: ['.bas', '.cls', '.frm', '.vbp'],
    vba: ['.bas', '.cls'],
    'java-legacy': ['.java'],
  };
  return extensions[language];
}

/**
 * Get file extension for target language
 */
export function getTargetExtension(language: TargetLanguage): string {
  const extensions: Record<TargetLanguage, string> = {
    java: '.java',
    python: '.py',
    typescript: '.ts',
    go: '.go',
    csharp: '.cs',
  };
  return extensions[language];
}

/**
 * Map COBOL PIC clause to DataType
 */
export function cobolPicToDataType(pic: string): DataType {
  const normalized = pic.toUpperCase().replace(/\s+/g, '');
  
  if (/^9+$/.test(normalized) || /^S?9+$/.test(normalized)) {
    return 'integer';
  }
  if (/^S?9+V9+$/.test(normalized) || /^S?9+\([\d]+\)V9+\([\d]+\)$/.test(normalized)) {
    return 'decimal';
  }
  if (/^X+$/.test(normalized) || /^X\([\d]+\)$/.test(normalized)) {
    return 'string';
  }
  if (/^A+$/.test(normalized) || /^A\([\d]+\)$/.test(normalized)) {
    return 'string';
  }
  
  return 'unknown';
}

/**
 * Format confidence as percentage
 */
export function formatConfidence(confidence: number): string {
  return `${(confidence * 100).toFixed(1)}%`;
}

/**
 * Truncate text with ellipsis
 */
export function truncate(text: string, maxLength: number): string {
  if (text.length <= maxLength) return text;
  return text.slice(0, maxLength - 3) + '...';
}

/**
 * Sleep for specified milliseconds
 */
export function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Retry a function with exponential backoff
 */
export async function retry<T>(
  fn: () => Promise<T>,
  options: {
    maxRetries?: number;
    initialDelay?: number;
    maxDelay?: number;
    factor?: number;
  } = {}
): Promise<T> {
  const { maxRetries = 3, initialDelay = 1000, maxDelay = 30000, factor = 2 } = options;
  
  let lastError: Error | undefined;
  let delay = initialDelay;
  
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error as Error;
      if (attempt < maxRetries) {
        await sleep(delay);
        delay = Math.min(delay * factor, maxDelay);
      }
    }
  }
  
  throw lastError;
}

/**
 * Chunk an array into smaller arrays
 */
export function chunk<T>(array: T[], size: number): T[][] {
  const chunks: T[][] = [];
  for (let i = 0; i < array.length; i += size) {
    chunks.push(array.slice(i, i + size));
  }
  return chunks;
}

/**
 * Deep clone an object, preserving Date objects.
 * Uses structuredClone when available for better handling of complex types.
 */
export function deepClone<T>(obj: T): T {
  // Use native structuredClone for comprehensive deep cloning
  if (typeof structuredClone === 'function') {
    return structuredClone(obj);
  }
  
  // Fallback for environments without structuredClone
  return JSON.parse(JSON.stringify(obj), (_key, value) => {
    // Restore ISO date strings to Date objects
    if (typeof value === 'string' && /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/.test(value)) {
      const date = new Date(value);
      if (!isNaN(date.getTime())) {
        return date;
      }
    }
    return value;
  });
}

/**
 * Merge objects deeply
 */
export function deepMerge<T extends object>(target: T, source: Partial<T>): T {
  const result = { ...target };
  
  for (const key in source) {
    const sourceValue = source[key];
    const targetValue = result[key];
    
    if (
      sourceValue !== undefined &&
      typeof sourceValue === 'object' &&
      sourceValue !== null &&
      !Array.isArray(sourceValue) &&
      typeof targetValue === 'object' &&
      targetValue !== null &&
      !Array.isArray(targetValue)
    ) {
      (result as Record<string, unknown>)[key] = deepMerge(
        targetValue as object,
        sourceValue as object
      );
    } else if (sourceValue !== undefined) {
      (result as Record<string, unknown>)[key] = sourceValue;
    }
  }
  
  return result;
}

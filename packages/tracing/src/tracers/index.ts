/**
 * Tracers Module
 * 
 * Language-specific execution tracers for legacy systems
 */

export { BaseTracer, DEFAULT_TRACER_CONFIG } from './base-tracer.js';
export { COBOLTracer, type COBOLTracerConfig } from './cobol-tracer.js';
export { FortranTracer, type FortranTracerConfig } from './fortran-tracer.js';
export { JavaLegacyTracer, type JavaLegacyTracerConfig } from './java-legacy-tracer.js';

import type { SourceLanguage } from '@migrationpilot/core';
import type { ITracer, TracerConfig } from '../types.js';
import { COBOLTracer, type COBOLTracerConfig } from './cobol-tracer.js';
import { FortranTracer, type FortranTracerConfig } from './fortran-tracer.js';
import { JavaLegacyTracer, type JavaLegacyTracerConfig } from './java-legacy-tracer.js';

export type TracerConfigByLanguage = {
  cobol: Partial<COBOLTracerConfig>;
  fortran: Partial<FortranTracerConfig>;
  'java-legacy': Partial<JavaLegacyTracerConfig>;
  vb6: Partial<TracerConfig>;
  vba: Partial<TracerConfig>;
};

/**
 * Factory function to create the appropriate tracer for a language
 */
export function createTracer(
  language: SourceLanguage,
  config: { projectId: string } & Record<string, unknown>
): ITracer {
  switch (language) {
    case 'cobol':
      return new COBOLTracer(config as unknown as COBOLTracerConfig);
    case 'fortran':
      return new FortranTracer(config as unknown as FortranTracerConfig);
    case 'java-legacy':
      return new JavaLegacyTracer(config as unknown as JavaLegacyTracerConfig);
    case 'vb6':
    case 'vba':
      throw new Error(`VB6/VBA tracer not yet implemented`);
    default:
      throw new Error(`Unsupported language for tracing: ${language}`);
  }
}

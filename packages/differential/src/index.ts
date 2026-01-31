/**
 * @migrationpilot/differential
 * 
 * Behavioral Differential Analysis Engine
 * 
 * Compares legacy and migrated code behavior to detect semantic differences.
 * Uses symbolic execution, input fuzzing, and trace comparison.
 */

// Types
export * from './types.js';

// Analyzers
export * from './analyzers/index.js';

// Comparators
export * from './comparators/index.js';

// Reporters
export * from './reporters/index.js';

// Main Engine
export * from './engine.js';

/**
 * @migrationpilot/data-planner
 * 
 * AI-Powered Data Migration Planner for legacy database modernization.
 * Supports DB2, IMS, VSAM, and legacy file structures.
 */

// Types
export * from './types.js';

// Schema Extractors
export {
  DB2SchemaExtractor,
  IMSSchemaExtractor,
  VSAMSchemaExtractor,
  COPYBOOKSchemaExtractor,
} from './extractors/index.js';

// Analyzers
export {
  RelationshipAnalyzer,
  DataQualityAnalyzer,
  MigrationComplexityAnalyzer,
} from './analyzers/index.js';

// Generators
export {
  MigrationPlanGenerator,
  ETLScriptGenerator,
  ValidationQueryGenerator,
} from './generators/index.js';

// Main service
export { DataMigrationPlanner } from './planner.js';

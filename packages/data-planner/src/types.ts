/**
 * Data Migration Planner Types
 */

// ============================================================================
// SOURCE DATABASE TYPES
// ============================================================================

export type LegacyDatabaseType = 'db2' | 'ims' | 'vsam' | 'idms' | 'adabas' | 'flat-file';
export type TargetDatabaseType = 'postgresql' | 'mysql' | 'oracle' | 'sqlserver' | 'mongodb' | 'dynamodb';

export interface LegacyDataSource {
  id: string;
  type: LegacyDatabaseType;
  name: string;
  connectionInfo?: ConnectionInfo;
  metadata?: Record<string, unknown>;
}

export interface ConnectionInfo {
  host?: string;
  port?: number;
  database?: string;
  schema?: string;
  credentials?: {
    username: string;
    passwordRef: string; // Reference to secret store
  };
}

// ============================================================================
// SCHEMA TYPES
// ============================================================================

export interface LegacySchema {
  sourceId: string;
  sourceType: LegacyDatabaseType;
  tables: LegacyTable[];
  segments?: IMSSegment[];      // For IMS
  clusters?: VSAMCluster[];     // For VSAM
  copybooks?: CopybookDefinition[]; // For file structures
  extractedAt: Date;
}

export interface LegacyTable {
  name: string;
  schema?: string;
  type: 'table' | 'view' | 'synonym' | 'alias';
  columns: LegacyColumn[];
  primaryKey?: string[];
  foreignKeys: ForeignKeyDefinition[];
  indexes: IndexDefinition[];
  rowCount?: number;
  sizeBytes?: number;
  ddl?: string;
  metadata?: Record<string, unknown>;
}

export interface LegacyColumn {
  name: string;
  dataType: string;
  nativeType: string;
  length?: number;
  precision?: number;
  scale?: number;
  nullable: boolean;
  defaultValue?: string;
  computed?: boolean;
  computeExpression?: string;
  description?: string;
}

export interface ForeignKeyDefinition {
  name: string;
  columns: string[];
  referencedTable: string;
  referencedColumns: string[];
  onDelete?: 'cascade' | 'set-null' | 'restrict' | 'no-action';
  onUpdate?: 'cascade' | 'set-null' | 'restrict' | 'no-action';
}

export interface IndexDefinition {
  name: string;
  columns: string[];
  unique: boolean;
  type: 'btree' | 'hash' | 'clustered' | 'bitmap';
}

// ============================================================================
// IMS-SPECIFIC TYPES
// ============================================================================

export interface IMSSegment {
  name: string;
  parentSegment?: string;
  fields: IMSField[];
  sequenceField?: string;
  pointerType?: 'hierarchical' | 'physical-twin' | 'logical-twin';
  rules?: string[];
}

export interface IMSField {
  name: string;
  startPosition: number;
  length: number;
  dataType: 'char' | 'packed' | 'zoned' | 'binary' | 'float';
  scale?: number;
  key?: boolean;
}

// ============================================================================
// VSAM-SPECIFIC TYPES
// ============================================================================

export interface VSAMCluster {
  name: string;
  type: 'ksds' | 'esds' | 'rrds' | 'lds';
  dataComponent: string;
  indexComponent?: string;
  recordSize: { min: number; max: number; average: number };
  keyPosition?: number;
  keyLength?: number;
  ciSize: number;
  freespace?: { ci: number; ca: number };
  copybook?: string;
}

// ============================================================================
// COPYBOOK TYPES
// ============================================================================

export interface CopybookDefinition {
  name: string;
  content: string;
  fields: CopybookField[];
  recordLength: number;
  redefines?: RedefineGroup[];
}

export interface CopybookField {
  level: number;
  name: string;
  picture?: string;
  usage?: string;
  occurs?: { min: number; max: number; dependingOn?: string };
  value?: string;
  redefines?: string;
  startPosition: number;
  length: number;
  mappedType: 'string' | 'integer' | 'decimal' | 'date' | 'binary' | 'group';
}

export interface RedefineGroup {
  name: string;
  redefinesField: string;
  fields: CopybookField[];
}

// ============================================================================
// TARGET SCHEMA TYPES
// ============================================================================

export interface TargetSchema {
  targetType: TargetDatabaseType;
  tables: TargetTable[];
  views?: TargetView[];
  functions?: TargetFunction[];
  triggers?: TargetTrigger[];
  generatedAt: Date;
}

export interface TargetTable {
  name: string;
  schema?: string;
  columns: TargetColumn[];
  primaryKey: string[];
  foreignKeys: ForeignKeyDefinition[];
  indexes: IndexDefinition[];
  partitioning?: PartitionStrategy;
  sourceMapping: TableMapping;
}

export interface TargetColumn {
  name: string;
  dataType: string;
  nullable: boolean;
  defaultValue?: string;
  generated?: GeneratedColumnDef;
  constraints?: ColumnConstraint[];
  sourceMapping: ColumnMapping;
}

export interface GeneratedColumnDef {
  type: 'identity' | 'computed' | 'uuid';
  expression?: string;
}

export interface ColumnConstraint {
  type: 'check' | 'unique' | 'not-null';
  expression?: string;
  name?: string;
}

export interface PartitionStrategy {
  type: 'range' | 'list' | 'hash';
  columns: string[];
  partitions?: PartitionDefinition[];
}

export interface PartitionDefinition {
  name: string;
  values?: unknown[];
  range?: { from: unknown; to: unknown };
}

export interface TargetView {
  name: string;
  schema?: string;
  definition: string;
  materialized: boolean;
}

export interface TargetFunction {
  name: string;
  schema?: string;
  language: string;
  definition: string;
  parameters: FunctionParameter[];
  returnType: string;
}

export interface FunctionParameter {
  name: string;
  type: string;
  mode: 'in' | 'out' | 'inout';
  default?: string;
}

export interface TargetTrigger {
  name: string;
  table: string;
  timing: 'before' | 'after' | 'instead-of';
  events: ('insert' | 'update' | 'delete')[];
  definition: string;
}

// ============================================================================
// MAPPING TYPES
// ============================================================================

export interface TableMapping {
  sourceTable: string;
  sourceType: LegacyDatabaseType;
  transformationType: 'direct' | 'split' | 'merge' | 'pivot' | 'normalize';
  additionalSources?: string[];
  notes?: string;
}

export interface ColumnMapping {
  sourceColumn: string;
  sourceTable: string;
  transformation?: ColumnTransformation;
}

export interface ColumnTransformation {
  type: 'cast' | 'substring' | 'concat' | 'compute' | 'lookup' | 'decode' | 'custom';
  expression?: string;
  lookupTable?: string;
  parameters?: Record<string, unknown>;
}

// ============================================================================
// RELATIONSHIP INFERENCE TYPES
// ============================================================================

export interface InferredRelationship {
  id: string;
  sourceTable: string;
  sourceColumns: string[];
  targetTable: string;
  targetColumns: string[];
  confidence: number;
  evidence: RelationshipEvidence[];
  inferenceMethod: 'naming-convention' | 'data-analysis' | 'code-analysis' | 'explicit-fk';
  status: 'suggested' | 'confirmed' | 'rejected';
}

export interface RelationshipEvidence {
  type: 'name-match' | 'data-overlap' | 'code-join' | 'explicit-definition';
  description: string;
  confidence: number;
  details?: Record<string, unknown>;
}

// ============================================================================
// MIGRATION PLAN TYPES
// ============================================================================

export interface DataMigrationPlan {
  id: string;
  projectId: string;
  name: string;
  description: string;
  
  // Source and target
  source: LegacySchema;
  target: TargetSchema;
  
  // Mappings and relationships
  tableMappings: TableMigrationPlan[];
  inferredRelationships: InferredRelationship[];
  
  // Migration strategy
  strategy: MigrationStrategy;
  
  // Validation
  validationQueries: ValidationQuery[];
  
  // Estimates
  estimates: MigrationEstimates;
  
  // Metadata
  status: 'draft' | 'approved' | 'in-progress' | 'completed' | 'failed';
  createdAt: Date;
  updatedAt: Date;
  createdBy: string;
}

export interface TableMigrationPlan {
  sourceTable: string;
  targetTable: string;
  columnMappings: ColumnMigrationPlan[];
  filterCondition?: string;
  orderBy?: string[];
  batchSize: number;
  parallelism: number;
  dependencies: string[];
  priority: number;
  etlScript?: string;
}

export interface ColumnMigrationPlan {
  sourceColumn: string;
  targetColumn: string;
  transformation: ColumnTransformation | null;
  defaultValue?: unknown;
  nullHandling: 'preserve' | 'default' | 'error';
  validationRule?: string;
}

export interface MigrationStrategy {
  approach: 'big-bang' | 'incremental' | 'parallel-run' | 'blue-green';
  phases: MigrationPhase[];
  rollbackPlan: RollbackPlan;
  dataValidation: ValidationStrategy;
}

export interface MigrationPhase {
  id: string;
  name: string;
  description: string;
  tables: string[];
  dependencies: string[];
  estimatedDuration: number; // minutes
  checkpoints: Checkpoint[];
}

export interface Checkpoint {
  name: string;
  type: 'row-count' | 'checksum' | 'custom-query';
  query?: string;
  tolerance?: number;
}

export interface RollbackPlan {
  strategy: 'full-restore' | 'incremental' | 'point-in-time';
  backupLocation?: string;
  maxRollbackTime: number; // minutes
  procedures: RollbackProcedure[];
}

export interface RollbackProcedure {
  phase: string;
  steps: string[];
  estimatedTime: number;
}

export interface ValidationStrategy {
  preValidation: ValidationQuery[];
  postValidation: ValidationQuery[];
  continuousValidation: ValidationQuery[];
  samplingRate?: number;
}

export interface ValidationQuery {
  id: string;
  name: string;
  description: string;
  type: 'row-count' | 'checksum' | 'sample' | 'referential' | 'custom';
  sourceQuery: string;
  targetQuery: string;
  tolerance: number;
  critical: boolean;
}

export interface MigrationEstimates {
  totalRows: number;
  totalSizeGB: number;
  estimatedDurationMinutes: number;
  complexityScore: number;
  riskScore: number;
  tableEstimates: TableEstimate[];
}

export interface TableEstimate {
  table: string;
  rows: number;
  sizeGB: number;
  complexity: 'low' | 'medium' | 'high';
  estimatedMinutes: number;
  risks: string[];
}

// ============================================================================
// DATA QUALITY TYPES
// ============================================================================

export interface DataQualityReport {
  sourceId: string;
  analyzedAt: Date;
  tables: TableQualityReport[];
  overallScore: number;
  issues: DataQualityIssue[];
}

export interface TableQualityReport {
  table: string;
  rowCount: number;
  columnReports: ColumnQualityReport[];
  score: number;
}

export interface ColumnQualityReport {
  column: string;
  nullRate: number;
  uniqueRate: number;
  patternMatches?: PatternMatch[];
  outliers?: number;
  dataTypeViolations?: number;
}

export interface PatternMatch {
  pattern: string;
  matchRate: number;
  examples: string[];
}

export interface DataQualityIssue {
  severity: 'critical' | 'warning' | 'info';
  category: 'null-values' | 'duplicates' | 'type-mismatch' | 'referential' | 'format';
  table: string;
  column?: string;
  description: string;
  affectedRows?: number;
  recommendation: string;
}

// ============================================================================
// ETL TYPES
// ============================================================================

export interface ETLScript {
  id: string;
  name: string;
  language: 'sql' | 'python' | 'spark' | 'dbt';
  content: string;
  sourceTable: string;
  targetTable: string;
  dependencies: string[];
  estimatedDuration: number;
}

export interface ETLPipeline {
  id: string;
  name: string;
  scripts: ETLScript[];
  schedule?: CronSchedule;
  notifications: NotificationConfig[];
}

export interface CronSchedule {
  expression: string;
  timezone: string;
}

export interface NotificationConfig {
  type: 'email' | 'slack' | 'webhook';
  events: ('start' | 'success' | 'failure' | 'warning')[];
  recipients: string[];
}

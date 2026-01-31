/**
 * Data Migration Planner Service
 * 
 * Main orchestrator for data migration planning workflow.
 */

import type {
  LegacySchema,
  LegacyDataSource,
  TargetDatabaseType,
  DataMigrationPlan,
  InferredRelationship,
  DataQualityReport,
  MigrationEstimates,
  ETLPipeline,
  ValidationQuery,
  CopybookDefinition,
} from './types.js';

import { DB2SchemaExtractor } from './extractors/db2-extractor.js';
import { IMSSchemaExtractor } from './extractors/ims-extractor.js';
import { VSAMSchemaExtractor } from './extractors/vsam-extractor.js';
import { COPYBOOKSchemaExtractor } from './extractors/copybook-extractor.js';
import { RelationshipAnalyzer } from './analyzers/relationship-analyzer.js';
import { DataQualityAnalyzer } from './analyzers/data-quality-analyzer.js';
import { MigrationComplexityAnalyzer } from './analyzers/complexity-analyzer.js';
import { MigrationPlanGenerator } from './generators/migration-plan-generator.js';
import { ETLScriptGenerator } from './generators/etl-script-generator.js';
import { ValidationQueryGenerator } from './generators/validation-query-generator.js';

export interface DataMigrationPlannerConfig {
  targetDatabase: TargetDatabaseType;
  schemaName?: string;
  namingConvention?: 'snake_case' | 'camelCase' | 'PascalCase';
  options?: {
    preserveNullability?: boolean;
    addAuditColumns?: boolean;
    generateIndexes?: boolean;
    batchSize?: number;
    parallelism?: number;
    sampleSize?: number;
    confidenceThreshold?: number;
  };
}

export interface PlanningResult {
  plan: DataMigrationPlan;
  qualityReport: DataQualityReport;
  estimates: MigrationEstimates;
  etlPipeline: ETLPipeline;
  ddlScript: string;
  validationQueries: {
    pre: ValidationQuery[];
    post: ValidationQuery[];
    continuous: ValidationQuery[];
  };
}

export interface ProgressCallback {
  (phase: string, progress: number, message: string): void;
}

export class DataMigrationPlanner {
  private relationshipAnalyzer: RelationshipAnalyzer;
  private qualityAnalyzer: DataQualityAnalyzer;
  private complexityAnalyzer: MigrationComplexityAnalyzer;
  private planGenerator: MigrationPlanGenerator;
  private etlGenerator: ETLScriptGenerator;
  private validationGenerator: ValidationQueryGenerator;

  constructor(config: DataMigrationPlannerConfig) {

    this.relationshipAnalyzer = new RelationshipAnalyzer({
      minConfidenceThreshold: config.options?.confidenceThreshold || 0.5,
    });

    this.qualityAnalyzer = new DataQualityAnalyzer({
      sampleSize: config.options?.sampleSize || 10000,
    });

    this.complexityAnalyzer = new MigrationComplexityAnalyzer();

    this.planGenerator = new MigrationPlanGenerator({
      targetDatabase: config.targetDatabase,
      schemaName: config.schemaName,
      namingConvention: config.namingConvention || 'snake_case',
      preserveNullability: config.options?.preserveNullability ?? true,
      addAuditColumns: config.options?.addAuditColumns ?? true,
      generateIndexes: config.options?.generateIndexes ?? true,
      batchSize: config.options?.batchSize || 10000,
      parallelism: config.options?.parallelism || 4,
    });

    this.etlGenerator = new ETLScriptGenerator({
      outputFormat: 'sql',
      targetDatabase: config.targetDatabase,
      includeErrorHandling: true,
      includeLogging: true,
      batchSize: config.options?.batchSize || 10000,
    });

    this.validationGenerator = new ValidationQueryGenerator({
      targetDatabase: config.targetDatabase,
      sampleSize: config.options?.sampleSize || 1000,
      checksumMethod: 'md5',
      includeDataSampling: true,
      includeReferentialChecks: true,
    });
  }

  /**
   * Extract schema from a legacy data source
   */
  async extractSchema(
    source: LegacyDataSource,
    options?: {
      copybookSources?: Map<string, string>;
      dbdSource?: string;
      idcamsOutput?: string;
    }
  ): Promise<LegacySchema> {
    switch (source.type) {
      case 'db2': {
        const extractor = new DB2SchemaExtractor({
          connection: source.connectionInfo || {},
        });
        // In real implementation, would execute queries and parse results
        // For now, return queries that need to be executed
        const queries = extractor.getCatalogQueries();
        return {
          sourceId: source.id,
          sourceType: 'db2',
          tables: [],
          extractedAt: new Date(),
          metadata: { queries } as unknown as Record<string, unknown>,
        } as unknown as LegacySchema;
      }

      case 'ims': {
        const extractor = new IMSSchemaExtractor({
          dbdSource: options?.dbdSource,
          copybookSources: options?.copybookSources,
        });
        if (options?.dbdSource) {
          const schema = extractor.extractFromDBD(options.dbdSource);
          return extractor.convertToRelational(schema);
        }
        throw new Error('DBD source required for IMS extraction');
      }

      case 'vsam': {
        const extractor = new VSAMSchemaExtractor({});
        const clusters = options?.idcamsOutput
          ? extractor.extractFromIDCAMS(options.idcamsOutput)
          : [];
        
        let copybooks: Map<string, CopybookDefinition> | undefined;
        if (options?.copybookSources) {
          const cbExtractor = new COPYBOOKSchemaExtractor();
          copybooks = cbExtractor.extractMultiple(options.copybookSources);
        }
        
        return extractor.buildSchema(clusters, copybooks);
      }

      case 'flat-file': {
        if (!options?.copybookSources) {
          throw new Error('Copybook sources required for flat file extraction');
        }
        const extractor = new COPYBOOKSchemaExtractor();
        const copybooks = extractor.extractMultiple(options.copybookSources);
        
        return {
          sourceId: source.id,
          sourceType: 'flat-file',
          tables: [],
          copybooks: Array.from(copybooks.values()),
          extractedAt: new Date(),
        };
      }

      default:
        throw new Error(`Unsupported source type: ${source.type}`);
    }
  }

  /**
   * Analyze relationships in a schema
   */
  analyzeRelationships(schema: LegacySchema): InferredRelationship[] {
    return this.relationshipAnalyzer.analyzeRelationships(schema);
  }

  /**
   * Analyze data quality (requires data samples)
   */
  analyzeDataQuality(
    schema: LegacySchema,
    samples: { table: string; column: string; values: unknown[]; totalRows: number }[]
  ): DataQualityReport {
    return this.qualityAnalyzer.analyzeDataSamples(schema, samples);
  }

  /**
   * Estimate migration complexity
   */
  estimateComplexity(
    schema: LegacySchema,
    relationships: InferredRelationship[],
    statistics?: Map<string, { rowCount: number; sizeBytes: number }>
  ): MigrationEstimates {
    return this.complexityAnalyzer.analyze(schema, relationships, statistics);
  }

  /**
   * Generate complete migration plan
   */
  generatePlan(
    schema: LegacySchema,
    relationships: InferredRelationship[],
    projectId: string,
    userId: string
  ): DataMigrationPlan {
    return this.planGenerator.generatePlan(schema, relationships, projectId, userId);
  }

  /**
   * Generate ETL pipeline
   */
  generateETLPipeline(plan: DataMigrationPlan): ETLPipeline {
    return this.etlGenerator.generatePipeline(
      plan.tableMappings,
      `ETL Pipeline - ${plan.name}`
    );
  }

  /**
   * Generate target DDL
   */
  generateDDL(plan: DataMigrationPlan): string {
    return this.planGenerator.generateDDL(plan.target);
  }

  /**
   * Generate validation queries
   */
  generateValidationQueries(plan: DataMigrationPlan): {
    pre: ValidationQuery[];
    post: ValidationQuery[];
    continuous: ValidationQuery[];
  } {
    const result = this.validationGenerator.generateAll(
      plan.source,
      plan.target,
      plan.tableMappings,
      plan.inferredRelationships
    );

    return {
      pre: result.preValidation,
      post: result.postValidation,
      continuous: result.continuousValidation,
    };
  }

  /**
   * Execute full planning workflow
   */
  async plan(
    source: LegacyDataSource,
    projectId: string,
    userId: string,
    options?: {
      copybookSources?: Map<string, string>;
      dbdSource?: string;
      idcamsOutput?: string;
      dataSamples?: { table: string; column: string; values: unknown[]; totalRows: number }[];
      statistics?: Map<string, { rowCount: number; sizeBytes: number }>;
      onProgress?: ProgressCallback;
    }
  ): Promise<PlanningResult> {
    const progress = options?.onProgress || (() => {});

    // Phase 1: Schema Extraction
    progress('extraction', 0, 'Extracting source schema...');
    const schema = await this.extractSchema(source, {
      copybookSources: options?.copybookSources,
      dbdSource: options?.dbdSource,
      idcamsOutput: options?.idcamsOutput,
    });
    progress('extraction', 100, `Extracted ${schema.tables.length} tables`);

    // Phase 2: Relationship Analysis
    progress('analysis', 0, 'Analyzing relationships...');
    const relationships = this.analyzeRelationships(schema);
    progress('analysis', 50, `Found ${relationships.length} relationships`);

    // Phase 3: Data Quality Analysis
    progress('quality', 0, 'Analyzing data quality...');
    const qualityReport = options?.dataSamples
      ? this.analyzeDataQuality(schema, options.dataSamples)
      : {
          sourceId: schema.sourceId,
          analyzedAt: new Date(),
          tables: [],
          overallScore: 100,
          issues: [],
        };
    progress('quality', 100, `Quality score: ${qualityReport.overallScore}`);

    // Phase 4: Complexity Estimation
    progress('estimation', 0, 'Estimating migration complexity...');
    const estimates = this.estimateComplexity(schema, relationships, options?.statistics);
    progress('estimation', 100, `Estimated duration: ${estimates.estimatedDurationMinutes} minutes`);

    // Phase 5: Plan Generation
    progress('planning', 0, 'Generating migration plan...');
    const plan = this.generatePlan(schema, relationships, projectId, userId);
    plan.estimates = estimates;
    progress('planning', 100, 'Migration plan generated');

    // Phase 6: ETL Generation
    progress('etl', 0, 'Generating ETL scripts...');
    const etlPipeline = this.generateETLPipeline(plan);
    progress('etl', 100, `Generated ${etlPipeline.scripts.length} ETL scripts`);

    // Phase 7: DDL Generation
    progress('ddl', 0, 'Generating DDL...');
    const ddlScript = this.generateDDL(plan);
    progress('ddl', 100, 'DDL generated');

    // Phase 8: Validation Queries
    progress('validation', 0, 'Generating validation queries...');
    const validationQueries = this.generateValidationQueries(plan);
    progress('validation', 100, 'Validation queries generated');

    return {
      plan,
      qualityReport,
      estimates,
      etlPipeline,
      ddlScript,
      validationQueries,
    };
  }

  /**
   * Generate data quality SQL queries (for when you need to collect samples)
   */
  getDataQualityQueries(schema: LegacySchema): Map<string, string[]> {
    return this.qualityAnalyzer.generateQualityQueries(schema);
  }

  /**
   * Generate effort report
   */
  generateEffortReport(estimates: MigrationEstimates): string {
    return this.complexityAnalyzer.generateEffortReport(estimates);
  }
}

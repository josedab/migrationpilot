/**
 * Migration Orchestrator
 * 
 * Coordinates all agents through the migration pipeline
 */

import type { SourceLanguage, TargetLanguage, BusinessRule } from '@migrationpilot/core';
import { generateId } from '@migrationpilot/core';
import { ArcheologistAgent, type AnalysisResult } from './archeologist/index.js';
import { ArchitectAgent, type ArchitectureDesign } from './architect/index.js';
import { BuilderAgent, type GenerationResult } from './builder/index.js';
import { ValidatorAgent, type ValidationResult } from './validator/index.js';
import type { AgentContext } from './common/base-agent.js';
import { AgentFactory, type AgentFactoryConfig } from './common/agent-factory.js';

export interface MigrationConfig {
  projectId: string;
  userId: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  targetFramework: string;
  options: {
    enableStranglerFig: boolean;
    generateTests: boolean;
    generateDocumentation: boolean;
    humanReviewRequired: boolean;
    confidenceThreshold: number;
  };
  callbacks?: {
    onProgress?: (phase: string, progress: number, message: string) => void;
    onAnalysisComplete?: (analysis: AnalysisResult) => void;
    onArchitectureComplete?: (design: ArchitectureDesign) => void;
    onGenerationComplete?: (result: GenerationResult) => void;
    onValidationComplete?: (result: ValidationResult) => void;
    onHumanReviewRequired?: (rules: BusinessRule[]) => Promise<BusinessRule[]>;
  };
}

export interface MigrationResult {
  success: boolean;
  projectId: string;
  
  // Phase results
  analysis?: AnalysisResult;
  architecture?: ArchitectureDesign;
  generation?: GenerationResult;
  validation?: ValidationResult;
  
  // Summary
  confidence: number;
  readyForProduction: boolean;
  warnings: string[];
  errors: string[];
  
  // Timing
  startedAt: Date;
  completedAt: Date;
  duration: number;
}

export class MigrationOrchestrator {
  private archeologist: ArcheologistAgent;
  private architect: ArchitectAgent;
  private builder: BuilderAgent;
  private validator: ValidatorAgent;
  private factory: AgentFactory;

  constructor(config?: AgentFactoryConfig) {
    this.factory = new AgentFactory(config);
    this.archeologist = this.factory.createArcheologist();
    this.architect = this.factory.createArchitect();
    this.builder = this.factory.createBuilder();
    this.validator = this.factory.createValidator();
  }

  /**
   * Execute full migration pipeline
   */
  async migrate(
    sourceCode: string,
    filename: string,
    config: MigrationConfig
  ): Promise<MigrationResult> {
    const startedAt = new Date();
    const result: Partial<MigrationResult> = {
      success: false,
      projectId: config.projectId,
      warnings: [],
      errors: [],
      startedAt,
    };

    const context: AgentContext = {
      projectId: config.projectId,
      sessionId: generateId(),
      userId: config.userId,
    };

    try {
      // Phase 1: Analysis
      result.analysis = await this.executeAnalysisPhase(context, sourceCode, filename, config);
      
      // Optional: Human review of business rules
      const reviewedRules = await this.executeHumanReviewPhase(result.analysis.businessRules, config);

      // Phase 2: Architecture Design
      result.architecture = await this.executeArchitecturePhase(context, result.analysis, config);

      // Phase 3: Code Generation
      result.generation = await this.executeGenerationPhase(context, result.architecture, config);

      // Phase 4: Validation
      result.validation = await this.executeValidationPhase(context, reviewedRules, result, config);

      // Calculate final confidence
      result.confidence = result.validation?.report.equivalenceScore || 0;
      result.readyForProduction = result.confidence >= config.options.confidenceThreshold * 100;
      result.success = true;

    } catch (error) {
      result.errors?.push(error instanceof Error ? error.message : 'Unknown error');
      result.success = false;
    }

    const completedAt = new Date();
    
    // Construct complete result without type assertion
    const migrationResult: MigrationResult = {
      success: result.success ?? false,
      projectId: config.projectId,
      analysis: result.analysis,
      architecture: result.architecture,
      generation: result.generation,
      validation: result.validation,
      confidence: result.confidence ?? 0,
      readyForProduction: result.readyForProduction ?? false,
      warnings: result.warnings ?? [],
      errors: result.errors ?? [],
      startedAt,
      completedAt,
      duration: completedAt.getTime() - startedAt.getTime(),
    };

    return migrationResult;
  }

  /**
   * Phase 1: Analyze source code and extract business rules
   */
  private async executeAnalysisPhase(
    context: AgentContext,
    sourceCode: string,
    filename: string,
    config: MigrationConfig
  ): Promise<AnalysisResult> {
    config.callbacks?.onProgress?.('analysis', 0, 'Starting code analysis...');
    
    const analysisResponse = await this.archeologist.analyzeFile(
      context,
      sourceCode,
      config.sourceLanguage,
      filename
    );

    if (!analysisResponse.success || !analysisResponse.data) {
      throw new Error(`Analysis failed: ${analysisResponse.error}`);
    }

    config.callbacks?.onAnalysisComplete?.(analysisResponse.data);
    config.callbacks?.onProgress?.('analysis', 100, `Extracted ${analysisResponse.data.businessRules.length} business rules`);

    return analysisResponse.data;
  }

  /**
   * Phase 1b: Optional human review of low-confidence business rules
   */
  private async executeHumanReviewPhase(
    businessRules: BusinessRule[],
    config: MigrationConfig
  ): Promise<BusinessRule[]> {
    if (!config.options.humanReviewRequired || !config.callbacks?.onHumanReviewRequired) {
      return businessRules;
    }

    const lowConfidenceRules = businessRules.filter(r => r.confidence < config.options.confidenceThreshold);
    if (lowConfidenceRules.length === 0) {
      return businessRules;
    }

    config.callbacks?.onProgress?.('review', 0, `${lowConfidenceRules.length} rules need review`);
    return await config.callbacks.onHumanReviewRequired(lowConfidenceRules);
  }

  /**
   * Phase 2: Design modern architecture from analysis results
   */
  private async executeArchitecturePhase(
    context: AgentContext,
    analysis: AnalysisResult,
    config: MigrationConfig
  ): Promise<ArchitectureDesign> {
    config.callbacks?.onProgress?.('architecture', 0, 'Designing modern architecture...');

    const architectureResponse = await this.architect.designArchitecture(
      { ...context, sessionId: generateId() },
      analysis,
      config.targetLanguage,
      config.targetFramework
    );

    if (!architectureResponse.success || !architectureResponse.data) {
      throw new Error(`Architecture design failed: ${architectureResponse.error}`);
    }

    config.callbacks?.onArchitectureComplete?.(architectureResponse.data);
    config.callbacks?.onProgress?.('architecture', 100, `Designed ${architectureResponse.data.services.length} services`);

    return architectureResponse.data;
  }

  /**
   * Phase 3: Generate modern code for all services
   */
  private async executeGenerationPhase(
    context: AgentContext,
    architecture: ArchitectureDesign,
    config: MigrationConfig
  ): Promise<GenerationResult> {
    config.callbacks?.onProgress?.('generation', 0, 'Generating modern code...');

    const totalServices = architecture.services.length;
    const allFiles: GenerationResult['files'] = [];

    for (let i = 0; i < totalServices; i++) {
      const service = architecture.services[i]!;
      const serviceApis = architecture.apis.filter(a => 
        service.responsibilities.some(r => a.name.toLowerCase().includes(r.toLowerCase()))
      );

      const generationResponse = await this.builder.generateService(
        { ...context, sessionId: generateId() },
        service,
        serviceApis,
        {
          targetLanguage: config.targetLanguage,
          framework: config.targetFramework,
          includeTests: config.options.generateTests,
          includeDocumentation: config.options.generateDocumentation,
        }
      );

      if (generationResponse.success && generationResponse.data) {
        allFiles.push(...generationResponse.data.files);
      }

      const progress = Math.round(((i + 1) / totalServices) * 100);
      config.callbacks?.onProgress?.('generation', progress, `Generated ${service.name}`);
    }

    const result: GenerationResult = {
      files: allFiles,
      summary: {
        totalFiles: allFiles.length,
        sourceFiles: allFiles.filter(f => f.type === 'source').length,
        testFiles: allFiles.filter(f => f.type === 'test').length,
        configFiles: allFiles.filter(f => f.type === 'config').length,
        documentationFiles: allFiles.filter(f => f.type === 'documentation').length,
        totalLines: allFiles.reduce((sum, f) => sum + f.content.split('\n').length, 0),
      },
      warnings: [],
    };

    config.callbacks?.onGenerationComplete?.(result);
    return result;
  }

  /**
   * Phase 4: Validate behavioral equivalence
   */
  private async executeValidationPhase(
    context: AgentContext,
    reviewedRules: BusinessRule[],
    result: Partial<MigrationResult>,
    config: MigrationConfig
  ): Promise<ValidationResult | undefined> {
    config.callbacks?.onProgress?.('validation', 0, 'Generating test cases...');

    const testCasesResponse = await this.validator.generateTestCases(
      { ...context, sessionId: generateId() },
      reviewedRules,
      {
        strategies: ['boundary', 'partition', 'property'],
        maxTestsPerRule: 10,
        includeEdgeCases: true,
      }
    );

    if (!testCasesResponse.success || !testCasesResponse.data) {
      result.warnings?.push('Failed to generate test cases');
    }

    const testCases = testCasesResponse.data || [];
    config.callbacks?.onProgress?.('validation', 50, `Generated ${testCases.length} test cases`);

    // Note: In production, you would execute tests against actual systems
    // For now, we simulate validation
    const validationResponse = await this.validator.validateEquivalence(
      { ...context, sessionId: generateId() },
      testCases,
      [], // legacyResults - would come from legacy system execution
      [], // modernResults - would come from modern system execution
      { default: 0.01 }
    );

    config.callbacks?.onProgress?.('validation', 100, 'Validation complete');

    if (validationResponse.success && validationResponse.data) {
      config.callbacks?.onValidationComplete?.(validationResponse.data);
      return validationResponse.data;
    }

    return undefined;
  }

  /**
   * Run analysis phase only
   */
  async analyzeOnly(
    sourceCode: string,
    filename: string,
    language: SourceLanguage,
    projectId: string,
    userId: string
  ): Promise<AnalysisResult | null> {
    const context: AgentContext = {
      projectId,
      sessionId: generateId(),
      userId,
    };

    const response = await this.archeologist.analyzeFile(context, sourceCode, language, filename);
    return response.success ? response.data ?? null : null;
  }

  /**
   * Design architecture from existing analysis
   */
  async designOnly(
    analysis: AnalysisResult,
    targetLanguage: TargetLanguage,
    targetFramework: string,
    projectId: string,
    userId: string
  ): Promise<ArchitectureDesign | null> {
    const context: AgentContext = {
      projectId,
      sessionId: generateId(),
      userId,
    };

    const response = await this.architect.designArchitecture(
      context,
      analysis,
      targetLanguage,
      targetFramework
    );
    return response.success ? response.data ?? null : null;
  }
}

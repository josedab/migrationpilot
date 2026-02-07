/**
 * Cost Estimator
 * Main service for generating migration cost estimates
 */

import type { SourceLanguage } from '@migrationpilot/core';
import type {
  CodeSample,
  CodeMetrics,
  ComplexityScore,
  CostEstimateRequest,
  CostEstimateResult,
  EffortBreakdown,
  CostBreakdown,
  TimelineEstimate,
  ApproachComparison,
  PhaseEffort,
  RoleEffort,
  EstimatorConfig,
  QuickEstimateRequest,
  QuickEstimateResult,
  BatchEstimateRequest,
  BatchEstimateResult,
  FileEstimateResult,
  EstimationOptions,
} from './types/index.js';
import { DEFAULT_ESTIMATOR_CONFIG } from './types/index.js';
import { CodeAnalyzer } from './analyzers/code-analyzer.js';
import { LanguageDetector } from './analyzers/language-detector.js';
import { generateEstimateId } from './utils/id.js';

export class CostEstimatorService {
  private config: EstimatorConfig;
  private codeAnalyzer: CodeAnalyzer;
  private languageDetector: LanguageDetector;

  constructor(config: Partial<EstimatorConfig> = {}) {
    this.config = { ...DEFAULT_ESTIMATOR_CONFIG, ...config };
    this.codeAnalyzer = new CodeAnalyzer();
    this.languageDetector = new LanguageDetector();
  }

  /**
   * Generate a full cost estimate for code samples
   */
  async estimate(request: CostEstimateRequest): Promise<CostEstimateResult> {
    const samples = Array.isArray(request.code) ? request.code : [request.code];
    const options = request.options || {};

    // Aggregate metrics from all samples
    const allMetrics: CodeMetrics[] = [];
    for (const sample of samples) {
      allMetrics.push(this.codeAnalyzer.analyze(sample));
    }

    const metrics = this.aggregateMetrics(allMetrics);

    // Detect language
    const firstSample = samples[0]!;
    const detectedLanguage = request.sourceLanguage
      ? { language: request.sourceLanguage, confidence: 1.0, indicators: ['User specified'] }
      : this.languageDetector.detect(firstSample.content);

    // Calculate complexity
    const complexity = this.codeAnalyzer.calculateComplexity(metrics, detectedLanguage.language);

    // Generate effort estimate
    const effort = this.calculateEffort(metrics, complexity, detectedLanguage.language, options);

    // Generate cost estimate
    const cost = this.calculateCost(effort, options);

    // Generate timeline estimate
    const timeline = this.calculateTimeline(effort, options);

    // Generate comparisons with other approaches
    const comparisons = this.generateComparisons(metrics, effort, cost, detectedLanguage.language);

    // Generate recommendations
    const { recommendations, warnings } = this.generateRecommendations(metrics, complexity, options);

    // Calculate confidence
    const { confidence, confidenceFactors } = this.calculateConfidence(metrics, complexity, detectedLanguage);

    return {
      id: generateEstimateId(),
      timestamp: new Date().toISOString(),
      metrics,
      detectedLanguage,
      complexity,
      effort,
      cost,
      timeline,
      comparisons,
      recommendations,
      warnings,
      confidence,
      confidenceFactors,
    };
  }

  /**
   * Generate a quick estimate (freemium tier)
   */
  async quickEstimate(request: QuickEstimateRequest): Promise<QuickEstimateResult> {
    const sample: CodeSample = { content: request.code, language: request.sourceLanguage };
    const metrics = this.codeAnalyzer.analyze(sample);

    const detectedLanguage = request.sourceLanguage || this.languageDetector.detect(request.code).language;
    const complexity = this.codeAnalyzer.calculateComplexity(metrics, detectedLanguage);

    // Quick effort calculation
    const baseHours = metrics.codeLines * this.config.baseRates[detectedLanguage];
    const adjustedHours = baseHours * this.config.complexityMultipliers[complexity.level];
    const days = adjustedHours / 8;

    // Quick cost range
    const midRate = this.config.hourlyRates.mid ?? 75;
    const seniorRate = this.config.hourlyRates.senior ?? 100;
    const avgHourlyRate = (midRate + seniorRate) / 2;
    const baseCost = adjustedHours * avgHourlyRate;

    return {
      linesOfCode: metrics.codeLines,
      detectedLanguage,
      complexityLevel: complexity.level,
      complexityScore: complexity.overall,
      estimatedDays: {
        min: Math.ceil(days * 0.8),
        max: Math.ceil(days * 1.5),
      },
      estimatedCost: {
        min: Math.round(baseCost * 0.8),
        max: Math.round(baseCost * 1.5 * (1 + this.config.contingencyPercentage / 100)),
        currency: this.config.currency,
      },
      recommendation: this.getQuickRecommendation(complexity.level, metrics.codeLines),
    };
  }

  /**
   * Generate estimates for multiple files (batch)
   */
  async batchEstimate(request: BatchEstimateRequest): Promise<BatchEstimateResult> {
    const options = request.options || {};
    const fileResults: FileEstimateResult[] = [];
    const allMetrics: CodeMetrics[] = [];

    for (const file of request.files) {
      const metrics = this.codeAnalyzer.analyze(file);
      const language = file.language || this.languageDetector.detect(file.content).language;
      const complexity = this.codeAnalyzer.calculateComplexity(metrics, language);

      const baseHours = metrics.codeLines * this.config.baseRates[language];
      const adjustedHours = baseHours * this.config.complexityMultipliers[complexity.level];
      const personDays = adjustedHours / 8;

      const batchMidRate = this.config.hourlyRates.mid ?? 75;
      const batchSeniorRate = this.config.hourlyRates.senior ?? 100;
      const avgHourlyRate = (batchMidRate + batchSeniorRate) / 2;
      const totalCost = adjustedHours * avgHourlyRate * (1 + this.config.contingencyPercentage / 100);

      fileResults.push({
        filename: file.filename || 'unknown',
        metrics,
        complexity,
        effort: { personDays: Math.ceil(personDays) },
        cost: { total: Math.round(totalCost) },
      });

      allMetrics.push(metrics);
    }

    // Aggregate
    const aggregateMetrics = this.aggregateMetrics(allMetrics);
    const primaryLanguage = this.languageDetector.detect(request.files[0]!.content).language;
    const aggregateComplexity = this.codeAnalyzer.calculateComplexity(aggregateMetrics, primaryLanguage);
    const aggregateEffort = this.calculateEffort(aggregateMetrics, aggregateComplexity, primaryLanguage, options);
    const aggregateCost = this.calculateCost(aggregateEffort, options);
    const timeline = this.calculateTimeline(aggregateEffort, options);

    return {
      id: generateEstimateId(),
      timestamp: new Date().toISOString(),
      fileCount: request.files.length,
      totalLines: aggregateMetrics.totalLines,
      aggregateMetrics,
      aggregateComplexity,
      aggregateEffort,
      aggregateCost,
      timeline,
      fileResults,
      recommendations: this.generateRecommendations(aggregateMetrics, aggregateComplexity, options).recommendations,
    };
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private aggregateMetrics(metricsArray: CodeMetrics[]): CodeMetrics {
    return {
      totalLines: metricsArray.reduce((sum, m) => sum + m.totalLines, 0),
      codeLines: metricsArray.reduce((sum, m) => sum + m.codeLines, 0),
      commentLines: metricsArray.reduce((sum, m) => sum + m.commentLines, 0),
      blankLines: metricsArray.reduce((sum, m) => sum + m.blankLines, 0),
      procedures: metricsArray.reduce((sum, m) => sum + m.procedures, 0),
      dataStructures: metricsArray.reduce((sum, m) => sum + m.dataStructures, 0),
      externalCalls: metricsArray.reduce((sum, m) => sum + m.externalCalls, 0),
      cyclomaticComplexity: metricsArray.reduce((sum, m) => sum + m.cyclomaticComplexity, 0),
      maxNestingDepth: Math.max(...metricsArray.map(m => m.maxNestingDepth)),
      uniqueOperations: Math.max(...metricsArray.map(m => m.uniqueOperations)),
    };
  }

  private calculateEffort(
    metrics: CodeMetrics,
    complexity: ComplexityScore,
    language: SourceLanguage,
    options: EstimationOptions
  ): EffortBreakdown {
    const baseHours = metrics.codeLines * this.config.baseRates[language];
    const complexityMultiplier = this.config.complexityMultipliers[complexity.level];

    // Team experience modifier
    const experienceMultipliers = { novice: 1.5, intermediate: 1.0, expert: 0.75 };
    const experienceMultiplier = experienceMultipliers[options.teamExperience || 'intermediate'];

    // Human review modifier
    const reviewMultipliers = { minimal: 0.9, standard: 1.0, comprehensive: 1.3 };
    const reviewMultiplier = reviewMultipliers[options.humanReviewLevel || 'standard'];

    // Optional features modifier
    let featureMultiplier = 1.0;
    if (options.includeTestGeneration) featureMultiplier += 0.2;
    if (options.includeDocumentation) featureMultiplier += 0.1;

    const adjustedHours = baseHours * complexityMultiplier * experienceMultiplier * reviewMultiplier * featureMultiplier;
    const totalPersonDays = adjustedHours / 8;

    // Phase breakdown
    const phases: PhaseEffort[] = [
      { phase: 'Discovery', personDays: Math.ceil(totalPersonDays * 0.05), percentage: 5, parallelizable: true },
      { phase: 'Analysis', personDays: Math.ceil(totalPersonDays * 0.15), percentage: 15, parallelizable: true },
      { phase: 'Design', personDays: Math.ceil(totalPersonDays * 0.10), percentage: 10, parallelizable: false },
      { phase: 'Implementation', personDays: Math.ceil(totalPersonDays * 0.40), percentage: 40, parallelizable: false },
      { phase: 'Testing', personDays: Math.ceil(totalPersonDays * 0.20), percentage: 20, parallelizable: true },
      { phase: 'Deployment', personDays: Math.ceil(totalPersonDays * 0.05), percentage: 5, parallelizable: false },
      { phase: 'Validation', personDays: Math.ceil(totalPersonDays * 0.05), percentage: 5, parallelizable: false },
    ];

    // Role breakdown
    const byRole: RoleEffort[] = [
      { role: 'Business Analyst', personDays: Math.ceil(totalPersonDays * 0.15), skillLevel: 'senior' },
      { role: 'Solution Architect', personDays: Math.ceil(totalPersonDays * 0.10), skillLevel: 'expert' },
      { role: 'Legacy Developer', personDays: Math.ceil(totalPersonDays * 0.10), skillLevel: 'senior' },
      { role: 'Senior Developer', personDays: Math.ceil(totalPersonDays * 0.25), skillLevel: 'senior' },
      { role: 'Developer', personDays: Math.ceil(totalPersonDays * 0.25), skillLevel: 'mid' },
      { role: 'QA Engineer', personDays: Math.ceil(totalPersonDays * 0.15), skillLevel: 'senior' },
    ];

    return {
      totalPersonDays: Math.ceil(totalPersonDays),
      totalPersonWeeks: Math.ceil(totalPersonDays / 5),
      phases,
      byRole,
    };
  }

  private calculateCost(effort: EffortBreakdown, options: EstimationOptions): CostBreakdown {
    const currency = options.currency || this.config.currency;

    // Labor cost
    let labor = 0;
    const defaultRate = this.config.hourlyRates.mid ?? 75;
    for (const role of effort.byRole) {
      const hourlyRate = this.config.hourlyRates[role.skillLevel] ?? defaultRate;
      labor += role.personDays * 8 * hourlyRate;
    }

    // Infrastructure cost
    const infraMultipliers = { simple: 300, standard: 500, enterprise: 1000 };
    const weeklyInfraCost = infraMultipliers[options.deploymentComplexity || 'standard'];
    const infrastructure = effort.totalPersonWeeks * weeklyInfraCost;

    // Tooling cost
    const tooling = effort.totalPersonWeeks * 100;

    // Training cost (2% of labor)
    const training = labor * 0.02;

    // Contingency
    const subtotal = labor + infrastructure + tooling + training;
    const contingency = subtotal * (this.config.contingencyPercentage / 100);

    const total = subtotal + contingency;

    return {
      currency,
      labor: Math.round(labor),
      infrastructure: Math.round(infrastructure),
      tooling: Math.round(tooling),
      training: Math.round(training),
      contingency: Math.round(contingency),
      total: Math.round(total),
      perLineOfCode: 0, // Will be calculated below
      perProcedure: 0,
    };
  }

  private calculateTimeline(effort: EffortBreakdown, options: EstimationOptions): TimelineEstimate {
    // Assuming 2-3 person team
    const teamSize = options.deploymentComplexity === 'enterprise' ? 4 : 2;

    const parallelDays = effort.phases
      .filter(p => p.parallelizable)
      .reduce((sum, p) => sum + p.personDays, 0) / teamSize;

    const sequentialDays = effort.phases
      .filter(p => !p.parallelizable)
      .reduce((sum, p) => sum + p.personDays, 0);

    const realistic = Math.ceil(parallelDays + sequentialDays);
    const optimistic = Math.ceil(realistic * 0.75);
    const pessimistic = Math.ceil(realistic * 1.5);

    const factors: string[] = [];
    if (options.humanReviewLevel === 'comprehensive') {
      factors.push('Comprehensive human review adds calendar time');
    }
    if (options.deploymentComplexity === 'enterprise') {
      factors.push('Enterprise deployment requires additional validation');
    }

    return {
      optimistic,
      realistic,
      pessimistic,
      recommended: realistic,
      factors,
    };
  }

  private generateComparisons(
    _metrics: CodeMetrics,
    effort: EffortBreakdown,
    cost: CostBreakdown,
    _language: SourceLanguage
  ): ApproachComparison[] {
    return [
      {
        approach: 'migrationpilot',
        label: 'MigrationPilot (AI-Assisted)',
        estimatedCost: cost.total,
        estimatedDays: effort.totalPersonDays,
        codeQuality: 'excellent',
        riskLevel: 'low',
        pros: [
          'Clean, idiomatic modern code',
          'Business rules preserved and documented',
          'Automated equivalence validation',
          'Human-in-the-loop oversight',
        ],
        cons: [
          'Requires AI API access',
          'Newer technology',
        ],
      },
      {
        approach: 'manual_rewrite',
        label: 'Manual Rewrite',
        estimatedCost: cost.total * 4,
        estimatedDays: effort.totalPersonDays * 5,
        codeQuality: 'excellent',
        riskLevel: 'very_high',
        pros: [
          'Full control over architecture',
          'Clean codebase',
        ],
        cons: [
          'Extremely time-consuming',
          'High cost',
          '50%+ failure rate',
          'Business logic often lost',
        ],
      },
      {
        approach: 'transpiler',
        label: 'Traditional Transpiler',
        estimatedCost: Math.round(cost.total * 0.3),
        estimatedDays: Math.ceil(effort.totalPersonDays * 0.1),
        codeQuality: 'poor',
        riskLevel: 'medium',
        pros: [
          'Very fast',
          'Low upfront cost',
          'Behavior preserved',
        ],
        cons: [
          'Produces unreadable code',
          'No architectural improvement',
          'High maintenance costs',
          'Developers refuse to work with output',
        ],
      },
      {
        approach: 'ai_assisted',
        label: 'Generic AI Conversion',
        estimatedCost: Math.round(cost.total * 0.7),
        estimatedDays: Math.ceil(effort.totalPersonDays * 0.6),
        codeQuality: 'good',
        riskLevel: 'medium',
        pros: [
          'Faster than manual',
          'Better code than transpilers',
        ],
        cons: [
          'Inconsistent quality',
          'No validation framework',
          'Limited business rule preservation',
          'No human oversight system',
        ],
      },
    ];
  }

  private generateRecommendations(
    metrics: CodeMetrics,
    complexity: ComplexityScore,
    options: EstimationOptions
  ): { recommendations: string[]; warnings: string[] } {
    const recommendations: string[] = [];
    const warnings: string[] = [];

    // Size-based recommendations
    if (metrics.codeLines > 50000) {
      recommendations.push('Consider breaking the migration into phases using the Strangler Fig pattern');
      recommendations.push('Prioritize modules by business criticality for incremental delivery');
    }

    // Complexity-based recommendations
    if (complexity.level === 'extreme' || complexity.level === 'very_high') {
      recommendations.push('Engage subject matter experts for business rule review');
      recommendations.push('Plan for comprehensive human review checkpoints');
      warnings.push('High complexity may require additional analysis time');
    }

    // External calls
    if (metrics.externalCalls > 20) {
      recommendations.push('Document all external integrations before migration begins');
      recommendations.push('Consider creating an integration test environment');
      warnings.push('Multiple external dependencies increase integration risk');
    }

    // Data structures
    if (metrics.dataStructures > 30) {
      recommendations.push('Create a data dictionary mapping legacy to modern structures');
    }

    // Review level
    if (!options.humanReviewLevel || options.humanReviewLevel === 'minimal') {
      warnings.push('Minimal human review increases risk for business-critical systems');
    }

    // General recommendations
    recommendations.push('Start with a pilot migration of 1-2 modules to validate approach');
    recommendations.push('Establish baseline test coverage before migration');

    return { recommendations, warnings };
  }

  private calculateConfidence(
    metrics: CodeMetrics,
    complexity: ComplexityScore,
    detection: { confidence: number }
  ): { confidence: number; confidenceFactors: string[] } {
    let confidence = 0.85; // Base confidence
    const factors: string[] = [];

    // Language detection confidence
    if (detection.confidence < 0.7) {
      confidence *= 0.9;
      factors.push('Language detection uncertain - verify source language');
    } else {
      factors.push('Language confidently detected');
    }

    // Sample size
    if (metrics.codeLines < 100) {
      confidence *= 0.8;
      factors.push('Small sample size - estimates may vary');
    } else if (metrics.codeLines > 1000) {
      factors.push('Adequate code sample for estimation');
    }

    // Complexity
    if (complexity.level === 'extreme') {
      confidence *= 0.85;
      factors.push('Extreme complexity introduces estimation uncertainty');
    }

    // Comment ratio (documentation)
    const commentRatio = metrics.commentLines / (metrics.codeLines || 1);
    if (commentRatio > 0.1) {
      confidence *= 1.05;
      factors.push('Good documentation improves understanding');
    }

    confidence = Math.min(0.95, Math.max(0.5, confidence));

    return { confidence, confidenceFactors: factors };
  }

  private getQuickRecommendation(level: string, lines: number): string {
    if (lines < 500 && (level === 'trivial' || level === 'low')) {
      return 'This is a small, straightforward migration. Consider using MigrationPilot for clean, documented code.';
    }
    if (level === 'extreme' || level === 'very_high') {
      return 'This is a complex codebase requiring careful analysis. We recommend a detailed assessment with MigrationPilot professional services.';
    }
    if (lines > 10000) {
      return 'Large codebase detected. Recommend phased migration with Strangler Fig pattern for risk mitigation.';
    }
    return 'MigrationPilot can efficiently modernize this codebase while preserving business logic and ensuring code quality.';
  }
}

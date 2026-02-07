/**
 * Cost Estimator Types
 * Types for self-service migration cost estimation
 */

import type { SourceLanguage, TargetLanguage } from '@migrationpilot/core';

// ============================================================================
// CODE ANALYSIS TYPES
// ============================================================================

export interface CodeSample {
  content: string;
  filename?: string;
  language?: SourceLanguage;
}

export interface CodeMetrics {
  totalLines: number;
  codeLines: number;
  commentLines: number;
  blankLines: number;
  procedures: number;
  dataStructures: number;
  externalCalls: number;
  cyclomaticComplexity: number;
  maxNestingDepth: number;
  uniqueOperations: number;
}

export interface LanguageDetectionResult {
  language: SourceLanguage;
  confidence: number;
  indicators: string[];
}

// ============================================================================
// COMPLEXITY SCORING TYPES
// ============================================================================

export type ComplexityLevel = 'trivial' | 'low' | 'medium' | 'high' | 'very_high' | 'extreme';

export interface ComplexityScore {
  overall: number; // 0-100
  level: ComplexityLevel;
  breakdown: ComplexityBreakdown;
  factors: ComplexityFactor[];
}

export interface ComplexityBreakdown {
  codeVolume: number;
  logicComplexity: number;
  dataComplexity: number;
  integrationComplexity: number;
  languageComplexity: number;
}

export interface ComplexityFactor {
  name: string;
  description: string;
  impact: 'low' | 'medium' | 'high';
  score: number;
  recommendation?: string;
}

// ============================================================================
// COST ESTIMATION TYPES
// ============================================================================

export interface CostEstimateRequest {
  code: CodeSample | CodeSample[];
  sourceLanguage?: SourceLanguage;
  targetLanguage?: TargetLanguage;
  targetFramework?: string;
  options?: EstimationOptions;
}

export interface EstimationOptions {
  includeTestGeneration?: boolean;
  includeDocumentation?: boolean;
  humanReviewLevel?: 'minimal' | 'standard' | 'comprehensive';
  deploymentComplexity?: 'simple' | 'standard' | 'enterprise';
  teamExperience?: 'novice' | 'intermediate' | 'expert';
  currency?: string;
}

export interface CostEstimateResult {
  id: string;
  timestamp: string;

  // Input analysis
  metrics: CodeMetrics;
  detectedLanguage: LanguageDetectionResult;
  complexity: ComplexityScore;

  // Estimates
  effort: EffortBreakdown;
  cost: CostBreakdown;
  timeline: TimelineEstimate;

  // Comparisons
  comparisons: ApproachComparison[];

  // Recommendations
  recommendations: string[];
  warnings: string[];

  // Confidence
  confidence: number;
  confidenceFactors: string[];
}

export interface EffortBreakdown {
  totalPersonDays: number;
  totalPersonWeeks: number;
  phases: PhaseEffort[];
  byRole: RoleEffort[];
}

export interface PhaseEffort {
  phase: string;
  personDays: number;
  percentage: number;
  parallelizable: boolean;
}

export interface RoleEffort {
  role: string;
  personDays: number;
  skillLevel: 'junior' | 'mid' | 'senior' | 'expert';
}

export interface CostBreakdown {
  currency: string;
  labor: number;
  infrastructure: number;
  tooling: number;
  training: number;
  contingency: number;
  total: number;

  // Per-unit costs
  perLineOfCode: number;
  perProcedure: number;
}

export interface TimelineEstimate {
  optimistic: number; // days
  realistic: number;
  pessimistic: number;
  recommended: number;
  factors: string[];
}

export interface ApproachComparison {
  approach: 'manual_rewrite' | 'transpiler' | 'ai_assisted' | 'migrationpilot';
  label: string;
  estimatedCost: number;
  estimatedDays: number;
  codeQuality: 'poor' | 'fair' | 'good' | 'excellent';
  riskLevel: 'low' | 'medium' | 'high' | 'very_high';
  pros: string[];
  cons: string[];
}

// ============================================================================
// QUICK ESTIMATE TYPES (for freemium tier)
// ============================================================================

export interface QuickEstimateRequest {
  code: string;
  sourceLanguage?: SourceLanguage;
}

export interface QuickEstimateResult {
  linesOfCode: number;
  detectedLanguage: SourceLanguage;
  complexityLevel: ComplexityLevel;
  complexityScore: number;
  estimatedDays: { min: number; max: number };
  estimatedCost: { min: number; max: number; currency: string };
  recommendation: string;
}

// ============================================================================
// BATCH ESTIMATION TYPES
// ============================================================================

export interface BatchEstimateRequest {
  files: CodeSample[];
  options?: EstimationOptions;
}

export interface BatchEstimateResult {
  id: string;
  timestamp: string;
  fileCount: number;
  totalLines: number;
  aggregateMetrics: CodeMetrics;
  aggregateComplexity: ComplexityScore;
  aggregateEffort: EffortBreakdown;
  aggregateCost: CostBreakdown;
  timeline: TimelineEstimate;
  fileResults: FileEstimateResult[];
  recommendations: string[];
}

export interface FileEstimateResult {
  filename: string;
  metrics: CodeMetrics;
  complexity: ComplexityScore;
  effort: { personDays: number };
  cost: { total: number };
}

// ============================================================================
// CONFIGURATION TYPES
// ============================================================================

export interface EstimatorConfig {
  // Base rates (person-hours per line of code)
  baseRates: Record<SourceLanguage, number>;

  // Complexity multipliers
  complexityMultipliers: Record<ComplexityLevel, number>;

  // Language migration difficulty
  migrationDifficulty: Record<string, number>; // "cobol->java": 1.3

  // Hourly rates by role
  hourlyRates: Record<string, number>;

  // Default currency
  currency: string;

  // Contingency percentage
  contingencyPercentage: number;
}

export const DEFAULT_ESTIMATOR_CONFIG: EstimatorConfig = {
  baseRates: {
    cobol: 0.08,      // 0.08 hours per LOC
    fortran: 0.07,
    vb6: 0.06,
    vba: 0.05,
    'java-legacy': 0.04,
  },
  complexityMultipliers: {
    trivial: 0.5,
    low: 0.75,
    medium: 1.0,
    high: 1.5,
    very_high: 2.0,
    extreme: 3.0,
  },
  migrationDifficulty: {
    'cobol->java': 1.3,
    'cobol->python': 1.4,
    'cobol->typescript': 1.5,
    'cobol->go': 1.4,
    'cobol->csharp': 1.3,
    'fortran->python': 1.1,
    'fortran->java': 1.3,
    'vb6->typescript': 1.2,
    'vb6->csharp': 1.1,
    'vba->typescript': 1.1,
    'java-legacy->java': 0.8,
  },
  hourlyRates: {
    junior: 50,
    mid: 75,
    senior: 100,
    expert: 150,
    architect: 175,
  },
  currency: 'USD',
  contingencyPercentage: 20,
};

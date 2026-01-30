/**
 * Migration Planner Types
 * Defines types for migration planning, risk assessment, and timeline estimation
 */

import type { KnowledgeNode, KnowledgeEdge } from '@migrationpilot/core';

// ============================================================================
// Module & Component Types
// ============================================================================

export type ModuleStatus = 'not-started' | 'in-progress' | 'completed' | 'blocked' | 'deferred';
export type RiskLevel = 'low' | 'medium' | 'high' | 'critical';
export type MigrationPhase = 'discovery' | 'analysis' | 'design' | 'implementation' | 'testing' | 'deployment' | 'validation';

export interface ModuleMetrics {
  linesOfCode: number;
  cyclomaticComplexity: number;
  numberOfProcedures: number;
  numberOfDataStructures: number;
  numberOfExternalCalls: number;
  numberOfBusinessRules: number;
  testCoverage?: number;
  documentationCoverage?: number;
}

export interface Module {
  id: string;
  name: string;
  path: string;
  type: 'program' | 'copybook' | 'subroutine' | 'library' | 'service';
  language: 'cobol' | 'fortran' | 'vb6' | 'java' | 'mixed';
  status: ModuleStatus;
  metrics: ModuleMetrics;
  dependencies: string[];
  dependents: string[];
  businessCriticality: 'low' | 'medium' | 'high' | 'critical';
  dataOwners?: string[];
  lastModified?: string;
  technicalDebt?: number;
}

// ============================================================================
// Risk Assessment Types
// ============================================================================

export interface RiskFactor {
  id: string;
  category: 'technical' | 'business' | 'resource' | 'schedule' | 'integration';
  name: string;
  description: string;
  severity: RiskLevel;
  probability: number; // 0-1
  impact: number; // 0-1
  score: number; // probability * impact * severity weight
  mitigations: RiskMitigation[];
  affectedModules: string[];
}

export interface RiskMitigation {
  id: string;
  strategy: 'avoid' | 'transfer' | 'mitigate' | 'accept';
  description: string;
  effort: 'low' | 'medium' | 'high';
  effectiveness: number; // 0-1
  implemented: boolean;
}

export interface RiskAssessment {
  moduleId: string;
  overallRisk: RiskLevel;
  riskScore: number;
  factors: RiskFactor[];
  recommendations: string[];
  assessedAt: string;
}

// ============================================================================
// Cost & Effort Estimation Types
// ============================================================================

export interface EffortEstimate {
  moduleId: string;
  phases: PhaseEstimate[];
  totalPersonDays: number;
  totalPersonWeeks: number;
  confidence: number; // 0-1
  assumptions: string[];
  adjustmentFactors: AdjustmentFactor[];
}

export interface PhaseEstimate {
  phase: MigrationPhase;
  personDays: number;
  parallelizable: boolean;
  dependencies: string[];
  skills: string[];
  resources: ResourceRequirement[];
}

export interface AdjustmentFactor {
  name: string;
  description: string;
  multiplier: number;
  applied: boolean;
}

export interface ResourceRequirement {
  role: string;
  skillLevel: 'junior' | 'mid' | 'senior' | 'expert';
  allocation: number; // percentage
  duration: number; // days
}

export interface CostEstimate {
  moduleId: string;
  effort: EffortEstimate;
  laborCost: number;
  infrastructureCost: number;
  toolingCost: number;
  trainingCost: number;
  contingency: number;
  totalCost: number;
  currency: string;
}

// ============================================================================
// Migration Sequence Types
// ============================================================================

export interface MigrationWave {
  id: string;
  name: string;
  sequence: number;
  modules: string[];
  startDate?: string;
  endDate?: string;
  status: 'planned' | 'in-progress' | 'completed' | 'delayed';
  dependencies: string[]; // wave IDs that must complete before this one
  totalEffort: EffortEstimate;
  risks: RiskAssessment[];
  milestones: Milestone[];
}

export interface Milestone {
  id: string;
  name: string;
  description: string;
  dueDate?: string;
  completedDate?: string;
  status: 'pending' | 'completed' | 'overdue' | 'at-risk';
  deliverables: string[];
  criteria: string[];
}

export interface MigrationSequence {
  modules: string[];
  reason: string;
  score: number;
  constraints: SequenceConstraint[];
}

export interface SequenceConstraint {
  type: 'dependency' | 'resource' | 'business' | 'technical';
  description: string;
  moduleIds: string[];
  priority: 'must' | 'should' | 'nice-to-have';
}

// ============================================================================
// Migration Plan Types
// ============================================================================

export interface MigrationStrategy {
  type: 'strangler-fig' | 'big-bang' | 'parallel-run' | 'pilot';
  description: string;
  pros: string[];
  cons: string[];
  suitability: number; // 0-1
  recommendedForModules: string[];
}

export interface MigrationPlan {
  id: string;
  projectId: string;
  name: string;
  version: number;
  status: 'draft' | 'approved' | 'active' | 'completed' | 'archived';
  
  // Strategy
  strategy: MigrationStrategy;
  
  // Modules
  modules: Module[];
  moduleCount: number;
  
  // Sequencing
  waves: MigrationWave[];
  optimalSequence: MigrationSequence;
  alternativeSequences: MigrationSequence[];
  
  // Estimates
  totalEffort: EffortEstimate;
  totalCost: CostEstimate;
  
  // Risk
  overallRisk: RiskLevel;
  riskFactors: RiskFactor[];
  
  // Timeline
  estimatedStartDate?: string;
  estimatedEndDate?: string;
  estimatedDuration: number; // days
  
  // Metadata
  createdAt: string;
  updatedAt: string;
  createdBy?: string;
  approvedBy?: string;
  approvedAt?: string;
  
  // Configuration
  config: PlannerConfig;
}

export interface PlannerConfig {
  // Effort estimation parameters
  baseEffortPerLOC: number; // person-hours per line of code
  complexityMultipliers: Record<string, number>;
  
  // Risk thresholds
  riskThresholds: {
    low: number;
    medium: number;
    high: number;
  };
  
  // Sequencing preferences
  sequencing: {
    maxModulesPerWave: number;
    preferParallelization: boolean;
    minimizeRisk: boolean;
    respectDependencies: boolean;
  };
  
  // Cost parameters
  cost: {
    hourlyRates: Record<string, number>;
    currency: string;
    contingencyPercentage: number;
  };
  
  // Custom weights for scoring
  weights: {
    complexity: number;
    criticality: number;
    dependencies: number;
    risk: number;
  };
}

export const DEFAULT_PLANNER_CONFIG: PlannerConfig = {
  baseEffortPerLOC: 0.5,
  complexityMultipliers: {
    low: 0.8,
    medium: 1.0,
    high: 1.5,
    veryHigh: 2.0,
  },
  riskThresholds: {
    low: 0.25,
    medium: 0.5,
    high: 0.75,
  },
  sequencing: {
    maxModulesPerWave: 10,
    preferParallelization: true,
    minimizeRisk: true,
    respectDependencies: true,
  },
  cost: {
    hourlyRates: {
      junior: 50,
      mid: 75,
      senior: 100,
      expert: 150,
    },
    currency: 'USD',
    contingencyPercentage: 20,
  },
  weights: {
    complexity: 0.3,
    criticality: 0.25,
    dependencies: 0.25,
    risk: 0.2,
  },
};

// ============================================================================
// Analysis Result Types
// ============================================================================

export interface DependencyAnalysis {
  moduleId: string;
  directDependencies: string[];
  transitiveDependencies: string[];
  directDependents: string[];
  transitiveDependents: string[];
  circularDependencies: string[][];
  couplingScore: number;
  afferentCoupling: number; // incoming dependencies
  efferentCoupling: number; // outgoing dependencies
  instability: number; // efferent / (afferent + efferent)
}

export interface CriticalPathAnalysis {
  criticalPath: string[];
  pathLength: number;
  bottlenecks: Bottleneck[];
  parallelizableBranches: string[][];
  totalSequentialEffort: number;
  totalParallelEffort: number;
  parallelizationFactor: number;
}

export interface Bottleneck {
  moduleId: string;
  reason: string;
  impact: number;
  mitigations: string[];
}

// ============================================================================
// Interface Types
// ============================================================================

export interface IMigrationPlanner {
  analyze(modules: Module[]): Promise<MigrationPlan>;
  estimateEffort(module: Module): EffortEstimate;
  assessRisk(module: Module, dependencies: DependencyAnalysis): RiskAssessment;
  suggestSequence(modules: Module[], dependencies: Map<string, DependencyAnalysis>): MigrationSequence;
  generateWaves(sequence: MigrationSequence, config: PlannerConfig): MigrationWave[];
  updatePlan(plan: MigrationPlan, updates: Partial<MigrationPlan>): MigrationPlan;
}

export interface IDependencyAnalyzer {
  analyze(moduleId: string, graph: { nodes: Map<string, KnowledgeNode>; edges: Map<string, KnowledgeEdge> }): DependencyAnalysis;
  findCircularDependencies(modules: string[], dependencies: Map<string, string[]>): string[][];
  calculateCoupling(moduleId: string, dependencies: DependencyAnalysis): number;
}

export interface IRiskAnalyzer {
  assess(module: Module, context: RiskContext): RiskAssessment;
  identifyFactors(module: Module): RiskFactor[];
  scoreFactor(factor: RiskFactor): number;
  suggestMitigations(factors: RiskFactor[]): RiskMitigation[];
}

export interface RiskContext {
  projectTimeline?: { start: string; end: string };
  availableResources?: ResourceRequirement[];
  businessConstraints?: string[];
  technicalConstraints?: string[];
  previousMigrations?: { moduleId: string; success: boolean; lessons: string[] }[];
}

export interface IEffortEstimator {
  estimate(module: Module, config: PlannerConfig): EffortEstimate;
  estimatePhase(module: Module, phase: MigrationPhase, config: PlannerConfig): PhaseEstimate;
  applyAdjustments(base: EffortEstimate, factors: AdjustmentFactor[]): EffortEstimate;
}

export interface ICostEstimator {
  estimate(effort: EffortEstimate, config: PlannerConfig): CostEstimate;
  calculateLaborCost(phases: PhaseEstimate[], rates: Record<string, number>): number;
  addContingency(cost: CostEstimate, percentage: number): CostEstimate;
}

export interface ISequenceOptimizer {
  optimize(modules: Module[], dependencies: Map<string, DependencyAnalysis>, config: PlannerConfig): MigrationSequence;
  findCriticalPath(modules: Module[], dependencies: Map<string, DependencyAnalysis>): CriticalPathAnalysis;
  generateAlternatives(optimal: MigrationSequence, count: number): MigrationSequence[];
}

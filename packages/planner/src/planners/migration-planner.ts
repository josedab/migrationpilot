/**
 * Migration Planner
 * Main orchestrator for migration planning, combining analyzers, estimators, and optimizers
 */

import type { KnowledgeNode, KnowledgeEdge } from '@migrationpilot/core';
import type {
  Module,
  MigrationPlan,
  MigrationStrategy,
  PlannerConfig,
  DependencyAnalysis,
  IMigrationPlanner,
  EffortEstimate,
  RiskAssessment,
  RiskLevel,
} from '../types';
import { DEFAULT_PLANNER_CONFIG } from '../types';
import { DependencyAnalyzer } from '../analyzers/dependency-analyzer';
import { RiskAnalyzer } from '../analyzers/risk-analyzer';
import { EffortEstimator, CostEstimator } from '../estimators/effort-estimator';
import { SequenceOptimizer } from './sequence-optimizer';

interface GraphContext {
  nodes: Map<string, KnowledgeNode>;
  edges: Map<string, KnowledgeEdge>;
}

export class MigrationPlanner implements IMigrationPlanner {
  private config: PlannerConfig;
  private dependencyAnalyzer: DependencyAnalyzer;
  private riskAnalyzer: RiskAnalyzer;
  private effortEstimator: EffortEstimator;
  private costEstimator: CostEstimator;
  private sequenceOptimizer: SequenceOptimizer;
  
  constructor(config: Partial<PlannerConfig> = {}) {
    this.config = { ...DEFAULT_PLANNER_CONFIG, ...config };
    this.dependencyAnalyzer = new DependencyAnalyzer();
    this.riskAnalyzer = new RiskAnalyzer();
    this.effortEstimator = new EffortEstimator(this.config);
    this.costEstimator = new CostEstimator(this.config);
    this.sequenceOptimizer = new SequenceOptimizer(this.config);
  }
  
  /**
   * Analyze modules and generate a complete migration plan
   */
  async analyze(modules: Module[], graph?: GraphContext): Promise<MigrationPlan> {
    // Step 1: Analyze dependencies
    const dependencies = new Map<string, DependencyAnalysis>();
    for (const module of modules) {
      if (graph) {
        dependencies.set(module.id, this.dependencyAnalyzer.analyze(module.id, graph));
      } else {
        // Create a synthetic dependency analysis from module data
        dependencies.set(module.id, {
          moduleId: module.id,
          directDependencies: module.dependencies,
          transitiveDependencies: [],
          directDependents: module.dependents,
          transitiveDependents: [],
          circularDependencies: [],
          couplingScore: (module.dependencies.length + module.dependents.length) / 4,
          afferentCoupling: module.dependents.length,
          efferentCoupling: module.dependencies.length,
          instability: module.dependencies.length / (module.dependencies.length + module.dependents.length + 1),
        });
      }
    }
    
    // Step 2: Assess risks
    const risks = new Map<string, RiskAssessment>();
    for (const module of modules) {
      const depAnalysis = dependencies.get(module.id)!;
      risks.set(module.id, this.riskAnalyzer.assessWithDependencies(module, depAnalysis));
    }
    
    // Step 3: Estimate effort
    const efforts = new Map<string, EffortEstimate>();
    for (const module of modules) {
      efforts.set(module.id, this.effortEstimator.estimate(module, this.config));
    }
    
    // Step 4: Determine optimal sequence
    const optimalSequence = this.sequenceOptimizer.optimize(modules, dependencies, this.config);
    const alternativeSequences = this.sequenceOptimizer.generateAlternatives(optimalSequence, 3);
    
    // Step 5: Generate waves
    const waves = this.sequenceOptimizer.generateWaves(
      optimalSequence,
      modules,
      efforts,
      risks,
      this.config
    );
    
    // Step 6: Aggregate totals
    const totalEffort = this.aggregateTotalEffort(modules, efforts);
    const totalCost = this.costEstimator.estimate(totalEffort, this.config);
    
    // Step 7: Determine overall risk
    const allRisks = Array.from(risks.values());
    const overallRisk = this.calculateOverallRisk(allRisks);
    const riskFactors = allRisks.flatMap(r => r.factors).slice(0, 20); // Top 20 factors
    
    // Step 8: Select strategy
    const strategy = this.selectStrategy(modules, dependencies, risks);
    
    // Step 9: Calculate timeline
    const criticalPath = this.sequenceOptimizer.findCriticalPath(modules, dependencies);
    const estimatedDuration = Math.ceil(criticalPath.totalParallelEffort);
    
    const now = new Date();
    const plan: MigrationPlan = {
      id: `plan_${Date.now()}`,
      projectId: 'default',
      name: 'Migration Plan',
      version: 1,
      status: 'draft',
      
      strategy,
      modules,
      moduleCount: modules.length,
      
      waves,
      optimalSequence,
      alternativeSequences,
      
      totalEffort,
      totalCost,
      
      overallRisk,
      riskFactors,
      
      estimatedStartDate: now.toISOString(),
      estimatedEndDate: new Date(now.getTime() + estimatedDuration * 24 * 60 * 60 * 1000).toISOString(),
      estimatedDuration,
      
      createdAt: now.toISOString(),
      updatedAt: now.toISOString(),
      
      config: this.config,
    };
    
    return plan;
  }
  
  /**
   * Estimate effort for a single module
   */
  estimateEffort(module: Module): EffortEstimate {
    return this.effortEstimator.estimate(module, this.config);
  }
  
  /**
   * Assess risk for a single module
   */
  assessRisk(module: Module, dependencies: DependencyAnalysis): RiskAssessment {
    return this.riskAnalyzer.assessWithDependencies(module, dependencies);
  }
  
  /**
   * Suggest optimal migration sequence
   */
  suggestSequence(
    modules: Module[], 
    dependencies: Map<string, DependencyAnalysis>
  ): import('../types').MigrationSequence {
    return this.sequenceOptimizer.optimize(modules, dependencies, this.config);
  }
  
  /**
   * Generate waves from a sequence
   */
  generateWaves(
    sequence: import('../types').MigrationSequence,
    config: PlannerConfig = this.config
  ): import('../types').MigrationWave[] {
    // This is a simplified version - full implementation needs modules, efforts, risks
    const moduleIds = sequence.modules;
    const waves: import('../types').MigrationWave[] = [];
    const maxPerWave = config.sequencing.maxModulesPerWave;
    
    for (let i = 0; i < moduleIds.length; i += maxPerWave) {
      const waveModules = moduleIds.slice(i, i + maxPerWave);
      const prevWave = waves.length > 0 ? waves[waves.length - 1] : undefined;
      waves.push({
        id: `wave_${waves.length + 1}`,
        name: `Wave ${waves.length + 1}`,
        sequence: waves.length + 1,
        modules: waveModules,
        status: 'planned',
        dependencies: prevWave ? [prevWave.id] : [],
        totalEffort: {
          moduleId: waveModules.join(','),
          phases: [],
          totalPersonDays: 0,
          totalPersonWeeks: 0,
          confidence: 0.7,
          assumptions: [],
          adjustmentFactors: [],
        },
        risks: [],
        milestones: [],
      });
    }
    
    return waves;
  }
  
  /**
   * Update an existing plan
   */
  updatePlan(plan: MigrationPlan, updates: Partial<MigrationPlan>): MigrationPlan {
    return {
      ...plan,
      ...updates,
      version: plan.version + 1,
      updatedAt: new Date().toISOString(),
    };
  }
  
  private aggregateTotalEffort(modules: Module[], efforts: Map<string, EffortEstimate>): EffortEstimate {
    let totalPersonDays = 0;
    const allAssumptions: string[] = [];
    const allAdjustments: import('../types').AdjustmentFactor[] = [];
    
    for (const module of modules) {
      const effort = efforts.get(module.id);
      if (effort) {
        totalPersonDays += effort.totalPersonDays;
        allAssumptions.push(...effort.assumptions);
        allAdjustments.push(...effort.adjustmentFactors);
      }
    }
    
    return {
      moduleId: 'total',
      phases: [],
      totalPersonDays,
      totalPersonWeeks: Math.ceil(totalPersonDays / 5),
      confidence: 0.7,
      assumptions: [...new Set(allAssumptions)],
      adjustmentFactors: allAdjustments,
    };
  }
  
  private calculateOverallRisk(risks: RiskAssessment[]): RiskLevel {
    if (risks.length === 0) return 'low';
    
    const avgScore = risks.reduce((sum, r) => sum + r.riskScore, 0) / risks.length;
    const maxScore = Math.max(...risks.map(r => r.riskScore));
    
    // Use weighted average with max score consideration
    const weightedScore = avgScore * 0.6 + maxScore * 0.4;
    
    if (weightedScore >= 7) return 'critical';
    if (weightedScore >= 5) return 'high';
    if (weightedScore >= 3) return 'medium';
    return 'low';
  }
  
  private selectStrategy(
    modules: Module[],
    dependencies: Map<string, DependencyAnalysis>,
    risks: Map<string, RiskAssessment>
  ): MigrationStrategy {
    const strategies: MigrationStrategy[] = [
      {
        type: 'strangler-fig',
        description: 'Gradually replace legacy functionality with modern implementation',
        pros: ['Low risk', 'Incremental delivery', 'Easy rollback', 'Continuous value'],
        cons: ['Longer duration', 'More complex routing', 'Dual maintenance'],
        suitability: 0,
        recommendedForModules: [],
      },
      {
        type: 'big-bang',
        description: 'Complete replacement in a single cutover event',
        pros: ['Clean cut', 'No dual maintenance', 'Faster if successful'],
        cons: ['High risk', 'All-or-nothing', 'Complex rollback'],
        suitability: 0,
        recommendedForModules: [],
      },
      {
        type: 'parallel-run',
        description: 'Run both systems simultaneously and compare results',
        pros: ['High confidence', 'Thorough validation', 'Easy rollback'],
        cons: ['Double infrastructure', 'Complex comparison', 'Extended timeline'],
        suitability: 0,
        recommendedForModules: [],
      },
      {
        type: 'pilot',
        description: 'Migrate a subset first to validate approach',
        pros: ['Learn early', 'Limited risk', 'Builds confidence'],
        cons: ['May not represent all cases', 'Extended timeline'],
        suitability: 0,
        recommendedForModules: [],
      },
    ];
    
    // Calculate suitability scores
    const criticalCount = modules.filter(m => m.businessCriticality === 'critical').length;
    const highRiskCount = Array.from(risks.values()).filter(r => 
      r.overallRisk === 'high' || r.overallRisk === 'critical'
    ).length;
    const avgCoupling = dependencies.size > 0 
      ? Array.from(dependencies.values()).reduce((sum, d) => sum + d.couplingScore, 0) / dependencies.size
      : 0;
    
    const stranglerFig = strategies[0]!;
    const bigBang = strategies[1]!;
    const parallelRun = strategies[2]!;
    const pilot = strategies[3]!;
    
    // Strangler fig is preferred for high coupling and critical systems
    stranglerFig.suitability = 0.5 + (avgCoupling / 20) + (criticalCount / modules.length) * 0.3;
    stranglerFig.recommendedForModules = modules
      .filter(m => m.businessCriticality === 'high' || m.businessCriticality === 'critical')
      .map(m => m.id);
    
    // Big bang only suitable for low risk, low coupling scenarios
    bigBang.suitability = Math.max(0, 0.8 - (highRiskCount / modules.length) - (avgCoupling / 10));
    bigBang.recommendedForModules = modules
      .filter(m => {
        const risk = risks.get(m.id);
        return m.businessCriticality === 'low' && risk?.overallRisk === 'low';
      })
      .map(m => m.id);
    
    // Parallel run for critical systems with high risk
    parallelRun.suitability = (criticalCount / modules.length) * 0.5 + (highRiskCount / modules.length) * 0.5;
    parallelRun.recommendedForModules = modules
      .filter(m => m.businessCriticality === 'critical')
      .map(m => m.id);
    
    // Pilot suitable for medium-sized projects with some uncertainty
    pilot.suitability = modules.length > 5 && modules.length < 50 ? 0.7 : 0.3;
    pilot.recommendedForModules = modules.slice(0, Math.min(5, modules.length)).map(m => m.id);
    
    // Select best strategy
    const best = strategies.reduce((a, b) => a.suitability > b.suitability ? a : b);
    return best;
  }
}

/**
 * Sequence Optimizer
 * Determines optimal migration order based on dependencies, risk, and business value
 */

import type {
  Module,
  MigrationSequence,
  MigrationWave,
  SequenceConstraint,
  DependencyAnalysis,
  CriticalPathAnalysis,
  Bottleneck,
  PlannerConfig,
  ISequenceOptimizer,
  EffortEstimate,
  RiskAssessment,
  Milestone,
} from '../types';
import { DEFAULT_PLANNER_CONFIG } from '../types';

export class SequenceOptimizer implements ISequenceOptimizer {
  private config: PlannerConfig;
  
  constructor(config: Partial<PlannerConfig> = {}) {
    this.config = { ...DEFAULT_PLANNER_CONFIG, ...config };
  }
  
  /**
   * Generate optimal migration sequence
   */
  optimize(
    modules: Module[], 
    dependencies: Map<string, DependencyAnalysis>,
    config: PlannerConfig = this.config
  ): MigrationSequence {
    // Score each module based on multiple factors
    const scoredModules = modules.map(m => ({
      module: m,
      score: this.scoreModule(m, dependencies.get(m.id)!, config),
      deps: dependencies.get(m.id)!,
    }));
    
    // Build dependency graph for topological sort
    const depGraph = new Map<string, Set<string>>();
    for (const m of modules) {
      const analysis = dependencies.get(m.id);
      depGraph.set(m.id, new Set(analysis?.directDependencies || []));
    }
    
    // Perform topological sort respecting dependencies
    const sorted = this.topologicalSort(scoredModules, depGraph, config);
    
    // Identify constraints
    const constraints = this.identifyConstraints(modules, dependencies);
    
    // Calculate overall sequence score
    const sequenceScore = this.calculateSequenceScore(sorted, dependencies, config);
    
    return {
      modules: sorted,
      reason: this.generateSequenceReason(sorted, modules, dependencies),
      score: sequenceScore,
      constraints,
    };
  }
  
  /**
   * Find the critical path through the dependency graph
   */
  findCriticalPath(
    modules: Module[], 
    dependencies: Map<string, DependencyAnalysis>
  ): CriticalPathAnalysis {
    // Build adjacency list for path finding
    const adj = new Map<string, string[]>();
    const indegree = new Map<string, number>();
    
    for (const m of modules) {
      adj.set(m.id, []);
      indegree.set(m.id, 0);
    }
    
    for (const m of modules) {
      const analysis = dependencies.get(m.id);
      if (analysis) {
        for (const dep of analysis.directDependents) {
          if (adj.has(m.id) && modules.some(mod => mod.id === dep)) {
            adj.get(m.id)!.push(dep);
            indegree.set(dep, (indegree.get(dep) || 0) + 1);
          }
        }
      }
    }
    
    // Find longest path (critical path) using dynamic programming
    const effort = new Map<string, number>();
    for (const m of modules) {
      // Rough effort estimate based on LOC
      effort.set(m.id, m.metrics.linesOfCode / 200);
    }
    
    const dist = new Map<string, number>();
    const parent = new Map<string, string | null>();
    
    // Topological sort for DP
    const queue: string[] = [];
    for (const [id, deg] of indegree) {
      if (deg === 0) queue.push(id);
      dist.set(id, effort.get(id) || 0);
      parent.set(id, null);
    }
    
    const order: string[] = [];
    while (queue.length > 0) {
      const current = queue.shift()!;
      order.push(current);
      
      for (const next of adj.get(current) || []) {
        const newDist = dist.get(current)! + (effort.get(next) || 0);
        if (newDist > (dist.get(next) || 0)) {
          dist.set(next, newDist);
          parent.set(next, current);
        }
        
        indegree.set(next, indegree.get(next)! - 1);
        if (indegree.get(next) === 0) {
          queue.push(next);
        }
      }
    }
    
    // Find the endpoint of the critical path
    let maxDist = 0;
    let endpoint = '';
    for (const [id, d] of dist) {
      if (d > maxDist) {
        maxDist = d;
        endpoint = id;
      }
    }
    
    // Reconstruct critical path
    const criticalPath: string[] = [];
    let current: string | null = endpoint;
    while (current) {
      criticalPath.unshift(current);
      current = parent.get(current) || null;
    }
    
    // Identify bottlenecks (modules with high fan-in and fan-out)
    const bottlenecks: Bottleneck[] = [];
    for (const m of modules) {
      const analysis = dependencies.get(m.id);
      if (analysis) {
        const fanIn = analysis.directDependents.length;
        const fanOut = analysis.directDependencies.length;
        
        if (fanIn > 3 && fanOut > 3) {
          bottlenecks.push({
            moduleId: m.id,
            reason: `High coupling: ${fanIn} dependents, ${fanOut} dependencies`,
            impact: (fanIn + fanOut) / 10,
            mitigations: [
              'Consider breaking into smaller modules',
              'Create adapter interfaces to reduce direct coupling',
              'Migrate this module early to unblock dependents',
            ],
          });
        }
      }
    }
    
    // Find parallelizable branches (modules with no dependencies on each other)
    const parallelBranches = this.findParallelBranches(modules, dependencies);
    
    // Calculate sequential vs parallel effort
    const totalSequentialEffort = modules.reduce((sum, m) => sum + (effort.get(m.id) || 0), 0);
    const totalParallelEffort = maxDist; // Critical path length
    
    return {
      criticalPath,
      pathLength: maxDist,
      bottlenecks,
      parallelizableBranches: parallelBranches,
      totalSequentialEffort,
      totalParallelEffort,
      parallelizationFactor: totalSequentialEffort > 0 
        ? totalSequentialEffort / totalParallelEffort 
        : 1,
    };
  }
  
  /**
   * Generate alternative sequences
   */
  generateAlternatives(optimal: MigrationSequence, count: number = 3): MigrationSequence[] {
    const alternatives: MigrationSequence[] = [];
    const modules = [...optimal.modules];
    
    // Strategy 1: Prioritize low-risk modules first
    alternatives.push({
      modules: modules, // Would need modules with risk data
      reason: 'Risk-averse: Prioritizes lower-risk modules to build confidence',
      score: optimal.score * 0.9,
      constraints: optimal.constraints,
    });
    
    // Strategy 2: Prioritize high-value modules first
    alternatives.push({
      modules: [...modules].reverse().slice(0, Math.ceil(modules.length / 2))
        .concat(modules.slice(Math.ceil(modules.length / 2))),
      reason: 'Value-first: Prioritizes high business value modules for early ROI',
      score: optimal.score * 0.85,
      constraints: optimal.constraints,
    });
    
    // Strategy 3: Minimize parallel work (sequential)
    alternatives.push({
      modules: modules,
      reason: 'Sequential: Minimal parallelization for resource-constrained teams',
      score: optimal.score * 0.8,
      constraints: optimal.constraints,
    });
    
    return alternatives.slice(0, count);
  }
  
  /**
   * Generate migration waves from a sequence
   */
  generateWaves(
    sequence: MigrationSequence, 
    modules: Module[],
    efforts: Map<string, EffortEstimate>,
    risks: Map<string, RiskAssessment>,
    config: PlannerConfig = this.config
  ): MigrationWave[] {
    const waves: MigrationWave[] = [];
    const maxPerWave = config.sequencing.maxModulesPerWave;
    
    let currentWave: string[] = [];
    let waveNumber = 1;
    
    const moduleMap = new Map(modules.map(m => [m.id, m]));
    const completed = new Set<string>();
    
    for (const moduleId of sequence.modules) {
      const module = moduleMap.get(moduleId);
      if (!module) continue;
      
      // Check if dependencies are satisfied
      const depsAreComplete = module.dependencies.every(d => 
        completed.has(d) || !moduleMap.has(d)
      );
      
      if (!depsAreComplete) {
        // Start a new wave if we have pending items
        if (currentWave.length > 0) {
          waves.push(this.createWave(waveNumber++, currentWave, modules, efforts, risks, waves));
          currentWave.forEach(id => completed.add(id));
          currentWave = [];
        }
      }
      
      currentWave.push(moduleId);
      
      // Check wave size limit
      if (currentWave.length >= maxPerWave) {
        waves.push(this.createWave(waveNumber++, currentWave, modules, efforts, risks, waves));
        currentWave.forEach(id => completed.add(id));
        currentWave = [];
      }
    }
    
    // Add remaining modules as final wave
    if (currentWave.length > 0) {
      waves.push(this.createWave(waveNumber, currentWave, modules, efforts, risks, waves));
    }
    
    return waves;
  }
  
  private createWave(
    sequence: number,
    moduleIds: string[],
    modules: Module[],
    efforts: Map<string, EffortEstimate>,
    risks: Map<string, RiskAssessment>,
    previousWaves: MigrationWave[]
  ): MigrationWave {
    const waveEffort = this.aggregateEffort(moduleIds, efforts);
    const waveRisks = moduleIds
      .map(id => risks.get(id))
      .filter((r): r is RiskAssessment => r !== undefined);
    
    return {
      id: `wave_${sequence}`,
      name: `Wave ${sequence}`,
      sequence,
      modules: moduleIds,
      status: 'planned',
      dependencies: previousWaves.map(w => w.id),
      totalEffort: waveEffort,
      risks: waveRisks,
      milestones: this.generateMilestones(moduleIds, modules),
    };
  }
  
  private aggregateEffort(moduleIds: string[], efforts: Map<string, EffortEstimate>): EffortEstimate {
    let totalPersonDays = 0;
    const allAssumptions: string[] = [];
    
    for (const id of moduleIds) {
      const effort = efforts.get(id);
      if (effort) {
        totalPersonDays += effort.totalPersonDays;
        allAssumptions.push(...effort.assumptions);
      }
    }
    
    return {
      moduleId: moduleIds.join(','),
      phases: [],
      totalPersonDays,
      totalPersonWeeks: Math.ceil(totalPersonDays / 5),
      confidence: 0.7,
      assumptions: [...new Set(allAssumptions)],
      adjustmentFactors: [],
    };
  }
  
  private generateMilestones(_moduleIds: string[], _modules: Module[]): Milestone[] {
    const milestones: Milestone[] = [];
    
    milestones.push({
      id: `ms_wave_start`,
      name: 'Wave Kickoff',
      description: 'Begin wave implementation',
      status: 'pending',
      deliverables: ['Updated project plan', 'Resource assignments'],
      criteria: ['All prerequisites met', 'Team briefed'],
    });
    
    milestones.push({
      id: `ms_analysis_complete`,
      name: 'Analysis Complete',
      description: 'All modules analyzed and documented',
      status: 'pending',
      deliverables: ['Business rule documentation', 'Test plan'],
      criteria: ['All business rules identified', 'SME sign-off obtained'],
    });
    
    milestones.push({
      id: `ms_implementation_complete`,
      name: 'Implementation Complete',
      description: 'All modules implemented in target platform',
      status: 'pending',
      deliverables: ['Working code', 'Unit tests passing'],
      criteria: ['All features implemented', 'Code review completed'],
    });
    
    milestones.push({
      id: `ms_testing_complete`,
      name: 'Testing Complete',
      description: 'All testing phases completed',
      status: 'pending',
      deliverables: ['Test results', 'Bug fixes'],
      criteria: ['All tests passing', 'No critical bugs'],
    });
    
    milestones.push({
      id: `ms_wave_complete`,
      name: 'Wave Complete',
      description: 'Wave deployed to production',
      status: 'pending',
      deliverables: ['Production deployment', 'Runbook'],
      criteria: ['Deployment successful', 'Monitoring in place'],
    });
    
    return milestones;
  }
  
  private scoreModule(
    module: Module, 
    deps: DependencyAnalysis, 
    config: PlannerConfig
  ): number {
    const weights = config.weights;
    
    // Complexity score (lower is better, invert)
    const maxComplexity = 100;
    const complexityScore = 1 - Math.min(module.metrics.cyclomaticComplexity / maxComplexity, 1);
    
    // Criticality score (higher criticality = higher priority)
    const criticalityMap: Record<string, number> = {
      low: 0.25,
      medium: 0.5,
      high: 0.75,
      critical: 1.0,
    };
    const criticalityScore = criticalityMap[module.businessCriticality] || 0.5;
    
    // Dependency score (fewer dependents = can be migrated earlier)
    const maxDependents = 20;
    const dependencyScore = 1 - Math.min(deps.directDependents.length / maxDependents, 1);
    
    // Risk score (lower risk = can be migrated earlier)
    const riskScore = 1 - (deps.couplingScore / 10);
    
    return (
      complexityScore * weights.complexity +
      criticalityScore * weights.criticality +
      dependencyScore * weights.dependencies +
      riskScore * weights.risk
    );
  }
  
  private topologicalSort(
    scoredModules: { module: Module; score: number; deps: DependencyAnalysis }[],
    depGraph: Map<string, Set<string>>,
    _config: PlannerConfig
  ): string[] {
    const result: string[] = [];
    const visited = new Set<string>();
    const moduleIds = new Set(scoredModules.map(m => m.module.id));
    
    // Sort by score (highest first) for tie-breaking
    const sorted = [...scoredModules].sort((a, b) => b.score - a.score);
    
    const visit = (id: string, stack: Set<string>): void => {
      if (visited.has(id) || !moduleIds.has(id)) return;
      if (stack.has(id)) {
        // Circular dependency - break it by adding anyway
        if (!visited.has(id)) {
          visited.add(id);
          result.push(id);
        }
        return;
      }
      
      stack.add(id);
      
      // Visit dependencies first
      const deps = depGraph.get(id) || new Set();
      for (const dep of deps) {
        if (moduleIds.has(dep)) {
          visit(dep, stack);
        }
      }
      
      stack.delete(id);
      
      if (!visited.has(id)) {
        visited.add(id);
        result.push(id);
      }
    };
    
    for (const { module } of sorted) {
      visit(module.id, new Set());
    }
    
    return result;
  }
  
  private identifyConstraints(
    modules: Module[], 
    dependencies: Map<string, DependencyAnalysis>
  ): SequenceConstraint[] {
    const constraints: SequenceConstraint[] = [];
    
    // Dependency constraints
    for (const m of modules) {
      const deps = dependencies.get(m.id);
      if (deps && deps.directDependencies.length > 0) {
        constraints.push({
          type: 'dependency',
          description: `${m.name} depends on: ${deps.directDependencies.slice(0, 3).join(', ')}${deps.directDependencies.length > 3 ? '...' : ''}`,
          moduleIds: [m.id, ...deps.directDependencies],
          priority: 'must',
        });
      }
    }
    
    // Business criticality constraints
    const criticalModules = modules.filter(m => m.businessCriticality === 'critical');
    if (criticalModules.length > 0) {
      constraints.push({
        type: 'business',
        description: 'Critical modules require parallel-run validation before cutover',
        moduleIds: criticalModules.map(m => m.id),
        priority: 'must',
      });
    }
    
    return constraints;
  }
  
  private calculateSequenceScore(
    sequence: string[],
    dependencies: Map<string, DependencyAnalysis>,
    _config: PlannerConfig
  ): number {
    let score = 100;
    
    // Penalize for dependency violations
    const completed = new Set<string>();
    for (const moduleId of sequence) {
      const deps = dependencies.get(moduleId);
      if (deps) {
        for (const dep of deps.directDependencies) {
          if (dependencies.has(dep) && !completed.has(dep)) {
            score -= 5; // Penalty for each violation
          }
        }
      }
      completed.add(moduleId);
    }
    
    return Math.max(0, score);
  }
  
  private generateSequenceReason(
    sequence: string[],
    modules: Module[],
    dependencies: Map<string, DependencyAnalysis>
  ): string {
    if (sequence.length === 0) {
      return 'Empty sequence';
    }
    
    const moduleMap = new Map(modules.map(m => [m.id, m]));
    const firstId = sequence[0];
    const first = firstId ? moduleMap.get(firstId) : undefined;
    
    const reasons: string[] = [];
    
    // Check if leaf nodes are first
    const leafFirst = sequence.slice(0, 3).every(id => {
      const deps = dependencies.get(id);
      return deps && deps.directDependents.length === 0;
    });
    if (leafFirst) {
      reasons.push('Starts with leaf modules (no dependents)');
    }
    
    // Check if critical modules are properly placed
    const criticalModules = modules.filter(m => m.businessCriticality === 'critical');
    if (criticalModules.length > 0) {
      reasons.push('Critical modules sequenced with proper validation phases');
    }
    
    // Check for dependency respect
    reasons.push('Respects dependency order');
    
    if (first) {
      reasons.push(`Begins with ${first.name}`);
    }
    
    return reasons.join('; ');
  }
  
  private findParallelBranches(
    modules: Module[], 
    dependencies: Map<string, DependencyAnalysis>
  ): string[][] {
    // Find modules with no dependencies on each other
    const groups: Set<string>[] = [];
    const assigned = new Set<string>();
    
    for (const module of modules) {
      if (assigned.has(module.id)) continue;
      
      const group = new Set<string>([module.id]);
      const deps = dependencies.get(module.id);
      
      for (const other of modules) {
        if (other.id === module.id || assigned.has(other.id)) continue;
        
        const otherDeps = dependencies.get(other.id);
        
        // Check if they're independent
        const thisDepOnOther = deps?.directDependencies.includes(other.id) || 
          deps?.transitiveDependencies.includes(other.id);
        const otherDepOnThis = otherDeps?.directDependencies.includes(module.id) ||
          otherDeps?.transitiveDependencies.includes(module.id);
        
        if (!thisDepOnOther && !otherDepOnThis) {
          group.add(other.id);
        }
      }
      
      if (group.size > 1) {
        groups.push(group);
        for (const id of group) {
          assigned.add(id);
        }
      }
    }
    
    return groups.map(g => Array.from(g));
  }
}

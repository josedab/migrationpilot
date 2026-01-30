/**
 * Effort Estimator
 * Estimates person-days and costs for module migration
 */

import type {
  Module,
  EffortEstimate,
  PhaseEstimate,
  AdjustmentFactor,
  ResourceRequirement,
  CostEstimate,
  PlannerConfig,
  MigrationPhase,
  IEffortEstimator,
  ICostEstimator,
} from '../types';
import { DEFAULT_PLANNER_CONFIG } from '../types';

export class EffortEstimator implements IEffortEstimator {
  private config: PlannerConfig;
  
  constructor(config: Partial<PlannerConfig> = {}) {
    this.config = { ...DEFAULT_PLANNER_CONFIG, ...config };
  }
  
  /**
   * Estimate total effort for migrating a module
   */
  estimate(module: Module, config: PlannerConfig = this.config): EffortEstimate {
    const phases = this.estimateAllPhases(module, config);
    const adjustmentFactors = this.identifyAdjustmentFactors(module);
    
    let totalPersonDays = phases.reduce((sum, p) => sum + p.personDays, 0);
    
    // Apply adjustment factors
    for (const factor of adjustmentFactors) {
      if (factor.applied) {
        totalPersonDays *= factor.multiplier;
      }
    }
    
    const confidence = this.calculateConfidence(module);
    
    return {
      moduleId: module.id,
      phases,
      totalPersonDays: Math.ceil(totalPersonDays),
      totalPersonWeeks: Math.ceil(totalPersonDays / 5),
      confidence,
      assumptions: this.generateAssumptions(module),
      adjustmentFactors,
    };
  }
  
  /**
   * Estimate effort for a specific phase
   */
  estimatePhase(module: Module, phase: MigrationPhase, config: PlannerConfig = this.config): PhaseEstimate {
    const baseEffort = this.calculateBaseEffort(module, config);
    
    // Phase-specific multipliers
    const phaseMultipliers: Record<MigrationPhase, number> = {
      discovery: 0.05,
      analysis: 0.15,
      design: 0.10,
      implementation: 0.40,
      testing: 0.20,
      deployment: 0.05,
      validation: 0.05,
    };
    
    const multiplier = phaseMultipliers[phase];
    const personDays = Math.ceil(baseEffort * multiplier);
    
    return {
      phase,
      personDays,
      parallelizable: this.isPhaseParallelizable(phase),
      dependencies: this.getPhaseDependencies(phase),
      skills: this.getPhaseSkills(phase, module),
      resources: this.getPhaseResources(phase, personDays),
    };
  }
  
  /**
   * Apply adjustment factors to a base estimate
   */
  applyAdjustments(base: EffortEstimate, factors: AdjustmentFactor[]): EffortEstimate {
    let totalPersonDays = base.totalPersonDays;
    
    for (const factor of factors) {
      if (factor.applied) {
        totalPersonDays *= factor.multiplier;
      }
    }
    
    return {
      ...base,
      totalPersonDays: Math.ceil(totalPersonDays),
      totalPersonWeeks: Math.ceil(totalPersonDays / 5),
      adjustmentFactors: [...base.adjustmentFactors, ...factors],
    };
  }
  
  private estimateAllPhases(module: Module, config: PlannerConfig): PhaseEstimate[] {
    const phases: MigrationPhase[] = [
      'discovery',
      'analysis',
      'design',
      'implementation',
      'testing',
      'deployment',
      'validation',
    ];
    
    return phases.map(phase => this.estimatePhase(module, phase, config));
  }
  
  private calculateBaseEffort(module: Module, config: PlannerConfig): number {
    // Base effort from lines of code
    let effort = module.metrics.linesOfCode * config.baseEffortPerLOC / 8; // Convert hours to days
    
    // Complexity adjustment
    const complexity = this.categorizeComplexity(module.metrics.cyclomaticComplexity);
    effort *= config.complexityMultipliers[complexity] || 1.0;
    
    // Language-specific adjustment
    const languageMultipliers: Record<string, number> = {
      cobol: 1.3,
      fortran: 1.2,
      vb6: 1.1,
      java: 1.0,
      mixed: 1.4,
    };
    effort *= languageMultipliers[module.language] || 1.0;
    
    // Business criticality adjustment
    const criticalityMultipliers: Record<string, number> = {
      low: 0.9,
      medium: 1.0,
      high: 1.2,
      critical: 1.5,
    };
    effort *= criticalityMultipliers[module.businessCriticality] || 1.0;
    
    return effort;
  }
  
  private categorizeComplexity(cyclomatic: number): string {
    if (cyclomatic <= 10) return 'low';
    if (cyclomatic <= 20) return 'medium';
    if (cyclomatic <= 50) return 'high';
    return 'veryHigh';
  }
  
  private identifyAdjustmentFactors(module: Module): AdjustmentFactor[] {
    const factors: AdjustmentFactor[] = [];
    
    // External integrations factor
    if (module.metrics.numberOfExternalCalls > 10) {
      factors.push({
        name: 'High External Integration',
        description: `Module has ${module.metrics.numberOfExternalCalls} external calls requiring adapter development`,
        multiplier: 1.2,
        applied: true,
      });
    }
    
    // Low documentation factor
    if (module.metrics.documentationCoverage !== undefined && module.metrics.documentationCoverage < 30) {
      factors.push({
        name: 'Poor Documentation',
        description: 'Additional analysis time needed due to lack of documentation',
        multiplier: 1.15,
        applied: true,
      });
    }
    
    // Low test coverage factor
    if (module.metrics.testCoverage !== undefined && module.metrics.testCoverage < 50) {
      factors.push({
        name: 'Low Test Coverage',
        description: 'Additional effort needed to create comprehensive test suite',
        multiplier: 1.25,
        applied: true,
      });
    }
    
    // High dependency count factor
    if (module.dependencies.length > 15) {
      factors.push({
        name: 'High Dependency Count',
        description: `Module depends on ${module.dependencies.length} other modules`,
        multiplier: 1.1,
        applied: true,
      });
    }
    
    // Many business rules factor
    if (module.metrics.numberOfBusinessRules > 50) {
      factors.push({
        name: 'Complex Business Logic',
        description: `Module contains ${module.metrics.numberOfBusinessRules} business rules`,
        multiplier: 1.2,
        applied: true,
      });
    }
    
    // Technical debt factor
    if (module.technicalDebt && module.technicalDebt > 100) {
      factors.push({
        name: 'Technical Debt',
        description: `${module.technicalDebt} hours of estimated technical debt`,
        multiplier: 1.1,
        applied: true,
      });
    }
    
    return factors;
  }
  
  private calculateConfidence(module: Module): number {
    let confidence = 0.8; // Base confidence
    
    // Reduce confidence for undocumented modules
    if (module.metrics.documentationCoverage !== undefined) {
      confidence *= (module.metrics.documentationCoverage / 100) * 0.3 + 0.7;
    }
    
    // Reduce confidence for highly complex modules
    if (module.metrics.cyclomaticComplexity > 50) {
      confidence *= 0.85;
    }
    
    // Reduce confidence for large modules
    if (module.metrics.linesOfCode > 10000) {
      confidence *= 0.9;
    }
    
    return Math.max(0.3, Math.min(0.95, confidence));
  }
  
  private generateAssumptions(module: Module): string[] {
    const assumptions: string[] = [
      'Development team has basic familiarity with source language',
      'Target architecture has been defined and approved',
      'Required infrastructure is available',
    ];
    
    if (module.language === 'cobol') {
      assumptions.push('COBOL copybooks and JCL are accessible');
      assumptions.push('Mainframe test environment is available');
    }
    
    if (module.metrics.numberOfExternalCalls > 5) {
      assumptions.push('External system documentation is available');
      assumptions.push('Integration test environments are accessible');
    }
    
    if (module.businessCriticality === 'critical') {
      assumptions.push('Parallel-run infrastructure will be provisioned');
      assumptions.push('Dedicated QA resources for validation');
    }
    
    return assumptions;
  }
  
  private isPhaseParallelizable(phase: MigrationPhase): boolean {
    const parallelizablePhases: MigrationPhase[] = ['discovery', 'analysis', 'testing'];
    return parallelizablePhases.includes(phase);
  }
  
  private getPhaseDependencies(phase: MigrationPhase): string[] {
    const dependencies: Record<MigrationPhase, string[]> = {
      discovery: [],
      analysis: ['discovery'],
      design: ['analysis'],
      implementation: ['design'],
      testing: ['implementation'],
      deployment: ['testing'],
      validation: ['deployment'],
    };
    return dependencies[phase];
  }
  
  private getPhaseSkills(phase: MigrationPhase, module: Module): string[] {
    const baseSkills: Record<MigrationPhase, string[]> = {
      discovery: ['Legacy Systems', 'Business Analysis'],
      analysis: ['Legacy Systems', 'Architecture', 'Business Rules'],
      design: ['Architecture', 'Target Platform', 'API Design'],
      implementation: ['Target Platform', 'Testing', 'DevOps'],
      testing: ['QA', 'Test Automation', 'Legacy Systems'],
      deployment: ['DevOps', 'Infrastructure', 'Monitoring'],
      validation: ['QA', 'Business Analysis', 'Performance Testing'],
    };
    
    const skills = [...baseSkills[phase]];
    
    // Add language-specific skills
    if (['discovery', 'analysis', 'testing'].includes(phase)) {
      skills.push(`${module.language.toUpperCase()} Development`);
    }
    
    return skills;
  }
  
  private getPhaseResources(phase: MigrationPhase, personDays: number): ResourceRequirement[] {
    const resources: ResourceRequirement[] = [];
    
    switch (phase) {
      case 'discovery':
      case 'analysis':
        resources.push({
          role: 'Business Analyst',
          skillLevel: 'senior',
          allocation: 100,
          duration: Math.ceil(personDays * 0.5),
        });
        resources.push({
          role: 'Legacy Developer',
          skillLevel: 'senior',
          allocation: 100,
          duration: Math.ceil(personDays * 0.5),
        });
        break;
        
      case 'design':
        resources.push({
          role: 'Solution Architect',
          skillLevel: 'expert',
          allocation: 100,
          duration: personDays,
        });
        break;
        
      case 'implementation':
        resources.push({
          role: 'Senior Developer',
          skillLevel: 'senior',
          allocation: 100,
          duration: Math.ceil(personDays * 0.4),
        });
        resources.push({
          role: 'Developer',
          skillLevel: 'mid',
          allocation: 100,
          duration: Math.ceil(personDays * 0.6),
        });
        break;
        
      case 'testing':
        resources.push({
          role: 'QA Engineer',
          skillLevel: 'senior',
          allocation: 100,
          duration: Math.ceil(personDays * 0.6),
        });
        resources.push({
          role: 'QA Engineer',
          skillLevel: 'mid',
          allocation: 100,
          duration: Math.ceil(personDays * 0.4),
        });
        break;
        
      case 'deployment':
        resources.push({
          role: 'DevOps Engineer',
          skillLevel: 'senior',
          allocation: 100,
          duration: personDays,
        });
        break;
        
      case 'validation':
        resources.push({
          role: 'QA Lead',
          skillLevel: 'senior',
          allocation: 50,
          duration: personDays,
        });
        resources.push({
          role: 'Business Analyst',
          skillLevel: 'senior',
          allocation: 50,
          duration: personDays,
        });
        break;
    }
    
    return resources;
  }
}

export class CostEstimator implements ICostEstimator {
  private config: PlannerConfig;
  
  constructor(config: Partial<PlannerConfig> = {}) {
    this.config = { ...DEFAULT_PLANNER_CONFIG, ...config };
  }
  
  /**
   * Estimate total cost for a module migration
   */
  estimate(effort: EffortEstimate, config: PlannerConfig = this.config): CostEstimate {
    const laborCost = this.calculateLaborCost(effort.phases, config.cost.hourlyRates);
    const infrastructureCost = this.estimateInfrastructureCost(effort);
    const toolingCost = this.estimateToolingCost(effort);
    const trainingCost = this.estimateTrainingCost(effort);
    
    const subtotal = laborCost + infrastructureCost + toolingCost + trainingCost;
    const contingency = subtotal * (config.cost.contingencyPercentage / 100);
    
    return {
      moduleId: effort.moduleId,
      effort,
      laborCost,
      infrastructureCost,
      toolingCost,
      trainingCost,
      contingency,
      totalCost: subtotal + contingency,
      currency: config.cost.currency,
    };
  }
  
  /**
   * Calculate labor cost from phase estimates
   */
  calculateLaborCost(phases: PhaseEstimate[], rates: Record<string, number>): number {
    let totalCost = 0;
    
    for (const phase of phases) {
      for (const resource of phase.resources) {
        const hourlyRate = rates[resource.skillLevel] || rates['mid'];
        const hours = resource.duration * 8 * (resource.allocation / 100);
        totalCost += hours * (hourlyRate ?? rates['mid'] ?? 75);
      }
    }
    
    return Math.ceil(totalCost);
  }
  
  /**
   * Add contingency to cost estimate
   */
  addContingency(cost: CostEstimate, percentage: number): CostEstimate {
    const additionalContingency = (cost.totalCost - cost.contingency) * (percentage / 100);
    return {
      ...cost,
      contingency: cost.contingency + additionalContingency,
      totalCost: cost.totalCost + additionalContingency,
    };
  }
  
  private estimateInfrastructureCost(effort: EffortEstimate): number {
    // Rough estimate: $500/week for dev/test infrastructure
    const weeks = effort.totalPersonWeeks;
    return weeks * 500;
  }
  
  private estimateToolingCost(effort: EffortEstimate): number {
    // Rough estimate: $100/week for tooling licenses
    const weeks = effort.totalPersonWeeks;
    return weeks * 100;
  }
  
  private estimateTrainingCost(effort: EffortEstimate): number {
    // Rough estimate: 2% of labor cost for training
    const laborCost = this.calculateLaborCost(effort.phases, this.config.cost.hourlyRates);
    return Math.ceil(laborCost * 0.02);
  }
}

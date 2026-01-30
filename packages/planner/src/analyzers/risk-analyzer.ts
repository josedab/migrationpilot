/**
 * Risk Analyzer
 * Assesses migration risks for modules and suggests mitigations
 */

import type {
  Module,
  RiskAssessment,
  RiskFactor,
  RiskMitigation,
  RiskContext,
  RiskLevel,
  IRiskAnalyzer,
  DependencyAnalysis,
} from '../types';

export class RiskAnalyzer implements IRiskAnalyzer {
  
  /**
   * Assess overall risk for a module
   */
  assess(module: Module, context: RiskContext = {}): RiskAssessment {
    const factors = this.identifyFactors(module, context);
    const scoredFactors = factors.map(f => ({
      ...f,
      score: this.scoreFactor(f),
      mitigations: this.suggestMitigationsForFactor(f),
    }));
    
    const totalScore = scoredFactors.reduce((sum, f) => sum + f.score, 0);
    const avgScore = scoredFactors.length > 0 ? totalScore / scoredFactors.length : 0;
    
    const overallRisk = this.scoreToLevel(avgScore);
    
    return {
      moduleId: module.id,
      overallRisk,
      riskScore: avgScore,
      factors: scoredFactors,
      recommendations: this.generateRecommendations(scoredFactors, module),
      assessedAt: new Date().toISOString(),
    };
  }
  
  /**
   * Identify risk factors for a module
   */
  identifyFactors(module: Module, context: RiskContext = {}): RiskFactor[] {
    const factors: RiskFactor[] = [];
    
    // Technical risks
    factors.push(...this.identifyTechnicalRisks(module));
    
    // Business risks
    factors.push(...this.identifyBusinessRisks(module));
    
    // Resource risks
    factors.push(...this.identifyResourceRisks(module, context));
    
    // Integration risks
    factors.push(...this.identifyIntegrationRisks(module));
    
    // Schedule risks
    if (context.projectTimeline) {
      factors.push(...this.identifyScheduleRisks(module, context));
    }
    
    return factors;
  }
  
  private identifyTechnicalRisks(module: Module): RiskFactor[] {
    const risks: RiskFactor[] = [];
    
    // Complexity risk
    if (module.metrics.cyclomaticComplexity > 50) {
      risks.push({
        id: `risk_complexity_${module.id}`,
        category: 'technical',
        name: 'High Cyclomatic Complexity',
        description: `Module has complexity of ${module.metrics.cyclomaticComplexity}, significantly above recommended threshold of 20`,
        severity: module.metrics.cyclomaticComplexity > 100 ? 'critical' : 'high',
        probability: 0.8,
        impact: 0.7,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    } else if (module.metrics.cyclomaticComplexity > 20) {
      risks.push({
        id: `risk_complexity_${module.id}`,
        category: 'technical',
        name: 'Moderate Cyclomatic Complexity',
        description: `Module has complexity of ${module.metrics.cyclomaticComplexity}, above recommended threshold`,
        severity: 'medium',
        probability: 0.5,
        impact: 0.4,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Large codebase risk
    if (module.metrics.linesOfCode > 10000) {
      risks.push({
        id: `risk_size_${module.id}`,
        category: 'technical',
        name: 'Large Codebase',
        description: `Module has ${module.metrics.linesOfCode} LOC, making it difficult to migrate as a single unit`,
        severity: 'high',
        probability: 0.7,
        impact: 0.6,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Low test coverage risk
    if (module.metrics.testCoverage !== undefined && module.metrics.testCoverage < 50) {
      risks.push({
        id: `risk_coverage_${module.id}`,
        category: 'technical',
        name: 'Insufficient Test Coverage',
        description: `Only ${module.metrics.testCoverage}% test coverage, increasing risk of undetected regression`,
        severity: module.metrics.testCoverage < 20 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.8,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Technical debt risk
    if (module.technicalDebt && module.technicalDebt > 100) {
      risks.push({
        id: `risk_debt_${module.id}`,
        category: 'technical',
        name: 'High Technical Debt',
        description: `Module has ${module.technicalDebt} hours of estimated technical debt`,
        severity: module.technicalDebt > 200 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.5,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Low documentation risk
    if (module.metrics.documentationCoverage !== undefined && module.metrics.documentationCoverage < 30) {
      risks.push({
        id: `risk_docs_${module.id}`,
        category: 'technical',
        name: 'Poor Documentation',
        description: `Only ${module.metrics.documentationCoverage}% documentation coverage, requiring more analysis effort`,
        severity: 'medium',
        probability: 0.7,
        impact: 0.4,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    return risks;
  }
  
  private identifyBusinessRisks(module: Module): RiskFactor[] {
    const risks: RiskFactor[] = [];
    
    // Business criticality risk
    if (module.businessCriticality === 'critical') {
      risks.push({
        id: `risk_critical_${module.id}`,
        category: 'business',
        name: 'Mission-Critical Module',
        description: 'Module is critical to business operations; any disruption has severe consequences',
        severity: 'critical',
        probability: 0.4,
        impact: 1.0,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    } else if (module.businessCriticality === 'high') {
      risks.push({
        id: `risk_important_${module.id}`,
        category: 'business',
        name: 'High Business Impact',
        description: 'Module has significant business impact; requires careful migration planning',
        severity: 'high',
        probability: 0.5,
        impact: 0.7,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Many business rules risk
    if (module.metrics.numberOfBusinessRules > 50) {
      risks.push({
        id: `risk_rules_${module.id}`,
        category: 'business',
        name: 'Complex Business Logic',
        description: `Module contains ${module.metrics.numberOfBusinessRules} business rules that must be preserved`,
        severity: module.metrics.numberOfBusinessRules > 100 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.7,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    return risks;
  }
  
  private identifyResourceRisks(module: Module, context: RiskContext): RiskFactor[] {
    const risks: RiskFactor[] = [];
    
    // Legacy language expertise risk
    if (['cobol', 'fortran'].includes(module.language)) {
      risks.push({
        id: `risk_expertise_${module.id}`,
        category: 'resource',
        name: 'Specialized Expertise Required',
        description: `Requires ${module.language.toUpperCase()} expertise which is increasingly scarce`,
        severity: 'medium',
        probability: 0.7,
        impact: 0.5,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // SME availability risk
    if (!module.dataOwners || module.dataOwners.length === 0) {
      risks.push({
        id: `risk_sme_${module.id}`,
        category: 'resource',
        name: 'No Identified SME',
        description: 'No subject matter expert identified for this module',
        severity: 'medium',
        probability: 0.6,
        impact: 0.6,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // Resource availability risk
    if (context.availableResources && context.availableResources.length < 3) {
      risks.push({
        id: `risk_resources_${module.id}`,
        category: 'resource',
        name: 'Limited Team Resources',
        description: 'Limited team resources may impact migration timeline',
        severity: 'medium',
        probability: 0.5,
        impact: 0.5,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    return risks;
  }
  
  private identifyIntegrationRisks(module: Module): RiskFactor[] {
    const risks: RiskFactor[] = [];
    
    // External system integration risk
    if (module.metrics.numberOfExternalCalls > 10) {
      risks.push({
        id: `risk_integration_${module.id}`,
        category: 'integration',
        name: 'Multiple External Integrations',
        description: `Module integrates with ${module.metrics.numberOfExternalCalls} external systems`,
        severity: module.metrics.numberOfExternalCalls > 20 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.6,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // High dependency count risk
    if (module.dependencies.length > 15) {
      risks.push({
        id: `risk_deps_${module.id}`,
        category: 'integration',
        name: 'High Dependency Count',
        description: `Module depends on ${module.dependencies.length} other modules`,
        severity: module.dependencies.length > 25 ? 'high' : 'medium',
        probability: 0.5,
        impact: 0.5,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    // High dependent count risk (others depend on this)
    if (module.dependents.length > 10) {
      risks.push({
        id: `risk_dependents_${module.id}`,
        category: 'integration',
        name: 'Many Downstream Dependencies',
        description: `${module.dependents.length} other modules depend on this module`,
        severity: module.dependents.length > 20 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.7,
        score: 0,
        mitigations: [],
        affectedModules: [module.id],
      });
    }
    
    return risks;
  }
  
  private identifyScheduleRisks(module: Module, context: RiskContext): RiskFactor[] {
    const risks: RiskFactor[] = [];
    
    if (context.projectTimeline) {
      const start = new Date(context.projectTimeline.start);
      const end = new Date(context.projectTimeline.end);
      const durationDays = (end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24);
      
      // Estimate effort based on LOC
      const estimatedDays = module.metrics.linesOfCode / 200; // rough estimate
      
      if (estimatedDays > durationDays * 0.5) {
        risks.push({
          id: `risk_schedule_${module.id}`,
          category: 'schedule',
          name: 'Timeline Constraint',
          description: 'Module migration may require significant portion of available timeline',
          severity: estimatedDays > durationDays ? 'high' : 'medium',
          probability: 0.6,
          impact: 0.7,
          score: 0,
          mitigations: [],
          affectedModules: [module.id],
        });
      }
    }
    
    return risks;
  }
  
  /**
   * Score a risk factor
   */
  scoreFactor(factor: RiskFactor): number {
    const severityWeights: Record<RiskLevel, number> = {
      low: 1,
      medium: 2,
      high: 3,
      critical: 4,
    };
    
    const severityWeight = severityWeights[factor.severity];
    const rawScore = factor.probability * factor.impact * severityWeight;
    
    // Normalize to 0-10 scale
    return Math.min(10, rawScore * 2.5);
  }
  
  /**
   * Suggest mitigations for all factors
   */
  suggestMitigations(factors: RiskFactor[]): RiskMitigation[] {
    const mitigations: RiskMitigation[] = [];
    
    for (const factor of factors) {
      mitigations.push(...this.suggestMitigationsForFactor(factor));
    }
    
    return mitigations;
  }
  
  private suggestMitigationsForFactor(factor: RiskFactor): RiskMitigation[] {
    const mitigations: RiskMitigation[] = [];
    
    switch (factor.category) {
      case 'technical':
        if (factor.name.includes('Complexity')) {
          mitigations.push({
            id: `mit_refactor_${factor.id}`,
            strategy: 'mitigate',
            description: 'Refactor module into smaller, more manageable components before migration',
            effort: 'high',
            effectiveness: 0.7,
            implemented: false,
          });
          mitigations.push({
            id: `mit_incremental_${factor.id}`,
            strategy: 'mitigate',
            description: 'Migrate in smaller increments with comprehensive testing at each step',
            effort: 'medium',
            effectiveness: 0.6,
            implemented: false,
          });
        }
        if (factor.name.includes('Test Coverage')) {
          mitigations.push({
            id: `mit_tests_${factor.id}`,
            strategy: 'mitigate',
            description: 'Generate comprehensive test suite before migration begins',
            effort: 'medium',
            effectiveness: 0.8,
            implemented: false,
          });
        }
        if (factor.name.includes('Documentation')) {
          mitigations.push({
            id: `mit_docs_${factor.id}`,
            strategy: 'mitigate',
            description: 'Use AI documentation generator to create baseline documentation',
            effort: 'low',
            effectiveness: 0.6,
            implemented: false,
          });
        }
        break;
        
      case 'business':
        if (factor.severity === 'critical') {
          mitigations.push({
            id: `mit_parallel_${factor.id}`,
            strategy: 'mitigate',
            description: 'Run legacy and modern systems in parallel with traffic shadowing',
            effort: 'high',
            effectiveness: 0.9,
            implemented: false,
          });
          mitigations.push({
            id: `mit_rollback_${factor.id}`,
            strategy: 'mitigate',
            description: 'Implement instant rollback capability with automated health checks',
            effort: 'medium',
            effectiveness: 0.8,
            implemented: false,
          });
        }
        if (factor.name.includes('Business Logic')) {
          mitigations.push({
            id: `mit_sme_${factor.id}`,
            strategy: 'mitigate',
            description: 'Engage SMEs for rule validation sessions throughout migration',
            effort: 'medium',
            effectiveness: 0.7,
            implemented: false,
          });
        }
        break;
        
      case 'resource':
        if (factor.name.includes('Expertise')) {
          mitigations.push({
            id: `mit_training_${factor.id}`,
            strategy: 'mitigate',
            description: 'Provide legacy language training for modern developers',
            effort: 'medium',
            effectiveness: 0.5,
            implemented: false,
          });
          mitigations.push({
            id: `mit_consultant_${factor.id}`,
            strategy: 'transfer',
            description: 'Engage specialized legacy migration consultants',
            effort: 'high',
            effectiveness: 0.8,
            implemented: false,
          });
        }
        if (factor.name.includes('SME')) {
          mitigations.push({
            id: `mit_knowledge_${factor.id}`,
            strategy: 'mitigate',
            description: 'Use execution tracing to capture business logic from runtime behavior',
            effort: 'medium',
            effectiveness: 0.7,
            implemented: false,
          });
        }
        break;
        
      case 'integration':
        mitigations.push({
          id: `mit_interface_${factor.id}`,
          strategy: 'mitigate',
          description: 'Create anti-corruption layer to isolate integration points',
          effort: 'medium',
          effectiveness: 0.7,
          implemented: false,
        });
        mitigations.push({
          id: `mit_contract_${factor.id}`,
          strategy: 'mitigate',
          description: 'Implement contract testing for all integration points',
          effort: 'medium',
          effectiveness: 0.8,
          implemented: false,
        });
        break;
        
      case 'schedule':
        mitigations.push({
          id: `mit_scope_${factor.id}`,
          strategy: 'mitigate',
          description: 'Reduce scope by deferring non-critical functionality',
          effort: 'low',
          effectiveness: 0.6,
          implemented: false,
        });
        mitigations.push({
          id: `mit_resources_${factor.id}`,
          strategy: 'mitigate',
          description: 'Add additional resources to parallel work streams',
          effort: 'high',
          effectiveness: 0.5,
          implemented: false,
        });
        break;
    }
    
    return mitigations;
  }
  
  private scoreToLevel(score: number): RiskLevel {
    if (score >= 7) return 'critical';
    if (score >= 5) return 'high';
    if (score >= 3) return 'medium';
    return 'low';
  }
  
  private generateRecommendations(factors: RiskFactor[], module: Module): string[] {
    const recommendations: string[] = [];
    
    const highRisks = factors.filter(f => f.severity === 'high' || f.severity === 'critical');
    
    if (highRisks.length > 3) {
      recommendations.push('Consider deferring migration of this module until lower-risk modules are complete');
    }
    
    if (factors.some(f => f.category === 'technical' && f.name.includes('Test Coverage'))) {
      recommendations.push('Prioritize test generation before starting migration');
    }
    
    if (factors.some(f => f.category === 'business' && f.severity === 'critical')) {
      recommendations.push('Implement parallel-run strategy with gradual traffic shifting');
    }
    
    if (factors.some(f => f.category === 'integration' && f.name.includes('External'))) {
      recommendations.push('Map all integration points and create adapter interfaces first');
    }
    
    if (module.dependencies.length > 10) {
      recommendations.push('Consider migrating dependencies first to reduce risk');
    }
    
    if (recommendations.length === 0) {
      recommendations.push('Module is suitable for migration with standard precautions');
    }
    
    return recommendations;
  }
  
  /**
   * Assess risks with dependency context
   */
  assessWithDependencies(
    module: Module, 
    depAnalysis: DependencyAnalysis, 
    context: RiskContext = {}
  ): RiskAssessment {
    // Add dependency-specific risks
    const assessment = this.assess(module, context);
    
    // Add circular dependency risk
    if (depAnalysis.circularDependencies.length > 0) {
      assessment.factors.push({
        id: `risk_circular_${module.id}`,
        category: 'integration',
        name: 'Circular Dependencies',
        description: `Module is part of ${depAnalysis.circularDependencies.length} circular dependency chain(s)`,
        severity: 'high',
        probability: 0.8,
        impact: 0.7,
        score: this.scoreFactor({
          id: '',
          category: 'integration',
          name: '',
          description: '',
          severity: 'high',
          probability: 0.8,
          impact: 0.7,
          score: 0,
          mitigations: [],
          affectedModules: [],
        }),
        mitigations: [{
          id: `mit_circular_${module.id}`,
          strategy: 'mitigate',
          description: 'Break circular dependencies by introducing interfaces or event-driven patterns',
          effort: 'high',
          effectiveness: 0.8,
          implemented: false,
        }],
        affectedModules: depAnalysis.circularDependencies.flat(),
      });
    }
    
    // Add high coupling risk
    if (depAnalysis.couplingScore > 5) {
      assessment.factors.push({
        id: `risk_coupling_${module.id}`,
        category: 'integration',
        name: 'High Coupling',
        description: `Module has coupling score of ${depAnalysis.couplingScore.toFixed(1)}/10`,
        severity: depAnalysis.couplingScore > 7 ? 'high' : 'medium',
        probability: 0.6,
        impact: 0.6,
        score: depAnalysis.couplingScore,
        mitigations: [{
          id: `mit_coupling_${module.id}`,
          strategy: 'mitigate',
          description: 'Decouple module by introducing facade pattern or event bus',
          effort: 'medium',
          effectiveness: 0.6,
          implemented: false,
        }],
        affectedModules: [module.id],
      });
    }
    
    // Recalculate overall risk
    const totalScore = assessment.factors.reduce((sum, f) => sum + f.score, 0);
    assessment.riskScore = assessment.factors.length > 0 ? totalScore / assessment.factors.length : 0;
    assessment.overallRisk = this.scoreToLevel(assessment.riskScore);
    
    return assessment;
  }
}

/**
 * Migration Cost Estimator
 * 
 * AI-powered analysis that estimates effort, risk, and cost for migration projects.
 * Uses historical data and complexity metrics to provide accurate estimates.
 */

import type { SourceLanguage, TargetLanguage, BusinessRule, Procedure, DataStructure } from '@migrationpilot/core';

export interface CostEstimate {
  projectId: string;
  estimatedAt: string;
  
  // Effort estimates (in person-days)
  effort: {
    analysis: number;
    design: number;
    development: number;
    testing: number;
    deployment: number;
    total: number;
    confidenceRange: { min: number; max: number };
  };
  
  // Cost breakdown
  cost: {
    labor: number;
    infrastructure: number;
    licensing: number;
    contingency: number;
    total: number;
    currency: string;
  };
  
  // Timeline
  timeline: {
    estimatedWeeks: number;
    phases: PhaseEstimate[];
    criticalPath: string[];
  };
  
  // Risk assessment
  risk: {
    overall: 'low' | 'medium' | 'high' | 'very-high';
    factors: RiskFactor[];
    mitigations: string[];
  };
  
  // Complexity metrics
  complexity: {
    overall: number; // 1-100
    codeComplexity: number;
    dataComplexity: number;
    integrationComplexity: number;
    businessLogicComplexity: number;
  };
  
  // Recommendations
  recommendations: Recommendation[];
  
  // Assumptions
  assumptions: string[];
}

export interface PhaseEstimate {
  name: string;
  description: string;
  effortDays: number;
  dependencies: string[];
  deliverables: string[];
}

export interface RiskFactor {
  name: string;
  description: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  probability: number; // 0-1
  impact: string;
  mitigation: string;
}

export interface Recommendation {
  type: 'approach' | 'tool' | 'team' | 'timeline' | 'scope';
  title: string;
  description: string;
  impact: string;
  priority: 'high' | 'medium' | 'low';
}

export interface EstimatorInput {
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  targetFramework?: string;
  
  // Code metrics
  totalLines: number;
  totalFiles: number;
  procedures: Procedure[];
  dataStructures: DataStructure[];
  businessRules: BusinessRule[];
  
  // Complexity indicators
  externalDependencies: number;
  databaseTables: number;
  fileIntegrations: number;
  apiIntegrations: number;
  
  // Team info
  teamSize?: number;
  teamExperience?: 'junior' | 'mid' | 'senior' | 'mixed';
  
  // Constraints
  deadline?: string;
  budget?: number;
  
  // Historical data (if available)
  historicalProjects?: HistoricalProject[];
}

export interface HistoricalProject {
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  totalLines: number;
  actualEffortDays: number;
  actualCost: number;
  actualDurationWeeks: number;
  complexityScore: number;
}

export interface EstimatorConfig {
  laborRatePerDay: number;
  currency: string;
  contingencyPercentage: number;
  includeInfrastructure: boolean;
  includeLicensing: boolean;
}

export class MigrationCostEstimator {
  private defaultConfig: EstimatorConfig = {
    laborRatePerDay: 800, // USD
    currency: 'USD',
    contingencyPercentage: 20,
    includeInfrastructure: true,
    includeLicensing: true,
  };

  // Base effort factors by language pair (person-days per 1000 lines)
  private baseEffortFactors: Record<string, number> = {
    'cobol-java': 15,
    'cobol-python': 12,
    'cobol-typescript': 14,
    'fortran-python': 10,
    'fortran-java': 12,
    'vb6-typescript': 8,
    'vb6-csharp': 6,
    'java-legacy-java': 5,
    'java-legacy-typescript': 8,
    'default': 12,
  };

  // Complexity multipliers
  private complexityMultipliers = {
    codeComplexity: { low: 1.0, medium: 1.3, high: 1.6, veryHigh: 2.0 },
    integrations: { few: 1.0, moderate: 1.2, many: 1.5, extensive: 1.8 },
    businessRules: { simple: 1.0, moderate: 1.25, complex: 1.5, veryComplex: 1.8 },
    teamExperience: { senior: 0.8, mid: 1.0, junior: 1.3, mixed: 1.1 },
  };

  /**
   * Generate a comprehensive cost estimate
   */
  estimate(input: EstimatorInput, config: Partial<EstimatorConfig> = {}): CostEstimate {
    const cfg = { ...this.defaultConfig, ...config };
    
    // Calculate complexity scores
    const complexity = this.calculateComplexity(input);
    
    // Calculate effort
    const effort = this.calculateEffort(input, complexity);
    
    // Calculate cost
    const cost = this.calculateCost(effort, cfg);
    
    // Calculate timeline
    const timeline = this.calculateTimeline(effort, input);
    
    // Assess risks
    const risk = this.assessRisks(input, complexity);
    
    // Generate recommendations
    const recommendations = this.generateRecommendations(input, complexity, risk);
    
    // Document assumptions
    const assumptions = this.documentAssumptions(input, cfg);
    
    return {
      projectId: `est_${Date.now()}`,
      estimatedAt: new Date().toISOString(),
      effort,
      cost,
      timeline,
      risk,
      complexity,
      recommendations,
      assumptions,
    };
  }

  /**
   * Calculate complexity metrics
   */
  private calculateComplexity(input: EstimatorInput): CostEstimate['complexity'] {
    // Code complexity based on cyclomatic complexity
    const avgProcedureComplexity = input.procedures.length > 0
      ? input.procedures.reduce((sum, p) => sum + p.complexity, 0) / input.procedures.length
      : 0;
    const codeComplexity = Math.min(100, Math.round(avgProcedureComplexity * 2));
    
    // Data complexity based on structure count and nesting
    const dataComplexity = Math.min(100, Math.round(
      (input.dataStructures.length * 2) +
      (input.databaseTables * 5) +
      (input.dataStructures.filter(d => d.children && d.children.length > 0).length * 3)
    ));
    
    // Integration complexity
    const integrationComplexity = Math.min(100, Math.round(
      (input.externalDependencies * 3) +
      (input.fileIntegrations * 2) +
      (input.apiIntegrations * 5)
    ));
    
    // Business logic complexity based on rule count and confidence
    const lowConfidenceRules = input.businessRules.filter(r => r.confidence < 0.7).length;
    const businessLogicComplexity = Math.min(100, Math.round(
      (input.businessRules.length * 2) +
      (lowConfidenceRules * 5)
    ));
    
    // Overall complexity (weighted average)
    const overall = Math.round(
      (codeComplexity * 0.3) +
      (dataComplexity * 0.2) +
      (integrationComplexity * 0.25) +
      (businessLogicComplexity * 0.25)
    );
    
    return {
      overall,
      codeComplexity,
      dataComplexity,
      integrationComplexity,
      businessLogicComplexity,
    };
  }

  /**
   * Calculate effort estimates
   */
  private calculateEffort(
    input: EstimatorInput,
    complexity: CostEstimate['complexity']
  ): CostEstimate['effort'] {
    const languagePair = `${input.sourceLanguage}-${input.targetLanguage}`;
    const baseRate = this.baseEffortFactors[languagePair] || this.baseEffortFactors['default'] || 12;
    
    // Base effort from lines of code
    const baseDays = (input.totalLines / 1000) * baseRate;
    
    // Apply complexity multipliers
    const complexityMultiplier = this.getComplexityMultiplier(complexity.overall);
    const integrationMultiplier = this.getIntegrationMultiplier(input);
    const teamMultiplier = this.complexityMultipliers.teamExperience[input.teamExperience || 'mid'];
    
    const adjustedDays = baseDays * complexityMultiplier * integrationMultiplier * teamMultiplier;
    
    // Break down by phase
    const analysis = Math.round(adjustedDays * 0.15);
    const design = Math.round(adjustedDays * 0.15);
    const development = Math.round(adjustedDays * 0.40);
    const testing = Math.round(adjustedDays * 0.20);
    const deployment = Math.round(adjustedDays * 0.10);
    
    const total = analysis + design + development + testing + deployment;
    
    // Calculate confidence range (±20% for complexity < 50, ±30% for higher)
    const variance = complexity.overall < 50 ? 0.2 : 0.3;
    
    return {
      analysis,
      design,
      development,
      testing,
      deployment,
      total,
      confidenceRange: {
        min: Math.round(total * (1 - variance)),
        max: Math.round(total * (1 + variance)),
      },
    };
  }

  /**
   * Calculate cost estimates
   */
  private calculateCost(
    effort: CostEstimate['effort'],
    config: EstimatorConfig
  ): CostEstimate['cost'] {
    const labor = effort.total * config.laborRatePerDay;
    
    // Infrastructure costs (cloud, tools, etc.)
    const infrastructure = config.includeInfrastructure
      ? labor * 0.1 // 10% of labor cost
      : 0;
    
    // Licensing costs
    const licensing = config.includeLicensing
      ? labor * 0.05 // 5% of labor cost
      : 0;
    
    const subtotal = labor + infrastructure + licensing;
    const contingency = subtotal * (config.contingencyPercentage / 100);
    
    return {
      labor: Math.round(labor),
      infrastructure: Math.round(infrastructure),
      licensing: Math.round(licensing),
      contingency: Math.round(contingency),
      total: Math.round(subtotal + contingency),
      currency: config.currency,
    };
  }

  /**
   * Calculate timeline
   */
  private calculateTimeline(
    effort: CostEstimate['effort'],
    input: EstimatorInput
  ): CostEstimate['timeline'] {
    const teamSize = input.teamSize || 3;
    const parallelizationFactor = Math.min(1, 0.6 + (teamSize * 0.1)); // Diminishing returns
    
    const workingDays = effort.total / (teamSize * parallelizationFactor);
    const estimatedWeeks = Math.ceil(workingDays / 5);
    
    const phases: PhaseEstimate[] = [
      {
        name: 'Discovery & Analysis',
        description: 'Code analysis, business rule extraction, and documentation',
        effortDays: effort.analysis,
        dependencies: [],
        deliverables: ['Analysis Report', 'Business Rule Catalog', 'Dependency Map'],
      },
      {
        name: 'Architecture Design',
        description: 'Modern architecture design and migration strategy',
        effortDays: effort.design,
        dependencies: ['Discovery & Analysis'],
        deliverables: ['Architecture Design', 'Migration Plan', 'Test Strategy'],
      },
      {
        name: 'Development',
        description: 'Code generation and customization',
        effortDays: effort.development,
        dependencies: ['Architecture Design'],
        deliverables: ['Migrated Code', 'Unit Tests', 'Integration Points'],
      },
      {
        name: 'Testing & Validation',
        description: 'Equivalence testing and UAT',
        effortDays: effort.testing,
        dependencies: ['Development'],
        deliverables: ['Test Results', 'Equivalence Report', 'UAT Sign-off'],
      },
      {
        name: 'Deployment',
        description: 'Production deployment and transition',
        effortDays: effort.deployment,
        dependencies: ['Testing & Validation'],
        deliverables: ['Production Deployment', 'Runbook', 'Training Materials'],
      },
    ];
    
    const criticalPath = phases.map(p => p.name);
    
    return {
      estimatedWeeks,
      phases,
      criticalPath,
    };
  }

  /**
   * Assess project risks
   */
  private assessRisks(
    input: EstimatorInput,
    complexity: CostEstimate['complexity']
  ): CostEstimate['risk'] {
    const factors: RiskFactor[] = [];
    
    // Complexity risk
    if (complexity.overall > 70) {
      factors.push({
        name: 'High Overall Complexity',
        description: `Complexity score of ${complexity.overall}/100 indicates challenging migration`,
        severity: 'high',
        probability: 0.7,
        impact: 'Timeline delays and increased testing requirements',
        mitigation: 'Allocate additional testing resources and consider phased migration',
      });
    }
    
    // Business rule risk
    const lowConfidenceRules = input.businessRules.filter(r => r.confidence < 0.7);
    if (lowConfidenceRules.length > 0) {
      factors.push({
        name: 'Undocumented Business Logic',
        description: `${lowConfidenceRules.length} business rules have low confidence scores`,
        severity: lowConfidenceRules.length > 10 ? 'critical' : 'high',
        probability: 0.8,
        impact: 'Incorrect behavior in migrated system',
        mitigation: 'Engage SMEs for rule validation before migration',
      });
    }
    
    // Integration risk
    if (input.externalDependencies > 5 || input.apiIntegrations > 3) {
      factors.push({
        name: 'External Dependencies',
        description: 'Multiple external systems require coordination',
        severity: 'medium',
        probability: 0.5,
        impact: 'Integration testing complexity and potential blocking',
        mitigation: 'Create mock services for testing and establish communication with external teams early',
      });
    }
    
    // Data migration risk
    if (input.databaseTables > 20) {
      factors.push({
        name: 'Large Data Model',
        description: `${input.databaseTables} database tables require migration`,
        severity: 'medium',
        probability: 0.6,
        impact: 'Data migration complexity and potential data integrity issues',
        mitigation: 'Implement robust data validation and rollback procedures',
      });
    }
    
    // Team risk
    if (input.teamExperience === 'junior') {
      factors.push({
        name: 'Team Experience',
        description: 'Junior team may require additional guidance',
        severity: 'medium',
        probability: 0.5,
        impact: 'Slower velocity and potential quality issues',
        mitigation: 'Pair programming, code reviews, and technical mentorship',
      });
    }
    
    // Legacy language expertise risk
    if (input.sourceLanguage === 'cobol' || input.sourceLanguage === 'fortran') {
      factors.push({
        name: 'Legacy Language Expertise',
        description: `${input.sourceLanguage.toUpperCase()} expertise is rare`,
        severity: 'medium',
        probability: 0.4,
        impact: 'Difficulty understanding and validating legacy behavior',
        mitigation: 'Leverage AI analysis tools and engage legacy consultants',
      });
    }
    
    // Calculate overall risk level
    const avgSeverity = factors.reduce((sum, f) => {
      const severityScores: Record<string, number> = { low: 1, medium: 2, high: 3, critical: 4 };
      return sum + (severityScores[f.severity] || 0);
    }, 0) / (factors.length || 1);
    
    let overall: CostEstimate['risk']['overall'];
    if (avgSeverity > 3) overall = 'very-high';
    else if (avgSeverity > 2) overall = 'high';
    else if (avgSeverity > 1.5) overall = 'medium';
    else overall = 'low';
    
    const mitigations = factors.map(f => f.mitigation);
    
    return { overall, factors, mitigations };
  }

  /**
   * Generate recommendations
   */
  private generateRecommendations(
    input: EstimatorInput,
    complexity: CostEstimate['complexity'],
    _risk: CostEstimate['risk']
  ): Recommendation[] {
    const recommendations: Recommendation[] = [];
    
    // Migration approach
    if (input.totalLines > 100000) {
      recommendations.push({
        type: 'approach',
        title: 'Consider Strangler Fig Pattern',
        description: 'Large codebase would benefit from incremental migration with parallel running systems',
        impact: 'Reduces risk and allows continuous value delivery',
        priority: 'high',
      });
    }
    
    // Team composition
    if (complexity.overall > 60 && input.teamExperience !== 'senior') {
      recommendations.push({
        type: 'team',
        title: 'Add Senior Developer',
        description: 'Complex migration requires experienced technical leadership',
        impact: 'Faster problem resolution and better architecture decisions',
        priority: 'high',
      });
    }
    
    // Testing
    if (input.businessRules.length > 20) {
      recommendations.push({
        type: 'tool',
        title: 'Invest in Automated Testing',
        description: 'Generate comprehensive test suite from business rules and historical data',
        impact: 'Ensures business logic preservation and enables confident refactoring',
        priority: 'high',
      });
    }
    
    // SME engagement
    if (input.businessRules.filter(r => r.confidence < 0.7).length > 5) {
      recommendations.push({
        type: 'team',
        title: 'Dedicated SME Engagement',
        description: 'Allocate SME time for business rule validation throughout the project',
        impact: 'Reduces risk of incorrect migration of critical business logic',
        priority: 'high',
      });
    }
    
    // Timeline
    if (input.deadline) {
      const deadlineDate = new Date(input.deadline);
      const estimatedDuration = Math.ceil(input.totalLines / 1000 * 0.5); // Rough weeks estimate
      const weeksUntilDeadline = Math.ceil((deadlineDate.getTime() - Date.now()) / (7 * 24 * 60 * 60 * 1000));
      
      if (weeksUntilDeadline < estimatedDuration * 0.8) {
        recommendations.push({
          type: 'timeline',
          title: 'Timeline at Risk',
          description: 'Current deadline may not accommodate full migration scope',
          impact: 'Consider scope reduction or additional resources',
          priority: 'high',
        });
      }
    }
    
    // Scope
    if (complexity.integrationComplexity > 70) {
      recommendations.push({
        type: 'scope',
        title: 'API-First Migration',
        description: 'Consider wrapping legacy code with APIs before full migration',
        impact: 'Reduces integration risk and enables incremental modernization',
        priority: 'medium',
      });
    }
    
    return recommendations;
  }

  /**
   * Document assumptions
   */
  private documentAssumptions(input: EstimatorInput, config: EstimatorConfig): string[] {
    return [
      `Labor rate: ${config.currency} ${config.laborRatePerDay}/day`,
      `Team experience level: ${input.teamExperience || 'mid'}`,
      `Team size: ${input.teamSize || 3} developers`,
      `Contingency: ${config.contingencyPercentage}%`,
      'Full-time dedicated resources assumed',
      'No major scope changes during project',
      'SME availability for rule validation',
      'Test environment availability',
      input.historicalProjects 
        ? `Estimate calibrated using ${input.historicalProjects.length} historical projects`
        : 'Estimate based on industry benchmarks (no historical data provided)',
    ];
  }

  /**
   * Helper: Get complexity multiplier
   */
  private getComplexityMultiplier(complexity: number): number {
    if (complexity < 30) return 1.0;
    if (complexity < 50) return 1.2;
    if (complexity < 70) return 1.5;
    return 1.8;
  }

  /**
   * Helper: Get integration multiplier
   */
  private getIntegrationMultiplier(input: EstimatorInput): number {
    const total = input.externalDependencies + input.apiIntegrations + input.fileIntegrations;
    if (total < 5) return 1.0;
    if (total < 10) return 1.2;
    if (total < 20) return 1.4;
    return 1.6;
  }
}

/**
 * Compliance Documentation Generator
 * 
 * Generates audit-ready documentation for regulatory compliance
 * (SOX, GDPR, SOC2, HIPAA, PCI-DSS, etc.)
 */

import type { 
  BusinessRule,
} from '@migrationpilot/core';

import type {
  Document,
  DocumentSection,
} from '../types.js';

// ============================================================================
// Compliance Types
// ============================================================================

export interface ComplianceFramework {
  id: string;
  name: string;
  version: string;
  requirements: ComplianceRequirement[];
  controlFamilies: ControlFamily[];
}

export interface ComplianceRequirement {
  id: string;
  code: string;
  title: string;
  description: string;
  controlFamily: string;
  criticality: 'critical' | 'high' | 'medium' | 'low';
  evidenceTypes: EvidenceType[];
}

export interface ControlFamily {
  id: string;
  name: string;
  description: string;
  requirements: string[];
}

export type EvidenceType = 
  | 'data-lineage'
  | 'access-control'
  | 'change-management'
  | 'audit-trail'
  | 'encryption'
  | 'data-retention'
  | 'business-continuity'
  | 'testing-evidence'
  | 'approval-records';

export interface ComplianceEvidence {
  id: string;
  requirementId: string;
  type: EvidenceType;
  title: string;
  description: string;
  sourceFile?: string;
  sourceCode?: string;
  metadata: Record<string, unknown>;
  collectedAt: Date;
  validUntil?: Date;
}

export interface ComplianceReport {
  id: string;
  projectId: string;
  framework: ComplianceFramework;
  generatedAt: Date;
  period: { start: Date; end: Date };
  
  // Overall assessment
  overallScore: number;
  status: 'compliant' | 'partially-compliant' | 'non-compliant';
  
  // Detailed results
  requirementResults: RequirementResult[];
  controlFamilyScores: Record<string, number>;
  
  // Gaps and recommendations
  gaps: ComplianceGap[];
  recommendations: ComplianceRecommendation[];
  
  // Documents
  generatedDocuments: string[];
}

export interface RequirementResult {
  requirementId: string;
  status: 'met' | 'partially-met' | 'not-met' | 'not-applicable';
  score: number;
  evidence: ComplianceEvidence[];
  notes: string;
  reviewedBy?: string;
  reviewedAt?: Date;
}

export interface ComplianceGap {
  requirementId: string;
  description: string;
  severity: 'critical' | 'high' | 'medium' | 'low';
  remediation: string;
  estimatedEffort: string;
  dueDate?: Date;
}

export interface ComplianceRecommendation {
  id: string;
  title: string;
  description: string;
  priority: 'high' | 'medium' | 'low';
  impactedRequirements: string[];
  implementationSteps: string[];
}

// ============================================================================
// Compliance Documentation Generator
// ============================================================================

export class ComplianceDocumentationGenerator {
  private frameworks: Map<string, ComplianceFramework> = new Map();
  private evidence: Map<string, ComplianceEvidence[]> = new Map();

  constructor() {
    this.initializeFrameworks();
  }

  /**
   * Generate compliance report for a project
   */
  async generateReport(
    projectId: string,
    frameworkId: string,
    period: { start: Date; end: Date },
    rules: BusinessRule[],
    auditTrail: AuditEntry[]
  ): Promise<ComplianceReport> {
    const framework = this.frameworks.get(frameworkId);
    if (!framework) {
      throw new Error(`Framework ${frameworkId} not found`);
    }

    // Collect evidence from various sources
    const collectedEvidence = await this.collectEvidence(projectId, rules, auditTrail);
    this.evidence.set(projectId, collectedEvidence);

    // Assess each requirement
    const requirementResults = this.assessRequirements(framework, collectedEvidence);

    // Calculate scores
    const controlFamilyScores = this.calculateControlFamilyScores(framework, requirementResults);
    const overallScore = this.calculateOverallScore(requirementResults);

    // Identify gaps
    const gaps = this.identifyGaps(framework, requirementResults);

    // Generate recommendations
    const recommendations = this.generateRecommendations(gaps);

    // Generate supporting documents
    const documents = await this.generateSupportingDocuments(
      projectId,
      framework,
      requirementResults,
      collectedEvidence
    );

    return {
      id: `report_${Date.now()}`,
      projectId,
      framework,
      generatedAt: new Date(),
      period,
      overallScore,
      status: this.determineStatus(overallScore),
      requirementResults,
      controlFamilyScores,
      gaps,
      recommendations,
      generatedDocuments: documents,
    };
  }

  /**
   * Generate data lineage documentation
   */
  async generateDataLineage(
    projectId: string,
    dataFlows: DataFlow[]
  ): Promise<Document> {
    const diagram = this.generateLineageDiagram(dataFlows);
    const codeBlocks = this.extractTransformationCode(dataFlows);

    const sections: DocumentSection[] = [
      {
        id: 'overview',
        title: 'Data Lineage Overview',
        level: 1,
        content: this.generateDataLineageOverview(dataFlows) + '\n\n```' + diagram.type + '\n' + diagram.content + '\n```',
      },
      {
        id: 'sources',
        title: 'Data Sources',
        level: 2,
        content: this.generateSourcesSection(dataFlows),
      },
      {
        id: 'transformations',
        title: 'Transformations',
        level: 2,
        content: this.generateTransformationsSection(dataFlows) + '\n\n' + codeBlocks.map(cb => '```' + cb.language + '\n' + cb.code + '\n```').join('\n'),
      },
      {
        id: 'destinations',
        title: 'Data Destinations',
        level: 2,
        content: this.generateDestinationsSection(dataFlows),
      },
      {
        id: 'controls',
        title: 'Data Quality Controls',
        level: 2,
        content: this.generateControlsSection(dataFlows),
      },
    ];

    return {
      id: `lineage_${Date.now()}`,
      projectId,
      type: 'system-overview',
      title: 'Data Lineage Documentation',
      version: '1.0.0',
      sections,
      generatedAt: new Date(),
      generatedBy: 'ComplianceDocumentationGenerator',
      lastModified: new Date(),
      sourceFiles: [],
      businessRulesReferenced: [],
      completeness: 1,
      confidenceScore: 0.95,
      reviewStatus: 'draft',
    };
  }

  /**
   * Generate change management documentation
   */
  async generateChangeManagement(
    projectId: string,
    changes: ChangeRecord[]
  ): Promise<Document> {
    const sections: DocumentSection[] = [
      {
        id: 'summary',
        title: 'Change Summary',
        level: 1,
        content: this.generateChangeSummary(changes),
      },
      {
        id: 'changelog',
        title: 'Detailed Change Log',
        level: 2,
        content: this.formatTable(this.generateChangeTable(changes)),
      },
      {
        id: 'approvals',
        title: 'Approval Records',
        level: 2,
        content: this.generateApprovalsSection(changes),
      },
      {
        id: 'testing',
        title: 'Testing Evidence',
        level: 2,
        content: this.generateTestingEvidence(changes),
      },
      {
        id: 'rollback',
        title: 'Rollback Procedures',
        level: 2,
        content: this.generateRollbackProcedures(),
      },
    ];

    return {
      id: `change_mgmt_${Date.now()}`,
      projectId,
      type: 'migration-runbook',
      title: 'Change Management Documentation',
      version: '1.0.0',
      sections,
      generatedAt: new Date(),
      generatedBy: 'ComplianceDocumentationGenerator',
      lastModified: new Date(),
      sourceFiles: [],
      businessRulesReferenced: [],
      completeness: 1,
      confidenceScore: 0.9,
      reviewStatus: 'draft',
    };
  }

  /**
   * Generate audit trail report
   */
  async generateAuditTrailReport(
    projectId: string,
    entries: AuditEntry[],
    period: { start: Date; end: Date }
  ): Promise<Document> {
    const filteredEntries = entries.filter(
      e => e.timestamp >= period.start && e.timestamp <= period.end
    );

    const sections: DocumentSection[] = [
      {
        id: 'executive-summary',
        title: 'Executive Summary',
        level: 1,
        content: this.generateAuditSummary(filteredEntries, period),
      },
      {
        id: 'access-log',
        title: 'Access Log',
        level: 2,
        content: this.formatTable(this.generateAccessLogTable(filteredEntries)),
      },
      {
        id: 'modifications',
        title: 'Data Modifications',
        level: 2,
        content: this.formatTable(this.generateModificationTable(filteredEntries)),
      },
      {
        id: 'anomalies',
        title: 'Anomalies and Alerts',
        level: 2,
        content: this.generateAnomaliesSection(filteredEntries),
      },
    ];

    return {
      id: `audit_trail_${Date.now()}`,
      projectId,
      type: 'system-overview',
      title: 'Audit Trail Report',
      version: '1.0.0',
      sections,
      generatedAt: new Date(),
      generatedBy: 'ComplianceDocumentationGenerator',
      lastModified: new Date(),
      sourceFiles: [],
      businessRulesReferenced: [],
      completeness: 1,
      confidenceScore: 1,
      reviewStatus: 'draft',
    };
  }

  /**
   * Get available frameworks
   */
  getFrameworks(): ComplianceFramework[] {
    return Array.from(this.frameworks.values());
  }

  /**
   * Add custom framework
   */
  addFramework(framework: ComplianceFramework): void {
    this.frameworks.set(framework.id, framework);
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private initializeFrameworks(): void {
    // SOX Framework
    this.frameworks.set('sox', {
      id: 'sox',
      name: 'Sarbanes-Oxley Act',
      version: '2002',
      requirements: [
        {
          id: 'sox-302',
          code: 'SOX 302',
          title: 'Corporate Responsibility for Financial Reports',
          description: 'Certify accuracy and completeness of financial reports',
          controlFamily: 'financial-reporting',
          criticality: 'critical',
          evidenceTypes: ['audit-trail', 'approval-records', 'change-management'],
        },
        {
          id: 'sox-404',
          code: 'SOX 404',
          title: 'Management Assessment of Internal Controls',
          description: 'Assess effectiveness of internal controls',
          controlFamily: 'internal-controls',
          criticality: 'critical',
          evidenceTypes: ['access-control', 'change-management', 'testing-evidence'],
        },
      ],
      controlFamilies: [
        { id: 'financial-reporting', name: 'Financial Reporting', description: 'Controls for financial data accuracy', requirements: ['sox-302'] },
        { id: 'internal-controls', name: 'Internal Controls', description: 'Controls effectiveness assessment', requirements: ['sox-404'] },
      ],
    });

    // SOC 2 Framework
    this.frameworks.set('soc2', {
      id: 'soc2',
      name: 'SOC 2 Type II',
      version: '2017',
      requirements: [
        {
          id: 'soc2-cc6.1',
          code: 'CC6.1',
          title: 'Logical Access Security',
          description: 'Implement logical access security software',
          controlFamily: 'security',
          criticality: 'high',
          evidenceTypes: ['access-control', 'encryption'],
        },
        {
          id: 'soc2-cc7.1',
          code: 'CC7.1',
          title: 'System Operations',
          description: 'Detect and respond to security incidents',
          controlFamily: 'operations',
          criticality: 'high',
          evidenceTypes: ['audit-trail', 'business-continuity'],
        },
        {
          id: 'soc2-cc8.1',
          code: 'CC8.1',
          title: 'Change Management',
          description: 'Manage changes to system components',
          controlFamily: 'change-management',
          criticality: 'high',
          evidenceTypes: ['change-management', 'testing-evidence', 'approval-records'],
        },
      ],
      controlFamilies: [
        { id: 'security', name: 'Security', description: 'Information security controls', requirements: ['soc2-cc6.1'] },
        { id: 'operations', name: 'Operations', description: 'System operations controls', requirements: ['soc2-cc7.1'] },
        { id: 'change-management', name: 'Change Management', description: 'Change management controls', requirements: ['soc2-cc8.1'] },
      ],
    });

    // GDPR Framework
    this.frameworks.set('gdpr', {
      id: 'gdpr',
      name: 'General Data Protection Regulation',
      version: '2018',
      requirements: [
        {
          id: 'gdpr-art5',
          code: 'Article 5',
          title: 'Principles of Processing',
          description: 'Lawfulness, fairness, transparency of data processing',
          controlFamily: 'data-processing',
          criticality: 'critical',
          evidenceTypes: ['data-lineage', 'audit-trail'],
        },
        {
          id: 'gdpr-art17',
          code: 'Article 17',
          title: 'Right to Erasure',
          description: 'Support for data subject right to be forgotten',
          controlFamily: 'data-rights',
          criticality: 'high',
          evidenceTypes: ['data-lineage', 'data-retention'],
        },
        {
          id: 'gdpr-art32',
          code: 'Article 32',
          title: 'Security of Processing',
          description: 'Implement appropriate technical and organizational measures',
          controlFamily: 'security',
          criticality: 'critical',
          evidenceTypes: ['encryption', 'access-control'],
        },
      ],
      controlFamilies: [
        { id: 'data-processing', name: 'Data Processing', description: 'Data processing principles', requirements: ['gdpr-art5'] },
        { id: 'data-rights', name: 'Data Subject Rights', description: 'Support for data subject rights', requirements: ['gdpr-art17'] },
        { id: 'security', name: 'Security', description: 'Data protection security measures', requirements: ['gdpr-art32'] },
      ],
    });
  }

  private async collectEvidence(
    _projectId: string,
    rules: BusinessRule[],
    auditTrail: AuditEntry[]
  ): Promise<ComplianceEvidence[]> {
    const evidence: ComplianceEvidence[] = [];

    // Collect evidence from business rules
    for (const rule of rules) {
      evidence.push({
        id: `evidence_rule_${rule.id}`,
        requirementId: '', // Will be mapped later
        type: 'data-lineage',
        title: `Business Rule: ${rule.name}`,
        description: rule.description,
        metadata: { ruleId: rule.id, category: rule.category },
        collectedAt: new Date(),
      });
    }

    // Collect evidence from audit trail
    const accessEvents = auditTrail.filter(e => e.type === 'access');
    if (accessEvents.length > 0) {
      evidence.push({
        id: `evidence_access_${Date.now()}`,
        requirementId: '',
        type: 'access-control',
        title: 'Access Control Evidence',
        description: `${accessEvents.length} access events recorded`,
        metadata: { eventCount: accessEvents.length },
        collectedAt: new Date(),
      });
    }

    const changeEvents = auditTrail.filter(e => e.type === 'modification');
    if (changeEvents.length > 0) {
      evidence.push({
        id: `evidence_changes_${Date.now()}`,
        requirementId: '',
        type: 'change-management',
        title: 'Change Management Evidence',
        description: `${changeEvents.length} changes recorded`,
        metadata: { eventCount: changeEvents.length },
        collectedAt: new Date(),
      });
    }

    return evidence;
  }

  private assessRequirements(
    framework: ComplianceFramework,
    evidence: ComplianceEvidence[]
  ): RequirementResult[] {
    return framework.requirements.map(req => {
      const relevantEvidence = evidence.filter(e =>
        req.evidenceTypes.includes(e.type)
      );

      const score = this.calculateRequirementScore(req, relevantEvidence);
      
      return {
        requirementId: req.id,
        status: this.determineRequirementStatus(score),
        score,
        evidence: relevantEvidence,
        notes: `Assessed with ${relevantEvidence.length} pieces of evidence`,
      };
    });
  }

  private calculateRequirementScore(
    _requirement: ComplianceRequirement,
    evidence: ComplianceEvidence[]
  ): number {
    if (evidence.length === 0) return 0;
    // Simple scoring: more evidence = higher score, capped at 1
    return Math.min(evidence.length * 0.25, 1);
  }

  private determineRequirementStatus(score: number): RequirementResult['status'] {
    if (score >= 0.8) return 'met';
    if (score >= 0.5) return 'partially-met';
    return 'not-met';
  }

  private calculateControlFamilyScores(
    framework: ComplianceFramework,
    results: RequirementResult[]
  ): Record<string, number> {
    const scores: Record<string, number> = {};

    for (const family of framework.controlFamilies) {
      const familyResults = results.filter(r =>
        family.requirements.includes(r.requirementId)
      );
      
      if (familyResults.length > 0) {
        scores[family.id] = familyResults.reduce((sum, r) => sum + r.score, 0) / familyResults.length;
      }
    }

    return scores;
  }

  private calculateOverallScore(results: RequirementResult[]): number {
    if (results.length === 0) return 0;
    return results.reduce((sum, r) => sum + r.score, 0) / results.length;
  }

  private determineStatus(score: number): ComplianceReport['status'] {
    if (score >= 0.8) return 'compliant';
    if (score >= 0.5) return 'partially-compliant';
    return 'non-compliant';
  }

  private identifyGaps(
    framework: ComplianceFramework,
    results: RequirementResult[]
  ): ComplianceGap[] {
    const gaps: ComplianceGap[] = [];

    for (const result of results) {
      if (result.status !== 'met') {
        const requirement = framework.requirements.find(r => r.id === result.requirementId);
        if (requirement) {
          gaps.push({
            requirementId: result.requirementId,
            description: `${requirement.title} is ${result.status}`,
            severity: requirement.criticality,
            remediation: `Provide additional evidence for ${requirement.evidenceTypes.join(', ')}`,
            estimatedEffort: result.score < 0.3 ? 'High' : 'Medium',
          });
        }
      }
    }

    return gaps;
  }

  private generateRecommendations(gaps: ComplianceGap[]): ComplianceRecommendation[] {
    const recommendations: ComplianceRecommendation[] = [];

    // Group gaps by severity
    const criticalGaps = gaps.filter(g => g.severity === 'critical');
    const highGaps = gaps.filter(g => g.severity === 'high');

    if (criticalGaps.length > 0) {
      recommendations.push({
        id: `rec_critical_${Date.now()}`,
        title: 'Address Critical Compliance Gaps',
        description: `${criticalGaps.length} critical gaps require immediate attention`,
        priority: 'high',
        impactedRequirements: criticalGaps.map(g => g.requirementId),
        implementationSteps: [
          'Review each critical gap',
          'Identify responsible parties',
          'Create remediation plan with deadlines',
          'Implement controls and collect evidence',
        ],
      });
    }

    if (highGaps.length > 0) {
      recommendations.push({
        id: `rec_high_${Date.now()}`,
        title: 'Remediate High-Priority Gaps',
        description: `${highGaps.length} high-priority gaps should be addressed soon`,
        priority: 'medium',
        impactedRequirements: highGaps.map(g => g.requirementId),
        implementationSteps: [
          'Prioritize based on business impact',
          'Allocate resources for remediation',
          'Track progress against targets',
        ],
      });
    }

    return recommendations;
  }

  private async generateSupportingDocuments(
    projectId: string,
    _framework: ComplianceFramework,
    _results: RequirementResult[],
    _evidence: ComplianceEvidence[]
  ): Promise<string[]> {
    // Generate document IDs
    return [
      `${projectId}_data_lineage`,
      `${projectId}_access_controls`,
      `${projectId}_change_log`,
    ];
  }

  // Document generation helpers
  private generateDataLineageOverview(flows: DataFlow[]): string {
    return `This document describes the data lineage for ${flows.length} data flows in the system.`;
  }

  private generateLineageDiagram(_flows: DataFlow[]): { type: string; content: string } {
    return { type: 'mermaid', content: 'graph LR\n  A[Source] --> B[Transform] --> C[Destination]' };
  }

  private generateSourcesSection(flows: DataFlow[]): string {
    const sources = [...new Set(flows.map(f => f.source))];
    return `Data sources: ${sources.join(', ')}`;
  }

  private generateTransformationsSection(flows: DataFlow[]): string {
    return flows.map(f => `- ${f.source} → ${f.destination}: ${f.transformation}`).join('\n');
  }

  private extractTransformationCode(flows: DataFlow[]): { language: string; code: string }[] {
    return flows.filter(f => f.code).map(f => ({ language: 'typescript', code: f.code! }));
  }

  private generateDestinationsSection(flows: DataFlow[]): string {
    const destinations = [...new Set(flows.map(f => f.destination))];
    return `Data destinations: ${destinations.join(', ')}`;
  }

  private generateControlsSection(_flows: DataFlow[]): string {
    return 'Data quality controls include validation, transformation verification, and reconciliation.';
  }

  private formatTable(table: { headers: string[]; rows: string[][] }): string {
    const { headers, rows } = table;
    const lines: string[] = [];
    
    // Header row
    lines.push('| ' + headers.join(' | ') + ' |');
    // Separator
    lines.push('| ' + headers.map(() => '---').join(' | ') + ' |');
    // Data rows
    for (const row of rows) {
      lines.push('| ' + row.join(' | ') + ' |');
    }
    
    return lines.join('\n');
  }

  private generateChangeSummary(changes: ChangeRecord[]): string {
    return `Total changes: ${changes.length}\nApproved: ${changes.filter(c => c.approved).length}`;
  }

  private generateChangeTable(changes: ChangeRecord[]): { headers: string[]; rows: string[][] } {
    return {
      headers: ['Date', 'Type', 'Description', 'Author', 'Approved'],
      rows: changes.map(c => [
        c.date.toISOString(),
        c.type,
        c.description,
        c.author,
        c.approved ? 'Yes' : 'No',
      ]),
    };
  }

  private generateApprovalsSection(changes: ChangeRecord[]): string {
    const approved = changes.filter(c => c.approved);
    return `${approved.length} of ${changes.length} changes have been approved.`;
  }

  private generateTestingEvidence(changes: ChangeRecord[]): string {
    const tested = changes.filter(c => c.tested);
    return `${tested.length} changes have associated test evidence.`;
  }

  private generateRollbackProcedures(): string {
    return 'Rollback procedures are documented in the migration runbook.';
  }

  private generateAuditSummary(entries: AuditEntry[], period: { start: Date; end: Date }): string {
    return `Audit period: ${period.start.toISOString()} to ${period.end.toISOString()}\nTotal entries: ${entries.length}`;
  }

  private generateAccessLogTable(entries: AuditEntry[]): { headers: string[]; rows: string[][] } {
    const accessEntries = entries.filter(e => e.type === 'access');
    return {
      headers: ['Timestamp', 'User', 'Resource', 'Action'],
      rows: accessEntries.map(e => [
        e.timestamp.toISOString(),
        e.user,
        e.resource,
        e.action,
      ]),
    };
  }

  private generateModificationTable(entries: AuditEntry[]): { headers: string[]; rows: string[][] } {
    const modEntries = entries.filter(e => e.type === 'modification');
    return {
      headers: ['Timestamp', 'User', 'Resource', 'Before', 'After'],
      rows: modEntries.map(e => [
        e.timestamp.toISOString(),
        e.user,
        e.resource,
        JSON.stringify(e.before || ''),
        JSON.stringify(e.after || ''),
      ]),
    };
  }

  private generateAnomaliesSection(entries: AuditEntry[]): string {
    const anomalies = entries.filter(e => e.anomaly);
    if (anomalies.length === 0) {
      return 'No anomalies detected during the audit period.';
    }
    return `${anomalies.length} anomalies detected:\n${anomalies.map(a => `- ${a.description}`).join('\n')}`;
  }
}

// ============================================================================
// Supporting Types
// ============================================================================

export interface AuditEntry {
  id: string;
  timestamp: Date;
  type: 'access' | 'modification' | 'deletion' | 'error';
  user: string;
  resource: string;
  action: string;
  before?: unknown;
  after?: unknown;
  anomaly?: boolean;
  description?: string;
}

export interface DataFlow {
  id: string;
  source: string;
  destination: string;
  transformation: string;
  code?: string;
  dataElements: string[];
}

export interface ChangeRecord {
  id: string;
  date: Date;
  type: string;
  description: string;
  author: string;
  approved: boolean;
  approver?: string;
  tested: boolean;
  testResults?: string;
}

// ============================================================================
// Factory
// ============================================================================

export function createComplianceDocumentationGenerator(): ComplianceDocumentationGenerator {
  return new ComplianceDocumentationGenerator();
}

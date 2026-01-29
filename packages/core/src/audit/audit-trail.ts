/**
 * Audit Trail System
 * 
 * Comprehensive audit logging for SOX, SOC 2, PCI-DSS compliant
 * migration tracking with complete traceability.
 */

// ============================================================================
// Audit Event Types
// ============================================================================

export type AuditEventCategory = 
  | 'rule-extraction'
  | 'code-generation'
  | 'validation'
  | 'review'
  | 'deployment'
  | 'configuration'
  | 'access'
  | 'data-handling';

export type AuditAction =
  | 'create'
  | 'read'
  | 'update'
  | 'delete'
  | 'approve'
  | 'reject'
  | 'execute'
  | 'export'
  | 'import';

export interface AuditEvent {
  id: string;
  timestamp: string;
  category: AuditEventCategory;
  action: AuditAction;
  actor: AuditActor;
  target: AuditTarget;
  details: AuditDetails;
  outcome: 'success' | 'failure' | 'partial';
  metadata: AuditMetadata;
}

export interface AuditActor {
  type: 'user' | 'system' | 'automated';
  id: string;
  name: string;
  role?: string;
  ip?: string;
  userAgent?: string;
}

export interface AuditTarget {
  type: 'project' | 'file' | 'rule' | 'test' | 'deployment' | 'configuration' | 'user';
  id: string;
  name: string;
  path?: string;
}

export interface AuditDetails {
  description: string;
  changes?: ChangeRecord[];
  inputs?: Record<string, unknown>;
  outputs?: Record<string, unknown>;
  duration?: number;
  errorMessage?: string;
}

export interface ChangeRecord {
  field: string;
  previousValue: unknown;
  newValue: unknown;
  reason?: string;
}

export interface AuditMetadata {
  projectId: string;
  environment: 'development' | 'staging' | 'production';
  version: string;
  correlationId?: string;
  sessionId?: string;
  complianceFrameworks?: string[];
}

// ============================================================================
// Audit Trail Interface
// ============================================================================

export interface AuditQuery {
  projectId?: string;
  category?: AuditEventCategory;
  action?: AuditAction;
  actorId?: string;
  targetId?: string;
  targetType?: string;
  startDate?: string;
  endDate?: string;
  outcome?: string;
  limit?: number;
  offset?: number;
}

export interface AuditReport {
  generatedAt: string;
  period: { start: string; end: string };
  projectId: string;
  summary: AuditSummary;
  events: AuditEvent[];
  complianceStatus: ComplianceStatus;
}

export interface AuditSummary {
  totalEvents: number;
  byCategory: Record<AuditEventCategory, number>;
  byAction: Record<AuditAction, number>;
  byOutcome: Record<string, number>;
  uniqueActors: number;
  criticalEvents: number;
}

export interface ComplianceStatus {
  frameworks: string[];
  overallCompliant: boolean;
  findings: ComplianceFinding[];
  lastAssessment: string;
}

export interface ComplianceFinding {
  framework: string;
  requirement: string;
  status: 'compliant' | 'non-compliant' | 'partial' | 'not-applicable';
  evidence: string[];
  gaps?: string[];
}

// ============================================================================
// Audit Trail Implementation
// ============================================================================

export class AuditTrail {
  private events: Map<string, AuditEvent> = new Map();
  private eventsByProject: Map<string, Set<string>> = new Map();
  private eventsByActor: Map<string, Set<string>> = new Map();
  private eventsByTarget: Map<string, Set<string>> = new Map();

  /**
   * Log an audit event
   */
  log(
    event: Omit<AuditEvent, 'id' | 'timestamp'>
  ): AuditEvent {
    const id = `audit_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const timestamp = new Date().toISOString();

    const fullEvent: AuditEvent = {
      ...event,
      id,
      timestamp,
    };

    this.events.set(id, fullEvent);
    this.indexEvent(fullEvent);

    return fullEvent;
  }

  /**
   * Log rule extraction event
   */
  logRuleExtraction(
    projectId: string,
    actor: AuditActor,
    ruleId: string,
    ruleName: string,
    confidence: number,
    sourceFile: string
  ): AuditEvent {
    return this.log({
      category: 'rule-extraction',
      action: 'create',
      actor,
      target: {
        type: 'rule',
        id: ruleId,
        name: ruleName,
        path: sourceFile,
      },
      details: {
        description: `Extracted business rule: ${ruleName}`,
        outputs: { confidence, sourceFile },
      },
      outcome: 'success',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Log code generation event
   */
  logCodeGeneration(
    projectId: string,
    actor: AuditActor,
    sourceFile: string,
    targetFile: string,
    language: string,
    linesGenerated: number
  ): AuditEvent {
    return this.log({
      category: 'code-generation',
      action: 'create',
      actor,
      target: {
        type: 'file',
        id: targetFile,
        name: targetFile.split('/').pop() || targetFile,
        path: targetFile,
      },
      details: {
        description: `Generated ${language} code from ${sourceFile}`,
        inputs: { sourceFile },
        outputs: { targetFile, language, linesGenerated },
      },
      outcome: 'success',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Log validation event
   */
  logValidation(
    projectId: string,
    actor: AuditActor,
    validationType: string,
    targetId: string,
    passed: boolean,
    details: { testCount?: number; passedCount?: number; failedCount?: number }
  ): AuditEvent {
    return this.log({
      category: 'validation',
      action: 'execute',
      actor,
      target: {
        type: 'test',
        id: targetId,
        name: validationType,
      },
      details: {
        description: `${validationType} validation ${passed ? 'passed' : 'failed'}`,
        outputs: details,
      },
      outcome: passed ? 'success' : 'failure',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Log review event
   */
  logReview(
    projectId: string,
    actor: AuditActor,
    reviewId: string,
    targetId: string,
    decision: 'approve' | 'reject',
    comment?: string
  ): AuditEvent {
    return this.log({
      category: 'review',
      action: decision,
      actor,
      target: {
        type: 'rule',
        id: targetId,
        name: reviewId,
      },
      details: {
        description: `Review ${decision}d: ${reviewId}`,
        inputs: { comment },
      },
      outcome: 'success',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Log deployment event
   */
  logDeployment(
    projectId: string,
    actor: AuditActor,
    environment: 'development' | 'staging' | 'production',
    version: string,
    success: boolean,
    details: Record<string, unknown>
  ): AuditEvent {
    return this.log({
      category: 'deployment',
      action: 'execute',
      actor,
      target: {
        type: 'deployment',
        id: `deploy_${environment}_${version}`,
        name: `${environment} deployment v${version}`,
      },
      details: {
        description: `Deployed to ${environment}`,
        outputs: details,
      },
      outcome: success ? 'success' : 'failure',
      metadata: {
        projectId,
        environment,
        version,
      },
    });
  }

  /**
   * Log configuration change
   */
  logConfigChange(
    projectId: string,
    actor: AuditActor,
    configType: string,
    changes: ChangeRecord[]
  ): AuditEvent {
    return this.log({
      category: 'configuration',
      action: 'update',
      actor,
      target: {
        type: 'configuration',
        id: configType,
        name: configType,
      },
      details: {
        description: `Configuration updated: ${configType}`,
        changes,
      },
      outcome: 'success',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Log access event
   */
  logAccess(
    projectId: string,
    actor: AuditActor,
    resource: string,
    action: 'read' | 'export',
    success: boolean
  ): AuditEvent {
    return this.log({
      category: 'access',
      action,
      actor,
      target: {
        type: 'project',
        id: projectId,
        name: resource,
      },
      details: {
        description: `${action} access to ${resource}`,
      },
      outcome: success ? 'success' : 'failure',
      metadata: {
        projectId,
        environment: 'development',
        version: '1.0.0',
      },
    });
  }

  /**
   * Query audit events
   */
  query(q: AuditQuery): AuditEvent[] {
    let results = Array.from(this.events.values());

    if (q.projectId) {
      const projectEventIds = this.eventsByProject.get(q.projectId);
      if (!projectEventIds) return [];
      results = results.filter(e => projectEventIds.has(e.id));
    }

    if (q.category) {
      results = results.filter(e => e.category === q.category);
    }

    if (q.action) {
      results = results.filter(e => e.action === q.action);
    }

    if (q.actorId) {
      const actorEventIds = this.eventsByActor.get(q.actorId);
      if (!actorEventIds) return [];
      results = results.filter(e => actorEventIds.has(e.id));
    }

    if (q.targetId) {
      const targetEventIds = this.eventsByTarget.get(q.targetId);
      if (!targetEventIds) return [];
      results = results.filter(e => targetEventIds.has(e.id));
    }

    if (q.targetType) {
      results = results.filter(e => e.target.type === q.targetType);
    }

    if (q.startDate) {
      const start = new Date(q.startDate);
      results = results.filter(e => new Date(e.timestamp) >= start);
    }

    if (q.endDate) {
      const end = new Date(q.endDate);
      results = results.filter(e => new Date(e.timestamp) <= end);
    }

    if (q.outcome) {
      results = results.filter(e => e.outcome === q.outcome);
    }

    // Sort by timestamp descending
    results.sort((a, b) => 
      new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
    );

    // Apply pagination
    const offset = q.offset || 0;
    const limit = q.limit || 100;
    return results.slice(offset, offset + limit);
  }

  /**
   * Generate audit report
   */
  generateReport(
    projectId: string,
    startDate: string,
    endDate: string
  ): AuditReport {
    const events = this.query({
      projectId,
      startDate,
      endDate,
    });

    const summary = this.calculateSummary(events);
    const complianceStatus = this.assessCompliance(events);

    return {
      generatedAt: new Date().toISOString(),
      period: { start: startDate, end: endDate },
      projectId,
      summary,
      events,
      complianceStatus,
    };
  }

  /**
   * Export audit trail for compliance purposes
   */
  exportForCompliance(
    projectId: string,
    framework: string,
    format: 'json' | 'csv'
  ): string {
    const events = this.query({ projectId });

    if (format === 'csv') {
      return this.toCSV(events);
    }

    return JSON.stringify({
      exportedAt: new Date().toISOString(),
      framework,
      projectId,
      eventCount: events.length,
      events,
    }, null, 2);
  }

  /**
   * Get traceability chain for a rule
   */
  getTraceability(ruleId: string): AuditEvent[] {
    const events = this.query({ targetId: ruleId });
    
    // Sort chronologically
    return events.sort((a, b) => 
      new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
    );
  }

  private indexEvent(event: AuditEvent): void {
    // Index by project
    const projectId = event.metadata.projectId;
    if (!this.eventsByProject.has(projectId)) {
      this.eventsByProject.set(projectId, new Set());
    }
    this.eventsByProject.get(projectId)!.add(event.id);

    // Index by actor
    if (!this.eventsByActor.has(event.actor.id)) {
      this.eventsByActor.set(event.actor.id, new Set());
    }
    this.eventsByActor.get(event.actor.id)!.add(event.id);

    // Index by target
    if (!this.eventsByTarget.has(event.target.id)) {
      this.eventsByTarget.set(event.target.id, new Set());
    }
    this.eventsByTarget.get(event.target.id)!.add(event.id);
  }

  private calculateSummary(events: AuditEvent[]): AuditSummary {
    const byCategory: Record<string, number> = {};
    const byAction: Record<string, number> = {};
    const byOutcome: Record<string, number> = {};
    const actors = new Set<string>();
    let criticalEvents = 0;

    for (const event of events) {
      byCategory[event.category] = (byCategory[event.category] || 0) + 1;
      byAction[event.action] = (byAction[event.action] || 0) + 1;
      byOutcome[event.outcome] = (byOutcome[event.outcome] || 0) + 1;
      actors.add(event.actor.id);
      
      if (event.outcome === 'failure') {
        criticalEvents++;
      }
    }

    return {
      totalEvents: events.length,
      byCategory: byCategory as Record<AuditEventCategory, number>,
      byAction: byAction as Record<AuditAction, number>,
      byOutcome,
      uniqueActors: actors.size,
      criticalEvents,
    };
  }

  private assessCompliance(events: AuditEvent[]): ComplianceStatus {
    const findings: ComplianceFinding[] = [];

    // Check for proper review process
    const reviewEvents = events.filter(e => e.category === 'review');
    const ruleEvents = events.filter(e => e.category === 'rule-extraction');
    const reviewRate = ruleEvents.length > 0 
      ? reviewEvents.length / ruleEvents.length 
      : 0;

    findings.push({
      framework: 'SOX',
      requirement: 'All business rules must be reviewed before deployment',
      status: reviewRate >= 0.9 ? 'compliant' : reviewRate >= 0.5 ? 'partial' : 'non-compliant',
      evidence: [`Review rate: ${(reviewRate * 100).toFixed(1)}%`],
      gaps: reviewRate < 0.9 ? [`${((1 - reviewRate) * 100).toFixed(1)}% of rules not reviewed`] : undefined,
    });

    // Check for validation
    const validationEvents = events.filter(e => e.category === 'validation');
    const deploymentEvents = events.filter(e => e.category === 'deployment');

    findings.push({
      framework: 'SOC2',
      requirement: 'All deployments must be preceded by validation',
      status: validationEvents.length >= deploymentEvents.length ? 'compliant' : 'non-compliant',
      evidence: [
        `Validations: ${validationEvents.length}`,
        `Deployments: ${deploymentEvents.length}`,
      ],
    });

    // Check for audit trail completeness
    findings.push({
      framework: 'PCI-DSS',
      requirement: 'Complete audit trail for all changes',
      status: events.length > 0 ? 'compliant' : 'non-compliant',
      evidence: [`${events.length} events logged`],
    });

    return {
      frameworks: ['SOX', 'SOC2', 'PCI-DSS'],
      overallCompliant: findings.every(f => f.status === 'compliant'),
      findings,
      lastAssessment: new Date().toISOString(),
    };
  }

  private toCSV(events: AuditEvent[]): string {
    const headers = [
      'id', 'timestamp', 'category', 'action', 'actor_id', 'actor_name',
      'target_id', 'target_type', 'outcome', 'description'
    ];

    const rows = events.map(e => [
      e.id,
      e.timestamp,
      e.category,
      e.action,
      e.actor.id,
      e.actor.name,
      e.target.id,
      e.target.type,
      e.outcome,
      `"${e.details.description.replace(/"/g, '""')}"`,
    ].join(','));

    return [headers.join(','), ...rows].join('\n');
  }
}

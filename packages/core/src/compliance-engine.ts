/**
 * Compliance Rule Engine
 * 
 * Pre-built rule sets for SOX, PCI-DSS, HIPAA that validate
 * migrated code meets regulatory requirements.
 */

export type ComplianceFramework = 'sox' | 'pci-dss' | 'hipaa' | 'gdpr' | 'custom';

export interface ComplianceRule {
  id: string;
  framework: ComplianceFramework;
  category: string;
  name: string;
  description: string;
  requirement: string;
  severity: 'critical' | 'high' | 'medium' | 'low';
  automated: boolean;
  check: ComplianceCheck;
  remediation: string;
  references: string[];
}

export interface ComplianceCheck {
  type: 'code-pattern' | 'data-flow' | 'access-control' | 'encryption' | 'logging' | 'configuration';
  patterns?: string[];
  requiredElements?: string[];
  prohibitedElements?: string[];
  customValidator?: (context: ValidationContext) => ComplianceViolation[];
}

export interface ValidationContext {
  code: string;
  language: string;
  filename: string;
  dataStructures: Array<{ name: string; type: string; sensitive?: boolean }>;
  procedures: Array<{ name: string; accessesData: string[] }>;
  businessRules: Array<{ id: string; name: string; inputs: string[]; outputs: string[] }>;
  externalCalls: Array<{ name: string; type: string }>;
  fileOperations: Array<{ filename: string; operation: string }>;
}

export interface ComplianceViolation {
  ruleId: string;
  ruleName: string;
  framework: ComplianceFramework;
  severity: 'critical' | 'high' | 'medium' | 'low';
  location: { file: string; line?: number; column?: number };
  description: string;
  evidence: string;
  remediation: string;
  falsePositiveRisk: 'low' | 'medium' | 'high';
}

export interface ComplianceReport {
  projectId: string;
  timestamp: string;
  frameworks: ComplianceFramework[];
  summary: {
    totalRulesChecked: number;
    passed: number;
    violated: number;
    warnings: number;
    notApplicable: number;
    complianceScore: number;
  };
  byFramework: Partial<Record<ComplianceFramework, {
    rulesChecked: number;
    violations: number;
    score: number;
  }>>;
  violations: ComplianceViolation[];
  recommendations: string[];
}

export class ComplianceRuleEngine {
  private rules: Map<string, ComplianceRule> = new Map();
  private frameworks: Set<ComplianceFramework> = new Set();

  constructor() {
    this.loadBuiltInRules();
  }

  /**
   * Load built-in compliance rules
   */
  private loadBuiltInRules(): void {
    // SOX Rules
    this.addRule({
      id: 'sox-001',
      framework: 'sox',
      category: 'Access Control',
      name: 'Segregation of Duties',
      description: 'Ensure proper separation between data access and modification functions',
      requirement: 'SOX Section 404 - Internal controls',
      severity: 'critical',
      automated: true,
      check: {
        type: 'access-control',
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          // Check if any procedure both reads and writes to financial data
          for (const proc of ctx.procedures) {
            const accessesFinancialData = proc.accessesData.some(d => 
              d.toLowerCase().includes('balance') ||
              d.toLowerCase().includes('account') ||
              d.toLowerCase().includes('amount') ||
              d.toLowerCase().includes('transaction')
            );
            if (accessesFinancialData && proc.name.toLowerCase().includes('update')) {
              // Check if same module also does reporting
              const hasReporting = ctx.procedures.some(p => 
                p.name.toLowerCase().includes('report') ||
                p.name.toLowerCase().includes('display')
              );
              if (hasReporting) {
                violations.push({
                  ruleId: 'sox-001',
                  ruleName: 'Segregation of Duties',
                  framework: 'sox',
                  severity: 'critical',
                  location: { file: ctx.filename },
                  description: 'Same module handles both financial data modification and reporting',
                  evidence: `Procedure ${proc.name} modifies financial data in module that also has reporting functions`,
                  remediation: 'Separate data modification and reporting into different modules',
                  falsePositiveRisk: 'medium',
                });
              }
            }
          }
          return violations;
        },
      },
      remediation: 'Separate read and write operations into different modules or implement role-based access',
      references: ['SOX Section 404', 'COSO Framework'],
    });

    this.addRule({
      id: 'sox-002',
      framework: 'sox',
      category: 'Audit Trail',
      name: 'Financial Transaction Logging',
      description: 'All financial transactions must be logged with full audit trail',
      requirement: 'SOX Section 302 - Corporate responsibility for financial reports',
      severity: 'critical',
      automated: true,
      check: {
        type: 'logging',
        requiredElements: ['audit', 'log', 'trace', 'journal'],
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const hasLogging = ctx.code.toLowerCase().includes('audit') ||
                           ctx.code.toLowerCase().includes('log') ||
                           ctx.procedures.some(p => p.name.toLowerCase().includes('log'));
          
          const hasFinancialOps = ctx.procedures.some(p =>
            p.name.toLowerCase().includes('transaction') ||
            p.name.toLowerCase().includes('payment') ||
            p.name.toLowerCase().includes('transfer')
          );
          
          if (hasFinancialOps && !hasLogging) {
            violations.push({
              ruleId: 'sox-002',
              ruleName: 'Financial Transaction Logging',
              framework: 'sox',
              severity: 'critical',
              location: { file: ctx.filename },
              description: 'Financial operations without audit logging',
              evidence: 'No audit/logging mechanism found for financial transactions',
              remediation: 'Implement comprehensive audit logging for all financial transactions',
              falsePositiveRisk: 'low',
            });
          }
          return violations;
        },
      },
      remediation: 'Implement audit logging for all financial transactions with timestamp, user, and change details',
      references: ['SOX Section 302', 'SOX Section 404'],
    });

    // PCI-DSS Rules
    this.addRule({
      id: 'pci-001',
      framework: 'pci-dss',
      category: 'Data Protection',
      name: 'PAN Masking',
      description: 'Primary Account Numbers must be masked when displayed',
      requirement: 'PCI-DSS Requirement 3.3',
      severity: 'critical',
      automated: true,
      check: {
        type: 'data-flow',
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const panPatterns = ['card-number', 'pan', 'account-number', 'credit-card'];
          
          for (const ds of ctx.dataStructures) {
            const isPAN = panPatterns.some(p => ds.name.toLowerCase().includes(p));
            if (isPAN) {
              // Check if it's displayed without masking
              const displayProcs = ctx.procedures.filter(p =>
                p.name.toLowerCase().includes('display') ||
                p.name.toLowerCase().includes('print') ||
                p.name.toLowerCase().includes('show')
              );
              
              for (const proc of displayProcs) {
                if (proc.accessesData.includes(ds.name)) {
                  const hasMasking = ctx.code.toLowerCase().includes('mask') ||
                                    ctx.code.includes('***') ||
                                    ctx.code.includes('xxxx');
                  
                  if (!hasMasking) {
                    violations.push({
                      ruleId: 'pci-001',
                      ruleName: 'PAN Masking',
                      framework: 'pci-dss',
                      severity: 'critical',
                      location: { file: ctx.filename },
                      description: 'PAN displayed without masking',
                      evidence: `Data item ${ds.name} appears to be displayed in ${proc.name} without masking`,
                      remediation: 'Implement masking to show only last 4 digits of PAN',
                      falsePositiveRisk: 'medium',
                    });
                  }
                }
              }
            }
          }
          return violations;
        },
      },
      remediation: 'Mask all but the last 4 digits when displaying PANs (e.g., ****-****-****-1234)',
      references: ['PCI-DSS v4.0 Requirement 3.3'],
    });

    this.addRule({
      id: 'pci-002',
      framework: 'pci-dss',
      category: 'Encryption',
      name: 'Cardholder Data Encryption',
      description: 'Cardholder data must be encrypted at rest',
      requirement: 'PCI-DSS Requirement 3.4',
      severity: 'critical',
      automated: true,
      check: {
        type: 'encryption',
        requiredElements: ['encrypt', 'cipher', 'aes', 'crypto'],
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const sensitiveData = ctx.dataStructures.filter(ds => 
            ds.sensitive ||
            ds.name.toLowerCase().includes('card') ||
            ds.name.toLowerCase().includes('cvv') ||
            ds.name.toLowerCase().includes('pin')
          );
          
          if (sensitiveData.length > 0) {
            const hasEncryption = ctx.code.toLowerCase().includes('encrypt') ||
                                 ctx.code.toLowerCase().includes('cipher') ||
                                 ctx.code.toLowerCase().includes('aes');
            
            if (!hasEncryption) {
              violations.push({
                ruleId: 'pci-002',
                ruleName: 'Cardholder Data Encryption',
                framework: 'pci-dss',
                severity: 'critical',
                location: { file: ctx.filename },
                description: 'Cardholder data without encryption',
                evidence: `Sensitive data items found: ${sensitiveData.map(d => d.name).join(', ')}`,
                remediation: 'Implement strong encryption (AES-256) for cardholder data at rest',
                falsePositiveRisk: 'medium',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Use strong cryptography (AES-256) to encrypt stored cardholder data',
      references: ['PCI-DSS v4.0 Requirement 3.4', 'PCI-DSS v4.0 Requirement 3.5'],
    });

    this.addRule({
      id: 'pci-003',
      framework: 'pci-dss',
      category: 'Data Retention',
      name: 'Prohibited Data Storage',
      description: 'CVV/CVC and PIN must never be stored after authorization',
      requirement: 'PCI-DSS Requirement 3.2',
      severity: 'critical',
      automated: true,
      check: {
        type: 'data-flow',
        prohibitedElements: ['cvv', 'cvc', 'pin', 'magnetic-stripe', 'track-data'],
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const prohibitedData = ctx.dataStructures.filter(ds =>
            ds.name.toLowerCase().includes('cvv') ||
            ds.name.toLowerCase().includes('cvc') ||
            ds.name.toLowerCase().includes('pin') ||
            ds.name.toLowerCase().includes('track')
          );
          
          for (const ds of prohibitedData) {
            // Check if there's any file or database write
            const isStored = ctx.fileOperations.some(f => f.operation === 'write') ||
                           ctx.externalCalls.some(c => c.type === 'database');
            
            if (isStored) {
              violations.push({
                ruleId: 'pci-003',
                ruleName: 'Prohibited Data Storage',
                framework: 'pci-dss',
                severity: 'critical',
                location: { file: ctx.filename },
                description: 'Prohibited sensitive data appears to be stored',
                evidence: `Data item ${ds.name} may be written to storage`,
                remediation: 'Remove storage of CVV/CVC, PIN, and magnetic stripe data',
                falsePositiveRisk: 'high',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Never store CVV/CVC, PIN, or full magnetic stripe data after authorization',
      references: ['PCI-DSS v4.0 Requirement 3.2'],
    });

    // HIPAA Rules
    this.addRule({
      id: 'hipaa-001',
      framework: 'hipaa',
      category: 'Privacy',
      name: 'PHI Access Control',
      description: 'Protected Health Information must have access controls',
      requirement: 'HIPAA Security Rule 45 CFR 164.312(a)(1)',
      severity: 'critical',
      automated: true,
      check: {
        type: 'access-control',
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const phiPatterns = ['patient', 'diagnosis', 'treatment', 'medical', 'health', 'ssn', 'dob'];
          
          const phiData = ctx.dataStructures.filter(ds =>
            phiPatterns.some(p => ds.name.toLowerCase().includes(p))
          );
          
          if (phiData.length > 0) {
            const hasAccessControl = ctx.code.toLowerCase().includes('authorize') ||
                                    ctx.code.toLowerCase().includes('permission') ||
                                    ctx.code.toLowerCase().includes('access-check') ||
                                    ctx.code.toLowerCase().includes('role');
            
            if (!hasAccessControl) {
              violations.push({
                ruleId: 'hipaa-001',
                ruleName: 'PHI Access Control',
                framework: 'hipaa',
                severity: 'critical',
                location: { file: ctx.filename },
                description: 'PHI access without authorization check',
                evidence: `PHI data items: ${phiData.map(d => d.name).join(', ')}`,
                remediation: 'Implement role-based access control for all PHI access',
                falsePositiveRisk: 'medium',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Implement access controls to limit PHI access to authorized personnel only',
      references: ['45 CFR 164.312(a)(1)', '45 CFR 164.312(d)'],
    });

    this.addRule({
      id: 'hipaa-002',
      framework: 'hipaa',
      category: 'Audit',
      name: 'PHI Access Logging',
      description: 'All access to PHI must be logged',
      requirement: 'HIPAA Security Rule 45 CFR 164.312(b)',
      severity: 'high',
      automated: true,
      check: {
        type: 'logging',
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const phiPatterns = ['patient', 'diagnosis', 'treatment', 'medical', 'health'];
          
          const accessesPHI = ctx.procedures.some(p =>
            p.accessesData.some(d => phiPatterns.some(pattern => d.toLowerCase().includes(pattern)))
          );
          
          if (accessesPHI) {
            const hasAuditLog = ctx.code.toLowerCase().includes('audit') ||
                               ctx.procedures.some(p => p.name.toLowerCase().includes('log'));
            
            if (!hasAuditLog) {
              violations.push({
                ruleId: 'hipaa-002',
                ruleName: 'PHI Access Logging',
                framework: 'hipaa',
                severity: 'high',
                location: { file: ctx.filename },
                description: 'PHI access without audit logging',
                evidence: 'No audit logging mechanism found for PHI access',
                remediation: 'Implement audit logging for all PHI access with user, timestamp, and action',
                falsePositiveRisk: 'low',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Log all PHI access including user ID, timestamp, action, and data accessed',
      references: ['45 CFR 164.312(b)', '45 CFR 164.308(a)(1)(ii)(D)'],
    });

    // GDPR Rules
    this.addRule({
      id: 'gdpr-001',
      framework: 'gdpr',
      category: 'Privacy',
      name: 'Data Minimization',
      description: 'Only collect and process data that is necessary',
      requirement: 'GDPR Article 5(1)(c)',
      severity: 'medium',
      automated: true,
      check: {
        type: 'data-flow',
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const personalData = ctx.dataStructures.filter(ds =>
            ds.name.toLowerCase().includes('name') ||
            ds.name.toLowerCase().includes('email') ||
            ds.name.toLowerCase().includes('address') ||
            ds.name.toLowerCase().includes('phone') ||
            ds.name.toLowerCase().includes('birth')
          );
          
          // Check if personal data is used
          for (const pd of personalData) {
            const usedIn = ctx.procedures.filter(p => p.accessesData.includes(pd.name));
            if (usedIn.length === 0) {
              violations.push({
                ruleId: 'gdpr-001',
                ruleName: 'Data Minimization',
                framework: 'gdpr',
                severity: 'medium',
                location: { file: ctx.filename },
                description: 'Personal data collected but not used',
                evidence: `Data item ${pd.name} is defined but not referenced`,
                remediation: 'Remove unnecessary personal data collection',
                falsePositiveRisk: 'high',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Review and remove collection of unnecessary personal data',
      references: ['GDPR Article 5(1)(c)'],
    });

    this.addRule({
      id: 'gdpr-002',
      framework: 'gdpr',
      category: 'Rights',
      name: 'Right to Erasure Support',
      description: 'System must support deletion of personal data',
      requirement: 'GDPR Article 17',
      severity: 'high',
      automated: true,
      check: {
        type: 'code-pattern',
        requiredElements: ['delete', 'remove', 'erase', 'purge'],
        customValidator: (ctx) => {
          const violations: ComplianceViolation[] = [];
          const hasPersonalData = ctx.dataStructures.some(ds =>
            ds.name.toLowerCase().includes('name') ||
            ds.name.toLowerCase().includes('email') ||
            ds.name.toLowerCase().includes('customer')
          );
          
          if (hasPersonalData) {
            const hasDeleteCapability = ctx.procedures.some(p =>
              p.name.toLowerCase().includes('delete') ||
              p.name.toLowerCase().includes('remove') ||
              p.name.toLowerCase().includes('erase')
            );
            
            if (!hasDeleteCapability) {
              violations.push({
                ruleId: 'gdpr-002',
                ruleName: 'Right to Erasure Support',
                framework: 'gdpr',
                severity: 'high',
                location: { file: ctx.filename },
                description: 'No data deletion capability found',
                evidence: 'Personal data present without delete/erase functionality',
                remediation: 'Implement data deletion functionality to support right to erasure',
                falsePositiveRisk: 'medium',
              });
            }
          }
          return violations;
        },
      },
      remediation: 'Implement functionality to completely erase personal data on request',
      references: ['GDPR Article 17', 'GDPR Article 19'],
    });
  }

  /**
   * Add a compliance rule
   */
  addRule(rule: ComplianceRule): void {
    this.rules.set(rule.id, rule);
    this.frameworks.add(rule.framework);
  }

  /**
   * Validate code against compliance rules
   */
  validate(
    context: ValidationContext,
    frameworks?: ComplianceFramework[]
  ): ComplianceReport {
    const targetFrameworks = frameworks || Array.from(this.frameworks);
    const violations: ComplianceViolation[] = [];
    const byFramework: Partial<Record<ComplianceFramework, { rulesChecked: number; violations: number; score: number }>> = {};

    let totalRulesChecked = 0;
    let totalPassed = 0;

    for (const framework of targetFrameworks) {
      const frameworkRules = Array.from(this.rules.values()).filter(r => r.framework === framework);
      let frameworkViolations = 0;

      for (const rule of frameworkRules) {
        if (!rule.automated) continue;
        
        totalRulesChecked++;
        const ruleViolations = this.checkRule(rule, context);
        
        if (ruleViolations.length === 0) {
          totalPassed++;
        } else {
          frameworkViolations += ruleViolations.length;
          violations.push(...ruleViolations);
        }
      }

      byFramework[framework] = {
        rulesChecked: frameworkRules.filter(r => r.automated).length,
        violations: frameworkViolations,
        score: frameworkRules.length > 0
          ? Math.round(((frameworkRules.length - frameworkViolations) / frameworkRules.length) * 100)
          : 100,
      };
    }

    const complianceScore = totalRulesChecked > 0
      ? Math.round((totalPassed / totalRulesChecked) * 100)
      : 100;

    const recommendations = this.generateRecommendations(violations);

    return {
      projectId: '',
      timestamp: new Date().toISOString(),
      frameworks: targetFrameworks,
      summary: {
        totalRulesChecked,
        passed: totalPassed,
        violated: violations.length,
        warnings: violations.filter(v => v.severity === 'medium' || v.severity === 'low').length,
        notApplicable: 0,
        complianceScore,
      },
      byFramework,
      violations,
      recommendations,
    };
  }

  /**
   * Check a single rule
   */
  private checkRule(rule: ComplianceRule, context: ValidationContext): ComplianceViolation[] {
    if (rule.check.customValidator) {
      return rule.check.customValidator(context);
    }

    const violations: ComplianceViolation[] = [];
    const codeLower = context.code.toLowerCase();

    // Check required elements
    if (rule.check.requiredElements) {
      const hasRequired = rule.check.requiredElements.some(e => codeLower.includes(e.toLowerCase()));
      if (!hasRequired) {
        violations.push({
          ruleId: rule.id,
          ruleName: rule.name,
          framework: rule.framework,
          severity: rule.severity,
          location: { file: context.filename },
          description: `Missing required element for ${rule.name}`,
          evidence: `Required: one of [${rule.check.requiredElements.join(', ')}]`,
          remediation: rule.remediation,
          falsePositiveRisk: 'medium',
        });
      }
    }

    // Check prohibited elements
    if (rule.check.prohibitedElements) {
      for (const prohibited of rule.check.prohibitedElements) {
        if (codeLower.includes(prohibited.toLowerCase())) {
          violations.push({
            ruleId: rule.id,
            ruleName: rule.name,
            framework: rule.framework,
            severity: rule.severity,
            location: { file: context.filename },
            description: `Prohibited element found: ${prohibited}`,
            evidence: `Found prohibited element: ${prohibited}`,
            remediation: rule.remediation,
            falsePositiveRisk: 'low',
          });
        }
      }
    }

    return violations;
  }

  /**
   * Generate recommendations based on violations
   */
  private generateRecommendations(violations: ComplianceViolation[]): string[] {
    const recommendations: string[] = [];
    const uniqueRules = new Set(violations.map(v => v.ruleId));

    if (violations.filter(v => v.severity === 'critical').length > 0) {
      recommendations.push('Address critical compliance violations immediately before production deployment');
    }

    if (uniqueRules.size > 5) {
      recommendations.push('Consider a comprehensive security review given the number of compliance issues');
    }

    if (violations.some(v => v.framework === 'pci-dss')) {
      recommendations.push('Engage a QSA (Qualified Security Assessor) for PCI-DSS compliance validation');
    }

    if (violations.some(v => v.framework === 'hipaa')) {
      recommendations.push('Conduct a formal HIPAA risk assessment before deployment');
    }

    if (violations.filter(v => v.falsePositiveRisk === 'high').length > 0) {
      recommendations.push('Review flagged violations for false positives with manual inspection');
    }

    return recommendations;
  }

  /**
   * Get rules for a specific framework
   */
  getRules(framework?: ComplianceFramework): ComplianceRule[] {
    const rules = Array.from(this.rules.values());
    return framework ? rules.filter(r => r.framework === framework) : rules;
  }

  /**
   * Get available frameworks
   */
  getFrameworks(): ComplianceFramework[] {
    return Array.from(this.frameworks);
  }
}

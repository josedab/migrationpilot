/**
 * Tests for Core Modules
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { SemanticDiffTool } from '../semantic-diff.js';
import { KnowledgeGraphBuilder } from '../knowledge-graph.js';
import { MigrationCostEstimator } from '../cost-estimator.js';
import { ComplianceRuleEngine } from '../compliance-engine.js';

describe('SemanticDiffTool', () => {
  let diffTool: SemanticDiffTool;

  beforeEach(() => {
    diffTool = new SemanticDiffTool();
  });

  describe('generateDiff', () => {
    it('should generate semantic diff between legacy and modern code', () => {
      const legacyCode = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.
       PROCEDURE DIVISION.
       CALC-INTEREST.
           COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE.
           STOP RUN.
      `;

      const modernCode = `
        public class InterestCalculator {
          public BigDecimal calculateInterest(BigDecimal principal, BigDecimal rate) {
            return principal.multiply(rate);
          }
        }
      `;

      const result = diffTool.generateDiff(
        legacyCode,
        'cobol',
        'CALCINT.cbl',
        modernCode,
        'java',
        'InterestCalculator.java',
        []
      );

      expect(result).toHaveProperty('summary');
      expect(result).toHaveProperty('mappings');
      expect(result.summary).toHaveProperty('totalMappings');
      expect(result.summary).toHaveProperty('coveragePercentage');
    });

    it('should identify risk areas', () => {
      const result = diffTool.generateDiff(
        'LEGACY CODE',
        'cobol',
        'test.cbl',
        'MODERN CODE',
        'java',
        'Test.java',
        []
      );

      expect(result).toHaveProperty('riskAreas');
      expect(Array.isArray(result.riskAreas)).toBe(true);
    });
  });

  describe('export formats', () => {
    it('should export to JSON', () => {
      const result = diffTool.generateDiff(
        'LEGACY',
        'cobol',
        'test.cbl',
        'MODERN',
        'java',
        'Test.java',
        []
      );

      const json = diffTool.exportToJSON(result);
      expect(() => JSON.parse(json)).not.toThrow();
    });

    it('should generate markdown report', () => {
      const result = diffTool.generateDiff(
        'LEGACY',
        'cobol',
        'test.cbl',
        'MODERN',
        'java',
        'Test.java',
        []
      );

      const markdown = diffTool.generateMarkdownReport(result);
      expect(markdown).toContain('# Semantic Diff Report');
      expect(markdown).toContain('## Summary');
    });

    it('should generate side-by-side HTML', () => {
      const result = diffTool.generateDiff(
        'LEGACY',
        'cobol',
        'test.cbl',
        'MODERN',
        'java',
        'Test.java',
        []
      );

      const html = diffTool.generateSideBySideView(result);
      expect(html).toContain('<div class="semantic-diff-container">');
      expect(html).toContain('<style>');
    });
  });
});

describe('KnowledgeGraphBuilder', () => {
  let builder: KnowledgeGraphBuilder;

  beforeEach(() => {
    builder = new KnowledgeGraphBuilder('test-project', 'Test Graph');
  });

  describe('node management', () => {
    it('should add nodes to the graph', () => {
      const node = builder.addNode({
        id: 'node-1',
        type: 'business-rule',
        name: 'Interest Calculation',
        properties: { category: 'calculation' },
      });

      expect(node.id).toBe('node-1');
      expect(node.metadata.createdAt).toBeDefined();
      
      const stats = builder.getStatistics();
      expect(stats.nodeCount).toBe(1);
    });

    it('should index nodes by type', () => {
      builder.addNode({ id: 'rule-1', type: 'business-rule', name: 'Rule 1', properties: {} });
      builder.addNode({ id: 'data-1', type: 'data-structure', name: 'Data 1', properties: {} });
      builder.addNode({ id: 'rule-2', type: 'business-rule', name: 'Rule 2', properties: {} });

      const stats = builder.getStatistics();
      expect(stats.nodesByType['business-rule']).toBe(2);
      expect(stats.nodesByType['data-structure']).toBe(1);
    });
  });

  describe('edge management', () => {
    it('should add edges between nodes', () => {
      builder.addNode({ id: 'node-1', type: 'program', name: 'Program', properties: {} });
      builder.addNode({ id: 'node-2', type: 'procedure', name: 'Procedure', properties: {} });
      
      const edge = builder.addEdge('node-1', 'node-2', 'contains');

      expect(edge.source).toBe('node-1');
      expect(edge.target).toBe('node-2');
      expect(edge.type).toBe('contains');
      
      const stats = builder.getStatistics();
      expect(stats.edgeCount).toBe(1);
    });
  });

  describe('querying', () => {
    beforeEach(() => {
      builder.addNode({ id: 'prog-1', type: 'program', name: 'MAINPROG', properties: {} });
      builder.addNode({ id: 'proc-1', type: 'procedure', name: 'CALC-INT', properties: {} });
      builder.addNode({ id: 'proc-2', type: 'procedure', name: 'DISPLAY', properties: {} });
      builder.addNode({ id: 'data-1', type: 'data-structure', name: 'WS-RATE', properties: {} });
      
      builder.addEdge('prog-1', 'proc-1', 'contains');
      builder.addEdge('prog-1', 'proc-2', 'contains');
      builder.addEdge('proc-1', 'data-1', 'uses');
    });

    it('should query nodes by type', () => {
      const result = builder.query({
        type: 'nodes',
        filters: { nodeTypes: ['procedure'] },
      });

      expect(result.nodes).toHaveLength(2);
      expect(result.nodes.every(n => n.type === 'procedure')).toBe(true);
    });

    it('should find subgraph from source', () => {
      const result = builder.query({
        type: 'subgraph',
        source: 'prog-1',
        maxDepth: 2,
      });

      expect(result.nodes.length).toBeGreaterThan(1);
      expect(result.edges.length).toBeGreaterThan(0);
    });

    it('should analyze impact', () => {
      const result = builder.query({
        type: 'impact',
        source: 'data-1',
        maxDepth: 3,
      });

      expect(result.nodes).toBeDefined();
      // data-1 should be included
      expect(result.nodes.some(n => n.id === 'data-1')).toBe(true);
    });
  });

  describe('export', () => {
    it('should export to JSON', () => {
      builder.addNode({ id: 'n1', type: 'program', name: 'Test', properties: {} });
      
      const json = builder.exportToJSON();
      expect(() => JSON.parse(json)).not.toThrow();
    });

    it('should export to Cytoscape format', () => {
      builder.addNode({ id: 'n1', type: 'program', name: 'Test', properties: {} });
      builder.addNode({ id: 'n2', type: 'procedure', name: 'Proc', properties: {} });
      builder.addEdge('n1', 'n2', 'contains');
      
      const cyto = builder.exportToCytoscape();
      expect(cyto.nodes).toHaveLength(2);
      expect(cyto.edges).toHaveLength(1);
    });

    it('should export to GraphML', () => {
      builder.addNode({ id: 'n1', type: 'program', name: 'Test', properties: {} });
      
      const graphml = builder.exportToGraphML();
      expect(graphml).toContain('<?xml version="1.0"');
      expect(graphml).toContain('<graphml');
      expect(graphml).toContain('<node id="n1"');
    });
  });

  describe('regulation tracking', () => {
    it('should track regulations and affected nodes', () => {
      builder.addNode({ id: 'rule-1', type: 'business-rule', name: 'Interest Rule', properties: {} });
      builder.addNode({ id: 'rule-2', type: 'business-rule', name: 'Balance Rule', properties: {} });
      
      builder.addRegulation(
        'SOX-404',
        'SOX Section 404',
        'Internal controls over financial reporting',
        ['rule-1', 'rule-2']
      );

      const affectedNodes = builder.findByRegulation('SOX-404');
      expect(affectedNodes).toHaveLength(2);
    });
  });

  describe('data flow tracing', () => {
    it('should trace data producers and consumers', () => {
      builder.addNode({ id: 'data-1', type: 'data-structure', name: 'WS-AMOUNT', properties: {} });
      builder.addNode({ id: 'proc-1', type: 'procedure', name: 'CALC', properties: {} });
      builder.addNode({ id: 'proc-2', type: 'procedure', name: 'DISPLAY', properties: {} });
      
      builder.addEdge('proc-1', 'data-1', 'modifies');
      builder.addEdge('data-1', 'proc-2', 'uses');

      const flow = builder.traceDataFlow('data-1');
      
      expect(flow.producers).toHaveLength(1);
      expect(flow.producers[0]?.id).toBe('proc-1');
      expect(flow.consumers).toHaveLength(1);
      expect(flow.consumers[0]?.id).toBe('proc-2');
    });
  });
});

describe('MigrationCostEstimator', () => {
  let estimator: MigrationCostEstimator;

  beforeEach(() => {
    estimator = new MigrationCostEstimator();
  });

  describe('estimate', () => {
    it('should generate cost estimate', () => {
      const estimate = estimator.estimate({
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        totalLines: 50000,
        totalFiles: 100,
        procedures: [
          { name: 'CALC-INT', type: 'paragraph', parameters: [], returnType: undefined, localVariables: [], calledProcedures: [], location: { file: 'test', startLine: 1, endLine: 10 }, complexity: 45 },
        ],
        dataStructures: [],
        businessRules: [],
        externalDependencies: 5,
        databaseTables: 10,
        fileIntegrations: 3,
        apiIntegrations: 2,
      });

      expect(estimate).toHaveProperty('effort');
      expect(estimate).toHaveProperty('cost');
      expect(estimate).toHaveProperty('timeline');
      expect(estimate).toHaveProperty('risk');
      expect(estimate.effort.total).toBeGreaterThan(0);
      expect(estimate.cost.total).toBeGreaterThan(0);
    });

    it('should calculate complexity metrics', () => {
      const estimate = estimator.estimate({
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        totalLines: 10000,
        totalFiles: 20,
        procedures: [
          { name: 'P1', type: 'paragraph', parameters: [], returnType: undefined, localVariables: [], calledProcedures: [], location: { file: 'test', startLine: 1, endLine: 10 }, complexity: 80 },
          { name: 'P2', type: 'paragraph', parameters: [], returnType: undefined, localVariables: [], calledProcedures: [], location: { file: 'test', startLine: 11, endLine: 20 }, complexity: 60 },
        ],
        dataStructures: [],
        businessRules: [],
        externalDependencies: 0,
        databaseTables: 0,
        fileIntegrations: 0,
        apiIntegrations: 0,
      });

      expect(estimate.complexity.overall).toBeGreaterThan(0);
      expect(estimate.complexity.codeComplexity).toBeGreaterThan(0);
    });

    it('should assess risks', () => {
      const estimate = estimator.estimate({
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        totalLines: 100000,
        totalFiles: 200,
        procedures: [],
        dataStructures: [],
        businessRules: [
          { id: '1', projectId: 'p', name: 'Low Confidence Rule', description: 'd', category: 'calculation', sourceFile: 'f', sourceLines: [1, 10], sourceCode: 'c', inputs: [], outputs: [], logic: 'l', edgeCases: [], assumptions: [], confidence: 0.4, reviewStatus: 'pending', extractedAt: new Date(), extractedBy: 'ai', version: 1 },
        ],
        externalDependencies: 10,
        databaseTables: 30,
        fileIntegrations: 5,
        apiIntegrations: 5,
        teamExperience: 'junior',
      });

      expect(estimate.risk.factors.length).toBeGreaterThan(0);
      expect(estimate.risk.mitigations.length).toBeGreaterThan(0);
    });

    it('should generate recommendations', () => {
      const estimate = estimator.estimate({
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        totalLines: 200000, // Large codebase
        totalFiles: 500,
        procedures: [],
        dataStructures: [],
        businessRules: Array(30).fill(null).map((_, i) => ({
          id: String(i),
          projectId: 'p',
          name: `Rule ${i}`,
          description: 'd',
          category: 'calculation' as const,
          sourceFile: 'f',
          sourceLines: [1, 10] as [number, number],
          sourceCode: 'c',
          inputs: [],
          outputs: [],
          logic: 'l',
          edgeCases: [],
          assumptions: [],
          confidence: i < 10 ? 0.5 : 0.9, // 10 low confidence rules
          reviewStatus: 'pending' as const,
          extractedAt: new Date(),
          extractedBy: 'ai',
          version: 1,
        })),
        externalDependencies: 0,
        databaseTables: 0,
        fileIntegrations: 0,
        apiIntegrations: 0,
      });

      expect(estimate.recommendations.length).toBeGreaterThan(0);
      // Should recommend strangler fig for large codebase
      expect(estimate.recommendations.some(r => r.title.includes('Strangler'))).toBe(true);
    });

    it('should include assumptions', () => {
      const estimate = estimator.estimate({
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        totalLines: 10000,
        totalFiles: 20,
        procedures: [],
        dataStructures: [],
        businessRules: [],
        externalDependencies: 0,
        databaseTables: 0,
        fileIntegrations: 0,
        apiIntegrations: 0,
        teamSize: 5,
        teamExperience: 'senior',
      });

      expect(estimate.assumptions.length).toBeGreaterThan(0);
      expect(estimate.assumptions.some(a => a.includes('senior'))).toBe(true);
    });
  });
});

describe('ComplianceRuleEngine', () => {
  let engine: ComplianceRuleEngine;

  beforeEach(() => {
    engine = new ComplianceRuleEngine();
  });

  describe('built-in rules', () => {
    it('should have SOX rules loaded', () => {
      const soxRules = engine.getRules('sox');
      expect(soxRules.length).toBeGreaterThan(0);
    });

    it('should have PCI-DSS rules loaded', () => {
      const pciRules = engine.getRules('pci-dss');
      expect(pciRules.length).toBeGreaterThan(0);
    });

    it('should have HIPAA rules loaded', () => {
      const hipaaRules = engine.getRules('hipaa');
      expect(hipaaRules.length).toBeGreaterThan(0);
    });

    it('should have GDPR rules loaded', () => {
      const gdprRules = engine.getRules('gdpr');
      expect(gdprRules.length).toBeGreaterThan(0);
    });
  });

  describe('validate', () => {
    it('should validate code against compliance rules', () => {
      const context = {
        code: `
          PROCEDURE DIVISION.
          PROCESS-PAYMENT.
              READ CARD-NUMBER.
              WRITE AUDIT-LOG.
              STOP RUN.
        `,
        language: 'cobol',
        filename: 'PAYMENT.cbl',
        dataStructures: [
          { name: 'CARD-NUMBER', type: 'string' },
        ],
        procedures: [
          { name: 'PROCESS-PAYMENT', accessesData: ['CARD-NUMBER'] },
        ],
        businessRules: [],
        externalCalls: [],
        fileOperations: [{ filename: 'AUDIT-LOG', operation: 'write' }],
      };

      const report = engine.validate(context, ['pci-dss']);

      expect(report).toHaveProperty('summary');
      expect(report).toHaveProperty('violations');
      expect(report.summary.totalRulesChecked).toBeGreaterThan(0);
    });

    it('should detect PCI-DSS PAN masking violations', () => {
      const context = {
        code: `
          DISPLAY-CARD.
              DISPLAY CARD-NUMBER.
        `,
        language: 'cobol',
        filename: 'DISPLAY.cbl',
        dataStructures: [
          { name: 'CARD-NUMBER', type: 'string' },
        ],
        procedures: [
          { name: 'DISPLAY-CARD', accessesData: ['CARD-NUMBER'] },
        ],
        businessRules: [],
        externalCalls: [],
        fileOperations: [],
      };

      const report = engine.validate(context, ['pci-dss']);

      // Should flag PAN displayed without masking
      const panViolation = report.violations.find(v => v.ruleId === 'pci-001');
      expect(panViolation).toBeDefined();
    });

    it('should pass when masking is present', () => {
      const context = {
        code: `
          DISPLAY-CARD.
              MOVE '****-****-****-' TO WS-MASKED.
              DISPLAY WS-MASKED.
        `,
        language: 'cobol',
        filename: 'DISPLAY.cbl',
        dataStructures: [
          { name: 'CARD-NUMBER', type: 'string' },
        ],
        procedures: [
          { name: 'DISPLAY-CARD', accessesData: ['CARD-NUMBER'] },
        ],
        businessRules: [],
        externalCalls: [],
        fileOperations: [],
      };

      const report = engine.validate(context, ['pci-dss']);

      // Should not flag when masking is present
      const panViolation = report.violations.find(v => v.ruleId === 'pci-001');
      expect(panViolation).toBeUndefined();
    });

    it('should generate recommendations', () => {
      const context = {
        code: 'TEST CODE',
        language: 'cobol',
        filename: 'test.cbl',
        dataStructures: [
          { name: 'PATIENT-SSN', type: 'string' },
          { name: 'CARD-CVV', type: 'string', sensitive: true },
        ],
        procedures: [],
        businessRules: [],
        externalCalls: [{ name: 'DB2', type: 'database' }],
        fileOperations: [{ filename: 'OUTPUT', operation: 'write' }],
      };

      const report = engine.validate(context);

      if (report.violations.length > 0) {
        expect(report.recommendations.length).toBeGreaterThan(0);
      }
    });
  });

  describe('custom rules', () => {
    it('should allow adding custom rules', () => {
      engine.addRule({
        id: 'custom-001',
        framework: 'custom',
        category: 'Security',
        name: 'Custom Rule',
        description: 'Test custom rule',
        requirement: 'Internal policy',
        severity: 'high',
        automated: true,
        check: {
          type: 'code-pattern',
          prohibitedElements: ['goto'],
        },
        remediation: 'Remove GOTO statements',
        references: [],
      });

      const rules = engine.getRules('custom');
      expect(rules.length).toBe(1);
      expect(rules[0]?.name).toBe('Custom Rule');
    });
  });

  describe('getFrameworks', () => {
    it('should return all available frameworks', () => {
      const frameworks = engine.getFrameworks();
      
      expect(frameworks).toContain('sox');
      expect(frameworks).toContain('pci-dss');
      expect(frameworks).toContain('hipaa');
      expect(frameworks).toContain('gdpr');
    });
  });
});

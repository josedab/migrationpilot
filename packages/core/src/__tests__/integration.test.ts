/**
 * Integration Tests: Trace → Oracle → Visualization Pipeline
 * 
 * Tests the complete flow from mainframe trace capture through
 * behavioral learning to knowledge graph visualization.
 */

import { describe, it, expect } from 'vitest';
import { KnowledgeGraphBuilder } from '../knowledge-graph.js';

// Note: Full pipeline integration tests would require cross-package imports
// which don't work well in vitest without complex setup. These tests focus
// on the KnowledgeGraph aspects of the pipeline.

describe('Trace → Oracle → Visualization Pipeline', () => {
  describe('Knowledge Graph Integration', () => {
    it('should build graph from trace-derived data', async () => {
      // Simulate data that would come from traces and oracle
      const traceData = {
        programs: [
          { id: 'CALCINT', name: 'Interest Calculator', language: 'COBOL' },
          { id: 'ACCUPD', name: 'Account Update', language: 'COBOL' },
        ],
        files: [
          { id: 'RATEFL', name: 'Interest Rate File', type: 'VSAM' },
          { id: 'CUSTFL', name: 'Customer File', type: 'VSAM' },
        ],
        tables: [
          { id: 'ACCOUNTS', name: 'Account Master', columns: ['ID', 'BALANCE'] },
          { id: 'CUSTOMERS', name: 'Customer Master', columns: ['ID', 'NAME'] },
        ],
        rules: [
          { id: 'interest-calc', name: 'Interest Calculation', confidence: 0.92 },
          { id: 'balance-check', name: 'Balance Validation', confidence: 0.85 },
        ],
        relationships: [
          { from: 'CALCINT', to: 'RATEFL', type: 'reads' as const },
          { from: 'CALCINT', to: 'ACCOUNTS', type: 'reads' as const },
          { from: 'CALCINT', to: 'interest-calc', type: 'implements' as const },
          { from: 'ACCUPD', to: 'ACCOUNTS', type: 'modifies' as const },
          { from: 'ACCUPD', to: 'balance-check', type: 'implements' as const },
        ],
      };

      // Build knowledge graph
      const graphBuilder = new KnowledgeGraphBuilder('test-project', 'Interest System');

      // Add program nodes
      for (const prog of traceData.programs) {
        graphBuilder.addNode({
          id: `prog-${prog.id}`,
          type: 'program',
          name: prog.name,
          properties: { language: prog.language },
        });
      }

      // Add file nodes
      for (const file of traceData.files) {
        graphBuilder.addNode({
          id: `file-${file.id}`,
          type: 'file',
          name: file.name,
          properties: { type: file.type },
        });
      }

      // Add table nodes
      for (const table of traceData.tables) {
        graphBuilder.addNode({
          id: `table-${table.id}`,
          type: 'database',
          name: table.name,
          properties: { columns: table.columns },
        });
      }

      // Add rule nodes
      for (const rule of traceData.rules) {
        graphBuilder.addNode({
          id: `rule-${rule.id}`,
          type: 'business-rule',
          name: rule.name,
          metadata: { confidence: rule.confidence },
          properties: {},
        });
      }

      // Add relationships
      for (const rel of traceData.relationships) {
        const fromId = rel.from.includes('-') ? `rule-${rel.from}` :
          traceData.programs.find(p => p.id === rel.from) ? `prog-${rel.from}` :
          traceData.files.find(f => f.id === rel.from) ? `file-${rel.from}` :
          `table-${rel.from}`;
        
        const toId = rel.to.includes('-') ? `rule-${rel.to}` :
          traceData.programs.find(p => p.id === rel.to) ? `prog-${rel.to}` :
          traceData.files.find(f => f.id === rel.to) ? `file-${rel.to}` :
          `table-${rel.to}`;

        graphBuilder.addEdge(fromId, toId, rel.type);
      }

      const graph = graphBuilder.getGraph();

      // Verify graph structure
      expect(graph.nodes.size).toBe(8); // 2 programs + 2 files + 2 tables + 2 rules
      expect(graph.edges.size).toBe(5);

      // Verify node types
      const programs = Array.from(graph.nodes.values()).filter(n => n.type === 'program');
      const rules = Array.from(graph.nodes.values()).filter(n => n.type === 'business-rule');
      
      expect(programs).toHaveLength(2);
      expect(rules).toHaveLength(2);

      // Verify rule confidence metadata
      const interestRule = graph.nodes.get('rule-interest-calc');
      expect(interestRule?.metadata?.confidence).toBe(0.92);
    });

    it('should support complex relationship patterns', () => {
      const graphBuilder = new KnowledgeGraphBuilder('test-project', 'Complex System');

      // Create a more complex graph structure
      graphBuilder.addNode({ id: 'main', type: 'program', name: 'Main Program', properties: {} });
      graphBuilder.addNode({ id: 'sub1', type: 'procedure', name: 'Subroutine 1', properties: {} });
      graphBuilder.addNode({ id: 'sub2', type: 'procedure', name: 'Subroutine 2', properties: {} });
      graphBuilder.addNode({ id: 'rule1', type: 'business-rule', name: 'Rule 1', properties: {} });
      graphBuilder.addNode({ id: 'rule2', type: 'business-rule', name: 'Rule 2', properties: {} });
      graphBuilder.addNode({ id: 'data1', type: 'data-structure', name: 'Data 1', properties: {} });

      // Hierarchical relationships
      graphBuilder.addEdge('main', 'sub1', 'contains');
      graphBuilder.addEdge('main', 'sub2', 'contains');
      
      // Implementation relationships
      graphBuilder.addEdge('sub1', 'rule1', 'implements');
      graphBuilder.addEdge('sub2', 'rule2', 'implements');
      
      // Data relationships
      graphBuilder.addEdge('sub1', 'data1', 'uses');
      graphBuilder.addEdge('sub2', 'data1', 'uses');
      
      // Cross-cutting relationships
      graphBuilder.addEdge('rule1', 'rule2', 'depends-on');

      const graph = graphBuilder.getGraph();

      expect(graph.nodes.size).toBe(6);
      expect(graph.edges.size).toBe(7);

      // Verify we can find all relationships for a node
      const mainEdges = Array.from(graph.edges.values()).filter(
        e => e.source === 'main' || e.target === 'main'
      );
      expect(mainEdges).toHaveLength(2); // contains sub1, contains sub2

      const data1Edges = Array.from(graph.edges.values()).filter(
        e => e.source === 'data1' || e.target === 'data1'
      );
      expect(data1Edges).toHaveLength(2); // used by sub1, used by sub2
    });

    it('should track provenance through metadata', () => {
      const graphBuilder = new KnowledgeGraphBuilder('test-project', 'Provenance Test');

      // Add nodes with trace provenance
      graphBuilder.addNode({
        id: 'rule-discovered',
        type: 'business-rule',
        name: 'Discovered Rule',
        description: 'Rule discovered through trace analysis',
        properties: {
          discoveryMethod: 'trace-inference',
          traceCount: 45,
          firstSeen: new Date().toISOString(),
          validatedBySME: false,
        },
        metadata: {
          confidence: 0.87,
        },
      });

      graphBuilder.addNode({
        id: 'rule-validated',
        type: 'business-rule',
        name: 'Validated Rule',
        description: 'Rule validated by SME',
        properties: {
          discoveryMethod: 'trace-inference',
          traceCount: 120,
          validatedBySME: true,
          validatedBy: 'john.doe@company.com',
          validatedAt: new Date().toISOString(),
        },
        metadata: {
          confidence: 0.95,
        },
      });

      const graph = graphBuilder.getGraph();

      const discoveredRule = graph.nodes.get('rule-discovered');
      const validatedRule = graph.nodes.get('rule-validated');

      expect(discoveredRule?.properties.validatedBySME).toBe(false);
      expect(validatedRule?.properties.validatedBySME).toBe(true);
      expect(validatedRule?.properties.validatedBy).toBe('john.doe@company.com');
    });
  });
});


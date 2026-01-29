/**
 * Tests for Knowledge Graph Visualization Module
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KnowledgeGraphBuilder } from '../knowledge-graph.js';
import { 
  GraphRenderer, 
  GraphAnalyzer, 
  GraphExporter,
  CollaborationManager,
  GraphVisualizationService 
} from '../visualization/index.js';
import type { ActiveFilters } from '../visualization/types.js';

describe('GraphRenderer', () => {
  let renderer: GraphRenderer;
  let graphBuilder: KnowledgeGraphBuilder;

  beforeEach(() => {
    renderer = new GraphRenderer();
    graphBuilder = new KnowledgeGraphBuilder('test-project', 'Test Graph');
    
    // Add sample nodes and edges
    graphBuilder.addNode({ id: 'prog-1', type: 'program', name: 'MAINPROG', properties: {} });
    graphBuilder.addNode({ id: 'proc-1', type: 'procedure', name: 'CALC-INT', properties: {} });
    graphBuilder.addNode({ id: 'proc-2', type: 'procedure', name: 'DISPLAY', properties: {} });
    graphBuilder.addNode({ id: 'rule-1', type: 'business-rule', name: 'Interest Calc', properties: {} });
    graphBuilder.addNode({ id: 'data-1', type: 'data-structure', name: 'WS-RATE', properties: {} });
    
    graphBuilder.addEdge('prog-1', 'proc-1', 'contains');
    graphBuilder.addEdge('prog-1', 'proc-2', 'contains');
    graphBuilder.addEdge('proc-1', 'data-1', 'uses');
    graphBuilder.addEdge('proc-1', 'rule-1', 'implements');
  });

  describe('render', () => {
    it('should render a graph with default settings', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await renderer.render(graph);

      expect(rendered).toHaveProperty('id');
      expect(rendered).toHaveProperty('nodes');
      expect(rendered).toHaveProperty('edges');
      expect(rendered).toHaveProperty('viewState');
      expect(rendered.nodes).toHaveLength(5);
      expect(rendered.edges).toHaveLength(4);
    });

    it('should assign positions to all nodes', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await renderer.render(graph);

      for (const node of rendered.nodes) {
        expect(typeof node.x).toBe('number');
        expect(typeof node.y).toBe('number');
        expect(node.width).toBeGreaterThan(0);
        expect(node.height).toBeGreaterThan(0);
      }
    });

    it('should compute node metrics', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await renderer.render(graph);

      for (const node of rendered.nodes) {
        expect(node.metrics).toHaveProperty('inDegree');
        expect(node.metrics).toHaveProperty('outDegree');
        expect(node.metrics).toHaveProperty('totalDegree');
        expect(node.metrics).toHaveProperty('pageRank');
      }
    });

    it('should compute render statistics', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await renderer.render(graph);

      expect(rendered.renderStats.totalNodes).toBe(5);
      expect(rendered.renderStats.visibleNodes).toBe(5);
      expect(rendered.renderStats.totalEdges).toBe(4);
      expect(rendered.renderStats.visibleEdges).toBe(4);
      expect(rendered.renderStats.renderTimeMs).toBeGreaterThanOrEqual(0);
    });
  });

  describe('filtering', () => {
    it('should filter nodes by type', async () => {
      const graph = graphBuilder.getGraph();
      const filters: ActiveFilters = { nodeTypes: ['procedure'] };
      const rendered = await renderer.render(graph, filters);

      expect(rendered.nodes).toHaveLength(2);
      expect(rendered.nodes.every(n => n.sourceNode.type === 'procedure')).toBe(true);
    });

    it('should filter nodes by search query', async () => {
      const graph = graphBuilder.getGraph();
      const filters: ActiveFilters = { search: 'CALC' };
      const rendered = await renderer.render(graph, filters);

      expect(rendered.nodes.length).toBeLessThan(5);
      expect(rendered.nodes.some(n => n.sourceNode.name.includes('CALC'))).toBe(true);
    });

    it('should filter edges to only show visible node connections', async () => {
      const graph = graphBuilder.getGraph();
      const filters: ActiveFilters = { nodeTypes: ['procedure'] };
      const rendered = await renderer.render(graph, filters);

      // Should only have edges between procedures (none in this case)
      expect(rendered.edges).toHaveLength(0);
    });
  });

  describe('layout algorithms', () => {
    it('should apply hierarchical layout', async () => {
      const hierarchicalRenderer = new GraphRenderer({ layout: 'hierarchical' });
      const graph = graphBuilder.getGraph();
      const rendered = await hierarchicalRenderer.render(graph);

      expect(rendered.nodes).toHaveLength(5);
      // Hierarchical layout should have different Y positions based on level
    });

    it('should apply circular layout', async () => {
      const circularRenderer = new GraphRenderer({ layout: 'circular' });
      const graph = graphBuilder.getGraph();
      const rendered = await circularRenderer.render(graph);

      expect(rendered.nodes).toHaveLength(5);
    });

    it('should apply grid layout', async () => {
      const gridRenderer = new GraphRenderer({ layout: 'grid' });
      const graph = graphBuilder.getGraph();
      const rendered = await gridRenderer.render(graph);

      expect(rendered.nodes).toHaveLength(5);
    });

    it('should update layout on existing rendered graph', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await renderer.render(graph);
      const originalPositions = rendered.nodes.map(n => ({ x: n.x, y: n.y }));

      const updated = await renderer.updateLayout(rendered, 'circular');

      // Positions should be different after layout change
      const newPositions = updated.nodes.map(n => ({ x: n.x, y: n.y }));
      const positionsChanged = originalPositions.some((p, i) => 
        p.x !== newPositions[i]?.x || p.y !== newPositions[i]?.y
      );
      expect(positionsChanged).toBe(true);
    });
  });
});

describe('GraphAnalyzer', () => {
  let analyzer: GraphAnalyzer;
  let graphBuilder: KnowledgeGraphBuilder;

  beforeEach(() => {
    analyzer = new GraphAnalyzer();
    graphBuilder = new KnowledgeGraphBuilder('test-project', 'Test Graph');
    
    // Create a more complex graph for analysis
    graphBuilder.addNode({ id: 'hub', type: 'program', name: 'Main Hub', properties: {} });
    graphBuilder.addNode({ id: 'a', type: 'procedure', name: 'Proc A', properties: {} });
    graphBuilder.addNode({ id: 'b', type: 'procedure', name: 'Proc B', properties: {} });
    graphBuilder.addNode({ id: 'c', type: 'procedure', name: 'Proc C', properties: {} });
    graphBuilder.addNode({ id: 'd', type: 'business-rule', name: 'Rule D', properties: {}, metadata: { confidence: 0.4 } });
    graphBuilder.addNode({ id: 'orphan', type: 'data-structure', name: 'Orphaned', properties: {} });
    
    graphBuilder.addEdge('hub', 'a', 'contains');
    graphBuilder.addEdge('hub', 'b', 'contains');
    graphBuilder.addEdge('hub', 'c', 'contains');
    graphBuilder.addEdge('a', 'b', 'calls');
    graphBuilder.addEdge('b', 'c', 'calls');
    graphBuilder.addEdge('c', 'a', 'calls'); // Circular dependency
    graphBuilder.addEdge('a', 'd', 'implements');
  });

  describe('structural analysis', () => {
    it('should calculate node and edge counts', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.structural.nodeCount).toBe(6);
      expect(analysis.structural.edgeCount).toBe(7);
    });

    it('should calculate graph density', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.structural.density).toBeGreaterThan(0);
      expect(analysis.structural.density).toBeLessThanOrEqual(1);
    });

    it('should calculate average degree', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.structural.averageDegree).toBeGreaterThan(0);
    });

    it('should count connected components', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      // Should have 2 components: main connected graph + orphan
      expect(analysis.structural.connectedComponents).toBe(2);
    });
  });

  describe('cluster analysis', () => {
    it('should detect clusters', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.clusters).toHaveProperty('clusterCount');
      expect(analysis.clusters).toHaveProperty('clusters');
      expect(analysis.clusters).toHaveProperty('modularity');
    });

    it('should calculate modularity', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(typeof analysis.clusters.modularity).toBe('number');
    });
  });

  describe('critical path analysis', () => {
    it('should identify critical nodes', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.criticalPaths.criticalNodes).toBeDefined();
      expect(Array.isArray(analysis.criticalPaths.criticalNodes)).toBe(true);
    });

    it('should identify bottlenecks', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.criticalPaths.bottlenecks).toBeDefined();
      expect(Array.isArray(analysis.criticalPaths.bottlenecks)).toBe(true);
    });
  });

  describe('risk analysis', () => {
    it('should identify high risk nodes', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.risks.highRiskNodes).toBeDefined();
      expect(Array.isArray(analysis.risks.highRiskNodes)).toBe(true);
    });

    it('should find orphaned nodes', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.risks.orphanedNodes).toContain('orphan');
    });

    it('should detect circular dependencies', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      // Should find the a -> b -> c -> a cycle
      expect(analysis.risks.circularDependencies.length).toBeGreaterThan(0);
    });
  });

  describe('recommendations', () => {
    it('should generate recommendations', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      expect(analysis.recommendations).toBeDefined();
      expect(Array.isArray(analysis.recommendations)).toBe(true);
    });

    it('should flag orphaned nodes', () => {
      const graph = graphBuilder.getGraph();
      const analysis = analyzer.analyze(graph);

      const orphanRec = analysis.recommendations.find(r => 
        r.title.toLowerCase().includes('orphan')
      );
      expect(orphanRec).toBeDefined();
    });
  });
});

describe('GraphExporter', () => {
  let exporter: GraphExporter;
  let graphBuilder: KnowledgeGraphBuilder;

  beforeEach(() => {
    exporter = new GraphExporter();
    graphBuilder = new KnowledgeGraphBuilder('test-project', 'Test Graph');
    
    graphBuilder.addNode({ id: 'n1', type: 'program', name: 'Program 1', description: 'Main program', properties: {} });
    graphBuilder.addNode({ id: 'n2', type: 'procedure', name: 'Procedure 1', properties: {} });
    graphBuilder.addEdge('n1', 'n2', 'contains');
  });

  describe('JSON export', () => {
    it('should export to JSON format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'json');

      expect(result.format).toBe('json');
      expect(() => JSON.parse(result.data as string)).not.toThrow();
    });

    it('should include metadata in JSON export', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'json');
      const data = JSON.parse(result.data as string);

      expect(data.metadata).toHaveProperty('id');
      expect(data.metadata).toHaveProperty('nodeCount');
      expect(data.metadata).toHaveProperty('edgeCount');
    });
  });

  describe('GraphML export', () => {
    it('should export to GraphML format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'graphml');

      expect(result.format).toBe('graphml');
      expect(result.data).toContain('<?xml version');
      expect(result.data).toContain('<graphml');
      expect(result.data).toContain('<node id="n1"');
    });
  });

  describe('GEXF export', () => {
    it('should export to GEXF format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'gexf');

      expect(result.format).toBe('gexf');
      expect(result.data).toContain('<gexf');
      expect(result.data).toContain('<nodes>');
      expect(result.data).toContain('<edges>');
    });
  });

  describe('DOT export', () => {
    it('should export to DOT/Graphviz format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'dot');

      expect(result.format).toBe('dot');
      expect(result.data).toContain('digraph');
      expect(result.data).toContain('->');
    });
  });

  describe('CSV export', () => {
    it('should export to CSV format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'csv');

      expect(result.format).toBe('csv');
      expect(result.data).toContain('# NODES');
      expect(result.data).toContain('# EDGES');
      expect(result.data).toContain('id,name,type');
    });
  });

  describe('Cytoscape export', () => {
    it('should export to Cytoscape JSON format', () => {
      const graph = graphBuilder.getGraph();
      const result = exporter.export(graph, 'cytoscape');

      expect(result.format).toBe('cytoscape');
      const data = JSON.parse(result.data as string);
      expect(data).toHaveProperty('nodes');
      expect(data).toHaveProperty('edges');
    });
  });
});

describe('CollaborationManager', () => {
  let collaboration: CollaborationManager;
  let sessionId: string;

  beforeEach(() => {
    collaboration = new CollaborationManager();
    const session = collaboration.createSession('test-project', 'graph-1', 'Test Session', 'user-1', 'Alice');
    sessionId = session.id;
  });

  describe('session management', () => {
    it('should create a session', () => {
      const session = collaboration.getSession(sessionId);

      expect(session).toBeDefined();
      expect(session?.name).toBe('Test Session');
      expect(session?.status).toBe('active');
      expect(session?.participants).toHaveLength(1);
    });

    it('should allow joining a session', () => {
      const session = collaboration.joinSession(sessionId, 'user-2', 'Bob', 'reviewer');

      expect(session.participants).toHaveLength(2);
      expect(session.participants.find(p => p.userId === 'user-2')).toBeDefined();
    });

    it('should track participant status', () => {
      collaboration.joinSession(sessionId, 'user-2', 'Bob');
      collaboration.leaveSession(sessionId, 'user-2');

      const session = collaboration.getSession(sessionId);
      const bob = session?.participants.find(p => p.userId === 'user-2');
      expect(bob?.status).toBe('offline');
    });

    it('should pause and resume sessions', () => {
      collaboration.pauseSession(sessionId);
      expect(collaboration.getSession(sessionId)?.status).toBe('paused');

      collaboration.resumeSession(sessionId);
      expect(collaboration.getSession(sessionId)?.status).toBe('active');
    });

    it('should complete sessions', () => {
      const session = collaboration.completeSession(sessionId);

      expect(session.status).toBe('completed');
      expect(session.completedAt).toBeDefined();
    });
  });

  describe('annotations', () => {
    it('should add annotations to nodes', () => {
      const annotation = collaboration.addAnnotation(
        sessionId,
        'node-1',
        'comment',
        'This rule needs review',
        'user-1'
      );

      expect(annotation).toHaveProperty('id');
      expect(annotation.content).toBe('This rule needs review');
      expect(annotation.type).toBe('comment');
      expect(annotation.resolved).toBe(false);
    });

    it('should resolve annotations', () => {
      const annotation = collaboration.addAnnotation(
        sessionId, 'node-1', 'question', 'Is this correct?', 'user-1'
      );
      
      collaboration.resolveAnnotation(sessionId, annotation.id, 'user-2');

      const session = collaboration.getSession(sessionId);
      const resolved = session?.annotations.find(a => a.id === annotation.id);
      expect(resolved?.resolved).toBe(true);
    });
  });

  describe('decisions', () => {
    it('should make decisions on nodes', () => {
      const decision = collaboration.makeDecision(
        sessionId,
        'rule-1',
        'approved',
        'user-1',
        'Verified against requirements'
      );

      expect(decision.decision).toBe('approved');
      expect(decision.rationale).toBe('Verified against requirements');
    });

    it('should update existing decisions', () => {
      collaboration.makeDecision(sessionId, 'rule-1', 'needs_clarification', 'user-1');
      const updated = collaboration.makeDecision(sessionId, 'rule-1', 'approved', 'user-2');

      expect(updated.decision).toBe('approved');
      // The reviewedBy includes the latest reviewer
      expect(updated.reviewedBy).toContain('user-2');
    });

    it('should track decision statistics', () => {
      collaboration.makeDecision(sessionId, 'rule-1', 'approved', 'user-1');
      collaboration.makeDecision(sessionId, 'rule-2', 'rejected', 'user-1');
      collaboration.makeDecision(sessionId, 'rule-3', 'approved', 'user-1');

      const stats = collaboration.getDecisionStats(sessionId);

      expect(stats.total).toBe(3);
      expect(stats.approved).toBe(2);
      expect(stats.rejected).toBe(1);
    });
  });

  describe('comments', () => {
    it('should add comments', () => {
      const comment = collaboration.addComment(
        sessionId,
        'This looks good',
        'user-1',
        'node-1'
      );

      expect(comment.content).toBe('This looks good');
      expect(comment.nodeId).toBe('node-1');
    });

    it('should reply to comments', () => {
      const comment = collaboration.addComment(sessionId, 'Question?', 'user-1');
      const reply = collaboration.replyToComment(sessionId, comment.id, 'Answer!', 'user-2');

      expect(reply.content).toBe('Answer!');
      
      const session = collaboration.getSession(sessionId);
      const parentComment = session?.comments.find(c => c.id === comment.id);
      expect(parentComment?.replies).toHaveLength(1);
    });

    it('should resolve comments', () => {
      const comment = collaboration.addComment(sessionId, 'Issue', 'user-1');
      collaboration.resolveComment(sessionId, comment.id, 'user-2');

      const session = collaboration.getSession(sessionId);
      const resolved = session?.comments.find(c => c.id === comment.id);
      expect(resolved?.resolved).toBe(true);
    });
  });

  describe('session summary', () => {
    it('should generate session summary', () => {
      collaboration.addAnnotation(sessionId, 'n1', 'comment', 'Test', 'user-1');
      collaboration.makeDecision(sessionId, 'r1', 'approved', 'user-1');
      collaboration.addComment(sessionId, 'Note', 'user-1');

      const summary = collaboration.getSessionSummary(sessionId);

      expect(summary.id).toBe(sessionId);
      expect(summary.annotationCount).toBe(1);
      expect(summary.decisions.approved).toBe(1);
      expect(summary.commentCount).toBe(1);
    });
  });

  describe('activity timeline', () => {
    it('should track activities', () => {
      collaboration.addAnnotation(sessionId, 'n1', 'flag', 'Flag', 'user-1');
      collaboration.makeDecision(sessionId, 'r1', 'approved', 'user-1');

      const timeline = collaboration.getActivityTimeline(sessionId);

      // Should have: joined + annotation + decision
      expect(timeline.length).toBeGreaterThanOrEqual(3);
    });
  });
});

describe('GraphVisualizationService', () => {
  let service: GraphVisualizationService;
  let graphBuilder: KnowledgeGraphBuilder;

  beforeEach(() => {
    service = new GraphVisualizationService();
    graphBuilder = new KnowledgeGraphBuilder('test-project', 'Test Graph');
    
    graphBuilder.addNode({ id: 'n1', type: 'program', name: 'Main', properties: {} });
    graphBuilder.addNode({ id: 'n2', type: 'procedure', name: 'Proc', properties: {} });
    graphBuilder.addEdge('n1', 'n2', 'contains');
  });

  describe('rendering', () => {
    it('should render and store current graph', async () => {
      const graph = graphBuilder.getGraph();
      const rendered = await service.render(graph);

      expect(rendered).toBeDefined();
      expect(service.getRenderedGraph()).toBe(rendered);
    });
  });

  describe('interaction', () => {
    it('should select nodes', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      service.selectNodes(['n1']);

      const rendered = service.getRenderedGraph();
      expect(rendered?.selectionState.selectedNodeIds.has('n1')).toBe(true);
    });

    it('should search nodes', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      const results = service.search('Main');

      expect(results).toHaveLength(1);
      expect(results[0]?.name).toBe('Main');
    });
  });

  describe('navigation', () => {
    it('should zoom', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      service.zoomTo(2);

      const rendered = service.getRenderedGraph();
      expect(rendered?.viewState.zoom).toBe(2);
    });

    it('should pan', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      service.panTo(100, 200);

      const rendered = service.getRenderedGraph();
      expect(rendered?.viewState.pan).toEqual({ x: 100, y: 200 });
    });
  });

  describe('export', () => {
    it('should export graph', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      const exported = await service.exportGraph('json');

      expect(exported.format).toBe('json');
      expect(exported.data).toBeDefined();
    });
  });

  describe('analysis', () => {
    it('should analyze graph', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      const analysis = await service.analyzeGraph();

      expect(analysis).toHaveProperty('structural');
      expect(analysis).toHaveProperty('clusters');
      expect(analysis).toHaveProperty('criticalPaths');
      expect(analysis).toHaveProperty('risks');
    });
  });

  describe('collaboration', () => {
    it('should start collaboration session', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      const session = await service.startCollaborationSession('Review Session');

      expect(session).toBeDefined();
      expect(session.name).toBe('Review Session');
      expect(service.getCurrentSession()).toBe(session);
    });

    it('should add annotations via service', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);
      await service.startCollaborationSession('Test');

      const annotation = service.addAnnotation('n1', {
        type: 'comment',
        content: 'Test comment',
        author: 'user-1',
        createdAt: new Date(),
        resolved: false,
      });

      expect(annotation).toHaveProperty('id');
      expect(annotation.content).toBe('Test comment');
    });
  });

  describe('events', () => {
    it('should emit and receive events', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      let eventReceived = false;
      service.on('selection-change', () => {
        eventReceived = true;
      });

      service.selectNodes(['n1']);

      expect(eventReceived).toBe(true);
    });

    it('should unsubscribe from events', async () => {
      const graph = graphBuilder.getGraph();
      await service.render(graph);

      let callCount = 0;
      const handler = () => { callCount++; };

      service.on('selection-change', handler);
      service.selectNodes(['n1']);
      expect(callCount).toBe(1);

      service.off('selection-change', handler);
      service.selectNodes(['n2']);
      expect(callCount).toBe(1); // Should not increment
    });
  });
});

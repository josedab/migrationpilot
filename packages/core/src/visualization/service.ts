/**
 * Graph Visualization Service
 * 
 * Main service that orchestrates graph rendering, analysis,
 * collaboration, and export capabilities.
 */

import type { KnowledgeGraph, KnowledgeNode } from '../knowledge-graph.js';
import type {
  IGraphVisualizationService,
  VisualizationConfig,
  RenderedGraph,
  LayoutAlgorithm,
  ActiveFilters,
  GraphExport,
  ExportFormat,
  ExportOptions,
  ShareableView,
  CollaborationSession,
  NodeAnnotation,
  RuleDecision,
  GraphAnalysis,
  GraphInteractionEvent,
  GraphEventType,
  LayoutOptions
} from './types.js';
import { GraphRenderer } from './renderer.js';
import { GraphAnalyzer } from './analyzer.js';
import { GraphExporter } from './exporter.js';
import { CollaborationManager } from './collaboration.js';

/**
 * Main visualization service that provides a unified API
 * for all graph visualization capabilities.
 */
export class GraphVisualizationService implements IGraphVisualizationService {
  private renderer: GraphRenderer;
  private analyzer: GraphAnalyzer;
  private exporter: GraphExporter;
  private collaboration: CollaborationManager;
  
  private currentGraph?: KnowledgeGraph;
  private renderedGraph?: RenderedGraph;
  private currentSession?: CollaborationSession;
  
  private eventHandlers: Map<GraphEventType, Set<(event: GraphInteractionEvent) => void>> = new Map();
  
  constructor(config?: Partial<VisualizationConfig>) {
    this.renderer = new GraphRenderer(config);
    this.analyzer = new GraphAnalyzer();
    this.exporter = new GraphExporter();
    this.collaboration = new CollaborationManager();
  }
  
  // ==========================================================================
  // RENDERING
  // ==========================================================================
  
  /**
   * Render a knowledge graph with the configured visualization settings
   */
  async render(graph: KnowledgeGraph, config?: VisualizationConfig): Promise<RenderedGraph> {
    if (config) {
      this.renderer = new GraphRenderer(config);
    }
    
    this.currentGraph = graph;
    this.renderedGraph = await this.renderer.render(graph);
    
    this.emitEvent('layout-complete', {
      nodeCount: this.renderedGraph.nodes.length,
      edgeCount: this.renderedGraph.edges.length,
    });
    
    return this.renderedGraph;
  }
  
  /**
   * Update the layout algorithm of the current graph
   */
  async updateLayout(
    renderedGraph: RenderedGraph, 
    layout: LayoutAlgorithm,
    options?: Partial<LayoutOptions>
  ): Promise<RenderedGraph> {
    this.renderedGraph = await this.renderer.updateLayout(renderedGraph, layout, options);
    
    this.emitEvent('layout-complete', {
      layout,
      nodeCount: this.renderedGraph.nodes.length,
    });
    
    return this.renderedGraph;
  }
  
  // ==========================================================================
  // INTERACTION
  // ==========================================================================
  
  /**
   * Select specific nodes
   */
  selectNodes(nodeIds: string[]): void {
    if (!this.renderedGraph) return;
    
    // Clear previous selection
    this.renderedGraph.selectionState.selectedNodeIds.clear();
    this.renderedGraph.selectionState.selectedEdgeIds.clear();
    
    // Add new selection
    nodeIds.forEach(id => {
      this.renderedGraph!.selectionState.selectedNodeIds.add(id);
      
      // Update node state
      const node = this.renderedGraph!.nodes.find(n => n.id === id);
      if (node) node.selected = true;
    });
    
    // Deselect others
    this.renderedGraph.nodes
      .filter(n => !nodeIds.includes(n.id))
      .forEach(n => n.selected = false);
    
    this.emitEvent('selection-change', { selectedNodeIds: nodeIds });
  }
  
  /**
   * Focus on a specific node (center view)
   */
  focusNode(nodeId: string): void {
    if (!this.renderedGraph) return;
    
    const node = this.renderedGraph.nodes.find(n => n.id === nodeId);
    if (!node) return;
    
    this.renderedGraph.selectionState.focusedNodeId = nodeId;
    this.renderedGraph.viewState.center = { x: node.x, y: node.y };
    
    this.emitEvent('node-click', {
      target: { type: 'node', id: nodeId },
      position: { x: node.x, y: node.y },
    });
  }
  
  /**
   * Expand a node to show its neighbors
   */
  async expandNode(nodeId: string, depth = 1): Promise<void> {
    if (!this.renderedGraph || !this.currentGraph) return;
    
    const node = this.renderedGraph.nodes.find(n => n.id === nodeId);
    if (!node) return;
    
    node.expanded = true;
    
    this.emitEvent('expand-node', { nodeId, depth });
  }
  
  /**
   * Collapse a node
   */
  collapseNode(nodeId: string): void {
    if (!this.renderedGraph) return;
    
    const node = this.renderedGraph.nodes.find(n => n.id === nodeId);
    if (!node) return;
    
    node.expanded = false;
    
    this.emitEvent('collapse-node', { nodeId });
  }
  
  // ==========================================================================
  // FILTERING
  // ==========================================================================
  
  /**
   * Apply filters to the current graph
   */
  async applyFilters(filters: ActiveFilters): Promise<RenderedGraph> {
    if (!this.currentGraph) {
      throw new Error('No graph loaded');
    }
    
    this.renderedGraph = await this.renderer.render(this.currentGraph, filters);
    
    this.emitEvent('filter-change', { filters });
    
    return this.renderedGraph;
  }
  
  /**
   * Search for nodes matching a query
   */
  search(query: string): KnowledgeNode[] {
    if (!this.currentGraph) return [];
    
    const queryLower = query.toLowerCase();
    const nodes = Array.from(this.currentGraph.nodes.values());
    
    const results = nodes.filter(node => {
      const nameMatch = node.name.toLowerCase().includes(queryLower);
      const descMatch = node.description?.toLowerCase().includes(queryLower);
      const typeMatch = node.type.toLowerCase().includes(queryLower);
      return nameMatch || descMatch || typeMatch;
    });
    
    this.emitEvent('search', { query, resultCount: results.length });
    
    return results;
  }
  
  // ==========================================================================
  // NAVIGATION
  // ==========================================================================
  
  /**
   * Zoom to a specific level
   */
  zoomTo(level: number): void {
    if (!this.renderedGraph) return;
    
    this.renderedGraph.viewState.zoom = Math.max(0.1, Math.min(5, level));
    
    this.emitEvent('zoom-change', { zoom: this.renderedGraph.viewState.zoom });
  }
  
  /**
   * Pan to a specific position
   */
  panTo(x: number, y: number): void {
    if (!this.renderedGraph) return;
    
    this.renderedGraph.viewState.pan = { x, y };
    
    this.emitEvent('pan-change', { pan: { x, y } });
  }
  
  /**
   * Fit the entire graph in the view
   */
  fitToView(): void {
    if (!this.renderedGraph) return;
    
    const { bounds } = this.renderedGraph.viewState;
    const width = bounds.maxX - bounds.minX;
    const height = bounds.maxY - bounds.minY;
    
    // Calculate zoom to fit
    const viewWidth = 1000; // Assumed view width
    const viewHeight = 800; // Assumed view height
    const zoomX = viewWidth / (width + 100);
    const zoomY = viewHeight / (height + 100);
    
    this.renderedGraph.viewState.zoom = Math.min(zoomX, zoomY, 1);
    this.renderedGraph.viewState.center = {
      x: (bounds.minX + bounds.maxX) / 2,
      y: (bounds.minY + bounds.maxY) / 2,
    };
    this.renderedGraph.viewState.pan = { x: 0, y: 0 };
    
    this.emitEvent('zoom-change', { zoom: this.renderedGraph.viewState.zoom });
  }
  
  // ==========================================================================
  // EXPORT
  // ==========================================================================
  
  /**
   * Export the graph to various formats
   */
  async exportGraph(
    format: ExportFormat, 
    options?: ExportOptions
  ): Promise<GraphExport> {
    if (!this.currentGraph) {
      throw new Error('No graph loaded');
    }
    
    const analysis = this.analyzer.analyze(this.currentGraph);
    
    return this.exporter.export(
      this.currentGraph,
      format,
      options,
      this.renderedGraph,
      analysis
    );
  }
  
  /**
   * Create a shareable view configuration
   */
  async createShareableView(
    name: string, 
    filters?: ActiveFilters
  ): Promise<ShareableView> {
    if (!this.renderedGraph) {
      throw new Error('No graph rendered');
    }
    
    return this.collaboration.createShareableView(
      name,
      undefined,
      filters || {},
      'force',
      {},
      this.renderedGraph.viewState,
      'system',
      'link',
      30
    );
  }
  
  // ==========================================================================
  // COLLABORATION
  // ==========================================================================
  
  /**
   * Start a new collaboration session
   */
  async startCollaborationSession(name: string): Promise<CollaborationSession> {
    if (!this.currentGraph) {
      throw new Error('No graph loaded');
    }
    
    this.currentSession = this.collaboration.createSession(
      this.currentGraph.projectId,
      this.currentGraph.id,
      name,
      'system',
      'System'
    );
    
    return this.currentSession;
  }
  
  /**
   * Join an existing collaboration session
   */
  async joinCollaborationSession(sessionId: string): Promise<CollaborationSession> {
    const session = this.collaboration.joinSession(
      sessionId,
      'user',
      'User',
      'reviewer'
    );
    
    this.currentSession = session;
    return session;
  }
  
  /**
   * Add an annotation to a node
   */
  addAnnotation(
    nodeId: string, 
    annotation: Omit<NodeAnnotation, 'id'>
  ): NodeAnnotation {
    if (!this.currentSession) {
      throw new Error('No active collaboration session');
    }
    
    return this.collaboration.addAnnotation(
      this.currentSession.id,
      nodeId,
      annotation.type,
      annotation.content,
      annotation.author
    );
  }
  
  /**
   * Make a decision on a node
   */
  makeDecision(
    nodeId: string, 
    decision: Omit<RuleDecision, 'id'>
  ): RuleDecision {
    if (!this.currentSession) {
      throw new Error('No active collaboration session');
    }
    
    return this.collaboration.makeDecision(
      this.currentSession.id,
      nodeId,
      decision.decision,
      decision.decidedBy,
      decision.rationale
    );
  }
  
  // ==========================================================================
  // ANALYSIS
  // ==========================================================================
  
  /**
   * Perform comprehensive analysis of the graph
   */
  async analyzeGraph(): Promise<GraphAnalysis> {
    if (!this.currentGraph) {
      throw new Error('No graph loaded');
    }
    
    return this.analyzer.analyze(this.currentGraph);
  }
  
  // ==========================================================================
  // EVENTS
  // ==========================================================================
  
  /**
   * Subscribe to graph events
   */
  on(event: GraphEventType, handler: (event: GraphInteractionEvent) => void): void {
    if (!this.eventHandlers.has(event)) {
      this.eventHandlers.set(event, new Set());
    }
    this.eventHandlers.get(event)!.add(handler);
  }
  
  /**
   * Unsubscribe from graph events
   */
  off(event: GraphEventType, handler: (event: GraphInteractionEvent) => void): void {
    this.eventHandlers.get(event)?.delete(handler);
  }
  
  private emitEvent(type: GraphEventType, data: Record<string, unknown>): void {
    const handlers = this.eventHandlers.get(type);
    if (!handlers) return;
    
    const event: GraphInteractionEvent = {
      type,
      timestamp: new Date(),
      data,
    };
    
    handlers.forEach(handler => handler(event));
  }
  
  // ==========================================================================
  // GETTERS
  // ==========================================================================
  
  /**
   * Get the current rendered graph
   */
  getRenderedGraph(): RenderedGraph | undefined {
    return this.renderedGraph;
  }
  
  /**
   * Get the current collaboration session
   */
  getCurrentSession(): CollaborationSession | undefined {
    return this.currentSession;
  }
  
  /**
   * Get the collaboration manager for advanced operations
   */
  getCollaborationManager(): CollaborationManager {
    return this.collaboration;
  }
}

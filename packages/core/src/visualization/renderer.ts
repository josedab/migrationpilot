/**
 * Graph Renderer
 * 
 * Renders knowledge graphs for visualization with layout algorithms,
 * styling, and interaction support.
 */

import type { 
  KnowledgeNode, 
  KnowledgeEdge, 
  KnowledgeGraph,
  NodeType,
  EdgeType 
} from '../knowledge-graph.js';
import type {
  VisualizationConfig,
  RenderedGraph,
  RenderedNode,
  RenderedEdge,
  RenderedCluster,
  ViewState,
  ActiveFilters,
  NodeMetrics,
  ComputedNodeStyle,
  ComputedEdgeStyle,
  LayoutAlgorithm,
  LayoutOptions,
  NodeShape
} from './types.js';
import { generateId } from '../index.js';

/**
 * Default visualization theme
 */
const DEFAULT_THEME = {
  name: 'default',
  colors: {
    background: '#ffffff',
    text: '#1a1a1a',
    selection: '#3b82f6',
    highlight: '#f59e0b',
    nodeColors: {
      program: '#3b82f6',
      procedure: '#8b5cf6',
      'business-rule': '#ef4444',
      'data-structure': '#f59e0b',
      file: '#10b981',
      database: '#06b6d4',
      'external-system': '#6366f1',
      regulation: '#ec4899',
      concept: '#84cc16',
    } as Record<NodeType, string>,
    edgeColors: {
      calls: '#64748b',
      uses: '#94a3b8',
      modifies: '#f97316',
      reads: '#22c55e',
      writes: '#a855f7',
      'depends-on': '#cbd5e1',
      implements: '#22c55e',
      'affected-by': '#f43f5e',
      contains: '#0ea5e9',
      'related-to': '#6b7280',
    } as Record<EdgeType, string>,
  },
  fonts: {
    nodeLabel: '12px Inter, sans-serif',
    edgeLabel: '10px Inter, sans-serif',
    tooltip: '14px Inter, sans-serif',
  },
};

/**
 * Default visualization configuration
 */
const DEFAULT_CONFIG: VisualizationConfig = {
  layout: 'force',
  layoutOptions: {
    nodeRepulsion: 100,
    edgeLength: 150,
    gravity: 0.1,
    animate: true,
    animationDuration: 500,
  },
  theme: DEFAULT_THEME,
  nodeStyles: {
    shapes: {
      program: 'rectangle',
      procedure: 'ellipse',
      'business-rule': 'hexagon',
      'data-structure': 'diamond',
      file: 'rectangle',
      database: 'rectangle',
      'external-system': 'hexagon',
      regulation: 'hexagon',
      concept: 'ellipse',
    } as Record<NodeType, NodeShape>,
    sizes: {
      default: 40,
      min: 20,
      max: 80,
      scaleBy: 'connections',
    },
    labels: {
      show: true,
      position: 'bottom',
      maxLength: 20,
      fontSize: 12,
    },
    badges: {
      showConfidence: true,
      showStatus: true,
      showCount: false,
    },
  },
  edgeStyles: {
    curves: {
      type: 'bezier',
    },
    width: {
      default: 2,
      min: 1,
      max: 5,
      scaleBy: 'confidence',
    },
    arrows: {
      show: true,
      size: 8,
      position: 'end',
    },
    labels: {
      show: false,
      fontSize: 10,
    },
  },
  interaction: {
    selectable: true,
    multiSelect: true,
    zoomable: true,
    pannable: true,
    minZoom: 0.1,
    maxZoom: 5,
    expandOnClick: false,
    collapseOnDoubleClick: false,
    showTooltipOnHover: true,
    highlightNeighborsOnHover: true,
    draggable: true,
    contextMenu: true,
  },
  performance: {
    maxVisibleNodes: 1000,
    maxVisibleEdges: 5000,
    virtualScrolling: true,
    simplifyAtZoom: 0.3,
    hideLabelsAtZoom: 0.5,
    hideEdgesAtZoom: 0.2,
    useWebGL: false,
    useCaching: true,
  },
};

/**
 * Renders knowledge graphs for visualization
 */
export class GraphRenderer {
  private config: VisualizationConfig;
  private nodePositions: Map<string, { x: number; y: number }> = new Map();
  
  constructor(config: Partial<VisualizationConfig> = {}) {
    this.config = this.mergeConfig(DEFAULT_CONFIG, config);
  }
  
  /**
   * Render a knowledge graph with the configured layout and styling
   */
  async render(graph: KnowledgeGraph, filters?: ActiveFilters): Promise<RenderedGraph> {
    const startTime = Date.now();
    
    // Get nodes and edges from graph (Map to array)
    const allNodes = Array.from(graph.nodes.values());
    const allEdges = Array.from(graph.edges.values());
    
    // Apply filters
    const filteredNodes = this.filterNodes(allNodes, filters);
    const filteredEdges = this.filterEdges(allEdges, filteredNodes, filters);
    
    // Calculate layout
    const layoutStartTime = Date.now();
    await this.calculateLayout(filteredNodes, filteredEdges);
    const layoutTimeMs = Date.now() - layoutStartTime;
    
    // Compute node metrics
    const nodeMetricsMap = this.computeNodeMetrics(filteredNodes, filteredEdges);
    
    // Render nodes
    const renderedNodes = filteredNodes.map(node => 
      this.renderNode(node, nodeMetricsMap.get(node.id)!)
    );
    
    // Render edges
    const renderedEdges = filteredEdges.map(edge => 
      this.renderEdge(edge)
    );
    
    // Detect clusters if enabled
    const clusters = this.config.layoutOptions.clusterByType 
      ? this.detectClusters(renderedNodes)
      : [];
    
    // Calculate view bounds
    const viewState = this.calculateViewState(renderedNodes);
    
    const renderTimeMs = Date.now() - startTime;
    
    return {
      id: generateId(),
      sourceGraphId: graph.id,
      projectId: graph.projectId,
      nodes: renderedNodes,
      edges: renderedEdges,
      clusters,
      viewState,
      selectionState: {
        selectedNodeIds: new Set(),
        selectedEdgeIds: new Set(),
      },
      activeFilters: filters || {},
      renderStats: {
        totalNodes: allNodes.length,
        visibleNodes: renderedNodes.length,
        totalEdges: allEdges.length,
        visibleEdges: renderedEdges.length,
        renderTimeMs,
        layoutTimeMs,
      },
      generatedAt: new Date(),
    };
  }
  
  /**
   * Update the layout of a rendered graph
   */
  async updateLayout(
    renderedGraph: RenderedGraph, 
    layout: LayoutAlgorithm,
    options?: Partial<LayoutOptions>
  ): Promise<RenderedGraph> {
    this.config.layout = layout;
    if (options) {
      this.config.layoutOptions = { ...this.config.layoutOptions, ...options };
    }
    
    // Extract nodes and edges
    const nodes = renderedGraph.nodes.map(n => n.sourceNode);
    const edges = renderedGraph.edges.map(e => e.sourceEdge);
    
    // Recalculate layout
    await this.calculateLayout(nodes, edges);
    
    // Update positions
    const updatedNodes = renderedGraph.nodes.map(node => ({
      ...node,
      x: this.nodePositions.get(node.id)?.x || node.x,
      y: this.nodePositions.get(node.id)?.y || node.y,
    }));
    
    // Update edges
    const updatedEdges = renderedGraph.edges.map(edge => {
      const sourcePos = this.nodePositions.get(edge.sourceEdge.source);
      const targetPos = this.nodePositions.get(edge.sourceEdge.target);
      return {
        ...edge,
        sourceX: sourcePos?.x || edge.sourceX,
        sourceY: sourcePos?.y || edge.sourceY,
        targetX: targetPos?.x || edge.targetX,
        targetY: targetPos?.y || edge.targetY,
        controlPoints: this.calculateControlPoints(
          sourcePos?.x || edge.sourceX,
          sourcePos?.y || edge.sourceY,
          targetPos?.x || edge.targetX,
          targetPos?.y || edge.targetY
        ),
      };
    });
    
    return {
      ...renderedGraph,
      nodes: updatedNodes,
      edges: updatedEdges,
      viewState: this.calculateViewState(updatedNodes),
    };
  }
  
  // ==========================================================================
  // FILTERING
  // ==========================================================================
  
  private filterNodes(nodes: KnowledgeNode[], filters?: ActiveFilters): KnowledgeNode[] {
    if (!filters) return nodes;
    
    return nodes.filter(node => {
      // Filter by node types
      if (filters.nodeTypes?.length && !filters.nodeTypes.includes(node.type)) {
        return false;
      }
      
      // Filter by tags
      if (filters.tags?.length) {
        const nodeTags = node.metadata?.tags || [];
        if (!filters.tags.some(tag => nodeTags.includes(tag))) {
          return false;
        }
      }
      
      // Filter by search
      if (filters.search) {
        const searchLower = filters.search.toLowerCase();
        const nameMatch = node.name.toLowerCase().includes(searchLower);
        const descMatch = node.description?.toLowerCase().includes(searchLower);
        if (!nameMatch && !descMatch) {
          return false;
        }
      }
      
      // Filter by confidence range
      if (filters.confidenceRange) {
        const confidence = node.metadata?.confidence || 1;
        if (confidence < filters.confidenceRange.min || confidence > filters.confidenceRange.max) {
          return false;
        }
      }
      
      return true;
    });
  }
  
  private filterEdges(
    edges: KnowledgeEdge[], 
    visibleNodes: KnowledgeNode[],
    filters?: ActiveFilters
  ): KnowledgeEdge[] {
    const visibleNodeIds = new Set(visibleNodes.map(n => n.id));
    
    return edges.filter(edge => {
      // Only show edges between visible nodes
      if (!visibleNodeIds.has(edge.source) || !visibleNodeIds.has(edge.target)) {
        return false;
      }
      
      // Filter by edge types
      if (filters?.edgeTypes?.length && !filters.edgeTypes.includes(edge.type)) {
        return false;
      }
      
      return true;
    });
  }
  
  // ==========================================================================
  // LAYOUT ALGORITHMS
  // ==========================================================================
  
  private async calculateLayout(nodes: KnowledgeNode[], edges: KnowledgeEdge[]): Promise<void> {
    switch (this.config.layout) {
      case 'force':
        this.forceDirectedLayout(nodes, edges);
        break;
      case 'hierarchical':
        this.hierarchicalLayout(nodes, edges);
        break;
      case 'radial':
        this.radialLayout(nodes, edges);
        break;
      case 'circular':
        this.circularLayout(nodes);
        break;
      case 'grid':
        this.gridLayout(nodes);
        break;
      default:
        this.forceDirectedLayout(nodes, edges);
    }
  }
  
  private forceDirectedLayout(nodes: KnowledgeNode[], edges: KnowledgeEdge[]): void {
    const options = this.config.layoutOptions;
    const nodeCount = nodes.length;
    const iterations = Math.min(300, nodeCount * 2);
    
    // Initialize random positions
    const positions = new Map<string, { x: number; y: number; vx: number; vy: number }>();
    nodes.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / nodeCount;
      const radius = Math.sqrt(nodeCount) * 50;
      positions.set(node.id, {
        x: Math.cos(angle) * radius + Math.random() * 10,
        y: Math.sin(angle) * radius + Math.random() * 10,
        vx: 0,
        vy: 0,
      });
    });
    
    // Build adjacency map
    const adjacency = new Map<string, Set<string>>();
    edges.forEach(edge => {
      if (!adjacency.has(edge.source)) adjacency.set(edge.source, new Set());
      if (!adjacency.has(edge.target)) adjacency.set(edge.target, new Set());
      adjacency.get(edge.source)!.add(edge.target);
      adjacency.get(edge.target)!.add(edge.source);
    });
    
    // Simulation
    const repulsion = options.nodeRepulsion || 100;
    const edgeLength = options.edgeLength || 150;
    const gravity = options.gravity || 0.1;
    
    for (let iter = 0; iter < iterations; iter++) {
      const alpha = 1 - iter / iterations;
      
      // Apply repulsion between all pairs
      for (const [id1, pos1] of positions) {
        for (const [id2, pos2] of positions) {
          if (id1 >= id2) continue;
          
          const dx = pos2.x - pos1.x;
          const dy = pos2.y - pos1.y;
          const distance = Math.sqrt(dx * dx + dy * dy) || 1;
          
          const force = (repulsion * alpha) / (distance * distance);
          const fx = (dx / distance) * force;
          const fy = (dy / distance) * force;
          
          pos1.vx -= fx;
          pos1.vy -= fy;
          pos2.vx += fx;
          pos2.vy += fy;
        }
      }
      
      // Apply edge attraction
      edges.forEach(edge => {
        const source = positions.get(edge.source);
        const target = positions.get(edge.target);
        if (!source || !target) return;
        
        const dx = target.x - source.x;
        const dy = target.y - source.y;
        const distance = Math.sqrt(dx * dx + dy * dy) || 1;
        
        const force = ((distance - edgeLength) / distance) * alpha * 0.5;
        const fx = dx * force;
        const fy = dy * force;
        
        source.vx += fx;
        source.vy += fy;
        target.vx -= fx;
        target.vy -= fy;
      });
      
      // Apply gravity toward center
      for (const pos of positions.values()) {
        pos.vx -= pos.x * gravity * alpha;
        pos.vy -= pos.y * gravity * alpha;
      }
      
      // Update positions
      for (const pos of positions.values()) {
        pos.x += pos.vx;
        pos.y += pos.vy;
        pos.vx *= 0.6;  // Damping
        pos.vy *= 0.6;
      }
    }
    
    // Store final positions
    for (const [id, pos] of positions) {
      this.nodePositions.set(id, { x: pos.x, y: pos.y });
    }
  }
  
  private hierarchicalLayout(nodes: KnowledgeNode[], edges: KnowledgeEdge[]): void {
    const options = this.config.layoutOptions;
    const direction = options.direction || 'TB';
    const levelSep = options.levelSeparation || 100;
    const nodeSep = options.nodeSeparation || 50;
    
    // Build parent->children map and find roots
    const children = new Map<string, string[]>();
    const parents = new Map<string, string[]>();
    
    edges.forEach(edge => {
      if (!children.has(edge.source)) children.set(edge.source, []);
      if (!parents.has(edge.target)) parents.set(edge.target, []);
      children.get(edge.source)!.push(edge.target);
      parents.get(edge.target)!.push(edge.source);
    });
    
    // Find root nodes (no parents)
    const roots = nodes.filter(n => !parents.has(n.id) || parents.get(n.id)!.length === 0);
    
    // Assign levels via BFS
    const levels = new Map<string, number>();
    const queue: { id: string; level: number }[] = roots.map(r => ({ id: r.id, level: 0 }));
    
    while (queue.length > 0) {
      const { id, level } = queue.shift()!;
      if (levels.has(id)) continue;
      levels.set(id, level);
      
      const childIds = children.get(id) || [];
      childIds.forEach(childId => {
        if (!levels.has(childId)) {
          queue.push({ id: childId, level: level + 1 });
        }
      });
    }
    
    // Handle disconnected nodes
    nodes.forEach(n => {
      if (!levels.has(n.id)) levels.set(n.id, 0);
    });
    
    // Group by level
    const levelGroups = new Map<number, string[]>();
    for (const [id, level] of levels) {
      if (!levelGroups.has(level)) levelGroups.set(level, []);
      levelGroups.get(level)!.push(id);
    }
    
    // Position nodes
    const isVertical = direction === 'TB' || direction === 'BT';
    const reverse = direction === 'BT' || direction === 'RL';
    
    for (const [level, ids] of levelGroups) {
      const levelOffset = reverse ? -level * levelSep : level * levelSep;
      ids.forEach((id, i) => {
        const nodeOffset = (i - (ids.length - 1) / 2) * nodeSep;
        
        if (isVertical) {
          this.nodePositions.set(id, { x: nodeOffset, y: levelOffset });
        } else {
          this.nodePositions.set(id, { x: levelOffset, y: nodeOffset });
        }
      });
    }
  }
  
  private radialLayout(nodes: KnowledgeNode[], edges: KnowledgeEdge[]): void {
    // Find central node (highest degree)
    const degrees = new Map<string, number>();
    nodes.forEach(n => degrees.set(n.id, 0));
    edges.forEach(e => {
      degrees.set(e.source, (degrees.get(e.source) || 0) + 1);
      degrees.set(e.target, (degrees.get(e.target) || 0) + 1);
    });
    
    let centralNodeId: string = nodes[0]?.id ?? '';
    let maxDegree = 0;
    for (const [id, degree] of degrees) {
      if (degree > maxDegree) {
        maxDegree = degree;
        centralNodeId = id;
      }
    }
    
    if (!centralNodeId) return;
    
    // BFS from central node
    const levels = new Map<string, number>();
    const queue: string[] = [centralNodeId];
    levels.set(centralNodeId, 0);
    
    // Build adjacency
    const adjacency = new Map<string, Set<string>>();
    edges.forEach(e => {
      if (!adjacency.has(e.source)) adjacency.set(e.source, new Set());
      if (!adjacency.has(e.target)) adjacency.set(e.target, new Set());
      adjacency.get(e.source)!.add(e.target);
      adjacency.get(e.target)!.add(e.source);
    });
    
    while (queue.length > 0) {
      const id = queue.shift()!;
      const level = levels.get(id)!;
      const neighbors = adjacency.get(id) || new Set();
      
      for (const neighbor of neighbors) {
        if (!levels.has(neighbor)) {
          levels.set(neighbor, level + 1);
          queue.push(neighbor);
        }
      }
    }
    
    // Handle disconnected nodes
    nodes.forEach(n => {
      if (!levels.has(n.id)) levels.set(n.id, 1);
    });
    
    // Group by level
    const levelGroups = new Map<number, string[]>();
    for (const [id, level] of levels) {
      if (!levelGroups.has(level)) levelGroups.set(level, []);
      levelGroups.get(level)!.push(id);
    }
    
    // Position in concentric circles
    const radiusStep = 100;
    for (const [level, ids] of levelGroups) {
      const radius = level * radiusStep;
      ids.forEach((id, i) => {
        const angle = (2 * Math.PI * i) / ids.length;
        this.nodePositions.set(id, {
          x: Math.cos(angle) * radius,
          y: Math.sin(angle) * radius,
        });
      });
    }
  }
  
  private circularLayout(nodes: KnowledgeNode[]): void {
    const radius = Math.max(100, nodes.length * 10);
    nodes.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / nodes.length;
      this.nodePositions.set(node.id, {
        x: Math.cos(angle) * radius,
        y: Math.sin(angle) * radius,
      });
    });
  }
  
  private gridLayout(nodes: KnowledgeNode[]): void {
    const cols = Math.ceil(Math.sqrt(nodes.length));
    const spacing = 80;
    
    nodes.forEach((node, i) => {
      const row = Math.floor(i / cols);
      const col = i % cols;
      this.nodePositions.set(node.id, {
        x: col * spacing,
        y: row * spacing,
      });
    });
  }
  
  // ==========================================================================
  // NODE RENDERING
  // ==========================================================================
  
  private renderNode(node: KnowledgeNode, metrics: NodeMetrics): RenderedNode {
    const position = this.nodePositions.get(node.id) || { x: 0, y: 0 };
    const size = this.computeNodeSize(node, metrics);
    const style = this.computeNodeStyle(node);
    
    return {
      id: node.id,
      sourceNode: node,
      x: position.x,
      y: position.y,
      width: size,
      height: size,
      style,
      visible: true,
      selected: false,
      highlighted: false,
      expanded: true,
      pinned: false,
      annotations: [],
      metrics,
    };
  }
  
  private computeNodeSize(node: KnowledgeNode, metrics: NodeMetrics): number {
    const config = this.config.nodeStyles.sizes;
    
    if (config.scaleBy === 'fixed') {
      return config.default;
    }
    
    let scale = 1;
    switch (config.scaleBy) {
      case 'connections':
        scale = Math.log2(metrics.totalDegree + 1) + 1;
        break;
      case 'importance':
        scale = metrics.pageRank * 10 + 1;
        break;
      case 'confidence':
        scale = (node.metadata?.confidence || 0.5) + 0.5;
        break;
    }
    
    const size = config.default * scale;
    return Math.min(config.max, Math.max(config.min, size));
  }
  
  private computeNodeStyle(node: KnowledgeNode): ComputedNodeStyle {
    const theme = this.config.theme;
    const nodeColors = theme.colors.nodeColors;
    const shape = this.config.nodeStyles.shapes[node.type] || 'circle';
    const color = nodeColors[node.type] || '#6b7280';
    
    return {
      shape,
      backgroundColor: color,
      borderColor: this.darkenColor(color, 0.2),
      borderWidth: 2,
      labelColor: theme.colors.text,
      fontSize: this.config.nodeStyles.labels.fontSize,
      opacity: 1,
    };
  }
  
  // ==========================================================================
  // EDGE RENDERING
  // ==========================================================================
  
  private renderEdge(edge: KnowledgeEdge): RenderedEdge {
    const sourcePos = this.nodePositions.get(edge.source) || { x: 0, y: 0 };
    const targetPos = this.nodePositions.get(edge.target) || { x: 0, y: 0 };
    const style = this.computeEdgeStyle(edge);
    
    return {
      id: edge.id,
      sourceEdge: edge,
      sourceX: sourcePos.x,
      sourceY: sourcePos.y,
      targetX: targetPos.x,
      targetY: targetPos.y,
      controlPoints: this.calculateControlPoints(
        sourcePos.x, sourcePos.y, targetPos.x, targetPos.y
      ),
      style,
      visible: true,
      selected: false,
      highlighted: false,
    };
  }
  
  private computeEdgeStyle(edge: KnowledgeEdge): ComputedEdgeStyle {
    const theme = this.config.theme;
    const config = this.config.edgeStyles;
    const color = theme.colors.edgeColors[edge.type] || '#94a3b8';
    
    let width = config.width.default;
    if (config.width.scaleBy === 'confidence' && edge.confidence) {
      width = config.width.min + 
        (config.width.max - config.width.min) * edge.confidence;
    } else if (config.width.scaleBy === 'weight' && edge.weight) {
      width = config.width.min + 
        (config.width.max - config.width.min) * Math.min(1, edge.weight / 10);
    }
    
    return {
      color,
      width,
      opacity: edge.confidence || 1,
      arrowSize: config.arrows.size,
    };
  }
  
  private calculateControlPoints(
    x1: number, y1: number, 
    x2: number, y2: number
  ): { x: number; y: number }[] {
    const curveType = this.config.edgeStyles.curves.type;
    
    if (curveType === 'straight') {
      return [];
    }
    
    if (curveType === 'bezier') {
      const midX = (x1 + x2) / 2;
      const midY = (y1 + y2) / 2;
      const dx = x2 - x1;
      const dy = y2 - y1;
      
      return [
        { x: midX - dy * 0.1, y: midY + dx * 0.1 },
      ];
    }
    
    return [];
  }
  
  // ==========================================================================
  // METRICS & CLUSTERING
  // ==========================================================================
  
  private computeNodeMetrics(
    nodes: KnowledgeNode[], 
    edges: KnowledgeEdge[]
  ): Map<string, NodeMetrics> {
    const metrics = new Map<string, NodeMetrics>();
    
    // Initialize
    nodes.forEach(node => {
      metrics.set(node.id, {
        inDegree: 0,
        outDegree: 0,
        totalDegree: 0,
        pageRank: 1 / nodes.length,
        betweennessCentrality: 0,
        clusteringCoefficient: 0,
      });
    });
    
    // Calculate degrees
    edges.forEach(edge => {
      const sourceMetrics = metrics.get(edge.source);
      const targetMetrics = metrics.get(edge.target);
      if (sourceMetrics) sourceMetrics.outDegree++;
      if (targetMetrics) targetMetrics.inDegree++;
    });
    
    for (const m of metrics.values()) {
      m.totalDegree = m.inDegree + m.outDegree;
    }
    
    // Simple PageRank (few iterations)
    const damping = 0.85;
    const iterations = 20;
    
    for (let i = 0; i < iterations; i++) {
      const newRanks = new Map<string, number>();
      
      for (const node of nodes) {
        let rank = (1 - damping) / nodes.length;
        
        // Sum contributions from incoming edges
        edges.forEach(edge => {
          if (edge.target === node.id) {
            const sourceMetrics = metrics.get(edge.source)!;
            rank += damping * (sourceMetrics.pageRank / Math.max(1, sourceMetrics.outDegree));
          }
        });
        
        newRanks.set(node.id, rank);
      }
      
      // Update ranks
      for (const [id, rank] of newRanks) {
        metrics.get(id)!.pageRank = rank;
      }
    }
    
    return metrics;
  }
  
  private detectClusters(nodes: RenderedNode[]): RenderedCluster[] {
    // Group by type
    const typeGroups = new Map<NodeType, RenderedNode[]>();
    
    nodes.forEach(node => {
      const type = node.sourceNode.type;
      if (!typeGroups.has(type)) typeGroups.set(type, []);
      typeGroups.get(type)!.push(node);
    });
    
    const clusters: RenderedCluster[] = [];
    
    for (const [type, groupNodes] of typeGroups) {
      if (groupNodes.length < 2) continue;
      
      // Calculate bounding box
      const xs = groupNodes.map(n => n.x);
      const ys = groupNodes.map(n => n.y);
      const minX = Math.min(...xs) - 20;
      const maxX = Math.max(...xs) + 20;
      const minY = Math.min(...ys) - 20;
      const maxY = Math.max(...ys) + 20;
      
      clusters.push({
        id: generateId(),
        name: type,
        nodeIds: groupNodes.map(n => n.id),
        x: minX,
        y: minY,
        width: maxX - minX,
        height: maxY - minY,
        backgroundColor: `${this.config.theme.colors.nodeColors[type]}20`,
        borderColor: this.config.theme.colors.nodeColors[type],
        collapsed: false,
      });
    }
    
    return clusters;
  }
  
  private calculateViewState(nodes: RenderedNode[]): ViewState {
    if (nodes.length === 0) {
      return {
        zoom: 1,
        pan: { x: 0, y: 0 },
        center: { x: 0, y: 0 },
        bounds: { minX: -100, minY: -100, maxX: 100, maxY: 100 },
      };
    }
    
    const xs = nodes.map(n => n.x);
    const ys = nodes.map(n => n.y);
    
    const minX = Math.min(...xs);
    const maxX = Math.max(...xs);
    const minY = Math.min(...ys);
    const maxY = Math.max(...ys);
    
    return {
      zoom: 1,
      pan: { x: 0, y: 0 },
      center: { x: (minX + maxX) / 2, y: (minY + maxY) / 2 },
      bounds: { minX, minY, maxX, maxY },
    };
  }
  
  // ==========================================================================
  // UTILITIES
  // ==========================================================================
  
  private mergeConfig(
    base: VisualizationConfig, 
    override: Partial<VisualizationConfig>
  ): VisualizationConfig {
    return {
      ...base,
      ...override,
      layoutOptions: { ...base.layoutOptions, ...override.layoutOptions },
      theme: { ...base.theme, ...override.theme },
      nodeStyles: { ...base.nodeStyles, ...override.nodeStyles },
      edgeStyles: { ...base.edgeStyles, ...override.edgeStyles },
      interaction: { ...base.interaction, ...override.interaction },
      performance: { ...base.performance, ...override.performance },
    };
  }
  
  private darkenColor(hex: string, amount: number): string {
    // Simple darkening by reducing RGB values
    const num = parseInt(hex.replace('#', ''), 16);
    const r = Math.max(0, ((num >> 16) & 255) * (1 - amount));
    const g = Math.max(0, ((num >> 8) & 255) * (1 - amount));
    const b = Math.max(0, (num & 255) * (1 - amount));
    return `#${Math.round(r).toString(16).padStart(2, '0')}${Math.round(g).toString(16).padStart(2, '0')}${Math.round(b).toString(16).padStart(2, '0')}`;
  }
}

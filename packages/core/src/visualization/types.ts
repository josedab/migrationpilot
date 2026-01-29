/**
 * Knowledge Graph Visualization Types
 * 
 * Types for interactive graph visualization, collaboration features,
 * and graph-based analysis.
 */

import type { 
  KnowledgeNode, 
  KnowledgeEdge, 
  KnowledgeGraph,
  NodeType, 
  EdgeType 
} from '../knowledge-graph.js';

// ============================================================================
// VISUALIZATION CONFIGURATION
// ============================================================================

export interface VisualizationConfig {
  // Layout
  layout: LayoutAlgorithm;
  layoutOptions: LayoutOptions;
  
  // Appearance
  theme: VisualizationTheme;
  nodeStyles: NodeStyleConfig;
  edgeStyles: EdgeStyleConfig;
  
  // Interaction
  interaction: InteractionConfig;
  
  // Performance
  performance: PerformanceConfig;
}

export type LayoutAlgorithm = 
  | 'force'           // Force-directed layout
  | 'hierarchical'    // Tree-like layout
  | 'radial'          // Radial layout from center
  | 'circular'        // Nodes arranged in a circle
  | 'grid'            // Grid layout
  | 'dagre'           // Directed acyclic graph
  | 'cola'            // Constraint-based layout
  | 'cose';           // Compound Spring Embedder

export interface LayoutOptions {
  // Force layout
  nodeRepulsion?: number;
  edgeLength?: number;
  gravity?: number;
  
  // Hierarchical
  direction?: 'TB' | 'BT' | 'LR' | 'RL';
  levelSeparation?: number;
  nodeSeparation?: number;
  
  // Animation
  animate?: boolean;
  animationDuration?: number;
  
  // Clustering
  clusterByType?: boolean;
  clusterBy?: string;
}

export interface VisualizationTheme {
  name: string;
  colors: {
    background: string;
    text: string;
    selection: string;
    highlight: string;
    nodeColors: Record<NodeType, string>;
    edgeColors: Record<EdgeType, string>;
  };
  fonts: {
    nodeLabel: string;
    edgeLabel: string;
    tooltip: string;
  };
}

export interface NodeStyleConfig {
  shapes: Record<NodeType, NodeShape>;
  sizes: {
    default: number;
    min: number;
    max: number;
    scaleBy?: 'connections' | 'importance' | 'confidence' | 'fixed';
  };
  labels: {
    show: boolean;
    position: 'center' | 'bottom' | 'top' | 'right';
    maxLength: number;
    fontSize: number;
  };
  badges: {
    showConfidence: boolean;
    showStatus: boolean;
    showCount: boolean;
  };
}

export type NodeShape = 'circle' | 'rectangle' | 'diamond' | 'hexagon' | 'triangle' | 'ellipse';

export interface EdgeStyleConfig {
  curves: {
    type: 'straight' | 'bezier' | 'taxi' | 'segments';
  };
  width: {
    default: number;
    scaleBy?: 'weight' | 'confidence' | 'fixed';
    min: number;
    max: number;
  };
  arrows: {
    show: boolean;
    size: number;
    position: 'end' | 'middle' | 'start';
  };
  labels: {
    show: boolean;
    fontSize: number;
  };
}

export interface InteractionConfig {
  // Selection
  selectable: boolean;
  multiSelect: boolean;
  
  // Navigation
  zoomable: boolean;
  pannable: boolean;
  minZoom: number;
  maxZoom: number;
  
  // Expansion
  expandOnClick: boolean;
  collapseOnDoubleClick: boolean;
  
  // Hover
  showTooltipOnHover: boolean;
  highlightNeighborsOnHover: boolean;
  
  // Dragging
  draggable: boolean;
  
  // Context menu
  contextMenu: boolean;
}

export interface PerformanceConfig {
  // Large graph handling
  maxVisibleNodes: number;
  maxVisibleEdges: number;
  virtualScrolling: boolean;
  
  // Level of detail
  simplifyAtZoom: number;
  hideLabelsAtZoom: number;
  hideEdgesAtZoom: number;
  
  // Rendering
  useWebGL: boolean;
  useCaching: boolean;
}

// ============================================================================
// RENDERED GRAPH TYPES
// ============================================================================

export interface RenderedGraph {
  id: string;
  sourceGraphId: string;
  projectId: string;
  
  // Visible elements
  nodes: RenderedNode[];
  edges: RenderedEdge[];
  clusters: RenderedCluster[];
  
  // View state
  viewState: ViewState;
  
  // Selection state
  selectionState: SelectionState;
  
  // Filters applied
  activeFilters: ActiveFilters;
  
  // Statistics
  renderStats: RenderStats;
  
  generatedAt: Date;
}

export interface RenderedNode {
  id: string;
  sourceNode: KnowledgeNode;
  
  // Position
  x: number;
  y: number;
  
  // Size
  width: number;
  height: number;
  
  // Style (computed)
  style: ComputedNodeStyle;
  
  // State
  visible: boolean;
  selected: boolean;
  highlighted: boolean;
  expanded: boolean;
  pinned: boolean;
  
  // Annotations
  annotations: NodeAnnotation[];
  
  // Computed metrics
  metrics: NodeMetrics;
}

export interface ComputedNodeStyle {
  shape: NodeShape;
  backgroundColor: string;
  borderColor: string;
  borderWidth: number;
  labelColor: string;
  fontSize: number;
  opacity: number;
}

export interface NodeAnnotation {
  id: string;
  type: 'comment' | 'flag' | 'warning' | 'approval' | 'rejection' | 'question';
  content: string;
  author: string;
  createdAt: Date;
  resolved: boolean;
}

export interface NodeMetrics {
  inDegree: number;
  outDegree: number;
  totalDegree: number;
  pageRank: number;
  betweennessCentrality: number;
  clusteringCoefficient: number;
}

export interface RenderedEdge {
  id: string;
  sourceEdge: KnowledgeEdge;
  
  // Positions
  sourceX: number;
  sourceY: number;
  targetX: number;
  targetY: number;
  controlPoints?: { x: number; y: number }[];
  
  // Style (computed)
  style: ComputedEdgeStyle;
  
  // State
  visible: boolean;
  selected: boolean;
  highlighted: boolean;
}

export interface ComputedEdgeStyle {
  color: string;
  width: number;
  opacity: number;
  dashArray?: string;
  arrowSize: number;
}

export interface RenderedCluster {
  id: string;
  name: string;
  nodeIds: string[];
  
  // Bounding box
  x: number;
  y: number;
  width: number;
  height: number;
  
  // Style
  backgroundColor: string;
  borderColor: string;
  
  // State
  collapsed: boolean;
}

export interface ViewState {
  zoom: number;
  pan: { x: number; y: number };
  center: { x: number; y: number };
  bounds: {
    minX: number;
    minY: number;
    maxX: number;
    maxY: number;
  };
}

export interface SelectionState {
  selectedNodeIds: Set<string>;
  selectedEdgeIds: Set<string>;
  focusedNodeId?: string;
  hoveredNodeId?: string;
  hoveredEdgeId?: string;
}

export interface ActiveFilters {
  nodeTypes?: NodeType[];
  edgeTypes?: EdgeType[];
  tags?: string[];
  search?: string;
  confidenceRange?: { min: number; max: number };
  depthFromFocus?: number;
  showOnlyConnected?: boolean;
  hideApproved?: boolean;
  showOnlyPending?: boolean;
}

export interface RenderStats {
  totalNodes: number;
  visibleNodes: number;
  totalEdges: number;
  visibleEdges: number;
  renderTimeMs: number;
  layoutTimeMs: number;
}

// ============================================================================
// INTERACTION EVENTS
// ============================================================================

export interface GraphInteractionEvent {
  type: GraphEventType;
  timestamp: Date;
  target?: InteractionTarget;
  position?: { x: number; y: number };
  data?: Record<string, unknown>;
}

export type GraphEventType = 
  | 'node-click'
  | 'node-double-click'
  | 'node-right-click'
  | 'node-hover-start'
  | 'node-hover-end'
  | 'node-drag-start'
  | 'node-drag'
  | 'node-drag-end'
  | 'edge-click'
  | 'edge-hover-start'
  | 'edge-hover-end'
  | 'selection-change'
  | 'zoom-change'
  | 'pan-change'
  | 'layout-complete'
  | 'expand-node'
  | 'collapse-node'
  | 'filter-change'
  | 'search';

export interface InteractionTarget {
  type: 'node' | 'edge' | 'cluster' | 'background';
  id?: string;
}

// ============================================================================
// COLLABORATION TYPES
// ============================================================================

export interface CollaborationSession {
  id: string;
  projectId: string;
  graphId: string;
  name: string;
  
  // Participants
  participants: CollaborationParticipant[];
  
  // State
  status: 'active' | 'paused' | 'completed';
  
  // Annotations
  annotations: NodeAnnotation[];
  
  // Decisions
  decisions: RuleDecision[];
  
  // Chat/comments
  comments: CollaborationComment[];
  
  // Timeline
  activities: CollaborationActivity[];
  
  startedAt: Date;
  lastActivityAt: Date;
  completedAt?: Date;
}

export interface CollaborationParticipant {
  userId: string;
  name: string;
  role: 'admin' | 'reviewer' | 'viewer';
  status: 'online' | 'away' | 'offline';
  cursor?: { x: number; y: number };
  selectedNodeIds?: string[];
  joinedAt: Date;
  lastSeenAt: Date;
}

export interface RuleDecision {
  id: string;
  nodeId: string;
  decision: 'approved' | 'rejected' | 'needs_clarification' | 'deferred';
  rationale?: string;
  decidedBy: string;
  decidedAt: Date;
  reviewedBy?: string[];
}

export interface CollaborationComment {
  id: string;
  nodeId?: string;
  edgeId?: string;
  content: string;
  author: string;
  createdAt: Date;
  editedAt?: Date;
  replies: CollaborationComment[];
  resolved: boolean;
}

export interface CollaborationActivity {
  id: string;
  type: ActivityType;
  actor: string;
  timestamp: Date;
  details: Record<string, unknown>;
}

export type ActivityType = 
  | 'joined'
  | 'left'
  | 'selected-node'
  | 'added-annotation'
  | 'resolved-annotation'
  | 'made-decision'
  | 'changed-decision'
  | 'added-comment'
  | 'replied-comment'
  | 'resolved-comment'
  | 'exported-graph'
  | 'changed-filter'
  | 'changed-layout';

// ============================================================================
// EXPORT/SHARE TYPES
// ============================================================================

export interface GraphExport {
  format: ExportFormat;
  options: ExportOptions;
  data?: string | Blob;
  url?: string;
  generatedAt: Date;
}

export type ExportFormat = 
  | 'json'
  | 'graphml'
  | 'gexf'
  | 'cytoscape'
  | 'dot'
  | 'svg'
  | 'png'
  | 'pdf'
  | 'csv';

export interface ExportOptions {
  includePositions: boolean;
  includeAnnotations: boolean;
  includeDecisions: boolean;
  includeMetrics: boolean;
  filterApplied: boolean;
  resolution?: number;  // For image exports
  pageSize?: string;    // For PDF exports
}

export interface ShareableView {
  id: string;
  name: string;
  description?: string;
  
  // View configuration
  filters: ActiveFilters;
  layout: LayoutAlgorithm;
  layoutOptions: LayoutOptions;
  viewState: ViewState;
  
  // Access
  accessType: 'public' | 'link' | 'authenticated';
  accessLink?: string;
  expiresAt?: Date;
  
  // Creator
  createdBy: string;
  createdAt: Date;
}

// ============================================================================
// ANALYSIS TYPES
// ============================================================================

export interface GraphAnalysis {
  graphId: string;
  analyzedAt: Date;
  
  // Structural metrics
  structural: StructuralMetrics;
  
  // Cluster analysis
  clusters: ClusterAnalysis;
  
  // Critical path analysis
  criticalPaths: CriticalPathAnalysis;
  
  // Risk analysis
  risks: RiskAnalysis;
  
  // Recommendations
  recommendations: AnalysisRecommendation[];
}

export interface StructuralMetrics {
  nodeCount: number;
  edgeCount: number;
  density: number;
  averageDegree: number;
  maxDegree: number;
  diameter: number;
  averagePathLength: number;
  clusteringCoefficient: number;
  connectedComponents: number;
}

export interface ClusterAnalysis {
  algorithm: string;
  clusterCount: number;
  clusters: {
    id: string;
    nodeIds: string[];
    size: number;
    dominantType: NodeType;
    cohesion: number;
    couplingToOthers: number;
  }[];
  modularity: number;
}

export interface CriticalPathAnalysis {
  criticalNodes: {
    nodeId: string;
    criticality: number;
    reason: string;
    dependentCount: number;
  }[];
  bottlenecks: {
    nodeId: string;
    throughputImpact: number;
    alternatives: string[];
  }[];
}

export interface RiskAnalysis {
  highRiskNodes: {
    nodeId: string;
    riskScore: number;
    riskFactors: string[];
    mitigations: string[];
  }[];
  singlePointsOfFailure: string[];
  orphanedNodes: string[];
  circularDependencies: string[][];
}

export interface AnalysisRecommendation {
  type: 'structure' | 'quality' | 'coverage' | 'risk';
  severity: 'high' | 'medium' | 'low';
  title: string;
  description: string;
  affectedNodes: string[];
  suggestedAction: string;
}

// ============================================================================
// VISUALIZATION SERVICE INTERFACE
// ============================================================================

export interface IGraphVisualizationService {
  // Rendering
  render(graph: KnowledgeGraph, config: VisualizationConfig): Promise<RenderedGraph>;
  updateLayout(renderedGraph: RenderedGraph, layout: LayoutAlgorithm): Promise<RenderedGraph>;
  
  // Interaction
  selectNodes(nodeIds: string[]): void;
  focusNode(nodeId: string): void;
  expandNode(nodeId: string, depth?: number): Promise<void>;
  collapseNode(nodeId: string): void;
  
  // Filtering
  applyFilters(filters: ActiveFilters): Promise<RenderedGraph>;
  search(query: string): KnowledgeNode[];
  
  // Navigation
  zoomTo(level: number): void;
  panTo(x: number, y: number): void;
  fitToView(): void;
  
  // Export
  exportGraph(format: ExportFormat, options?: ExportOptions): Promise<GraphExport>;
  createShareableView(name: string, filters?: ActiveFilters): Promise<ShareableView>;
  
  // Collaboration
  startCollaborationSession(name: string): Promise<CollaborationSession>;
  joinCollaborationSession(sessionId: string): Promise<CollaborationSession>;
  addAnnotation(nodeId: string, annotation: Omit<NodeAnnotation, 'id'>): NodeAnnotation;
  makeDecision(nodeId: string, decision: Omit<RuleDecision, 'id'>): RuleDecision;
  
  // Analysis
  analyzeGraph(): Promise<GraphAnalysis>;
  
  // Events
  on(event: GraphEventType, handler: (event: GraphInteractionEvent) => void): void;
  off(event: GraphEventType, handler: (event: GraphInteractionEvent) => void): void;
}

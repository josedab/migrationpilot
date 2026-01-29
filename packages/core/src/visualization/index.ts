/**
 * Knowledge Graph Visualization Module
 * 
 * Provides interactive visualization, analysis, collaboration,
 * and export capabilities for knowledge graphs.
 */

// Types
export type {
  // Visualization Configuration
  VisualizationConfig,
  LayoutAlgorithm,
  LayoutOptions,
  VisualizationTheme,
  NodeStyleConfig,
  EdgeStyleConfig,
  InteractionConfig,
  PerformanceConfig,
  NodeShape,
  
  // Rendered Graph
  RenderedGraph,
  RenderedNode,
  RenderedEdge,
  RenderedCluster,
  ViewState,
  SelectionState,
  ActiveFilters,
  RenderStats,
  NodeMetrics,
  ComputedNodeStyle,
  ComputedEdgeStyle,
  NodeAnnotation,
  
  // Interaction Events
  GraphInteractionEvent,
  GraphEventType,
  InteractionTarget,
  
  // Collaboration
  CollaborationSession,
  CollaborationParticipant,
  RuleDecision,
  CollaborationComment,
  CollaborationActivity,
  ActivityType,
  
  // Export & Share
  GraphExport,
  ExportFormat,
  ExportOptions,
  ShareableView,
  
  // Analysis
  GraphAnalysis,
  StructuralMetrics,
  ClusterAnalysis,
  CriticalPathAnalysis,
  RiskAnalysis,
  AnalysisRecommendation,
  
  // Service Interface
  IGraphVisualizationService,
} from './types.js';

// Classes
export { GraphRenderer } from './renderer.js';
export { GraphAnalyzer } from './analyzer.js';
export { GraphExporter } from './exporter.js';
export { CollaborationManager, type DecisionStats, type SessionSummary, type CollaborationEvent, type CollaborationEventHandler } from './collaboration.js';
export { GraphVisualizationService } from './service.js';

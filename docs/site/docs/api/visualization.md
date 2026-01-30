---
sidebar_position: 7
---

# Knowledge Graph Visualization API

The Knowledge Graph Visualization module provides tools for rendering, analyzing, and collaborating on knowledge graphs that represent legacy system structure and business rules.

## Overview

The visualization module includes:
- **GraphRenderer**: Renders knowledge graphs with multiple layout algorithms
- **GraphAnalyzer**: Analyzes graph structure, clusters, and risks
- **CollaborationManager**: Enables SME annotations and review workflows
- **GraphExporter**: Exports graphs to various formats
- **GraphVisualizationService**: Unified API combining all capabilities

## GraphRenderer

Renders knowledge graphs with various layout options.

### Constructor

```typescript
import { GraphRenderer } from '@migrationpilot/core';

const renderer = new GraphRenderer({
  layout: 'force',           // 'force' | 'hierarchical' | 'radial' | 'circular' | 'grid'
  width: 1200,
  height: 800,
  nodeSize: { min: 30, max: 80 },
  edgeWidth: { min: 1, max: 5 },
  showLabels: true,
  showEdgeLabels: false,
  animateTransitions: true,
});
```

### Methods

#### `render(graph, filters?)`

Renders a knowledge graph with optional filters.

```typescript
const graph = graphBuilder.getGraph();

const rendered = await renderer.render(graph, {
  nodeTypes: ['program', 'business-rule'],  // Filter by node types
  confidence: { min: 0.7 },                  // Filter by confidence
  search: 'CALC',                            // Search filter
});

// Result includes positioned nodes and edges
console.log(rendered.nodes);  // Array of positioned nodes
console.log(rendered.edges);  // Array of rendered edges
console.log(rendered.renderStats);  // Rendering statistics
```

#### `updateLayout(renderedGraph, newLayout)`

Changes the layout of an already-rendered graph.

```typescript
const updated = await renderer.updateLayout(rendered, 'hierarchical');
```

### Layout Algorithms

| Layout | Description | Best For |
|--------|-------------|----------|
| `force` | Physics-based simulation | General-purpose, organic layouts |
| `hierarchical` | Top-down layered layout | Call hierarchies, dependencies |
| `radial` | Concentric circles | Central component analysis |
| `circular` | Nodes on a circle | Small graphs, ring structures |
| `grid` | Regular grid placement | Comparing similar components |

## GraphAnalyzer

Analyzes graph structure and identifies patterns, clusters, and risks.

### Constructor

```typescript
import { GraphAnalyzer } from '@migrationpilot/core';

const analyzer = new GraphAnalyzer();
```

### Methods

#### `analyze(graph)`

Performs comprehensive analysis of a knowledge graph.

```typescript
const analysis = analyzer.analyze(graph);

// Structural metrics
console.log(analysis.structural.nodeCount);
console.log(analysis.structural.edgeCount);
console.log(analysis.structural.density);
console.log(analysis.structural.averageDegree);
console.log(analysis.structural.connectedComponents);

// Cluster analysis
console.log(analysis.clusters.clusterCount);
console.log(analysis.clusters.clusters);  // Array of cluster info
console.log(analysis.clusters.modularity);

// Critical path analysis
console.log(analysis.criticalPaths.criticalNodes);
console.log(analysis.criticalPaths.bottlenecks);

// Risk analysis
console.log(analysis.risks.highRiskNodes);
console.log(analysis.risks.orphanedNodes);
console.log(analysis.risks.circularDependencies);

// Recommendations
analysis.recommendations.forEach(rec => {
  console.log(`${rec.priority}: ${rec.title}`);
  console.log(`  ${rec.description}`);
});
```

## CollaborationManager

Enables SME collaboration through annotations, decisions, and comments.

### Constructor

```typescript
import { CollaborationManager } from '@migrationpilot/core';

const collaboration = new CollaborationManager();
```

### Session Management

```typescript
// Create a collaboration session
const session = collaboration.createSession(
  'project-id',
  'graph-id',
  'Review Session',
  'user-1',
  'Alice'
);

// Join an existing session
collaboration.joinSession(session.id, 'user-2', 'Bob', 'reviewer');

// Leave session
collaboration.leaveSession(session.id, 'user-2');

// Get session status
const status = collaboration.getSession(session.id);
```

### Annotations

```typescript
// Add annotation to a node
const annotation = collaboration.addAnnotation(
  session.id,
  'node-id',
  'question',      // 'comment' | 'question' | 'suggestion' | 'flag' | 'correction'
  'Is this rule still valid?',
  'user-1'
);

// Resolve annotation
collaboration.resolveAnnotation(session.id, annotation.id, 'user-2');
```

### Decisions

```typescript
// Make a decision on a rule/node
const decision = collaboration.makeDecision(
  session.id,
  'rule-id',
  'approved',      // 'approved' | 'rejected' | 'needs_clarification' | 'deferred'
  'user-1',
  'Verified against business requirements'
);

// Get decision statistics
const stats = collaboration.getDecisionStats(session.id);
console.log(stats.approved, stats.rejected, stats.pending);
```

### Comments

```typescript
// Add comment
const comment = collaboration.addComment(
  session.id,
  'This looks correct',
  'user-1',
  'node-id'  // Optional - associates with specific node
);

// Reply to comment
collaboration.replyToComment(session.id, comment.id, 'Thanks!', 'user-2');

// Resolve comment thread
collaboration.resolveComment(session.id, comment.id, 'user-1');
```

## GraphExporter

Exports knowledge graphs to various formats.

### Supported Formats

| Format | Extension | Description |
|--------|-----------|-------------|
| `json` | `.json` | Native JSON format with full metadata |
| `graphml` | `.graphml` | XML-based graph format |
| `gexf` | `.gexf` | Gephi Exchange Format |
| `dot` | `.dot` | Graphviz DOT format |
| `csv` | `.csv` | Nodes and edges as CSV |
| `svg` | `.svg` | Scalable Vector Graphics |
| `cytoscape` | `.json` | Cytoscape.js JSON format |

### Usage

```typescript
import { GraphExporter } from '@migrationpilot/core';

const exporter = new GraphExporter();
const graph = graphBuilder.getGraph();

// Export to various formats
const jsonResult = exporter.export(graph, 'json');
const graphmlResult = exporter.export(graph, 'graphml');
const dotResult = exporter.export(graph, 'dot');

// Access exported data
console.log(jsonResult.data);       // String content
console.log(jsonResult.mimeType);   // 'application/json'
console.log(jsonResult.filename);   // 'knowledge-graph.json'
```

## GraphVisualizationService

Unified service combining all visualization capabilities.

### Constructor

```typescript
import { GraphVisualizationService } from '@migrationpilot/core';

const service = new GraphVisualizationService();
```

### Rendering

```typescript
const graph = graphBuilder.getGraph();
const rendered = await service.render(graph);

// Access current rendered state
const current = service.getRenderedGraph();
```

### Interaction

```typescript
// Node selection
service.selectNodes(['node-1', 'node-2']);
service.clearSelection();

// Search
const results = service.search('CALC');

// Navigation
service.zoomTo(1.5);
service.panTo(100, 200);
service.fitToView();
service.focusOnNode('node-id');
```

### Analysis & Export

```typescript
// Analyze
const analysis = await service.analyzeGraph();

// Export
const exported = await service.exportGraph('graphml');
```

### Collaboration

```typescript
// Start collaboration session
const session = await service.startCollaborationSession('Review Session');

// Add annotation
const annotation = service.addAnnotation('node-id', {
  type: 'comment',
  content: 'Needs review',
  author: 'user-1',
  createdAt: new Date(),
  resolved: false,
});

// Get current session
const currentSession = service.getCurrentSession();
```

### Events

```typescript
// Subscribe to events
service.on('selection-change', (selection) => {
  console.log('Selected:', selection);
});

service.on('layout-change', (layout) => {
  console.log('Layout changed to:', layout);
});

service.on('analysis-complete', (analysis) => {
  console.log('Analysis:', analysis);
});

// Unsubscribe
service.off('selection-change', handler);
```

## TypeScript Types

Key types for the visualization module:

```typescript
interface VisualizationConfig {
  layout: LayoutAlgorithm;
  width: number;
  height: number;
  nodeSize: { min: number; max: number };
  edgeWidth: { min: number; max: number };
  showLabels: boolean;
  showEdgeLabels: boolean;
  animateTransitions: boolean;
  theme: VisualizationTheme;
}

interface RenderedGraph {
  id: string;
  nodes: RenderedNode[];
  edges: RenderedEdge[];
  viewState: ViewState;
  selectionState: SelectionState;
  renderStats: RenderStatistics;
}

interface GraphAnalysis {
  structural: StructuralAnalysis;
  clusters: ClusterAnalysis;
  criticalPaths: CriticalPathAnalysis;
  risks: RiskAnalysis;
  recommendations: AnalysisRecommendation[];
  analyzedAt: Date;
}

interface CollaborationSession {
  id: string;
  projectId: string;
  graphId: string;
  name: string;
  status: 'active' | 'paused' | 'completed';
  participants: SessionParticipant[];
  annotations: GraphAnnotation[];
  decisions: NodeDecision[];
  comments: CollaborationComment[];
}
```

## Example: Complete Workflow

```typescript
import {
  KnowledgeGraphBuilder,
  GraphVisualizationService,
} from '@migrationpilot/core';

// 1. Build the knowledge graph
const graphBuilder = new KnowledgeGraphBuilder('project-1', 'Legacy System');

graphBuilder.addNode({ id: 'prog-main', type: 'program', name: 'MAINPROG', properties: {} });
graphBuilder.addNode({ id: 'rule-calc', type: 'business-rule', name: 'Interest Calc', properties: {} });
graphBuilder.addEdge('prog-main', 'rule-calc', 'implements');

const graph = graphBuilder.getGraph();

// 2. Create visualization service
const service = new GraphVisualizationService();

// 3. Render the graph
const rendered = await service.render(graph);

// 4. Analyze the structure
const analysis = await service.analyzeGraph();
console.log(`Found ${analysis.risks.highRiskNodes.length} high-risk nodes`);

// 5. Start collaboration
const session = await service.startCollaborationSession('SME Review');

// 6. Add annotations
service.addAnnotation('rule-calc', {
  type: 'question',
  content: 'Is this rule still accurate?',
  author: 'analyst-1',
  createdAt: new Date(),
  resolved: false,
});

// 7. Export for sharing
const exported = await service.exportGraph('graphml');
```

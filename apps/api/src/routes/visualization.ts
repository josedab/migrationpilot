/**
 * Knowledge Graph Visualization API Routes
 * Interactive visualization endpoints for business rules, data flows, and system dependencies
 */

import { Hono } from 'hono';
import { z } from 'zod';
import {
  GraphVisualizationService,
  GraphAnalyzer,
  type LayoutAlgorithm,
  type ExportFormat,
  type ActiveFilters,
} from '@migrationpilot/core';
import type { KnowledgeGraph, NodeType, EdgeType } from '@migrationpilot/core';

export const visualizationRoutes = new Hono();

// In-memory storage for demo (would be database in production)
const graphCache = new Map<string, KnowledgeGraph>();
const sessionCache = new Map<string, { service: GraphVisualizationService; graph: KnowledgeGraph }>();

// Schema definitions
const LayoutOptionsSchema = z.object({
  nodeRepulsion: z.number().optional(),
  edgeLength: z.number().optional(),
  gravity: z.number().optional(),
  direction: z.enum(['TB', 'BT', 'LR', 'RL']).optional(),
  levelSeparation: z.number().optional(),
  nodeSeparation: z.number().optional(),
  animate: z.boolean().optional(),
  animationDuration: z.number().optional(),
  clusterByType: z.boolean().optional(),
  clusterBy: z.string().optional(),
});

const FiltersSchema = z.object({
  nodeTypes: z.array(z.string()).optional(),
  edgeTypes: z.array(z.string()).optional(),
  tags: z.array(z.string()).optional(),
  search: z.string().optional(),
  confidenceRange: z.object({
    min: z.number(),
    max: z.number(),
  }).optional(),
  depthFromFocus: z.number().optional(),
  showOnlyConnected: z.boolean().optional(),
  hideApproved: z.boolean().optional(),
  showOnlyPending: z.boolean().optional(),
});

const RenderRequestSchema = z.object({
  projectId: z.string(),
  graphId: z.string().optional(),
  layout: z.enum(['force', 'hierarchical', 'radial', 'circular', 'grid', 'dagre', 'cola', 'cose']).default('force'),
  layoutOptions: LayoutOptionsSchema.optional(),
  filters: FiltersSchema.optional(),
  theme: z.enum(['light', 'dark', 'high-contrast']).default('light'),
});

/**
 * POST /api/visualization/render
 * Render a knowledge graph for visualization
 */
visualizationRoutes.post('/render', async (c) => {
  const body = await c.req.json();
  const parsed = RenderRequestSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const { projectId, layout, layoutOptions, filters, theme } = parsed.data;

    // Get or create graph from cache
    let graph = graphCache.get(projectId);
    if (!graph) {
      // Create demo graph for visualization
      graph = createDemoGraph(projectId);
      graphCache.set(projectId, graph);
    }

    // Create visualization service
    const service = new GraphVisualizationService();

    // Build configuration
    const config = buildVisualizationConfig(layout as LayoutAlgorithm, layoutOptions, theme);

    // Render graph
    const renderedGraph = await service.render(graph, config);

    // Apply filters if provided
    let finalGraph = renderedGraph;
    if (filters) {
      finalGraph = await service.applyFilters(filters as ActiveFilters);
    }

    // Store session for subsequent operations
    sessionCache.set(projectId, { service, graph });

    return c.json({
      data: {
        rendered: serializeRenderedGraph(finalGraph),
        statistics: graph.statistics,
        config: {
          layout,
          theme,
          availableLayouts: ['force', 'hierarchical', 'radial', 'circular', 'grid', 'dagre', 'cola', 'cose'],
        },
      },
    });
  } catch (error) {
    return c.json({
      error: 'Render failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/layout
 * Update layout for existing visualization
 */
visualizationRoutes.post('/layout', async (c) => {
  const body = await c.req.json();
  const { projectId, layout, layoutOptions } = body;

  if (!projectId || !layout) {
    return c.json({ error: 'projectId and layout required' }, 400);
  }

  try {
    const session = sessionCache.get(projectId);
    if (!session) {
      return c.json({ error: 'No active visualization session. Call /render first.' }, 404);
    }

    const config = buildVisualizationConfig(layout as LayoutAlgorithm, layoutOptions);
    const renderedGraph = await session.service.render(session.graph, config);

    return c.json({
      data: serializeRenderedGraph(renderedGraph),
    });
  } catch (error) {
    return c.json({
      error: 'Layout update failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/filter
 * Apply filters to visualization
 */
visualizationRoutes.post('/filter', async (c) => {
  const body = await c.req.json();
  const { projectId, filters } = body;

  if (!projectId) {
    return c.json({ error: 'projectId required' }, 400);
  }

  try {
    const session = sessionCache.get(projectId);
    if (!session) {
      return c.json({ error: 'No active visualization session. Call /render first.' }, 404);
    }

    const renderedGraph = await session.service.applyFilters(filters as ActiveFilters);

    return c.json({
      data: serializeRenderedGraph(renderedGraph),
    });
  } catch (error) {
    return c.json({
      error: 'Filter failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/visualization/node/:projectId/:nodeId
 * Get detailed information about a specific node
 */
visualizationRoutes.get('/node/:projectId/:nodeId', async (c) => {
  const { projectId, nodeId } = c.req.param();

  try {
    const graph = graphCache.get(projectId);
    if (!graph) {
      return c.json({ error: 'Graph not found' }, 404);
    }

    const node = graph.nodes.get(nodeId);
    if (!node) {
      return c.json({ error: 'Node not found' }, 404);
    }

    // Get connected edges
    const incomingEdgeIds = graph.indices.incomingEdges.get(nodeId) || new Set();
    const outgoingEdgeIds = graph.indices.outgoingEdges.get(nodeId) || new Set();

    const incomingEdges = Array.from(incomingEdgeIds).map(id => graph.edges.get(id)).filter(Boolean);
    const outgoingEdges = Array.from(outgoingEdgeIds).map(id => graph.edges.get(id)).filter(Boolean);

    // Get connected nodes
    const connectedNodeIds = new Set<string>();
    for (const edge of incomingEdges) {
      if (edge) connectedNodeIds.add(edge.source);
    }
    for (const edge of outgoingEdges) {
      if (edge) connectedNodeIds.add(edge.target);
    }

    const connectedNodes = Array.from(connectedNodeIds)
      .map(id => graph.nodes.get(id))
      .filter(Boolean);

    return c.json({
      data: {
        node: serializeNode(node),
        connections: {
          incoming: incomingEdges.map(e => e && serializeEdge(e)),
          outgoing: outgoingEdges.map(e => e && serializeEdge(e)),
          connectedNodes: connectedNodes.map(n => n && serializeNode(n)),
        },
        metrics: {
          inDegree: incomingEdgeIds.size,
          outDegree: outgoingEdgeIds.size,
          totalDegree: incomingEdgeIds.size + outgoingEdgeIds.size,
        },
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to get node details',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/impact/:projectId/:nodeId
 * Analyze impact of a node (what depends on it)
 */
visualizationRoutes.post('/impact/:projectId/:nodeId', async (c) => {
  const { projectId, nodeId } = c.req.param();
  const body = await c.req.json().catch(() => ({}));
  const maxDepth = body.maxDepth || 3;

  try {
    // Get or create graph
    let graph = graphCache.get(projectId);
    if (!graph) {
      graph = createDemoGraph(projectId);
      graphCache.set(projectId, graph);
    }

    // Perform simple BFS-based impact analysis
    const impactedNodeIds = new Set<string>();
    const impactedEdgeIds = new Set<string>();
    const visited = new Set<string>();
    const queue: { id: string; depth: number }[] = [{ id: nodeId, depth: 0 }];

    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current.id) || current.depth > maxDepth) continue;
      visited.add(current.id);
      impactedNodeIds.add(current.id);

      // Find outgoing edges
      const outgoingEdges = graph.indices.outgoingEdges.get(current.id);
      if (outgoingEdges) {
        for (const edgeId of outgoingEdges) {
          const edge = graph.edges.get(edgeId);
          if (edge) {
            impactedEdgeIds.add(edgeId);
            if (!visited.has(edge.target)) {
              queue.push({ id: edge.target, depth: current.depth + 1 });
            }
          }
        }
      }

      // Find incoming edges (reverse dependencies)
      const incomingEdges = graph.indices.incomingEdges.get(current.id);
      if (incomingEdges) {
        for (const edgeId of incomingEdges) {
          const edge = graph.edges.get(edgeId);
          if (edge) {
            impactedEdgeIds.add(edgeId);
            if (!visited.has(edge.source)) {
              queue.push({ id: edge.source, depth: current.depth + 1 });
            }
          }
        }
      }
    }

    const impactedNodes = Array.from(impactedNodeIds)
      .map(id => graph!.nodes.get(id))
      .filter((n): n is NonNullable<typeof n> => n !== undefined);

    const impactedEdges = Array.from(impactedEdgeIds)
      .map(id => graph!.edges.get(id))
      .filter((e): e is NonNullable<typeof e> => e !== undefined);

    // Count direct dependents
    const directDependents = impactedNodes.filter(n => {
      const edges = graph!.indices.outgoingEdges.get(nodeId);
      return edges && Array.from(edges).some(eId => {
        const e = graph!.edges.get(eId);
        return e && e.target === n.id;
      });
    }).length;

    return c.json({
      data: {
        impactedNodes: impactedNodes.map(n => serializeNode(n)),
        impactedEdges: impactedEdges.map(e => serializeEdge(e)),
        summary: {
          directDependents,
          totalImpacted: impactedNodes.length,
          maxDepthReached: maxDepth,
        },
      },
    });
  } catch (error) {
    return c.json({
      error: 'Impact analysis failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/search/:projectId
 * Search for nodes in the graph
 */
visualizationRoutes.post('/search/:projectId', async (c) => {
  const { projectId } = c.req.param();
  const body = await c.req.json();
  const { query, nodeTypes, limit = 50 } = body;

  if (!query) {
    return c.json({ error: 'query required' }, 400);
  }

  try {
    const graph = graphCache.get(projectId);
    if (!graph) {
      return c.json({ error: 'Graph not found' }, 404);
    }

    const results: typeof Array.prototype = [];
    const lowerQuery = query.toLowerCase();

    for (const node of graph.nodes.values()) {
      // Filter by type if specified
      if (nodeTypes && nodeTypes.length > 0 && !nodeTypes.includes(node.type)) {
        continue;
      }

      // Search in name and description
      const nameMatch = node.name.toLowerCase().includes(lowerQuery);
      const descMatch = node.description?.toLowerCase().includes(lowerQuery);
      const tagMatch = node.metadata.tags?.some(t => t.toLowerCase().includes(lowerQuery));

      if (nameMatch || descMatch || tagMatch) {
        results.push({
          node: serializeNode(node),
          matchType: nameMatch ? 'name' : descMatch ? 'description' : 'tag',
        });

        if (results.length >= limit) break;
      }
    }

    return c.json({
      data: {
        results,
        total: results.length,
        query,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Search failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/export/:projectId
 * Export graph in various formats
 */
visualizationRoutes.post('/export/:projectId', async (c) => {
  const { projectId } = c.req.param();
  const body = await c.req.json();
  const { format = 'json', options = {} } = body;

  try {
    const session = sessionCache.get(projectId);
    if (!session) {
      return c.json({ error: 'No active visualization session. Call /render first.' }, 404);
    }

    const exportResult = await session.service.exportGraph(format as ExportFormat, options);

    // For image formats, return URL or base64
    if (['png', 'svg', 'pdf'].includes(format)) {
      return c.json({
        data: {
          format,
          url: exportResult.url,
          generatedAt: exportResult.generatedAt,
        },
      });
    }

    // For data formats, return the content
    return c.json({
      data: {
        format,
        content: exportResult.data,
        generatedAt: exportResult.generatedAt,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Export failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/visualization/analyze/:projectId
 * Run structural analysis on the graph
 */
visualizationRoutes.post('/analyze/:projectId', async (c) => {
  const { projectId } = c.req.param();

  try {
    const graph = graphCache.get(projectId);
    if (!graph) {
      return c.json({ error: 'Graph not found' }, 404);
    }

    const analyzer = new GraphAnalyzer();
    const analysis = analyzer.analyze(graph);

    return c.json({
      data: analysis,
    });
  } catch (error) {
    return c.json({
      error: 'Analysis failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/visualization/statistics/:projectId
 * Get graph statistics
 */
visualizationRoutes.get('/statistics/:projectId', async (c) => {
  const { projectId } = c.req.param();

  try {
    const graph = graphCache.get(projectId);
    if (!graph) {
      return c.json({ error: 'Graph not found' }, 404);
    }

    return c.json({
      data: {
        ...graph.statistics,
        nodeTypeDistribution: graph.statistics.nodesByType,
        edgeTypeDistribution: graph.statistics.edgesByType,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to get statistics',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function buildVisualizationConfig(
  layout: LayoutAlgorithm,
  layoutOptions?: z.infer<typeof LayoutOptionsSchema>,
  theme: string = 'light'
) {
  const themeColors = theme === 'dark'
    ? {
        background: '#1a1a2e',
        text: '#eaeaea',
        selection: '#4a90d9',
        highlight: '#f39c12',
        nodeColors: {
          'program': '#3498db',
          'procedure': '#2ecc71',
          'business-rule': '#e74c3c',
          'data-structure': '#9b59b6',
          'file': '#f39c12',
          'database': '#1abc9c',
          'external-system': '#e67e22',
          'regulation': '#c0392b',
          'concept': '#7f8c8d',
        } as Record<NodeType, string>,
        edgeColors: {
          'calls': '#3498db',
          'uses': '#2ecc71',
          'modifies': '#e74c3c',
          'reads': '#9b59b6',
          'writes': '#f39c12',
          'depends-on': '#1abc9c',
          'implements': '#e67e22',
          'affected-by': '#c0392b',
          'contains': '#95a5a6',
          'related-to': '#7f8c8d',
        } as Record<EdgeType, string>,
      }
    : {
        background: '#ffffff',
        text: '#2c3e50',
        selection: '#3498db',
        highlight: '#e74c3c',
        nodeColors: {
          'program': '#3498db',
          'procedure': '#27ae60',
          'business-rule': '#e74c3c',
          'data-structure': '#8e44ad',
          'file': '#f1c40f',
          'database': '#16a085',
          'external-system': '#d35400',
          'regulation': '#c0392b',
          'concept': '#95a5a6',
        } as Record<NodeType, string>,
        edgeColors: {
          'calls': '#3498db',
          'uses': '#27ae60',
          'modifies': '#e74c3c',
          'reads': '#8e44ad',
          'writes': '#f1c40f',
          'depends-on': '#16a085',
          'implements': '#d35400',
          'affected-by': '#c0392b',
          'contains': '#bdc3c7',
          'related-to': '#95a5a6',
        } as Record<EdgeType, string>,
      };

  return {
    layout,
    layoutOptions: {
      nodeRepulsion: 400,
      edgeLength: 150,
      gravity: 0.05,
      animate: true,
      animationDuration: 500,
      ...layoutOptions,
    },
    theme: {
      name: theme,
      colors: themeColors,
      fonts: {
        nodeLabel: '12px Inter, sans-serif',
        edgeLabel: '10px Inter, sans-serif',
        tooltip: '14px Inter, sans-serif',
      },
    },
    nodeStyles: {
      shapes: {
        'program': 'rectangle',
        'procedure': 'ellipse',
        'business-rule': 'diamond',
        'data-structure': 'hexagon',
        'file': 'rectangle',
        'database': 'ellipse',
        'external-system': 'rectangle',
        'regulation': 'triangle',
        'concept': 'circle',
      } as Record<NodeType, 'circle' | 'rectangle' | 'diamond' | 'hexagon' | 'triangle' | 'ellipse'>,
      sizes: { default: 40, min: 20, max: 80, scaleBy: 'connections' as const },
      labels: { show: true, position: 'bottom' as const, maxLength: 20, fontSize: 12 },
      badges: { showConfidence: true, showStatus: true, showCount: false },
    },
    edgeStyles: {
      curves: { type: 'bezier' as const },
      width: { default: 2, min: 1, max: 5, scaleBy: 'weight' as const },
      arrows: { show: true, size: 8, position: 'end' as const },
      labels: { show: false, fontSize: 10 },
    },
    interaction: {
      selectable: true,
      multiSelect: true,
      zoomable: true,
      pannable: true,
      minZoom: 0.1,
      maxZoom: 4,
      expandOnClick: true,
      collapseOnDoubleClick: true,
      showTooltipOnHover: true,
      highlightNeighborsOnHover: true,
      draggable: true,
      contextMenu: true,
    },
    performance: {
      maxVisibleNodes: 500,
      maxVisibleEdges: 1000,
      virtualScrolling: true,
      simplifyAtZoom: 0.5,
      hideLabelsAtZoom: 0.3,
      hideEdgesAtZoom: 0.2,
      useWebGL: false,
      useCaching: true,
    },
  };
}

function createDemoGraph(projectId: string): KnowledgeGraph {
  const now = new Date().toISOString();

  const nodes = new Map();
  const edges = new Map();

  // Create demo nodes representing a loan processing system
  const demoNodes = [
    { id: 'prog_loanproc', type: 'program', name: 'LOANPROC', description: 'Main loan processing program' },
    { id: 'rule_interest', type: 'business-rule', name: 'Interest Calculation', description: 'Calculate loan interest based on principal, rate, and term' },
    { id: 'rule_eligibility', type: 'business-rule', name: 'Eligibility Check', description: 'Determine if applicant meets loan criteria' },
    { id: 'rule_risk', type: 'business-rule', name: 'Risk Assessment', description: 'Calculate risk score for loan application' },
    { id: 'data_customer', type: 'data-structure', name: 'Customer Record', description: 'Customer information including credit history' },
    { id: 'data_loan', type: 'data-structure', name: 'Loan Application', description: 'Loan application details' },
    { id: 'data_payment', type: 'data-structure', name: 'Payment Schedule', description: 'Generated payment schedule' },
    { id: 'proc_validate', type: 'procedure', name: 'VALIDATE-INPUT', description: 'Validate input data' },
    { id: 'proc_calc', type: 'procedure', name: 'CALCULATE-LOAN', description: 'Main calculation procedure' },
    { id: 'proc_output', type: 'procedure', name: 'GENERATE-OUTPUT', description: 'Generate output documents' },
    { id: 'db_customers', type: 'database', name: 'CUSTDB', description: 'Customer database' },
    { id: 'db_loans', type: 'database', name: 'LOANDB', description: 'Loan database' },
    { id: 'ext_credit', type: 'external-system', name: 'Credit Bureau API', description: 'External credit check service' },
    { id: 'reg_tila', type: 'regulation', name: 'TILA Compliance', description: 'Truth in Lending Act requirements' },
  ];

  for (const node of demoNodes) {
    nodes.set(node.id, {
      ...node,
      properties: { category: node.type },
      metadata: {
        createdAt: now,
        updatedAt: now,
        confidence: Math.random() * 0.3 + 0.7,
        tags: [node.type],
      },
    });
  }

  // Create demo edges
  const demoEdges = [
    { source: 'prog_loanproc', target: 'proc_validate', type: 'contains' },
    { source: 'prog_loanproc', target: 'proc_calc', type: 'contains' },
    { source: 'prog_loanproc', target: 'proc_output', type: 'contains' },
    { source: 'proc_validate', target: 'proc_calc', type: 'calls' },
    { source: 'proc_calc', target: 'proc_output', type: 'calls' },
    { source: 'proc_calc', target: 'rule_interest', type: 'implements' },
    { source: 'proc_calc', target: 'rule_eligibility', type: 'implements' },
    { source: 'proc_calc', target: 'rule_risk', type: 'implements' },
    { source: 'rule_interest', target: 'data_loan', type: 'uses' },
    { source: 'rule_interest', target: 'data_payment', type: 'modifies' },
    { source: 'rule_eligibility', target: 'data_customer', type: 'uses' },
    { source: 'rule_risk', target: 'data_customer', type: 'uses' },
    { source: 'rule_risk', target: 'ext_credit', type: 'depends-on' },
    { source: 'data_customer', target: 'db_customers', type: 'reads' },
    { source: 'data_loan', target: 'db_loans', type: 'reads' },
    { source: 'data_payment', target: 'db_loans', type: 'writes' },
    { source: 'reg_tila', target: 'rule_interest', type: 'affected-by' },
    { source: 'reg_tila', target: 'rule_eligibility', type: 'affected-by' },
  ];

  for (const edge of demoEdges) {
    const id = `edge_${edge.source}_${edge.type}_${edge.target}`;
    edges.set(id, {
      id,
      source: edge.source,
      target: edge.target,
      type: edge.type,
      properties: {},
      weight: 1,
      confidence: Math.random() * 0.3 + 0.7,
      metadata: { createdAt: now },
    });
  }

  // Build indices
  const byType = new Map<NodeType, Set<string>>();
  const byTag = new Map<string, Set<string>>();
  const incomingEdges = new Map<string, Set<string>>();
  const outgoingEdges = new Map<string, Set<string>>();

  for (const node of nodes.values()) {
    if (!byType.has(node.type)) byType.set(node.type, new Set());
    byType.get(node.type)!.add(node.id);

    if (node.metadata.tags) {
      for (const tag of node.metadata.tags) {
        if (!byTag.has(tag)) byTag.set(tag, new Set());
        byTag.get(tag)!.add(node.id);
      }
    }
  }

  for (const edge of edges.values()) {
    if (!outgoingEdges.has(edge.source)) outgoingEdges.set(edge.source, new Set());
    outgoingEdges.get(edge.source)!.add(edge.id);

    if (!incomingEdges.has(edge.target)) incomingEdges.set(edge.target, new Set());
    incomingEdges.get(edge.target)!.add(edge.id);
  }

  // Build statistics
  const nodesByType: Record<string, number> = {};
  const edgesByType: Record<string, number> = {};

  for (const [type, set] of byType) {
    nodesByType[type] = set.size;
  }

  for (const edge of edges.values()) {
    edgesByType[edge.type] = (edgesByType[edge.type] || 0) + 1;
  }

  return {
    id: `kg_${projectId}`,
    projectId,
    name: 'Loan Processing Knowledge Graph',
    version: 1,
    nodes,
    edges,
    indices: { byType, byTag, incomingEdges, outgoingEdges },
    statistics: {
      nodeCount: nodes.size,
      edgeCount: edges.size,
      nodesByType,
      edgesByType,
    },
    createdAt: now,
    updatedAt: now,
  };
}

function serializeNode(node: any) {
  return {
    id: node.id,
    type: node.type,
    name: node.name,
    description: node.description,
    properties: node.properties,
    metadata: {
      ...node.metadata,
      confidence: node.metadata?.confidence,
      tags: node.metadata?.tags,
    },
  };
}

function serializeEdge(edge: any) {
  return {
    id: edge.id,
    source: edge.source,
    target: edge.target,
    type: edge.type,
    weight: edge.weight,
    confidence: edge.confidence,
    properties: edge.properties,
  };
}

function serializeRenderedGraph(graph: any) {
  return {
    id: graph.id,
    nodes: graph.nodes.map((n: any) => ({
      id: n.id,
      x: n.x,
      y: n.y,
      width: n.width,
      height: n.height,
      data: serializeNode(n.sourceNode),
      style: n.style,
      state: {
        visible: n.visible,
        selected: n.selected,
        highlighted: n.highlighted,
        expanded: n.expanded,
      },
    })),
    edges: graph.edges.map((e: any) => ({
      id: e.id,
      source: e.sourceEdge.source,
      target: e.sourceEdge.target,
      sourceX: e.sourceX,
      sourceY: e.sourceY,
      targetX: e.targetX,
      targetY: e.targetY,
      data: serializeEdge(e.sourceEdge),
      style: e.style,
      state: {
        visible: e.visible,
        selected: e.selected,
        highlighted: e.highlighted,
      },
    })),
    viewState: graph.viewState,
    statistics: graph.renderStats,
  };
}

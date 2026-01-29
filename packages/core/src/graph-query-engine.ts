/**
 * Knowledge Graph Query Engine
 * 
 * Provides query capabilities for the knowledge graph.
 */

import type {
  KnowledgeNode,
  KnowledgeEdge,
  KnowledgeGraph,
  QueryResult,
  GraphQuery,
} from './knowledge-graph.js';

/**
 * Engine for querying knowledge graphs
 */
export class GraphQueryEngine {
  constructor(private graph: KnowledgeGraph) {}

  /**
   * Execute a query against the knowledge graph
   */
  query(q: GraphQuery): QueryResult {
    const startTime = Date.now();
    const result: QueryResult = {
      nodes: [],
      edges: [],
      statistics: {
        nodesFound: 0,
        edgesFound: 0,
        queryTimeMs: 0,
      },
    };

    switch (q.type) {
      case 'nodes':
        result.nodes = this.queryNodes(q);
        break;
      
      case 'edges':
        result.edges = this.queryEdges(q);
        break;
      
      case 'path':
        if (q.source && q.target) {
          result.paths = this.findPaths(q.source, q.target, q.maxDepth || 5);
          result.nodes = this.getNodesFromPaths(result.paths);
        }
        break;
      
      case 'subgraph':
        if (q.source) {
          const { nodes, edges } = this.getSubgraph(q.source, q.maxDepth || 2, q.filters);
          result.nodes = nodes;
          result.edges = edges;
        }
        break;
      
      case 'impact':
        if (q.source) {
          const { nodes, edges } = this.analyzeImpact(q.source, q.maxDepth || 3);
          result.nodes = nodes;
          result.edges = edges;
        }
        break;
    }

    result.statistics.nodesFound = result.nodes.length;
    result.statistics.edgesFound = result.edges.length;
    result.statistics.queryTimeMs = Date.now() - startTime;

    if (q.limit && result.nodes.length > q.limit) {
      result.nodes = result.nodes.slice(0, q.limit);
    }

    return result;
  }

  /**
   * Find all nodes affected by a change to the source node
   */
  analyzeImpact(sourceId: string, maxDepth: number = 3): { nodes: KnowledgeNode[]; edges: KnowledgeEdge[] } {
    const visited = new Set<string>();
    const impactedNodes: KnowledgeNode[] = [];
    const impactedEdges: KnowledgeEdge[] = [];

    const traverse = (nodeId: string, depth: number) => {
      if (depth > maxDepth || visited.has(nodeId)) return;
      visited.add(nodeId);

      const node = this.graph.nodes.get(nodeId);
      if (node) impactedNodes.push(node);

      // Follow outgoing edges (things that depend on this node)
      const outgoing = this.graph.indices.outgoingEdges.get(nodeId);
      if (outgoing) {
        for (const edgeId of outgoing) {
          const edge = this.graph.edges.get(edgeId);
          if (edge) {
            impactedEdges.push(edge);
            traverse(edge.target, depth + 1);
          }
        }
      }

      // Follow incoming "uses" and "depends-on" edges (things that use this node)
      const incoming = this.graph.indices.incomingEdges.get(nodeId);
      if (incoming) {
        for (const edgeId of incoming) {
          const edge = this.graph.edges.get(edgeId);
          if (edge && ['uses', 'depends-on', 'reads'].includes(edge.type)) {
            impactedEdges.push(edge);
            traverse(edge.source, depth + 1);
          }
        }
      }
    };

    traverse(sourceId, 0);
    return { nodes: impactedNodes, edges: impactedEdges };
  }

  /**
   * Find nodes by regulation
   */
  findByRegulation(regulationId: string): KnowledgeNode[] {
    const regNodeId = `reg_${regulationId}`;
    const result: KnowledgeNode[] = [];

    const outgoing = this.graph.indices.outgoingEdges.get(regNodeId);
    if (outgoing) {
      for (const edgeId of outgoing) {
        const edge = this.graph.edges.get(edgeId);
        if (edge && edge.type === 'affected-by') {
          const node = this.graph.nodes.get(edge.target);
          if (node) result.push(node);
        }
      }
    }

    return result;
  }

  /**
   * Find all data flows for a data item
   */
  traceDataFlow(dataItemId: string): { producers: KnowledgeNode[]; consumers: KnowledgeNode[] } {
    const producers: KnowledgeNode[] = [];
    const consumers: KnowledgeNode[] = [];

    // Find producers (nodes that modify this data)
    const incoming = this.graph.indices.incomingEdges.get(dataItemId);
    if (incoming) {
      for (const edgeId of incoming) {
        const edge = this.graph.edges.get(edgeId);
        if (edge && ['modifies', 'writes'].includes(edge.type)) {
          const node = this.graph.nodes.get(edge.source);
          if (node) producers.push(node);
        }
      }
    }

    // Find consumers (nodes that use this data)
    const outgoing = this.graph.indices.outgoingEdges.get(dataItemId);
    if (outgoing) {
      for (const edgeId of outgoing) {
        const edge = this.graph.edges.get(edgeId);
        if (edge && ['uses', 'reads'].includes(edge.type)) {
          const node = this.graph.nodes.get(edge.target);
          if (node) consumers.push(node);
        }
      }
    }

    return { producers, consumers };
  }

  private queryNodes(q: GraphQuery): KnowledgeNode[] {
    let nodeIds: Set<string>;

    // Start with type filter if specified
    if (q.filters?.nodeTypes && q.filters.nodeTypes.length > 0) {
      nodeIds = new Set<string>();
      for (const type of q.filters.nodeTypes) {
        const typeNodes = this.graph.indices.byType.get(type);
        if (typeNodes) {
          for (const id of typeNodes) nodeIds.add(id);
        }
      }
    } else {
      nodeIds = new Set(this.graph.nodes.keys());
    }

    // Apply tag filter
    if (q.filters?.tags && q.filters.tags.length > 0) {
      const taggedNodes = new Set<string>();
      for (const tag of q.filters.tags) {
        const tagNodes = this.graph.indices.byTag.get(tag);
        if (tagNodes) {
          for (const id of tagNodes) {
            if (nodeIds.has(id)) taggedNodes.add(id);
          }
        }
      }
      nodeIds = taggedNodes;
    }

    // Apply name pattern filter
    if (q.filters?.namePattern) {
      const pattern = new RegExp(q.filters.namePattern, 'i');
      const filtered = new Set<string>();
      for (const id of nodeIds) {
        const node = this.graph.nodes.get(id);
        if (node && pattern.test(node.name)) {
          filtered.add(id);
        }
      }
      nodeIds = filtered;
    }

    return Array.from(nodeIds)
      .map(id => this.graph.nodes.get(id)!)
      .filter(Boolean);
  }

  private queryEdges(q: GraphQuery): KnowledgeEdge[] {
    let edges = Array.from(this.graph.edges.values());

    if (q.filters?.edgeTypes && q.filters.edgeTypes.length > 0) {
      edges = edges.filter(e => q.filters!.edgeTypes!.includes(e.type));
    }

    return edges;
  }

  private findPaths(
    sourceId: string,
    targetId: string,
    maxDepth: number
  ): KnowledgeNode[][] {
    const paths: KnowledgeNode[][] = [];
    const visited = new Set<string>();

    const dfs = (currentId: string, path: KnowledgeNode[]) => {
      if (path.length > maxDepth) return;
      if (currentId === targetId) {
        const targetNode = this.graph.nodes.get(targetId);
        if (targetNode) paths.push([...path, targetNode]);
        return;
      }
      if (visited.has(currentId)) return;

      visited.add(currentId);
      const currentNode = this.graph.nodes.get(currentId);
      if (!currentNode) return;

      const outgoing = this.graph.indices.outgoingEdges.get(currentId);
      if (outgoing) {
        for (const edgeId of outgoing) {
          const edge = this.graph.edges.get(edgeId);
          if (edge) {
            dfs(edge.target, [...path, currentNode]);
          }
        }
      }

      visited.delete(currentId);
    };

    dfs(sourceId, []);
    return paths;
  }

  private getNodesFromPaths(paths: KnowledgeNode[][]): KnowledgeNode[] {
    const nodeIds = new Set<string>();
    const nodes: KnowledgeNode[] = [];

    for (const path of paths) {
      for (const node of path) {
        if (!nodeIds.has(node.id)) {
          nodeIds.add(node.id);
          nodes.push(node);
        }
      }
    }

    return nodes;
  }

  private getSubgraph(
    sourceId: string,
    maxDepth: number,
    filters?: GraphQuery['filters']
  ): { nodes: KnowledgeNode[]; edges: KnowledgeEdge[] } {
    const visited = new Set<string>();
    const nodes: KnowledgeNode[] = [];
    const edges: KnowledgeEdge[] = [];

    const traverse = (nodeId: string, depth: number) => {
      if (depth > maxDepth || visited.has(nodeId)) return;
      visited.add(nodeId);

      const node = this.graph.nodes.get(nodeId);
      if (!node) return;

      // Apply filters
      if (filters?.nodeTypes && !filters.nodeTypes.includes(node.type)) return;

      nodes.push(node);

      // Traverse outgoing edges
      const outgoing = this.graph.indices.outgoingEdges.get(nodeId);
      if (outgoing) {
        for (const edgeId of outgoing) {
          const edge = this.graph.edges.get(edgeId);
          if (edge) {
            if (!filters?.edgeTypes || filters.edgeTypes.includes(edge.type)) {
              edges.push(edge);
              traverse(edge.target, depth + 1);
            }
          }
        }
      }

      // Traverse incoming edges
      const incoming = this.graph.indices.incomingEdges.get(nodeId);
      if (incoming) {
        for (const edgeId of incoming) {
          const edge = this.graph.edges.get(edgeId);
          if (edge) {
            if (!filters?.edgeTypes || filters.edgeTypes.includes(edge.type)) {
              edges.push(edge);
              traverse(edge.source, depth + 1);
            }
          }
        }
      }
    };

    traverse(sourceId, 0);
    return { nodes, edges };
  }
}

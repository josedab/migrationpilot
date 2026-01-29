/**
 * Knowledge Graph Builder
 * 
 * Constructs a queryable graph of business rules, data flows, and system dependencies.
 * Enables queries like "find all code affected by regulation X" or "what systems use customer data".
 */

import type { BusinessRule, Procedure, SourceLocation } from './types/index.js';
import { GraphQueryEngine } from './graph-query-engine.js';
import { GraphExporter } from './graph-exporter.js';

export type NodeType = 
  | 'program'
  | 'procedure'
  | 'business-rule'
  | 'data-structure'
  | 'file'
  | 'database'
  | 'external-system'
  | 'regulation'
  | 'concept';

export type EdgeType =
  | 'calls'
  | 'uses'
  | 'modifies'
  | 'reads'
  | 'writes'
  | 'depends-on'
  | 'implements'
  | 'affected-by'
  | 'contains'
  | 'related-to';

export interface KnowledgeNode {
  id: string;
  type: NodeType;
  name: string;
  description?: string;
  properties: Record<string, unknown>;
  metadata: {
    source?: SourceLocation;
    confidence?: number;
    createdAt: string;
    updatedAt: string;
    tags?: string[];
  };
}

export interface KnowledgeEdge {
  id: string;
  source: string;
  target: string;
  type: EdgeType;
  properties: Record<string, unknown>;
  weight: number;
  confidence: number;
  metadata: {
    evidence?: string[];
    createdAt: string;
  };
}

export interface KnowledgeGraph {
  id: string;
  projectId: string;
  name: string;
  version: number;
  nodes: Map<string, KnowledgeNode>;
  edges: Map<string, KnowledgeEdge>;
  indices: {
    byType: Map<NodeType, Set<string>>;
    byTag: Map<string, Set<string>>;
    incomingEdges: Map<string, Set<string>>;
    outgoingEdges: Map<string, Set<string>>;
  };
  statistics: {
    nodeCount: number;
    edgeCount: number;
    nodesByType: Record<string, number>;
    edgesByType: Record<string, number>;
  };
  createdAt: string;
  updatedAt: string;
}

export interface QueryResult {
  nodes: KnowledgeNode[];
  edges: KnowledgeEdge[];
  paths?: KnowledgeNode[][];
  statistics: {
    nodesFound: number;
    edgesFound: number;
    queryTimeMs: number;
  };
}

export interface GraphQuery {
  type: 'nodes' | 'edges' | 'path' | 'subgraph' | 'impact';
  filters?: {
    nodeTypes?: NodeType[];
    edgeTypes?: EdgeType[];
    tags?: string[];
    properties?: Record<string, unknown>;
    namePattern?: string;
  };
  source?: string;
  target?: string;
  maxDepth?: number;
  limit?: number;
}

export class KnowledgeGraphBuilder {
  private graph: KnowledgeGraph;
  private queryEngine: GraphQueryEngine;
  private exporter: GraphExporter;

  constructor(projectId: string, name: string = 'Knowledge Graph') {
    this.graph = {
      id: `kg_${Date.now()}`,
      projectId,
      name,
      version: 1,
      nodes: new Map(),
      edges: new Map(),
      indices: {
        byType: new Map(),
        byTag: new Map(),
        incomingEdges: new Map(),
        outgoingEdges: new Map(),
      },
      statistics: {
        nodeCount: 0,
        edgeCount: 0,
        nodesByType: {},
        edgesByType: {},
      },
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    };
    this.queryEngine = new GraphQueryEngine(this.graph);
    this.exporter = new GraphExporter(this.graph);
  }

  /**
   * Add a node to the graph
   */
  addNode(node: Omit<KnowledgeNode, 'metadata'> & { metadata?: Partial<KnowledgeNode['metadata']> }): KnowledgeNode {
    const fullNode: KnowledgeNode = {
      ...node,
      metadata: {
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        ...node.metadata,
      },
    };

    this.graph.nodes.set(node.id, fullNode);
    
    // Update indices
    if (!this.graph.indices.byType.has(node.type)) {
      this.graph.indices.byType.set(node.type, new Set());
    }
    this.graph.indices.byType.get(node.type)!.add(node.id);
    
    if (fullNode.metadata.tags) {
      for (const tag of fullNode.metadata.tags) {
        if (!this.graph.indices.byTag.has(tag)) {
          this.graph.indices.byTag.set(tag, new Set());
        }
        this.graph.indices.byTag.get(tag)!.add(node.id);
      }
    }
    
    // Update statistics
    this.graph.statistics.nodeCount++;
    this.graph.statistics.nodesByType[node.type] = 
      (this.graph.statistics.nodesByType[node.type] || 0) + 1;
    
    this.graph.updatedAt = new Date().toISOString();
    
    return fullNode;
  }

  /**
   * Add an edge to the graph
   */
  addEdge(
    source: string,
    target: string,
    type: EdgeType,
    properties: Record<string, unknown> = {},
    weight: number = 1,
    confidence: number = 1
  ): KnowledgeEdge {
    const id = `edge_${source}_${type}_${target}`;
    
    const edge: KnowledgeEdge = {
      id,
      source,
      target,
      type,
      properties,
      weight,
      confidence,
      metadata: {
        createdAt: new Date().toISOString(),
      },
    };

    this.graph.edges.set(id, edge);
    
    // Update indices
    if (!this.graph.indices.outgoingEdges.has(source)) {
      this.graph.indices.outgoingEdges.set(source, new Set());
    }
    this.graph.indices.outgoingEdges.get(source)!.add(id);
    
    if (!this.graph.indices.incomingEdges.has(target)) {
      this.graph.indices.incomingEdges.set(target, new Set());
    }
    this.graph.indices.incomingEdges.get(target)!.add(id);
    
    // Update statistics
    this.graph.statistics.edgeCount++;
    this.graph.statistics.edgesByType[type] = 
      (this.graph.statistics.edgesByType[type] || 0) + 1;
    
    this.graph.updatedAt = new Date().toISOString();
    
    return edge;
  }

  /**
   * Build graph from business rules
   */
  addBusinessRules(rules: BusinessRule[]): void {
    for (const rule of rules) {
      // Add rule node
      this.addNode({
        id: `rule_${rule.id}`,
        type: 'business-rule',
        name: rule.name,
        description: rule.description,
        properties: {
          category: rule.category,
          formula: rule.formula,
          logic: rule.logic,
          confidence: rule.confidence,
          edgeCases: rule.edgeCases,
        },
        metadata: {
          source: {
            file: rule.sourceFile,
            startLine: rule.sourceLines[0],
            endLine: rule.sourceLines[1],
          },
          confidence: rule.confidence,
          tags: [rule.category, rule.reviewStatus],
        },
      });

      // Add input data nodes and edges
      for (const input of rule.inputs) {
        const inputId = `data_${input.name}`;
        if (!this.graph.nodes.has(inputId)) {
          this.addNode({
            id: inputId,
            type: 'data-structure',
            name: input.name,
            description: input.description,
            properties: {
              dataType: input.type,
              source: input.source,
              constraints: input.constraints,
            },
          });
        }
        this.addEdge(inputId, `rule_${rule.id}`, 'uses', { role: 'input' });
      }

      // Add output data nodes and edges
      for (const output of rule.outputs) {
        const outputId = `data_${output.name}`;
        if (!this.graph.nodes.has(outputId)) {
          this.addNode({
            id: outputId,
            type: 'data-structure',
            name: output.name,
            description: output.description,
            properties: {
              dataType: output.type,
            },
          });
        }
        this.addEdge(`rule_${rule.id}`, outputId, 'modifies', { role: 'output' });
      }
    }
  }

  /**
   * Add procedures to the graph
   */
  addProcedures(procedures: Procedure[], programId: string): void {
    for (const proc of procedures) {
      const procId = `proc_${programId}_${proc.name}`;
      
      this.addNode({
        id: procId,
        type: 'procedure',
        name: proc.name,
        properties: {
          procType: proc.type,
          complexity: proc.complexity,
          parameters: proc.parameters,
        },
        metadata: {
          source: proc.location,
        },
      });

      // Link to program
      this.addEdge(programId, procId, 'contains');

      // Link to called procedures
      for (const calledProc of proc.calledProcedures) {
        const calledId = `proc_${programId}_${calledProc}`;
        this.addEdge(procId, calledId, 'calls');
      }
    }
  }

  /**
   * Add regulatory/compliance annotations
   */
  addRegulation(
    regulationId: string,
    name: string,
    description: string,
    affectedNodeIds: string[]
  ): void {
    // Add regulation node
    this.addNode({
      id: `reg_${regulationId}`,
      type: 'regulation',
      name,
      description,
      properties: {
        regulationId,
      },
      metadata: {
        tags: ['compliance', 'regulation'],
      },
    });

    // Link to affected nodes
    for (const nodeId of affectedNodeIds) {
      if (this.graph.nodes.has(nodeId)) {
        this.addEdge(`reg_${regulationId}`, nodeId, 'affected-by');
      }
    }
  }

  /**
   * Query the knowledge graph (delegates to GraphQueryEngine)
   */
  query(q: GraphQuery): QueryResult {
    return this.queryEngine.query(q);
  }

  /**
   * Find all nodes affected by a change to the source node (delegates to GraphQueryEngine)
   */
  analyzeImpact(sourceId: string, maxDepth: number = 3): { nodes: KnowledgeNode[]; edges: KnowledgeEdge[] } {
    return this.queryEngine.analyzeImpact(sourceId, maxDepth);
  }

  /**
   * Find nodes by regulation (delegates to GraphQueryEngine)
   */
  findByRegulation(regulationId: string): KnowledgeNode[] {
    return this.queryEngine.findByRegulation(regulationId);
  }

  /**
   * Find all data flows for a data item (delegates to GraphQueryEngine)
   */
  traceDataFlow(dataItemId: string): { producers: KnowledgeNode[]; consumers: KnowledgeNode[] } {
    return this.queryEngine.traceDataFlow(dataItemId);
  }

  /**
   * Export graph to JSON format (delegates to GraphExporter)
   */
  exportToJSON(): string {
    return this.exporter.toJSON();
  }

  /**
   * Export to Cytoscape format (delegates to GraphExporter)
   */
  exportToCytoscape(): { nodes: object[]; edges: object[] } {
    return this.exporter.toCytoscape();
  }

  /**
   * Export to GraphML format (delegates to GraphExporter)
   */
  exportToGraphML(): string {
    return this.exporter.toGraphML();
  }

  /**
   * Export to DOT format (Graphviz) (delegates to GraphExporter)
   */
  exportToDOT(): string {
    return this.exporter.toDOT();
  }

  /**
   * Export to Mermaid diagram format (delegates to GraphExporter)
   */
  exportToMermaid(): string {
    return this.exporter.toMermaid();
  }

  /**
   * Get graph statistics
   */
  getStatistics(): KnowledgeGraph['statistics'] {
    return { ...this.graph.statistics };
  }

  /**
   * Get the full graph
   */
  getGraph(): KnowledgeGraph {
    return this.graph;
  }

  /**
   * Get the query engine for advanced queries
   */
  getQueryEngine(): GraphQueryEngine {
    return this.queryEngine;
  }

  /**
   * Get the exporter for advanced export options
   */
  getExporter(): GraphExporter {
    return this.exporter;
  }
}

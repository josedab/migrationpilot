/**
 * Knowledge Graph Exporter
 * 
 * Exports knowledge graphs to various formats for visualization and analysis.
 */

import type { KnowledgeGraph } from './knowledge-graph.js';

/**
 * Exporter for knowledge graphs to various formats
 */
export class GraphExporter {
  constructor(private graph: KnowledgeGraph) {}

  /**
   * Export graph to JSON format
   */
  toJSON(): string {
    return JSON.stringify({
      ...this.graph,
      nodes: Array.from(this.graph.nodes.entries()),
      edges: Array.from(this.graph.edges.entries()),
      indices: undefined, // Don't export indices
    }, null, 2);
  }

  /**
   * Export to Cytoscape format for visualization
   */
  toCytoscape(): { nodes: object[]; edges: object[] } {
    const nodes = Array.from(this.graph.nodes.values()).map(node => ({
      data: {
        id: node.id,
        label: node.name,
        type: node.type,
        ...node.properties,
      },
    }));

    const edges = Array.from(this.graph.edges.values()).map(edge => ({
      data: {
        id: edge.id,
        source: edge.source,
        target: edge.target,
        label: edge.type,
        weight: edge.weight,
      },
    }));

    return { nodes, edges };
  }

  /**
   * Export to GraphML format
   */
  toGraphML(): string {
    const lines: string[] = [];
    
    lines.push('<?xml version="1.0" encoding="UTF-8"?>');
    lines.push('<graphml xmlns="http://graphml.graphdrawing.org/xmlns">');
    lines.push('  <key id="label" for="node" attr.name="label" attr.type="string"/>');
    lines.push('  <key id="type" for="node" attr.name="type" attr.type="string"/>');
    lines.push('  <key id="edgeType" for="edge" attr.name="type" attr.type="string"/>');
    lines.push(`  <graph id="${this.graph.id}" edgedefault="directed">`);

    for (const node of this.graph.nodes.values()) {
      lines.push(`    <node id="${node.id}">`);
      lines.push(`      <data key="label">${this.escapeXml(node.name)}</data>`);
      lines.push(`      <data key="type">${node.type}</data>`);
      lines.push(`    </node>`);
    }

    for (const edge of this.graph.edges.values()) {
      lines.push(`    <edge id="${edge.id}" source="${edge.source}" target="${edge.target}">`);
      lines.push(`      <data key="edgeType">${edge.type}</data>`);
      lines.push(`    </edge>`);
    }

    lines.push('  </graph>');
    lines.push('</graphml>');

    return lines.join('\n');
  }

  /**
   * Export to DOT format (Graphviz)
   */
  toDOT(): string {
    const lines: string[] = [];
    
    lines.push(`digraph "${this.graph.name}" {`);
    lines.push('  rankdir=LR;');
    lines.push('  node [shape=box, style=rounded];');
    lines.push('');

    // Group nodes by type with subgraphs
    const nodesByType = new Map<string, string[]>();
    for (const node of this.graph.nodes.values()) {
      if (!nodesByType.has(node.type)) {
        nodesByType.set(node.type, []);
      }
      nodesByType.get(node.type)!.push(node.id);
      lines.push(`  "${node.id}" [label="${this.escapeQuotes(node.name)}", tooltip="${node.type}"];`);
    }

    lines.push('');

    for (const edge of this.graph.edges.values()) {
      lines.push(`  "${edge.source}" -> "${edge.target}" [label="${edge.type}"];`);
    }

    lines.push('}');

    return lines.join('\n');
  }

  /**
   * Export to Mermaid diagram format
   */
  toMermaid(): string {
    const lines: string[] = [];
    
    lines.push('graph LR');

    for (const node of this.graph.nodes.values()) {
      const shape = this.getMermaidShape(node.type);
      lines.push(`  ${node.id}${shape.open}"${this.escapeQuotes(node.name)}"${shape.close}`);
    }

    for (const edge of this.graph.edges.values()) {
      lines.push(`  ${edge.source} -->|${edge.type}| ${edge.target}`);
    }

    return lines.join('\n');
  }

  private getMermaidShape(type: string): { open: string; close: string } {
    switch (type) {
      case 'business-rule':
        return { open: '{{', close: '}}' };
      case 'procedure':
        return { open: '(', close: ')' };
      case 'data-structure':
        return { open: '[(', close: ')]' };
      case 'database':
        return { open: '[(', close: ')]' };
      case 'regulation':
        return { open: '>', close: ']' };
      default:
        return { open: '[', close: ']' };
    }
  }

  private escapeXml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }

  private escapeQuotes(text: string): string {
    return text.replace(/"/g, '\\"');
  }
}

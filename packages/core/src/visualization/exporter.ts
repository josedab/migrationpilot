/**
 * Graph Exporter
 * 
 * Exports knowledge graphs to various formats for interoperability
 * with other visualization tools and data exchange.
 */

import type { 
  KnowledgeGraph
} from '../knowledge-graph.js';
import type {
  GraphExport,
  ExportFormat,
  ExportOptions,
  RenderedGraph,
  GraphAnalysis
} from './types.js';

/**
 * Default export options
 */
const DEFAULT_OPTIONS: ExportOptions = {
  includePositions: true,
  includeAnnotations: true,
  includeDecisions: true,
  includeMetrics: true,
  filterApplied: false,
};

/**
 * Exports knowledge graphs to various formats
 */
export class GraphExporter {
  
  /**
   * Export a graph to the specified format
   */
  export(
    graph: KnowledgeGraph,
    format: ExportFormat,
    options: Partial<ExportOptions> = {},
    renderedGraph?: RenderedGraph,
    analysis?: GraphAnalysis
  ): GraphExport {
    const mergedOptions = { ...DEFAULT_OPTIONS, ...options };
    
    let data: string;
    
    switch (format) {
      case 'json':
        data = this.exportToJSON(graph, mergedOptions, renderedGraph, analysis);
        break;
      case 'graphml':
        data = this.exportToGraphML(graph, mergedOptions, renderedGraph);
        break;
      case 'gexf':
        data = this.exportToGEXF(graph, mergedOptions, renderedGraph);
        break;
      case 'cytoscape':
        data = this.exportToCytoscape(graph, mergedOptions, renderedGraph);
        break;
      case 'dot':
        data = this.exportToDOT(graph, mergedOptions);
        break;
      case 'csv':
        data = this.exportToCSV(graph, mergedOptions);
        break;
      case 'svg':
        data = this.exportToSVG(graph, mergedOptions, renderedGraph);
        break;
      default:
        data = this.exportToJSON(graph, mergedOptions, renderedGraph, analysis);
    }
    
    return {
      format,
      options: mergedOptions,
      data,
      generatedAt: new Date(),
    };
  }
  
  // ==========================================================================
  // JSON EXPORT
  // ==========================================================================
  
  private exportToJSON(
    graph: KnowledgeGraph,
    options: ExportOptions,
    renderedGraph?: RenderedGraph,
    analysis?: GraphAnalysis
  ): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    const exportData: Record<string, unknown> = {
      metadata: {
        id: graph.id,
        projectId: graph.projectId,
        name: graph.name,
        exportedAt: new Date().toISOString(),
        nodeCount: nodes.length,
        edgeCount: edges.length,
      },
      nodes: nodes.map(node => {
        const exportNode: Record<string, unknown> = {
          id: node.id,
          type: node.type,
          name: node.name,
          description: node.description,
          metadata: node.metadata,
        };
        
        if (options.includePositions && renderedGraph) {
          const rendered = renderedGraph.nodes.find(n => n.id === node.id);
          if (rendered) {
            exportNode.position = { x: rendered.x, y: rendered.y };
          }
        }
        
        if (options.includeMetrics && renderedGraph) {
          const rendered = renderedGraph.nodes.find(n => n.id === node.id);
          if (rendered) {
            exportNode.metrics = rendered.metrics;
          }
        }
        
        if (options.includeAnnotations && renderedGraph) {
          const rendered = renderedGraph.nodes.find(n => n.id === node.id);
          if (rendered?.annotations.length) {
            exportNode.annotations = rendered.annotations;
          }
        }
        
        return exportNode;
      }),
      edges: edges.map(edge => ({
        id: edge.id,
        source: edge.source,
        target: edge.target,
        type: edge.type,
        metadata: edge.metadata,
      })),
    };
    
    if (analysis) {
      exportData.analysis = {
        structural: analysis.structural,
        clusterCount: analysis.clusters.clusterCount,
        modularity: analysis.clusters.modularity,
        criticalNodes: analysis.criticalPaths.criticalNodes,
        recommendations: analysis.recommendations,
      };
    }
    
    return JSON.stringify(exportData, null, 2);
  }
  
  // ==========================================================================
  // GRAPHML EXPORT
  // ==========================================================================
  
  private exportToGraphML(
    graph: KnowledgeGraph,
    options: ExportOptions,
    renderedGraph?: RenderedGraph
  ): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    let xml = `<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
         http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">

  <!-- Node attributes -->
  <key id="d0" for="node" attr.name="name" attr.type="string"/>
  <key id="d1" for="node" attr.name="type" attr.type="string"/>
  <key id="d2" for="node" attr.name="description" attr.type="string"/>
  <key id="d3" for="node" attr.name="confidence" attr.type="double"/>`;

    if (options.includePositions) {
      xml += `
  <key id="d4" for="node" attr.name="x" attr.type="double"/>
  <key id="d5" for="node" attr.name="y" attr.type="double"/>`;
    }

    xml += `
  
  <!-- Edge attributes -->
  <key id="e0" for="edge" attr.name="type" attr.type="string"/>
  <key id="e1" for="edge" attr.name="confidence" attr.type="double"/>

  <graph id="${this.escapeXml(graph.id)}" edgedefault="directed">
`;

    // Add nodes
    for (const node of nodes) {
      const rendered = renderedGraph?.nodes.find(n => n.id === node.id);
      
      xml += `    <node id="${this.escapeXml(node.id)}">
      <data key="d0">${this.escapeXml(node.name)}</data>
      <data key="d1">${this.escapeXml(node.type)}</data>
      <data key="d2">${this.escapeXml(node.description || '')}</data>
      <data key="d3">${node.metadata?.confidence || 1}</data>`;
      
      if (options.includePositions && rendered) {
        xml += `
      <data key="d4">${rendered.x}</data>
      <data key="d5">${rendered.y}</data>`;
      }
      
      xml += `
    </node>
`;
    }
    
    // Add edges
    for (const edge of edges) {
      xml += `    <edge id="${this.escapeXml(edge.id)}" source="${this.escapeXml(edge.source)}" target="${this.escapeXml(edge.target)}">
      <data key="e0">${this.escapeXml(edge.type)}</data>
      <data key="e1">${edge.confidence || 1}</data>
    </edge>
`;
    }
    
    xml += `  </graph>
</graphml>`;
    
    return xml;
  }
  
  // ==========================================================================
  // GEXF EXPORT (Gephi)
  // ==========================================================================
  
  private exportToGEXF(
    graph: KnowledgeGraph,
    options: ExportOptions,
    renderedGraph?: RenderedGraph
  ): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    const now = new Date().toISOString().split('T')[0];
    
    let xml = `<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.3"
      xmlns:viz="http://www.gexf.net/1.3/viz"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.gexf.net/1.3 http://www.gexf.net/1.3/gexf.xsd"
      version="1.3">
  <meta lastmodifieddate="${now}">
    <creator>MigrationPilot</creator>
    <description>Knowledge Graph Export</description>
  </meta>
  <graph mode="static" defaultedgetype="directed">
    <attributes class="node">
      <attribute id="0" title="type" type="string"/>
      <attribute id="1" title="description" type="string"/>
      <attribute id="2" title="confidence" type="float"/>
    </attributes>
    <attributes class="edge">
      <attribute id="0" title="type" type="string"/>
      <attribute id="1" title="confidence" type="float"/>
    </attributes>
    <nodes>
`;

    // Node type colors
    const typeColors: Record<string, { r: number; g: number; b: number }> = {
      program: { r: 59, g: 130, b: 246 },
      procedure: { r: 139, g: 92, b: 246 },
      'business-rule': { r: 239, g: 68, b: 68 },
      'data-structure': { r: 245, g: 158, b: 11 },
      file: { r: 16, g: 185, b: 129 },
      database: { r: 6, g: 182, b: 212 },
      'external-system': { r: 99, g: 102, b: 241 },
      regulation: { r: 236, g: 72, b: 153 },
      concept: { r: 132, g: 204, b: 22 },
    };

    for (const node of nodes) {
      const rendered = renderedGraph?.nodes.find(n => n.id === node.id);
      const color = typeColors[node.type] || { r: 128, g: 128, b: 128 };
      
      xml += `      <node id="${this.escapeXml(node.id)}" label="${this.escapeXml(node.name)}">
        <attvalues>
          <attvalue for="0" value="${this.escapeXml(node.type)}"/>
          <attvalue for="1" value="${this.escapeXml(node.description || '')}"/>
          <attvalue for="2" value="${node.metadata?.confidence || 1}"/>
        </attvalues>
        <viz:color r="${color.r}" g="${color.g}" b="${color.b}"/>`;
      
      if (options.includePositions && rendered) {
        xml += `
        <viz:position x="${rendered.x}" y="${rendered.y}" z="0"/>`;
      }
      
      xml += `
      </node>
`;
    }
    
    xml += `    </nodes>
    <edges>
`;
    
    for (let i = 0; i < edges.length; i++) {
      const edge = edges[i];
      if (!edge) continue;
      xml += `      <edge id="${i}" source="${this.escapeXml(edge.source)}" target="${this.escapeXml(edge.target)}">
        <attvalues>
          <attvalue for="0" value="${this.escapeXml(edge.type)}"/>
          <attvalue for="1" value="${edge.confidence || 1}"/>
        </attvalues>
      </edge>
`;
    }
    
    xml += `    </edges>
  </graph>
</gexf>`;
    
    return xml;
  }
  
  // ==========================================================================
  // CYTOSCAPE JSON EXPORT
  // ==========================================================================
  
  private exportToCytoscape(
    graph: KnowledgeGraph,
    options: ExportOptions,
    renderedGraph?: RenderedGraph
  ): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    const elements = {
      nodes: nodes.map(node => {
        const rendered = renderedGraph?.nodes.find(n => n.id === node.id);
        
        const element: Record<string, unknown> = {
          data: {
            id: node.id,
            label: node.name,
            type: node.type,
            description: node.description,
            confidence: node.metadata?.confidence || 1,
          },
        };
        
        if (options.includePositions && rendered) {
          element.position = { x: rendered.x, y: rendered.y };
        }
        
        return element;
      }),
      edges: edges.map(edge => ({
        data: {
          id: edge.id,
          source: edge.source,
          target: edge.target,
          type: edge.type,
          confidence: edge.confidence || 1,
        },
      })),
    };
    
    return JSON.stringify(elements, null, 2);
  }
  
  // ==========================================================================
  // DOT/GRAPHVIZ EXPORT
  // ==========================================================================
  
  private exportToDOT(graph: KnowledgeGraph, _options: ExportOptions): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    // Node type shapes
    const typeShapes: Record<string, string> = {
      program: 'box',
      procedure: 'ellipse',
      'business-rule': 'hexagon',
      'data-structure': 'diamond',
      file: 'note',
      database: 'box3d',
      'external-system': 'octagon',
      regulation: 'hexagon',
      concept: 'ellipse',
    };
    
    // Node type colors
    const typeColors: Record<string, string> = {
      program: '#3b82f6',
      procedure: '#8b5cf6',
      'business-rule': '#ef4444',
      'data-structure': '#f59e0b',
      file: '#10b981',
      database: '#06b6d4',
      'external-system': '#6366f1',
      regulation: '#ec4899',
      concept: '#84cc16',
    };
    
    let dot = `digraph "${this.escapeDot(graph.name || graph.id)}" {
  // Graph properties
  graph [
    rankdir=TB
    fontname="Arial"
    fontsize=14
    bgcolor="#ffffff"
  ];
  
  // Default node properties
  node [
    fontname="Arial"
    fontsize=10
    style="filled"
  ];
  
  // Default edge properties
  edge [
    fontname="Arial"
    fontsize=8
  ];
  
  // Nodes
`;

    for (const node of nodes) {
      const shape = typeShapes[node.type] || 'ellipse';
      const color = typeColors[node.type] || '#6b7280';
      const label = this.escapeDot(node.name);
      
      dot += `  "${this.escapeDot(node.id)}" [
    label="${label}"
    shape=${shape}
    fillcolor="${color}"
    fontcolor="white"
  ];
`;
    }
    
    dot += `
  // Edges
`;
    
    for (const edge of edges) {
      const style = edge.type === 'depends-on' ? 'dashed' : 'solid';
      dot += `  "${this.escapeDot(edge.source)}" -> "${this.escapeDot(edge.target)}" [style=${style}];
`;
    }
    
    dot += `}`;
    
    return dot;
  }
  
  // ==========================================================================
  // CSV EXPORT
  // ==========================================================================
  
  private exportToCSV(graph: KnowledgeGraph, _options: ExportOptions): string {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    // Nodes CSV
    let csv = '# NODES\n';
    csv += 'id,name,type,description,confidence\n';
    
    for (const node of nodes) {
      csv += `"${this.escapeCsv(node.id)}","${this.escapeCsv(node.name)}","${this.escapeCsv(node.type)}","${this.escapeCsv(node.description || '')}",${node.metadata?.confidence || 1}\n`;
    }
    
    // Edges CSV
    csv += '\n# EDGES\n';
    csv += 'id,source,target,type,confidence\n';
    
    for (const edge of edges) {
      csv += `"${this.escapeCsv(edge.id)}","${this.escapeCsv(edge.source)}","${this.escapeCsv(edge.target)}","${this.escapeCsv(edge.type)}",${edge.confidence || 1}\n`;
    }
    
    return csv;
  }
  
  // ==========================================================================
  // SVG EXPORT
  // ==========================================================================
  
  private exportToSVG(
    _graph: KnowledgeGraph,
    _options: ExportOptions,
    renderedGraph?: RenderedGraph
  ): string {
    if (!renderedGraph) {
      return '<svg xmlns="http://www.w3.org/2000/svg"><text>No rendered graph available</text></svg>';
    }
    
    const { bounds } = renderedGraph.viewState;
    const padding = 50;
    const width = (bounds.maxX - bounds.minX) + padding * 2;
    const height = (bounds.maxY - bounds.minY) + padding * 2;
    const offsetX = -bounds.minX + padding;
    const offsetY = -bounds.minY + padding;
    
    let svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto">
      <polygon points="0 0, 10 3.5, 0 7" fill="#64748b"/>
    </marker>
  </defs>
  <rect width="100%" height="100%" fill="#ffffff"/>
  <g transform="translate(${offsetX}, ${offsetY})">
`;

    // Draw edges first
    for (const edge of renderedGraph.edges) {
      svg += `    <line 
      x1="${edge.sourceX}" y1="${edge.sourceY}" 
      x2="${edge.targetX}" y2="${edge.targetY}" 
      stroke="${edge.style.color}" 
      stroke-width="${edge.style.width}"
      stroke-opacity="${edge.style.opacity}"
      marker-end="url(#arrowhead)"
    />
`;
    }
    
    // Draw nodes
    for (const node of renderedGraph.nodes) {
      const { style } = node;
      const halfWidth = node.width / 2;
      const halfHeight = node.height / 2;
      
      if (style.shape === 'circle' || style.shape === 'ellipse') {
        svg += `    <ellipse cx="${node.x}" cy="${node.y}" rx="${halfWidth}" ry="${halfHeight}" fill="${style.backgroundColor}" stroke="${style.borderColor}" stroke-width="${style.borderWidth}"/>
`;
      } else if (style.shape === 'diamond') {
        const points = [
          `${node.x},${node.y - halfHeight}`,
          `${node.x + halfWidth},${node.y}`,
          `${node.x},${node.y + halfHeight}`,
          `${node.x - halfWidth},${node.y}`,
        ].join(' ');
        svg += `    <polygon points="${points}" fill="${style.backgroundColor}" stroke="${style.borderColor}" stroke-width="${style.borderWidth}"/>
`;
      } else {
        svg += `    <rect x="${node.x - halfWidth}" y="${node.y - halfHeight}" width="${node.width}" height="${node.height}" rx="4" fill="${style.backgroundColor}" stroke="${style.borderColor}" stroke-width="${style.borderWidth}"/>
`;
      }
      
      // Add label
      const label = node.sourceNode.name.length > 15 
        ? node.sourceNode.name.substring(0, 12) + '...'
        : node.sourceNode.name;
      svg += `    <text x="${node.x}" y="${node.y + halfHeight + 15}" text-anchor="middle" font-family="Arial" font-size="${style.fontSize}" fill="${style.labelColor}">${this.escapeXml(label)}</text>
`;
    }
    
    svg += `  </g>
</svg>`;
    
    return svg;
  }
  
  // ==========================================================================
  // UTILITIES
  // ==========================================================================
  
  private escapeXml(str: string): string {
    return str
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }
  
  private escapeDot(str: string): string {
    return str.replace(/"/g, '\\"').replace(/\n/g, '\\n');
  }
  
  private escapeCsv(str: string): string {
    return str.replace(/"/g, '""');
  }
}

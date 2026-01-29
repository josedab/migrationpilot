/**
 * Graph Command
 * 
 * Manage and export knowledge graphs
 */

import { Command } from 'commander';
import chalk from 'chalk';
import ora from 'ora';
import { writeFile, readFile, mkdir } from 'fs/promises';
import { existsSync } from 'fs';
import { table } from 'table';

interface GraphBuildOptions {
  name: string;
  output?: string;
  includeRules?: boolean;
  includeData?: boolean;
  verbose?: boolean;
}

interface GraphExportOptions {
  format: 'json' | 'graphml' | 'gexf' | 'dot' | 'csv' | 'cytoscape';
  output?: string;
}

interface GraphAnalyzeOptions {
  output?: string;
  format?: 'json' | 'table';
}

export const graphCommand = new Command('graph')
  .description('Manage and export knowledge graphs')
  .addCommand(
    new Command('build')
      .description('Build a knowledge graph from analyzed code')
      .argument('<input>', 'Input analysis file or directory')
      .option('-n, --name <name>', 'Graph name', 'Knowledge Graph')
      .option('-o, --output <file>', 'Output file path', 'knowledge-graph.json')
      .option('--include-rules', 'Include business rules in graph', true)
      .option('--include-data', 'Include data structures in graph', true)
      .option('--verbose', 'Show detailed output')
      .action(async (input: string, options: GraphBuildOptions) => {
        const spinner = ora('Loading analysis data...').start();

        try {
          // Read input file
          if (!existsSync(input)) {
            throw new Error(`Input file not found: ${input}`);
          }

          const content = await readFile(input, 'utf-8');
          const data = JSON.parse(content);

          spinner.text = 'Building knowledge graph...';

          // Build graph from analysis data
          const graph = buildGraphFromAnalysis(data, options);

          spinner.succeed(chalk.green('Knowledge graph built successfully'));

          // Display summary
          console.log('');
          console.log(chalk.bold('📊 Knowledge Graph Summary'));
          console.log(chalk.gray('─'.repeat(50)));

          const summaryData = [
            ['Property', 'Value'],
            ['Name', graph.name],
            ['Total Nodes', graph.nodes.length.toString()],
            ['Total Edges', graph.edges.length.toString()],
            ['Programs', graph.nodes.filter((n: any) => n.type === 'program').length.toString()],
            ['Procedures', graph.nodes.filter((n: any) => n.type === 'procedure').length.toString()],
            ['Business Rules', graph.nodes.filter((n: any) => n.type === 'business-rule').length.toString()],
            ['Data Structures', graph.nodes.filter((n: any) => n.type === 'database' || n.type === 'file').length.toString()],
          ];

          console.log(table(summaryData, {
            border: {
              topBody: `─`, topJoin: `┬`, topLeft: `┌`, topRight: `┐`,
              bottomBody: `─`, bottomJoin: `┴`, bottomLeft: `└`, bottomRight: `┘`,
              bodyLeft: `│`, bodyRight: `│`, bodyJoin: `│`,
              joinBody: `─`, joinLeft: `├`, joinRight: `┤`, joinJoin: `┼`,
            },
          }));

          // Show node breakdown if verbose
          if (options.verbose) {
            console.log('');
            console.log(chalk.bold('📋 Nodes by Type'));
            console.log(chalk.gray('─'.repeat(50)));

            const nodeTypes: Record<string, number> = {};
            for (const node of graph.nodes) {
              nodeTypes[node.type] = (nodeTypes[node.type] || 0) + 1;
            }

            for (const [type, count] of Object.entries(nodeTypes)) {
              console.log(`  ${chalk.cyan(type)}: ${count}`);
            }

            console.log('');
            console.log(chalk.bold('🔗 Edges by Type'));
            console.log(chalk.gray('─'.repeat(50)));

            const edgeTypes: Record<string, number> = {};
            for (const edge of graph.edges) {
              edgeTypes[edge.type] = (edgeTypes[edge.type] || 0) + 1;
            }

            for (const [type, count] of Object.entries(edgeTypes)) {
              console.log(`  ${chalk.cyan(type)}: ${count}`);
            }
          }

          // Save graph
          await writeFile(options.output || 'knowledge-graph.json', JSON.stringify(graph, null, 2));

          console.log('');
          console.log(chalk.green(`✓ Graph saved to ${options.output || 'knowledge-graph.json'}`));

        } catch (error) {
          spinner.fail(chalk.red('Graph building failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  )
  .addCommand(
    new Command('export')
      .description('Export a knowledge graph to various formats')
      .argument('<input>', 'Input knowledge graph file (JSON)')
      .option('-f, --format <format>', 'Export format (json, graphml, gexf, dot, csv, cytoscape)', 'graphml')
      .option('-o, --output <file>', 'Output file path')
      .action(async (input: string, options: GraphExportOptions) => {
        const spinner = ora('Loading knowledge graph...').start();

        try {
          // Read input file
          const content = await readFile(input, 'utf-8');
          const graph = JSON.parse(content);

          spinner.text = `Exporting to ${options.format}...`;

          // Export based on format
          let output: string;
          let extension: string;

          switch (options.format) {
            case 'graphml':
              output = exportToGraphML(graph);
              extension = 'graphml';
              break;
            case 'gexf':
              output = exportToGEXF(graph);
              extension = 'gexf';
              break;
            case 'dot':
              output = exportToDOT(graph);
              extension = 'dot';
              break;
            case 'csv':
              output = exportToCSV(graph);
              extension = 'csv';
              break;
            case 'cytoscape':
              output = exportToCytoscape(graph);
              extension = 'json';
              break;
            default:
              output = JSON.stringify(graph, null, 2);
              extension = 'json';
          }

          // Determine output filename
          const outputFile = options.output || `knowledge-graph.${extension}`;

          await writeFile(outputFile, output);

          spinner.succeed(chalk.green(`Exported to ${options.format} format`));

          console.log('');
          console.log(chalk.bold('📁 Export Details'));
          console.log(chalk.gray('─'.repeat(50)));
          console.log(`  Format: ${chalk.cyan(options.format)}`);
          console.log(`  File: ${chalk.cyan(outputFile)}`);
          console.log(`  Nodes: ${chalk.cyan(graph.nodes?.length || 0)}`);
          console.log(`  Edges: ${chalk.cyan(graph.edges?.length || 0)}`);

        } catch (error) {
          spinner.fail(chalk.red('Export failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  )
  .addCommand(
    new Command('analyze')
      .description('Analyze a knowledge graph structure')
      .argument('<input>', 'Input knowledge graph file (JSON)')
      .option('-o, --output <file>', 'Output file for analysis results')
      .option('-f, --format <format>', 'Output format (json, table)', 'table')
      .action(async (input: string, options: GraphAnalyzeOptions) => {
        const spinner = ora('Analyzing knowledge graph...').start();

        try {
          // Read input file
          const content = await readFile(input, 'utf-8');
          const graph = JSON.parse(content);

          spinner.text = 'Computing structural metrics...';

          // Compute analysis
          const analysis = analyzeGraph(graph);

          spinner.succeed(chalk.green('Analysis complete'));

          // Display results
          console.log('');
          console.log(chalk.bold('📊 Structural Analysis'));
          console.log(chalk.gray('─'.repeat(50)));

          const structuralData = [
            ['Metric', 'Value'],
            ['Node Count', analysis.structural.nodeCount.toString()],
            ['Edge Count', analysis.structural.edgeCount.toString()],
            ['Graph Density', `${(analysis.structural.density * 100).toFixed(2)}%`],
            ['Average Degree', analysis.structural.averageDegree.toFixed(2)],
            ['Connected Components', analysis.structural.connectedComponents.toString()],
          ];

          console.log(table(structuralData, {
            border: {
              topBody: `─`, topJoin: `┬`, topLeft: `┌`, topRight: `┐`,
              bottomBody: `─`, bottomJoin: `┴`, bottomLeft: `└`, bottomRight: `┘`,
              bodyLeft: `│`, bodyRight: `│`, bodyJoin: `│`,
              joinBody: `─`, joinLeft: `├`, joinRight: `┤`, joinJoin: `┼`,
            },
          }));

          // Show risks
          if (analysis.risks.orphanedNodes.length > 0 || analysis.risks.circularDependencies.length > 0) {
            console.log('');
            console.log(chalk.bold('⚠️ Identified Risks'));
            console.log(chalk.gray('─'.repeat(50)));

            if (analysis.risks.orphanedNodes.length > 0) {
              console.log(`  ${chalk.yellow('Orphaned Nodes:')} ${analysis.risks.orphanedNodes.join(', ')}`);
            }

            if (analysis.risks.circularDependencies.length > 0) {
              console.log(`  ${chalk.yellow('Circular Dependencies:')} ${analysis.risks.circularDependencies.length} detected`);
            }

            if (analysis.risks.highRiskNodes.length > 0) {
              console.log(`  ${chalk.red('High Risk Nodes:')} ${analysis.risks.highRiskNodes.join(', ')}`);
            }
          }

          // Show recommendations
          if (analysis.recommendations.length > 0) {
            console.log('');
            console.log(chalk.bold('💡 Recommendations'));
            console.log(chalk.gray('─'.repeat(50)));

            for (const rec of analysis.recommendations.slice(0, 5)) {
              const priorityColor = rec.priority === 'high' ? 'red' : rec.priority === 'medium' ? 'yellow' : 'gray';
              console.log(`  ${chalk[priorityColor](`[${rec.priority.toUpperCase()}]`)} ${rec.title}`);
              console.log(chalk.gray(`    ${rec.description}`));
            }
          }

          // Save results if output specified
          if (options.output) {
            await writeFile(options.output, JSON.stringify(analysis, null, 2));
            console.log('');
            console.log(chalk.green(`✓ Analysis saved to ${options.output}`));
          }

        } catch (error) {
          spinner.fail(chalk.red('Analysis failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  )
  .addCommand(
    new Command('search')
      .description('Search for nodes in the knowledge graph')
      .argument('<input>', 'Input knowledge graph file (JSON)')
      .argument('<query>', 'Search query')
      .option('-t, --type <type>', 'Filter by node type')
      .option('-l, --limit <count>', 'Maximum results to show', '10')
      .action(async (input: string, query: string, options: any) => {
        const spinner = ora('Searching knowledge graph...').start();

        try {
          const content = await readFile(input, 'utf-8');
          const graph = JSON.parse(content);

          // Search nodes
          const queryLower = query.toLowerCase();
          let results = (graph.nodes || []).filter((node: any) => {
            const nameMatch = node.name?.toLowerCase().includes(queryLower);
            const descMatch = node.description?.toLowerCase().includes(queryLower);
            const idMatch = node.id?.toLowerCase().includes(queryLower);
            return nameMatch || descMatch || idMatch;
          });

          // Filter by type if specified
          if (options.type) {
            results = results.filter((node: any) => node.type === options.type);
          }

          // Limit results
          const limit = parseInt(options.limit || '10', 10);
          results = results.slice(0, limit);

          spinner.succeed(chalk.green(`Found ${results.length} results`));

          if (results.length === 0) {
            console.log(chalk.yellow('  No nodes found matching your query'));
          } else {
            console.log('');
            const tableData = [
              ['ID', 'Name', 'Type', 'Description'],
              ...results.map((n: any) => [
                n.id,
                n.name,
                n.type,
                (n.description || '').substring(0, 40) + ((n.description?.length || 0) > 40 ? '...' : ''),
              ]),
            ];

            console.log(table(tableData, {
              border: {
                topBody: `─`, topJoin: `┬`, topLeft: `┌`, topRight: `┐`,
                bottomBody: `─`, bottomJoin: `┴`, bottomLeft: `└`, bottomRight: `┘`,
                bodyLeft: `│`, bodyRight: `│`, bodyJoin: `│`,
                joinBody: `─`, joinLeft: `├`, joinRight: `┤`, joinJoin: `┼`,
              },
            }));
          }

        } catch (error) {
          spinner.fail(chalk.red('Search failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  );

// Helper functions
function buildGraphFromAnalysis(data: any, options: GraphBuildOptions): any {
  const nodes: any[] = [];
  const edges: any[] = [];
  const nodeMap = new Map<string, boolean>();

  // Add programs
  const results = data.results || [data];
  for (const result of results) {
    if (result.file) {
      const programId = `prog-${result.file.replace(/[^a-zA-Z0-9]/g, '-')}`;
      if (!nodeMap.has(programId)) {
        nodes.push({
          id: programId,
          type: 'program',
          name: result.file.split('/').pop(),
          properties: { language: result.language || 'unknown' },
        });
        nodeMap.set(programId, true);
      }

      // Add procedures
      for (const proc of result.procedures || []) {
        const procId = `proc-${programId}-${proc.name}`;
        if (!nodeMap.has(procId)) {
          nodes.push({
            id: procId,
            type: 'procedure',
            name: proc.name,
            properties: { complexity: proc.complexity, linesOfCode: proc.endLine - proc.startLine },
          });
          nodeMap.set(procId, true);
          edges.push({ id: `edge-${programId}-${procId}`, source: programId, target: procId, type: 'contains' });
        }
      }

      // Add data structures if requested
      if (options.includeData) {
        for (const ds of result.dataStructures || []) {
          const dsId = `data-${programId}-${ds.name}`;
          if (!nodeMap.has(dsId)) {
            nodes.push({
              id: dsId,
              type: 'data-structure',
              name: ds.name,
              properties: { fields: ds.fields?.length || 0 },
            });
            nodeMap.set(dsId, true);
            edges.push({ id: `edge-${programId}-${dsId}`, source: programId, target: dsId, type: 'uses' });
          }
        }
      }
    }
  }

  // Add business rules if requested
  if (options.includeRules && data.rules) {
    for (const rule of data.rules) {
      const ruleId = `rule-${rule.id || rule.name?.replace(/[^a-zA-Z0-9]/g, '-')}`;
      if (!nodeMap.has(ruleId)) {
        nodes.push({
          id: ruleId,
          type: 'business-rule',
          name: rule.name,
          description: rule.description,
          metadata: { confidence: rule.confidence },
        });
        nodeMap.set(ruleId, true);
      }
    }
  }

  return {
    id: `graph-${Date.now()}`,
    name: options.name,
    createdAt: new Date().toISOString(),
    nodes,
    edges,
    metadata: {
      totalNodes: nodes.length,
      totalEdges: edges.length,
    },
  };
}

function exportToGraphML(graph: any): string {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];

  let xml = `<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <key id="name" for="node" attr.name="name" attr.type="string"/>
  <key id="type" for="node" attr.name="type" attr.type="string"/>
  <key id="edgeType" for="edge" attr.name="type" attr.type="string"/>
  <graph id="G" edgedefault="directed">
`;

  for (const node of nodes) {
    xml += `    <node id="${node.id}">
      <data key="name">${escapeXml(node.name || '')}</data>
      <data key="type">${node.type}</data>
    </node>\n`;
  }

  for (const edge of edges) {
    xml += `    <edge id="${edge.id}" source="${edge.source}" target="${edge.target}">
      <data key="edgeType">${edge.type}</data>
    </edge>\n`;
  }

  xml += `  </graph>
</graphml>`;

  return xml;
}

function exportToGEXF(graph: any): string {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];

  let xml = `<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
  <meta lastmodifieddate="${new Date().toISOString().split('T')[0]}">
    <creator>MigrationPilot</creator>
    <description>${escapeXml(graph.name || 'Knowledge Graph')}</description>
  </meta>
  <graph mode="static" defaultedgetype="directed">
    <attributes class="node">
      <attribute id="0" title="type" type="string"/>
    </attributes>
    <nodes>
`;

  for (const node of nodes) {
    xml += `      <node id="${node.id}" label="${escapeXml(node.name || '')}">
        <attvalues>
          <attvalue for="0" value="${node.type}"/>
        </attvalues>
      </node>\n`;
  }

  xml += `    </nodes>
    <edges>\n`;

  for (let i = 0; i < edges.length; i++) {
    const edge = edges[i];
    xml += `      <edge id="${i}" source="${edge.source}" target="${edge.target}" label="${edge.type}"/>\n`;
  }

  xml += `    </edges>
  </graph>
</gexf>`;

  return xml;
}

function exportToDOT(graph: any): string {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];

  let dot = `digraph "${escapeQuotes(graph.name || 'KnowledgeGraph')}" {\n`;
  dot += `  rankdir=TB;\n`;
  dot += `  node [shape=box];\n\n`;

  // Define node styles by type
  const typeStyles: Record<string, string> = {
    'program': 'shape=component,color=blue',
    'procedure': 'shape=box,color=green',
    'business-rule': 'shape=diamond,color=orange',
    'database': 'shape=cylinder,color=purple',
    'file': 'shape=folder,color=brown',
    'data-structure': 'shape=record,color=gray',
  };

  for (const node of nodes) {
    const style = typeStyles[node.type] || 'shape=box';
    dot += `  "${node.id}" [label="${escapeQuotes(node.name || node.id)}" ${style}];\n`;
  }

  dot += '\n';

  for (const edge of edges) {
    dot += `  "${edge.source}" -> "${edge.target}" [label="${edge.type}"];\n`;
  }

  dot += '}\n';

  return dot;
}

function exportToCSV(graph: any): string {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];

  let csv = '# NODES\n';
  csv += 'id,name,type,description\n';

  for (const node of nodes) {
    csv += `"${node.id}","${escapeCSV(node.name || '')}","${node.type}","${escapeCSV(node.description || '')}"\n`;
  }

  csv += '\n# EDGES\n';
  csv += 'id,source,target,type\n';

  for (const edge of edges) {
    csv += `"${edge.id}","${edge.source}","${edge.target}","${edge.type}"\n`;
  }

  return csv;
}

function exportToCytoscape(graph: any): string {
  const nodes = (graph.nodes || []).map((n: any) => ({
    data: { id: n.id, label: n.name || n.id, type: n.type },
  }));

  const edges = (graph.edges || []).map((e: any) => ({
    data: { id: e.id, source: e.source, target: e.target, label: e.type },
  }));

  return JSON.stringify({ nodes, edges }, null, 2);
}

function analyzeGraph(graph: any): any {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];

  // Build adjacency lists
  const outgoing = new Map<string, string[]>();
  const incoming = new Map<string, string[]>();

  for (const node of nodes) {
    outgoing.set(node.id, []);
    incoming.set(node.id, []);
  }

  for (const edge of edges) {
    outgoing.get(edge.source)?.push(edge.target);
    incoming.get(edge.target)?.push(edge.source);
  }

  // Calculate metrics
  const nodeCount = nodes.length;
  const edgeCount = edges.length;
  const maxEdges = nodeCount * (nodeCount - 1);
  const density = maxEdges > 0 ? edgeCount / maxEdges : 0;

  let totalDegree = 0;
  for (const node of nodes) {
    totalDegree += (outgoing.get(node.id)?.length || 0) + (incoming.get(node.id)?.length || 0);
  }
  const averageDegree = nodeCount > 0 ? totalDegree / nodeCount : 0;

  // Find orphaned nodes
  const orphanedNodes = nodes
    .filter((n: any) => (outgoing.get(n.id)?.length || 0) === 0 && (incoming.get(n.id)?.length || 0) === 0)
    .map((n: any) => n.id);

  // Find connected components (simplified)
  const visited = new Set<string>();
  let connectedComponents = 0;

  for (const node of nodes) {
    if (!visited.has(node.id)) {
      connectedComponents++;
      const queue = [node.id];
      while (queue.length > 0) {
        const current = queue.shift()!;
        if (visited.has(current)) continue;
        visited.add(current);
        for (const neighbor of [...(outgoing.get(current) || []), ...(incoming.get(current) || [])]) {
          if (!visited.has(neighbor)) {
            queue.push(neighbor);
          }
        }
      }
    }
  }

  // Generate recommendations
  const recommendations = [];
  if (orphanedNodes.length > 0) {
    recommendations.push({
      priority: 'medium',
      title: 'Orphaned Nodes Detected',
      description: `${orphanedNodes.length} nodes have no connections. Consider removing or connecting them.`,
    });
  }
  if (density < 0.1) {
    recommendations.push({
      priority: 'low',
      title: 'Sparse Graph',
      description: 'The graph has low connectivity. Ensure all relevant relationships are captured.',
    });
  }
  if (connectedComponents > 1) {
    recommendations.push({
      priority: 'medium',
      title: 'Disconnected Components',
      description: `The graph has ${connectedComponents} separate components. Consider if they should be connected.`,
    });
  }

  return {
    structural: {
      nodeCount,
      edgeCount,
      density,
      averageDegree,
      connectedComponents,
    },
    risks: {
      orphanedNodes,
      circularDependencies: [],
      highRiskNodes: [],
    },
    recommendations,
    analyzedAt: new Date().toISOString(),
  };
}

function escapeXml(str: string): string {
  return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function escapeQuotes(str: string): string {
  return str.replace(/"/g, '\\"');
}

function escapeCSV(str: string): string {
  return str.replace(/"/g, '""');
}

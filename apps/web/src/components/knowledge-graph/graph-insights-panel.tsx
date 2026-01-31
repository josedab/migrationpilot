'use client';

/**
 * Graph Insights Panel
 * 
 * Provides AI-generated insights from the knowledge graph:
 * - Dead code detection
 * - Circular dependency finder
 * - Migration sequence recommendations
 */

import React, { useMemo } from 'react';
import type { GraphNode, GraphEdge, NodeType } from './knowledge-graph-visualization';

interface GraphInsightsPanelProps {
  nodes: GraphNode[];
  edges: GraphEdge[];
  onHighlightPath: (nodeIds: string[]) => void;
  onNodeSelect: (nodeId: string) => void;
}

interface Insight {
  id: string;
  type: 'dead-code' | 'circular-dependency' | 'high-risk' | 'migration-order' | 'orphan';
  severity: 'info' | 'warning' | 'error';
  title: string;
  description: string;
  affectedNodes: string[];
  recommendation?: string;
}

export function GraphInsightsPanel({
  nodes,
  edges,
  onHighlightPath,
  onNodeSelect,
}: GraphInsightsPanelProps) {
  
  // Analyze graph for insights
  const insights = useMemo(() => {
    const results: Insight[] = [];
    
    // 1. Find dead code (nodes with no incoming edges except from programs)
    const deadCodeNodes = findDeadCode(nodes, edges);
    deadCodeNodes.forEach((node, i) => {
      results.push({
        id: `dead-code-${i}`,
        type: 'dead-code',
        severity: 'warning',
        title: `Potentially dead code: ${node.name}`,
        description: `This ${node.type} has no incoming dependencies and may be unused.`,
        affectedNodes: [node.id],
        recommendation: 'Review this code and consider removing it if no longer needed.',
      });
    });

    // 2. Find circular dependencies
    const cycles = findCircularDependencies(nodes, edges);
    cycles.forEach((cycle, i) => {
      const nodeNames = cycle.map(id => nodes.find(n => n.id === id)?.name || id);
      results.push({
        id: `circular-dep-${i}`,
        type: 'circular-dependency',
        severity: 'error',
        title: `Circular dependency detected`,
        description: `Cycle found: ${nodeNames.join(' → ')} → ${nodeNames[0]}`,
        affectedNodes: cycle,
        recommendation: 'Refactor to break the circular dependency. Consider introducing an interface or reorganizing modules.',
      });
    });

    // 3. Find orphan nodes (no connections at all)
    const orphans = findOrphanNodes(nodes, edges);
    orphans.forEach((node, i) => {
      results.push({
        id: `orphan-${i}`,
        type: 'orphan',
        severity: 'info',
        title: `Isolated node: ${node.name}`,
        description: `This ${node.type} has no connections to other parts of the system.`,
        affectedNodes: [node.id],
        recommendation: 'Verify if this is intentional or if relationships are missing.',
      });
    });

    // 4. Find high-risk nodes (many dependencies)
    const highRiskNodes = findHighRiskNodes(nodes, edges);
    highRiskNodes.forEach((item, i) => {
      results.push({
        id: `high-risk-${i}`,
        type: 'high-risk',
        severity: 'warning',
        title: `High-impact node: ${item.node.name}`,
        description: `This node has ${item.dependentCount} dependent nodes. Changes here will have wide impact.`,
        affectedNodes: [item.node.id, ...item.dependents],
        recommendation: 'Migrate this node carefully with comprehensive testing. Consider migrating dependents first.',
      });
    });

    // 5. Generate migration order recommendations
    const migrationOrder = calculateMigrationOrder(nodes, edges);
    if (migrationOrder.length > 0) {
      const phases = groupIntoPhases(migrationOrder, 5);
      phases.forEach((phase, i) => {
        const phaseNodes = phase.map(id => nodes.find(n => n.id === id)?.name || id);
        results.push({
          id: `migration-phase-${i}`,
          type: 'migration-order',
          severity: 'info',
          title: `Migration Phase ${i + 1}`,
          description: `Migrate these ${phase.length} components together: ${phaseNodes.slice(0, 3).join(', ')}${phase.length > 3 ? '...' : ''}`,
          affectedNodes: phase,
          recommendation: 'These nodes have minimal dependencies on later phases and can be migrated safely.',
        });
      });
    }

    return results;
  }, [nodes, edges]);

  const severityIcons: Record<string, string> = {
    info: 'ℹ️',
    warning: '⚠️',
    error: '❌',
  };

  const severityColors: Record<string, string> = {
    info: 'bg-blue-50 border-blue-200',
    warning: 'bg-yellow-50 border-yellow-200',
    error: 'bg-red-50 border-red-200',
  };

  const typeIcons: Record<string, string> = {
    'dead-code': '🗑️',
    'circular-dependency': '🔄',
    'high-risk': '⚡',
    'migration-order': '📋',
    'orphan': '🏝️',
  };

  // Group insights by type
  const groupedInsights = useMemo(() => {
    const groups: Record<string, Insight[]> = {};
    insights.forEach(insight => {
      if (!groups[insight.type]) {
        groups[insight.type] = [];
      }
      groups[insight.type]!.push(insight);
    });
    return groups;
  }, [insights]);

  return (
    <div className="h-full flex flex-col bg-white">
      {/* Header */}
      <div className="p-4 border-b border-gray-200">
        <h2 className="text-lg font-semibold text-gray-900">Graph Insights</h2>
        <p className="text-sm text-gray-500 mt-1">
          {insights.length} insights found • {insights.filter(i => i.severity === 'error').length} errors • {insights.filter(i => i.severity === 'warning').length} warnings
        </p>
      </div>

      {/* Summary Stats */}
      <div className="p-4 border-b border-gray-200 grid grid-cols-2 gap-4">
        <div className="bg-gray-50 rounded-lg p-3">
          <div className="text-2xl font-bold text-gray-900">{nodes.length}</div>
          <div className="text-xs text-gray-500">Total Nodes</div>
        </div>
        <div className="bg-gray-50 rounded-lg p-3">
          <div className="text-2xl font-bold text-gray-900">{edges.length}</div>
          <div className="text-xs text-gray-500">Total Edges</div>
        </div>
      </div>

      {/* Insights List */}
      <div className="flex-1 overflow-y-auto p-4 space-y-6">
        {Object.entries(groupedInsights).map(([type, typeInsights]) => (
          <div key={type}>
            <h3 className="text-sm font-medium text-gray-700 mb-3 flex items-center gap-2">
              <span>{typeIcons[type]}</span>
              <span className="capitalize">{type.replace('-', ' ')}</span>
              <span className="text-gray-400">({typeInsights.length})</span>
            </h3>
            <div className="space-y-3">
              {typeInsights.slice(0, 5).map(insight => (
                <div
                  key={insight.id}
                  className={`p-3 rounded-lg border ${severityColors[insight.severity]}`}
                >
                  <div className="flex items-start gap-2">
                    <span>{severityIcons[insight.severity]}</span>
                    <div className="flex-1">
                      <h4 className="text-sm font-medium text-gray-900">{insight.title}</h4>
                      <p className="text-xs text-gray-600 mt-1">{insight.description}</p>
                      {insight.recommendation && (
                        <p className="text-xs text-gray-500 mt-2 italic">
                          💡 {insight.recommendation}
                        </p>
                      )}
                      <div className="flex gap-2 mt-3">
                        <button
                          onClick={() => onHighlightPath(insight.affectedNodes)}
                          className="text-xs px-2 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200"
                        >
                          Highlight
                        </button>
                        {insight.affectedNodes.length === 1 && (
                          <button
                            onClick={() => onNodeSelect(insight.affectedNodes[0]!)}
                            className="text-xs px-2 py-1 bg-gray-100 text-gray-700 rounded hover:bg-gray-200"
                          >
                            View Details
                          </button>
                        )}
                      </div>
                    </div>
                  </div>
                </div>
              ))}
              {typeInsights.length > 5 && (
                <p className="text-xs text-gray-500 text-center">
                  +{typeInsights.length - 5} more {type.replace('-', ' ')} insights
                </p>
              )}
            </div>
          </div>
        ))}

        {insights.length === 0 && (
          <div className="text-center py-8 text-gray-500">
            <p className="text-4xl mb-2">✨</p>
            <p className="text-sm">No issues detected!</p>
            <p className="text-xs">Your codebase graph looks healthy.</p>
          </div>
        )}
      </div>
    </div>
  );
}

// Helper functions for graph analysis

function findDeadCode(nodes: GraphNode[], edges: GraphEdge[]): GraphNode[] {
  const nodesWithIncoming = new Set(edges.map(e => e.target));
  return nodes.filter(node => 
    !nodesWithIncoming.has(node.id) && 
    node.type !== 'program' &&
    node.type !== 'regulation' &&
    node.type !== 'concept'
  );
}

function findCircularDependencies(nodes: GraphNode[], edges: GraphEdge[]): string[][] {
  const cycles: string[][] = [];
  const visited = new Set<string>();
  const recursionStack = new Set<string>();
  const path: string[] = [];

  const adjacencyList = new Map<string, string[]>();
  edges.forEach(edge => {
    if (!adjacencyList.has(edge.source)) {
      adjacencyList.set(edge.source, []);
    }
    adjacencyList.get(edge.source)!.push(edge.target);
  });

  function dfs(nodeId: string): boolean {
    visited.add(nodeId);
    recursionStack.add(nodeId);
    path.push(nodeId);

    const neighbors = adjacencyList.get(nodeId) || [];
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        if (dfs(neighbor)) return true;
      } else if (recursionStack.has(neighbor)) {
        // Found a cycle
        const cycleStart = path.indexOf(neighbor);
        if (cycleStart !== -1) {
          cycles.push(path.slice(cycleStart));
        }
      }
    }

    path.pop();
    recursionStack.delete(nodeId);
    return false;
  }

  nodes.forEach(node => {
    if (!visited.has(node.id)) {
      dfs(node.id);
    }
  });

  return cycles.slice(0, 5); // Limit to 5 cycles
}

function findOrphanNodes(nodes: GraphNode[], edges: GraphEdge[]): GraphNode[] {
  const connectedNodes = new Set<string>();
  edges.forEach(edge => {
    connectedNodes.add(edge.source);
    connectedNodes.add(edge.target);
  });
  return nodes.filter(node => !connectedNodes.has(node.id));
}

function findHighRiskNodes(nodes: GraphNode[], edges: GraphEdge[]): { node: GraphNode; dependentCount: number; dependents: string[] }[] {
  const dependentCounts = new Map<string, Set<string>>();
  
  // Count how many nodes depend on each node (incoming edges)
  edges.forEach(edge => {
    if (!dependentCounts.has(edge.target)) {
      dependentCounts.set(edge.target, new Set());
    }
    dependentCounts.get(edge.target)!.add(edge.source);
  });

  const threshold = Math.max(5, nodes.length * 0.1); // Top 10% or at least 5 dependents
  
  return nodes
    .map(node => ({
      node,
      dependentCount: dependentCounts.get(node.id)?.size || 0,
      dependents: Array.from(dependentCounts.get(node.id) || []),
    }))
    .filter(item => item.dependentCount >= threshold)
    .sort((a, b) => b.dependentCount - a.dependentCount)
    .slice(0, 5);
}

function calculateMigrationOrder(nodes: GraphNode[], edges: GraphEdge[]): string[] {
  // Topological sort to determine migration order
  const inDegree = new Map<string, number>();
  const adjacencyList = new Map<string, string[]>();

  nodes.forEach(node => {
    inDegree.set(node.id, 0);
    adjacencyList.set(node.id, []);
  });

  edges.forEach(edge => {
    inDegree.set(edge.target, (inDegree.get(edge.target) || 0) + 1);
    adjacencyList.get(edge.source)?.push(edge.target);
  });

  const queue: string[] = [];
  const result: string[] = [];

  // Start with nodes that have no dependencies
  nodes.forEach(node => {
    if (inDegree.get(node.id) === 0) {
      queue.push(node.id);
    }
  });

  while (queue.length > 0) {
    const nodeId = queue.shift()!;
    result.push(nodeId);

    const neighbors = adjacencyList.get(nodeId) || [];
    neighbors.forEach(neighbor => {
      inDegree.set(neighbor, (inDegree.get(neighbor) || 0) - 1);
      if (inDegree.get(neighbor) === 0) {
        queue.push(neighbor);
      }
    });
  }

  return result;
}

function groupIntoPhases(order: string[], maxPhaseSize: number): string[][] {
  const phases: string[][] = [];
  for (let i = 0; i < order.length; i += maxPhaseSize) {
    phases.push(order.slice(i, i + maxPhaseSize));
  }
  return phases;
}

export default GraphInsightsPanel;

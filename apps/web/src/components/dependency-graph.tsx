'use client';

/**
 * Dependency Graph Component
 * 
 * Visualizes module dependencies as an interactive DAG
 * Uses SVG for rendering (can be enhanced with D3.js or React Flow)
 */

import React, { useState, useMemo } from 'react';

interface DependencyNode {
  id: string;
  name: string;
  type: 'program' | 'copybook' | 'file' | 'database' | 'service' | 'module';
  status: 'pending' | 'analyzing' | 'migrating' | 'testing' | 'completed' | 'failed';
  riskLevel: 'low' | 'medium' | 'high' | 'critical';
  progress: number;
  metrics: {
    linesOfCode: number;
    businessRules: number;
    complexity: number;
    coverage: number;
  };
}

interface DependencyEdge {
  source: string;
  target: string;
  type: 'calls' | 'includes' | 'reads' | 'writes' | 'depends';
}

interface DependencyGraphProps {
  nodes: DependencyNode[];
  edges: DependencyEdge[];
  onNodeClick?: (node: DependencyNode) => void;
  selectedNodeId?: string;
}

const statusColors: Record<string, { bg: string; border: string; text: string }> = {
  pending: { bg: '#f3f4f6', border: '#d1d5db', text: '#6b7280' },
  analyzing: { bg: '#fef3c7', border: '#f59e0b', text: '#92400e' },
  migrating: { bg: '#dbeafe', border: '#3b82f6', text: '#1e40af' },
  testing: { bg: '#e0e7ff', border: '#6366f1', text: '#3730a3' },
  completed: { bg: '#d1fae5', border: '#10b981', text: '#065f46' },
  failed: { bg: '#fee2e2', border: '#ef4444', text: '#991b1b' },
};

const riskColors: Record<string, string> = {
  low: '#10b981',
  medium: '#f59e0b',
  high: '#f97316',
  critical: '#ef4444',
};

const typeIcons: Record<string, string> = {
  program: '📄',
  copybook: '📋',
  file: '📁',
  database: '🗄️',
  service: '⚙️',
  module: '📦',
};

export function DependencyGraph({
  nodes,
  edges,
  onNodeClick,
  selectedNodeId,
}: DependencyGraphProps) {
  const [hoveredNode, setHoveredNode] = useState<string | null>(null);
  const [filter, setFilter] = useState<string>('all');

  // Calculate positions using a simple layered layout
  const { nodePositions, svgWidth, svgHeight } = useMemo(() => {
    const positions: Record<string, { x: number; y: number }> = {};
    
    // Group nodes by layer (based on dependencies)
    const layers: string[][] = [];
    const processed = new Set<string>();
    
    // Find root nodes (no incoming edges)
    const hasIncoming = new Set(edges.map(e => e.target));
    const roots = nodes.filter(n => !hasIncoming.has(n.id)).map(n => n.id);
    
    if (roots.length > 0) {
      layers.push(roots);
      roots.forEach(id => processed.add(id));
    }
    
    // Build layers
    while (processed.size < nodes.length) {
      const nextLayer: string[] = [];
      for (const node of nodes) {
        if (processed.has(node.id)) continue;
        
        // Check if all dependencies are processed
        const deps = edges.filter(e => e.target === node.id).map(e => e.source);
        if (deps.every(d => processed.has(d))) {
          nextLayer.push(node.id);
        }
      }
      
      if (nextLayer.length === 0) {
        // Handle cycles by adding remaining nodes
        const remaining = nodes.filter(n => !processed.has(n.id)).map(n => n.id);
        layers.push(remaining);
        remaining.forEach(id => processed.add(id));
      } else {
        layers.push(nextLayer);
        nextLayer.forEach(id => processed.add(id));
      }
    }
    
    // Calculate positions
    const nodeWidth = 180;
    const nodeHeight = 80;
    const horizontalGap = 50;
    const verticalGap = 100;
    
    let maxLayerWidth = 0;
    layers.forEach((layer, layerIndex) => {
      const layerWidth = layer.length * (nodeWidth + horizontalGap);
      maxLayerWidth = Math.max(maxLayerWidth, layerWidth);
      
      layer.forEach((nodeId, nodeIndex) => {
        const x = (nodeIndex + 0.5) * (nodeWidth + horizontalGap) + 
                  (maxLayerWidth - layerWidth) / 2 + 20;
        const y = layerIndex * (nodeHeight + verticalGap) + 40;
        positions[nodeId] = { x, y };
      });
    });
    
    return {
      nodePositions: positions,
      svgWidth: maxLayerWidth + 100,
      svgHeight: layers.length * (nodeHeight + verticalGap) + 40,
    };
  }, [nodes, edges]);

  const filteredNodes = filter === 'all' 
    ? nodes 
    : nodes.filter(n => n.status === filter || n.riskLevel === filter);

  const filteredNodeIds = new Set(filteredNodes.map(n => n.id));
  const filteredEdges = edges.filter(
    e => filteredNodeIds.has(e.source) && filteredNodeIds.has(e.target)
  );

  return (
    <div className="bg-white rounded-lg shadow-md">
      {/* Filter Controls */}
      <div className="p-4 border-b border-gray-200">
        <div className="flex items-center gap-4">
          <span className="text-sm font-medium text-gray-700">Filter:</span>
          <select
            value={filter}
            onChange={(e) => setFilter(e.target.value)}
            className="text-sm border border-gray-300 rounded-md px-3 py-1"
          >
            <option value="all">All Nodes</option>
            <optgroup label="By Status">
              <option value="pending">Pending</option>
              <option value="analyzing">Analyzing</option>
              <option value="migrating">Migrating</option>
              <option value="testing">Testing</option>
              <option value="completed">Completed</option>
              <option value="failed">Failed</option>
            </optgroup>
            <optgroup label="By Risk">
              <option value="low">Low Risk</option>
              <option value="medium">Medium Risk</option>
              <option value="high">High Risk</option>
              <option value="critical">Critical Risk</option>
            </optgroup>
          </select>
          <div className="ml-auto flex items-center gap-4 text-xs">
            {Object.entries(statusColors).map(([status, colors]) => (
              <div key={status} className="flex items-center gap-1">
                <div 
                  className="w-3 h-3 rounded" 
                  style={{ backgroundColor: colors.bg, border: `1px solid ${colors.border}` }}
                />
                <span className="capitalize">{status}</span>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Graph SVG */}
      <div className="overflow-auto p-4">
        <svg 
          width={svgWidth} 
          height={svgHeight}
          className="mx-auto"
        >
          {/* Edges */}
          <defs>
            <marker
              id="arrowhead"
              markerWidth="10"
              markerHeight="7"
              refX="9"
              refY="3.5"
              orient="auto"
            >
              <polygon points="0 0, 10 3.5, 0 7" fill="#9ca3af" />
            </marker>
          </defs>
          
          {filteredEdges.map((edge, i) => {
            const source = nodePositions[edge.source];
            const target = nodePositions[edge.target];
            if (!source || !target) return null;
            
            const isHighlighted = hoveredNode === edge.source || hoveredNode === edge.target;
            
            return (
              <g key={i}>
                <line
                  x1={source.x + 90}
                  y1={source.y + 40}
                  x2={target.x + 90}
                  y2={target.y}
                  stroke={isHighlighted ? '#3b82f6' : '#9ca3af'}
                  strokeWidth={isHighlighted ? 2 : 1}
                  markerEnd="url(#arrowhead)"
                  opacity={isHighlighted ? 1 : 0.5}
                />
                <text
                  x={(source.x + target.x) / 2 + 90}
                  y={(source.y + target.y) / 2 + 20}
                  fontSize="10"
                  fill="#6b7280"
                  textAnchor="middle"
                >
                  {edge.type}
                </text>
              </g>
            );
          })}

          {/* Nodes */}
          {filteredNodes.map((node) => {
            const pos = nodePositions[node.id];
            if (!pos) return null;
            
            const colors = statusColors[node.status];
            const isSelected = selectedNodeId === node.id;
            const isHovered = hoveredNode === node.id;
            
            return (
              <g
                key={node.id}
                transform={`translate(${pos.x}, ${pos.y})`}
                onClick={() => onNodeClick?.(node)}
                onMouseEnter={() => setHoveredNode(node.id)}
                onMouseLeave={() => setHoveredNode(null)}
                style={{ cursor: 'pointer' }}
              >
                {/* Node background */}
                <rect
                  width="180"
                  height="80"
                  rx="8"
                  fill={colors.bg}
                  stroke={isSelected ? '#3b82f6' : colors.border}
                  strokeWidth={isSelected || isHovered ? 3 : 1}
                />
                
                {/* Risk indicator */}
                <circle
                  cx="165"
                  cy="15"
                  r="8"
                  fill={riskColors[node.riskLevel]}
                />
                
                {/* Type icon and name */}
                <text x="10" y="25" fontSize="14" fill={colors.text}>
                  {typeIcons[node.type]} {node.name.length > 15 ? node.name.slice(0, 12) + '...' : node.name}
                </text>
                
                {/* Status */}
                <text x="10" y="45" fontSize="11" fill={colors.text} opacity={0.8}>
                  {node.status.charAt(0).toUpperCase() + node.status.slice(1)}
                </text>
                
                {/* Progress bar */}
                <rect x="10" y="55" width="160" height="6" rx="3" fill="#e5e7eb" />
                <rect 
                  x="10" 
                  y="55" 
                  width={Math.max(0, Math.min(160, node.progress * 1.6))} 
                  height="6" 
                  rx="3" 
                  fill={colors.border}
                />
                <text x="10" y="72" fontSize="10" fill={colors.text}>
                  {node.progress}%
                </text>
                <text x="155" y="72" fontSize="10" fill={colors.text} textAnchor="end">
                  {node.metrics.linesOfCode} LOC
                </text>
              </g>
            );
          })}
        </svg>
      </div>

      {/* Legend */}
      <div className="p-4 border-t border-gray-200">
        <div className="flex items-center justify-center gap-6 text-xs text-gray-600">
          <div className="flex items-center gap-2">
            <span>Risk Level:</span>
            {Object.entries(riskColors).map(([level, color]) => (
              <div key={level} className="flex items-center gap-1">
                <div className="w-3 h-3 rounded-full" style={{ backgroundColor: color }} />
                <span className="capitalize">{level}</span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}

export default DependencyGraph;

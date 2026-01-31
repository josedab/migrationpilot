'use client';

/**
 * Knowledge Graph Visualization
 * 
 * Interactive visualization of the codebase knowledge graph with:
 * - Force-directed layout
 * - Filtering and search
 * - Drill-down navigation
 * - Impact analysis visualization
 */

import React, { useState, useCallback, useMemo, useEffect, useRef } from 'react';

// Types matching the core KnowledgeGraph types
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

export interface GraphNode {
  id: string;
  type: NodeType;
  name: string;
  description?: string;
  properties: Record<string, unknown>;
  metadata: {
    source?: { file: string; startLine: number; endLine: number };
    confidence?: number;
    tags?: string[];
  };
  // Computed for visualization
  x?: number;
  y?: number;
  vx?: number;
  vy?: number;
  fx?: number | null;
  fy?: number | null;
}

export interface GraphEdge {
  id: string;
  source: string;
  target: string;
  type: EdgeType;
  weight: number;
  confidence: number;
}

export interface KnowledgeGraphVisualizationProps {
  nodes: GraphNode[];
  edges: GraphEdge[];
  width?: number;
  height?: number;
  onNodeClick?: (node: GraphNode) => void;
  onNodeDoubleClick?: (node: GraphNode) => void;
  onEdgeClick?: (edge: GraphEdge) => void;
  selectedNodeId?: string;
  highlightedPath?: string[];
}

// Node type colors
const nodeColors: Record<NodeType, { fill: string; stroke: string; text: string }> = {
  'program': { fill: '#dbeafe', stroke: '#3b82f6', text: '#1e40af' },
  'procedure': { fill: '#e0e7ff', stroke: '#6366f1', text: '#3730a3' },
  'business-rule': { fill: '#fef3c7', stroke: '#f59e0b', text: '#92400e' },
  'data-structure': { fill: '#d1fae5', stroke: '#10b981', text: '#065f46' },
  'file': { fill: '#f3f4f6', stroke: '#6b7280', text: '#374151' },
  'database': { fill: '#fce7f3', stroke: '#ec4899', text: '#9d174d' },
  'external-system': { fill: '#fee2e2', stroke: '#ef4444', text: '#991b1b' },
  'regulation': { fill: '#fef2f2', stroke: '#dc2626', text: '#7f1d1d' },
  'concept': { fill: '#f5f3ff', stroke: '#8b5cf6', text: '#5b21b6' },
};

// Node type icons
const nodeIcons: Record<NodeType, string> = {
  'program': '📄',
  'procedure': '⚡',
  'business-rule': '📋',
  'data-structure': '📊',
  'file': '📁',
  'database': '🗄️',
  'external-system': '🔗',
  'regulation': '⚖️',
  'concept': '💡',
};

// Edge type styles
const edgeStyles: Record<EdgeType, { color: string; dash: string }> = {
  'calls': { color: '#3b82f6', dash: '' },
  'uses': { color: '#10b981', dash: '' },
  'modifies': { color: '#f59e0b', dash: '' },
  'reads': { color: '#6b7280', dash: '5,5' },
  'writes': { color: '#ef4444', dash: '' },
  'depends-on': { color: '#6366f1', dash: '10,5' },
  'implements': { color: '#8b5cf6', dash: '' },
  'affected-by': { color: '#ec4899', dash: '3,3' },
  'contains': { color: '#374151', dash: '' },
  'related-to': { color: '#9ca3af', dash: '2,2' },
};

export function KnowledgeGraphVisualization({
  nodes,
  edges,
  width = 1200,
  height = 800,
  onNodeClick,
  onNodeDoubleClick,
  onEdgeClick,
  selectedNodeId,
  highlightedPath = [],
}: KnowledgeGraphVisualizationProps) {
  const svgRef = useRef<SVGSVGElement>(null);
  const [hoveredNode, setHoveredNode] = useState<string | null>(null);
  const [hoveredEdge, setHoveredEdge] = useState<string | null>(null);
  const [draggedNode, setDraggedNode] = useState<string | null>(null);
  const [transform, setTransform] = useState({ x: 0, y: 0, scale: 1 });
  const [filter, setFilter] = useState<{
    nodeTypes: NodeType[];
    edgeTypes: EdgeType[];
    searchTerm: string;
    confidenceMin: number;
  }>({
    nodeTypes: [],
    edgeTypes: [],
    searchTerm: '',
    confidenceMin: 0,
  });

  // Initialize node positions with force-directed layout simulation
  const [nodePositions, setNodePositions] = useState<Map<string, { x: number; y: number }>>(() => {
    const positions = new Map<string, { x: number; y: number }>();
    nodes.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / nodes.length;
      const radius = Math.min(width, height) / 3;
      positions.set(node.id, {
        x: width / 2 + radius * Math.cos(angle),
        y: height / 2 + radius * Math.sin(angle),
      });
    });
    return positions;
  });

  // Simple force-directed layout simulation
  useEffect(() => {
    const positions = new Map(nodePositions);
    const velocities = new Map<string, { vx: number; vy: number }>();
    
    nodes.forEach(node => {
      velocities.set(node.id, { vx: 0, vy: 0 });
    });

    const simulate = () => {
      const alpha = 0.1;
      const repulsionStrength = 5000;
      const attractionStrength = 0.05;
      const damping = 0.9;

      // Apply forces
      nodes.forEach(node1 => {
        const pos1 = positions.get(node1.id)!;
        const vel1 = velocities.get(node1.id)!;

        // Repulsion from other nodes
        nodes.forEach(node2 => {
          if (node1.id === node2.id) return;
          const pos2 = positions.get(node2.id)!;
          const dx = pos1.x - pos2.x;
          const dy = pos1.y - pos2.y;
          const distance = Math.sqrt(dx * dx + dy * dy) || 1;
          const force = repulsionStrength / (distance * distance);
          vel1.vx += (dx / distance) * force * alpha;
          vel1.vy += (dy / distance) * force * alpha;
        });

        // Center gravity
        vel1.vx += (width / 2 - pos1.x) * 0.001;
        vel1.vy += (height / 2 - pos1.y) * 0.001;
      });

      // Attraction along edges
      edges.forEach(edge => {
        const sourcePos = positions.get(edge.source);
        const targetPos = positions.get(edge.target);
        if (!sourcePos || !targetPos) return;

        const sourceVel = velocities.get(edge.source);
        const targetVel = velocities.get(edge.target);
        if (!sourceVel || !targetVel) return;

        const dx = targetPos.x - sourcePos.x;
        const dy = targetPos.y - sourcePos.y;
        const distance = Math.sqrt(dx * dx + dy * dy) || 1;
        
        sourceVel.vx += dx * attractionStrength * alpha;
        sourceVel.vy += dy * attractionStrength * alpha;
        targetVel.vx -= dx * attractionStrength * alpha;
        targetVel.vy -= dy * attractionStrength * alpha;
      });

      // Apply velocities and damping
      let maxVelocity = 0;
      nodes.forEach(node => {
        const pos = positions.get(node.id)!;
        const vel = velocities.get(node.id)!;
        
        vel.vx *= damping;
        vel.vy *= damping;
        
        pos.x += vel.vx;
        pos.y += vel.vy;
        
        // Keep within bounds
        pos.x = Math.max(50, Math.min(width - 50, pos.x));
        pos.y = Math.max(50, Math.min(height - 50, pos.y));

        maxVelocity = Math.max(maxVelocity, Math.abs(vel.vx), Math.abs(vel.vy));
      });

      setNodePositions(new Map(positions));
      
      // Continue simulation if not settled
      if (maxVelocity > 0.1) {
        requestAnimationFrame(simulate);
      }
    };

    // Run simulation for a few iterations
    let iterations = 0;
    const runSimulation = () => {
      simulate();
      iterations++;
      if (iterations < 100) {
        requestAnimationFrame(runSimulation);
      }
    };
    runSimulation();
  }, [nodes.length, edges.length, width, height]);

  // Filter nodes and edges
  const filteredNodes = useMemo(() => {
    return nodes.filter(node => {
      if (filter.nodeTypes.length > 0 && !filter.nodeTypes.includes(node.type)) {
        return false;
      }
      if (filter.searchTerm && !node.name.toLowerCase().includes(filter.searchTerm.toLowerCase())) {
        return false;
      }
      if (filter.confidenceMin > 0 && (node.metadata.confidence || 0) < filter.confidenceMin) {
        return false;
      }
      return true;
    });
  }, [nodes, filter]);

  const filteredNodeIds = useMemo(() => new Set(filteredNodes.map(n => n.id)), [filteredNodes]);

  const filteredEdges = useMemo(() => {
    return edges.filter(edge => {
      if (!filteredNodeIds.has(edge.source) || !filteredNodeIds.has(edge.target)) {
        return false;
      }
      if (filter.edgeTypes.length > 0 && !filter.edgeTypes.includes(edge.type)) {
        return false;
      }
      return true;
    });
  }, [edges, filteredNodeIds, filter.edgeTypes]);

  // Drag handling
  const handleMouseDown = useCallback((nodeId: string, e: React.MouseEvent) => {
    e.preventDefault();
    setDraggedNode(nodeId);
  }, []);

  const handleMouseMove = useCallback((e: React.MouseEvent) => {
    if (!draggedNode || !svgRef.current) return;
    
    const rect = svgRef.current.getBoundingClientRect();
    const x = (e.clientX - rect.left - transform.x) / transform.scale;
    const y = (e.clientY - rect.top - transform.y) / transform.scale;
    
    setNodePositions(prev => {
      const next = new Map(prev);
      next.set(draggedNode, { x, y });
      return next;
    });
  }, [draggedNode, transform]);

  const handleMouseUp = useCallback(() => {
    setDraggedNode(null);
  }, []);

  // Zoom handling
  const handleWheel = useCallback((e: React.WheelEvent) => {
    e.preventDefault();
    const scaleDelta = e.deltaY > 0 ? 0.9 : 1.1;
    setTransform(prev => ({
      ...prev,
      scale: Math.max(0.1, Math.min(3, prev.scale * scaleDelta)),
    }));
  }, []);

  // Pan handling
  const [isPanning, setIsPanning] = useState(false);
  const [panStart, setPanStart] = useState({ x: 0, y: 0 });

  const handlePanStart = useCallback((e: React.MouseEvent) => {
    if (draggedNode) return;
    setIsPanning(true);
    setPanStart({ x: e.clientX - transform.x, y: e.clientY - transform.y });
  }, [draggedNode, transform]);

  const handlePanMove = useCallback((e: React.MouseEvent) => {
    if (!isPanning) return;
    setTransform(prev => ({
      ...prev,
      x: e.clientX - panStart.x,
      y: e.clientY - panStart.y,
    }));
  }, [isPanning, panStart]);

  const handlePanEnd = useCallback(() => {
    setIsPanning(false);
  }, []);

  // Calculate edge path
  const getEdgePath = (edge: GraphEdge): string => {
    const sourcePos = nodePositions.get(edge.source);
    const targetPos = nodePositions.get(edge.target);
    if (!sourcePos || !targetPos) return '';

    const dx = targetPos.x - sourcePos.x;
    const dy = targetPos.y - sourcePos.y;
    const distance = Math.sqrt(dx * dx + dy * dy);
    
    // Shorten path to not overlap with nodes
    const nodeRadius = 40;
    const startX = sourcePos.x + (dx / distance) * nodeRadius;
    const startY = sourcePos.y + (dy / distance) * nodeRadius;
    const endX = targetPos.x - (dx / distance) * nodeRadius;
    const endY = targetPos.y - (dy / distance) * nodeRadius;

    // Curved path
    const midX = (startX + endX) / 2;
    const midY = (startY + endY) / 2;
    const curveOffset = distance * 0.1;
    const controlX = midX - (dy / distance) * curveOffset;
    const controlY = midY + (dx / distance) * curveOffset;

    return `M ${startX} ${startY} Q ${controlX} ${controlY} ${endX} ${endY}`;
  };

  const isInHighlightedPath = (nodeId: string) => highlightedPath.includes(nodeId);
  const isEdgeInHighlightedPath = (edge: GraphEdge) => 
    highlightedPath.includes(edge.source) && highlightedPath.includes(edge.target);

  return (
    <div className="bg-white rounded-lg shadow-lg overflow-hidden">
      {/* Controls */}
      <div className="p-4 border-b border-gray-200 space-y-3">
        <div className="flex items-center gap-4 flex-wrap">
          {/* Search */}
          <div className="flex-1 min-w-[200px]">
            <input
              type="text"
              placeholder="Search nodes..."
              value={filter.searchTerm}
              onChange={(e) => setFilter(prev => ({ ...prev, searchTerm: e.target.value }))}
              className="w-full px-3 py-2 border border-gray-300 rounded-md text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            />
          </div>

          {/* Node type filter */}
          <select
            multiple
            value={filter.nodeTypes}
            onChange={(e) => {
              const values = Array.from(e.target.selectedOptions, option => option.value as NodeType);
              setFilter(prev => ({ ...prev, nodeTypes: values }));
            }}
            className="px-3 py-2 border border-gray-300 rounded-md text-sm"
          >
            <option value="">All Node Types</option>
            {Object.keys(nodeColors).map(type => (
              <option key={type} value={type}>{nodeIcons[type as NodeType]} {type}</option>
            ))}
          </select>

          {/* Confidence slider */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-gray-600">Min Confidence:</span>
            <input
              type="range"
              min="0"
              max="1"
              step="0.1"
              value={filter.confidenceMin}
              onChange={(e) => setFilter(prev => ({ ...prev, confidenceMin: parseFloat(e.target.value) }))}
              className="w-24"
            />
            <span className="text-sm text-gray-600">{(filter.confidenceMin * 100).toFixed(0)}%</span>
          </div>

          {/* Reset view */}
          <button
            onClick={() => setTransform({ x: 0, y: 0, scale: 1 })}
            className="px-3 py-2 bg-gray-100 hover:bg-gray-200 rounded-md text-sm"
          >
            Reset View
          </button>
        </div>

        {/* Stats */}
        <div className="flex items-center gap-6 text-sm text-gray-600">
          <span>Nodes: {filteredNodes.length}/{nodes.length}</span>
          <span>Edges: {filteredEdges.length}/{edges.length}</span>
          <span>Zoom: {(transform.scale * 100).toFixed(0)}%</span>
        </div>
      </div>

      {/* Graph Canvas */}
      <svg
        ref={svgRef}
        width={width}
        height={height}
        onMouseMove={(e) => { handleMouseMove(e); handlePanMove(e); }}
        onMouseUp={() => { handleMouseUp(); handlePanEnd(); }}
        onMouseLeave={() => { handleMouseUp(); handlePanEnd(); }}
        onMouseDown={handlePanStart}
        onWheel={handleWheel}
        style={{ cursor: isPanning ? 'grabbing' : 'grab' }}
      >
        <defs>
          {/* Arrow markers for each edge type */}
          {Object.entries(edgeStyles).map(([type, style]) => (
            <marker
              key={type}
              id={`arrow-${type}`}
              viewBox="0 0 10 10"
              refX="9"
              refY="5"
              markerWidth="6"
              markerHeight="6"
              orient="auto-start-reverse"
            >
              <path d="M 0 0 L 10 5 L 0 10 z" fill={style.color} />
            </marker>
          ))}
        </defs>

        <g transform={`translate(${transform.x}, ${transform.y}) scale(${transform.scale})`}>
          {/* Edges */}
          {filteredEdges.map((edge) => {
            const style = edgeStyles[edge.type];
            const isHovered = hoveredEdge === edge.id;
            const isHighlighted = isEdgeInHighlightedPath(edge);
            const isConnectedToHovered = hoveredNode === edge.source || hoveredNode === edge.target;
            
            return (
              <g key={edge.id}>
                <path
                  d={getEdgePath(edge)}
                  fill="none"
                  stroke={isHighlighted ? '#ef4444' : style.color}
                  strokeWidth={isHovered || isHighlighted ? 3 : isConnectedToHovered ? 2 : 1}
                  strokeDasharray={style.dash}
                  opacity={isHovered || isHighlighted || isConnectedToHovered ? 1 : 0.5}
                  markerEnd={`url(#arrow-${edge.type})`}
                  onMouseEnter={() => setHoveredEdge(edge.id)}
                  onMouseLeave={() => setHoveredEdge(null)}
                  onClick={() => onEdgeClick?.(edge)}
                  style={{ cursor: 'pointer' }}
                />
              </g>
            );
          })}

          {/* Nodes */}
          {filteredNodes.map((node) => {
            const pos = nodePositions.get(node.id);
            if (!pos) return null;

            const colors = nodeColors[node.type];
            const isSelected = selectedNodeId === node.id;
            const isHovered = hoveredNode === node.id;
            const isHighlighted = isInHighlightedPath(node.id);
            const confidence = node.metadata.confidence || 1;
            
            return (
              <g
                key={node.id}
                transform={`translate(${pos.x}, ${pos.y})`}
                onMouseEnter={() => setHoveredNode(node.id)}
                onMouseLeave={() => setHoveredNode(null)}
                onMouseDown={(e) => handleMouseDown(node.id, e)}
                onClick={() => onNodeClick?.(node)}
                onDoubleClick={() => onNodeDoubleClick?.(node)}
                style={{ cursor: draggedNode === node.id ? 'grabbing' : 'pointer' }}
              >
                {/* Selection ring */}
                {(isSelected || isHighlighted) && (
                  <circle
                    r="48"
                    fill="none"
                    stroke={isHighlighted ? '#ef4444' : '#3b82f6'}
                    strokeWidth="3"
                    opacity="0.5"
                  />
                )}
                
                {/* Node circle */}
                <circle
                  r="40"
                  fill={colors.fill}
                  stroke={colors.stroke}
                  strokeWidth={isHovered ? 3 : 2}
                  opacity={confidence}
                />
                
                {/* Confidence indicator ring */}
                <circle
                  r="44"
                  fill="none"
                  stroke={colors.stroke}
                  strokeWidth="2"
                  strokeDasharray={`${confidence * 276} 276`}
                  transform="rotate(-90)"
                  opacity="0.3"
                />
                
                {/* Icon */}
                <text
                  textAnchor="middle"
                  dominantBaseline="middle"
                  fontSize="24"
                  y="-5"
                >
                  {nodeIcons[node.type]}
                </text>
                
                {/* Name */}
                <text
                  textAnchor="middle"
                  dominantBaseline="middle"
                  fontSize="10"
                  fill={colors.text}
                  y="18"
                  fontWeight="500"
                >
                  {node.name.length > 12 ? node.name.slice(0, 10) + '...' : node.name}
                </text>
                
                {/* Tags indicator */}
                {node.metadata.tags && node.metadata.tags.length > 0 && (
                  <circle
                    cx="28"
                    cy="-28"
                    r="10"
                    fill="#8b5cf6"
                    stroke="white"
                    strokeWidth="2"
                  />
                )}
                {node.metadata.tags && node.metadata.tags.length > 0 && (
                  <text
                    x="28"
                    y="-24"
                    textAnchor="middle"
                    fontSize="8"
                    fill="white"
                    fontWeight="bold"
                  >
                    {node.metadata.tags.length}
                  </text>
                )}
              </g>
            );
          })}
        </g>
      </svg>

      {/* Legend */}
      <div className="p-4 border-t border-gray-200">
        <div className="flex flex-wrap gap-4 text-xs">
          <div className="flex items-center gap-2">
            <span className="font-medium">Node Types:</span>
            {Object.entries(nodeColors).map(([type, colors]) => (
              <div key={type} className="flex items-center gap-1">
                <div 
                  className="w-4 h-4 rounded-full border-2"
                  style={{ backgroundColor: colors.fill, borderColor: colors.stroke }}
                />
                <span>{nodeIcons[type as NodeType]}</span>
              </div>
            ))}
          </div>
          <div className="flex items-center gap-2">
            <span className="font-medium">Edge Types:</span>
            {Object.entries(edgeStyles).slice(0, 5).map(([type, style]) => (
              <div key={type} className="flex items-center gap-1">
                <div 
                  className="w-8 h-0.5"
                  style={{ 
                    backgroundColor: style.color,
                    backgroundImage: style.dash ? `repeating-linear-gradient(90deg, ${style.color}, ${style.color} 2px, transparent 2px, transparent 4px)` : 'none'
                  }}
                />
                <span className="text-gray-600">{type}</span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}

export default KnowledgeGraphVisualization;

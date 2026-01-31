'use client';

/**
 * Node Details Panel
 * 
 * Shows detailed information about a selected node in the knowledge graph
 */

import React from 'react';
import type { GraphNode, GraphEdge, NodeType, EdgeType } from './knowledge-graph-visualization';

interface NodeDetailsPanelProps {
  node: GraphNode | null;
  edges: GraphEdge[];
  allNodes: GraphNode[];
  onNodeSelect: (nodeId: string) => void;
  onClose: () => void;
}

const nodeTypeLabels: Record<NodeType, string> = {
  'program': 'Program',
  'procedure': 'Procedure',
  'business-rule': 'Business Rule',
  'data-structure': 'Data Structure',
  'file': 'File',
  'database': 'Database',
  'external-system': 'External System',
  'regulation': 'Regulation',
  'concept': 'Concept',
};

const edgeTypeLabels: Record<EdgeType, string> = {
  'calls': 'Calls',
  'uses': 'Uses',
  'modifies': 'Modifies',
  'reads': 'Reads',
  'writes': 'Writes',
  'depends-on': 'Depends On',
  'implements': 'Implements',
  'affected-by': 'Affected By',
  'contains': 'Contains',
  'related-to': 'Related To',
};

export function NodeDetailsPanel({
  node,
  edges,
  allNodes,
  onNodeSelect,
  onClose,
}: NodeDetailsPanelProps) {
  if (!node) {
    return (
      <div className="p-6 text-center text-gray-500">
        <p>Select a node to view details</p>
      </div>
    );
  }

  // Find connected edges
  const incomingEdges = edges.filter(e => e.target === node.id);
  const outgoingEdges = edges.filter(e => e.source === node.id);

  // Get node by ID helper
  const getNodeById = (id: string) => allNodes.find(n => n.id === id);

  return (
    <div className="h-full flex flex-col bg-white">
      {/* Header */}
      <div className="p-4 border-b border-gray-200 flex items-center justify-between">
        <div>
          <span className="text-xs text-gray-500 uppercase tracking-wide">
            {nodeTypeLabels[node.type]}
          </span>
          <h2 className="text-lg font-semibold text-gray-900">{node.name}</h2>
        </div>
        <button
          onClick={onClose}
          className="p-2 hover:bg-gray-100 rounded-full"
        >
          <svg className="w-5 h-5 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
          </svg>
        </button>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-y-auto p-4 space-y-6">
        {/* Description */}
        {node.description && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">Description</h3>
            <p className="text-sm text-gray-600">{node.description}</p>
          </div>
        )}

        {/* Confidence */}
        {node.metadata.confidence !== undefined && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">Confidence Score</h3>
            <div className="flex items-center gap-2">
              <div className="flex-1 h-2 bg-gray-200 rounded-full overflow-hidden">
                <div
                  className="h-full bg-blue-500 rounded-full"
                  style={{ width: `${node.metadata.confidence * 100}%` }}
                />
              </div>
              <span className="text-sm font-medium text-gray-700">
                {(node.metadata.confidence * 100).toFixed(0)}%
              </span>
            </div>
          </div>
        )}

        {/* Source Location */}
        {node.metadata.source && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">Source Location</h3>
            <div className="bg-gray-50 rounded-md p-3 text-sm font-mono">
              <p className="text-gray-800">{node.metadata.source.file}</p>
              <p className="text-gray-500">
                Lines {node.metadata.source.startLine} - {node.metadata.source.endLine}
              </p>
            </div>
          </div>
        )}

        {/* Tags */}
        {node.metadata.tags && node.metadata.tags.length > 0 && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">Tags</h3>
            <div className="flex flex-wrap gap-2">
              {node.metadata.tags.map((tag, i) => (
                <span
                  key={i}
                  className="px-2 py-1 bg-purple-100 text-purple-700 rounded-full text-xs"
                >
                  {tag}
                </span>
              ))}
            </div>
          </div>
        )}

        {/* Properties */}
        {Object.keys(node.properties).length > 0 && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">Properties</h3>
            <div className="bg-gray-50 rounded-md p-3 space-y-2">
              {Object.entries(node.properties).map(([key, value]) => (
                <div key={key} className="flex justify-between text-sm">
                  <span className="text-gray-600">{key}</span>
                  <span className="text-gray-900 font-medium">
                    {typeof value === 'object' ? JSON.stringify(value) : String(value)}
                  </span>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Incoming Connections */}
        {incomingEdges.length > 0 && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">
              Incoming Connections ({incomingEdges.length})
            </h3>
            <div className="space-y-2">
              {incomingEdges.map(edge => {
                const sourceNode = getNodeById(edge.source);
                if (!sourceNode) return null;
                return (
                  <button
                    key={edge.id}
                    onClick={() => onNodeSelect(edge.source)}
                    className="w-full flex items-center gap-3 p-2 bg-gray-50 hover:bg-gray-100 rounded-md text-left transition-colors"
                  >
                    <span className="text-xs text-gray-500 uppercase w-20">
                      {edgeTypeLabels[edge.type]}
                    </span>
                    <span className="text-sm text-gray-900">{sourceNode.name}</span>
                    <span className="text-xs text-gray-500 ml-auto">
                      {nodeTypeLabels[sourceNode.type]}
                    </span>
                  </button>
                );
              })}
            </div>
          </div>
        )}

        {/* Outgoing Connections */}
        {outgoingEdges.length > 0 && (
          <div>
            <h3 className="text-sm font-medium text-gray-700 mb-2">
              Outgoing Connections ({outgoingEdges.length})
            </h3>
            <div className="space-y-2">
              {outgoingEdges.map(edge => {
                const targetNode = getNodeById(edge.target);
                if (!targetNode) return null;
                return (
                  <button
                    key={edge.id}
                    onClick={() => onNodeSelect(edge.target)}
                    className="w-full flex items-center gap-3 p-2 bg-gray-50 hover:bg-gray-100 rounded-md text-left transition-colors"
                  >
                    <span className="text-xs text-gray-500 uppercase w-20">
                      {edgeTypeLabels[edge.type]}
                    </span>
                    <span className="text-sm text-gray-900">{targetNode.name}</span>
                    <span className="text-xs text-gray-500 ml-auto">
                      {nodeTypeLabels[targetNode.type]}
                    </span>
                  </button>
                );
              })}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

export default NodeDetailsPanel;

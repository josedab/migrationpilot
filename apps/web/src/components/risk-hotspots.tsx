'use client';

/**
 * Risk Hotspots Component
 * 
 * Displays modules with highest risk scores
 */

import React from 'react';

interface RiskHotspot {
  nodeId: string;
  nodeName: string;
  riskScore: number;
  reasons: string[];
}

interface RiskHotspotsProps {
  hotspots: RiskHotspot[];
  onHotspotClick?: (nodeId: string) => void;
}

export function RiskHotspots({ hotspots, onHotspotClick }: RiskHotspotsProps) {
  const getRiskColor = (score: number) => {
    if (score >= 0.8) return 'bg-red-500';
    if (score >= 0.6) return 'bg-orange-500';
    if (score >= 0.4) return 'bg-yellow-500';
    return 'bg-green-500';
  };

  const getRiskLabel = (score: number) => {
    if (score >= 0.8) return 'Critical';
    if (score >= 0.6) return 'High';
    if (score >= 0.4) return 'Medium';
    return 'Low';
  };

  return (
    <div className="bg-white rounded-lg shadow-md p-6">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-semibold text-gray-900">⚠️ Risk Hotspots</h3>
        <span className="text-sm text-gray-500">{hotspots.length} identified</span>
      </div>

      {hotspots.length === 0 ? (
        <div className="text-center py-8 text-gray-500">
          <span className="text-4xl">✅</span>
          <p className="mt-2">No high-risk modules identified</p>
        </div>
      ) : (
        <div className="space-y-4">
          {hotspots.map((hotspot) => (
            <div
              key={hotspot.nodeId}
              className="border border-gray-200 rounded-lg p-4 hover:bg-gray-50 cursor-pointer transition-colors"
              onClick={() => onHotspotClick?.(hotspot.nodeId)}
            >
              <div className="flex items-center justify-between mb-2">
                <span className="font-medium text-gray-900">{hotspot.nodeName}</span>
                <div className="flex items-center gap-2">
                  <div 
                    className={`w-3 h-3 rounded-full ${getRiskColor(hotspot.riskScore)}`}
                  />
                  <span className="text-sm font-medium">
                    {getRiskLabel(hotspot.riskScore)} ({Math.round(hotspot.riskScore * 100)}%)
                  </span>
                </div>
              </div>
              
              {/* Risk Score Bar */}
              <div className="w-full bg-gray-200 rounded-full h-2 mb-3">
                <div
                  className={`h-2 rounded-full ${getRiskColor(hotspot.riskScore)}`}
                  style={{ width: `${hotspot.riskScore * 100}%` }}
                />
              </div>

              {/* Reasons */}
              <ul className="text-sm text-gray-600 space-y-1">
                {hotspot.reasons.map((reason, idx) => (
                  <li key={idx} className="flex items-start gap-2">
                    <span className="text-gray-400">•</span>
                    <span>{reason}</span>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default RiskHotspots;

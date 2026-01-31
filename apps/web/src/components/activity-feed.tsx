'use client';

/**
 * Activity Feed Component
 * 
 * Shows recent migration activity
 */

import React from 'react';

interface Activity {
  timestamp: string;
  type: 'analysis' | 'migration' | 'validation' | 'review';
  description: string;
  nodeId?: string;
}

interface ActivityFeedProps {
  activities: Activity[];
  onActivityClick?: (nodeId: string) => void;
}

const activityIcons: Record<string, string> = {
  analysis: '🔍',
  migration: '🚀',
  validation: '✅',
  review: '👀',
};

const activityColors: Record<string, string> = {
  analysis: 'bg-yellow-100 text-yellow-800',
  migration: 'bg-blue-100 text-blue-800',
  validation: 'bg-green-100 text-green-800',
  review: 'bg-purple-100 text-purple-800',
};

export function ActivityFeed({ activities, onActivityClick }: ActivityFeedProps) {
  const formatTimestamp = (timestamp: string) => {
    const date = new Date(timestamp);
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMs / 3600000);
    const diffDays = Math.floor(diffMs / 86400000);

    if (diffMins < 1) return 'Just now';
    if (diffMins < 60) return `${diffMins}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays < 7) return `${diffDays}d ago`;
    return date.toLocaleDateString();
  };

  return (
    <div className="bg-white rounded-lg shadow-md p-6">
      <h3 className="text-lg font-semibold text-gray-900 mb-4">📋 Recent Activity</h3>

      {activities.length === 0 ? (
        <div className="text-center py-8 text-gray-500">
          <p>No recent activity</p>
        </div>
      ) : (
        <div className="space-y-4">
          {activities.map((activity, idx) => (
            <div
              key={idx}
              className={`flex items-start gap-3 ${activity.nodeId ? 'cursor-pointer hover:bg-gray-50' : ''} rounded-lg p-2 -mx-2 transition-colors`}
              onClick={() => activity.nodeId && onActivityClick?.(activity.nodeId)}
            >
              <div className="flex-shrink-0 mt-1">
                <span className="text-xl">{activityIcons[activity.type]}</span>
              </div>
              <div className="flex-grow min-w-0">
                <p className="text-sm text-gray-900">{activity.description}</p>
                <div className="flex items-center gap-2 mt-1">
                  <span className={`text-xs px-2 py-0.5 rounded-full ${activityColors[activity.type]}`}>
                    {activity.type}
                  </span>
                  <span className="text-xs text-gray-500">
                    {formatTimestamp(activity.timestamp)}
                  </span>
                </div>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default ActivityFeed;

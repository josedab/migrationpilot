'use client';

/**
 * SME Activity Feed Component
 * 
 * Real-time feed of SME collaboration activities
 */

import React from 'react';

export interface SMEActivity {
  id: string;
  type: 'approval' | 'rejection' | 'comment' | 'edit' | 'revision_request' | 'resolution';
  userId: string;
  userName: string;
  userAvatar?: string;
  ruleId: string;
  ruleName: string;
  description: string;
  metadata?: {
    commentType?: string;
    oldValue?: string;
    newValue?: string;
  };
  timestamp: string;
}

interface SMEActivityFeedProps {
  activities: SMEActivity[];
  maxItems?: number;
  onActivityClick?: (activity: SMEActivity) => void;
  onRuleClick?: (ruleId: string) => void;
}

const activityConfig: Record<string, { icon: string; color: string; label: string }> = {
  approval: { icon: '✅', color: 'bg-green-100 text-green-700 border-green-200', label: 'Approved' },
  rejection: { icon: '❌', color: 'bg-red-100 text-red-700 border-red-200', label: 'Rejected' },
  comment: { icon: '💬', color: 'bg-blue-100 text-blue-700 border-blue-200', label: 'Commented' },
  edit: { icon: '✏️', color: 'bg-purple-100 text-purple-700 border-purple-200', label: 'Edited' },
  revision_request: { icon: '🔄', color: 'bg-orange-100 text-orange-700 border-orange-200', label: 'Revision Requested' },
  resolution: { icon: '✓', color: 'bg-teal-100 text-teal-700 border-teal-200', label: 'Resolved Comment' },
};

export function SMEActivityFeed({
  activities,
  maxItems = 20,
  onActivityClick,
  onRuleClick,
}: SMEActivityFeedProps) {
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

  const displayActivities = activities.slice(0, maxItems);

  return (
    <div className="bg-white rounded-lg shadow-md overflow-hidden">
      <div className="px-4 py-3 bg-gray-50 border-b flex items-center justify-between">
        <h3 className="font-semibold text-gray-900 flex items-center gap-2">
          <span>👥</span> Team Activity
        </h3>
        <span className="text-xs text-gray-500">
          {activities.length} activities
        </span>
      </div>

      {activities.length === 0 ? (
        <div className="p-8 text-center text-gray-500">
          <div className="text-3xl mb-2">📝</div>
          <p>No activity yet</p>
          <p className="text-sm">Start reviewing rules to see activity here</p>
        </div>
      ) : (
        <div className="divide-y divide-gray-100 max-h-[500px] overflow-y-auto">
          {displayActivities.map((activity) => {
            const config = activityConfig[activity.type];
            
            return (
              <div
                key={activity.id}
                className="p-4 hover:bg-gray-50 transition-colors cursor-pointer"
                onClick={() => onActivityClick?.(activity)}
              >
                <div className="flex items-start gap-3">
                  {/* Avatar */}
                  <div className="flex-shrink-0">
                    {activity.userAvatar ? (
                      <img
                        src={activity.userAvatar}
                        alt={activity.userName}
                        className="w-10 h-10 rounded-full"
                      />
                    ) : (
                      <div className="w-10 h-10 rounded-full bg-gradient-to-br from-blue-400 to-purple-500 flex items-center justify-center text-white font-semibold">
                        {activity.userName.charAt(0).toUpperCase()}
                      </div>
                    )}
                  </div>

                  {/* Content */}
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 flex-wrap">
                      <span className="font-semibold text-gray-900">
                        {activity.userName}
                      </span>
                      <span className={`text-xs px-2 py-0.5 rounded-full border ${config.color}`}>
                        {config.icon} {config.label}
                      </span>
                    </div>

                    <p className="text-sm text-gray-600 mt-1">
                      {activity.description}
                    </p>

                    {/* Rule link */}
                    <button
                      onClick={(e) => {
                        e.stopPropagation();
                        onRuleClick?.(activity.ruleId);
                      }}
                      className="text-sm text-blue-600 hover:text-blue-800 hover:underline mt-1 inline-flex items-center gap-1"
                    >
                      📋 {activity.ruleName}
                      <span className="text-xs font-mono text-gray-400">
                        ({activity.ruleId})
                      </span>
                    </button>

                    {/* Metadata */}
                    {activity.metadata?.commentType && (
                      <div className="mt-2 text-xs text-gray-500 bg-gray-50 px-2 py-1 rounded inline-block">
                        {activity.metadata.commentType}
                      </div>
                    )}

                    {/* Timestamp */}
                    <div className="text-xs text-gray-400 mt-2">
                      {formatTimestamp(activity.timestamp)}
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}

      {activities.length > maxItems && (
        <div className="px-4 py-3 bg-gray-50 border-t text-center">
          <button className="text-sm text-blue-600 hover:text-blue-800">
            View all {activities.length} activities →
          </button>
        </div>
      )}
    </div>
  );
}

export default SMEActivityFeed;

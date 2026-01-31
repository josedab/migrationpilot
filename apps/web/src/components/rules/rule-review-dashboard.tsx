'use client';

/**
 * Rule Review Dashboard Component
 * 
 * Main interface for SME collaboration on business rule review
 */

import React, { useState, useMemo } from 'react';
import { RuleReviewCard, type BusinessRule, type RuleComment } from './rule-review-card';

interface RuleReviewDashboardProps {
  rules: BusinessRule[];
  projectId: string;
  projectName: string;
  confidenceThreshold?: number;
  currentUserId?: string;
  currentUserName?: string;
  onRulesUpdate?: (rules: BusinessRule[]) => void;
}

type FilterStatus = 'all' | 'pending' | 'approved' | 'rejected' | 'needs_revision' | 'low_confidence';
type SortBy = 'confidence' | 'name' | 'status' | 'comments';

export function RuleReviewDashboard({
  rules: initialRules,
  projectId,
  projectName,
  confidenceThreshold = 0.85,
  currentUserId = 'user',
  currentUserName = 'User',
  onRulesUpdate,
}: RuleReviewDashboardProps) {
  const [rules, setRules] = useState<BusinessRule[]>(initialRules);
  const [filterStatus, setFilterStatus] = useState<FilterStatus>('all');
  const [sortBy, setSortBy] = useState<SortBy>('confidence');
  const [searchQuery, setSearchQuery] = useState('');
  const [showStats, setShowStats] = useState(true);

  // Calculate statistics
  const stats = useMemo(() => {
    const total = rules.length;
    const pending = rules.filter(r => r.status === 'pending').length;
    const approved = rules.filter(r => r.status === 'approved').length;
    const rejected = rules.filter(r => r.status === 'rejected').length;
    const needsRevision = rules.filter(r => r.status === 'needs_revision').length;
    const lowConfidence = rules.filter(r => r.confidence < confidenceThreshold).length;
    const avgConfidence = rules.length > 0 
      ? rules.reduce((sum, r) => sum + r.confidence, 0) / rules.length 
      : 0;
    const unresolvedComments = rules.reduce((sum, r) => 
      sum + r.comments.filter(c => !c.resolved).length, 0
    );

    return {
      total,
      pending,
      approved,
      rejected,
      needsRevision,
      lowConfidence,
      avgConfidence,
      unresolvedComments,
      approvalRate: total > 0 ? ((approved / total) * 100).toFixed(1) : '0',
      completionRate: total > 0 ? (((approved + rejected) / total) * 100).toFixed(1) : '0',
    };
  }, [rules, confidenceThreshold]);

  // Filter and sort rules
  const filteredRules = useMemo(() => {
    let filtered = [...rules];

    // Apply search filter
    if (searchQuery) {
      const query = searchQuery.toLowerCase();
      filtered = filtered.filter(r =>
        r.name.toLowerCase().includes(query) ||
        r.description.toLowerCase().includes(query) ||
        r.id.toLowerCase().includes(query) ||
        r.category.toLowerCase().includes(query)
      );
    }

    // Apply status filter
    if (filterStatus !== 'all') {
      if (filterStatus === 'low_confidence') {
        filtered = filtered.filter(r => r.confidence < confidenceThreshold);
      } else {
        filtered = filtered.filter(r => r.status === filterStatus);
      }
    }

    // Apply sorting
    filtered.sort((a, b) => {
      switch (sortBy) {
        case 'confidence':
          return a.confidence - b.confidence; // Low confidence first (needs attention)
        case 'name':
          return a.name.localeCompare(b.name);
        case 'status':
          const statusOrder = { pending: 0, needs_revision: 1, rejected: 2, approved: 3 };
          return statusOrder[a.status] - statusOrder[b.status];
        case 'comments':
          const aComments = a.comments.filter(c => !c.resolved).length;
          const bComments = b.comments.filter(c => !c.resolved).length;
          return bComments - aComments; // Most comments first
        default:
          return 0;
      }
    });

    return filtered;
  }, [rules, filterStatus, sortBy, searchQuery, confidenceThreshold]);

  // Rule update handlers
  const updateRule = (ruleId: string, updates: Partial<BusinessRule>) => {
    const newRules = rules.map(r =>
      r.id === ruleId ? { ...r, ...updates } : r
    );
    setRules(newRules);
    onRulesUpdate?.(newRules);
  };

  const handleApprove = (ruleId: string) => {
    updateRule(ruleId, {
      status: 'approved',
      reviewedBy: currentUserName,
      reviewedAt: new Date().toISOString(),
    });
  };

  const handleReject = (ruleId: string, reason: string) => {
    const rule = rules.find(r => r.id === ruleId);
    if (!rule) return;

    const newComment: RuleComment = {
      id: `comment-${Date.now()}`,
      userId: currentUserId,
      userName: currentUserName,
      content: `Rejection reason: ${reason}`,
      type: 'comment',
      createdAt: new Date().toISOString(),
    };

    updateRule(ruleId, {
      status: 'rejected',
      reviewedBy: currentUserName,
      reviewedAt: new Date().toISOString(),
      comments: [...rule.comments, newComment],
    });
  };

  const handleRequestRevision = (ruleId: string, comment: string) => {
    const rule = rules.find(r => r.id === ruleId);
    if (!rule) return;

    const newComment: RuleComment = {
      id: `comment-${Date.now()}`,
      userId: currentUserId,
      userName: currentUserName,
      content: comment,
      type: 'suggestion',
      createdAt: new Date().toISOString(),
    };

    updateRule(ruleId, {
      status: 'needs_revision',
      comments: [...rule.comments, newComment],
    });
  };

  const handleAddComment = (ruleId: string, comment: Omit<RuleComment, 'id' | 'createdAt'>) => {
    const rule = rules.find(r => r.id === ruleId);
    if (!rule) return;

    const newComment: RuleComment = {
      ...comment,
      id: `comment-${Date.now()}`,
      createdAt: new Date().toISOString(),
    };

    updateRule(ruleId, {
      comments: [...rule.comments, newComment],
    });
  };

  const handleEditRule = (ruleId: string, updates: Partial<BusinessRule>) => {
    updateRule(ruleId, updates);
  };

  const handleResolveComment = (ruleId: string, commentId: string) => {
    const rule = rules.find(r => r.id === ruleId);
    if (!rule) return;

    updateRule(ruleId, {
      comments: rule.comments.map(c =>
        c.id === commentId ? { ...c, resolved: true } : c
      ),
    });
  };

  // Bulk actions
  const handleApproveAll = () => {
    const newRules = rules.map(r =>
      r.status === 'pending' && r.confidence >= confidenceThreshold
        ? { ...r, status: 'approved' as const, reviewedBy: currentUserName, reviewedAt: new Date().toISOString() }
        : r
    );
    setRules(newRules);
    onRulesUpdate?.(newRules);
  };

  return (
    <div className="min-h-screen bg-gray-100">
      {/* Header */}
      <div className="bg-white border-b sticky top-0 z-10">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-2xl font-bold text-gray-900">📋 Business Rule Review</h1>
              <p className="text-sm text-gray-500">{projectName}</p>
            </div>
            <div className="flex items-center gap-3">
              <button
                onClick={handleApproveAll}
                disabled={stats.pending === 0}
                className="px-4 py-2 bg-green-500 text-white rounded-lg hover:bg-green-600 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                ✓ Approve All High-Confidence
              </button>
              <button
                onClick={() => setShowStats(!showStats)}
                className="px-4 py-2 bg-gray-100 text-gray-700 rounded-lg hover:bg-gray-200"
              >
                {showStats ? '▲ Hide Stats' : '▼ Show Stats'}
              </button>
            </div>
          </div>

          {/* Statistics */}
          {showStats && (
            <div className="grid grid-cols-2 md:grid-cols-4 lg:grid-cols-8 gap-3 mt-4">
              <StatCard label="Total Rules" value={stats.total} />
              <StatCard label="Pending" value={stats.pending} color="yellow" />
              <StatCard label="Approved" value={stats.approved} color="green" />
              <StatCard label="Rejected" value={stats.rejected} color="red" />
              <StatCard label="Needs Revision" value={stats.needsRevision} color="orange" />
              <StatCard label="Low Confidence" value={stats.lowConfidence} color="red" />
              <StatCard label="Avg Confidence" value={`${(stats.avgConfidence * 100).toFixed(0)}%`} />
              <StatCard label="Completion" value={`${stats.completionRate}%`} color="blue" />
            </div>
          )}

          {/* Filters */}
          <div className="flex flex-wrap items-center gap-3 mt-4">
            <div className="flex-1 min-w-[200px]">
              <input
                type="text"
                placeholder="🔍 Search rules..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                className="w-full px-4 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
            </div>
            <select
              value={filterStatus}
              onChange={(e) => setFilterStatus(e.target.value as FilterStatus)}
              className="px-4 py-2 border rounded-lg bg-white"
            >
              <option value="all">All Status</option>
              <option value="pending">⏳ Pending</option>
              <option value="approved">✅ Approved</option>
              <option value="rejected">❌ Rejected</option>
              <option value="needs_revision">🔄 Needs Revision</option>
              <option value="low_confidence">⚠️ Low Confidence</option>
            </select>
            <select
              value={sortBy}
              onChange={(e) => setSortBy(e.target.value as SortBy)}
              className="px-4 py-2 border rounded-lg bg-white"
            >
              <option value="confidence">Sort: Confidence ↑</option>
              <option value="name">Sort: Name A-Z</option>
              <option value="status">Sort: Status</option>
              <option value="comments">Sort: Most Comments</option>
            </select>
          </div>
        </div>
      </div>

      {/* Rules List */}
      <div className="max-w-7xl mx-auto px-4 py-6">
        {filteredRules.length === 0 ? (
          <div className="text-center py-12 bg-white rounded-lg shadow">
            <div className="text-4xl mb-4">📭</div>
            <h3 className="text-lg font-semibold text-gray-700">No rules found</h3>
            <p className="text-gray-500">Try adjusting your filters or search query</p>
          </div>
        ) : (
          <div className="space-y-4">
            <p className="text-sm text-gray-500">
              Showing {filteredRules.length} of {rules.length} rules
            </p>
            {filteredRules.map((rule) => (
              <RuleReviewCard
                key={rule.id}
                rule={rule}
                confidenceThreshold={confidenceThreshold}
                currentUserId={currentUserId}
                currentUserName={currentUserName}
                onApprove={handleApprove}
                onReject={handleReject}
                onRequestRevision={handleRequestRevision}
                onAddComment={handleAddComment}
                onEditRule={handleEditRule}
                onResolveComment={handleResolveComment}
              />
            ))}
          </div>
        )}
      </div>
    </div>
  );
}

// Stat Card Component
interface StatCardProps {
  label: string;
  value: string | number;
  color?: 'green' | 'yellow' | 'red' | 'orange' | 'blue';
}

function StatCard({ label, value, color }: StatCardProps) {
  const colorClasses = {
    green: 'bg-green-50 text-green-700 border-green-200',
    yellow: 'bg-yellow-50 text-yellow-700 border-yellow-200',
    red: 'bg-red-50 text-red-700 border-red-200',
    orange: 'bg-orange-50 text-orange-700 border-orange-200',
    blue: 'bg-blue-50 text-blue-700 border-blue-200',
  };

  return (
    <div className={`p-3 rounded-lg border ${color ? colorClasses[color] : 'bg-gray-50 text-gray-700 border-gray-200'}`}>
      <div className="text-xl font-bold">{value}</div>
      <div className="text-xs opacity-75">{label}</div>
    </div>
  );
}

export default RuleReviewDashboard;

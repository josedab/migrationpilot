'use client';

/**
 * Rule Review Card Component
 * 
 * Displays a business rule with SME review capabilities
 */

import React, { useState } from 'react';

export interface BusinessRule {
  id: string;
  name: string;
  description: string;
  confidence: number;
  category: string;
  inputs: { name: string; type: string; description?: string }[];
  outputs: { name: string; type: string; description?: string }[];
  logic?: {
    calculation?: string;
    conditions?: string[];
    edgeCases?: string[];
  };
  sourceLocation?: {
    file: string;
    startLine: number;
    endLine: number;
  };
  status: 'pending' | 'approved' | 'rejected' | 'needs_revision';
  reviewedBy?: string;
  reviewedAt?: string;
  comments: RuleComment[];
}

export interface RuleComment {
  id: string;
  userId: string;
  userName: string;
  content: string;
  type: 'comment' | 'suggestion' | 'correction' | 'question';
  createdAt: string;
  resolved?: boolean;
}

interface RuleReviewCardProps {
  rule: BusinessRule;
  confidenceThreshold?: number;
  currentUserId?: string;
  currentUserName?: string;
  onApprove?: (ruleId: string) => void;
  onReject?: (ruleId: string, reason: string) => void;
  onRequestRevision?: (ruleId: string, comment: string) => void;
  onAddComment?: (ruleId: string, comment: Omit<RuleComment, 'id' | 'createdAt'>) => void;
  onEditRule?: (ruleId: string, updates: Partial<BusinessRule>) => void;
  onResolveComment?: (ruleId: string, commentId: string) => void;
}

const statusColors: Record<string, { bg: string; text: string; border: string }> = {
  pending: { bg: 'bg-yellow-50', text: 'text-yellow-700', border: 'border-yellow-200' },
  approved: { bg: 'bg-green-50', text: 'text-green-700', border: 'border-green-200' },
  rejected: { bg: 'bg-red-50', text: 'text-red-700', border: 'border-red-200' },
  needs_revision: { bg: 'bg-orange-50', text: 'text-orange-700', border: 'border-orange-200' },
};

const statusLabels: Record<string, string> = {
  pending: '⏳ Pending Review',
  approved: '✅ Approved',
  rejected: '❌ Rejected',
  needs_revision: '🔄 Needs Revision',
};

export function RuleReviewCard({
  rule,
  confidenceThreshold = 0.85,
  currentUserId = 'user',
  currentUserName = 'User',
  onApprove,
  onReject,
  onRequestRevision,
  onAddComment,
  onEditRule,
  onResolveComment,
}: RuleReviewCardProps) {
  const [isExpanded, setIsExpanded] = useState(false);
  const [isEditing, setIsEditing] = useState(false);
  const [showCommentForm, setShowCommentForm] = useState(false);
  const [commentType, setCommentType] = useState<RuleComment['type']>('comment');
  const [commentContent, setCommentContent] = useState('');
  const [rejectReason, setRejectReason] = useState('');
  const [showRejectModal, setShowRejectModal] = useState(false);
  const [editedDescription, setEditedDescription] = useState(rule.description);

  const needsAttention = rule.confidence < confidenceThreshold;
  const statusStyle = statusColors[rule.status];
  const unresolvedComments = rule.comments.filter(c => !c.resolved).length;

  const handleSubmitComment = () => {
    if (!commentContent.trim()) return;
    
    onAddComment?.(rule.id, {
      userId: currentUserId,
      userName: currentUserName,
      content: commentContent,
      type: commentType,
    });
    
    setCommentContent('');
    setShowCommentForm(false);
  };

  const handleReject = () => {
    if (rejectReason.trim()) {
      onReject?.(rule.id, rejectReason);
      setShowRejectModal(false);
      setRejectReason('');
    }
  };

  const handleSaveEdit = () => {
    onEditRule?.(rule.id, { description: editedDescription });
    setIsEditing(false);
  };

  return (
    <div className={`rounded-lg border-2 ${statusStyle.border} ${statusStyle.bg} overflow-hidden transition-all`}>
      {/* Header */}
      <div className="p-4">
        <div className="flex items-start justify-between">
          <div className="flex-1">
            <div className="flex items-center gap-2 mb-1">
              <span className="text-xs font-mono bg-gray-200 px-2 py-0.5 rounded">
                {rule.id}
              </span>
              <span className={`text-xs px-2 py-0.5 rounded-full ${statusStyle.text} ${statusStyle.bg} border ${statusStyle.border}`}>
                {statusLabels[rule.status]}
              </span>
              {needsAttention && rule.status === 'pending' && (
                <span className="text-xs px-2 py-0.5 rounded-full bg-red-100 text-red-700 border border-red-200 animate-pulse">
                  ⚠️ Low Confidence
                </span>
              )}
              {unresolvedComments > 0 && (
                <span className="text-xs px-2 py-0.5 rounded-full bg-blue-100 text-blue-700 border border-blue-200">
                  💬 {unresolvedComments}
                </span>
              )}
            </div>
            <h3 className="text-lg font-semibold text-gray-900">{rule.name}</h3>
            <p className="text-sm text-gray-500">{rule.category}</p>
          </div>
          <div className="flex flex-col items-end gap-1">
            <div className={`text-2xl font-bold ${
              rule.confidence >= 0.9 ? 'text-green-600' : 
              rule.confidence >= 0.7 ? 'text-yellow-600' : 'text-red-600'
            }`}>
              {(rule.confidence * 100).toFixed(0)}%
            </div>
            <span className="text-xs text-gray-500">confidence</span>
          </div>
        </div>

        {/* Description */}
        <div className="mt-3">
          {isEditing ? (
            <div className="space-y-2">
              <textarea
                value={editedDescription}
                onChange={(e) => setEditedDescription(e.target.value)}
                className="w-full p-2 border rounded-md text-sm"
                rows={3}
              />
              <div className="flex gap-2">
                <button
                  onClick={handleSaveEdit}
                  className="px-3 py-1 text-sm bg-blue-500 text-white rounded hover:bg-blue-600"
                >
                  Save
                </button>
                <button
                  onClick={() => {
                    setIsEditing(false);
                    setEditedDescription(rule.description);
                  }}
                  className="px-3 py-1 text-sm bg-gray-200 text-gray-700 rounded hover:bg-gray-300"
                >
                  Cancel
                </button>
              </div>
            </div>
          ) : (
            <p className="text-gray-700">{rule.description}</p>
          )}
        </div>

        {/* Quick Actions */}
        <div className="flex items-center gap-2 mt-4">
          <button
            onClick={() => setIsExpanded(!isExpanded)}
            className="px-3 py-1.5 text-sm bg-white border border-gray-200 rounded-md hover:bg-gray-50"
          >
            {isExpanded ? '▲ Hide Details' : '▼ Show Details'}
          </button>
          
          {rule.status === 'pending' && (
            <>
              <button
                onClick={() => onApprove?.(rule.id)}
                className="px-3 py-1.5 text-sm bg-green-500 text-white rounded-md hover:bg-green-600"
              >
                ✓ Approve
              </button>
              <button
                onClick={() => setShowRejectModal(true)}
                className="px-3 py-1.5 text-sm bg-red-500 text-white rounded-md hover:bg-red-600"
              >
                ✗ Reject
              </button>
              <button
                onClick={() => onRequestRevision?.(rule.id, 'Needs further clarification')}
                className="px-3 py-1.5 text-sm bg-orange-500 text-white rounded-md hover:bg-orange-600"
              >
                ↻ Request Revision
              </button>
            </>
          )}
          
          <button
            onClick={() => setIsEditing(true)}
            className="px-3 py-1.5 text-sm bg-blue-100 text-blue-700 rounded-md hover:bg-blue-200 ml-auto"
          >
            ✎ Edit
          </button>
          <button
            onClick={() => setShowCommentForm(!showCommentForm)}
            className="px-3 py-1.5 text-sm bg-gray-100 text-gray-700 rounded-md hover:bg-gray-200"
          >
            💬 Comment
          </button>
        </div>
      </div>

      {/* Expanded Details */}
      {isExpanded && (
        <div className="border-t border-gray-200 p-4 bg-white/50">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* Inputs */}
            <div>
              <h4 className="text-sm font-semibold text-gray-700 mb-2">📥 Inputs</h4>
              <div className="space-y-1">
                {rule.inputs.map((input, idx) => (
                  <div key={idx} className="text-sm bg-white p-2 rounded border">
                    <span className="font-mono text-blue-600">{input.name}</span>
                    <span className="text-gray-400 mx-1">:</span>
                    <span className="text-gray-600">{input.type}</span>
                    {input.description && (
                      <p className="text-xs text-gray-500 mt-1">{input.description}</p>
                    )}
                  </div>
                ))}
              </div>
            </div>

            {/* Outputs */}
            <div>
              <h4 className="text-sm font-semibold text-gray-700 mb-2">📤 Outputs</h4>
              <div className="space-y-1">
                {rule.outputs.map((output, idx) => (
                  <div key={idx} className="text-sm bg-white p-2 rounded border">
                    <span className="font-mono text-green-600">{output.name}</span>
                    <span className="text-gray-400 mx-1">:</span>
                    <span className="text-gray-600">{output.type}</span>
                    {output.description && (
                      <p className="text-xs text-gray-500 mt-1">{output.description}</p>
                    )}
                  </div>
                ))}
              </div>
            </div>
          </div>

          {/* Logic */}
          {rule.logic && (
            <div className="mt-4">
              <h4 className="text-sm font-semibold text-gray-700 mb-2">🔢 Business Logic</h4>
              {rule.logic.calculation && (
                <pre className="text-sm bg-gray-900 text-green-400 p-3 rounded-md overflow-x-auto">
                  {rule.logic.calculation}
                </pre>
              )}
              {rule.logic.conditions && rule.logic.conditions.length > 0 && (
                <div className="mt-2">
                  <span className="text-xs font-semibold text-gray-500">Conditions:</span>
                  <ul className="list-disc list-inside text-sm text-gray-600 mt-1">
                    {rule.logic.conditions.map((cond, idx) => (
                      <li key={idx}>{cond}</li>
                    ))}
                  </ul>
                </div>
              )}
              {rule.logic.edgeCases && rule.logic.edgeCases.length > 0 && (
                <div className="mt-2">
                  <span className="text-xs font-semibold text-gray-500">Edge Cases:</span>
                  <ul className="list-disc list-inside text-sm text-orange-600 mt-1">
                    {rule.logic.edgeCases.map((edge, idx) => (
                      <li key={idx}>{edge}</li>
                    ))}
                  </ul>
                </div>
              )}
            </div>
          )}

          {/* Source Location */}
          {rule.sourceLocation && (
            <div className="mt-4 text-sm text-gray-500">
              <span className="font-semibold">📁 Source:</span>{' '}
              <span className="font-mono">{rule.sourceLocation.file}</span>
              <span className="mx-1">:</span>
              <span>Lines {rule.sourceLocation.startLine}-{rule.sourceLocation.endLine}</span>
            </div>
          )}
        </div>
      )}

      {/* Comment Form */}
      {showCommentForm && (
        <div className="border-t border-gray-200 p-4 bg-gray-50">
          <div className="flex gap-2 mb-2">
            {(['comment', 'suggestion', 'correction', 'question'] as const).map((type) => (
              <button
                key={type}
                onClick={() => setCommentType(type)}
                className={`px-2 py-1 text-xs rounded ${
                  commentType === type
                    ? 'bg-blue-500 text-white'
                    : 'bg-gray-200 text-gray-600 hover:bg-gray-300'
                }`}
              >
                {type === 'comment' && '💬'}
                {type === 'suggestion' && '💡'}
                {type === 'correction' && '✏️'}
                {type === 'question' && '❓'}
                {' '}{type.charAt(0).toUpperCase() + type.slice(1)}
              </button>
            ))}
          </div>
          <textarea
            value={commentContent}
            onChange={(e) => setCommentContent(e.target.value)}
            placeholder={`Add a ${commentType}...`}
            className="w-full p-2 border rounded-md text-sm"
            rows={2}
          />
          <div className="flex justify-end gap-2 mt-2">
            <button
              onClick={() => setShowCommentForm(false)}
              className="px-3 py-1 text-sm text-gray-600 hover:text-gray-800"
            >
              Cancel
            </button>
            <button
              onClick={handleSubmitComment}
              disabled={!commentContent.trim()}
              className="px-3 py-1 text-sm bg-blue-500 text-white rounded hover:bg-blue-600 disabled:opacity-50"
            >
              Submit
            </button>
          </div>
        </div>
      )}

      {/* Comments Section */}
      {rule.comments.length > 0 && (
        <div className="border-t border-gray-200 p-4 bg-white/30">
          <h4 className="text-sm font-semibold text-gray-700 mb-3">
            💬 Comments ({rule.comments.length})
          </h4>
          <div className="space-y-3">
            {rule.comments.map((comment) => (
              <div
                key={comment.id}
                className={`p-3 rounded-md border ${
                  comment.resolved ? 'bg-gray-100 border-gray-200' : 'bg-white border-gray-200'
                }`}
              >
                <div className="flex items-start justify-between">
                  <div className="flex items-center gap-2">
                    <span className="font-semibold text-sm">{comment.userName}</span>
                    <span className={`text-xs px-1.5 py-0.5 rounded ${
                      comment.type === 'suggestion' ? 'bg-yellow-100 text-yellow-700' :
                      comment.type === 'correction' ? 'bg-red-100 text-red-700' :
                      comment.type === 'question' ? 'bg-purple-100 text-purple-700' :
                      'bg-gray-100 text-gray-700'
                    }`}>
                      {comment.type}
                    </span>
                    {comment.resolved && (
                      <span className="text-xs text-green-600">✓ Resolved</span>
                    )}
                  </div>
                  <span className="text-xs text-gray-400">
                    {new Date(comment.createdAt).toLocaleDateString()}
                  </span>
                </div>
                <p className="text-sm text-gray-700 mt-1">{comment.content}</p>
                {!comment.resolved && (
                  <button
                    onClick={() => onResolveComment?.(rule.id, comment.id)}
                    className="text-xs text-blue-500 hover:text-blue-700 mt-2"
                  >
                    Mark as resolved
                  </button>
                )}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Review Info */}
      {rule.reviewedBy && (
        <div className="border-t border-gray-200 px-4 py-2 bg-gray-50 text-xs text-gray-500">
          Reviewed by {rule.reviewedBy} on {rule.reviewedAt && new Date(rule.reviewedAt).toLocaleDateString()}
        </div>
      )}

      {/* Reject Modal */}
      {showRejectModal && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
          <div className="bg-white rounded-lg p-6 max-w-md w-full mx-4">
            <h3 className="text-lg font-semibold mb-4">Reject Rule</h3>
            <p className="text-sm text-gray-600 mb-3">
              Please provide a reason for rejecting this rule:
            </p>
            <textarea
              value={rejectReason}
              onChange={(e) => setRejectReason(e.target.value)}
              placeholder="Reason for rejection..."
              className="w-full p-2 border rounded-md text-sm"
              rows={3}
            />
            <div className="flex justify-end gap-2 mt-4">
              <button
                onClick={() => setShowRejectModal(false)}
                className="px-4 py-2 text-sm text-gray-600 hover:text-gray-800"
              >
                Cancel
              </button>
              <button
                onClick={handleReject}
                disabled={!rejectReason.trim()}
                className="px-4 py-2 text-sm bg-red-500 text-white rounded hover:bg-red-600 disabled:opacity-50"
              >
                Reject Rule
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default RuleReviewCard;

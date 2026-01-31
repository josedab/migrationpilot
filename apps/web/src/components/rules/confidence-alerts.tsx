'use client';

/**
 * Confidence Alerts Component
 * 
 * Displays alerts for rules that need attention based on confidence thresholds
 */

import React from 'react';
import type { BusinessRule } from './rule-review-card';

interface ConfidenceAlertsProps {
  rules: BusinessRule[];
  confidenceThreshold?: number;
  criticalThreshold?: number;
  onRuleClick?: (ruleId: string) => void;
  onDismiss?: (ruleId: string) => void;
}

export function ConfidenceAlerts({
  rules,
  confidenceThreshold = 0.85,
  criticalThreshold = 0.5,
  onRuleClick,
  onDismiss,
}: ConfidenceAlertsProps) {
  // Get rules that need attention
  const criticalRules = rules.filter(
    r => r.confidence < criticalThreshold && r.status === 'pending'
  );
  const warningRules = rules.filter(
    r => r.confidence >= criticalThreshold && 
        r.confidence < confidenceThreshold && 
        r.status === 'pending'
  );
  const unreviewedLowConfidence = [...criticalRules, ...warningRules];

  if (unreviewedLowConfidence.length === 0) {
    return null;
  }

  return (
    <div className="space-y-3">
      {/* Critical Alerts */}
      {criticalRules.length > 0 && (
        <div className="bg-red-50 border-l-4 border-red-500 p-4 rounded-r-lg">
          <div className="flex items-start">
            <div className="flex-shrink-0">
              <span className="text-2xl">🚨</span>
            </div>
            <div className="ml-3 flex-1">
              <h3 className="text-sm font-semibold text-red-800">
                Critical: {criticalRules.length} Rule{criticalRules.length !== 1 ? 's' : ''} Need Immediate Review
              </h3>
              <p className="text-sm text-red-700 mt-1">
                These rules have confidence below {(criticalThreshold * 100).toFixed(0)}% and may contain significant errors.
              </p>
              <div className="mt-3 space-y-2">
                {criticalRules.slice(0, 3).map((rule) => (
                  <AlertRuleItem
                    key={rule.id}
                    rule={rule}
                    variant="critical"
                    onRuleClick={onRuleClick}
                    onDismiss={onDismiss}
                  />
                ))}
                {criticalRules.length > 3 && (
                  <button
                    className="text-sm text-red-700 hover:text-red-900 font-medium"
                    onClick={() => onRuleClick?.(criticalRules[0].id)}
                  >
                    + {criticalRules.length - 3} more critical rules
                  </button>
                )}
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Warning Alerts */}
      {warningRules.length > 0 && (
        <div className="bg-yellow-50 border-l-4 border-yellow-500 p-4 rounded-r-lg">
          <div className="flex items-start">
            <div className="flex-shrink-0">
              <span className="text-2xl">⚠️</span>
            </div>
            <div className="ml-3 flex-1">
              <h3 className="text-sm font-semibold text-yellow-800">
                Warning: {warningRules.length} Rule{warningRules.length !== 1 ? 's' : ''} Below Confidence Threshold
              </h3>
              <p className="text-sm text-yellow-700 mt-1">
                These rules have confidence below {(confidenceThreshold * 100).toFixed(0)}% and should be reviewed by an SME.
              </p>
              <div className="mt-3 space-y-2">
                {warningRules.slice(0, 3).map((rule) => (
                  <AlertRuleItem
                    key={rule.id}
                    rule={rule}
                    variant="warning"
                    onRuleClick={onRuleClick}
                    onDismiss={onDismiss}
                  />
                ))}
                {warningRules.length > 3 && (
                  <button
                    className="text-sm text-yellow-700 hover:text-yellow-900 font-medium"
                    onClick={() => onRuleClick?.(warningRules[0].id)}
                  >
                    + {warningRules.length - 3} more rules to review
                  </button>
                )}
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Summary */}
      <div className="bg-blue-50 border border-blue-200 p-3 rounded-lg">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <span className="text-lg">📊</span>
            <span className="text-sm font-medium text-blue-800">
              Review Progress
            </span>
          </div>
          <div className="flex items-center gap-4 text-sm">
            <span className="text-red-600">
              🔴 {criticalRules.length} Critical
            </span>
            <span className="text-yellow-600">
              🟡 {warningRules.length} Warning
            </span>
            <span className="text-green-600">
              🟢 {rules.filter(r => r.confidence >= confidenceThreshold).length} High Confidence
            </span>
          </div>
        </div>
        <div className="mt-2 h-2 bg-gray-200 rounded-full overflow-hidden">
          <div className="h-full flex">
            <div 
              className="bg-green-500 transition-all"
              style={{ width: `${(rules.filter(r => r.status === 'approved').length / rules.length) * 100}%` }}
            />
            <div 
              className="bg-yellow-500 transition-all"
              style={{ width: `${(rules.filter(r => r.status === 'needs_revision').length / rules.length) * 100}%` }}
            />
            <div 
              className="bg-red-500 transition-all"
              style={{ width: `${(rules.filter(r => r.status === 'rejected').length / rules.length) * 100}%` }}
            />
          </div>
        </div>
      </div>
    </div>
  );
}

// Alert Rule Item Component
interface AlertRuleItemProps {
  rule: BusinessRule;
  variant: 'critical' | 'warning';
  onRuleClick?: (ruleId: string) => void;
  onDismiss?: (ruleId: string) => void;
}

function AlertRuleItem({ rule, variant, onRuleClick, onDismiss }: AlertRuleItemProps) {
  const colors = {
    critical: 'bg-red-100 hover:bg-red-200 border-red-200',
    warning: 'bg-yellow-100 hover:bg-yellow-200 border-yellow-200',
  };

  return (
    <div className={`flex items-center justify-between p-2 rounded border ${colors[variant]} transition-colors`}>
      <div 
        className="flex-1 cursor-pointer"
        onClick={() => onRuleClick?.(rule.id)}
      >
        <div className="flex items-center gap-2">
          <span className="font-mono text-xs bg-white/50 px-1.5 py-0.5 rounded">
            {rule.id}
          </span>
          <span className="text-sm font-medium truncate">
            {rule.name}
          </span>
        </div>
        <div className="flex items-center gap-2 mt-1">
          <span className={`text-xs font-bold ${
            rule.confidence < 0.5 ? 'text-red-700' : 'text-yellow-700'
          }`}>
            {(rule.confidence * 100).toFixed(0)}% confidence
          </span>
          <span className="text-xs text-gray-500">
            • {rule.category}
          </span>
        </div>
      </div>
      <div className="flex items-center gap-2">
        <button
          onClick={() => onRuleClick?.(rule.id)}
          className="px-2 py-1 text-xs bg-white rounded border hover:bg-gray-50"
        >
          Review
        </button>
        {onDismiss && (
          <button
            onClick={() => onDismiss(rule.id)}
            className="p-1 text-gray-400 hover:text-gray-600"
            title="Dismiss"
          >
            ✕
          </button>
        )}
      </div>
    </div>
  );
}

export default ConfidenceAlerts;

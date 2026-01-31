'use client';

/**
 * Migration Progress Overview Component
 * 
 * Displays key metrics and progress for a migration project
 */

import React from 'react';

interface Metrics {
  totalModules: number;
  completedModules: number;
  inProgressModules: number;
  failedModules: number;
  totalLinesOfCode: number;
  migratedLinesOfCode: number;
  totalBusinessRules: number;
  validatedBusinessRules: number;
  averageComplexity: number;
  estimatedCompletion: string;
}

interface MigrationProgressProps {
  projectName: string;
  status: string;
  overallProgress: number;
  metrics: Metrics;
}

export function MigrationProgress({
  projectName,
  status,
  overallProgress,
  metrics,
}: MigrationProgressProps) {
  const statusColors: Record<string, string> = {
    'in_progress': 'bg-blue-100 text-blue-800',
    'completed': 'bg-green-100 text-green-800',
    'failed': 'bg-red-100 text-red-800',
    'paused': 'bg-yellow-100 text-yellow-800',
  };

  const progressColor = overallProgress >= 80 ? 'bg-green-500' : 
                       overallProgress >= 50 ? 'bg-blue-500' : 
                       overallProgress >= 25 ? 'bg-yellow-500' : 'bg-red-500';

  return (
    <div className="bg-white rounded-lg shadow-md p-6">
      <div className="flex items-center justify-between mb-6">
        <div>
          <h2 className="text-2xl font-bold text-gray-900">{projectName}</h2>
          <span className={`inline-block px-3 py-1 rounded-full text-sm font-medium mt-2 ${statusColors[status] || 'bg-gray-100 text-gray-800'}`}>
            {status.replace('_', ' ').toUpperCase()}
          </span>
        </div>
        <div className="text-right">
          <div className="text-4xl font-bold text-gray-900">{overallProgress}%</div>
          <div className="text-sm text-gray-500">Overall Progress</div>
        </div>
      </div>

      {/* Progress Bar */}
      <div className="w-full bg-gray-200 rounded-full h-4 mb-6">
        <div 
          className={`${progressColor} h-4 rounded-full transition-all duration-500`}
          style={{ width: `${overallProgress}%` }}
        />
      </div>

      {/* Metrics Grid */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <MetricCard
          label="Modules"
          value={`${metrics.completedModules}/${metrics.totalModules}`}
          subtext={`${metrics.inProgressModules} in progress`}
          icon="📦"
        />
        <MetricCard
          label="Lines of Code"
          value={formatNumber(metrics.migratedLinesOfCode)}
          subtext={`of ${formatNumber(metrics.totalLinesOfCode)}`}
          icon="📝"
        />
        <MetricCard
          label="Business Rules"
          value={`${metrics.validatedBusinessRules}/${metrics.totalBusinessRules}`}
          subtext="validated"
          icon="📋"
        />
        <MetricCard
          label="Avg Complexity"
          value={metrics.averageComplexity.toString()}
          subtext={getComplexityLabel(metrics.averageComplexity)}
          icon="📊"
        />
      </div>

      {/* Estimated Completion */}
      <div className="mt-6 pt-4 border-t border-gray-200">
        <div className="flex items-center justify-between text-sm">
          <span className="text-gray-500">Estimated Completion</span>
          <span className="font-medium text-gray-900">
            {new Date(metrics.estimatedCompletion).toLocaleDateString('en-US', {
              month: 'long',
              day: 'numeric',
              year: 'numeric',
            })}
          </span>
        </div>
      </div>
    </div>
  );
}

interface MetricCardProps {
  label: string;
  value: string;
  subtext: string;
  icon: string;
}

function MetricCard({ label, value, subtext, icon }: MetricCardProps) {
  return (
    <div className="bg-gray-50 rounded-lg p-4">
      <div className="flex items-center gap-2 mb-2">
        <span className="text-xl">{icon}</span>
        <span className="text-sm font-medium text-gray-600">{label}</span>
      </div>
      <div className="text-2xl font-bold text-gray-900">{value}</div>
      <div className="text-xs text-gray-500">{subtext}</div>
    </div>
  );
}

function formatNumber(num: number): string {
  if (num >= 1000000) {
    return (num / 1000000).toFixed(1) + 'M';
  }
  if (num >= 1000) {
    return (num / 1000).toFixed(1) + 'K';
  }
  return num.toString();
}

function getComplexityLabel(complexity: number): string {
  if (complexity < 20) return 'Low';
  if (complexity < 40) return 'Moderate';
  if (complexity < 60) return 'High';
  return 'Very High';
}

export default MigrationProgress;

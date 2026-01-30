import * as React from 'react';
import { cn } from '../utils.js';
import { Badge } from './badge.js';

export interface TestResult {
  id: string;
  name: string;
  category: string;
  status: 'passed' | 'failed' | 'skipped';
  legacyOutput?: string;
  modernOutput?: string;
  difference?: string;
  duration: number;
}

export interface TestResultsTableProps {
  results: TestResult[];
  summary?: {
    total: number;
    passed: number;
    failed: number;
    skipped: number;
  };
  onViewDetails?: (result: TestResult) => void;
  className?: string;
}

export function TestResultsTable({
  results,
  summary,
  onViewDetails,
  className,
}: TestResultsTableProps) {
  const [filter, setFilter] = React.useState<'all' | 'passed' | 'failed' | 'skipped'>('all');
  const [expandedId, setExpandedId] = React.useState<string | null>(null);

  const filteredResults = filter === 'all' 
    ? results 
    : results.filter(r => r.status === filter);

  const statusColors: Record<TestResult['status'], string> = {
    passed: 'bg-green-500',
    failed: 'bg-red-500',
    skipped: 'bg-slate-400',
  };

  return (
    <div className={cn('space-y-4', className)}>
      {/* Summary */}
      {summary && (
        <div className="flex gap-4 p-4 bg-slate-50 dark:bg-slate-800 rounded-lg">
          <div className="text-center">
            <div className="text-2xl font-bold">{summary.total}</div>
            <div className="text-xs text-slate-500">Total</div>
          </div>
          <div className="text-center">
            <div className="text-2xl font-bold text-green-500">{summary.passed}</div>
            <div className="text-xs text-slate-500">Passed</div>
          </div>
          <div className="text-center">
            <div className="text-2xl font-bold text-red-500">{summary.failed}</div>
            <div className="text-xs text-slate-500">Failed</div>
          </div>
          <div className="text-center">
            <div className="text-2xl font-bold text-slate-400">{summary.skipped}</div>
            <div className="text-xs text-slate-500">Skipped</div>
          </div>
          <div className="flex-1" />
          <div className="text-center">
            <div className="text-2xl font-bold text-blue-500">
              {summary.total > 0 ? Math.round((summary.passed / summary.total) * 100) : 0}%
            </div>
            <div className="text-xs text-slate-500">Pass Rate</div>
          </div>
        </div>
      )}

      {/* Filters */}
      <div className="flex gap-2">
        {(['all', 'passed', 'failed', 'skipped'] as const).map((f) => (
          <button
            key={f}
            onClick={() => setFilter(f)}
            className={cn(
              'px-3 py-1 text-sm rounded-full transition-colors',
              filter === f
                ? 'bg-slate-900 text-white dark:bg-slate-100 dark:text-slate-900'
                : 'bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700'
            )}
          >
            {f.charAt(0).toUpperCase() + f.slice(1)}
          </button>
        ))}
      </div>

      {/* Table */}
      <div className="border rounded-lg overflow-hidden">
        <table className="w-full">
          <thead>
            <tr className="bg-slate-50 dark:bg-slate-800 text-left">
              <th className="px-4 py-3 text-sm font-medium">Status</th>
              <th className="px-4 py-3 text-sm font-medium">Test Name</th>
              <th className="px-4 py-3 text-sm font-medium">Category</th>
              <th className="px-4 py-3 text-sm font-medium text-right">Duration</th>
            </tr>
          </thead>
          <tbody>
            {filteredResults.map((result) => (
              <React.Fragment key={result.id}>
                <tr
                  className={cn(
                    'border-t cursor-pointer hover:bg-slate-50 dark:hover:bg-slate-800/50',
                    expandedId === result.id && 'bg-slate-50 dark:bg-slate-800/50'
                  )}
                  onClick={() => setExpandedId(expandedId === result.id ? null : result.id)}
                >
                  <td className="px-4 py-3">
                    <span className={cn('w-2 h-2 rounded-full inline-block', statusColors[result.status])} />
                  </td>
                  <td className="px-4 py-3 font-medium">{result.name}</td>
                  <td className="px-4 py-3">
                    <Badge variant="outline">{result.category}</Badge>
                  </td>
                  <td className="px-4 py-3 text-right text-sm text-slate-500">
                    {result.duration}ms
                  </td>
                </tr>
                {expandedId === result.id && result.status === 'failed' && (
                  <tr className="border-t bg-slate-50 dark:bg-slate-800/30">
                    <td colSpan={4} className="px-4 py-4">
                      <div className="space-y-2 text-sm">
                        {result.legacyOutput && (
                          <div>
                            <span className="text-slate-500">Legacy output:</span>
                            <code className="ml-2 px-2 py-1 bg-slate-200 dark:bg-slate-700 rounded">
                              {result.legacyOutput}
                            </code>
                          </div>
                        )}
                        {result.modernOutput && (
                          <div>
                            <span className="text-slate-500">Modern output:</span>
                            <code className="ml-2 px-2 py-1 bg-slate-200 dark:bg-slate-700 rounded">
                              {result.modernOutput}
                            </code>
                          </div>
                        )}
                        {result.difference && (
                          <div className="text-red-500">
                            <span>Difference:</span> {result.difference}
                          </div>
                        )}
                        {onViewDetails && (
                          <button
                            onClick={(e) => {
                              e.stopPropagation();
                              onViewDetails(result);
                            }}
                            className="mt-2 px-3 py-1 text-sm bg-slate-900 text-white dark:bg-slate-100 dark:text-slate-900 rounded hover:opacity-80"
                          >
                            View Full Details
                          </button>
                        )}
                      </div>
                    </td>
                  </tr>
                )}
              </React.Fragment>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

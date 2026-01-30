import { cn } from '../utils.js';
import { Progress } from './progress.js';

export interface MigrationProgressProps {
  phases: Array<{
    id: string;
    name: string;
    status: 'pending' | 'in-progress' | 'complete' | 'error';
    progress?: number;
    description?: string;
  }>;
  currentPhase: string;
  overallProgress: number;
  className?: string;
}

const statusIcons: Record<string, string> = {
  pending: '○',
  'in-progress': '⋯',
  complete: '✓',
  error: '✗',
};

const statusColors: Record<string, string> = {
  pending: 'text-slate-400',
  'in-progress': 'text-blue-500',
  complete: 'text-green-500',
  error: 'text-red-500',
};

export function MigrationProgress({
  phases,
  currentPhase,
  overallProgress,
  className,
}: MigrationProgressProps) {
  return (
    <div className={cn('space-y-6', className)}>
      {/* Overall progress */}
      <div className="space-y-2">
        <div className="flex justify-between text-sm">
          <span className="text-slate-600 dark:text-slate-400">Overall Progress</span>
          <span className="font-medium">{overallProgress}%</span>
        </div>
        <Progress value={overallProgress} />
      </div>

      {/* Phase timeline */}
      <div className="relative">
        {/* Vertical line */}
        <div className="absolute left-4 top-8 bottom-8 w-0.5 bg-slate-200 dark:bg-slate-700" />

        <div className="space-y-4">
          {phases.map((phase, index) => {
            const isActive = phase.id === currentPhase;
            const isComplete = phase.status === 'complete';

            return (
              <div
                key={phase.id}
                className={cn(
                  'relative flex items-start gap-4 p-4 rounded-lg transition-colors',
                  isActive && 'bg-blue-50 dark:bg-blue-950',
                  isComplete && 'opacity-70'
                )}
              >
                {/* Status indicator */}
                <div
                  className={cn(
                    'relative z-10 flex items-center justify-center w-8 h-8 rounded-full border-2 text-sm font-semibold',
                    phase.status === 'complete' && 'bg-green-500 border-green-500 text-white',
                    phase.status === 'in-progress' && 'bg-blue-500 border-blue-500 text-white',
                    phase.status === 'error' && 'bg-red-500 border-red-500 text-white',
                    phase.status === 'pending' && 'bg-white dark:bg-slate-800 border-slate-300 dark:border-slate-600'
                  )}
                >
                  {phase.status === 'complete' ? '✓' : index + 1}
                </div>

                {/* Phase info */}
                <div className="flex-1 min-w-0">
                  <div className="flex items-center justify-between">
                    <h4 className={cn(
                      'font-medium',
                      isActive && 'text-blue-600 dark:text-blue-400'
                    )}>
                      {phase.name}
                    </h4>
                    <span className={cn('text-sm', statusColors[phase.status])}>
                      {statusIcons[phase.status]} {phase.status}
                    </span>
                  </div>
                  {phase.description && (
                    <p className="text-sm text-slate-500 mt-1">{phase.description}</p>
                  )}
                  {phase.status === 'in-progress' && phase.progress !== undefined && (
                    <div className="mt-2">
                      <Progress value={phase.progress} className="h-2" />
                    </div>
                  )}
                </div>
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
}

import { cn } from '../utils.js';
import { Card, CardHeader, CardTitle, CardDescription, CardContent, CardFooter } from './card.js';
import { Badge } from './badge.js';
import { Button } from './button.js';

export interface RuleCardProps {
  id: string;
  name: string;
  description: string;
  type: 'calculation' | 'validation' | 'decision' | 'temporal' | 'data-transform';
  confidence: number;
  status: 'pending' | 'approved' | 'rejected' | 'needs-review';
  sourceFile: string;
  sourceLines: string;
  onApprove?: () => void;
  onReject?: () => void;
  onViewDetails?: () => void;
  className?: string;
}

const typeColors: Record<RuleCardProps['type'], string> = {
  calculation: 'bg-blue-500',
  validation: 'bg-purple-500',
  decision: 'bg-orange-500',
  temporal: 'bg-cyan-500',
  'data-transform': 'bg-green-500',
};

const statusVariants: Record<RuleCardProps['status'], 'default' | 'success' | 'destructive' | 'warning'> = {
  pending: 'default',
  approved: 'success',
  rejected: 'destructive',
  'needs-review': 'warning',
};

export function RuleCard({
  id,
  name,
  description,
  type,
  confidence,
  status,
  sourceFile,
  sourceLines,
  onApprove,
  onReject,
  onViewDetails,
  className,
}: RuleCardProps) {
  const confidenceColor = confidence >= 90 ? 'text-green-500' : confidence >= 70 ? 'text-yellow-500' : 'text-red-500';

  return (
    <Card className={cn('hover:shadow-md transition-shadow', className)}>
      <CardHeader>
        <div className="flex items-start justify-between">
          <div className="space-y-1">
            <CardTitle className="text-lg flex items-center gap-2">
              <span className={cn('w-2 h-2 rounded-full', typeColors[type])} />
              {name}
            </CardTitle>
            <CardDescription className="text-xs text-slate-500">
              {id} • {sourceFile}:{sourceLines}
            </CardDescription>
          </div>
          <Badge variant={statusVariants[status]}>{status}</Badge>
        </div>
      </CardHeader>
      <CardContent>
        <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">
          {description}
        </p>
        <div className="flex items-center gap-4 text-sm">
          <div className="flex items-center gap-2">
            <span className="text-slate-500">Type:</span>
            <Badge variant="outline">{type}</Badge>
          </div>
          <div className="flex items-center gap-2">
            <span className="text-slate-500">Confidence:</span>
            <span className={cn('font-semibold', confidenceColor)}>{confidence}%</span>
          </div>
        </div>
      </CardContent>
      <CardFooter className="gap-2">
        {status === 'pending' || status === 'needs-review' ? (
          <>
            <Button size="sm" variant="outline" onClick={onReject}>
              Reject
            </Button>
            <Button size="sm" onClick={onApprove}>
              Approve
            </Button>
          </>
        ) : null}
        <Button size="sm" variant="ghost" onClick={onViewDetails}>
          View Details
        </Button>
      </CardFooter>
    </Card>
  );
}

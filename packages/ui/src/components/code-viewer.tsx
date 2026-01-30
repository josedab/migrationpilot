import { cn } from '../utils.js';

export interface CodeViewerProps {
  code: string;
  language: string;
  filename?: string;
  lineNumbers?: boolean;
  highlightLines?: number[];
  className?: string;
}

export function CodeViewer({
  code,
  language,
  filename,
  lineNumbers = true,
  highlightLines = [],
  className,
}: CodeViewerProps) {
  const lines = code.split('\n');

  return (
    <div className={cn('rounded-lg border bg-slate-950 overflow-hidden', className)}>
      {filename && (
        <div className="flex items-center gap-2 px-4 py-2 bg-slate-900 border-b border-slate-800">
          <span className="text-xs text-slate-400">{filename}</span>
          <span className="text-xs text-slate-500">({language})</span>
        </div>
      )}
      <div className="overflow-x-auto">
        <pre className="p-4 text-sm">
          <code>
            {lines.map((line, index) => {
              const lineNumber = index + 1;
              const isHighlighted = highlightLines.includes(lineNumber);
              
              return (
                <div
                  key={index}
                  className={cn(
                    'flex',
                    isHighlighted && 'bg-yellow-500/20 -mx-4 px-4'
                  )}
                >
                  {lineNumbers && (
                    <span className="select-none w-12 text-right pr-4 text-slate-500">
                      {lineNumber}
                    </span>
                  )}
                  <span className="text-slate-300 flex-1">{line || ' '}</span>
                </div>
              );
            })}
          </code>
        </pre>
      </div>
    </div>
  );
}

import * as React from 'react';
import { cn } from '../utils.js';
import { CodeViewer } from './code-viewer.js';

export interface CodeComparisonProps {
  legacyCode: string;
  legacyLanguage: string;
  legacyFilename?: string;
  modernCode: string;
  modernLanguage: string;
  modernFilename?: string;
  mappings?: Array<{
    legacyLines: number[];
    modernLines: number[];
    description?: string;
  }>;
  className?: string;
}

export function CodeComparison({
  legacyCode,
  legacyLanguage,
  legacyFilename,
  modernCode,
  modernLanguage,
  modernFilename,
  mappings = [],
  className,
}: CodeComparisonProps) {
  const [selectedMapping, setSelectedMapping] = React.useState<number | null>(null);

  const legacyHighlights = selectedMapping !== null 
    ? mappings[selectedMapping]?.legacyLines || []
    : [];
  
  const modernHighlights = selectedMapping !== null
    ? mappings[selectedMapping]?.modernLines || []
    : [];

  return (
    <div className={cn('flex flex-col gap-4', className)}>
      {/* Mapping legend */}
      {mappings.length > 0 && (
        <div className="flex flex-wrap gap-2 p-2 bg-slate-100 dark:bg-slate-800 rounded-lg">
          <span className="text-sm text-slate-600 dark:text-slate-400">Line mappings:</span>
          {mappings.map((mapping, index) => (
            <button
              key={index}
              onClick={() => setSelectedMapping(selectedMapping === index ? null : index)}
              className={cn(
                'px-2 py-1 text-xs rounded transition-colors',
                selectedMapping === index
                  ? 'bg-yellow-500 text-white'
                  : 'bg-slate-200 dark:bg-slate-700 hover:bg-slate-300 dark:hover:bg-slate-600'
              )}
            >
              {mapping.description || `L${mapping.legacyLines[0]} → M${mapping.modernLines[0]}`}
            </button>
          ))}
        </div>
      )}

      {/* Side-by-side code viewers */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
        <div>
          <h3 className="text-sm font-medium text-slate-600 dark:text-slate-400 mb-2">
            Legacy Code ({legacyLanguage})
          </h3>
          <CodeViewer
            code={legacyCode}
            language={legacyLanguage}
            filename={legacyFilename}
            highlightLines={legacyHighlights}
          />
        </div>
        <div>
          <h3 className="text-sm font-medium text-slate-600 dark:text-slate-400 mb-2">
            Modern Code ({modernLanguage})
          </h3>
          <CodeViewer
            code={modernCode}
            language={modernLanguage}
            filename={modernFilename}
            highlightLines={modernHighlights}
          />
        </div>
      </div>
    </div>
  );
}

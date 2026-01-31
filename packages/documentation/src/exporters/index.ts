/**
 * Exporters Module
 * 
 * Document export functionality for various formats
 */

export { MarkdownExporter } from './markdown-exporter.js';
export { HTMLExporter } from './html-exporter.js';
export { ConfluenceExporter } from './confluence-exporter.js';

import type { Document, ExportFormat, ExportOptions, ExportResult } from '../types.js';
import { MarkdownExporter } from './markdown-exporter.js';
import { HTMLExporter } from './html-exporter.js';
import { ConfluenceExporter } from './confluence-exporter.js';

/**
 * Export a document to the specified format
 */
export function exportDocument(
  document: Document,
  format: ExportFormat,
  options?: ExportOptions
): ExportResult {
  switch (format) {
    case 'markdown':
      return new MarkdownExporter().export(document, options);
    case 'html':
      return new HTMLExporter().export(document, options);
    case 'confluence':
      return new ConfluenceExporter().export(document, options);
    case 'pdf': {
      // PDF export would require additional dependencies (puppeteer, pdfkit, etc.)
      // For now, export to HTML and note that conversion to PDF is needed
      const htmlResult = new HTMLExporter().export(document, options);
      return {
        ...htmlResult,
        format: 'pdf',
        warnings: ['PDF generated as HTML. Use a PDF converter to create the final PDF.'],
      } as ExportResult;
    }
    case 'docx':
      return {
        success: false,
        format: 'docx',
        error: 'DOCX export not yet implemented. Use HTML export and convert.',
      };
    case 'notion':
      return {
        success: false,
        format: 'notion',
        error: 'Notion export not yet implemented. Use Markdown export.',
      };
    default:
      return {
        success: false,
        format,
        error: `Unsupported export format: ${format}`,
      };
  }
}

/**
 * Get supported export formats
 */
export function getSupportedFormats(): { format: ExportFormat; supported: boolean; notes?: string }[] {
  return [
    { format: 'markdown', supported: true },
    { format: 'html', supported: true },
    { format: 'pdf', supported: true, notes: 'Exported as HTML, requires conversion' },
    { format: 'confluence', supported: true },
    { format: 'docx', supported: false, notes: 'Coming soon' },
    { format: 'notion', supported: false, notes: 'Coming soon' },
  ];
}

/**
 * Document Templates
 * 
 * Pre-defined templates for different document types
 */

export { systemOverviewTemplate } from './system-overview.js';
export { dataDictionaryTemplate } from './data-dictionary.js';
export { businessRuleCatalogTemplate } from './business-rule-catalog.js';
export { apiSpecificationTemplate } from './api-specification.js';

import type { DocumentTemplate, DocumentType } from '../types.js';
import { systemOverviewTemplate } from './system-overview.js';
import { dataDictionaryTemplate } from './data-dictionary.js';
import { businessRuleCatalogTemplate } from './business-rule-catalog.js';
import { apiSpecificationTemplate } from './api-specification.js';

/**
 * Get template by document type
 */
export function getTemplate(type: DocumentType): DocumentTemplate | undefined {
  const templates: Record<DocumentType, DocumentTemplate | undefined> = {
    'system-overview': systemOverviewTemplate,
    'data-dictionary': dataDictionaryTemplate,
    'business-rule-catalog': businessRuleCatalogTemplate,
    'api-specification': apiSpecificationTemplate,
    'architecture-overview': undefined, // TODO: Implement
    'migration-runbook': undefined, // TODO: Implement
    'test-plan': undefined, // TODO: Implement
  };
  
  return templates[type];
}

/**
 * Get all available templates
 */
export function getAllTemplates(): DocumentTemplate[] {
  return [
    systemOverviewTemplate,
    dataDictionaryTemplate,
    businessRuleCatalogTemplate,
    apiSpecificationTemplate,
  ];
}

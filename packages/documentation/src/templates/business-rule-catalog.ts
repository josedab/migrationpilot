/**
 * Business Rule Catalog Template
 * 
 * Comprehensive catalog of all extracted business rules
 */

import type { DocumentTemplate } from '../types.js';

export const businessRuleCatalogTemplate: DocumentTemplate = {
  type: 'business-rule-catalog',
  name: 'Business Rule Catalog',
  description: 'Complete catalog of business rules extracted from the legacy system with plain-language explanations',
  
  sections: [
    {
      id: 'introduction',
      title: 'Introduction',
      description: 'Purpose and usage of this catalog',
      level: 1,
      required: true,
      contentType: 'static',
      staticContent: `This Business Rule Catalog documents all business rules extracted from the legacy system. Each rule includes:

- **Unique Identifier**: For tracking and reference
- **Category**: Type of rule (calculation, validation, decision, etc.)
- **Plain Language Description**: What the rule does in business terms
- **Formula/Logic**: Technical implementation details
- **Inputs/Outputs**: Data involved in the rule
- **Edge Cases**: Special conditions and exceptions
- **Confidence Score**: AI confidence in the extraction accuracy

## How to Use This Catalog

1. **Review for Accuracy**: SMEs should validate rules, especially those with lower confidence scores
2. **Identify Dependencies**: Rules may depend on each other - check the references
3. **Plan Testing**: Use edge cases to create test scenarios
4. **Guide Migration**: Rules inform how to implement logic in the new system`,
    },
    {
      id: 'executive-summary',
      title: 'Executive Summary',
      description: 'Overview of business rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'rulesSummary',
    },
    {
      id: 'rules-by-category',
      title: 'Rules by Category',
      description: 'Business rules organized by type',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'rulesByCategory',
      subsections: [
        {
          id: 'calculation-rules',
          title: 'Calculation Rules',
          description: 'Rules that compute values',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'calculationRules',
        },
        {
          id: 'validation-rules',
          title: 'Validation Rules',
          description: 'Rules that validate data',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'validationRules',
        },
        {
          id: 'decision-rules',
          title: 'Decision Rules',
          description: 'Rules that determine outcomes',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'decisionRules',
        },
        {
          id: 'transformation-rules',
          title: 'Transformation Rules',
          description: 'Rules that convert data formats',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'transformationRules',
        },
        {
          id: 'workflow-rules',
          title: 'Workflow Rules',
          description: 'Rules that control process flow',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'workflowRules',
        },
      ],
    },
    {
      id: 'critical-rules',
      title: 'Critical Business Rules',
      description: 'High-impact rules requiring careful migration',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'criticalRules',
    },
    {
      id: 'low-confidence-rules',
      title: 'Rules Requiring SME Review',
      description: 'Rules with lower confidence scores that need validation',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'lowConfidenceRules',
    },
    {
      id: 'rule-dependencies',
      title: 'Rule Dependencies',
      description: 'Relationships between rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'ruleDependencies',
    },
    {
      id: 'edge-cases-summary',
      title: 'Edge Cases Summary',
      description: 'All identified edge cases across rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'edgeCasesSummary',
    },
    {
      id: 'coverage-analysis',
      title: 'Coverage Analysis',
      description: 'How well business rules cover the codebase',
      level: 1,
      required: false,
      contentType: 'ai-generated',
      aiPrompt: 'Analyze the coverage of business rules against the codebase. Identify areas that may have undocumented rules or need further analysis.',
    },
    {
      id: 'appendix-full-catalog',
      title: 'Appendix: Full Rule Catalog',
      description: 'Complete list of all rules with full details',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'fullRuleCatalog',
    },
  ],
  
  variables: [
    { name: 'projectName', type: 'string', description: 'Name of the project', required: true },
    { name: 'totalRules', type: 'number', description: 'Total number of business rules', required: true },
    { name: 'avgConfidence', type: 'number', description: 'Average confidence score', required: true },
    { name: 'reviewedRules', type: 'number', description: 'Number of reviewed rules', required: true },
  ],
  
  outputFormats: ['markdown', 'html', 'pdf', 'confluence'],
};

export default businessRuleCatalogTemplate;

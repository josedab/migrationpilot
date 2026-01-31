/**
 * System Overview Template
 * 
 * Generates a comprehensive overview document for a legacy system
 */

import type { DocumentTemplate } from '../types.js';

export const systemOverviewTemplate: DocumentTemplate = {
  type: 'system-overview',
  name: 'System Overview',
  description: 'Comprehensive overview of the legacy system including architecture, key components, and migration considerations',
  
  sections: [
    {
      id: 'executive-summary',
      title: 'Executive Summary',
      description: 'High-level summary of the system and migration scope',
      level: 1,
      required: true,
      contentType: 'ai-generated',
      aiPrompt: `Write an executive summary for a legacy {{sourceLanguage}} system migration to {{targetLanguage}}. 
Include:
- System purpose and business value
- Migration scope ({{totalFiles}} files, {{totalLines}} lines of code)
- Key challenges and risks
- Recommended approach
Keep it concise and business-focused.`,
    },
    {
      id: 'system-context',
      title: 'System Context',
      description: 'Business context and system purpose',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'systemContext',
      subsections: [
        {
          id: 'business-purpose',
          title: 'Business Purpose',
          description: 'What the system does and why it matters',
          level: 2,
          required: true,
          contentType: 'ai-generated',
          aiPrompt: 'Describe the business purpose of this system based on the extracted business rules and data structures.',
        },
        {
          id: 'stakeholders',
          title: 'Stakeholders',
          description: 'Who uses and maintains the system',
          level: 2,
          required: false,
          contentType: 'static',
          staticContent: '| Role | Description |\n|------|-------------|\n| Business Users | Primary users of the system |\n| IT Operations | Maintain and support the system |\n| Developers | Implement changes and fixes |',
        },
      ],
    },
    {
      id: 'technical-overview',
      title: 'Technical Overview',
      description: 'Technical architecture and components',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'technicalOverview',
      subsections: [
        {
          id: 'technology-stack',
          title: 'Technology Stack',
          description: 'Languages, frameworks, and platforms',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'technologyStack',
        },
        {
          id: 'program-inventory',
          title: 'Program Inventory',
          description: 'List of all programs and modules',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'programInventory',
        },
        {
          id: 'data-assets',
          title: 'Data Assets',
          description: 'Files, databases, and data stores',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'dataAssets',
        },
        {
          id: 'external-interfaces',
          title: 'External Interfaces',
          description: 'Integrations with other systems',
          level: 2,
          required: false,
          contentType: 'dynamic',
          dynamicContentKey: 'externalInterfaces',
        },
      ],
    },
    {
      id: 'business-rules-summary',
      title: 'Business Rules Summary',
      description: 'Overview of extracted business rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'businessRulesSummary',
      subsections: [
        {
          id: 'rules-by-category',
          title: 'Rules by Category',
          description: 'Business rules grouped by type',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'rulesByCategory',
        },
        {
          id: 'critical-rules',
          title: 'Critical Business Rules',
          description: 'Most important rules requiring special attention',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'criticalRules',
        },
      ],
    },
    {
      id: 'complexity-analysis',
      title: 'Complexity Analysis',
      description: 'Code quality and complexity metrics',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'complexityAnalysis',
      subsections: [
        {
          id: 'metrics-summary',
          title: 'Metrics Summary',
          description: 'Key code quality metrics',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'metricsSummary',
        },
        {
          id: 'high-risk-components',
          title: 'High-Risk Components',
          description: 'Components requiring special attention',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'highRiskComponents',
        },
      ],
    },
    {
      id: 'migration-considerations',
      title: 'Migration Considerations',
      description: 'Key factors for migration planning',
      level: 1,
      required: true,
      contentType: 'ai-generated',
      aiPrompt: `Based on the analysis of this {{sourceLanguage}} system, provide migration considerations including:
- Recommended migration approach (big bang vs incremental)
- Key technical challenges
- Risk factors
- Suggested target architecture patterns for {{targetLanguage}}
- Testing strategy recommendations`,
    },
    {
      id: 'appendix',
      title: 'Appendix',
      description: 'Supporting details and references',
      level: 1,
      required: false,
      contentType: 'dynamic',
      dynamicContentKey: 'appendix',
    },
  ],
  
  variables: [
    { name: 'projectName', type: 'string', description: 'Name of the project', required: true },
    { name: 'sourceLanguage', type: 'string', description: 'Source language', required: true },
    { name: 'targetLanguage', type: 'string', description: 'Target language', required: true },
    { name: 'totalFiles', type: 'number', description: 'Total number of source files', required: true },
    { name: 'totalLines', type: 'number', description: 'Total lines of code', required: true },
    { name: 'generatedDate', type: 'date', description: 'Document generation date', required: true },
    { name: 'version', type: 'string', description: 'Document version', required: true, defaultValue: '1.0' },
  ],
  
  outputFormats: ['markdown', 'html', 'pdf', 'confluence'],
};

export default systemOverviewTemplate;

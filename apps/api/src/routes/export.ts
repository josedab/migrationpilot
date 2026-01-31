/**
 * Export Routes
 * 
 * API endpoints for exporting documentation and integrations
 */

import { Hono } from 'hono';
import { documentationExportService, type ExportFormat, type ExportOptions } from '../services/export.js';
import type { Project, BusinessRule } from '@migrationpilot/core';

export const exportRoutes = new Hono();

// Mock data store (in production, fetch from database)
const getProjectById = (id: string): Project | null => {
  return {
    id,
    name: 'Sample Project',
    description: 'A sample migration project',
    organizationId: 'org_1',
    sourceLanguage: 'cobol',
    targetLanguage: 'java',
    status: 'analysis_complete',
    settings: {
      enableStranglerFig: false,
      generateTests: true,
      generateDocumentation: true,
      humanReviewRequired: true,
      confidenceThreshold: 0.85,
    },
    statistics: {
      totalFiles: 10,
      totalLines: 5000,
      analyzedFiles: 10,
      analyzedLines: 5000,
      extractedRules: 25,
      generatedFiles: 0,
      generatedLines: 0,
      testsGenerated: 0,
      testsPassed: 0,
      equivalenceScore: 0,
    },
    createdAt: new Date(),
    updatedAt: new Date(),
    createdBy: 'user_1',
  };
};

const getRulesByProjectId = (projectId: string): BusinessRule[] => {
  return [
    {
      id: 'BR-001',
      projectId,
      name: 'Interest Rate Calculation',
      description: 'Calculates interest based on account type and balance',
      category: 'calculation',
      sourceFile: 'CALCINT.cbl',
      sourceLines: [245, 280],
      sourceCode: 'COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE * WS-YEARS.',
      inputs: [
        { name: 'principal', type: 'decimal', source: 'WS-PRINCIPAL' },
        { name: 'rate', type: 'decimal', source: 'WS-RATE' },
        { name: 'years', type: 'integer', source: 'WS-YEARS' },
      ],
      outputs: [{ name: 'interest', type: 'decimal' }],
      logic: 'Simple interest calculation',
      formula: 'interest = principal * rate * years',
      edgeCases: ['Zero principal', 'Negative years'],
      assumptions: ['Rate is annual'],
      confidence: 0.95,
      reviewStatus: 'approved',
      extractedAt: new Date(),
      extractedBy: 'archeologist-agent',
      version: 1,
    },
    {
      id: 'BR-002',
      projectId,
      name: 'Account Validation',
      description: 'Validates account balance is within limits',
      category: 'validation',
      sourceFile: 'ACCTVAL.cbl',
      sourceLines: [120, 145],
      sourceCode: 'IF WS-BALANCE < 0 SET VALIDATION-ERROR TO TRUE.',
      inputs: [{ name: 'balance', type: 'decimal', source: 'WS-BALANCE' }],
      outputs: [{ name: 'valid', type: 'boolean' }],
      logic: 'Balance must be non-negative',
      edgeCases: ['Zero balance', 'Maximum value'],
      assumptions: [],
      confidence: 0.88,
      reviewStatus: 'pending',
      extractedAt: new Date(),
      extractedBy: 'archeologist-agent',
      version: 1,
    },
  ];
};

// Export business rules
exportRoutes.post('/projects/:projectId/export', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<{
    format: ExportFormat;
    options?: Partial<ExportOptions>;
  }>();
  
  const project = getProjectById(projectId);
  if (!project) {
    return c.json({ success: false, error: 'Project not found' }, 404);
  }
  
  const rules = getRulesByProjectId(projectId);
  
  const options: ExportOptions = {
    format: body.format,
    includeSourceCode: body.options?.includeSourceCode ?? true,
    includeGeneratedCode: body.options?.includeGeneratedCode ?? false,
    includeTestResults: body.options?.includeTestResults ?? false,
    includeComments: body.options?.includeComments ?? true,
    confluenceConfig: body.options?.confluenceConfig,
  };
  
  const result = await documentationExportService.exportRules(project, rules, options);
  
  if (!result.success) {
    return c.json({ success: false, error: result.error }, 500);
  }
  
  // Return based on format
  if (result.format === 'confluence') {
    return c.json({
      success: true,
      data: {
        format: 'confluence',
        url: result.url,
      },
    });
  }
  
  // For downloadable formats
  const contentTypes: Record<ExportFormat, string> = {
    json: 'application/json',
    markdown: 'text/markdown',
    html: 'text/html',
    pdf: 'application/pdf',
    word: 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
    confluence: 'text/plain',
  };
  
  const extensions: Record<ExportFormat, string> = {
    json: 'json',
    markdown: 'md',
    html: 'html',
    pdf: 'pdf',
    word: 'docx',
    confluence: 'txt',
  };
  
  c.header('Content-Type', contentTypes[result.format]);
  c.header(
    'Content-Disposition',
    `attachment; filename="${project.name.replace(/[^a-z0-9]/gi, '-')}-rules.${extensions[result.format]}"`
  );
  
  return c.body(result.content as string);
});

// Preview export (returns content inline)
exportRoutes.post('/projects/:projectId/export/preview', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<{
    format: ExportFormat;
    ruleIds?: string[];
  }>();
  
  const project = getProjectById(projectId);
  if (!project) {
    return c.json({ success: false, error: 'Project not found' }, 404);
  }
  
  let rules = getRulesByProjectId(projectId);
  if (body.ruleIds) {
    rules = rules.filter(r => body.ruleIds!.includes(r.id));
  }
  
  const result = await documentationExportService.exportRules(project, rules, {
    format: body.format,
    includeSourceCode: true,
  });
  
  return c.json({
    success: result.success,
    data: {
      format: result.format,
      content: result.content,
      preview: true,
    },
    error: result.error,
  });
});

// Get available export formats
exportRoutes.get('/formats', async (c) => {
  return c.json({
    success: true,
    data: [
      { format: 'json', name: 'JSON', description: 'Machine-readable format' },
      { format: 'markdown', name: 'Markdown', description: 'Text-based documentation' },
      { format: 'html', name: 'HTML', description: 'Web page format' },
      { format: 'pdf', name: 'PDF', description: 'Printable document' },
      { format: 'word', name: 'Word', description: 'Microsoft Word document' },
      { format: 'confluence', name: 'Confluence', description: 'Atlassian Confluence page' },
      { format: 'github-issues', name: 'GitHub Issues', description: 'Create issues for rules needing review' },
    ],
  });
});

// Export rules needing review as GitHub Issues
exportRoutes.post('/projects/:projectId/export/github-issues', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<{
    repository: string;          // owner/repo format
    githubToken?: string;        // Optional, falls back to env
    confidenceThreshold?: number; // Export rules below this confidence
    labels?: string[];           // Additional labels to add
    assignees?: string[];        // GitHub usernames to assign
    dryRun?: boolean;            // Preview without creating issues
  }>();

  const project = getProjectById(projectId);
  if (!project) {
    return c.json({ success: false, error: 'Project not found' }, 404);
  }

  const rules = getRulesByProjectId(projectId);
  const threshold = body.confidenceThreshold ?? 0.9;
  
  // Filter rules needing review
  const rulesToExport = rules.filter(r => 
    r.confidence < threshold || r.reviewStatus === 'pending' || r.reviewStatus === 'needs_clarification'
  );

  if (rulesToExport.length === 0) {
    return c.json({
      success: true,
      data: {
        message: 'No rules need review based on current threshold',
        threshold,
        totalRules: rules.length,
        issuesCreated: 0,
      },
    });
  }

  // Generate issue content for each rule
  const issues = rulesToExport.map(rule => ({
    title: `[Business Rule Review] ${rule.name}`,
    body: generateIssueBody(rule, project),
    labels: [
      'business-rule',
      'needs-review',
      rule.category,
      ...(body.labels || []),
    ],
    assignees: body.assignees || [],
  }));

  if (body.dryRun) {
    return c.json({
      success: true,
      data: {
        dryRun: true,
        repository: body.repository,
        threshold,
        rulesToExport: rulesToExport.length,
        issues: issues.map(i => ({
          title: i.title,
          labels: i.labels,
          bodyPreview: i.body.slice(0, 500) + (i.body.length > 500 ? '...' : ''),
        })),
      },
    });
  }

  // Create issues via GitHub API
  const githubToken = body.githubToken || process.env.GITHUB_TOKEN;
  if (!githubToken) {
    return c.json({ 
      success: false, 
      error: 'GitHub token required. Provide in request body or set GITHUB_TOKEN environment variable.',
    }, 400);
  }

  const [owner, repo] = body.repository.split('/');
  if (!owner || !repo) {
    return c.json({ success: false, error: 'Invalid repository format. Use owner/repo' }, 400);
  }

  const createdIssues: Array<{ number: number; url: string; title: string }> = [];
  const errors: Array<{ rule: string; error: string }> = [];

  for (const issue of issues) {
    try {
      const response = await fetch(`https://api.github.com/repos/${owner}/${repo}/issues`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${githubToken}`,
          'Accept': 'application/vnd.github+json',
          'X-GitHub-Api-Version': '2022-11-28',
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(issue),
      });

      if (!response.ok) {
        const error = await response.text();
        errors.push({ rule: issue.title, error: `HTTP ${response.status}: ${error}` });
        continue;
      }

      const data = await response.json() as { number: number; html_url: string };
      createdIssues.push({
        number: data.number,
        url: data.html_url,
        title: issue.title,
      });
    } catch (err) {
      errors.push({ 
        rule: issue.title, 
        error: err instanceof Error ? err.message : 'Unknown error',
      });
    }
  }

  return c.json({
    success: errors.length === 0,
    data: {
      repository: body.repository,
      threshold,
      totalRules: rules.length,
      rulesExported: rulesToExport.length,
      issuesCreated: createdIssues.length,
      issues: createdIssues,
      errors: errors.length > 0 ? errors : undefined,
    },
  });
});

// Helper function to generate issue body
function generateIssueBody(rule: BusinessRule, project: Project): string {
  return `## Business Rule Review Required

**Project:** ${project.name}
**Source Language:** ${project.sourceLanguage.toUpperCase()}
**Target Language:** ${project.targetLanguage}

---

### Rule Details

| Property | Value |
|----------|-------|
| **Rule ID** | \`${rule.id}\` |
| **Name** | ${rule.name} |
| **Category** | ${rule.category} |
| **Confidence** | ${(rule.confidence * 100).toFixed(1)}% |
| **Status** | ${rule.reviewStatus} |
| **Source File** | \`${rule.sourceFile}\` |
| **Lines** | ${rule.sourceLines.join(' - ')} |

### Description

${rule.description}

### Business Logic

${rule.logic}

${rule.formula ? `### Formula\n\n\`\`\`\n${rule.formula}\n\`\`\`\n` : ''}

### Source Code

\`\`\`${project.sourceLanguage}
${rule.sourceCode}
\`\`\`

### Inputs

${rule.inputs.map(i => `- **${i.name}** (\`${i.type}\`) - Source: \`${i.source}\``).join('\n')}

### Outputs

${rule.outputs.map(o => `- **${o.name}** (\`${o.type}\`)`).join('\n')}

${rule.edgeCases.length > 0 ? `### Edge Cases\n\n${rule.edgeCases.map(e => `- ${e}`).join('\n')}\n` : ''}

${rule.assumptions.length > 0 ? `### Assumptions\n\n${rule.assumptions.map(a => `- ${a}`).join('\n')}\n` : ''}

---

### Review Actions

- [ ] Verify the extracted business logic is correct
- [ ] Confirm edge cases are properly identified
- [ ] Validate assumptions are accurate
- [ ] Approve or request corrections

**Extracted by:** ${rule.extractedBy}
**Extracted at:** ${new Date(rule.extractedAt).toISOString()}
`;
}

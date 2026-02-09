/**
 * Code Reviewer API Routes
 * AI-powered code review that validates changes against business rules
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { CodeReviewer } from '@migrationpilot/code-reviewer';
import type { BusinessRule } from '@migrationpilot/core';

export const codeReviewerRoutes = new Hono();

// Create reviewer instance
const reviewer = new CodeReviewer();

// In-memory cache for demo (would be database in production)
const rulesCache = new Map<string, BusinessRule[]>();
const reviewCache = new Map<string, unknown>();

// ============================================================================
// SCHEMA DEFINITIONS
// ============================================================================

const DiffChangeSchema = z.object({
  type: z.enum(['add', 'delete', 'context']),
  lineNumber: z.number(),
  content: z.string(),
});

const DiffHunkSchema = z.object({
  oldStart: z.number(),
  oldLines: z.number(),
  newStart: z.number(),
  newLines: z.number(),
  content: z.string(),
  changes: z.array(DiffChangeSchema),
});

const ChangedFileSchema = z.object({
  path: z.string(),
  status: z.enum(['added', 'modified', 'deleted', 'renamed']),
  additions: z.number(),
  deletions: z.number(),
  patch: z.string().optional(),
  hunks: z.array(DiffHunkSchema),
});

const PullRequestSchema = z.object({
  id: z.string(),
  number: z.number(),
  title: z.string(),
  description: z.string(),
  author: z.string(),
  baseBranch: z.string(),
  headBranch: z.string(),
  repository: z.string(),
  files: z.array(ChangedFileSchema),
  createdAt: z.string(),
  updatedAt: z.string(),
});

const ReviewOptionsSchema = z.object({
  strictMode: z.boolean().optional(),
  checkBusinessRules: z.boolean().optional(),
  checkCodeQuality: z.boolean().optional(),
  checkNamingConventions: z.boolean().optional(),
  checkDataIntegrity: z.boolean().optional(),
  minConfidenceThreshold: z.number().optional(),
  maxComments: z.number().optional(),
});

const ReviewRequestSchema = z.object({
  pr: PullRequestSchema,
  projectId: z.string(),
  options: ReviewOptionsSchema.optional(),
});

const AffectedRulesRequestSchema = z.object({
  pr: PullRequestSchema,
  projectId: z.string(),
});

// Config update schema (available for future use)
// @ts-expect-error Schema defined for future API expansion
const _ConfigUpdateSchema = z.object({
  minConfidenceToReport: z.number().min(0).max(1).optional(),
  maxFindingsPerFile: z.number().min(1).max(100).optional(),
  maxTotalFindings: z.number().min(1).max(500).optional(),
  includeSuggestedFixes: z.boolean().optional(),
  includeRuleReferences: z.boolean().optional(),
  includeConfidenceScores: z.boolean().optional(),
  ignoredPaths: z.array(z.string()).optional(),
  ignoredRuleCategories: z.array(z.string()).optional(),
});

// ============================================================================
// ROUTES
// ============================================================================

/**
 * POST /api/code-reviewer/review
 * Review a pull request against business rules
 */
codeReviewerRoutes.post('/review', async (c) => {
  const body = await c.req.json();
  const parsed = ReviewRequestSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { pr, projectId, options } = parsed.data;

    // Get rules for project
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    // Perform review
    const result = await reviewer.review({ pr, projectId, options }, rules);

    // Cache the result
    reviewCache.set(result.id, result);

    return c.json({ data: result });
  } catch (error) {
    return c.json({
      error: 'Review failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/code-reviewer/review/github
 * Review a PR and format output for GitHub
 */
codeReviewerRoutes.post('/review/github', async (c) => {
  const body = await c.req.json();
  const parsed = ReviewRequestSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { pr, projectId, options } = parsed.data;

    // Get rules for project
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    // Perform review
    const result = await reviewer.review({ pr, projectId, options }, rules);

    // Format for GitHub
    const output = reviewer.formatForGitHub(result);

    return c.json({
      data: {
        reviewId: result.id,
        output,
        summary: result.summary,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Review failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/code-reviewer/review/gitlab
 * Review a PR and format output for GitLab
 */
codeReviewerRoutes.post('/review/gitlab', async (c) => {
  const body = await c.req.json();
  const parsed = ReviewRequestSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { pr, projectId, options } = parsed.data;

    // Get rules for project
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    // Perform review
    const result = await reviewer.review({ pr, projectId, options }, rules);

    // Format for GitLab
    const output = reviewer.formatForGitLab(result);

    return c.json({
      data: {
        reviewId: result.id,
        output,
        summary: result.summary,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Review failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/code-reviewer/affected-rules
 * Find business rules affected by a PR without full review
 */
codeReviewerRoutes.post('/affected-rules', async (c) => {
  const body = await c.req.json();
  const parsed = AffectedRulesRequestSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { pr, projectId } = parsed.data;

    // Get rules for project
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    // Find affected rules
    const affectedRules = reviewer.findAffectedRules(pr, rules);

    return c.json({
      data: {
        projectId,
        prId: pr.id,
        affectedRules,
        totalRulesChecked: rules.length,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Analysis failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/code-reviewer/review/:reviewId
 * Get a cached review result
 */
codeReviewerRoutes.get('/review/:reviewId', (c) => {
  const { reviewId } = c.req.param();

  const result = reviewCache.get(reviewId);
  if (!result) {
    return c.json({ error: 'Review not found' }, 404);
  }

  return c.json({ data: result });
});

/**
 * GET /api/code-reviewer/rules/:projectId
 * Get business rules for a project
 */
codeReviewerRoutes.get('/rules/:projectId', (c) => {
  const { projectId } = c.req.param();

  let rules = rulesCache.get(projectId);
  if (!rules) {
    rules = createDemoRules(projectId);
    rulesCache.set(projectId, rules);
  }

  return c.json({
    data: {
      projectId,
      rules,
      count: rules.length,
    },
  });
});

/**
 * POST /api/code-reviewer/rules/:projectId
 * Add business rules for a project
 */
codeReviewerRoutes.post('/rules/:projectId', async (c) => {
  const { projectId } = c.req.param();
  const body = await c.req.json();

  if (!Array.isArray(body.rules)) {
    return c.json({ error: 'rules array required' }, 400);
  }

  // Get existing rules or create empty array
  const existingRules = rulesCache.get(projectId) || [];

  // Add new rules
  const newRules = [...existingRules, ...body.rules];
  rulesCache.set(projectId, newRules);

  return c.json({
    data: {
      projectId,
      addedCount: body.rules.length,
      totalCount: newRules.length,
    },
  });
});

/**
 * GET /api/code-reviewer/config
 * Get current reviewer configuration
 */
codeReviewerRoutes.get('/config', (c) => {
  // Return default config (in production, would get from reviewer instance)
  return c.json({
    data: {
      minConfidenceToReport: 0.6,
      maxFindingsPerFile: 10,
      maxTotalFindings: 50,
      businessRuleViolationSeverity: 'critical',
      calculationChangeSeverity: 'warning',
      namingConventionSeverity: 'info',
      includeSuggestedFixes: true,
      includeRuleReferences: true,
      includeConfidenceScores: true,
      ignoredPaths: ['test/', 'tests/', '__tests__/', '*.test.*', '*.spec.*'],
      ignoredRuleCategories: [],
    },
  });
});

/**
 * GET /api/code-reviewer/stats
 * Get review statistics
 */
codeReviewerRoutes.get('/stats', (c) => {
  const totalReviews = reviewCache.size;
  const totalProjects = rulesCache.size;
  const totalRules = Array.from(rulesCache.values()).reduce((sum, rules) => sum + rules.length, 0);

  return c.json({
    data: {
      totalReviews,
      totalProjects,
      totalRules,
      cacheSize: {
        reviews: totalReviews,
        projects: totalProjects,
      },
    },
  });
});

/**
 * GET /api/code-reviewer/supported-platforms
 * Get list of supported platforms
 */
codeReviewerRoutes.get('/supported-platforms', (c) => {
  return c.json({
    data: {
      platforms: [
        {
          id: 'github',
          name: 'GitHub',
          features: ['PR comments', 'Line-level comments', 'Review status'],
          formatMethod: 'formatForGitHub',
        },
        {
          id: 'gitlab',
          name: 'GitLab',
          features: ['MR comments', 'Line-level comments', 'Review status'],
          formatMethod: 'formatForGitLab',
        },
      ],
    },
  });
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function createDemoRules(projectId: string): BusinessRule[] {
  const now = new Date();
  return [
    {
      id: 'br_001',
      projectId,
      name: 'Interest Calculation',
      description: 'Calculate simple interest on loan principal using I = P × R × T formula',
      category: 'calculation',
      sourceFile: 'src/services/loan-calculator.ts',
      sourceLines: [100, 150],
      sourceCode: 'const interest = principal * rate * years;',
      inputs: [
        { name: 'principal', type: 'decimal', source: 'Loan Application' },
        { name: 'rate', type: 'decimal', source: 'Rate Table' },
        { name: 'years', type: 'integer', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'interest', type: 'decimal', description: 'Calculated interest amount' },
      ],
      logic: 'Interest = Principal × Rate × Years',
      formula: 'I = P × R × T',
      edgeCases: ['Zero principal', 'Negative rate', 'Zero years'],
      assumptions: ['Rate is annual percentage'],
      confidence: 0.95,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_002',
      projectId,
      name: 'Loan Eligibility Check',
      description: 'Determine if customer is eligible for loan based on credit score and income',
      category: 'validation',
      sourceFile: 'src/services/eligibility-checker.ts',
      sourceLines: [50, 100],
      sourceCode: 'if (creditScore >= 650 && income >= loanAmount / 3)',
      inputs: [
        { name: 'creditScore', type: 'integer', source: 'Credit Bureau' },
        { name: 'income', type: 'decimal', source: 'Customer Application' },
        { name: 'loanAmount', type: 'decimal', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'eligible', type: 'boolean', description: 'Whether customer is eligible' },
      ],
      logic: 'Customer is eligible if credit score >= 650 AND annual income >= loan amount / 3',
      edgeCases: ['No credit history', 'Self-employed income', 'Multiple income sources'],
      assumptions: ['Credit score from primary bureau', 'Income is annual'],
      confidence: 0.92,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_003',
      projectId,
      name: 'Risk Score Calculation',
      description: 'Calculate risk score based on credit score and debt-to-income ratio',
      category: 'calculation',
      sourceFile: 'src/services/risk-assessor.ts',
      sourceLines: [75, 120],
      sourceCode: 'const riskScore = (1000 - creditScore) / 10 + dtiRatio * 5;',
      inputs: [
        { name: 'creditScore', type: 'integer', source: 'Credit Bureau' },
        { name: 'dtiRatio', type: 'decimal', source: 'Calculated' },
      ],
      outputs: [
        { name: 'riskScore', type: 'decimal', description: 'Risk score 0-100' },
      ],
      logic: 'Risk Score = (1000 - Credit Score) / 10 + DTI Ratio × 5',
      formula: 'R = (1000 - C) / 10 + D × 5',
      edgeCases: ['Credit score over 850', 'DTI over 100%'],
      assumptions: ['Credit score 300-850 range', 'DTI as percentage'],
      confidence: 0.88,
      reviewStatus: 'pending',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_004',
      projectId,
      name: 'Payment Schedule Generation',
      description: 'Generate monthly payment schedule for approved loans',
      category: 'calculation',
      sourceFile: 'src/services/payment-scheduler.ts',
      sourceLines: [200, 280],
      sourceCode: 'const monthlyPayment = (principal + interest) / (years * 12);',
      inputs: [
        { name: 'principal', type: 'decimal', source: 'Loan Application' },
        { name: 'interest', type: 'decimal', source: 'Interest Calculation' },
        { name: 'years', type: 'integer', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'monthlyPayment', type: 'decimal', description: 'Monthly payment amount' },
        { name: 'schedule', type: 'array', description: 'Array of payment dates and amounts' },
      ],
      logic: 'Monthly Payment = (Principal + Total Interest) / (Years × 12)',
      edgeCases: ['Partial months', 'Early payoff'],
      assumptions: ['Fixed rate loan', '12 payments per year'],
      confidence: 0.90,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_005',
      projectId,
      name: 'Late Payment Penalty',
      description: 'Calculate late payment penalty based on days overdue',
      category: 'calculation',
      sourceFile: 'src/services/penalty-calculator.ts',
      sourceLines: [25, 50],
      sourceCode: 'const penalty = Math.min(monthlyPayment * 0.05 * daysLate, monthlyPayment * 0.15);',
      inputs: [
        { name: 'monthlyPayment', type: 'decimal', source: 'Payment Schedule' },
        { name: 'daysLate', type: 'integer', source: 'Payment Status' },
      ],
      outputs: [
        { name: 'penalty', type: 'decimal', description: 'Late payment penalty amount' },
      ],
      logic: 'Penalty = 5% per day overdue, max 15% of monthly payment',
      formula: 'P = min(M × 0.05 × D, M × 0.15)',
      edgeCases: ['Grace period', 'Waived penalties'],
      assumptions: ['5% daily rate', '15% maximum cap'],
      confidence: 0.85,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
  ];
}

/**
 * Natural Language Query API Routes
 * Ask questions about legacy systems in plain English
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { NLQueryEngine, IntentClassifier } from '@migrationpilot/nl-query';
import type { KnowledgeGraph, NodeType, BusinessRule } from '@migrationpilot/core';

export const nlQueryRoutes = new Hono();

const queryEngine = new NLQueryEngine();
const intentClassifier = new IntentClassifier();

// In-memory cache for demo (would be database in production)
const graphCache = new Map<string, KnowledgeGraph>();
const rulesCache = new Map<string, BusinessRule[]>();

// Schema definitions
const QuerySchema = z.object({
  question: z.string().min(3).max(500),
  projectId: z.string(),
  context: z.object({
    focusNodeId: z.string().optional(),
    focusNodeType: z.string().optional(),
    detailLevel: z.enum(['brief', 'normal', 'detailed']).optional(),
    includeCode: z.boolean().optional(),
  }).optional(),
  options: z.object({
    maxResults: z.number().optional(),
    includeSourceCode: z.boolean().optional(),
    includeRelatedNodes: z.boolean().optional(),
    includeDataFlow: z.boolean().optional(),
  }).optional(),
});

/**
 * POST /api/nl-query/ask
 * Ask a question about the legacy system
 */
nlQueryRoutes.post('/ask', async (c) => {
  const body = await c.req.json();
  const parsed = QuerySchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { question, projectId, context, options } = parsed.data;

    // Get or create demo graph
    let graph = graphCache.get(projectId);
    if (!graph) {
      graph = createDemoGraph(projectId);
      graphCache.set(projectId, graph);
    }

    // Get rules
    const rules = rulesCache.get(projectId) || createDemoRules();
    rulesCache.set(projectId, rules);

    // Execute query
    const response = await queryEngine.query(
      { question, projectId, context, options },
      graph,
      rules
    );

    return c.json({ data: response });
  } catch (error) {
    return c.json({
      error: 'Query failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/nl-query/classify
 * Classify the intent of a question (useful for debugging/understanding)
 */
nlQueryRoutes.post('/classify', async (c) => {
  const body = await c.req.json();
  const { question } = body;

  if (!question) {
    return c.json({ error: 'question required' }, 400);
  }

  try {
    const classification = intentClassifier.classify(question);

    return c.json({
      data: {
        question,
        classification,
        explanation: getIntentExplanation(classification.intent),
      },
    });
  } catch (error) {
    return c.json({
      error: 'Classification failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/nl-query/suggestions/:projectId
 * Get suggested questions based on the project's knowledge graph
 */
nlQueryRoutes.get('/suggestions/:projectId', async (c) => {
  const { projectId } = c.req.param();
  const context = c.req.query('context');

  try {
    // Get or create demo graph
    let graph = graphCache.get(projectId);
    if (!graph) {
      graph = createDemoGraph(projectId);
      graphCache.set(projectId, graph);
    }

    const suggestions = queryEngine.suggestQuestions(graph, context);

    return c.json({
      data: {
        suggestions,
        projectId,
        context: context || null,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to generate suggestions',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/nl-query/history/:projectId
 * Get conversation history for a project
 */
nlQueryRoutes.get('/history/:projectId', async (c) => {
  const { projectId } = c.req.param();

  try {
    const history = queryEngine.getConversationHistory(projectId);

    return c.json({
      data: {
        projectId,
        turns: history,
        count: history.length,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to get history',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/nl-query/examples
 * Get example questions for each query type
 */
nlQueryRoutes.get('/examples', (c) => {
  return c.json({
    data: {
      examples: [
        {
          type: 'code-location',
          question: 'Where is the interest calculation procedure?',
          description: 'Find code locations by name or function',
        },
        {
          type: 'data-flow',
          question: 'What uses the customer record?',
          description: 'Trace how data flows through the system',
        },
        {
          type: 'business-rule',
          question: 'What rule handles loan eligibility?',
          description: 'Find business rules and their logic',
        },
        {
          type: 'dependency',
          question: 'What does LOANPROC depend on?',
          description: 'Find dependencies for a component',
        },
        {
          type: 'impact',
          question: 'What would be affected if I change the interest calculation?',
          description: 'Analyze downstream impact of changes',
        },
        {
          type: 'explanation',
          question: 'Explain how the loan processing works',
          description: 'Get explanations of system components',
        },
        {
          type: 'search',
          question: 'Find all validation procedures',
          description: 'Search for components by criteria',
        },
        {
          type: 'summary',
          question: 'Give me an overview of this system',
          description: 'Get a high-level summary',
        },
      ],
    },
  });
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function getIntentExplanation(intent: string): string {
  const explanations: Record<string, string> = {
    'code-location': 'Looking for where code is located',
    'data-flow': 'Tracing how data moves through the system',
    'business-rule': 'Finding business rules and their logic',
    'dependency': 'Finding what a component depends on',
    'impact': 'Analyzing what would be affected by a change',
    'explanation': 'Getting an explanation of how something works',
    'comparison': 'Comparing two or more things',
    'search': 'Searching for components matching criteria',
    'summary': 'Getting a high-level overview',
    'unknown': 'Could not determine specific intent',
  };
  return explanations[intent] || 'Unknown intent type';
}

function createDemoGraph(projectId: string): KnowledgeGraph {
  const now = new Date().toISOString();
  const nodes = new Map();
  const edges = new Map();

  // Create comprehensive demo nodes
  const demoNodes = [
    { id: 'prog_loanproc', type: 'program', name: 'LOANPROC', description: 'Main loan processing program that handles loan applications' },
    { id: 'prog_custmgmt', type: 'program', name: 'CUSTMGMT', description: 'Customer management module' },
    { id: 'rule_interest', type: 'business-rule', name: 'Interest Calculation', description: 'Calculate loan interest based on principal, rate, and term. Uses simple interest formula.' },
    { id: 'rule_eligibility', type: 'business-rule', name: 'Eligibility Check', description: 'Determine if applicant meets loan criteria based on credit score and income' },
    { id: 'rule_risk', type: 'business-rule', name: 'Risk Assessment', description: 'Calculate risk score for loan application based on multiple factors' },
    { id: 'rule_payment', type: 'business-rule', name: 'Payment Schedule', description: 'Generate monthly payment schedule for approved loans' },
    { id: 'data_customer', type: 'data-structure', name: 'Customer Record', description: 'Customer information including name, address, credit history' },
    { id: 'data_loan', type: 'data-structure', name: 'Loan Application', description: 'Loan application details including amount, term, purpose' },
    { id: 'data_payment', type: 'data-structure', name: 'Payment Schedule', description: 'Generated payment schedule with dates and amounts' },
    { id: 'data_credit', type: 'data-structure', name: 'Credit Score', description: 'Customer credit score and history' },
    { id: 'proc_validate', type: 'procedure', name: 'VALIDATE-INPUT', description: 'Validate all input data before processing' },
    { id: 'proc_calc', type: 'procedure', name: 'CALCULATE-LOAN', description: 'Main calculation procedure for loan terms' },
    { id: 'proc_output', type: 'procedure', name: 'GENERATE-OUTPUT', description: 'Generate output documents and reports' },
    { id: 'proc_credit', type: 'procedure', name: 'CHECK-CREDIT', description: 'Check customer credit score' },
    { id: 'db_customers', type: 'database', name: 'CUSTDB', description: 'Customer master database' },
    { id: 'db_loans', type: 'database', name: 'LOANDB', description: 'Loan transactions database' },
    { id: 'ext_credit', type: 'external-system', name: 'Credit Bureau API', description: 'External credit check service' },
    { id: 'reg_tila', type: 'regulation', name: 'TILA Compliance', description: 'Truth in Lending Act requirements' },
    { id: 'reg_ecoa', type: 'regulation', name: 'ECOA Compliance', description: 'Equal Credit Opportunity Act requirements' },
  ];

  for (const node of demoNodes) {
    nodes.set(node.id, {
      ...node,
      properties: { category: node.type },
      metadata: {
        createdAt: now,
        updatedAt: now,
        confidence: Math.random() * 0.3 + 0.7,
        tags: [node.type],
        source: node.type === 'procedure' ? {
          file: `${node.name}.cbl`,
          startLine: Math.floor(Math.random() * 100) + 1,
          endLine: Math.floor(Math.random() * 50) + 150,
        } : undefined,
      },
    });
  }

  // Create comprehensive edges
  const demoEdges = [
    { source: 'prog_loanproc', target: 'proc_validate', type: 'contains' },
    { source: 'prog_loanproc', target: 'proc_calc', type: 'contains' },
    { source: 'prog_loanproc', target: 'proc_output', type: 'contains' },
    { source: 'prog_loanproc', target: 'proc_credit', type: 'contains' },
    { source: 'proc_validate', target: 'proc_calc', type: 'calls' },
    { source: 'proc_calc', target: 'proc_credit', type: 'calls' },
    { source: 'proc_calc', target: 'proc_output', type: 'calls' },
    { source: 'proc_calc', target: 'rule_interest', type: 'implements' },
    { source: 'proc_calc', target: 'rule_eligibility', type: 'implements' },
    { source: 'proc_calc', target: 'rule_risk', type: 'implements' },
    { source: 'proc_calc', target: 'rule_payment', type: 'implements' },
    { source: 'rule_interest', target: 'data_loan', type: 'uses' },
    { source: 'rule_interest', target: 'data_payment', type: 'modifies' },
    { source: 'rule_eligibility', target: 'data_customer', type: 'uses' },
    { source: 'rule_eligibility', target: 'data_credit', type: 'uses' },
    { source: 'rule_risk', target: 'data_customer', type: 'uses' },
    { source: 'rule_risk', target: 'data_credit', type: 'uses' },
    { source: 'rule_payment', target: 'data_loan', type: 'uses' },
    { source: 'proc_credit', target: 'ext_credit', type: 'depends-on' },
    { source: 'data_customer', target: 'db_customers', type: 'reads' },
    { source: 'data_loan', target: 'db_loans', type: 'reads' },
    { source: 'data_payment', target: 'db_loans', type: 'writes' },
    { source: 'reg_tila', target: 'rule_interest', type: 'affected-by' },
    { source: 'reg_tila', target: 'rule_payment', type: 'affected-by' },
    { source: 'reg_ecoa', target: 'rule_eligibility', type: 'affected-by' },
  ];

  for (const edge of demoEdges) {
    const id = `edge_${edge.source}_${edge.type}_${edge.target}`;
    edges.set(id, {
      id,
      source: edge.source,
      target: edge.target,
      type: edge.type,
      properties: {},
      weight: 1,
      confidence: Math.random() * 0.3 + 0.7,
      metadata: { createdAt: now },
    });
  }

  // Build indices
  const byType = new Map<NodeType, Set<string>>();
  const byTag = new Map<string, Set<string>>();
  const incomingEdges = new Map<string, Set<string>>();
  const outgoingEdges = new Map<string, Set<string>>();

  for (const node of nodes.values()) {
    if (!byType.has(node.type)) byType.set(node.type, new Set());
    byType.get(node.type)!.add(node.id);
    for (const tag of node.metadata.tags || []) {
      if (!byTag.has(tag)) byTag.set(tag, new Set());
      byTag.get(tag)!.add(node.id);
    }
  }

  for (const edge of edges.values()) {
    if (!outgoingEdges.has(edge.source)) outgoingEdges.set(edge.source, new Set());
    outgoingEdges.get(edge.source)!.add(edge.id);
    if (!incomingEdges.has(edge.target)) incomingEdges.set(edge.target, new Set());
    incomingEdges.get(edge.target)!.add(edge.id);
  }

  // Build statistics
  const nodesByType: Record<string, number> = {};
  const edgesByType: Record<string, number> = {};
  for (const [type, set] of byType) nodesByType[type] = set.size;
  for (const edge of edges.values()) edgesByType[edge.type] = (edgesByType[edge.type] || 0) + 1;

  return {
    id: `kg_${projectId}`,
    projectId,
    name: 'Loan Processing Knowledge Graph',
    version: 1,
    nodes,
    edges,
    indices: { byType, byTag, incomingEdges, outgoingEdges },
    statistics: { nodeCount: nodes.size, edgeCount: edges.size, nodesByType, edgesByType },
    createdAt: now,
    updatedAt: now,
  };
}

function createDemoRules(): BusinessRule[] {
  const now = new Date();
  return [
    {
      id: 'br_001',
      projectId: 'demo',
      name: 'Interest Calculation',
      description: 'Calculate simple interest on loan principal',
      category: 'calculation',
      sourceFile: 'CALCINT.cbl',
      sourceLines: [100, 150],
      sourceCode: 'COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE * WS-YEARS',
      inputs: [
        { name: 'Principal', type: 'decimal', source: 'Loan Application' },
        { name: 'Rate', type: 'decimal', source: 'Rate Table' },
        { name: 'Years', type: 'integer', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'Interest', type: 'decimal', description: 'Calculated interest amount' },
      ],
      logic: 'Interest = Principal × Rate × Years',
      formula: 'I = P × R × T',
      edgeCases: ['Zero principal', 'Negative rate'],
      assumptions: ['Rate is annual'],
      confidence: 0.95,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_002',
      projectId: 'demo',
      name: 'Loan Eligibility',
      description: 'Determine if customer is eligible for loan based on credit score and income',
      category: 'validation',
      sourceFile: 'LOANELIG.cbl',
      sourceLines: [200, 280],
      sourceCode: 'IF WS-CREDIT-SCORE >= 650 AND WS-INCOME >= WS-LOAN-AMOUNT / 3',
      inputs: [
        { name: 'Credit Score', type: 'integer', source: 'Credit Bureau' },
        { name: 'Income', type: 'decimal', source: 'Customer Application' },
        { name: 'Loan Amount', type: 'decimal', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'Eligible', type: 'boolean', description: 'Whether customer is eligible' },
      ],
      logic: 'Customer is eligible if credit score >= 650 AND income >= loan amount / 3',
      edgeCases: ['No credit history', 'Self-employed income'],
      assumptions: ['Credit score from primary bureau'],
      confidence: 0.92,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_003',
      projectId: 'demo',
      name: 'Risk Assessment',
      description: 'Calculate risk score based on multiple factors',
      category: 'calculation',
      sourceFile: 'RISKCALC.cbl',
      sourceLines: [300, 400],
      sourceCode: 'COMPUTE WS-RISK-SCORE = (1000 - WS-CREDIT-SCORE) / 10 + WS-DTI-RATIO * 5',
      inputs: [
        { name: 'Credit Score', type: 'integer', source: 'Credit Bureau' },
        { name: 'DTI Ratio', type: 'decimal', source: 'Calculated' },
      ],
      outputs: [
        { name: 'Risk Score', type: 'decimal', description: 'Risk score 0-100' },
      ],
      logic: 'Risk Score = (1000 - Credit Score) / 10 + DTI Ratio × 5',
      edgeCases: ['Credit score over 850', 'DTI over 100%'],
      assumptions: ['Credit score 300-850 range'],
      confidence: 0.88,
      reviewStatus: 'pending',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
  ];
}

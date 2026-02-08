/**
 * Natural Language Query Types
 */

import type { KnowledgeNode, KnowledgeEdge, BusinessRule, SourceLocation } from '@migrationpilot/core';

// ============================================================================
// QUERY TYPES
// ============================================================================

export interface NLQueryRequest {
  question: string;
  projectId: string;
  context?: QueryContext;
  options?: QueryOptions;
}

export interface QueryContext {
  // Focus area
  focusNodeId?: string;
  focusNodeType?: string;

  // Previous questions for follow-up
  conversationHistory?: ConversationTurn[];

  // User preferences
  detailLevel?: 'brief' | 'normal' | 'detailed';
  includeCode?: boolean;
}

export interface QueryOptions {
  maxResults?: number;
  includeSourceCode?: boolean;
  includeRelatedNodes?: boolean;
  includeDataFlow?: boolean;
  language?: string;
}

export interface ConversationTurn {
  role: 'user' | 'assistant';
  content: string;
  timestamp: string;
}

// ============================================================================
// RESPONSE TYPES
// ============================================================================

export interface NLQueryResponse {
  id: string;
  question: string;
  answer: string;
  confidence: number;

  // Structured data backing the answer
  evidence: QueryEvidence;

  // Source references
  sources: SourceReference[];

  // Suggested follow-up questions
  followUpQuestions: string[];

  // Metadata
  queryType: QueryType;
  processingTimeMs: number;
  timestamp: string;
}

export type QueryType =
  | 'code-location'        // Where is X?
  | 'data-flow'            // What uses/modifies X?
  | 'business-rule'        // What rule handles X?
  | 'dependency'           // What depends on X?
  | 'impact'               // What would be affected if X changes?
  | 'explanation'          // Explain X
  | 'comparison'           // Compare X and Y
  | 'search'               // Find all X
  | 'summary'              // Summarize X
  | 'unknown';

export interface QueryEvidence {
  nodes: KnowledgeNode[];
  edges: KnowledgeEdge[];
  businessRules: BusinessRule[];
  codePaths: CodePath[];
  dataFlows: DataFlow[];
}

export interface CodePath {
  description: string;
  steps: CodePathStep[];
}

export interface CodePathStep {
  nodeId: string;
  nodeName: string;
  nodeType: string;
  description: string;
  location?: SourceLocation;
  codeSnippet?: string;
}

export interface DataFlow {
  dataItem: string;
  producers: FlowNode[];
  consumers: FlowNode[];
  transformations: FlowTransformation[];
}

export interface FlowNode {
  nodeId: string;
  nodeName: string;
  nodeType: string;
  operation: 'read' | 'write' | 'transform';
}

export interface FlowTransformation {
  sourceNode: string;
  targetNode: string;
  description: string;
  businessRuleId?: string;
}

export interface SourceReference {
  nodeId: string;
  nodeName: string;
  nodeType: string;
  location?: SourceLocation;
  relevance: number;
  snippet?: string;
}

// ============================================================================
// INTENT CLASSIFICATION
// ============================================================================

export interface IntentClassification {
  intent: QueryType;
  confidence: number;
  entities: ExtractedEntity[];
  keywords: string[];
}

export interface ExtractedEntity {
  type: 'procedure' | 'data' | 'rule' | 'file' | 'system' | 'concept';
  value: string;
  position: [number, number];
  confidence: number;
}

// ============================================================================
// QUERY ENGINE INTERFACE
// ============================================================================

export interface INLQueryEngine {
  query(request: NLQueryRequest): Promise<NLQueryResponse>;
  classifyIntent(question: string): Promise<IntentClassification>;
  suggestQuestions(projectId: string, context?: string): Promise<string[]>;
  getConversationContext(conversationId: string): ConversationTurn[];
}

/**
 * Natural Language Query Engine
 * Processes natural language questions and returns structured answers
 */

import type {
  NLQueryRequest,
  NLQueryResponse,
  IntentClassification,
  QueryEvidence,
  SourceReference,
  CodePath,
  DataFlow,
  ConversationTurn,
} from './types.js';
import type {
  KnowledgeGraph,
  KnowledgeNode,
  KnowledgeEdge,
  BusinessRule,
} from '@migrationpilot/core';
import { IntentClassifier } from './intent-classifier.js';

export class NLQueryEngine {
  private intentClassifier: IntentClassifier;
  private conversationHistory: Map<string, ConversationTurn[]> = new Map();

  constructor() {
    this.intentClassifier = new IntentClassifier();
  }

  /**
   * Process a natural language query
   */
  async query(
    request: NLQueryRequest,
    graph: KnowledgeGraph,
    businessRules: BusinessRule[] = []
  ): Promise<NLQueryResponse> {
    const startTime = Date.now();

    // Classify intent
    const classification = this.intentClassifier.classify(request.question);

    // Execute query based on intent
    const evidence = await this.executeQuery(
      classification,
      graph,
      businessRules,
      request
    );

    // Generate natural language answer
    const answer = this.generateAnswer(classification, evidence, request);

    // Find source references
    const sources = this.extractSourceReferences(evidence);

    // Generate follow-up questions
    const followUpQuestions = this.generateFollowUpQuestions(
      classification,
      evidence,
      request.question
    );

    // Calculate confidence
    const confidence = this.calculateConfidence(classification, evidence);

    const response: NLQueryResponse = {
      id: `nlq_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`,
      question: request.question,
      answer,
      confidence,
      evidence,
      sources,
      followUpQuestions,
      queryType: classification.intent,
      processingTimeMs: Date.now() - startTime,
      timestamp: new Date().toISOString(),
    };

    // Store in conversation history
    this.addToConversation(request.projectId, {
      role: 'user',
      content: request.question,
      timestamp: new Date().toISOString(),
    });
    this.addToConversation(request.projectId, {
      role: 'assistant',
      content: answer,
      timestamp: new Date().toISOString(),
    });

    return response;
  }

  /**
   * Classify the intent of a question
   */
  classifyIntent(question: string): IntentClassification {
    return this.intentClassifier.classify(question);
  }

  /**
   * Suggest questions based on the graph
   */
  suggestQuestions(graph: KnowledgeGraph, context?: string): string[] {
    const suggestions: string[] = [];

    // Get node types present in the graph
    const nodeTypes = graph.indices.byType;

    if (nodeTypes.has('business-rule')) {
      suggestions.push('What business rules handle interest calculation?');
      suggestions.push('Show me all validation rules');
      suggestions.push('What rules would be affected if I change customer data?');
    }

    if (nodeTypes.has('procedure')) {
      suggestions.push('Where is the main calculation procedure?');
      suggestions.push('What procedures call the validation function?');
      suggestions.push('Explain how the processing workflow works');
    }

    if (nodeTypes.has('data-structure')) {
      suggestions.push('What uses the customer record?');
      suggestions.push('Trace the data flow for payment information');
      suggestions.push('Which fields are used in loan calculations?');
    }

    if (nodeTypes.has('external-system')) {
      suggestions.push('What systems depend on external services?');
      suggestions.push('Show me all external integrations');
    }

    if (nodeTypes.has('regulation')) {
      suggestions.push('What code is affected by compliance requirements?');
      suggestions.push('List all regulated business rules');
    }

    // Add context-specific suggestions
    if (context) {
      suggestions.push(`Explain the ${context} in more detail`);
      suggestions.push(`What depends on ${context}?`);
      suggestions.push(`Show me code related to ${context}`);
    }

    return suggestions.slice(0, 10);
  }

  /**
   * Get conversation history
   */
  getConversationHistory(projectId: string): ConversationTurn[] {
    return this.conversationHistory.get(projectId) || [];
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private async executeQuery(
    classification: IntentClassification,
    graph: KnowledgeGraph,
    businessRules: BusinessRule[],
    request: NLQueryRequest
  ): Promise<QueryEvidence> {
    const evidence: QueryEvidence = {
      nodes: [],
      edges: [],
      businessRules: [],
      codePaths: [],
      dataFlows: [],
    };

    const keywords = classification.keywords;
    const entities = classification.entities;

    switch (classification.intent) {
      case 'code-location':
        evidence.nodes = this.findNodesByKeywords(graph, keywords, entities);
        break;

      case 'data-flow':
        const dataNodes = this.findNodesByType(graph, 'data-structure', keywords);
        for (const node of dataNodes) {
          const flow = this.traceDataFlow(graph, node.id);
          evidence.dataFlows.push(flow);
        }
        evidence.nodes = dataNodes;
        break;

      case 'business-rule':
        evidence.businessRules = this.findMatchingRules(businessRules, keywords);
        evidence.nodes = this.findNodesByType(graph, 'business-rule', keywords);
        break;

      case 'dependency':
        const targetNodes = this.findNodesByKeywords(graph, keywords, entities);
        for (const node of targetNodes) {
          const deps = this.findDependencies(graph, node.id);
          evidence.nodes.push(...deps.nodes);
          evidence.edges.push(...deps.edges);
        }
        break;

      case 'impact':
        const impactTargets = this.findNodesByKeywords(graph, keywords, entities);
        for (const node of impactTargets) {
          const impact = this.analyzeImpact(graph, node.id);
          evidence.nodes.push(...impact.nodes);
          evidence.edges.push(...impact.edges);
          evidence.codePaths.push(...impact.paths);
        }
        break;

      case 'explanation':
        evidence.nodes = this.findNodesByKeywords(graph, keywords, entities);
        if (evidence.nodes.length > 0 && evidence.nodes[0]) {
          evidence.codePaths = this.buildCodePaths(graph, evidence.nodes[0].id);
        }
        break;

      case 'search':
        evidence.nodes = this.searchNodes(graph, keywords, entities, request.options?.maxResults);
        break;

      case 'summary':
        evidence.nodes = this.getGraphSummaryNodes(graph);
        evidence.businessRules = businessRules.slice(0, 5);
        break;

      default:
        // Generic search
        evidence.nodes = this.findNodesByKeywords(graph, keywords, entities);
    }

    return evidence;
  }

  private findNodesByKeywords(
    graph: KnowledgeGraph,
    keywords: string[],
    entities: IntentClassification['entities']
  ): KnowledgeNode[] {
    const matches: { node: KnowledgeNode; score: number }[] = [];

    for (const node of graph.nodes.values()) {
      let score = 0;

      // Check entity matches
      for (const entity of entities) {
        if (node.name.toLowerCase().includes(entity.value.toLowerCase())) {
          score += 20;
        }
        if (node.description?.toLowerCase().includes(entity.value.toLowerCase())) {
          score += 10;
        }
      }

      // Check keyword matches
      for (const keyword of keywords) {
        if (node.name.toLowerCase().includes(keyword)) {
          score += 5;
        }
        if (node.description?.toLowerCase().includes(keyword)) {
          score += 3;
        }
        if (node.metadata.tags?.some(t => t.toLowerCase().includes(keyword))) {
          score += 2;
        }
      }

      if (score > 0) {
        matches.push({ node, score });
      }
    }

    // Sort by score and return top matches
    return matches
      .sort((a, b) => b.score - a.score)
      .slice(0, 20)
      .map(m => m.node);
  }

  private findNodesByType(
    graph: KnowledgeGraph,
    type: string,
    keywords: string[]
  ): KnowledgeNode[] {
    const typeNodes = graph.indices.byType.get(type as any);
    if (!typeNodes) return [];

    const nodes: KnowledgeNode[] = [];
    for (const nodeId of typeNodes) {
      const node = graph.nodes.get(nodeId);
      if (!node) continue;

      // If keywords provided, filter by them
      if (keywords.length > 0) {
        const matches = keywords.some(k =>
          node.name.toLowerCase().includes(k) ||
          node.description?.toLowerCase().includes(k)
        );
        if (matches) nodes.push(node);
      } else {
        nodes.push(node);
      }
    }

    return nodes;
  }

  private findMatchingRules(rules: BusinessRule[], keywords: string[]): BusinessRule[] {
    if (keywords.length === 0) return rules.slice(0, 10);

    return rules.filter(rule => {
      const text = `${rule.name} ${rule.description} ${rule.logic}`.toLowerCase();
      return keywords.some(k => text.includes(k));
    }).slice(0, 10);
  }

  private findDependencies(
    graph: KnowledgeGraph,
    nodeId: string
  ): { nodes: KnowledgeNode[]; edges: KnowledgeEdge[] } {
    const nodes: KnowledgeNode[] = [];
    const edges: KnowledgeEdge[] = [];
    const visited = new Set<string>();

    const traverse = (id: string, depth: number) => {
      if (depth > 3 || visited.has(id)) return;
      visited.add(id);

      const outgoing = graph.indices.outgoingEdges.get(id);
      if (outgoing) {
        for (const edgeId of outgoing) {
          const edge = graph.edges.get(edgeId);
          if (edge && ['calls', 'uses', 'depends-on', 'reads'].includes(edge.type)) {
            edges.push(edge);
            const targetNode = graph.nodes.get(edge.target);
            if (targetNode && !visited.has(targetNode.id)) {
              nodes.push(targetNode);
              traverse(edge.target, depth + 1);
            }
          }
        }
      }
    };

    traverse(nodeId, 0);
    return { nodes, edges };
  }

  private analyzeImpact(
    graph: KnowledgeGraph,
    nodeId: string
  ): { nodes: KnowledgeNode[]; edges: KnowledgeEdge[]; paths: CodePath[] } {
    const nodes: KnowledgeNode[] = [];
    const edges: KnowledgeEdge[] = [];
    const paths: CodePath[] = [];
    const visited = new Set<string>();

    const traverse = (id: string, path: KnowledgeNode[], depth: number) => {
      if (depth > 4 || visited.has(id)) return;
      visited.add(id);

      const incoming = graph.indices.incomingEdges.get(id);
      if (incoming) {
        for (const edgeId of incoming) {
          const edge = graph.edges.get(edgeId);
          if (edge) {
            edges.push(edge);
            const sourceNode = graph.nodes.get(edge.source);
            if (sourceNode && !visited.has(sourceNode.id)) {
              nodes.push(sourceNode);
              traverse(edge.source, [...path, sourceNode], depth + 1);
            }
          }
        }
      }

      // Build path if we found dependencies
      if (path.length > 1 && path[0]) {
        paths.push({
          description: `Impact path from ${path[0].name}`,
          steps: path.map(n => ({
            nodeId: n.id,
            nodeName: n.name,
            nodeType: n.type,
            description: n.description || '',
          })),
        });
      }
    };

    const startNode = graph.nodes.get(nodeId);
    if (startNode) {
      traverse(nodeId, [startNode], 0);
    }

    return { nodes, edges, paths };
  }

  private traceDataFlow(graph: KnowledgeGraph, dataNodeId: string): DataFlow {
    const producers: DataFlow['producers'] = [];
    const consumers: DataFlow['consumers'] = [];
    const transformations: DataFlow['transformations'] = [];

    const node = graph.nodes.get(dataNodeId);

    // Find incoming edges (producers)
    const incoming = graph.indices.incomingEdges.get(dataNodeId);
    if (incoming) {
      for (const edgeId of incoming) {
        const edge = graph.edges.get(edgeId);
        if (edge) {
          const sourceNode = graph.nodes.get(edge.source);
          if (sourceNode) {
            producers.push({
              nodeId: sourceNode.id,
              nodeName: sourceNode.name,
              nodeType: sourceNode.type,
              operation: edge.type === 'writes' ? 'write' : 'transform',
            });
          }
        }
      }
    }

    // Find outgoing edges (consumers)
    const outgoing = graph.indices.outgoingEdges.get(dataNodeId);
    if (outgoing) {
      for (const edgeId of outgoing) {
        const edge = graph.edges.get(edgeId);
        if (edge) {
          const targetNode = graph.nodes.get(edge.target);
          if (targetNode) {
            consumers.push({
              nodeId: targetNode.id,
              nodeName: targetNode.name,
              nodeType: targetNode.type,
              operation: edge.type === 'reads' ? 'read' : 'transform',
            });
          }
        }
      }
    }

    return {
      dataItem: node?.name || dataNodeId,
      producers,
      consumers,
      transformations,
    };
  }

  private buildCodePaths(graph: KnowledgeGraph, nodeId: string): CodePath[] {
    const paths: CodePath[] = [];
    const node = graph.nodes.get(nodeId);
    if (!node) return paths;

    // Build a simple path from this node
    const steps: CodePath['steps'] = [{
      nodeId: node.id,
      nodeName: node.name,
      nodeType: node.type,
      description: node.description || `${node.type}: ${node.name}`,
    }];

    // Add connected nodes
    const outgoing = graph.indices.outgoingEdges.get(nodeId);
    if (outgoing) {
      for (const edgeId of outgoing) {
        const edge = graph.edges.get(edgeId);
        if (edge) {
          const targetNode = graph.nodes.get(edge.target);
          if (targetNode) {
            steps.push({
              nodeId: targetNode.id,
              nodeName: targetNode.name,
              nodeType: targetNode.type,
              description: `${edge.type} → ${targetNode.name}`,
            });
          }
        }
      }
    }

    paths.push({
      description: `Code path starting from ${node.name}`,
      steps,
    });

    return paths;
  }

  private searchNodes(
    graph: KnowledgeGraph,
    keywords: string[],
    entities: IntentClassification['entities'],
    maxResults: number = 20
  ): KnowledgeNode[] {
    return this.findNodesByKeywords(graph, keywords, entities).slice(0, maxResults);
  }

  private getGraphSummaryNodes(graph: KnowledgeGraph): KnowledgeNode[] {
    const summary: KnowledgeNode[] = [];

    // Get representative nodes from each type
    for (const [_type, nodeIds] of graph.indices.byType) {
      const ids = Array.from(nodeIds);
      const firstId = ids[0];
      if (ids.length > 0 && firstId) {
        const node = graph.nodes.get(firstId);
        if (node) summary.push(node);
      }
    }

    return summary;
  }

  private generateAnswer(
    classification: IntentClassification,
    evidence: QueryEvidence,
    request: NLQueryRequest
  ): string {
    const { intent } = classification;
    const detailLevel = request.context?.detailLevel || 'normal';

    if (evidence.nodes.length === 0 && evidence.businessRules.length === 0) {
      return `I couldn't find specific information matching your question. Try rephrasing or being more specific about what you're looking for.`;
    }

    switch (intent) {
      case 'code-location':
        return this.formatLocationAnswer(evidence, detailLevel);

      case 'data-flow':
        return this.formatDataFlowAnswer(evidence, detailLevel);

      case 'business-rule':
        return this.formatBusinessRuleAnswer(evidence, detailLevel);

      case 'dependency':
        return this.formatDependencyAnswer(evidence, detailLevel);

      case 'impact':
        return this.formatImpactAnswer(evidence, detailLevel);

      case 'explanation':
        return this.formatExplanationAnswer(evidence, detailLevel);

      case 'search':
        return this.formatSearchAnswer(evidence, detailLevel);

      case 'summary':
        return this.formatSummaryAnswer(evidence, detailLevel);

      default:
        return this.formatGenericAnswer(evidence, detailLevel);
    }
  }

  private formatLocationAnswer(evidence: QueryEvidence, level: string): string {
    const nodes = evidence.nodes;
    if (nodes.length === 0) return 'No matching code locations found.';

    if (level === 'brief') {
      return `Found in: ${nodes.map(n => n.name).join(', ')}`;
    }

    const parts = nodes.slice(0, 5).map(n => {
      const loc = n.metadata.source;
      const location = loc ? ` (${loc.file}:${loc.startLine})` : '';
      return `• **${n.name}** (${n.type})${location}: ${n.description || 'No description'}`;
    });

    return `I found ${nodes.length} relevant location(s):\n\n${parts.join('\n')}`;
  }

  private formatDataFlowAnswer(evidence: QueryEvidence, _level: string): string {
    if (evidence.dataFlows.length === 0) return 'No data flows found for the specified data.';

    const parts: string[] = [];
    for (const flow of evidence.dataFlows.slice(0, 3)) {
      parts.push(`**${flow.dataItem}**:`);
      if (flow.producers.length > 0) {
        parts.push(`  • Produced by: ${flow.producers.map(p => p.nodeName).join(', ')}`);
      }
      if (flow.consumers.length > 0) {
        parts.push(`  • Consumed by: ${flow.consumers.map(c => c.nodeName).join(', ')}`);
      }
    }

    return `Data flow analysis:\n\n${parts.join('\n')}`;
  }

  private formatBusinessRuleAnswer(evidence: QueryEvidence, level: string): string {
    const rules = evidence.businessRules;
    const ruleNodes = evidence.nodes.filter(n => n.type === 'business-rule');

    if (rules.length === 0 && ruleNodes.length === 0) {
      return 'No matching business rules found.';
    }

    const parts: string[] = [];

    for (const rule of rules.slice(0, 5)) {
      parts.push(`**${rule.name}** (${rule.category}):`);
      parts.push(`  ${rule.description}`);
      if (rule.formula && level !== 'brief') {
        parts.push(`  Formula: \`${rule.formula}\``);
      }
      parts.push(`  Confidence: ${Math.round(rule.confidence * 100)}%`);
    }

    for (const node of ruleNodes.slice(0, 5 - rules.length)) {
      parts.push(`**${node.name}**: ${node.description || 'Business rule'}`);
    }

    return `Found ${rules.length + ruleNodes.length} business rule(s):\n\n${parts.join('\n')}`;
  }

  private formatDependencyAnswer(evidence: QueryEvidence, _level: string): string {
    if (evidence.nodes.length === 0) return 'No dependencies found.';

    const deps = evidence.nodes.slice(0, 10);
    const parts = deps.map(n => `• **${n.name}** (${n.type})`);

    return `Found ${evidence.nodes.length} dependencies:\n\n${parts.join('\n')}`;
  }

  private formatImpactAnswer(evidence: QueryEvidence, level: string): string {
    if (evidence.nodes.length === 0) return 'No downstream impact detected.';

    const impacted = evidence.nodes.slice(0, 10);
    const parts = [`**${impacted.length} components would be affected:**\n`];
    parts.push(...impacted.map(n => `• ${n.name} (${n.type})`));

    if (evidence.codePaths.length > 0 && level === 'detailed') {
      parts.push('\n**Impact paths:**');
      for (const path of evidence.codePaths.slice(0, 3)) {
        parts.push(`  ${path.steps.map(s => s.nodeName).join(' → ')}`);
      }
    }

    return parts.join('\n');
  }

  private formatExplanationAnswer(evidence: QueryEvidence, level: string): string {
    const node = evidence.nodes[0];
    if (!node) return 'No information found to explain.';

    const parts = [
      `**${node.name}** is a ${node.type}.`,
      node.description || '',
    ];

    const firstPath = evidence.codePaths[0];
    if (evidence.codePaths.length > 0 && level !== 'brief' && firstPath) {
      if (firstPath.steps.length > 1) {
        parts.push(`\nIt connects to: ${firstPath.steps.slice(1).map(s => s.nodeName).join(', ')}`);
      }
    }

    return parts.filter(Boolean).join('\n');
  }

  private formatSearchAnswer(evidence: QueryEvidence, _level: string): string {
    if (evidence.nodes.length === 0) return 'No results found.';

    const parts = evidence.nodes.slice(0, 15).map(n =>
      `• **${n.name}** (${n.type})${n.description ? ': ' + n.description.substring(0, 60) : ''}`
    );

    return `Found ${evidence.nodes.length} result(s):\n\n${parts.join('\n')}`;
  }

  private formatSummaryAnswer(evidence: QueryEvidence, _level: string): string {
    const typeCount: Record<string, number> = {};
    for (const node of evidence.nodes) {
      typeCount[node.type] = (typeCount[node.type] || 0) + 1;
    }

    const parts = [
      '**System Overview:**\n',
      ...Object.entries(typeCount).map(([type, count]) => `• ${count} ${type}(s)`),
    ];

    if (evidence.businessRules.length > 0) {
      parts.push(`\n**Key Business Rules:**`);
      for (const rule of evidence.businessRules.slice(0, 3)) {
        parts.push(`• ${rule.name}: ${rule.description.substring(0, 80)}`);
      }
    }

    return parts.join('\n');
  }

  private formatGenericAnswer(evidence: QueryEvidence, _level: string): string {
    if (evidence.nodes.length === 0) return 'No relevant information found.';

    const parts = evidence.nodes.slice(0, 10).map(n =>
      `• **${n.name}** (${n.type}): ${n.description || 'No description'}`
    );

    return `Here's what I found:\n\n${parts.join('\n')}`;
  }

  private extractSourceReferences(evidence: QueryEvidence): SourceReference[] {
    const refs: SourceReference[] = [];

    for (const node of evidence.nodes.slice(0, 10)) {
      refs.push({
        nodeId: node.id,
        nodeName: node.name,
        nodeType: node.type,
        location: node.metadata.source,
        relevance: 0.8,
      });
    }

    return refs;
  }

  private generateFollowUpQuestions(
    classification: IntentClassification,
    evidence: QueryEvidence,
    _originalQuestion: string
  ): string[] {
    const questions: string[] = [];

    // Based on what we found, suggest related questions
    const firstNode = evidence.nodes[0];
    if (evidence.nodes.length > 0 && firstNode) {
      if (classification.intent !== 'impact') {
        questions.push(`What would be affected if ${firstNode.name} changes?`);
      }
      if (classification.intent !== 'dependency') {
        questions.push(`What does ${firstNode.name} depend on?`);
      }
      if (classification.intent !== 'data-flow' && firstNode.type === 'data-structure') {
        questions.push(`Show me the data flow for ${firstNode.name}`);
      }
      if (classification.intent !== 'explanation') {
        questions.push(`Explain how ${firstNode.name} works`);
      }
    }

    const firstRule = evidence.businessRules[0];
    if (evidence.businessRules.length > 0 && firstRule) {
      questions.push(`What data does ${firstRule.name} use?`);
      questions.push(`Where is ${firstRule.name} implemented?`);
    }

    return questions.slice(0, 4);
  }

  private calculateConfidence(
    classification: IntentClassification,
    evidence: QueryEvidence
  ): number {
    let confidence = classification.confidence;

    // Adjust based on evidence quality
    if (evidence.nodes.length === 0 && evidence.businessRules.length === 0) {
      confidence *= 0.3;
    } else if (evidence.nodes.length > 5) {
      confidence *= 1.1;
    }

    return Math.min(0.95, Math.max(0.1, confidence));
  }

  private addToConversation(projectId: string, turn: ConversationTurn): void {
    if (!this.conversationHistory.has(projectId)) {
      this.conversationHistory.set(projectId, []);
    }
    const history = this.conversationHistory.get(projectId)!;
    history.push(turn);

    // Keep only last 20 turns
    if (history.length > 20) {
      history.splice(0, history.length - 20);
    }
  }
}

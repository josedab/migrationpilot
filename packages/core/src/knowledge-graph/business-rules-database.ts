/**
 * Business Rules Database
 * 
 * A searchable, versioned database for business rules built on the knowledge graph.
 * Provides semantic search, categorization, and industry pattern templates.
 */

import { KnowledgeGraphBuilder } from '../knowledge-graph.js';

// ============================================================================
// Business Rule Database Types
// ============================================================================

export interface BusinessRuleRecord {
  id: string;
  name: string;
  description: string;
  category: RuleDatabaseCategory;
  subcategory?: string;
  domain: BusinessDomain;
  
  // Rule details
  logic: RuleLogic;
  inputs: RuleParameter[];
  outputs: RuleParameter[];
  preconditions: string[];
  postconditions: string[];
  
  // Source traceability
  sourceFile: string;
  sourceLines: [number, number];
  sourceCode: string;
  
  // Metadata
  confidence: number;
  complexity: RuleComplexity;
  criticality: 'low' | 'medium' | 'high' | 'critical';
  reviewStatus: 'pending' | 'approved' | 'rejected' | 'needs-sme';
  
  // Versioning
  version: number;
  createdAt: Date;
  updatedAt: Date;
  createdBy?: string;
  updatedBy?: string;
  
  // Relations
  relatedRules: string[];
  dependencies: string[];
  regulations: string[];
  tags: string[];
  
  // Industry patterns
  industryPattern?: IndustryPattern;
}

export type RuleDatabaseCategory = 
  | 'calculation'
  | 'validation'
  | 'transformation'
  | 'workflow'
  | 'authorization'
  | 'pricing'
  | 'eligibility'
  | 'notification'
  | 'reporting'
  | 'compliance';

export type BusinessDomain =
  | 'finance'
  | 'insurance'
  | 'healthcare'
  | 'retail'
  | 'manufacturing'
  | 'banking'
  | 'government'
  | 'telecommunications'
  | 'logistics'
  | 'general';

export interface RuleLogic {
  type: 'formula' | 'decision-table' | 'condition-action' | 'state-machine' | 'procedure';
  expression?: string;
  decisionTable?: DecisionTable;
  conditions?: Condition[];
  states?: StateTransition[];
  pseudocode?: string;
}

export interface DecisionTable {
  conditionColumns: string[];
  actionColumns: string[];
  rows: DecisionTableRow[];
}

export interface DecisionTableRow {
  conditions: (string | '*')[];
  actions: string[];
}

export interface Condition {
  expression: string;
  operator: 'eq' | 'ne' | 'gt' | 'lt' | 'gte' | 'lte' | 'in' | 'between' | 'matches';
  threshold?: unknown;
  action?: string;
}

export interface StateTransition {
  fromState: string;
  toState: string;
  trigger: string;
  guard?: string;
  action?: string;
}

export interface RuleParameter {
  name: string;
  type: string;
  description: string;
  constraints?: ParameterConstraint[];
  example?: unknown;
}

export interface ParameterConstraint {
  type: 'required' | 'min' | 'max' | 'pattern' | 'enum' | 'length' | 'precision';
  value: unknown;
}

export type RuleComplexity = 'simple' | 'moderate' | 'complex' | 'very-complex';

export interface IndustryPattern {
  patternId: string;
  patternName: string;
  industry: BusinessDomain;
  description: string;
  templateCode?: string;
  matchConfidence: number;
}

// ============================================================================
// Search & Query Types
// ============================================================================

export interface RuleSearchQuery {
  text?: string;
  categories?: RuleDatabaseCategory[];
  domains?: BusinessDomain[];
  tags?: string[];
  regulations?: string[];
  criticality?: ('low' | 'medium' | 'high' | 'critical')[];
  reviewStatus?: ('pending' | 'approved' | 'rejected' | 'needs-sme')[];
  minConfidence?: number;
  maxComplexity?: RuleComplexity;
  sourceFilePattern?: string;
  createdAfter?: Date;
  createdBefore?: Date;
  limit?: number;
  offset?: number;
  sortBy?: 'name' | 'confidence' | 'criticality' | 'complexity' | 'createdAt' | 'updatedAt';
  sortOrder?: 'asc' | 'desc';
}

export interface RuleSearchResult {
  rules: BusinessRuleRecord[];
  total: number;
  page: number;
  pageSize: number;
  facets: SearchFacets;
  queryTimeMs: number;
}

export interface SearchFacets {
  categories: Record<RuleDatabaseCategory, number>;
  domains: Record<BusinessDomain, number>;
  criticality: Record<string, number>;
  reviewStatus: Record<string, number>;
  tags: Record<string, number>;
}

export interface SemanticSearchQuery {
  naturalLanguage: string;
  limit?: number;
  minSimilarity?: number;
}

// ============================================================================
// Business Rules Database
// ============================================================================

export class BusinessRulesDatabase {
  private rules: Map<string, BusinessRuleRecord> = new Map();
  private graph: KnowledgeGraphBuilder;
  private industryPatterns: Map<string, IndustryPattern[]> = new Map();
  private searchIndex: SearchIndex;
  private versionHistory: Map<string, BusinessRuleRecord[]> = new Map();

  constructor(projectId: string) {
    this.graph = new KnowledgeGraphBuilder(projectId, 'Business Rules Knowledge Graph');
    this.searchIndex = new SearchIndex();
    this.initializeIndustryPatterns();
  }

  /**
   * Add a business rule to the database
   */
  addRule(rule: Omit<BusinessRuleRecord, 'id' | 'version' | 'createdAt' | 'updatedAt'>): BusinessRuleRecord {
    const id = `rule_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    
    const fullRule: BusinessRuleRecord = {
      ...rule,
      id,
      version: 1,
      createdAt: new Date(),
      updatedAt: new Date(),
    };

    // Store rule
    this.rules.set(id, fullRule);
    
    // Initialize version history
    this.versionHistory.set(id, [fullRule]);
    
    // Add to knowledge graph
    this.addRuleToGraph(fullRule);
    
    // Index for search
    this.searchIndex.indexRule(fullRule);
    
    // Match industry patterns
    const pattern = this.matchIndustryPattern(fullRule);
    if (pattern) {
      fullRule.industryPattern = pattern;
    }

    return fullRule;
  }

  /**
   * Update an existing business rule
   */
  updateRule(
    id: string,
    updates: Partial<Omit<BusinessRuleRecord, 'id' | 'version' | 'createdAt'>>
  ): BusinessRuleRecord | null {
    const existing = this.rules.get(id);
    if (!existing) return null;

    const updated: BusinessRuleRecord = {
      ...existing,
      ...updates,
      version: existing.version + 1,
      updatedAt: new Date(),
    };

    // Store updated rule
    this.rules.set(id, updated);
    
    // Add to version history
    const history = this.versionHistory.get(id) || [];
    history.push(updated);
    this.versionHistory.set(id, history);
    
    // Re-index
    this.searchIndex.indexRule(updated);

    return updated;
  }

  /**
   * Get a rule by ID
   */
  getRule(id: string): BusinessRuleRecord | null {
    return this.rules.get(id) || null;
  }

  /**
   * Get rule version history
   */
  getRuleHistory(id: string): BusinessRuleRecord[] {
    return this.versionHistory.get(id) || [];
  }

  /**
   * Search rules
   */
  searchRules(query: RuleSearchQuery): RuleSearchResult {
    const startTime = Date.now();
    let results = Array.from(this.rules.values());

    // Apply filters
    if (query.text) {
      const text = query.text.toLowerCase();
      results = results.filter(r =>
        r.name.toLowerCase().includes(text) ||
        r.description.toLowerCase().includes(text) ||
        r.tags.some(t => t.toLowerCase().includes(text))
      );
    }

    if (query.categories && query.categories.length > 0) {
      results = results.filter(r => query.categories!.includes(r.category));
    }

    if (query.domains && query.domains.length > 0) {
      results = results.filter(r => query.domains!.includes(r.domain));
    }

    if (query.tags && query.tags.length > 0) {
      results = results.filter(r => 
        query.tags!.some(t => r.tags.includes(t))
      );
    }

    if (query.regulations && query.regulations.length > 0) {
      results = results.filter(r =>
        query.regulations!.some(reg => r.regulations.includes(reg))
      );
    }

    if (query.criticality && query.criticality.length > 0) {
      results = results.filter(r => query.criticality!.includes(r.criticality));
    }

    if (query.reviewStatus && query.reviewStatus.length > 0) {
      results = results.filter(r => query.reviewStatus!.includes(r.reviewStatus));
    }

    if (query.minConfidence !== undefined) {
      results = results.filter(r => r.confidence >= query.minConfidence!);
    }

    if (query.maxComplexity) {
      const complexityOrder: RuleComplexity[] = ['simple', 'moderate', 'complex', 'very-complex'];
      const maxIdx = complexityOrder.indexOf(query.maxComplexity);
      results = results.filter(r => complexityOrder.indexOf(r.complexity) <= maxIdx);
    }

    if (query.sourceFilePattern) {
      const pattern = new RegExp(query.sourceFilePattern, 'i');
      results = results.filter(r => pattern.test(r.sourceFile));
    }

    if (query.createdAfter) {
      results = results.filter(r => r.createdAt >= query.createdAfter!);
    }

    if (query.createdBefore) {
      results = results.filter(r => r.createdAt <= query.createdBefore!);
    }

    // Calculate facets before pagination
    const facets = this.calculateFacets(results);

    // Sort
    const sortField = query.sortBy || 'name';
    const sortOrder = query.sortOrder || 'asc';
    results.sort((a, b) => {
      let aVal: unknown = a[sortField as keyof BusinessRuleRecord];
      let bVal: unknown = b[sortField as keyof BusinessRuleRecord];
      
      if (aVal instanceof Date) aVal = aVal.getTime();
      if (bVal instanceof Date) bVal = bVal.getTime();
      
      if (typeof aVal === 'string') aVal = aVal.toLowerCase();
      if (typeof bVal === 'string') bVal = bVal.toLowerCase();
      
      const comparison = aVal! < bVal! ? -1 : aVal! > bVal! ? 1 : 0;
      return sortOrder === 'asc' ? comparison : -comparison;
    });

    // Paginate
    const total = results.length;
    const offset = query.offset || 0;
    const limit = query.limit || 50;
    results = results.slice(offset, offset + limit);

    return {
      rules: results,
      total,
      page: Math.floor(offset / limit) + 1,
      pageSize: limit,
      facets,
      queryTimeMs: Date.now() - startTime,
    };
  }

  /**
   * Semantic search using natural language
   */
  semanticSearch(query: SemanticSearchQuery): BusinessRuleRecord[] {
    // Simplified semantic search - in production would use embeddings
    const keywords = this.extractKeywords(query.naturalLanguage);
    const limit = query.limit || 10;
    const minSimilarity = query.minSimilarity || 0.3;

    const scored: Array<{ rule: BusinessRuleRecord; score: number }> = [];

    for (const rule of this.rules.values()) {
      const score = this.calculateSimilarity(keywords, rule);
      if (score >= minSimilarity) {
        scored.push({ rule, score });
      }
    }

    scored.sort((a, b) => b.score - a.score);
    return scored.slice(0, limit).map(s => s.rule);
  }

  /**
   * Find related rules
   */
  findRelatedRules(ruleId: string, maxResults: number = 10): BusinessRuleRecord[] {
    const rule = this.rules.get(ruleId);
    if (!rule) return [];

    const related: Array<{ rule: BusinessRuleRecord; score: number }> = [];

    for (const [id, candidate] of this.rules) {
      if (id === ruleId) continue;

      let score = 0;

      // Same category bonus
      if (candidate.category === rule.category) score += 3;
      
      // Same domain bonus
      if (candidate.domain === rule.domain) score += 2;
      
      // Shared tags
      const sharedTags = rule.tags.filter(t => candidate.tags.includes(t)).length;
      score += sharedTags * 1.5;
      
      // Shared regulations
      const sharedRegs = rule.regulations.filter(r => candidate.regulations.includes(r)).length;
      score += sharedRegs * 2;
      
      // Input/output overlap
      const ruleInputNames = rule.inputs.map(i => i.name);
      const candidateInputNames = candidate.inputs.map(i => i.name);
      const inputOverlap = ruleInputNames.filter(n => candidateInputNames.includes(n)).length;
      score += inputOverlap;

      if (score > 0) {
        related.push({ rule: candidate, score });
      }
    }

    related.sort((a, b) => b.score - a.score);
    return related.slice(0, maxResults).map(r => r.rule);
  }

  /**
   * Get rules by regulation
   */
  getRulesByRegulation(regulation: string): BusinessRuleRecord[] {
    return Array.from(this.rules.values()).filter(r =>
      r.regulations.includes(regulation)
    );
  }

  /**
   * Get rules by domain and category
   */
  getRulesByDomainAndCategory(
    domain: BusinessDomain,
    category?: RuleDatabaseCategory
  ): BusinessRuleRecord[] {
    return Array.from(this.rules.values()).filter(r => {
      if (r.domain !== domain) return false;
      if (category && r.category !== category) return false;
      return true;
    });
  }

  /**
   * Import rules from external source
   */
  importRules(rules: Array<Omit<BusinessRuleRecord, 'id' | 'version' | 'createdAt' | 'updatedAt'>>): number {
    let imported = 0;
    for (const rule of rules) {
      try {
        this.addRule(rule);
        imported++;
      } catch (_e) {
        console.warn(`Failed to import rule: ${rule.name}`);
      }
    }
    return imported;
  }

  /**
   * Export rules to JSON
   */
  exportRules(query?: RuleSearchQuery): string {
    const rules = query 
      ? this.searchRules(query).rules 
      : Array.from(this.rules.values());
    
    return JSON.stringify(rules, null, 2);
  }

  /**
   * Get database statistics
   */
  getStatistics(): DatabaseStatistics {
    const rules = Array.from(this.rules.values());

    const byCategory: Record<string, number> = {};
    const byDomain: Record<string, number> = {};
    const byCriticality: Record<string, number> = {};
    const byReviewStatus: Record<string, number> = {};
    const byComplexity: Record<string, number> = {};

    for (const rule of rules) {
      byCategory[rule.category] = (byCategory[rule.category] || 0) + 1;
      byDomain[rule.domain] = (byDomain[rule.domain] || 0) + 1;
      byCriticality[rule.criticality] = (byCriticality[rule.criticality] || 0) + 1;
      byReviewStatus[rule.reviewStatus] = (byReviewStatus[rule.reviewStatus] || 0) + 1;
      byComplexity[rule.complexity] = (byComplexity[rule.complexity] || 0) + 1;
    }

    const avgConfidence = rules.length > 0
      ? rules.reduce((sum, r) => sum + r.confidence, 0) / rules.length
      : 0;

    return {
      totalRules: rules.length,
      byCategory,
      byDomain,
      byCriticality,
      byReviewStatus,
      byComplexity,
      averageConfidence: avgConfidence,
      graphStatistics: this.graph.getStatistics(),
    };
  }

  /**
   * Get the underlying knowledge graph
   */
  getKnowledgeGraph(): KnowledgeGraphBuilder {
    return this.graph;
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private addRuleToGraph(rule: BusinessRuleRecord): void {
    // Add rule node
    this.graph.addNode({
      id: rule.id,
      type: 'business-rule',
      name: rule.name,
      description: rule.description,
      properties: {
        category: rule.category,
        domain: rule.domain,
        criticality: rule.criticality,
        complexity: rule.complexity,
        confidence: rule.confidence,
      },
      metadata: {
        source: {
          file: rule.sourceFile,
          startLine: rule.sourceLines[0],
          endLine: rule.sourceLines[1],
        },
        confidence: rule.confidence,
        tags: rule.tags,
      },
    });

    // Add input data nodes and edges
    for (const input of rule.inputs) {
      const inputId = `data_${input.name}`;
      if (!this.graph.getGraph().nodes.has(inputId)) {
        this.graph.addNode({
          id: inputId,
          type: 'data-structure',
          name: input.name,
          description: input.description,
          properties: {
            dataType: input.type,
            constraints: input.constraints,
          },
        });
      }
      this.graph.addEdge(inputId, rule.id, 'uses', { role: 'input' });
    }

    // Add output data nodes and edges
    for (const output of rule.outputs) {
      const outputId = `data_${output.name}`;
      if (!this.graph.getGraph().nodes.has(outputId)) {
        this.graph.addNode({
          id: outputId,
          type: 'data-structure',
          name: output.name,
          description: output.description,
          properties: {
            dataType: output.type,
          },
        });
      }
      this.graph.addEdge(rule.id, outputId, 'modifies', { role: 'output' });
    }

    // Add regulation relationships
    for (const regulation of rule.regulations) {
      const regId = `reg_${regulation}`;
      if (!this.graph.getGraph().nodes.has(regId)) {
        this.graph.addNode({
          id: regId,
          type: 'regulation',
          name: regulation,
          properties: {},
          metadata: { tags: ['compliance'] },
        });
      }
      this.graph.addEdge(regId, rule.id, 'affected-by');
    }

    // Add dependencies
    for (const depId of rule.dependencies) {
      if (this.rules.has(depId)) {
        this.graph.addEdge(rule.id, depId, 'depends-on');
      }
    }

    // Add related rules
    for (const relatedId of rule.relatedRules) {
      if (this.rules.has(relatedId)) {
        this.graph.addEdge(rule.id, relatedId, 'related-to');
      }
    }
  }

  private initializeIndustryPatterns(): void {
    // Finance patterns
    this.industryPatterns.set('finance', [
      {
        patternId: 'fin-001',
        patternName: 'Interest Calculation',
        industry: 'finance',
        description: 'Standard interest calculation pattern',
        matchConfidence: 0,
      },
      {
        patternId: 'fin-002',
        patternName: 'Amortization Schedule',
        industry: 'finance',
        description: 'Loan amortization calculation',
        matchConfidence: 0,
      },
      {
        patternId: 'fin-003',
        patternName: 'Credit Scoring',
        industry: 'finance',
        description: 'Credit score calculation',
        matchConfidence: 0,
      },
    ]);

    // Insurance patterns
    this.industryPatterns.set('insurance', [
      {
        patternId: 'ins-001',
        patternName: 'Premium Calculation',
        industry: 'insurance',
        description: 'Insurance premium calculation',
        matchConfidence: 0,
      },
      {
        patternId: 'ins-002',
        patternName: 'Claims Processing',
        industry: 'insurance',
        description: 'Claims eligibility and processing',
        matchConfidence: 0,
      },
      {
        patternId: 'ins-003',
        patternName: 'Risk Assessment',
        industry: 'insurance',
        description: 'Risk scoring and assessment',
        matchConfidence: 0,
      },
    ]);

    // Banking patterns
    this.industryPatterns.set('banking', [
      {
        patternId: 'bank-001',
        patternName: 'Account Balance Calculation',
        industry: 'banking',
        description: 'Real-time balance calculation with holds',
        matchConfidence: 0,
      },
      {
        patternId: 'bank-002',
        patternName: 'Transaction Authorization',
        industry: 'banking',
        description: 'Transaction approval workflow',
        matchConfidence: 0,
      },
      {
        patternId: 'bank-003',
        patternName: 'AML Screening',
        industry: 'banking',
        description: 'Anti-money laundering checks',
        matchConfidence: 0,
      },
    ]);

    // Healthcare patterns
    this.industryPatterns.set('healthcare', [
      {
        patternId: 'health-001',
        patternName: 'Eligibility Verification',
        industry: 'healthcare',
        description: 'Insurance eligibility verification',
        matchConfidence: 0,
      },
      {
        patternId: 'health-002',
        patternName: 'Dosage Calculation',
        industry: 'healthcare',
        description: 'Medication dosage calculation',
        matchConfidence: 0,
      },
    ]);
  }

  private matchIndustryPattern(rule: BusinessRuleRecord): IndustryPattern | null {
    const patterns = this.industryPatterns.get(rule.domain);
    if (!patterns) return null;

    const keywords = [
      ...this.extractKeywords(rule.name),
      ...this.extractKeywords(rule.description),
    ];

    let bestMatch: IndustryPattern | null = null;
    let bestScore = 0;

    for (const pattern of patterns) {
      const patternKeywords = this.extractKeywords(pattern.patternName + ' ' + pattern.description);
      const overlap = keywords.filter(k => patternKeywords.includes(k)).length;
      const score = overlap / Math.max(keywords.length, patternKeywords.length);

      if (score > bestScore && score >= 0.3) {
        bestScore = score;
        bestMatch = { ...pattern, matchConfidence: score };
      }
    }

    return bestMatch;
  }

  private calculateFacets(rules: BusinessRuleRecord[]): SearchFacets {
    const categories: Record<RuleDatabaseCategory, number> = {} as Record<RuleDatabaseCategory, number>;
    const domains: Record<BusinessDomain, number> = {} as Record<BusinessDomain, number>;
    const criticality: Record<string, number> = {};
    const reviewStatus: Record<string, number> = {};
    const tags: Record<string, number> = {};

    for (const rule of rules) {
      categories[rule.category] = (categories[rule.category] || 0) + 1;
      domains[rule.domain] = (domains[rule.domain] || 0) + 1;
      criticality[rule.criticality] = (criticality[rule.criticality] || 0) + 1;
      reviewStatus[rule.reviewStatus] = (reviewStatus[rule.reviewStatus] || 0) + 1;
      
      for (const tag of rule.tags) {
        tags[tag] = (tags[tag] || 0) + 1;
      }
    }

    return { categories, domains, criticality, reviewStatus, tags };
  }

  private extractKeywords(text: string): string[] {
    return text
      .toLowerCase()
      .split(/[\s\-_.,;:!?()[\]{}'"]+/)
      .filter(w => w.length > 2)
      .filter(w => !['the', 'and', 'for', 'with', 'that', 'this', 'from', 'are', 'was'].includes(w));
  }

  private calculateSimilarity(keywords: string[], rule: BusinessRuleRecord): number {
    const ruleKeywords = [
      ...this.extractKeywords(rule.name),
      ...this.extractKeywords(rule.description),
      ...rule.tags,
    ];

    const matches = keywords.filter(k => 
      ruleKeywords.some(rk => rk.includes(k) || k.includes(rk))
    ).length;

    return matches / Math.max(keywords.length, 1);
  }
}

// ============================================================================
// Search Index (simplified in-memory implementation)
// ============================================================================

class SearchIndex {
  private index: Map<string, Set<string>> = new Map();

  indexRule(rule: BusinessRuleRecord): void {
    const terms = [
      ...rule.name.toLowerCase().split(/\s+/),
      ...rule.description.toLowerCase().split(/\s+/),
      ...rule.tags,
      rule.category,
      rule.domain,
    ];

    for (const term of terms) {
      if (term.length < 2) continue;
      if (!this.index.has(term)) {
        this.index.set(term, new Set());
      }
      this.index.get(term)!.add(rule.id);
    }
  }

  search(terms: string[]): string[] {
    const results = new Map<string, number>();

    for (const term of terms) {
      const matches = this.index.get(term.toLowerCase());
      if (matches) {
        for (const id of matches) {
          results.set(id, (results.get(id) || 0) + 1);
        }
      }
    }

    return Array.from(results.entries())
      .sort((a, b) => b[1] - a[1])
      .map(([id]) => id);
  }
}

// ============================================================================
// Statistics Type
// ============================================================================

export interface DatabaseStatistics {
  totalRules: number;
  byCategory: Record<string, number>;
  byDomain: Record<string, number>;
  byCriticality: Record<string, number>;
  byReviewStatus: Record<string, number>;
  byComplexity: Record<string, number>;
  averageConfidence: number;
  graphStatistics: {
    nodeCount: number;
    edgeCount: number;
    nodesByType: Record<string, number>;
    edgesByType: Record<string, number>;
  };
}

// ============================================================================
// Factory
// ============================================================================

export function createBusinessRulesDatabase(projectId: string): BusinessRulesDatabase {
  return new BusinessRulesDatabase(projectId);
}

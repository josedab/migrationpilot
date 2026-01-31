/**
 * Natural Language Rule Parser
 */

import type {
  ParseResult,
  ParsedRule,
  ParseError,
  Ambiguity,
  Token,
  TokenType,
  RuleIntent,
  RuleSubject,
  RuleCondition,
  RuleAction,
  ConditionOperator,
  ConditionValue,
  ActionType,
  DomainContext,
} from '../types.js';

// ============================================================================
// Rule Parser
// ============================================================================

export class NLRuleParser {
  private context: DomainContext;
  private operatorPatterns: Map<string, ConditionOperator>;
  private _actionPatterns: Map<string, ActionType>;
  private intentPatterns: Map<RegExp, RuleIntent>;

  constructor(context?: DomainContext) {
    this.context = context || this.getDefaultContext();
    this.operatorPatterns = this.buildOperatorPatterns();
    this._actionPatterns = this.buildActionPatterns();
    this.intentPatterns = this.buildIntentPatterns();
  }

  /**
   * Parse natural language text into a structured rule
   */
  parse(text: string): ParseResult {
    const errors: ParseError[] = [];
    const ambiguities: Ambiguity[] = [];

    // Normalize input
    const normalizedText = this.normalizeText(text);

    // Tokenize
    const tokens = this.tokenize(normalizedText);

    // Detect intent
    const intent = this.detectIntent(normalizedText, tokens);

    // Extract subject
    const subject = this.extractSubject(tokens);

    // Extract conditions
    const conditions = this.extractConditions(tokens, ambiguities);

    // Extract actions
    const actions = this.extractActions(tokens, intent);

    // Validate parsed components
    this.validateParsedRule({ intent, subject, conditions, actions, exceptions: [] }, errors);

    // Calculate confidence
    const confidence = this.calculateConfidence(tokens, ambiguities, errors);

    const rule: ParsedRule = {
      intent,
      subject,
      conditions,
      actions,
      exceptions: [],
    };

    return {
      success: errors.filter(e => e.severity === 'error').length === 0,
      rule,
      ambiguities,
      errors,
      tokens,
      confidence,
      suggestions: this.generateSuggestions(rule, ambiguities),
    };
  }

  /**
   * Update domain context
   */
  setContext(context: DomainContext): void {
    this.context = context;
  }

  /**
   * Add entity to context
   */
  addEntity(name: string, aliases: string[], fields: string[]): void {
    this.context.entities.push({ name, aliases, fields });
  }

  /**
   * Add synonym mapping
   */
  addSynonym(term: string, synonyms: string[], category: 'entity' | 'field' | 'operator' | 'action'): void {
    this.context.synonyms.push({ term, synonyms, category });
  }

  /**
   * Get action patterns for external use
   */
  getActionPatterns(): Map<string, ActionType> {
    return this._actionPatterns;
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private normalizeText(text: string): string {
    return text
      .toLowerCase()
      .replace(/\s+/g, ' ')
      .replace(/['']/g, "'")
      .replace(/[""]/g, '"')
      .trim();
  }

  private tokenize(text: string): Token[] {
    const tokens: Token[] = [];
    const patterns = [
      { type: 'number' as TokenType, pattern: /\b\d+(?:\.\d+)?%?\b/g },
      { type: 'date' as TokenType, pattern: /\b\d{1,2}[-/]\d{1,2}[-/]\d{2,4}\b/g },
      { type: 'string' as TokenType, pattern: /"[^"]*"|'[^']*'/g },
    ];

    // Extract special patterns first
    let processedText = text;
    for (const { type, pattern } of patterns) {
      let match;
      while ((match = pattern.exec(text)) !== null) {
        tokens.push({
          type,
          value: match[0],
          position: { start: match.index, end: match.index + match[0].length },
        });
      }
    }

    // Tokenize remaining words
    const words = processedText.split(/\s+/);
    let position = 0;

    for (const word of words) {
      const cleanWord = word.replace(/[.,;:!?]/g, '');
      if (!cleanWord) continue;

      const tokenType = this.classifyToken(cleanWord);
      const start = text.indexOf(cleanWord, position);

      tokens.push({
        type: tokenType,
        value: cleanWord,
        position: { start, end: start + cleanWord.length },
        normalized: this.normalizeToken(cleanWord, tokenType),
      });

      position = start + cleanWord.length;
    }

    // Sort by position
    tokens.sort((a, b) => a.position.start - b.position.start);

    return tokens;
  }

  private classifyToken(word: string): TokenType {
    // Check operators
    if (this.operatorPatterns.has(word)) return 'operator';

    // Check conjunctions
    if (['and', 'or', 'but', 'except', 'unless'].includes(word)) return 'conjunction';

    // Check keywords
    if (['if', 'when', 'then', 'must', 'should', 'shall', 'may', 'cannot', 'is', 'are', 'has', 'have'].includes(word)) {
      return 'keyword';
    }

    // Check entities from context
    for (const entity of this.context.entities) {
      if (entity.name === word || entity.aliases.includes(word)) return 'entity';
    }

    // Check fields from context
    for (const field of this.context.fields) {
      if (field.name === word || field.aliases.includes(word)) return 'field';
    }

    // Check if it's a number
    if (/^\d+(?:\.\d+)?$/.test(word)) return 'number';

    return 'unknown';
  }

  private normalizeToken(word: string, _type: TokenType): string {
    // Check synonyms
    for (const synonym of this.context.synonyms) {
      if (synonym.synonyms.includes(word)) {
        return synonym.term;
      }
    }

    return word;
  }

  private detectIntent(text: string, _tokens: Token[]): RuleIntent {
    for (const [pattern, intent] of this.intentPatterns) {
      if (pattern.test(text)) {
        return intent;
      }
    }

    // Default based on keywords
    if (text.includes('calculate') || text.includes('compute') || text.includes('sum')) {
      return 'calculation';
    }
    if (text.includes('must') || text.includes('required') || text.includes('valid')) {
      return 'validation';
    }
    if (text.includes('if') && text.includes('then')) {
      return 'decision';
    }
    if (text.includes('transform') || text.includes('convert')) {
      return 'transformation';
    }

    return 'validation';
  }

  private extractSubject(tokens: Token[]): RuleSubject {
    // Find entity token
    const entityToken = tokens.find(t => t.type === 'entity');
    const fieldToken = tokens.find(t => t.type === 'field');

    return {
      entity: entityToken?.normalized || entityToken?.value || 'unknown',
      field: fieldToken?.normalized || fieldToken?.value,
    };
  }

  private extractConditions(tokens: Token[], ambiguities: Ambiguity[]): RuleCondition[] {
    const conditions: RuleCondition[] = [];
    let i = 0;

    while (i < tokens.length) {
      const token = tokens[i];
      if (!token) {
        i++;
        continue;
      }

      // Look for field + operator + value pattern
      if (token.type === 'field' || token.type === 'entity') {
        const operatorToken = tokens[i + 1];
        const valueToken = tokens[i + 2];

        if (operatorToken && this.operatorPatterns.has(operatorToken.value)) {
          const operator = this.operatorPatterns.get(operatorToken.value)!;
          
          let value: ConditionValue;
          if (valueToken) {
            value = this.parseConditionValue(valueToken);
          } else {
            value = { type: 'literal', value: true };
            ambiguities.push({
              type: 'value_type',
              position: operatorToken.position,
              text: operatorToken.value,
              options: [
                { value: 'true', description: 'Boolean true', confidence: 0.7 },
                { value: 'missing value', description: 'Value needs specification', confidence: 0.3 },
              ],
              severity: 'medium',
              resolved: false,
            });
          }

          conditions.push({
            field: token.normalized || token.value,
            operator,
            value,
            negated: false,
          });

          i += 3;
          continue;
        }
      }

      i++;
    }

    // If no conditions found, look for implicit conditions
    if (conditions.length === 0) {
      const implicitCondition = this.extractImplicitCondition(tokens);
      if (implicitCondition) {
        conditions.push(implicitCondition);
        ambiguities.push({
          type: 'implicit_condition',
          position: { start: 0, end: 0 },
          text: 'Implicit condition detected',
          options: [],
          severity: 'low',
          resolved: true,
          resolution: `Interpreted as: ${implicitCondition.field} ${implicitCondition.operator}`,
        });
      }
    }

    return conditions;
  }

  private parseConditionValue(token: Token): ConditionValue {
    switch (token.type) {
      case 'number':
        return { type: 'literal', value: parseFloat(token.value) };
      case 'string':
        return { type: 'literal', value: token.value.replace(/['"]/g, '') };
      case 'field':
        return { type: 'field', value: token.normalized || token.value };
      default:
        return { type: 'literal', value: token.value };
    }
  }

  private extractImplicitCondition(tokens: Token[]): RuleCondition | null {
    const fieldToken = tokens.find(t => t.type === 'field');
    if (!fieldToken) return null;

    // Look for validation keywords
    if (tokens.some(t => ['required', 'mandatory', 'needed'].includes(t.value))) {
      return {
        field: fieldToken.normalized || fieldToken.value,
        operator: 'is_not_empty',
        value: { type: 'literal', value: true },
        negated: false,
      };
    }

    return null;
  }

  private extractActions(tokens: Token[], intent: RuleIntent): RuleAction[] {
    const actions: RuleAction[] = [];

    // Find action patterns based on intent
    switch (intent) {
      case 'validation':
        actions.push({
          type: 'reject',
          target: 'validation',
        });
        break;
      case 'calculation':
        const fieldToken = tokens.find(t => t.type === 'field');
        actions.push({
          type: 'calculate',
          target: fieldToken?.normalized || fieldToken?.value || 'result',
        });
        break;
      case 'transformation':
        actions.push({
          type: 'transform',
          target: 'output',
        });
        break;
      default:
        actions.push({
          type: 'log',
          target: 'rule_execution',
        });
    }

    return actions;
  }

  private validateParsedRule(rule: ParsedRule, errors: ParseError[]): void {
    if (!rule.subject.entity || rule.subject.entity === 'unknown') {
      errors.push({
        code: 'MISSING_SUBJECT',
        message: 'Could not identify the subject entity of the rule',
        severity: 'warning',
        suggestion: 'Specify which entity this rule applies to (e.g., "The customer..." or "When an order...")',
      });
    }

    if (rule.conditions.length === 0) {
      errors.push({
        code: 'NO_CONDITIONS',
        message: 'No conditions were identified in the rule',
        severity: 'warning',
        suggestion: 'Add conditions using words like "if", "when", "greater than", "equals", etc.',
      });
    }

    if (rule.actions.length === 0) {
      errors.push({
        code: 'NO_ACTIONS',
        message: 'No actions were identified in the rule',
        severity: 'error',
        suggestion: 'Specify what should happen (e.g., "reject", "calculate", "set to")',
      });
    }
  }

  private calculateConfidence(tokens: Token[], ambiguities: Ambiguity[], errors: ParseError[]): number {
    let confidence = 1.0;

    // Reduce for unknown tokens
    const unknownCount = tokens.filter(t => t.type === 'unknown').length;
    confidence -= unknownCount * 0.05;

    // Reduce for ambiguities
    for (const ambiguity of ambiguities) {
      switch (ambiguity.severity) {
        case 'high': confidence -= 0.15; break;
        case 'medium': confidence -= 0.1; break;
        case 'low': confidence -= 0.05; break;
      }
    }

    // Reduce for errors
    for (const error of errors) {
      if (error.severity === 'error') confidence -= 0.2;
      else confidence -= 0.1;
    }

    return Math.max(0, Math.min(1, confidence));
  }

  private generateSuggestions(rule: ParsedRule, ambiguities: Ambiguity[]): string[] {
    const suggestions: string[] = [];

    // Suggest resolving ambiguities
    for (const ambiguity of ambiguities.filter(a => !a.resolved)) {
      if (ambiguity.options.length > 0) {
        suggestions.push(`Clarify "${ambiguity.text}": Did you mean ${ambiguity.options.map(o => `"${o.value}"`).join(' or ')}?`);
      }
    }

    // Suggest adding test cases
    if (rule.conditions.length > 0) {
      suggestions.push('Consider adding test cases to verify the rule behavior');
    }

    return suggestions;
  }

  private getDefaultContext(): DomainContext {
    return {
      entities: [
        { name: 'customer', aliases: ['client', 'user', 'buyer'], fields: ['name', 'id', 'status', 'balance'] },
        { name: 'order', aliases: ['purchase', 'transaction'], fields: ['total', 'items', 'date', 'status'] },
        { name: 'account', aliases: ['acct'], fields: ['balance', 'type', 'status', 'limit'] },
      ],
      fields: [
        { name: 'amount', entity: '*', type: 'number', aliases: ['value', 'sum', 'total'] },
        { name: 'date', entity: '*', type: 'date', aliases: ['time', 'when'] },
        { name: 'status', entity: '*', type: 'string', aliases: ['state', 'condition'] },
      ],
      functions: [
        { name: 'sum', aliases: ['total', 'add'], parameters: [{ name: 'values', type: 'array', required: true }], returnType: 'number' },
        { name: 'count', aliases: ['number of'], parameters: [{ name: 'items', type: 'array', required: true }], returnType: 'number' },
      ],
      synonyms: [
        { term: 'greater_than', synonyms: ['more than', 'exceeds', 'above', 'over'], category: 'operator' },
        { term: 'less_than', synonyms: ['less than', 'below', 'under', 'fewer than'], category: 'operator' },
        { term: 'equals', synonyms: ['is', 'equal to', 'same as', 'matches'], category: 'operator' },
      ],
      patterns: [],
    };
  }

  private buildOperatorPatterns(): Map<string, ConditionOperator> {
    return new Map([
      ['equals', 'equals'],
      ['is', 'equals'],
      ['greater', 'greater_than'],
      ['more', 'greater_than'],
      ['exceeds', 'greater_than'],
      ['above', 'greater_than'],
      ['less', 'less_than'],
      ['fewer', 'less_than'],
      ['below', 'less_than'],
      ['contains', 'contains'],
      ['includes', 'contains'],
      ['starts', 'starts_with'],
      ['ends', 'ends_with'],
      ['between', 'between'],
      ['empty', 'is_empty'],
      ['null', 'is_null'],
      ['exists', 'exists'],
    ]);
  }

  private buildActionPatterns(): Map<string, ActionType> {
    return new Map([
      ['set', 'set_value'],
      ['assign', 'set_value'],
      ['calculate', 'calculate'],
      ['compute', 'calculate'],
      ['reject', 'reject'],
      ['deny', 'reject'],
      ['approve', 'approve'],
      ['accept', 'approve'],
      ['notify', 'notify'],
      ['alert', 'notify'],
      ['transform', 'transform'],
      ['convert', 'transform'],
      ['flag', 'flag'],
      ['mark', 'flag'],
    ]);
  }

  private buildIntentPatterns(): Map<RegExp, RuleIntent> {
    return new Map([
      [/must be valid|validation|validate|check|verify/i, 'validation'],
      [/calculate|compute|sum|total|multiply|divide/i, 'calculation'],
      [/transform|convert|change|map/i, 'transformation'],
      [/if.*then|when.*do|decide/i, 'decision'],
      [/must not|cannot|prohibited|forbidden/i, 'constraint'],
      [/notify|alert|send|email/i, 'notification'],
      [/authorized|permission|allowed|access/i, 'authorization'],
    ]);
  }
}

// ============================================================================
// Factory
// ============================================================================

export function createNLRuleParser(context?: DomainContext): NLRuleParser {
  return new NLRuleParser(context);
}

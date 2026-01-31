/**
 * Natural Language Rules Types
 */

// ============================================================================
// Rule Types
// ============================================================================

export interface NLRule {
  id: string;
  naturalLanguage: string;
  parsedRule: ParsedRule;
  formalRule?: FormalRule;
  confidence: number;
  ambiguities: Ambiguity[];
  status: RuleStatus;
  metadata: RuleMetadata;
  createdAt: Date;
  updatedAt: Date;
}

export interface ParsedRule {
  intent: RuleIntent;
  subject: RuleSubject;
  conditions: RuleCondition[];
  actions: RuleAction[];
  exceptions: RuleException[];
  temporalConstraints?: TemporalConstraint[];
  quantifiers?: Quantifier[];
}

export interface FormalRule {
  id: string;
  type: FormalRuleType;
  expression: string;
  pseudocode: string;
  testCases: TestCase[];
}

export type RuleIntent = 
  | 'validation'
  | 'calculation'
  | 'transformation'
  | 'decision'
  | 'constraint'
  | 'notification'
  | 'authorization';

export interface RuleSubject {
  entity: string;
  field?: string;
  context?: string;
  aliases?: string[];
}

export interface RuleCondition {
  field: string;
  operator: ConditionOperator;
  value: ConditionValue;
  conjunction?: 'and' | 'or';
  negated: boolean;
}

export type ConditionOperator = 
  | 'equals'
  | 'not_equals'
  | 'greater_than'
  | 'less_than'
  | 'greater_or_equal'
  | 'less_or_equal'
  | 'contains'
  | 'starts_with'
  | 'ends_with'
  | 'in_list'
  | 'not_in_list'
  | 'is_empty'
  | 'is_not_empty'
  | 'matches_pattern'
  | 'between'
  | 'exists'
  | 'is_null';

export interface ConditionValue {
  type: 'literal' | 'field' | 'function' | 'range';
  value: unknown;
  secondaryValue?: unknown;
}

export interface RuleAction {
  type: ActionType;
  target: string;
  value?: unknown;
  parameters?: Record<string, unknown>;
}

export type ActionType =
  | 'set_value'
  | 'calculate'
  | 'reject'
  | 'approve'
  | 'notify'
  | 'transform'
  | 'route'
  | 'log'
  | 'flag';

export interface RuleException {
  condition: RuleCondition;
  action: RuleAction;
  reason: string;
}

export interface TemporalConstraint {
  type: 'before' | 'after' | 'during' | 'within';
  reference: string;
  duration?: string;
  unit?: 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'months' | 'years';
}

export interface Quantifier {
  type: 'all' | 'any' | 'none' | 'at_least' | 'at_most' | 'exactly';
  count?: number;
  scope: string;
}

export type FormalRuleType = 'expression' | 'decision_table' | 'state_machine' | 'workflow';
export type RuleStatus = 'draft' | 'pending_review' | 'approved' | 'active' | 'deprecated';

export interface RuleMetadata {
  author?: string;
  domain?: string;
  category?: string;
  tags?: string[];
  priority?: 'low' | 'medium' | 'high' | 'critical';
  effectiveDate?: Date;
  expirationDate?: Date;
  version?: string;
}

// ============================================================================
// Ambiguity Types
// ============================================================================

export interface Ambiguity {
  type: AmbiguityType;
  position: TextPosition;
  text: string;
  options: AmbiguityOption[];
  severity: 'low' | 'medium' | 'high';
  resolved: boolean;
  resolution?: string;
}

export type AmbiguityType =
  | 'entity_reference'
  | 'field_reference'
  | 'operator_unclear'
  | 'value_type'
  | 'temporal_reference'
  | 'scope_unclear'
  | 'pronoun_reference'
  | 'implicit_condition';

export interface AmbiguityOption {
  value: string;
  description: string;
  confidence: number;
}

export interface TextPosition {
  start: number;
  end: number;
  line?: number;
  column?: number;
}

// ============================================================================
// Test Case Types
// ============================================================================

export interface TestCase {
  id: string;
  name: string;
  description?: string;
  inputs: Record<string, unknown>;
  expectedOutputs: Record<string, unknown>;
  expectedResult: 'pass' | 'fail' | 'exception';
  tags?: string[];
  generated: boolean;
}

// ============================================================================
// Parser Types
// ============================================================================

export interface ParseResult {
  success: boolean;
  rule?: ParsedRule;
  ambiguities: Ambiguity[];
  errors: ParseError[];
  tokens: Token[];
  confidence: number;
  suggestions?: string[];
}

export interface ParseError {
  code: string;
  message: string;
  position?: TextPosition;
  severity: 'warning' | 'error';
  suggestion?: string;
}

export interface Token {
  type: TokenType;
  value: string;
  position: TextPosition;
  normalized?: string;
  metadata?: Record<string, unknown>;
}

export type TokenType =
  | 'entity'
  | 'field'
  | 'operator'
  | 'value'
  | 'conjunction'
  | 'keyword'
  | 'number'
  | 'date'
  | 'string'
  | 'pattern'
  | 'function'
  | 'unknown';

// ============================================================================
// Generator Types
// ============================================================================

export interface GeneratorConfig {
  targetLanguage: GeneratorTargetLanguage;
  style: CodeStyle;
  includeComments: boolean;
  includeTestCases: boolean;
  optimization: 'none' | 'basic' | 'aggressive';
}

export type GeneratorTargetLanguage = 
  | 'javascript'
  | 'typescript'
  | 'java'
  | 'python'
  | 'sql'
  | 'drools'
  | 'dmn';

export interface CodeStyle {
  indentation: 'spaces' | 'tabs';
  indentSize: number;
  maxLineLength: number;
  namingConvention: 'camelCase' | 'snake_case' | 'PascalCase';
}

export interface GeneratedCode {
  code: string;
  language: GeneratorTargetLanguage;
  testCode?: string;
  imports: string[];
  dependencies: string[];
  warnings: string[];
}

// ============================================================================
// Validation Types
// ============================================================================

export interface ValidationResult {
  valid: boolean;
  errors: ValidationError[];
  warnings: ValidationWarning[];
  coverage: RuleCoverage;
}

export interface ValidationError {
  code: string;
  message: string;
  path?: string;
  suggestion?: string;
}

export interface ValidationWarning {
  code: string;
  message: string;
  path?: string;
}

export interface RuleCoverage {
  conditionsCovered: number;
  totalConditions: number;
  actionsCovered: number;
  totalActions: number;
  edgeCasesCovered: number;
  totalEdgeCases: number;
  coveragePercentage: number;
}

// ============================================================================
// Context Types
// ============================================================================

export interface DomainContext {
  entities: EntityDefinition[];
  fields: FieldDefinition[];
  functions: FunctionDefinition[];
  synonyms: SynonymMapping[];
  patterns: PatternDefinition[];
}

export interface EntityDefinition {
  name: string;
  aliases: string[];
  fields: string[];
  description?: string;
}

export interface FieldDefinition {
  name: string;
  entity: string;
  type: 'string' | 'number' | 'boolean' | 'date' | 'array' | 'object';
  aliases: string[];
  constraints?: FieldConstraint[];
}

export interface FieldConstraint {
  type: 'required' | 'min' | 'max' | 'pattern' | 'enum';
  value: unknown;
}

export interface FunctionDefinition {
  name: string;
  aliases: string[];
  parameters: FunctionParameter[];
  returnType: string;
  description?: string;
}

export interface FunctionParameter {
  name: string;
  type: string;
  required: boolean;
  default?: unknown;
}

export interface SynonymMapping {
  term: string;
  synonyms: string[];
  category: 'entity' | 'field' | 'operator' | 'action';
}

export interface PatternDefinition {
  name: string;
  pattern: string;
  description?: string;
  examples: string[];
}

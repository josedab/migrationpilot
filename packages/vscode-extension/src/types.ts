/**
 * VS Code Extension Types
 * Types for MigrationPilot VS Code extension
 */

export interface ExtensionConfig {
  apiUrl: string;
  targetLanguage: string;
  autoAnalyze: boolean;
  showInlineHints: boolean;
  confidenceThreshold: number;
}

export interface AnalysisResult {
  fileId: string;
  filePath: string;
  language: string;
  complexity: number;
  procedures: ProcedureInfo[];
  businessRules: BusinessRuleInfo[];
  dataStructures: DataStructureInfo[];
  dependencies: DependencyInfo[];
  suggestions: MigrationSuggestion[];
}

export interface ProcedureInfo {
  name: string;
  type: string;
  startLine: number;
  endLine: number;
  complexity: number;
  description?: string;
}

export interface BusinessRuleInfo {
  id: string;
  name: string;
  description: string;
  category: string;
  startLine: number;
  endLine: number;
  confidence: number;
  inputs: string[];
  outputs: string[];
  formula?: string;
}

export interface DataStructureInfo {
  name: string;
  type: string;
  startLine: number;
  endLine: number;
  fields: FieldInfo[];
}

export interface FieldInfo {
  name: string;
  type: string;
  size?: number;
}

export interface DependencyInfo {
  name: string;
  type: 'internal' | 'external' | 'database' | 'file';
  source?: string;
}

export interface MigrationSuggestion {
  id: string;
  title: string;
  description: string;
  targetCode?: string;
  targetLanguage: string;
  confidence: number;
  location: {
    startLine: number;
    endLine: number;
  };
  priority: 'high' | 'medium' | 'low';
}

export interface ExplanationResult {
  summary: string;
  detailedExplanation: string;
  businessPurpose?: string;
  relatedRules: string[];
  dataFlow?: DataFlowInfo;
  complexity: string;
}

export interface DataFlowInfo {
  inputs: string[];
  outputs: string[];
  transformations: string[];
}

export interface TestGenerationResult {
  testCases: GeneratedTestCase[];
  coverage: CoverageInfo;
  framework: string;
}

export interface GeneratedTestCase {
  name: string;
  description: string;
  code: string;
  type: string;
}

export interface CoverageInfo {
  rulesTotal: number;
  rulesCovered: number;
  percentage: number;
}

export interface QuestionResult {
  answer: string;
  confidence: number;
  sources: SourceReference[];
  relatedNodes?: RelatedNode[];
}

export interface SourceReference {
  type: string;
  name: string;
  file?: string;
  line?: number;
}

export interface RelatedNode {
  id: string;
  name: string;
  type: string;
}

export interface ApiClient {
  analyzeFile(content: string, language: string): Promise<AnalysisResult>;
  explainCode(content: string, language: string): Promise<ExplanationResult>;
  extractRules(content: string, language: string): Promise<BusinessRuleInfo[]>;
  suggestMigration(content: string, sourceLanguage: string, targetLanguage: string): Promise<MigrationSuggestion[]>;
  generateTests(ruleIds: string[]): Promise<TestGenerationResult>;
  askQuestion(question: string, context?: string): Promise<QuestionResult>;
}

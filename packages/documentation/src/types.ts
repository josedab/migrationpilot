/**
 * Documentation Types
 * 
 * Types for document generation and export
 */

import type { 
  BusinessRule, 
  DataStructure, 
  Procedure,
  SourceLanguage, 
  TargetLanguage,
} from '@migrationpilot/core';

// ============================================================================
// DOCUMENT TYPES
// ============================================================================

export type DocumentType = 
  | 'system-overview'
  | 'data-dictionary'
  | 'business-rule-catalog'
  | 'api-specification'
  | 'architecture-overview'
  | 'migration-runbook'
  | 'test-plan';

export interface Document {
  id: string;
  projectId: string;
  type: DocumentType;
  title: string;
  version: string;
  
  // Content sections
  sections: DocumentSection[];
  
  // Metadata
  generatedAt: Date;
  generatedBy: string;
  lastModified: Date;
  
  // Source tracking
  sourceFiles: string[];
  businessRulesReferenced: string[];
  
  // Quality metrics
  completeness: number; // 0-1
  confidenceScore: number; // 0-1
  reviewStatus: 'draft' | 'pending-review' | 'approved';
}

export interface DocumentSection {
  id: string;
  title: string;
  level: number; // 1-6 for heading levels
  content: string;
  subsections?: DocumentSection[];
  
  // Source references
  sourceReferences?: SourceReference[];
  
  // Confidence
  confidence?: number;
  aiGenerated?: boolean;
}

export interface SourceReference {
  type: 'code' | 'rule' | 'data' | 'external';
  id: string;
  name: string;
  location?: {
    file: string;
    startLine: number;
    endLine: number;
  };
}

// ============================================================================
// TEMPLATE TYPES
// ============================================================================

export interface DocumentTemplate {
  type: DocumentType;
  name: string;
  description: string;
  sections: TemplateSectionDefinition[];
  variables: TemplateVariable[];
  outputFormats: ExportFormat[];
}

export interface TemplateSectionDefinition {
  id: string;
  title: string;
  description: string;
  level: number;
  required: boolean;
  contentType: 'static' | 'dynamic' | 'ai-generated';
  staticContent?: string;
  dynamicContentKey?: string;
  aiPrompt?: string;
  subsections?: TemplateSectionDefinition[];
}

export interface TemplateVariable {
  name: string;
  type: 'string' | 'number' | 'date' | 'list' | 'object';
  description: string;
  required: boolean;
  defaultValue?: unknown;
}

// ============================================================================
// GENERATION TYPES
// ============================================================================

export interface GenerationContext {
  project: ProjectInfo;
  analysis: AnalysisInfo;
  options: GenerationOptions;
}

export interface ProjectInfo {
  id: string;
  name: string;
  description?: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  createdAt: Date;
}

export interface AnalysisInfo {
  programs: ProgramInfo[];
  dataStructures: DataStructure[];
  procedures: Procedure[];
  businessRules: BusinessRule[];
  dependencies: DependencyInfo[];
  metrics: AnalysisMetrics;
}

export interface ProgramInfo {
  id: string;
  name: string;
  type: string;
  language: SourceLanguage;
  linesOfCode: number;
  complexity: number;
  description?: string;
}

export interface DependencyInfo {
  sourceId: string;
  targetId: string;
  type: 'calls' | 'uses' | 'includes' | 'depends-on';
}

export interface AnalysisMetrics {
  totalFiles: number;
  totalLines: number;
  totalProcedures: number;
  totalDataItems: number;
  totalBusinessRules: number;
  averageComplexity: number;
  highRiskCount: number;
}

export interface GenerationOptions {
  format: ExportFormat;
  includeSourceCode: boolean;
  includeConfidenceScores: boolean;
  includeLineNumbers: boolean;
  maxCodeBlockLines: number;
  aiEnhance: boolean;
  detailLevel: 'summary' | 'standard' | 'detailed';
}

// ============================================================================
// EXPORT TYPES
// ============================================================================

export type ExportFormat = 'markdown' | 'html' | 'pdf' | 'docx' | 'confluence' | 'notion';

export interface ExportOptions {
  format: ExportFormat;
  outputPath?: string;
  
  // Styling
  theme?: 'light' | 'dark' | 'corporate';
  customCss?: string;
  logoUrl?: string;
  
  // Content options
  includeConfidenceScores?: boolean;
  includeSourceReferences?: boolean;
  
  // PDF-specific
  pageSize?: 'A4' | 'Letter' | 'Legal';
  margins?: { top: number; right: number; bottom: number; left: number };
  headerText?: string;
  footerText?: string;
  
  // Confluence-specific
  spaceKey?: string;
  parentPageId?: string;
  
  // Table of contents
  includeToc?: boolean;
  tocDepth?: number;
}

export interface ExportResult {
  success: boolean;
  format: ExportFormat;
  outputPath?: string;
  content?: string;
  size?: number;
  error?: string;
}

// ============================================================================
// AI GENERATION TYPES
// ============================================================================

export interface AIGenerationRequest {
  documentType: DocumentType;
  section: string;
  context: {
    projectName: string;
    sourceLanguage: string;
    targetLanguage: string;
    relevantCode?: string;
    relevantRules?: BusinessRule[];
    relevantData?: DataStructure[];
  };
  instructions?: string;
  maxLength?: number;
  temperature?: number;
}

export interface AIGenerationResult {
  content: string;
  confidence: number;
  sourceReferences: SourceReference[];
  suggestions?: string[];
  warnings?: string[];
}

// ============================================================================
// DATA DICTIONARY TYPES
// ============================================================================

export interface DataDictionaryEntry {
  name: string;
  type: string;
  format?: string;
  length?: number;
  description: string;
  source: string;
  usage: 'input' | 'output' | 'working' | 'constant';
  constraints?: string[];
  relatedRules?: string[];
  examples?: string[];
}

// ============================================================================
// API SPECIFICATION TYPES
// ============================================================================

export interface APIEndpoint {
  path: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';
  summary: string;
  description: string;
  parameters: APIParameter[];
  requestBody?: APIRequestBody;
  responses: APIResponse[];
  tags?: string[];
  sourceRule?: string;
}

export interface APIParameter {
  name: string;
  in: 'path' | 'query' | 'header' | 'cookie';
  type: string;
  description: string;
  required: boolean;
  example?: unknown;
}

export interface APIRequestBody {
  description: string;
  contentType: string;
  schema: Record<string, unknown>;
  example?: unknown;
}

export interface APIResponse {
  statusCode: number;
  description: string;
  contentType?: string;
  schema?: Record<string, unknown>;
  example?: unknown;
}

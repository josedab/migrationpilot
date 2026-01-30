/**
 * Types for Code Generation
 */

// Local type definitions to avoid strict coupling with core types
export interface GeneratorBusinessRule {
  id: string;
  name: string;
  description: string;
  inputs?: GeneratorRuleInput[];
  outputs?: GeneratorRuleOutput[];
  edgeCases?: string[];
  confidence: number;
  logic?: {
    calculation?: string;
  };
  sourceLocation?: {
    file: string;
    startLine: number;
    endLine: number;
  };
}

export interface GeneratorRuleInput {
  name: string;
  type: string;
  description?: string;
}

export interface GeneratorRuleOutput {
  name: string;
  type: string;
  description?: string;
}

export interface GeneratorDataStructure {
  name: string;
  type?: string;
  description?: string;
  fields?: GeneratorField[];
  sourceLocation?: {
    file: string;
    startLine: number;
    endLine: number;
  };
}

export interface GeneratorField {
  name: string;
  type: string;
  description?: string;
  nullable?: boolean;
}

export interface GeneratorProcedure {
  name: string;
  type: string;
  parameters: { name: string; type: string }[];
  complexity: number;
}

export interface GeneratorConfig {
  targetLanguage: TargetLanguage;
  framework?: string;
  outputDir: string;
  packageName: string;
  generateTests: boolean;
  generateDocs: boolean;
  codeStyle: CodeStyle;
}

export type TargetLanguage = 'java' | 'python' | 'typescript' | 'go' | 'csharp';

export interface CodeStyle {
  indentSize: number;
  useTabs: boolean;
  maxLineLength: number;
  braceStyle: 'same-line' | 'new-line';
}

export interface GeneratedFile {
  path: string;
  content: string;
  type: FileType;
  sourceMapping?: SourceMapping[];
}

export type FileType = 
  | 'source'
  | 'test'
  | 'interface'
  | 'model'
  | 'service'
  | 'controller'
  | 'repository'
  | 'config'
  | 'documentation';

export interface SourceMapping {
  generatedLine: number;
  generatedColumn?: number;
  sourcePath: string;
  sourceLine: number;
  sourceColumn?: number;
  description?: string;
}

export interface GenerationResult {
  files: GeneratedFile[];
  mappings: CodeMappingDocument;
  stats: GenerationStats;
  warnings: GenerationWarning[];
}

export interface GenerationStats {
  totalFiles: number;
  totalLines: number;
  sourceFiles: number;
  testFiles: number;
  coverageEstimate: number;
}

export interface GenerationWarning {
  severity: 'info' | 'warning' | 'error';
  message: string;
  sourcePath?: string;
  sourceLine?: number;
}

export interface CodeMappingDocument {
  version: string;
  sourceLanguage: string;
  targetLanguage: string;
  mappings: FileMappingEntry[];
  ruleMappings: RuleMappingEntry[];
}

export interface FileMappingEntry {
  sourceFile: string;
  targetFiles: string[];
  description: string;
}

export interface RuleMappingEntry {
  ruleId: string;
  ruleName: string;
  sourceLocations: SourceLocation[];
  targetLocations: TargetLocation[];
}

export interface SourceLocation {
  file: string;
  startLine: number;
  endLine: number;
  code?: string;
}

export interface TargetLocation {
  file: string;
  startLine: number;
  endLine: number;
  functionName?: string;
  className?: string;
}

export interface ModuleDesign {
  name: string;
  description: string;
  classes: ClassDesign[];
  interfaces: InterfaceDesign[];
  services: ServiceDesign[];
}

export interface ClassDesign {
  name: string;
  description: string;
  fields: FieldDesign[];
  methods: MethodDesign[];
  sourceReferences: string[];
}

export interface InterfaceDesign {
  name: string;
  description: string;
  methods: MethodSignature[];
}

export interface ServiceDesign {
  name: string;
  description: string;
  endpoints?: EndpointDesign[];
  dependencies: string[];
  businessRules: string[];
}

export interface FieldDesign {
  name: string;
  type: string;
  description?: string;
  nullable: boolean;
  defaultValue?: string;
  sourceField?: string;
}

export interface MethodDesign {
  name: string;
  description: string;
  parameters: ParameterDesign[];
  returnType: string;
  throws?: string[];
  sourceReferences: string[];
  businessRuleId?: string;
}

export interface MethodSignature {
  name: string;
  parameters: ParameterDesign[];
  returnType: string;
}

export interface ParameterDesign {
  name: string;
  type: string;
  description?: string;
  required: boolean;
  defaultValue?: string;
}

export interface EndpointDesign {
  path: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';
  description: string;
  requestBody?: string;
  responseType: string;
}

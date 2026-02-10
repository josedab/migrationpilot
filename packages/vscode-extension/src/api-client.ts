/**
 * API Client
 * Communicates with MigrationPilot API server
 */

import axios, { AxiosInstance } from 'axios';
import type {
  AnalysisResult,
  ExplanationResult,
  BusinessRuleInfo,
  MigrationSuggestion,
  TestGenerationResult,
  QuestionResult,
} from './types';

export class MigrationPilotApiClient {
  private client: AxiosInstance;
  private projectId: string = 'vscode-session';

  constructor(baseUrl: string) {
    this.client = axios.create({
      baseURL: baseUrl,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });
  }

  /**
   * Update the API base URL
   */
  setBaseUrl(url: string): void {
    this.client.defaults.baseURL = url;
  }

  /**
   * Set the project ID for API calls
   */
  setProjectId(projectId: string): void {
    this.projectId = projectId;
  }

  /**
   * Analyze a file to extract procedures, rules, and dependencies
   */
  async analyzeFile(content: string, language: string, filePath: string): Promise<AnalysisResult> {
    try {
      // First, try to parse the file
      const parseResponse = await this.client.post('/analysis/parse', {
        content,
        language: this.mapLanguage(language),
        fileName: filePath,
      });

      if (parseResponse.data.error) {
        throw new Error(parseResponse.data.error);
      }

      const parsed = parseResponse.data.data;

      // Extract business rules
      const rulesResponse = await this.client.post('/analysis/extract-rules', {
        content,
        language: this.mapLanguage(language),
        projectId: this.projectId,
      });

      const rules = rulesResponse.data.data?.rules || [];

      // Build analysis result
      return {
        fileId: `file_${Date.now()}`,
        filePath,
        language,
        complexity: parsed.procedures?.reduce((sum: number, p: { complexity: number }) => sum + (p.complexity || 0), 0) / (parsed.procedures?.length || 1),
        procedures: (parsed.procedures || []).map((p: Record<string, unknown>) => ({
          name: p.name as string,
          type: p.type as string,
          startLine: (p.location as { startLine: number })?.startLine || 1,
          endLine: (p.location as { endLine: number })?.endLine || 1,
          complexity: p.complexity as number,
          description: `${p.type} procedure`,
        })),
        businessRules: rules.map((r: Record<string, unknown>) => ({
          id: r.id as string,
          name: r.name as string,
          description: r.description as string,
          category: r.category as string,
          startLine: (r.sourceLines as number[])?.[0] || 1,
          endLine: (r.sourceLines as number[])?.[1] || 1,
          confidence: r.confidence as number,
          inputs: ((r.inputs as Array<{ name: string }>) || []).map(i => i.name),
          outputs: ((r.outputs as Array<{ name: string }>) || []).map(o => o.name),
          formula: r.formula as string | undefined,
        })),
        dataStructures: (parsed.dataStructures || []).map((ds: Record<string, unknown>) => ({
          name: ds.name as string,
          type: ds.type as string,
          startLine: (ds.location as { startLine: number })?.startLine || 1,
          endLine: (ds.location as { endLine: number })?.endLine || 1,
          fields: ((ds.children as Array<{ name: string; type: string }>) || []).map(c => ({
            name: c.name,
            type: c.type,
          })),
        })),
        dependencies: this.extractDependencies(parsed),
        suggestions: [],
      };
    } catch (error) {
      // Return a basic result if API fails
      return this.createFallbackAnalysis(filePath, language, content);
    }
  }

  /**
   * Explain selected code
   */
  async explainCode(content: string, language: string): Promise<ExplanationResult> {
    try {
      const response = await this.client.post('/explainer/explain', {
        code: content,
        language: this.mapLanguage(language),
        detailLevel: 'detailed',
      });

      if (response.data.error) {
        throw new Error(response.data.error);
      }

      const explanation = response.data.data;

      return {
        summary: explanation.summary || 'Code analysis complete',
        detailedExplanation: explanation.detailedExplanation || explanation.explanation || '',
        businessPurpose: explanation.businessPurpose,
        relatedRules: explanation.relatedRules || [],
        dataFlow: explanation.dataFlow,
        complexity: explanation.complexity || 'moderate',
      };
    } catch (error) {
      return this.createFallbackExplanation(content, language);
    }
  }

  /**
   * Extract business rules from code
   */
  async extractRules(content: string, language: string): Promise<BusinessRuleInfo[]> {
    try {
      const response = await this.client.post('/analysis/extract-rules', {
        content,
        language: this.mapLanguage(language),
        projectId: this.projectId,
      });

      if (response.data.error) {
        throw new Error(response.data.error);
      }

      const rules = response.data.data?.rules || [];

      return rules.map((r: Record<string, unknown>) => ({
        id: r.id as string,
        name: r.name as string,
        description: r.description as string,
        category: r.category as string,
        startLine: (r.sourceLines as number[])?.[0] || 1,
        endLine: (r.sourceLines as number[])?.[1] || 1,
        confidence: r.confidence as number,
        inputs: ((r.inputs as Array<{ name: string }>) || []).map(i => i.name),
        outputs: ((r.outputs as Array<{ name: string }>) || []).map(o => o.name),
        formula: r.formula as string | undefined,
      }));
    } catch (error) {
      return [];
    }
  }

  /**
   * Get migration suggestions
   */
  async suggestMigration(
    content: string,
    sourceLanguage: string,
    targetLanguage: string
  ): Promise<MigrationSuggestion[]> {
    try {
      const response = await this.client.post('/migration/suggest', {
        code: content,
        sourceLanguage: this.mapLanguage(sourceLanguage),
        targetLanguage,
        projectId: this.projectId,
      });

      if (response.data.error) {
        throw new Error(response.data.error);
      }

      const suggestions = response.data.data?.suggestions || [];

      return suggestions.map((s: Record<string, unknown>, index: number) => ({
        id: `suggestion_${index}`,
        title: s.title as string || 'Migration Suggestion',
        description: s.description as string || '',
        targetCode: s.targetCode as string | undefined,
        targetLanguage,
        confidence: s.confidence as number || 0.8,
        location: {
          startLine: (s.location as { startLine?: number })?.startLine || 1,
          endLine: (s.location as { endLine?: number })?.endLine || 1,
        },
        priority: (s.priority as 'high' | 'medium' | 'low') || 'medium',
      }));
    } catch (error) {
      return this.createFallbackSuggestions(content, targetLanguage);
    }
  }

  /**
   * Generate tests for business rules
   */
  async generateTests(ruleIds: string[]): Promise<TestGenerationResult> {
    try {
      const response = await this.client.post('/regression/generate-code', {
        projectId: this.projectId,
        ruleIds,
        language: 'typescript',
        framework: 'vitest',
      });

      if (response.data.error) {
        throw new Error(response.data.error);
      }

      const result = response.data.data;

      return {
        testCases: (result.files || []).map((f: { path: string; content: string; testCaseIds: string[] }) => ({
          name: f.path,
          description: `Generated test file with ${f.testCaseIds?.length || 0} test cases`,
          code: f.content,
          type: 'generated',
        })),
        coverage: {
          rulesTotal: ruleIds.length,
          rulesCovered: ruleIds.length,
          percentage: 100,
        },
        framework: 'vitest',
      };
    } catch (error) {
      return {
        testCases: [],
        coverage: { rulesTotal: ruleIds.length, rulesCovered: 0, percentage: 0 },
        framework: 'vitest',
      };
    }
  }

  /**
   * Ask a natural language question about the code
   */
  async askQuestion(question: string, context?: string): Promise<QuestionResult> {
    try {
      const response = await this.client.post('/nl-query/ask', {
        question,
        projectId: this.projectId,
        context: context ? { focusCode: context } : undefined,
      });

      if (response.data.error) {
        throw new Error(response.data.error);
      }

      const result = response.data.data;

      return {
        answer: result.answer || result.formattedAnswer || 'I could not find an answer to that question.',
        confidence: result.confidence || 0.7,
        sources: (result.sources || []).map((s: { type: string; name: string; file?: string; line?: number }) => ({
          type: s.type,
          name: s.name,
          file: s.file,
          line: s.line,
        })),
        relatedNodes: result.relatedNodes,
      };
    } catch (error) {
      return {
        answer: 'Unable to process the question. Please ensure the MigrationPilot API is running.',
        confidence: 0,
        sources: [],
      };
    }
  }

  // ============================================================================
  // HELPER METHODS
  // ============================================================================

  private mapLanguage(language: string): string {
    const mapping: Record<string, string> = {
      cobol: 'cobol',
      fortran: 'fortran',
      vb: 'vb6',
      vba: 'vba',
      java: 'java-legacy',
      plaintext: 'cobol',
    };
    return mapping[language.toLowerCase()] || language;
  }

  private extractDependencies(parsed: Record<string, unknown>): { name: string; type: 'internal' | 'external' | 'database' | 'file'; source?: string }[] {
    const deps: { name: string; type: 'internal' | 'external' | 'database' | 'file'; source?: string }[] = [];

    // Extract from copybooks
    const copybooks = parsed.copybooks as string[] | undefined;
    if (copybooks) {
      for (const cb of copybooks) {
        deps.push({ name: cb, type: 'internal', source: 'copybook' });
      }
    }

    // Extract from procedure calls
    const procedures = parsed.procedures as Array<{ calledProcedures?: string[] }> | undefined;
    if (procedures) {
      for (const proc of procedures) {
        for (const called of proc.calledProcedures || []) {
          if (!deps.find(d => d.name === called)) {
            deps.push({ name: called, type: 'internal', source: 'call' });
          }
        }
      }
    }

    return deps;
  }

  private createFallbackAnalysis(filePath: string, language: string, content: string): AnalysisResult {
    const lines = content.split('\n');
    return {
      fileId: `file_${Date.now()}`,
      filePath,
      language,
      complexity: 1,
      procedures: [],
      businessRules: [],
      dataStructures: [],
      dependencies: [],
      suggestions: [{
        id: 'suggestion_connect',
        title: 'Connect to MigrationPilot API',
        description: 'For full analysis, ensure the MigrationPilot API server is running.',
        targetLanguage: 'typescript',
        confidence: 1,
        location: { startLine: 1, endLine: lines.length },
        priority: 'high',
      }],
    };
  }

  private createFallbackExplanation(content: string, language: string): ExplanationResult {
    const lines = content.split('\n').length;
    return {
      summary: `${language} code block with ${lines} lines`,
      detailedExplanation: 'Connect to MigrationPilot API for detailed explanation.',
      complexity: lines > 50 ? 'high' : lines > 20 ? 'moderate' : 'low',
      relatedRules: [],
    };
  }

  private createFallbackSuggestions(content: string, targetLanguage: string): MigrationSuggestion[] {
    return [{
      id: 'suggestion_1',
      title: `Migrate to ${targetLanguage}`,
      description: 'Connect to MigrationPilot API for detailed migration suggestions.',
      targetLanguage,
      confidence: 0.5,
      location: { startLine: 1, endLine: content.split('\n').length },
      priority: 'medium',
    }];
  }
}

/**
 * Search Engine - Full-text search for marketplace
 */

import type {
  MigrationTemplate,
  MigrationProvider,
} from '../types.js';

// ============================================================================
// Search Engine
// ============================================================================

export class MarketplaceSearchEngine {
  private templateIndex: Map<string, IndexedDocument> = new Map();
  private providerIndex: Map<string, IndexedDocument> = new Map();

  /**
   * Index a template for search
   */
  indexTemplate(template: MigrationTemplate): void {
    const tokens = this.tokenize([
      template.name,
      template.description,
      template.longDescription || '',
      ...template.tags,
      template.author.name,
      ...template.sourceLanguages,
      ...template.targetLanguages,
    ]);

    this.templateIndex.set(template.id, {
      id: template.id,
      type: 'template',
      tokens,
      tokenFrequency: this.calculateFrequency(tokens),
      data: template,
    });
  }

  /**
   * Index a provider for search
   */
  indexProvider(provider: MigrationProvider): void {
    const tokens = this.tokenize([
      provider.name,
      provider.description,
      ...provider.capabilities.map(c => c.sourceLanguage),
      ...provider.capabilities.map(c => c.targetLanguage),
    ]);

    this.providerIndex.set(provider.id, {
      id: provider.id,
      type: 'provider',
      tokens,
      tokenFrequency: this.calculateFrequency(tokens),
      data: provider,
    });
  }

  /**
   * Remove template from index
   */
  removeTemplate(id: string): void {
    this.templateIndex.delete(id);
  }

  /**
   * Remove provider from index
   */
  removeProvider(id: string): void {
    this.providerIndex.delete(id);
  }

  /**
   * Search templates
   */
  searchTemplates(query: string, options?: SearchOptions): RankedResult<MigrationTemplate>[] {
    if (!query.trim()) {
      return Array.from(this.templateIndex.values())
        .map(doc => ({
          item: doc.data as MigrationTemplate,
          score: 1,
          matches: [],
        }));
    }

    const queryTokens = this.tokenize([query]);
    const results: RankedResult<MigrationTemplate>[] = [];

    for (const doc of this.templateIndex.values()) {
      const score = this.calculateScore(queryTokens, doc);
      if (score > 0) {
        results.push({
          item: doc.data as MigrationTemplate,
          score,
          matches: this.findMatches(queryTokens, doc),
        });
      }
    }

    // Sort by score descending
    results.sort((a, b) => b.score - a.score);

    // Apply limit
    if (options?.limit) {
      return results.slice(0, options.limit);
    }

    return results;
  }

  /**
   * Search providers
   */
  searchProviders(query: string, options?: SearchOptions): RankedResult<MigrationProvider>[] {
    if (!query.trim()) {
      return Array.from(this.providerIndex.values())
        .map(doc => ({
          item: doc.data as MigrationProvider,
          score: 1,
          matches: [],
        }));
    }

    const queryTokens = this.tokenize([query]);
    const results: RankedResult<MigrationProvider>[] = [];

    for (const doc of this.providerIndex.values()) {
      const score = this.calculateScore(queryTokens, doc);
      if (score > 0) {
        results.push({
          item: doc.data as MigrationProvider,
          score,
          matches: this.findMatches(queryTokens, doc),
        });
      }
    }

    results.sort((a, b) => b.score - a.score);

    if (options?.limit) {
      return results.slice(0, options.limit);
    }

    return results;
  }

  /**
   * Search across all types
   */
  searchAll(query: string, options?: SearchOptions): RankedResult<MigrationTemplate | MigrationProvider>[] {
    const templateResults = this.searchTemplates(query, options);
    const providerResults = this.searchProviders(query, options);

    const combined = [
      ...templateResults.map(r => ({ ...r, type: 'template' as const })),
      ...providerResults.map(r => ({ ...r, type: 'provider' as const })),
    ];

    combined.sort((a, b) => b.score - a.score);

    if (options?.limit) {
      return combined.slice(0, options.limit);
    }

    return combined;
  }

  /**
   * Get search suggestions
   */
  getSuggestions(prefix: string, limit: number = 10): string[] {
    if (!prefix || prefix.length < 2) return [];

    const prefixLower = prefix.toLowerCase();
    const suggestions = new Set<string>();

    // Collect matching tokens from templates
    for (const doc of this.templateIndex.values()) {
      for (const token of doc.tokens) {
        if (token.startsWith(prefixLower) && token !== prefixLower) {
          suggestions.add(token);
        }
      }
    }

    // Collect matching tokens from providers
    for (const doc of this.providerIndex.values()) {
      for (const token of doc.tokens) {
        if (token.startsWith(prefixLower) && token !== prefixLower) {
          suggestions.add(token);
        }
      }
    }

    return Array.from(suggestions).slice(0, limit);
  }

  /**
   * Get popular searches
   */
  getPopularSearches(): string[] {
    // Return common migration-related terms
    return [
      'cobol to java',
      'mainframe migration',
      'db2 to postgresql',
      'legacy modernization',
      'cloud migration',
      'microservices',
      'api migration',
    ];
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private tokenize(texts: string[]): string[] {
    const tokens: string[] = [];

    for (const text of texts) {
      const words = text
        .toLowerCase()
        .replace(/[^a-z0-9\s]/g, ' ')
        .split(/\s+/)
        .filter(w => w.length > 1);

      tokens.push(...words);
    }

    return tokens;
  }

  private calculateFrequency(tokens: string[]): Map<string, number> {
    const freq = new Map<string, number>();
    for (const token of tokens) {
      freq.set(token, (freq.get(token) || 0) + 1);
    }
    return freq;
  }

  private calculateScore(queryTokens: string[], doc: IndexedDocument): number {
    let score = 0;
    const docTokens = doc.tokenFrequency;

    for (const queryToken of queryTokens) {
      // Exact match
      if (docTokens.has(queryToken)) {
        score += docTokens.get(queryToken)! * 10;
      }

      // Partial match
      for (const [docToken, freq] of docTokens) {
        if (docToken.includes(queryToken) || queryToken.includes(docToken)) {
          score += freq * 5;
        }
      }
    }

    // Boost based on document importance (for templates: rating, downloads)
    if (doc.type === 'template') {
      const template = doc.data as MigrationTemplate;
      score *= 1 + template.stats.rating * 0.1;
      score *= 1 + Math.log(template.stats.downloads + 1) * 0.05;
    }

    if (doc.type === 'provider') {
      const provider = doc.data as MigrationProvider;
      score *= 1 + provider.stats.averageRating * 0.1;
      score *= 1 + Math.log(provider.stats.projectsCompleted + 1) * 0.05;
    }

    return score;
  }

  private findMatches(queryTokens: string[], doc: IndexedDocument): MatchHighlight[] {
    const matches: MatchHighlight[] = [];

    for (const queryToken of queryTokens) {
      for (const docToken of doc.tokens) {
        if (docToken.includes(queryToken)) {
          matches.push({
            field: 'content',
            token: docToken,
            queryToken,
          });
        }
      }
    }

    // Dedupe
    const seen = new Set<string>();
    return matches.filter(m => {
      const key = `${m.field}:${m.token}`;
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }
}

// ============================================================================
// Types
// ============================================================================

interface IndexedDocument {
  id: string;
  type: 'template' | 'provider';
  tokens: string[];
  tokenFrequency: Map<string, number>;
  data: MigrationTemplate | MigrationProvider;
}

export interface SearchOptions {
  limit?: number;
  offset?: number;
  boostFields?: string[];
}

export interface RankedResult<T> {
  item: T;
  score: number;
  matches: MatchHighlight[];
}

export interface MatchHighlight {
  field: string;
  token: string;
  queryToken: string;
}

// ============================================================================
// Factory
// ============================================================================

export function createSearchEngine(): MarketplaceSearchEngine {
  return new MarketplaceSearchEngine();
}

/**
 * Template Registry - Manages migration templates
 */

import type {
  MigrationTemplate,
  TemplateCategory,
  TemplateVisibility,
  TemplateAuthor,
  TemplateConfiguration,
  TemplateFile,
  TemplatePricing,
  SearchQuery,
  SearchResult,
  SearchFacets,
  FacetCount,
} from '../types.js';

// ============================================================================
// Template Registry
// ============================================================================

export class TemplateRegistry {
  private templates: Map<string, MigrationTemplate> = new Map();
  private templatesBySlug: Map<string, string> = new Map();
  private templatesByCategory: Map<TemplateCategory, Set<string>> = new Map();
  private templatesByAuthor: Map<string, Set<string>> = new Map();

  /**
   * Register a new template
   */
  async register(input: CreateTemplateInput): Promise<MigrationTemplate> {
    const id = `tpl_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const slug = this.generateSlug(input.name);

    if (this.templatesBySlug.has(slug)) {
      throw new Error(`Template with slug "${slug}" already exists`);
    }

    const template: MigrationTemplate = {
      id,
      name: input.name,
      slug,
      version: input.version || '1.0.0',
      description: input.description,
      longDescription: input.longDescription,
      author: input.author,
      category: input.category,
      tags: input.tags || [],
      sourceLanguages: input.sourceLanguages,
      targetLanguages: input.targetLanguages,
      pricing: input.pricing || { model: 'free' },
      stats: {
        downloads: 0,
        uses: 0,
        rating: 0,
        reviewCount: 0,
        successRate: 0,
      },
      configuration: input.configuration || {
        requiredInputs: [],
        optionalInputs: [],
        outputFormats: [],
        hooks: [],
      },
      files: input.files || [],
      status: 'draft',
      visibility: input.visibility || 'private',
      createdAt: new Date(),
      updatedAt: new Date(),
    };

    this.templates.set(id, template);
    this.templatesBySlug.set(slug, id);
    this.indexTemplate(template);

    return template;
  }

  /**
   * Get template by ID
   */
  async getById(id: string): Promise<MigrationTemplate | null> {
    return this.templates.get(id) || null;
  }

  /**
   * Get template by slug
   */
  async getBySlug(slug: string): Promise<MigrationTemplate | null> {
    const id = this.templatesBySlug.get(slug);
    return id ? this.templates.get(id) || null : null;
  }

  /**
   * Update template
   */
  async update(id: string, updates: Partial<UpdateTemplateInput>): Promise<MigrationTemplate | null> {
    const template = this.templates.get(id);
    if (!template) return null;

    // Remove from old indexes
    this.removeFromIndex(template);

    // Apply updates
    Object.assign(template, updates, { updatedAt: new Date() });

    // Re-index
    this.indexTemplate(template);

    return template;
  }

  /**
   * Publish template
   */
  async publish(id: string): Promise<MigrationTemplate | null> {
    const template = this.templates.get(id);
    if (!template) return null;

    if (template.status !== 'draft' && template.status !== 'rejected') {
      throw new Error(`Cannot publish template with status: ${template.status}`);
    }

    template.status = 'pending-review';
    template.updatedAt = new Date();

    return template;
  }

  /**
   * Approve template
   */
  async approve(id: string): Promise<MigrationTemplate | null> {
    const template = this.templates.get(id);
    if (!template) return null;

    template.status = 'approved';
    template.publishedAt = new Date();
    template.updatedAt = new Date();

    return template;
  }

  /**
   * Search templates
   */
  async search(query: SearchQuery): Promise<SearchResult<MigrationTemplate>> {
    let results = Array.from(this.templates.values());

    // Filter by visibility (only public/approved for general search)
    results = results.filter(t => 
      t.visibility === 'public' && t.status === 'approved'
    );

    // Text search
    if (query.text) {
      const text = query.text.toLowerCase();
      results = results.filter(t =>
        t.name.toLowerCase().includes(text) ||
        t.description.toLowerCase().includes(text) ||
        t.tags.some(tag => tag.toLowerCase().includes(text))
      );
    }

    // Category filter
    if (query.categories && query.categories.length > 0) {
      results = results.filter(t => query.categories!.includes(t.category));
    }

    // Source language filter
    if (query.sourceLanguages && query.sourceLanguages.length > 0) {
      results = results.filter(t =>
        t.sourceLanguages.some(lang => query.sourceLanguages!.includes(lang))
      );
    }

    // Target language filter
    if (query.targetLanguages && query.targetLanguages.length > 0) {
      results = results.filter(t =>
        t.targetLanguages.some(lang => query.targetLanguages!.includes(lang))
      );
    }

    // Pricing model filter
    if (query.pricingModel && query.pricingModel.length > 0) {
      results = results.filter(t => query.pricingModel!.includes(t.pricing.model));
    }

    // Rating filter
    if (query.minRating !== undefined) {
      results = results.filter(t => t.stats.rating >= query.minRating!);
    }

    // Max price filter
    if (query.maxPrice !== undefined) {
      results = results.filter(t =>
        t.pricing.model === 'free' || (t.pricing.amount !== undefined && t.pricing.amount <= query.maxPrice!)
      );
    }

    // Tags filter
    if (query.tags && query.tags.length > 0) {
      results = results.filter(t =>
        query.tags!.some(tag => t.tags.includes(tag))
      );
    }

    // Author filter
    if (query.author) {
      results = results.filter(t => t.author.id === query.author);
    }

    // Calculate facets before pagination
    const facets = this.calculateFacets(results);

    // Sort
    results = this.sortResults(results, query.sortBy || 'relevance', query.sortOrder || 'desc');

    // Pagination
    const page = query.page || 1;
    const pageSize = query.pageSize || 20;
    const startIndex = (page - 1) * pageSize;
    const paginatedResults = results.slice(startIndex, startIndex + pageSize);

    return {
      items: paginatedResults,
      total: results.length,
      page,
      pageSize,
      totalPages: Math.ceil(results.length / pageSize),
      facets,
    };
  }

  /**
   * Get templates by category
   */
  async getByCategory(category: TemplateCategory): Promise<MigrationTemplate[]> {
    const ids = this.templatesByCategory.get(category) || new Set();
    return Array.from(ids)
      .map(id => this.templates.get(id))
      .filter((t): t is MigrationTemplate => t !== undefined && t.status === 'approved');
  }

  /**
   * Get templates by author
   */
  async getByAuthor(authorId: string): Promise<MigrationTemplate[]> {
    const ids = this.templatesByAuthor.get(authorId) || new Set();
    return Array.from(ids)
      .map(id => this.templates.get(id))
      .filter((t): t is MigrationTemplate => t !== undefined);
  }

  /**
   * Increment download count
   */
  async recordDownload(id: string): Promise<void> {
    const template = this.templates.get(id);
    if (template) {
      template.stats.downloads++;
    }
  }

  /**
   * Increment usage count
   */
  async recordUsage(id: string, success: boolean): Promise<void> {
    const template = this.templates.get(id);
    if (template) {
      template.stats.uses++;
      // Recalculate success rate
      const totalSuccesses = template.stats.successRate * (template.stats.uses - 1);
      template.stats.successRate = (totalSuccesses + (success ? 1 : 0)) / template.stats.uses;
    }
  }

  /**
   * Add rating
   */
  async addRating(id: string, rating: number): Promise<void> {
    const template = this.templates.get(id);
    if (template) {
      const totalRating = template.stats.rating * template.stats.reviewCount;
      template.stats.reviewCount++;
      template.stats.rating = (totalRating + rating) / template.stats.reviewCount;
    }
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private generateSlug(name: string): string {
    return name
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
  }

  private indexTemplate(template: MigrationTemplate): void {
    // Index by category
    let categorySet = this.templatesByCategory.get(template.category);
    if (!categorySet) {
      categorySet = new Set();
      this.templatesByCategory.set(template.category, categorySet);
    }
    categorySet.add(template.id);

    // Index by author
    let authorSet = this.templatesByAuthor.get(template.author.id);
    if (!authorSet) {
      authorSet = new Set();
      this.templatesByAuthor.set(template.author.id, authorSet);
    }
    authorSet.add(template.id);
  }

  private removeFromIndex(template: MigrationTemplate): void {
    const categorySet = this.templatesByCategory.get(template.category);
    if (categorySet) categorySet.delete(template.id);

    const authorSet = this.templatesByAuthor.get(template.author.id);
    if (authorSet) authorSet.delete(template.id);
  }

  private sortResults(
    results: MigrationTemplate[],
    sortBy: string,
    sortOrder: 'asc' | 'desc'
  ): MigrationTemplate[] {
    const multiplier = sortOrder === 'asc' ? 1 : -1;

    return [...results].sort((a, b) => {
      switch (sortBy) {
        case 'downloads':
          return (a.stats.downloads - b.stats.downloads) * multiplier;
        case 'rating':
          return (a.stats.rating - b.stats.rating) * multiplier;
        case 'date':
          return (a.createdAt.getTime() - b.createdAt.getTime()) * multiplier;
        case 'price':
          const priceA = a.pricing.amount || 0;
          const priceB = b.pricing.amount || 0;
          return (priceA - priceB) * multiplier;
        case 'relevance':
        default:
          // Simple relevance: combine rating and downloads
          const scoreA = a.stats.rating * 0.5 + Math.log(a.stats.downloads + 1) * 0.5;
          const scoreB = b.stats.rating * 0.5 + Math.log(b.stats.downloads + 1) * 0.5;
          return (scoreA - scoreB) * multiplier;
      }
    });
  }

  private calculateFacets(templates: MigrationTemplate[]): SearchFacets {
    const categories = new Map<string, number>();
    const sourceLanguages = new Map<string, number>();
    const targetLanguages = new Map<string, number>();
    const pricingModels = new Map<string, number>();
    const ratings = new Map<string, number>();

    for (const template of templates) {
      // Categories
      categories.set(
        template.category,
        (categories.get(template.category) || 0) + 1
      );

      // Source languages
      for (const lang of template.sourceLanguages) {
        sourceLanguages.set(lang, (sourceLanguages.get(lang) || 0) + 1);
      }

      // Target languages
      for (const lang of template.targetLanguages) {
        targetLanguages.set(lang, (targetLanguages.get(lang) || 0) + 1);
      }

      // Pricing models
      pricingModels.set(
        template.pricing.model,
        (pricingModels.get(template.pricing.model) || 0) + 1
      );

      // Ratings (buckets: 1-2, 2-3, 3-4, 4-5)
      const ratingBucket = Math.floor(template.stats.rating);
      const bucketKey = `${ratingBucket}+`;
      ratings.set(bucketKey, (ratings.get(bucketKey) || 0) + 1);
    }

    const toFacetArray = (map: Map<string, number>): FacetCount[] =>
      Array.from(map.entries())
        .map(([value, count]) => ({ value, count }))
        .sort((a, b) => b.count - a.count);

    return {
      categories: toFacetArray(categories),
      sourceLanguages: toFacetArray(sourceLanguages),
      targetLanguages: toFacetArray(targetLanguages),
      pricingModels: toFacetArray(pricingModels),
      ratings: toFacetArray(ratings),
    };
  }
}

// ============================================================================
// Input Types
// ============================================================================

export interface CreateTemplateInput {
  name: string;
  version?: string;
  description: string;
  longDescription?: string;
  author: TemplateAuthor;
  category: TemplateCategory;
  tags?: string[];
  sourceLanguages: string[];
  targetLanguages: string[];
  pricing?: TemplatePricing;
  configuration?: TemplateConfiguration;
  files?: TemplateFile[];
  visibility?: TemplateVisibility;
}

export interface UpdateTemplateInput {
  name?: string;
  description?: string;
  longDescription?: string;
  category?: TemplateCategory;
  tags?: string[];
  pricing?: TemplatePricing;
  configuration?: TemplateConfiguration;
  files?: TemplateFile[];
  visibility?: TemplateVisibility;
}

// ============================================================================
// Factory
// ============================================================================

export function createTemplateRegistry(): TemplateRegistry {
  return new TemplateRegistry();
}

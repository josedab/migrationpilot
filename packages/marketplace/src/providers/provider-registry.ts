/**
 * Provider Registry - Manages migration service providers
 */

import type {
  MigrationProvider,
  ProviderCapability,
  ProviderPricing,
  ProviderAvailability,
  ProviderVerification,
  ProviderContact,
  SearchQuery,
  SearchResult,
  FacetCount,
} from '../types.js';

// ============================================================================
// Provider Registry
// ============================================================================

export class ProviderRegistry {
  private providers: Map<string, MigrationProvider> = new Map();
  private providersBySlug: Map<string, string> = new Map();
  private providersByCapability: Map<string, Set<string>> = new Map();

  /**
   * Register a new provider
   */
  async register(input: CreateProviderInput): Promise<MigrationProvider> {
    const id = `prv_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const slug = this.generateSlug(input.name);

    if (this.providersBySlug.has(slug)) {
      throw new Error(`Provider with slug "${slug}" already exists`);
    }

    const provider: MigrationProvider = {
      id,
      name: input.name,
      slug,
      description: input.description,
      tenantId: input.tenantId,
      capabilities: input.capabilities,
      pricing: input.pricing,
      availability: input.availability || this.getDefaultAvailability(),
      stats: {
        projectsCompleted: 0,
        totalLocMigrated: 0,
        averageRating: 0,
        reviewCount: 0,
        onTimeDelivery: 1,
        repeatClientRate: 0,
      },
      verification: {
        identityVerified: false,
        skillsVerified: false,
        backgroundChecked: false,
        enterpriseApproved: false,
        certifications: [],
      },
      contact: input.contact,
      status: 'pending-verification',
      createdAt: new Date(),
      updatedAt: new Date(),
    };

    this.providers.set(id, provider);
    this.providersBySlug.set(slug, id);
    this.indexProvider(provider);

    return provider;
  }

  /**
   * Get provider by ID
   */
  async getById(id: string): Promise<MigrationProvider | null> {
    return this.providers.get(id) || null;
  }

  /**
   * Get provider by slug
   */
  async getBySlug(slug: string): Promise<MigrationProvider | null> {
    const id = this.providersBySlug.get(slug);
    return id ? this.providers.get(id) || null : null;
  }

  /**
   * Update provider
   */
  async update(id: string, updates: Partial<UpdateProviderInput>): Promise<MigrationProvider | null> {
    const provider = this.providers.get(id);
    if (!provider) return null;

    // Remove from old indexes
    this.removeFromIndex(provider);

    // Apply updates
    Object.assign(provider, updates, { updatedAt: new Date() });

    // Re-index
    this.indexProvider(provider);

    return provider;
  }

  /**
   * Verify provider
   */
  async verify(
    id: string,
    verification: Partial<ProviderVerification>
  ): Promise<MigrationProvider | null> {
    const provider = this.providers.get(id);
    if (!provider) return null;

    Object.assign(provider.verification, verification);
    provider.updatedAt = new Date();

    // Update status if fully verified
    if (
      provider.verification.identityVerified &&
      provider.verification.skillsVerified
    ) {
      provider.status = 'active';
    }

    return provider;
  }

  /**
   * Search providers
   */
  async search(query: ProviderSearchQuery): Promise<SearchResult<MigrationProvider>> {
    let results = Array.from(this.providers.values());

    // Filter by status (only active for general search)
    results = results.filter(p => p.status === 'active');

    // Text search
    if (query.text) {
      const text = query.text.toLowerCase();
      results = results.filter(p =>
        p.name.toLowerCase().includes(text) ||
        p.description.toLowerCase().includes(text)
      );
    }

    // Source language filter
    if (query.sourceLanguages && query.sourceLanguages.length > 0) {
      results = results.filter(p =>
        p.capabilities.some(cap =>
          query.sourceLanguages!.includes(cap.sourceLanguage)
        )
      );
    }

    // Target language filter
    if (query.targetLanguages && query.targetLanguages.length > 0) {
      results = results.filter(p =>
        p.capabilities.some(cap =>
          query.targetLanguages!.includes(cap.targetLanguage)
        )
      );
    }

    // Complexity filter
    if (query.complexity) {
      results = results.filter(p =>
        p.capabilities.some(cap => cap.complexity === query.complexity)
      );
    }

    // Min rating filter
    if (query.minRating !== undefined) {
      results = results.filter(p => p.stats.averageRating >= query.minRating!);
    }

    // Verified only filter
    if (query.verifiedOnly) {
      results = results.filter(p =>
        p.verification.identityVerified && p.verification.skillsVerified
      );
    }

    // Enterprise approved filter
    if (query.enterpriseApproved) {
      results = results.filter(p => p.verification.enterpriseApproved);
    }

    // Calculate facets
    const facets = this.calculateProviderFacets(results);

    // Sort
    results = this.sortResults(results, query.sortBy || 'rating', query.sortOrder || 'desc');

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
      facets: {
        categories: [],
        sourceLanguages: facets.sourceLanguages,
        targetLanguages: facets.targetLanguages,
        pricingModels: facets.pricingModels,
        ratings: facets.ratings,
      },
    };
  }

  /**
   * Find providers by capability
   */
  async findByCapability(
    sourceLanguage: string,
    targetLanguage: string
  ): Promise<MigrationProvider[]> {
    const key = `${sourceLanguage}:${targetLanguage}`;
    const ids = this.providersByCapability.get(key) || new Set();
    return Array.from(ids)
      .map(id => this.providers.get(id))
      .filter((p): p is MigrationProvider => 
        p !== undefined && p.status === 'active'
      );
  }

  /**
   * Record completed project
   */
  async recordProject(
    id: string,
    locMigrated: number,
    onTime: boolean,
    isRepeatClient: boolean
  ): Promise<void> {
    const provider = this.providers.get(id);
    if (provider) {
      const stats = provider.stats;
      const prevProjects = stats.projectsCompleted;

      stats.projectsCompleted++;
      stats.totalLocMigrated += locMigrated;

      // Update on-time delivery rate
      const prevOnTimeTotal = stats.onTimeDelivery * prevProjects;
      stats.onTimeDelivery = (prevOnTimeTotal + (onTime ? 1 : 0)) / stats.projectsCompleted;

      // Update repeat client rate
      const prevRepeatTotal = stats.repeatClientRate * prevProjects;
      stats.repeatClientRate = (prevRepeatTotal + (isRepeatClient ? 1 : 0)) / stats.projectsCompleted;
    }
  }

  /**
   * Add rating
   */
  async addRating(id: string, rating: number): Promise<void> {
    const provider = this.providers.get(id);
    if (provider) {
      const totalRating = provider.stats.averageRating * provider.stats.reviewCount;
      provider.stats.reviewCount++;
      provider.stats.averageRating = (totalRating + rating) / provider.stats.reviewCount;
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

  private getDefaultAvailability(): ProviderAvailability {
    return {
      timezone: 'UTC',
      workingHours: { start: '09:00', end: '17:00' },
      workingDays: [1, 2, 3, 4, 5],
      currentCapacity: 100,
      maxCapacity: 100,
      leadTimeDays: 7,
    };
  }

  private indexProvider(provider: MigrationProvider): void {
    for (const cap of provider.capabilities) {
      const key = `${cap.sourceLanguage}:${cap.targetLanguage}`;
      let capSet = this.providersByCapability.get(key);
      if (!capSet) {
        capSet = new Set();
        this.providersByCapability.set(key, capSet);
      }
      capSet.add(provider.id);
    }
  }

  private removeFromIndex(provider: MigrationProvider): void {
    for (const cap of provider.capabilities) {
      const key = `${cap.sourceLanguage}:${cap.targetLanguage}`;
      const capSet = this.providersByCapability.get(key);
      if (capSet) capSet.delete(provider.id);
    }
  }

  private sortResults(
    results: MigrationProvider[],
    sortBy: string,
    sortOrder: 'asc' | 'desc'
  ): MigrationProvider[] {
    const multiplier = sortOrder === 'asc' ? 1 : -1;

    return [...results].sort((a, b) => {
      switch (sortBy) {
        case 'projects':
          return (a.stats.projectsCompleted - b.stats.projectsCompleted) * multiplier;
        case 'rating':
          return (a.stats.averageRating - b.stats.averageRating) * multiplier;
        case 'price':
          const priceA = a.pricing.baseRate || 0;
          const priceB = b.pricing.baseRate || 0;
          return (priceA - priceB) * multiplier;
        default:
          return 0;
      }
    });
  }

  private calculateProviderFacets(providers: MigrationProvider[]): {
    sourceLanguages: FacetCount[];
    targetLanguages: FacetCount[];
    pricingModels: FacetCount[];
    ratings: FacetCount[];
  } {
    const sourceLanguages = new Map<string, number>();
    const targetLanguages = new Map<string, number>();
    const pricingModels = new Map<string, number>();
    const ratings = new Map<string, number>();

    for (const provider of providers) {
      for (const cap of provider.capabilities) {
        sourceLanguages.set(
          cap.sourceLanguage,
          (sourceLanguages.get(cap.sourceLanguage) || 0) + 1
        );
        targetLanguages.set(
          cap.targetLanguage,
          (targetLanguages.get(cap.targetLanguage) || 0) + 1
        );
      }

      pricingModels.set(
        provider.pricing.model,
        (pricingModels.get(provider.pricing.model) || 0) + 1
      );

      const ratingBucket = Math.floor(provider.stats.averageRating);
      const bucketKey = `${ratingBucket}+`;
      ratings.set(bucketKey, (ratings.get(bucketKey) || 0) + 1);
    }

    const toFacetArray = (map: Map<string, number>): FacetCount[] =>
      Array.from(map.entries())
        .map(([value, count]) => ({ value, count }))
        .sort((a, b) => b.count - a.count);

    return {
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

export interface CreateProviderInput {
  name: string;
  description: string;
  tenantId: string;
  capabilities: ProviderCapability[];
  pricing: ProviderPricing;
  availability?: ProviderAvailability;
  contact: ProviderContact;
}

export interface UpdateProviderInput {
  name?: string;
  description?: string;
  capabilities?: ProviderCapability[];
  pricing?: ProviderPricing;
  availability?: ProviderAvailability;
  contact?: ProviderContact;
}

export interface ProviderSearchQuery extends SearchQuery {
  complexity?: 'simple' | 'moderate' | 'complex' | 'enterprise';
  verifiedOnly?: boolean;
  enterpriseApproved?: boolean;
}

// ============================================================================
// Factory
// ============================================================================

export function createProviderRegistry(): ProviderRegistry {
  return new ProviderRegistry();
}

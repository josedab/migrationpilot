/**
 * Marketplace Service
 * Core service for multi-tenant marketplace operations
 */

import type {
  Tenant,
  TenantPlan,
  MarketplaceListing,
  ListingContent,
  ListingStatus,
  Review,
  Transaction,
  SearchQuery,
  SearchResult,
  SearchFacets,
} from './types.js';

export class MarketplaceService {
  private tenants: Map<string, Tenant> = new Map();
  private listings: Map<string, MarketplaceListing> = new Map();
  private reviews: Map<string, Review[]> = new Map();
  private transactions: Map<string, Transaction[]> = new Map();

  // ============================================================================
  // TENANT MANAGEMENT
  // ============================================================================

  async createTenant(name: string, slug: string, plan: TenantPlan = 'free'): Promise<Tenant> {
    const id = `tenant_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const tenant: Tenant = {
      id,
      name,
      slug: this.slugify(slug),
      verified: false,
      plan,
      settings: {
        allowPublicListings: true,
        requireApproval: plan !== 'enterprise',
        defaultLicense: 'mit',
        pricingEnabled: plan !== 'free',
        defaultCurrency: 'USD',
      },
      stats: {
        totalListings: 0,
        totalDownloads: 0,
        totalRevenue: 0,
        averageRating: 0,
      },
      createdAt: now,
      updatedAt: now,
    };

    this.tenants.set(id, tenant);
    return tenant;
  }

  async getTenant(tenantId: string): Promise<Tenant | null> {
    return this.tenants.get(tenantId) || null;
  }

  async getTenantBySlug(slug: string): Promise<Tenant | null> {
    for (const tenant of this.tenants.values()) {
      if (tenant.slug === slug) {
        return tenant;
      }
    }
    return null;
  }

  async updateTenant(tenantId: string, updates: Partial<Tenant>): Promise<Tenant> {
    const tenant = this.tenants.get(tenantId);
    if (!tenant) {
      throw new Error(`Tenant not found: ${tenantId}`);
    }

    const updated = {
      ...tenant,
      ...updates,
      id: tenant.id,
      updatedAt: new Date(),
    };

    this.tenants.set(tenantId, updated);
    return updated;
  }

  async listTenants(): Promise<Tenant[]> {
    return Array.from(this.tenants.values());
  }

  // ============================================================================
  // LISTING MANAGEMENT
  // ============================================================================

  async createListing(
    tenantId: string,
    data: Partial<MarketplaceListing>
  ): Promise<MarketplaceListing> {
    const tenant = this.tenants.get(tenantId);
    if (!tenant) {
      throw new Error(`Tenant not found: ${tenantId}`);
    }

    const id = `listing_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const listing: MarketplaceListing = {
      id,
      tenantId,
      type: data.type || 'rule-pack',
      name: data.name || 'Untitled Listing',
      slug: this.slugify(data.name || 'untitled'),
      description: data.description || '',
      version: data.version || '1.0.0',
      category: data.category || 'general',
      tags: data.tags || [],
      sourceLanguages: data.sourceLanguages || [],
      targetLanguages: data.targetLanguages || [],
      pricing: data.pricing || { type: 'free' },
      license: data.license || tenant.settings.defaultLicense,
      visibility: data.visibility || 'public',
      status: 'draft',
      content: data.content || {},
      metadata: data.metadata || {
        author: tenant.name,
        keywords: [],
        dependencies: [],
      },
      stats: {
        downloads: 0,
        views: 0,
        stars: 0,
        forks: 0,
        averageRating: 0,
        totalReviews: 0,
      },
      reviews: [],
      createdAt: now,
      updatedAt: now,
    };

    this.listings.set(id, listing);

    tenant.stats.totalListings++;
    this.tenants.set(tenantId, { ...tenant, updatedAt: now });

    return listing;
  }

  async getListing(listingId: string): Promise<MarketplaceListing | null> {
    const listing = this.listings.get(listingId);
    if (listing) {
      listing.stats.views++;
      this.listings.set(listingId, listing);
    }
    return listing || null;
  }

  async getListingBySlug(tenantSlug: string, listingSlug: string): Promise<MarketplaceListing | null> {
    const tenant = await this.getTenantBySlug(tenantSlug);
    if (!tenant) return null;

    for (const listing of this.listings.values()) {
      if (listing.tenantId === tenant.id && listing.slug === listingSlug) {
        return listing;
      }
    }
    return null;
  }

  async updateListing(
    listingId: string,
    updates: Partial<MarketplaceListing>
  ): Promise<MarketplaceListing> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    const updated: MarketplaceListing = {
      ...listing,
      ...updates,
      id: listing.id,
      tenantId: listing.tenantId,
      updatedAt: new Date(),
    };

    this.listings.set(listingId, updated);
    return updated;
  }

  async publishListing(listingId: string): Promise<MarketplaceListing> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    const tenant = this.tenants.get(listing.tenantId);
    if (!tenant) {
      throw new Error(`Tenant not found: ${listing.tenantId}`);
    }

    const status: ListingStatus = tenant.settings.requireApproval ? 'pending-review' : 'published';

    const updated = {
      ...listing,
      status,
      publishedAt: status === 'published' ? new Date() : undefined,
      updatedAt: new Date(),
    };

    this.listings.set(listingId, updated);
    return updated;
  }

  async archiveListing(listingId: string): Promise<void> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    listing.status = 'archived';
    listing.updatedAt = new Date();
    this.listings.set(listingId, listing);

    const tenant = this.tenants.get(listing.tenantId);
    if (tenant) {
      tenant.stats.totalListings--;
      this.tenants.set(listing.tenantId, tenant);
    }
  }

  async listTenantListings(tenantId: string): Promise<MarketplaceListing[]> {
    return Array.from(this.listings.values())
      .filter(l => l.tenantId === tenantId);
  }

  // ============================================================================
  // SEARCH
  // ============================================================================

  async search(query: SearchQuery): Promise<SearchResult> {
    let results = Array.from(this.listings.values())
      .filter(l => l.status === 'published' && l.visibility === 'public');

    if (query.query) {
      const q = query.query.toLowerCase();
      results = results.filter(l =>
        l.name.toLowerCase().includes(q) ||
        l.description.toLowerCase().includes(q) ||
        l.tags.some(t => t.toLowerCase().includes(q))
      );
    }

    if (query.type) {
      results = results.filter(l => l.type === query.type);
    }

    if (query.category) {
      results = results.filter(l => l.category === query.category);
    }

    if (query.sourceLanguage) {
      results = results.filter(l => l.sourceLanguages.includes(query.sourceLanguage!));
    }

    if (query.targetLanguage) {
      results = results.filter(l => l.targetLanguages.includes(query.targetLanguage!));
    }

    if (query.pricingType) {
      results = results.filter(l => l.pricing.type === query.pricingType);
    }

    if (query.minRating !== undefined) {
      results = results.filter(l => l.stats.averageRating >= query.minRating!);
    }

    if (query.tags && query.tags.length > 0) {
      results = results.filter(l =>
        query.tags!.some(tag => l.tags.includes(tag))
      );
    }

    if (query.tenantId) {
      results = results.filter(l => l.tenantId === query.tenantId);
    }

    const facets = this.calculateFacets(results);

    results = this.sortResults(results, query.sortBy || 'relevance', query.sortOrder || 'desc');

    const page = query.page || 1;
    const pageSize = query.pageSize || 20;
    const start = (page - 1) * pageSize;
    const paginatedResults = results.slice(start, start + pageSize);

    return {
      listings: paginatedResults,
      total: results.length,
      page,
      pageSize,
      facets,
    };
  }

  private sortResults(
    results: MarketplaceListing[],
    sortBy: string,
    sortOrder: string
  ): MarketplaceListing[] {
    const multiplier = sortOrder === 'asc' ? 1 : -1;

    return [...results].sort((a, b) => {
      switch (sortBy) {
        case 'downloads':
          return (a.stats.downloads - b.stats.downloads) * multiplier;
        case 'rating':
          return (a.stats.averageRating - b.stats.averageRating) * multiplier;
        case 'newest':
          return (new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime()) * multiplier;
        case 'price':
          return ((a.pricing.amount || 0) - (b.pricing.amount || 0)) * multiplier;
        case 'relevance':
        default:
          const scoreA = a.stats.downloads * 0.3 + a.stats.averageRating * 20 + (a.stats.stars * 2);
          const scoreB = b.stats.downloads * 0.3 + b.stats.averageRating * 20 + (b.stats.stars * 2);
          return (scoreB - scoreA);
      }
    });
  }

  private calculateFacets(listings: MarketplaceListing[]): SearchFacets {
    const typeCounts = new Map<string, number>();
    const categoryCounts = new Map<string, number>();
    const sourceLangCounts = new Map<string, number>();
    const targetLangCounts = new Map<string, number>();
    const pricingCounts = new Map<string, number>();

    for (const listing of listings) {
      typeCounts.set(listing.type, (typeCounts.get(listing.type) || 0) + 1);
      categoryCounts.set(listing.category, (categoryCounts.get(listing.category) || 0) + 1);
      pricingCounts.set(listing.pricing.type, (pricingCounts.get(listing.pricing.type) || 0) + 1);

      for (const lang of listing.sourceLanguages) {
        sourceLangCounts.set(lang, (sourceLangCounts.get(lang) || 0) + 1);
      }
      for (const lang of listing.targetLanguages) {
        targetLangCounts.set(lang, (targetLangCounts.get(lang) || 0) + 1);
      }
    }

    return {
      types: Array.from(typeCounts.entries()).map(([value, count]) => ({ value, count })),
      categories: Array.from(categoryCounts.entries()).map(([value, count]) => ({ value, count })),
      sourceLanguages: Array.from(sourceLangCounts.entries()).map(([value, count]) => ({ value, count })),
      targetLanguages: Array.from(targetLangCounts.entries()).map(([value, count]) => ({ value, count })),
      pricingTypes: Array.from(pricingCounts.entries()).map(([value, count]) => ({ value, count })),
    };
  }

  // ============================================================================
  // DOWNLOADS AND PURCHASES
  // ============================================================================

  async downloadListing(listingId: string, userId: string): Promise<ListingContent> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    if (listing.status !== 'published') {
      throw new Error('Listing is not available for download');
    }

    if (listing.pricing.type === 'paid' || listing.pricing.type === 'subscription') {
      const hasPurchased = await this.checkPurchase(listingId, userId);
      if (!hasPurchased) {
        throw new Error('Purchase required to download this listing');
      }
    }

    listing.stats.downloads++;
    this.listings.set(listingId, listing);

    const tenant = this.tenants.get(listing.tenantId);
    if (tenant) {
      tenant.stats.totalDownloads++;
      this.tenants.set(listing.tenantId, tenant);
    }

    return listing.content;
  }

  async purchaseListing(listingId: string, userId: string): Promise<Transaction> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    if (listing.pricing.type === 'free') {
      throw new Error('This listing is free');
    }

    const id = `txn_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const transaction: Transaction = {
      id,
      tenantId: listing.tenantId,
      buyerId: userId,
      listingId,
      type: listing.pricing.type === 'subscription' ? 'subscription' : 'purchase',
      amount: listing.pricing.amount || 0,
      currency: listing.pricing.currency || 'USD',
      status: 'completed',
      createdAt: now,
      completedAt: now,
    };

    const tenantTransactions = this.transactions.get(listing.tenantId) || [];
    tenantTransactions.push(transaction);
    this.transactions.set(listing.tenantId, tenantTransactions);

    const tenant = this.tenants.get(listing.tenantId);
    if (tenant) {
      tenant.stats.totalRevenue += transaction.amount;
      this.tenants.set(listing.tenantId, tenant);
    }

    return transaction;
  }

  private async checkPurchase(listingId: string, userId: string): Promise<boolean> {
    for (const transactions of this.transactions.values()) {
      const found = transactions.find(
        t => t.listingId === listingId && t.buyerId === userId && t.status === 'completed'
      );
      if (found) return true;
    }
    return false;
  }

  // ============================================================================
  // REVIEWS
  // ============================================================================

  async addReview(
    listingId: string,
    userId: string,
    userName: string,
    data: { rating: number; title: string; content: string }
  ): Promise<Review> {
    const listing = this.listings.get(listingId);
    if (!listing) {
      throw new Error(`Listing not found: ${listingId}`);
    }

    const id = `review_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const review: Review = {
      id,
      listingId,
      userId,
      userName,
      rating: Math.max(1, Math.min(5, data.rating)),
      title: data.title,
      content: data.content,
      helpfulCount: 0,
      verified: false,
      createdAt: now,
      updatedAt: now,
    };

    const listingReviews = this.reviews.get(listingId) || [];
    listingReviews.push(review);
    this.reviews.set(listingId, listingReviews);

    listing.stats.totalReviews = listingReviews.length;
    listing.stats.averageRating = listingReviews.reduce((sum, r) => sum + r.rating, 0) / listingReviews.length;
    listing.reviews = listingReviews;
    this.listings.set(listingId, listing);

    this.updateTenantAverageRating(listing.tenantId);

    return review;
  }

  async getReviews(listingId: string): Promise<Review[]> {
    return this.reviews.get(listingId) || [];
  }

  private updateTenantAverageRating(tenantId: string): void {
    const tenant = this.tenants.get(tenantId);
    if (!tenant) return;

    const tenantListings = Array.from(this.listings.values())
      .filter(l => l.tenantId === tenantId && l.stats.totalReviews > 0);

    if (tenantListings.length === 0) {
      tenant.stats.averageRating = 0;
    } else {
      const totalRating = tenantListings.reduce((sum, l) => sum + l.stats.averageRating, 0);
      tenant.stats.averageRating = totalRating / tenantListings.length;
    }

    this.tenants.set(tenantId, tenant);
  }

  // ============================================================================
  // FEATURED LISTINGS
  // ============================================================================

  async getFeaturedListings(limit: number = 10): Promise<MarketplaceListing[]> {
    return Array.from(this.listings.values())
      .filter(l => l.status === 'published' && l.visibility === 'public')
      .sort((a, b) => b.stats.downloads - a.stats.downloads)
      .slice(0, limit);
  }

  async getTrendingListings(limit: number = 10): Promise<MarketplaceListing[]> {
    return Array.from(this.listings.values())
      .filter(l => l.status === 'published' && l.visibility === 'public')
      .sort((a, b) => {
        const scoreA = (a.stats.stars * 2) + a.stats.downloads;
        const scoreB = (b.stats.stars * 2) + b.stats.downloads;
        return scoreB - scoreA;
      })
      .slice(0, limit);
  }

  async getRecentListings(limit: number = 10): Promise<MarketplaceListing[]> {
    return Array.from(this.listings.values())
      .filter(l => l.status === 'published' && l.visibility === 'public')
      .sort((a, b) => new Date(b.publishedAt || b.createdAt).getTime() - new Date(a.publishedAt || a.createdAt).getTime())
      .slice(0, limit);
  }

  // ============================================================================
  // UTILITIES
  // ============================================================================

  private slugify(text: string): string {
    return text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
  }
}

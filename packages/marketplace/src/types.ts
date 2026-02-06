/**
 * Marketplace Types
 * Types for multi-tenant marketplace operations
 */

// ============================================================================
// TENANT TYPES
// ============================================================================

export interface Tenant {
  id: string;
  name: string;
  slug: string;
  description?: string;
  logo?: string;
  verified: boolean;
  plan: TenantPlan;
  settings: TenantSettings;
  stats: TenantStats;
  createdAt: Date;
  updatedAt: Date;
}

export interface TenantSettings {
  allowPublicListings: boolean;
  requireApproval: boolean;
  defaultLicense: LicenseType;
  pricingEnabled: boolean;
  defaultCurrency: string;
}

export interface TenantStats {
  totalListings: number;
  totalDownloads: number;
  totalRevenue: number;
  averageRating: number;
}

export type TenantPlan = 'free' | 'starter' | 'professional' | 'enterprise';

// ============================================================================
// LISTING TYPES
// ============================================================================

export interface MarketplaceListing {
  id: string;
  tenantId: string;
  type: ListingType;
  name: string;
  slug: string;
  description: string;
  longDescription?: string;
  version: string;
  category: ListingCategory;
  tags: string[];
  sourceLanguages: string[];
  targetLanguages: string[];
  pricing: PricingInfo;
  license: LicenseType;
  visibility: ListingVisibility;
  status: ListingStatus;
  content: ListingContent;
  metadata: ListingMetadata;
  stats: ListingStats;
  reviews: Review[];
  createdAt: Date;
  updatedAt: Date;
  publishedAt?: Date;
}

export interface ListingContent {
  rules?: SerializedRule[];
  patterns?: SerializedPattern[];
  transformations?: SerializedTransformation[];
  testCases?: SerializedTestCase[];
  documentation?: string;
  examples?: Example[];
}

export interface SerializedRule {
  id: string;
  name: string;
  category: string;
  description: string;
  logic?: string;
  formula?: string;
  inputs: Array<{ name: string; type: string }>;
  outputs: Array<{ name: string; type: string }>;
}

export interface SerializedPattern {
  id: string;
  name: string;
  sourcePattern: string;
  targetPattern: string;
  description: string;
}

export interface SerializedTransformation {
  id: string;
  name: string;
  sourceLanguage: string;
  targetLanguage: string;
  rules: string[];
}

export interface SerializedTestCase {
  id: string;
  name: string;
  inputs: Record<string, unknown>;
  expectedOutputs: Record<string, unknown>;
}

export interface Example {
  title: string;
  description: string;
  sourceCode: string;
  targetCode: string;
}

export interface ListingMetadata {
  author: string;
  authorEmail?: string;
  repository?: string;
  homepage?: string;
  keywords: string[];
  dependencies: string[];
  minVersion?: string;
  maxVersion?: string;
}

export interface ListingStats {
  downloads: number;
  views: number;
  stars: number;
  forks: number;
  averageRating: number;
  totalReviews: number;
}

export interface PricingInfo {
  type: PricingType;
  amount?: number;
  currency?: string;
  billingPeriod?: 'monthly' | 'yearly' | 'one-time';
  trialDays?: number;
}

export type ListingType = 'rule-pack' | 'pattern-library' | 'transformation-set' | 'full-migration' | 'test-suite';
export type ListingCategory = 'cobol' | 'fortran' | 'vb' | 'java' | 'mainframe' | 'database' | 'general';
export type ListingVisibility = 'public' | 'private' | 'unlisted' | 'tenant-only';
export type ListingStatus = 'draft' | 'pending-review' | 'published' | 'rejected' | 'archived';
export type PricingType = 'free' | 'paid' | 'subscription' | 'contact';
export type LicenseType = 'mit' | 'apache-2.0' | 'gpl-3.0' | 'proprietary' | 'custom';

// ============================================================================
// REVIEW TYPES
// ============================================================================

export interface Review {
  id: string;
  listingId: string;
  userId: string;
  userName: string;
  rating: number;
  title: string;
  content: string;
  helpfulCount: number;
  verified: boolean;
  createdAt: Date;
  updatedAt: Date;
}

// ============================================================================
// TRANSACTION TYPES
// ============================================================================

export interface Transaction {
  id: string;
  tenantId: string;
  buyerId: string;
  listingId: string;
  type: TransactionType;
  amount: number;
  currency: string;
  status: TransactionStatus;
  paymentMethod?: string;
  paymentId?: string;
  createdAt: Date;
  completedAt?: Date;
}

export type TransactionType = 'purchase' | 'subscription' | 'refund';
export type TransactionStatus = 'pending' | 'completed' | 'failed' | 'refunded';

// ============================================================================
// SEARCH TYPES
// ============================================================================

export interface SearchQuery {
  query?: string;
  type?: ListingType;
  category?: ListingCategory;
  sourceLanguage?: string;
  targetLanguage?: string;
  pricingType?: PricingType;
  minRating?: number;
  tags?: string[];
  tenantId?: string;
  sortBy?: 'relevance' | 'downloads' | 'rating' | 'newest' | 'price';
  sortOrder?: 'asc' | 'desc';
  page?: number;
  pageSize?: number;
}

export interface SearchResult {
  listings: MarketplaceListing[];
  total: number;
  page: number;
  pageSize: number;
  facets: SearchFacets;
}

export interface SearchFacets {
  types: Array<{ value: string; count: number }>;
  categories: Array<{ value: string; count: number }>;
  sourceLanguages: Array<{ value: string; count: number }>;
  targetLanguages: Array<{ value: string; count: number }>;
  pricingTypes: Array<{ value: string; count: number }>;
}

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface IMarketplaceService {
  // Tenant management
  createTenant(name: string, slug: string): Promise<Tenant>;
  getTenant(tenantId: string): Promise<Tenant | null>;
  updateTenant(tenantId: string, updates: Partial<Tenant>): Promise<Tenant>;

  // Listing management
  createListing(tenantId: string, listing: Partial<MarketplaceListing>): Promise<MarketplaceListing>;
  getListing(listingId: string): Promise<MarketplaceListing | null>;
  updateListing(listingId: string, updates: Partial<MarketplaceListing>): Promise<MarketplaceListing>;
  publishListing(listingId: string): Promise<MarketplaceListing>;
  archiveListing(listingId: string): Promise<void>;

  // Search
  search(query: SearchQuery): Promise<SearchResult>;

  // Downloads and purchases
  downloadListing(listingId: string, userId: string): Promise<ListingContent>;
  purchaseListing(listingId: string, userId: string): Promise<Transaction>;

  // Reviews
  addReview(listingId: string, userId: string, review: Partial<Review>): Promise<Review>;
  getReviews(listingId: string): Promise<Review[]>;
}

// ============================================================================
// LEGACY TYPES (for backward compatibility with existing code)
// ============================================================================

export interface MigrationTemplate {
  id: string;
  slug: string;
  name: string;
  description: string;
  category: TemplateCategory;
  author: TemplateAuthor;
  version: string;
  visibility: TemplateVisibility;
  pricing: TemplatePricing;
  sourceLanguages: string[];
  targetLanguages: string[];
  tags: string[];
  configuration: TemplateConfiguration;
  files: TemplateFile[];
  downloads: number;
  rating: number;
  reviewCount: number;
  createdAt: Date;
  updatedAt: Date;
  publishedAt?: Date;
}

export type TemplateCategory = 'cobol-to-java' | 'cobol-to-csharp' | 'fortran-to-python' | 'vb-to-csharp' | 'mainframe' | 'database' | 'other';
export type TemplateVisibility = 'public' | 'private' | 'unlisted';

export interface TemplateAuthor {
  id: string;
  name: string;
  avatar?: string;
  verified: boolean;
}

export interface TemplateConfiguration {
  parameters: TemplateParameter[];
  hooks: TemplateHook[];
}

export interface TemplateParameter {
  name: string;
  type: string;
  required: boolean;
  defaultValue?: unknown;
  description?: string;
}

export interface TemplateHook {
  event: string;
  action: string;
}

export interface TemplateFile {
  path: string;
  type: 'template' | 'static' | 'config';
  content?: string;
}

export interface TemplatePricing {
  model: 'free' | 'paid' | 'subscription';
  amount?: number;
  currency?: string;
  period?: string;
}

export interface FacetCount {
  value: string;
  count: number;
}

// ============================================================================
// PROVIDER TYPES
// ============================================================================

export interface MigrationProvider {
  id: string;
  name: string;
  slug: string;
  description: string;
  logo?: string;
  capabilities: ProviderCapability[];
  pricing: ProviderPricing;
  availability: ProviderAvailability;
  verification: ProviderVerification;
  contact: ProviderContact;
  rating: number;
  reviewCount: number;
  completedProjects: number;
  createdAt: Date;
  updatedAt: Date;
}

export interface ProviderCapability {
  sourceLanguages: string[];
  targetLanguages: string[];
  specializations: string[];
}

export interface ProviderPricing {
  model: 'hourly' | 'project' | 'subscription';
  minRate?: number;
  maxRate?: number;
  currency: string;
}

export interface ProviderAvailability {
  status: 'available' | 'busy' | 'unavailable';
  nextAvailable?: Date;
  timezone: string;
}

export interface ProviderVerification {
  verified: boolean;
  verifiedAt?: Date;
  badges: string[];
}

export interface ProviderContact {
  email?: string;
  website?: string;
  calendlyUrl?: string;
}

export interface ProviderSearchQuery {
  text?: string;
  sourceLanguage?: string;
  targetLanguage?: string;
  specialization?: string;
  pricingModel?: string;
  availability?: string;
  minRating?: number;
  verified?: boolean;
  page?: number;
  pageSize?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

// ============================================================================
// PAYMENT TYPES
// ============================================================================

export interface PaymentTransaction {
  id: string;
  type: PaymentTransactionType;
  status: PaymentTransactionStatus;
  amount: number;
  currency: string;
  fees: TransactionFees;
  payerId: string;
  payeeId: string;
  reference: PaymentReference;
  paymentMethod?: PaymentMethod;
  createdAt: Date;
  completedAt?: Date;
  metadata?: Record<string, unknown>;
}

export type PaymentTransactionType = 'purchase' | 'subscription' | 'payout' | 'refund';
export type PaymentTransactionStatus = 'pending' | 'processing' | 'completed' | 'failed' | 'refunded';

export interface TransactionFees {
  platform: number;
  processing: number;
  total: number;
}

export interface PaymentReference {
  type: 'template' | 'provider' | 'subscription';
  id: string;
  name: string;
}

export interface PaymentMethod {
  type: 'card' | 'bank' | 'paypal';
  last4?: string;
  brand?: string;
}

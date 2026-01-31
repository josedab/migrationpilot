/**
 * Marketplace Types
 */

// ============================================================================
// Tenant Types
// ============================================================================

export interface Tenant {
  id: string;
  name: string;
  slug: string;
  plan: TenantPlan;
  status: TenantStatus;
  settings: TenantSettings;
  createdAt: Date;
  updatedAt: Date;
  metadata?: Record<string, unknown>;
}

export type TenantPlan = 'free' | 'starter' | 'professional' | 'enterprise';
export type TenantStatus = 'active' | 'suspended' | 'pending' | 'archived';

export interface TenantSettings {
  maxTemplates: number;
  maxProviders: number;
  maxMigrationsPerMonth: number;
  allowedSourceLanguages: string[];
  allowedTargetLanguages: string[];
  features: TenantFeature[];
}

export type TenantFeature = 
  | 'custom-templates'
  | 'private-marketplace'
  | 'analytics'
  | 'api-access'
  | 'priority-support'
  | 'sla-guarantee'
  | 'white-label';

// ============================================================================
// Template Types
// ============================================================================

export interface MigrationTemplate {
  id: string;
  name: string;
  slug: string;
  version: string;
  description: string;
  longDescription?: string;
  author: TemplateAuthor;
  category: TemplateCategory;
  tags: string[];
  sourceLanguages: string[];
  targetLanguages: string[];
  pricing: TemplatePricing;
  stats: TemplateStats;
  configuration: TemplateConfiguration;
  files: TemplateFile[];
  status: TemplateStatus;
  visibility: TemplateVisibility;
  createdAt: Date;
  updatedAt: Date;
  publishedAt?: Date;
}

export interface TemplateAuthor {
  id: string;
  name: string;
  tenantId: string;
  avatarUrl?: string;
  verified: boolean;
}

export type TemplateCategory =
  | 'language-migration'
  | 'database-migration'
  | 'framework-upgrade'
  | 'cloud-migration'
  | 'refactoring'
  | 'security-hardening'
  | 'performance-optimization'
  | 'custom';

export interface TemplatePricing {
  model: 'free' | 'one-time' | 'per-use' | 'subscription';
  amount?: number;
  currency?: string;
  usageLimit?: number;
}

export interface TemplateStats {
  downloads: number;
  uses: number;
  rating: number;
  reviewCount: number;
  successRate: number;
}

export interface TemplateConfiguration {
  requiredInputs: TemplateInput[];
  optionalInputs: TemplateInput[];
  outputFormats: string[];
  hooks: TemplateHook[];
}

export interface TemplateInput {
  name: string;
  type: 'string' | 'number' | 'boolean' | 'file' | 'directory' | 'select';
  label: string;
  description?: string;
  default?: unknown;
  options?: string[];
  validation?: InputValidation;
}

export interface InputValidation {
  required?: boolean;
  pattern?: string;
  min?: number;
  max?: number;
  minLength?: number;
  maxLength?: number;
}

export interface TemplateHook {
  event: 'pre-migration' | 'post-migration' | 'on-error' | 'on-success';
  type: 'script' | 'webhook' | 'notification';
  config: Record<string, unknown>;
}

export interface TemplateFile {
  path: string;
  type: 'transform' | 'config' | 'script' | 'readme' | 'example';
  content?: string;
  url?: string;
}

export type TemplateStatus = 'draft' | 'pending-review' | 'approved' | 'rejected' | 'deprecated';
export type TemplateVisibility = 'public' | 'private' | 'tenant-only' | 'unlisted';

// ============================================================================
// Provider Types
// ============================================================================

export interface MigrationProvider {
  id: string;
  name: string;
  slug: string;
  description: string;
  tenantId: string;
  capabilities: ProviderCapability[];
  pricing: ProviderPricing;
  availability: ProviderAvailability;
  stats: ProviderStats;
  verification: ProviderVerification;
  contact: ProviderContact;
  status: ProviderStatus;
  createdAt: Date;
  updatedAt: Date;
}

export interface ProviderCapability {
  sourceLanguage: string;
  targetLanguage: string;
  complexity: 'simple' | 'moderate' | 'complex' | 'enterprise';
  certifications?: string[];
}

export interface ProviderPricing {
  model: 'fixed' | 'hourly' | 'per-loc' | 'custom';
  baseRate?: number;
  currency: string;
  minimumCharge?: number;
  estimates: PriceEstimate[];
}

export interface PriceEstimate {
  complexity: string;
  locRange: { min: number; max: number };
  estimatedPrice: { min: number; max: number };
  timeframeDays: { min: number; max: number };
}

export interface ProviderAvailability {
  timezone: string;
  workingHours: { start: string; end: string };
  workingDays: number[];
  currentCapacity: number;
  maxCapacity: number;
  leadTimeDays: number;
}

export interface ProviderStats {
  projectsCompleted: number;
  totalLocMigrated: number;
  averageRating: number;
  reviewCount: number;
  onTimeDelivery: number;
  repeatClientRate: number;
}

export interface ProviderVerification {
  identityVerified: boolean;
  skillsVerified: boolean;
  backgroundChecked: boolean;
  enterpriseApproved: boolean;
  certifications: Certification[];
}

export interface Certification {
  name: string;
  issuer: string;
  issuedAt: Date;
  expiresAt?: Date;
  verificationUrl?: string;
}

export interface ProviderContact {
  email: string;
  phone?: string;
  website?: string;
  calendlyUrl?: string;
}

export type ProviderStatus = 'active' | 'inactive' | 'suspended' | 'pending-verification';

// ============================================================================
// Search Types
// ============================================================================

export interface SearchQuery {
  text?: string;
  categories?: TemplateCategory[];
  sourceLanguages?: string[];
  targetLanguages?: string[];
  pricingModel?: TemplatePricing['model'][];
  minRating?: number;
  maxPrice?: number;
  tags?: string[];
  author?: string;
  sortBy?: SearchSortField;
  sortOrder?: 'asc' | 'desc';
  page?: number;
  pageSize?: number;
}

export type SearchSortField = 'relevance' | 'downloads' | 'rating' | 'date' | 'price';

export interface SearchResult<T> {
  items: T[];
  total: number;
  page: number;
  pageSize: number;
  totalPages: number;
  facets: SearchFacets;
}

export interface SearchFacets {
  categories: FacetCount[];
  sourceLanguages: FacetCount[];
  targetLanguages: FacetCount[];
  pricingModels: FacetCount[];
  ratings: FacetCount[];
}

export interface FacetCount {
  value: string;
  count: number;
}

// ============================================================================
// Payment Types
// ============================================================================

export interface PaymentTransaction {
  id: string;
  tenantId: string;
  customerId: string;
  amount: number;
  currency: string;
  type: TransactionType;
  status: TransactionStatus;
  reference: PaymentReference;
  paymentMethod?: PaymentMethod;
  fees: TransactionFees;
  metadata?: Record<string, unknown>;
  createdAt: Date;
  completedAt?: Date;
}

export type TransactionType = 'purchase' | 'subscription' | 'usage' | 'refund' | 'payout';
export type TransactionStatus = 'pending' | 'processing' | 'completed' | 'failed' | 'refunded';

export interface PaymentReference {
  type: 'template' | 'provider' | 'subscription';
  id: string;
  name: string;
}

export interface PaymentMethod {
  type: 'card' | 'bank' | 'wallet' | 'invoice';
  last4?: string;
  brand?: string;
  expiresAt?: string;
}

export interface TransactionFees {
  platformFee: number;
  processingFee: number;
  authorShare: number;
}

// ============================================================================
// Analytics Types
// ============================================================================

export interface MarketplaceAnalytics {
  tenantId: string;
  period: AnalyticsPeriod;
  templates: TemplateAnalytics;
  revenue: RevenueAnalytics;
  users: UserAnalytics;
}

export interface AnalyticsPeriod {
  start: Date;
  end: Date;
  granularity: 'hour' | 'day' | 'week' | 'month';
}

export interface TemplateAnalytics {
  totalViews: number;
  totalDownloads: number;
  totalUses: number;
  conversionRate: number;
  topTemplates: { templateId: string; metric: number }[];
  trendsOverTime: TimeSeriesData[];
}

export interface RevenueAnalytics {
  totalRevenue: number;
  netRevenue: number;
  platformFees: number;
  refunds: number;
  averageOrderValue: number;
  revenueByType: Record<TransactionType, number>;
  trendsOverTime: TimeSeriesData[];
}

export interface UserAnalytics {
  totalUsers: number;
  activeUsers: number;
  newUsers: number;
  churnRate: number;
  topUsers: { userId: string; metric: number }[];
}

export interface TimeSeriesData {
  timestamp: Date;
  value: number;
  label?: string;
}

// ============================================================================
// Event Types
// ============================================================================

export interface MarketplaceEvent {
  id: string;
  type: MarketplaceEventType;
  tenantId: string;
  actorId?: string;
  resourceType: 'template' | 'provider' | 'tenant' | 'transaction';
  resourceId: string;
  data: Record<string, unknown>;
  timestamp: Date;
}

export type MarketplaceEventType =
  | 'template:created'
  | 'template:published'
  | 'template:downloaded'
  | 'template:used'
  | 'template:rated'
  | 'provider:registered'
  | 'provider:verified'
  | 'provider:hired'
  | 'transaction:created'
  | 'transaction:completed'
  | 'tenant:created'
  | 'tenant:upgraded';

/**
 * Payment Service - Handles marketplace transactions
 */

import type {
  PaymentTransaction,
  TransactionType,
  TransactionStatus,
  PaymentReference,
  PaymentMethod,
  TransactionFees,
} from '../types.js';

// ============================================================================
// Payment Service
// ============================================================================

export class PaymentService {
  private transactions: Map<string, PaymentTransaction> = new Map();
  private transactionsByTenant: Map<string, Set<string>> = new Map();
  private transactionsByCustomer: Map<string, Set<string>> = new Map();
  private feeConfig: FeeConfiguration;

  constructor(feeConfig?: Partial<FeeConfiguration>) {
    this.feeConfig = {
      platformFeePercent: feeConfig?.platformFeePercent ?? 15,
      processingFeePercent: feeConfig?.processingFeePercent ?? 2.9,
      processingFeeFixed: feeConfig?.processingFeeFixed ?? 0.30,
      minimumPlatformFee: feeConfig?.minimumPlatformFee ?? 1,
    };
  }

  /**
   * Create a new transaction
   */
  async createTransaction(input: CreateTransactionInput): Promise<PaymentTransaction> {
    const id = `txn_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const fees = this.calculateFees(input.amount);

    const transaction: PaymentTransaction = {
      id,
      tenantId: input.tenantId,
      customerId: input.customerId,
      amount: input.amount,
      currency: input.currency || 'USD',
      type: input.type,
      status: 'pending',
      reference: input.reference,
      paymentMethod: input.paymentMethod,
      fees,
      metadata: input.metadata,
      createdAt: new Date(),
    };

    this.transactions.set(id, transaction);
    this.indexTransaction(transaction);

    return transaction;
  }

  /**
   * Process a pending transaction
   */
  async processTransaction(id: string): Promise<PaymentTransaction | null> {
    const transaction = this.transactions.get(id);
    if (!transaction) return null;

    if (transaction.status !== 'pending') {
      throw new Error(`Cannot process transaction with status: ${transaction.status}`);
    }

    transaction.status = 'processing';

    // Simulate payment processing
    try {
      await this.simulatePaymentProcessing(transaction);
      transaction.status = 'completed';
      transaction.completedAt = new Date();
    } catch (_error) {
      transaction.status = 'failed';
    }

    return transaction;
  }

  /**
   * Refund a completed transaction
   */
  async refundTransaction(
    id: string,
    reason: string
  ): Promise<PaymentTransaction | null> {
    const originalTransaction = this.transactions.get(id);
    if (!originalTransaction) return null;

    if (originalTransaction.status !== 'completed') {
      throw new Error('Can only refund completed transactions');
    }

    // Create refund transaction
    const refundTransaction = await this.createTransaction({
      tenantId: originalTransaction.tenantId,
      customerId: originalTransaction.customerId,
      amount: -originalTransaction.amount,
      currency: originalTransaction.currency,
      type: 'refund',
      reference: originalTransaction.reference,
      paymentMethod: originalTransaction.paymentMethod,
      metadata: {
        originalTransactionId: originalTransaction.id,
        reason,
      },
    });

    // Process refund immediately
    refundTransaction.status = 'completed';
    refundTransaction.completedAt = new Date();

    // Mark original as refunded
    originalTransaction.status = 'refunded';

    return refundTransaction;
  }

  /**
   * Get transaction by ID
   */
  async getById(id: string): Promise<PaymentTransaction | null> {
    return this.transactions.get(id) || null;
  }

  /**
   * Get transactions for tenant
   */
  async getByTenant(
    tenantId: string,
    options?: TransactionQueryOptions
  ): Promise<PaymentTransaction[]> {
    const ids = this.transactionsByTenant.get(tenantId) || new Set();
    let transactions = Array.from(ids)
      .map(id => this.transactions.get(id))
      .filter((t): t is PaymentTransaction => t !== undefined);

    // Apply filters
    if (options?.type) {
      transactions = transactions.filter(t => t.type === options.type);
    }
    if (options?.status) {
      transactions = transactions.filter(t => t.status === options.status);
    }
    if (options?.startDate) {
      transactions = transactions.filter(t => t.createdAt >= options.startDate!);
    }
    if (options?.endDate) {
      transactions = transactions.filter(t => t.createdAt <= options.endDate!);
    }

    // Sort by date descending
    transactions.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());

    return transactions;
  }

  /**
   * Get transactions for customer
   */
  async getByCustomer(customerId: string): Promise<PaymentTransaction[]> {
    const ids = this.transactionsByCustomer.get(customerId) || new Set();
    return Array.from(ids)
      .map(id => this.transactions.get(id))
      .filter((t): t is PaymentTransaction => t !== undefined)
      .sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
  }

  /**
   * Calculate revenue summary for tenant
   */
  async getRevenueSummary(
    tenantId: string,
    period: { start: Date; end: Date }
  ): Promise<RevenueSummary> {
    const transactions = await this.getByTenant(tenantId, {
      startDate: period.start,
      endDate: period.end,
    });

    const completedTransactions = transactions.filter(t => t.status === 'completed');

    let totalRevenue = 0;
    let platformFees = 0;
    let processingFees = 0;
    let refunds = 0;
    const revenueByType: Record<string, number> = {};

    for (const t of completedTransactions) {
      if (t.type === 'refund') {
        refunds += Math.abs(t.amount);
      } else {
        totalRevenue += t.amount;
        platformFees += t.fees.platformFee;
        processingFees += t.fees.processingFee;
        revenueByType[t.type] = (revenueByType[t.type] || 0) + t.amount;
      }
    }

    return {
      period,
      totalRevenue,
      netRevenue: totalRevenue - platformFees - processingFees - refunds,
      platformFees,
      processingFees,
      refunds,
      transactionCount: completedTransactions.length,
      averageTransactionValue: completedTransactions.length > 0
        ? totalRevenue / completedTransactions.filter(t => t.type !== 'refund').length
        : 0,
      revenueByType,
    };
  }

  /**
   * Create payout to author
   */
  async createPayout(
    tenantId: string,
    authorId: string,
    amount: number
  ): Promise<PaymentTransaction> {
    return this.createTransaction({
      tenantId,
      customerId: authorId,
      amount,
      type: 'payout',
      reference: {
        type: 'template',
        id: authorId,
        name: 'Author Payout',
      },
    });
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private calculateFees(amount: number): TransactionFees {
    const processingFee = Math.max(
      amount * (this.feeConfig.processingFeePercent / 100) + this.feeConfig.processingFeeFixed,
      0
    );
    const platformFee = Math.max(
      amount * (this.feeConfig.platformFeePercent / 100),
      this.feeConfig.minimumPlatformFee
    );
    const authorShare = amount - platformFee - processingFee;

    return {
      platformFee: Math.round(platformFee * 100) / 100,
      processingFee: Math.round(processingFee * 100) / 100,
      authorShare: Math.round(authorShare * 100) / 100,
    };
  }

  private indexTransaction(transaction: PaymentTransaction): void {
    // Index by tenant
    let tenantSet = this.transactionsByTenant.get(transaction.tenantId);
    if (!tenantSet) {
      tenantSet = new Set();
      this.transactionsByTenant.set(transaction.tenantId, tenantSet);
    }
    tenantSet.add(transaction.id);

    // Index by customer
    let customerSet = this.transactionsByCustomer.get(transaction.customerId);
    if (!customerSet) {
      customerSet = new Set();
      this.transactionsByCustomer.set(transaction.customerId, customerSet);
    }
    customerSet.add(transaction.id);
  }

  private async simulatePaymentProcessing(_transaction: PaymentTransaction): Promise<void> {
    // Simulate network delay
    await new Promise(resolve => setTimeout(resolve, 100));

    // Simulate 5% failure rate
    if (Math.random() < 0.05) {
      throw new Error('Payment processing failed');
    }
  }
}

// ============================================================================
// Types
// ============================================================================

export interface FeeConfiguration {
  platformFeePercent: number;
  processingFeePercent: number;
  processingFeeFixed: number;
  minimumPlatformFee: number;
}

export interface CreateTransactionInput {
  tenantId: string;
  customerId: string;
  amount: number;
  currency?: string;
  type: TransactionType;
  reference: PaymentReference;
  paymentMethod?: PaymentMethod;
  metadata?: Record<string, unknown>;
}

export interface TransactionQueryOptions {
  type?: TransactionType;
  status?: TransactionStatus;
  startDate?: Date;
  endDate?: Date;
}

export interface RevenueSummary {
  period: { start: Date; end: Date };
  totalRevenue: number;
  netRevenue: number;
  platformFees: number;
  processingFees: number;
  refunds: number;
  transactionCount: number;
  averageTransactionValue: number;
  revenueByType: Record<string, number>;
}

// ============================================================================
// Factory
// ============================================================================

export function createPaymentService(feeConfig?: Partial<FeeConfiguration>): PaymentService {
  return new PaymentService(feeConfig);
}

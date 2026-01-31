/**
 * Retry Policy - Implements retry logic with backoff
 */

import type { RetryPolicy } from '../types.js';

// ============================================================================
// Retry Handler
// ============================================================================

export class RetryHandler {
  private policy: RetryPolicy;

  constructor(policy: RetryPolicy) {
    this.policy = policy;
  }

  /**
   * Execute with retry
   */
  async execute<T>(
    fn: () => Promise<T>,
    _context?: { stageId?: string; operation?: string }
  ): Promise<RetryResult<T>> {
    let lastError: Error | undefined;
    let attempt = 0;

    while (attempt <= this.policy.maxRetries) {
      try {
        const result = await fn();
        return {
          success: true,
          result,
          attempts: attempt + 1,
        };
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));

        // Check if error is retryable
        if (!this.isRetryable(lastError)) {
          return {
            success: false,
            error: lastError,
            attempts: attempt + 1,
            retryable: false,
          };
        }

        if (attempt < this.policy.maxRetries) {
          const delay = this.calculateDelay(attempt);
          await this.sleep(delay);
        }

        attempt++;
      }
    }

    return {
      success: false,
      error: lastError,
      attempts: attempt,
      retryable: true,
    };
  }

  /**
   * Calculate delay for attempt
   */
  calculateDelay(attempt: number): number {
    const delay = Math.min(
      this.policy.initialDelay * Math.pow(this.policy.backoffMultiplier, attempt),
      this.policy.maxDelay
    );
    
    // Add jitter (±10%)
    const jitter = delay * 0.1 * (Math.random() * 2 - 1);
    return Math.round(delay + jitter);
  }

  /**
   * Check if error is retryable
   */
  isRetryable(error: Error): boolean {
    if (!this.policy.retryableErrors || this.policy.retryableErrors.length === 0) {
      // By default, retry all errors
      return true;
    }

    return this.policy.retryableErrors.some(pattern => {
      const regex = new RegExp(pattern, 'i');
      return regex.test(error.message) || regex.test(error.name);
    });
  }

  /**
   * Get policy
   */
  getPolicy(): RetryPolicy {
    return { ...this.policy };
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// ============================================================================
// Retry Result
// ============================================================================

export interface RetryResult<T> {
  success: boolean;
  result?: T;
  error?: Error;
  attempts: number;
  retryable?: boolean;
}

// ============================================================================
// Default Policies
// ============================================================================

export const DEFAULT_RETRY_POLICY: RetryPolicy = {
  maxRetries: 3,
  initialDelay: 1000,
  maxDelay: 30000,
  backoffMultiplier: 2,
};

export const AGGRESSIVE_RETRY_POLICY: RetryPolicy = {
  maxRetries: 5,
  initialDelay: 500,
  maxDelay: 60000,
  backoffMultiplier: 1.5,
};

export const CONSERVATIVE_RETRY_POLICY: RetryPolicy = {
  maxRetries: 2,
  initialDelay: 2000,
  maxDelay: 10000,
  backoffMultiplier: 2,
};

// ============================================================================
// Factory
// ============================================================================

export function createRetryHandler(policy?: Partial<RetryPolicy>): RetryHandler {
  return new RetryHandler({ ...DEFAULT_RETRY_POLICY, ...policy });
}

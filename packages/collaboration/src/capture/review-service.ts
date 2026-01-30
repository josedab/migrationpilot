/**
 * Review Service
 * Manages SME review workflow for business rules and other artifacts
 */

import type {
  ReviewRequest,
  ReviewType,
  Priority,
  ReviewStatus,
  ReviewContext,
  ReviewDecision,
  IReviewService,
} from '../types';
import { NotificationService } from '../notifications/notification-service';

export interface ReviewMetrics {
  totalReviews: number;
  byStatus: Record<ReviewStatus, number>;
  averageResponseTime: number; // hours
  approvalRate: number;
  rejectionRate: number;
}

export class ReviewService implements IReviewService {
  private reviews: Map<string, ReviewRequest> = new Map();
  private notificationService: NotificationService;

  constructor(notificationService?: NotificationService) {
    this.notificationService = notificationService || new NotificationService();
  }

  /**
   * Create a new review request
   */
  async create(
    request: Omit<ReviewRequest, 'id' | 'status' | 'createdAt' | 'updatedAt'>
  ): Promise<ReviewRequest> {
    const id = `rev_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const now = new Date().toISOString();

    const fullRequest: ReviewRequest = {
      ...request,
      id,
      status: 'pending',
      createdAt: now,
      updatedAt: now,
    };

    this.reviews.set(id, fullRequest);

    // Notify assignees
    for (const assigneeId of fullRequest.assignees) {
      await this.notificationService.sendFromTemplate(
        assigneeId,
        'review-requested',
        {
          reviewId: id,
          projectId: fullRequest.projectId,
          ruleId: fullRequest.context.ruleId,
        },
        { priority: fullRequest.priority }
      );
    }

    return fullRequest;
  }

  /**
   * Assign reviewers to a review request
   */
  async assign(reviewId: string, assignees: string[]): Promise<ReviewRequest> {
    const review = this.reviews.get(reviewId);
    if (!review) {
      throw new Error(`Review not found: ${reviewId}`);
    }

    const newAssignees = assignees.filter(a => !review.assignees.includes(a));
    
    const updated: ReviewRequest = {
      ...review,
      assignees: [...new Set([...review.assignees, ...assignees])],
      updatedAt: new Date().toISOString(),
    };

    this.reviews.set(reviewId, updated);

    // Notify new assignees
    for (const assigneeId of newAssignees) {
      await this.notificationService.sendFromTemplate(
        assigneeId,
        'review-requested',
        {
          reviewId,
          projectId: updated.projectId,
          ruleId: updated.context.ruleId,
        }
      );
    }

    return updated;
  }

  /**
   * Submit a review decision
   */
  async submit(reviewId: string, decision: ReviewDecision): Promise<ReviewRequest> {
    const review = this.reviews.get(reviewId);
    if (!review) {
      throw new Error(`Review not found: ${reviewId}`);
    }

    // Add decision to context
    const previousDecisions = review.context.previousDecisions || [];
    const context: ReviewContext = {
      ...review.context,
      previousDecisions: [...previousDecisions, decision],
    };

    // Determine new status
    let status: ReviewStatus;
    switch (decision.decision) {
      case 'approve':
        status = 'approved';
        break;
      case 'reject':
        status = 'rejected';
        break;
      case 'modify':
        status = 'needs-clarification';
        break;
      default:
        status = review.status;
    }

    const now = new Date().toISOString();
    const updated: ReviewRequest = {
      ...review,
      status,
      context,
      updatedAt: now,
      completedAt: ['approved', 'rejected'].includes(status) ? now : undefined,
    };

    this.reviews.set(reviewId, updated);

    // Notify requester
    await this.notificationService.sendFromTemplate(
      review.requestedBy,
      'review-completed',
      {
        reviewId,
        projectId: review.projectId,
        ruleId: review.context.ruleId,
      }
    );

    return updated;
  }

  /**
   * Get reviews assigned to a user
   */
  async getByAssignee(assigneeId: string): Promise<ReviewRequest[]> {
    return Array.from(this.reviews.values())
      .filter(r => r.assignees.includes(assigneeId))
      .sort((a, b) => {
        // Sort by priority, then by date
        const priorityOrder: Record<Priority, number> = {
          critical: 0,
          high: 1,
          medium: 2,
          low: 3,
        };
        const priorityDiff = priorityOrder[a.priority] - priorityOrder[b.priority];
        if (priorityDiff !== 0) return priorityDiff;
        return new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
      });
  }

  /**
   * Get reviews for a project
   */
  async getByProject(projectId: string): Promise<ReviewRequest[]> {
    return Array.from(this.reviews.values())
      .filter(r => r.projectId === projectId)
      .sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());
  }

  /**
   * Get pending reviews that are past due
   */
  async getOverdue(): Promise<ReviewRequest[]> {
    const now = new Date();
    return Array.from(this.reviews.values())
      .filter(r => {
        if (r.status !== 'pending' && r.status !== 'in-progress') return false;
        if (!r.dueDate) return false;
        return new Date(r.dueDate) < now;
      })
      .sort((a, b) => new Date(a.dueDate!).getTime() - new Date(b.dueDate!).getTime());
  }

  /**
   * Send reminders for approaching deadlines
   */
  async sendDeadlineReminders(hoursBeforeDeadline: number = 24): Promise<void> {
    const now = new Date();
    const reminderThreshold = new Date(now.getTime() + hoursBeforeDeadline * 60 * 60 * 1000);

    const upcomingReviews = Array.from(this.reviews.values())
      .filter(r => {
        if (r.status !== 'pending' && r.status !== 'in-progress') return false;
        if (!r.dueDate) return false;
        const dueDate = new Date(r.dueDate);
        return dueDate > now && dueDate <= reminderThreshold;
      });

    for (const review of upcomingReviews) {
      for (const assigneeId of review.assignees) {
        await this.notificationService.sendFromTemplate(
          assigneeId,
          'deadline-reminder',
          {
            reviewId: review.id,
            projectId: review.projectId,
            deadline: review.dueDate,
          }
        );
      }
    }
  }

  /**
   * Request clarification on a review
   */
  async requestClarification(
    reviewId: string,
    _question: string,
    _requesterId: string
  ): Promise<ReviewRequest> {
    const review = this.reviews.get(reviewId);
    if (!review) {
      throw new Error(`Review not found: ${reviewId}`);
    }

    const updated: ReviewRequest = {
      ...review,
      status: 'needs-clarification',
      updatedAt: new Date().toISOString(),
    };

    this.reviews.set(reviewId, updated);

    // Notify original requester
    await this.notificationService.sendFromTemplate(
      review.requestedBy,
      'clarification-needed',
      {
        reviewId,
        projectId: review.projectId,
        ruleId: review.context.ruleId,
      }
    );

    return updated;
  }

  /**
   * Create a batch of review requests for multiple rules
   */
  async createBatch(
    projectId: string,
    ruleIds: string[],
    type: ReviewType,
    assignees: string[],
    requestedBy: string,
    options?: {
      priority?: Priority;
      dueDate?: string;
    }
  ): Promise<ReviewRequest[]> {
    return Promise.all(
      ruleIds.map(ruleId =>
        this.create({
          projectId,
          type,
          priority: options?.priority || 'medium',
          title: `Review rule: ${ruleId}`,
          description: `Please review the extracted business rule ${ruleId}`,
          context: { ruleId },
          assignees,
          requestedBy,
          dueDate: options?.dueDate,
        })
      )
    );
  }

  /**
   * Get review metrics for a project
   */
  getMetrics(projectId: string): ReviewMetrics {
    const projectReviews = Array.from(this.reviews.values())
      .filter(r => r.projectId === projectId);

    const byStatus: Record<ReviewStatus, number> = {
      'pending': 0,
      'in-progress': 0,
      'approved': 0,
      'rejected': 0,
      'needs-clarification': 0,
    };

    let totalResponseTime = 0;
    let completedCount = 0;

    for (const review of projectReviews) {
      byStatus[review.status]++;

      if (review.completedAt) {
        const responseTime = new Date(review.completedAt).getTime() - 
                            new Date(review.createdAt).getTime();
        totalResponseTime += responseTime;
        completedCount++;
      }
    }

    const total = projectReviews.length;

    return {
      totalReviews: total,
      byStatus,
      averageResponseTime: completedCount > 0 
        ? totalResponseTime / completedCount / (1000 * 60 * 60) // Convert to hours
        : 0,
      approvalRate: total > 0 ? byStatus['approved'] / total : 0,
      rejectionRate: total > 0 ? byStatus['rejected'] / total : 0,
    };
  }
}

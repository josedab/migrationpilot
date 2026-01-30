/**
 * Microsoft Teams Integration
 * Integration for Teams notifications and adaptive cards
 */

import type {
  IntegrationConfig,
  TeamsMessage,
  TeamsAdaptiveCard,
  ReviewRequest,
  Notification,
  Priority,
} from '../types';

export interface TeamsConfig extends IntegrationConfig {
  type: 'teams';
  tenantId?: string;
  clientId?: string;
  clientSecret?: string;
}

export class TeamsIntegration {
  private config: TeamsConfig;

  constructor(config: TeamsConfig) {
    this.config = config;
  }

  /**
   * Send a message to Teams via webhook
   */
  async sendMessage(webhookUrl: string, message: TeamsMessage): Promise<void> {
    if (!this.config.enabled) {
      console.log('[Teams] Integration disabled, skipping message');
      return;
    }

    // In a real implementation, this would make an HTTP POST to the webhook
    console.log('[Teams] Would send message:', webhookUrl, message);
  }

  /**
   * Format a notification as a Teams Adaptive Card
   */
  formatNotification(notification: Notification): TeamsMessage {
    const priorityColors: Record<Priority, string> = {
      critical: 'attention',
      high: 'warning',
      medium: 'accent',
      low: 'good',
    };

    const card: TeamsAdaptiveCard = {
      contentType: 'application/vnd.microsoft.card.adaptive',
      content: {
        type: 'AdaptiveCard',
        version: '1.4',
        body: [
          {
            type: 'TextBlock',
            text: notification.title,
            weight: 'bolder',
            size: 'large',
            color: priorityColors[notification.priority],
          },
          {
            type: 'TextBlock',
            text: notification.message,
            wrap: true,
          },
          {
            type: 'FactSet',
            facts: [
              { title: 'Priority', value: notification.priority },
              { title: 'Type', value: notification.type },
              { title: 'Time', value: new Date(notification.createdAt).toLocaleString() },
            ],
          },
        ],
        actions: notification.actionUrl
          ? [
              {
                type: 'Action.OpenUrl',
                title: 'View Details',
                url: notification.actionUrl,
              },
            ]
          : undefined,
      },
    };

    return {
      type: 'message',
      attachments: [card],
    };
  }

  /**
   * Format a review request as a Teams Adaptive Card
   */
  formatReviewRequest(review: ReviewRequest): TeamsMessage {
    const priorityColors: Record<Priority, string> = {
      critical: 'attention',
      high: 'warning',
      medium: 'accent',
      low: 'good',
    };

    const body: unknown[] = [
      {
        type: 'TextBlock',
        text: `📋 Review Request: ${review.title}`,
        weight: 'bolder',
        size: 'large',
      },
      {
        type: 'TextBlock',
        text: review.description,
        wrap: true,
      },
      {
        type: 'ColumnSet',
        columns: [
          {
            type: 'Column',
            width: 'auto',
            items: [
              {
                type: 'TextBlock',
                text: 'Priority',
                weight: 'bolder',
              },
              {
                type: 'TextBlock',
                text: review.priority,
                color: priorityColors[review.priority],
              },
            ],
          },
          {
            type: 'Column',
            width: 'auto',
            items: [
              {
                type: 'TextBlock',
                text: 'Type',
                weight: 'bolder',
              },
              {
                type: 'TextBlock',
                text: review.type,
              },
            ],
          },
          {
            type: 'Column',
            width: 'auto',
            items: [
              {
                type: 'TextBlock',
                text: 'Status',
                weight: 'bolder',
              },
              {
                type: 'TextBlock',
                text: review.status,
              },
            ],
          },
        ],
      },
    ];

    if (review.context.codeSnippet) {
      body.push({
        type: 'TextBlock',
        text: 'Code Snippet',
        weight: 'bolder',
        spacing: 'medium',
      });
      body.push({
        type: 'TextBlock',
        text: review.context.codeSnippet,
        fontType: 'monospace',
        wrap: true,
      });
    }

    if (review.dueDate) {
      body.push({
        type: 'TextBlock',
        text: `⏰ Due: ${new Date(review.dueDate).toLocaleDateString()}`,
        spacing: 'medium',
      });
    }

    const card: TeamsAdaptiveCard = {
      contentType: 'application/vnd.microsoft.card.adaptive',
      content: {
        type: 'AdaptiveCard',
        version: '1.4',
        body,
        actions: [
          {
            type: 'Action.Submit',
            title: '✅ Approve',
            data: {
              action: 'approve',
              reviewId: review.id,
            },
          },
          {
            type: 'Action.Submit',
            title: '❌ Reject',
            data: {
              action: 'reject',
              reviewId: review.id,
            },
          },
          {
            type: 'Action.Submit',
            title: '❓ Need More Info',
            data: {
              action: 'clarify',
              reviewId: review.id,
            },
          },
        ],
      },
    };

    return {
      type: 'message',
      attachments: [card],
    };
  }

  /**
   * Create a pending reviews summary card
   */
  formatReviewsSummary(reviews: ReviewRequest[]): TeamsMessage {
    const byPriority: Record<Priority, number> = {
      critical: 0,
      high: 0,
      medium: 0,
      low: 0,
    };

    for (const review of reviews) {
      byPriority[review.priority]++;
    }

    const body: unknown[] = [
      {
        type: 'TextBlock',
        text: `📊 Pending Reviews Summary`,
        weight: 'bolder',
        size: 'large',
      },
      {
        type: 'TextBlock',
        text: `Total: ${reviews.length} reviews pending`,
        spacing: 'medium',
      },
      {
        type: 'FactSet',
        facts: [
          { title: '🔴 Critical', value: byPriority.critical.toString() },
          { title: '🟠 High', value: byPriority.high.toString() },
          { title: '🟡 Medium', value: byPriority.medium.toString() },
          { title: '🟢 Low', value: byPriority.low.toString() },
        ],
      },
    ];

    // Add top 5 reviews
    if (reviews.length > 0) {
      body.push({
        type: 'TextBlock',
        text: 'Top Priority Items',
        weight: 'bolder',
        spacing: 'medium',
      });

      const topReviews = reviews
        .sort((a, b) => {
          const order: Record<Priority, number> = { critical: 0, high: 1, medium: 2, low: 3 };
          return order[a.priority] - order[b.priority];
        })
        .slice(0, 5);

      for (const review of topReviews) {
        const emoji = { critical: '🔴', high: '🟠', medium: '🟡', low: '🟢' }[review.priority];
        body.push({
          type: 'TextBlock',
          text: `${emoji} ${review.title}`,
          wrap: true,
        });
      }
    }

    const card: TeamsAdaptiveCard = {
      contentType: 'application/vnd.microsoft.card.adaptive',
      content: {
        type: 'AdaptiveCard',
        version: '1.4',
        body,
      },
    };

    return {
      type: 'message',
      attachments: [card],
    };
  }
}

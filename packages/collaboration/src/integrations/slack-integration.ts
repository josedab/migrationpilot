/**
 * Slack Integration
 * Integration for Slack notifications and interactive workflows
 */

import type {
  IntegrationConfig,
  IntegrationSettings,
  SlackMessage,
  SlackBlock,
  SlackAttachment,
  ReviewRequest,
  Notification,
  Priority,
} from '../types';

export interface SlackConfig extends IntegrationConfig {
  type: 'slack';
  botToken?: string;
  signingSecret?: string;
  defaultChannel?: string;
}

export class SlackIntegration {
  private config: SlackConfig;

  constructor(config: SlackConfig) {
    this.config = config;
  }

  /**
   * Send a message to Slack
   */
  async sendMessage(message: SlackMessage): Promise<void> {
    if (!this.config.enabled) {
      console.log('[Slack] Integration disabled, skipping message');
      return;
    }

    if (!this.config.webhookUrl && !this.config.botToken) {
      throw new Error('Slack integration requires either webhookUrl or botToken');
    }

    // Use webhook URL if available
    if (this.config.webhookUrl) {
      await this.sendViaWebhook(message);
    } else {
      // Would use Slack API with botToken
      console.log('[Slack] Would send message via API:', message);
    }
  }

  /**
   * Send message via webhook
   */
  private async sendViaWebhook(message: SlackMessage): Promise<void> {
    // In a real implementation, this would make an HTTP request
    console.log('[Slack] Would send via webhook:', this.config.webhookUrl, message);
  }

  /**
   * Format a notification for Slack
   */
  formatNotification(notification: Notification): SlackMessage {
    const priorityColors: Record<Priority, string> = {
      critical: '#ff0000',
      high: '#ff9900',
      medium: '#ffcc00',
      low: '#00cc00',
    };

    const blocks: SlackBlock[] = [
      {
        type: 'header',
        text: { type: 'plain_text', text: notification.title },
      },
      {
        type: 'section',
        text: { type: 'mrkdwn', text: notification.message },
      },
    ];

    if (notification.actionUrl) {
      blocks.push({
        type: 'actions',
        elements: [
          {
            type: 'button',
            text: { type: 'plain_text', text: 'View Details' },
            url: notification.actionUrl,
          } as unknown,
        ],
      });
    }

    const attachments: SlackAttachment[] = [
      {
        color: priorityColors[notification.priority],
        fields: [
          { title: 'Priority', value: notification.priority, short: true },
          { title: 'Type', value: notification.type, short: true },
        ],
        footer: 'MigrationPilot',
        ts: Math.floor(new Date(notification.createdAt).getTime() / 1000),
      },
    ];

    return {
      channel: this.config.channelId || this.config.defaultChannel || '#general',
      text: notification.title,
      blocks,
      attachments,
    };
  }

  /**
   * Format a review request for Slack
   */
  formatReviewRequest(review: ReviewRequest): SlackMessage {
    const priorityEmojis: Record<Priority, string> = {
      critical: '🔴',
      high: '🟠',
      medium: '🟡',
      low: '🟢',
    };

    const blocks: SlackBlock[] = [
      {
        type: 'header',
        text: { type: 'plain_text', text: `${priorityEmojis[review.priority]} New Review Request` },
      },
      {
        type: 'section',
        text: { type: 'mrkdwn', text: `*${review.title}*\n${review.description}` },
      },
      { type: 'divider' },
      {
        type: 'section',
        text: {
          type: 'mrkdwn',
          text: [
            `*Type:* ${review.type}`,
            `*Priority:* ${review.priority}`,
            review.dueDate ? `*Due:* ${new Date(review.dueDate).toLocaleDateString()}` : '',
          ].filter(Boolean).join('\n'),
        },
      },
    ];

    if (review.context.codeSnippet) {
      blocks.push({
        type: 'section',
        text: {
          type: 'mrkdwn',
          text: `\`\`\`${review.context.codeSnippet}\`\`\``,
        },
      });
    }

    blocks.push({
      type: 'actions',
      elements: [
        {
          type: 'button',
          text: { type: 'plain_text', text: '✅ Approve' },
          style: 'primary',
          action_id: `review_approve_${review.id}`,
        } as unknown,
        {
          type: 'button',
          text: { type: 'plain_text', text: '❌ Reject' },
          style: 'danger',
          action_id: `review_reject_${review.id}`,
        } as unknown,
        {
          type: 'button',
          text: { type: 'plain_text', text: '❓ Need Info' },
          action_id: `review_clarify_${review.id}`,
        } as unknown,
      ],
    });

    return {
      channel: this.config.channelId || this.config.defaultChannel || '#reviews',
      text: `New review request: ${review.title}`,
      blocks,
    };
  }

  /**
   * Send a daily digest of pending reviews
   */
  async sendPendingReviewsDigest(
    reviews: ReviewRequest[],
    channel?: string
  ): Promise<void> {
    if (reviews.length === 0) return;

    const priorityOrder: Record<Priority, number> = {
      critical: 0,
      high: 1,
      medium: 2,
      low: 3,
    };

    const sortedReviews = reviews.sort(
      (a, b) => priorityOrder[a.priority] - priorityOrder[b.priority]
    );

    const blocks: SlackBlock[] = [
      {
        type: 'header',
        text: { type: 'plain_text', text: `📋 Pending Reviews (${reviews.length})` },
      },
      { type: 'divider' },
    ];

    for (const review of sortedReviews.slice(0, 10)) {
      const priorityEmoji = { critical: '🔴', high: '🟠', medium: '🟡', low: '🟢' }[review.priority];
      blocks.push({
        type: 'section',
        text: {
          type: 'mrkdwn',
          text: `${priorityEmoji} *${review.title}*\nType: ${review.type} | Due: ${review.dueDate ? new Date(review.dueDate).toLocaleDateString() : 'No deadline'}`,
        },
      });
    }

    if (reviews.length > 10) {
      blocks.push({
        type: 'context',
        elements: [
          { type: 'mrkdwn', text: `_...and ${reviews.length - 10} more_` } as unknown,
        ],
      });
    }

    await this.sendMessage({
      channel: channel || this.config.defaultChannel || '#reviews',
      text: `Pending Reviews Digest: ${reviews.length} items`,
      blocks,
    });
  }

  /**
   * Check if notifications should be sent based on settings
   */
  shouldNotify(
    type: 'review' | 'mention' | 'deadline' | 'completion',
    settings: IntegrationSettings
  ): boolean {
    switch (type) {
      case 'review':
        return settings.notifyOnReviewRequested;
      case 'completion':
        return settings.notifyOnReviewCompleted;
      case 'mention':
        return settings.notifyOnMention;
      case 'deadline':
        return settings.notifyOnDeadline;
      default:
        return true;
    }
  }

  /**
   * Check if current time is within quiet hours
   */
  isQuietHours(settings: IntegrationSettings): boolean {
    if (!settings.quietHours) return false;

    const now = new Date();
    const currentTime = now.getHours() * 60 + now.getMinutes();

    const startParts = settings.quietHours.start.split(':').map(Number);
    const endParts = settings.quietHours.end.split(':').map(Number);
    
    const startHour = startParts[0] ?? 0;
    const startMin = startParts[1] ?? 0;
    const endHour = endParts[0] ?? 0;
    const endMin = endParts[1] ?? 0;

    const startTime = startHour * 60 + startMin;
    const endTime = endHour * 60 + endMin;

    if (startTime < endTime) {
      return currentTime >= startTime && currentTime < endTime;
    } else {
      // Spans midnight
      return currentTime >= startTime || currentTime < endTime;
    }
  }
}

/**
 * Notification Service
 * Manages sending notifications across multiple channels
 */

import type {
  Notification,
  NotificationType,
  Priority,
  CommunicationChannel,
  NotificationData,
  INotificationService,
  SMEProfile,
} from '../types';

export interface NotificationTemplate {
  type: NotificationType;
  title: (data: NotificationData) => string;
  message: (data: NotificationData) => string;
  defaultPriority: Priority;
}

const NOTIFICATION_TEMPLATES: Record<NotificationType, NotificationTemplate> = {
  'review-requested': {
    type: 'review-requested',
    title: () => 'New Review Request',
    message: (data) => `You have been assigned a new review for project ${data.projectId || 'Unknown'}`,
    defaultPriority: 'high',
  },
  'review-completed': {
    type: 'review-completed',
    title: () => 'Review Completed',
    message: (data) => `Review ${data.reviewId || ''} has been completed`,
    defaultPriority: 'medium',
  },
  'rule-updated': {
    type: 'rule-updated',
    title: () => 'Business Rule Updated',
    message: (data) => `Rule ${data.ruleId || ''} has been updated`,
    defaultPriority: 'low',
  },
  'clarification-needed': {
    type: 'clarification-needed',
    title: () => 'Clarification Needed',
    message: (data) => `Your input is needed for rule ${data.ruleId || ''}`,
    defaultPriority: 'high',
  },
  'deadline-reminder': {
    type: 'deadline-reminder',
    title: () => 'Deadline Approaching',
    message: (data) => `Review deadline: ${data.deadline || 'soon'}`,
    defaultPriority: 'high',
  },
  'mention': {
    type: 'mention',
    title: () => 'You were mentioned',
    message: (data) => `${data.mentionedBy || 'Someone'} mentioned you`,
    defaultPriority: 'medium',
  },
  'knowledge-added': {
    type: 'knowledge-added',
    title: () => 'New Knowledge Entry',
    message: () => 'A new knowledge entry has been added',
    defaultPriority: 'low',
  },
  'conflict-detected': {
    type: 'conflict-detected',
    title: () => 'Conflict Detected',
    message: (data) => `A conflict was detected in rule ${data.ruleId || ''}`,
    defaultPriority: 'critical',
  },
};

export class NotificationService implements INotificationService {
  private notifications: Map<string, Notification> = new Map();
  private smeProfiles: Map<string, SMEProfile> = new Map();

  /**
   * Register an SME profile for notification routing
   */
  registerSME(profile: SMEProfile): void {
    this.smeProfiles.set(profile.id, profile);
  }

  /**
   * Send a notification
   */
  async send(
    notification: Omit<Notification, 'id' | 'createdAt'>
  ): Promise<Notification> {
    const id = `notif_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    
    const fullNotification: Notification = {
      ...notification,
      id,
      createdAt: new Date().toISOString(),
    };

    this.notifications.set(id, fullNotification);

    // Route to appropriate channel
    await this.routeNotification(fullNotification);

    return fullNotification;
  }

  /**
   * Send multiple notifications
   */
  async sendBulk(
    notifications: Omit<Notification, 'id' | 'createdAt'>[]
  ): Promise<Notification[]> {
    return Promise.all(notifications.map(n => this.send(n)));
  }

  /**
   * Create and send a notification from a template
   */
  async sendFromTemplate(
    recipientId: string,
    type: NotificationType,
    data: NotificationData,
    options?: {
      channel?: CommunicationChannel;
      priority?: Priority;
      actionUrl?: string;
    }
  ): Promise<Notification> {
    const template = NOTIFICATION_TEMPLATES[type];
    const sme = this.smeProfiles.get(recipientId);

    return this.send({
      recipientId,
      type,
      priority: options?.priority || template.defaultPriority,
      channel: options?.channel || sme?.preferredChannel || 'in-app',
      title: template.title(data),
      message: template.message(data),
      data,
      actionUrl: options?.actionUrl,
      read: false,
    });
  }

  /**
   * Mark notification as read
   */
  async markAsRead(notificationId: string): Promise<void> {
    const notification = this.notifications.get(notificationId);
    if (notification) {
      notification.read = true;
      notification.readAt = new Date().toISOString();
      this.notifications.set(notificationId, notification);
    }
  }

  /**
   * Get unread notifications for a user
   */
  async getUnread(userId: string): Promise<Notification[]> {
    return Array.from(this.notifications.values())
      .filter(n => n.recipientId === userId && !n.read)
      .sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());
  }

  /**
   * Get all notifications for a user
   */
  async getAll(
    userId: string,
    options?: { limit?: number; offset?: number }
  ): Promise<Notification[]> {
    const userNotifications = Array.from(this.notifications.values())
      .filter(n => n.recipientId === userId)
      .sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());

    const offset = options?.offset || 0;
    const limit = options?.limit || 50;

    return userNotifications.slice(offset, offset + limit);
  }

  /**
   * Notify all SMEs with specific expertise
   */
  async notifyByExpertise(
    domain: string,
    type: NotificationType,
    data: NotificationData,
    options?: { excludeIds?: string[] }
  ): Promise<Notification[]> {
    const relevantSMEs = Array.from(this.smeProfiles.values())
      .filter(sme => {
        if (options?.excludeIds?.includes(sme.id)) return false;
        return sme.expertise.some(e => 
          e.domain.toLowerCase() === domain.toLowerCase() ||
          e.subdomains?.some(s => s.toLowerCase() === domain.toLowerCase())
        );
      });

    return Promise.all(
      relevantSMEs.map(sme =>
        this.sendFromTemplate(sme.id, type, data)
      )
    );
  }

  /**
   * Route notification to appropriate channel
   */
  private async routeNotification(notification: Notification): Promise<void> {
    notification.sentAt = new Date().toISOString();
    
    switch (notification.channel) {
      case 'slack':
        await this.sendToSlack(notification);
        break;
      case 'teams':
        await this.sendToTeams(notification);
        break;
      case 'email':
        await this.sendEmail(notification);
        break;
      case 'in-app':
      default:
        // Already stored, no additional routing needed
        break;
    }
  }

  private async sendToSlack(_notification: Notification): Promise<void> {
    // Placeholder - actual implementation would use Slack integration
    console.log(`[Slack] Would send notification: ${_notification.title}`);
  }

  private async sendToTeams(_notification: Notification): Promise<void> {
    // Placeholder - actual implementation would use Teams integration
    console.log(`[Teams] Would send notification: ${_notification.title}`);
  }

  private async sendEmail(_notification: Notification): Promise<void> {
    // Placeholder - actual implementation would use email service
    console.log(`[Email] Would send notification: ${_notification.title}`);
  }
}

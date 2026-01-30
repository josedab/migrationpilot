/**
 * Collaboration Types
 * Types for SME collaboration, knowledge capture, and team communication
 */

// ============================================================================
// User & SME Types
// ============================================================================

export interface SMEProfile {
  id: string;
  name: string;
  email: string;
  role: 'sme' | 'developer' | 'analyst' | 'architect' | 'manager';
  expertise: ExpertiseArea[];
  availability: AvailabilityStatus;
  preferredChannel: CommunicationChannel;
  timezone: string;
  responseTime?: number; // Average response time in minutes
  metadata?: Record<string, unknown>;
}

export interface ExpertiseArea {
  domain: string;
  subdomains?: string[];
  proficiencyLevel: 'beginner' | 'intermediate' | 'expert';
  yearsOfExperience?: number;
  certifications?: string[];
}

export type AvailabilityStatus = 'available' | 'busy' | 'away' | 'offline' | 'do-not-disturb';
export type CommunicationChannel = 'slack' | 'teams' | 'email' | 'in-app';

// ============================================================================
// Review & Feedback Types
// ============================================================================

export interface ReviewRequest {
  id: string;
  projectId: string;
  type: ReviewType;
  priority: Priority;
  status: ReviewStatus;
  title: string;
  description: string;
  context: ReviewContext;
  assignees: string[];
  requestedBy: string;
  dueDate?: string;
  createdAt: string;
  updatedAt: string;
  completedAt?: string;
}

export type ReviewType = 
  | 'business-rule'
  | 'data-mapping'
  | 'edge-case'
  | 'domain-concept'
  | 'validation'
  | 'general';

export type Priority = 'low' | 'medium' | 'high' | 'critical';
export type ReviewStatus = 'pending' | 'in-progress' | 'approved' | 'rejected' | 'needs-clarification';

export interface ReviewContext {
  ruleId?: string;
  sourceFile?: string;
  sourceLines?: [number, number];
  codeSnippet?: string;
  relatedRules?: string[];
  aiConfidence?: number;
  previousDecisions?: ReviewDecision[];
}

export interface ReviewDecision {
  decision: 'approve' | 'reject' | 'modify';
  comment: string;
  reviewerId: string;
  timestamp: string;
  modifications?: RuleModification[];
}

export interface RuleModification {
  field: string;
  originalValue: unknown;
  newValue: unknown;
  reason: string;
}

// ============================================================================
// Knowledge Capture Types
// ============================================================================

export interface KnowledgeEntry {
  id: string;
  projectId: string;
  type: KnowledgeType;
  title: string;
  content: string;
  source: KnowledgeSource;
  tags: string[];
  relatedEntities: RelatedEntity[];
  confidence: number;
  status: 'draft' | 'verified' | 'deprecated';
  createdBy: string;
  verifiedBy?: string;
  createdAt: string;
  updatedAt: string;
  verifiedAt?: string;
}

export type KnowledgeType = 
  | 'business-rule-explanation'
  | 'domain-concept'
  | 'historical-context'
  | 'exception-case'
  | 'workaround'
  | 'best-practice'
  | 'terminology'
  | 'data-definition'
  | 'process-flow';

export interface KnowledgeSource {
  type: 'sme-interview' | 'document' | 'code-comment' | 'meeting-notes' | 'chat' | 'manual-entry';
  reference?: string;
  capturedAt: string;
  capturedBy: string;
}

export interface RelatedEntity {
  type: 'rule' | 'file' | 'procedure' | 'data-structure' | 'knowledge-entry';
  id: string;
  relationship: 'explains' | 'modifies' | 'depends-on' | 'related-to';
}

// ============================================================================
// Notification Types
// ============================================================================

export interface Notification {
  id: string;
  recipientId: string;
  type: NotificationType;
  priority: Priority;
  channel: CommunicationChannel;
  title: string;
  message: string;
  data?: NotificationData;
  actionUrl?: string;
  read: boolean;
  sentAt?: string;
  readAt?: string;
  createdAt: string;
}

export type NotificationType = 
  | 'review-requested'
  | 'review-completed'
  | 'rule-updated'
  | 'clarification-needed'
  | 'deadline-reminder'
  | 'mention'
  | 'knowledge-added'
  | 'conflict-detected';

export interface NotificationData {
  reviewId?: string;
  ruleId?: string;
  projectId?: string;
  mentionedBy?: string;
  deadline?: string;
  changes?: Record<string, unknown>;
}

// ============================================================================
// Discussion Types
// ============================================================================

export interface Discussion {
  id: string;
  projectId: string;
  entityType: 'rule' | 'file' | 'procedure' | 'general';
  entityId?: string;
  title: string;
  status: 'open' | 'resolved' | 'archived';
  participants: string[];
  messages: DiscussionMessage[];
  resolution?: DiscussionResolution;
  createdBy: string;
  createdAt: string;
  updatedAt: string;
  resolvedAt?: string;
}

export interface DiscussionMessage {
  id: string;
  authorId: string;
  content: string;
  type: 'comment' | 'question' | 'answer' | 'decision' | 'action-item';
  attachments?: Attachment[];
  reactions?: Reaction[];
  mentions?: string[];
  createdAt: string;
  editedAt?: string;
}

export interface Attachment {
  id: string;
  name: string;
  type: string;
  url: string;
  size: number;
}

export interface Reaction {
  emoji: string;
  users: string[];
}

export interface DiscussionResolution {
  summary: string;
  decision: string;
  actionItems: ActionItem[];
  resolvedBy: string;
}

export interface ActionItem {
  id: string;
  description: string;
  assignee: string;
  dueDate?: string;
  status: 'pending' | 'in-progress' | 'completed';
}

// ============================================================================
// Integration Types
// ============================================================================

export interface IntegrationConfig {
  type: 'slack' | 'teams' | 'email';
  enabled: boolean;
  webhookUrl?: string;
  apiToken?: string;
  channelId?: string;
  settings: IntegrationSettings;
}

export interface IntegrationSettings {
  notifyOnReviewRequested: boolean;
  notifyOnReviewCompleted: boolean;
  notifyOnMention: boolean;
  notifyOnDeadline: boolean;
  digestFrequency: 'immediate' | 'hourly' | 'daily' | 'weekly';
  quietHours?: { start: string; end: string };
}

export interface SlackMessage {
  channel: string;
  text: string;
  blocks?: SlackBlock[];
  attachments?: SlackAttachment[];
  threadTs?: string;
}

export interface SlackBlock {
  type: 'section' | 'divider' | 'context' | 'actions' | 'header';
  text?: { type: 'mrkdwn' | 'plain_text'; text: string };
  elements?: unknown[];
  accessory?: unknown;
}

export interface SlackAttachment {
  color?: string;
  fallback?: string;
  title?: string;
  text?: string;
  fields?: { title: string; value: string; short?: boolean }[];
  footer?: string;
  ts?: number;
}

export interface TeamsMessage {
  type: 'message';
  attachments: TeamsAdaptiveCard[];
}

export interface TeamsAdaptiveCard {
  contentType: 'application/vnd.microsoft.card.adaptive';
  content: {
    type: 'AdaptiveCard';
    version: string;
    body: unknown[];
    actions?: unknown[];
  };
}

// ============================================================================
// Session Types
// ============================================================================

export interface CollaborationSession {
  id: string;
  projectId: string;
  type: 'review' | 'knowledge-capture' | 'discussion' | 'training';
  title: string;
  description?: string;
  participants: SessionParticipant[];
  status: 'scheduled' | 'active' | 'completed' | 'cancelled';
  scheduledStart: string;
  scheduledEnd: string;
  actualStart?: string;
  actualEnd?: string;
  artifacts: SessionArtifact[];
  notes?: string;
  createdBy: string;
  createdAt: string;
}

export interface SessionParticipant {
  userId: string;
  role: 'host' | 'presenter' | 'participant' | 'observer';
  status: 'invited' | 'accepted' | 'declined' | 'attended';
}

export interface SessionArtifact {
  id: string;
  type: 'recording' | 'transcript' | 'notes' | 'action-items' | 'knowledge-entries';
  name: string;
  url?: string;
  content?: string;
  createdAt: string;
}

// ============================================================================
// Interface Types
// ============================================================================

export interface INotificationService {
  send(notification: Omit<Notification, 'id' | 'createdAt'>): Promise<Notification>;
  sendBulk(notifications: Omit<Notification, 'id' | 'createdAt'>[]): Promise<Notification[]>;
  markAsRead(notificationId: string): Promise<void>;
  getUnread(userId: string): Promise<Notification[]>;
  getAll(userId: string, options?: { limit?: number; offset?: number }): Promise<Notification[]>;
}

export interface IKnowledgeCaptureService {
  create(entry: Omit<KnowledgeEntry, 'id' | 'createdAt' | 'updatedAt'>): Promise<KnowledgeEntry>;
  update(id: string, updates: Partial<KnowledgeEntry>): Promise<KnowledgeEntry>;
  verify(id: string, verifierId: string): Promise<KnowledgeEntry>;
  search(query: KnowledgeSearchQuery): Promise<KnowledgeEntry[]>;
  getRelated(entityId: string, entityType: string): Promise<KnowledgeEntry[]>;
}

export interface KnowledgeSearchQuery {
  projectId: string;
  query?: string;
  types?: KnowledgeType[];
  tags?: string[];
  status?: string;
  limit?: number;
  offset?: number;
}

export interface IReviewService {
  create(request: Omit<ReviewRequest, 'id' | 'status' | 'createdAt' | 'updatedAt'>): Promise<ReviewRequest>;
  assign(reviewId: string, assignees: string[]): Promise<ReviewRequest>;
  submit(reviewId: string, decision: ReviewDecision): Promise<ReviewRequest>;
  getByAssignee(assigneeId: string): Promise<ReviewRequest[]>;
  getByProject(projectId: string): Promise<ReviewRequest[]>;
}

export interface IIntegrationService {
  configure(config: IntegrationConfig): Promise<void>;
  sendSlackMessage(message: SlackMessage): Promise<void>;
  sendTeamsMessage(webhookUrl: string, message: TeamsMessage): Promise<void>;
  sendEmail(to: string[], subject: string, body: string): Promise<void>;
}

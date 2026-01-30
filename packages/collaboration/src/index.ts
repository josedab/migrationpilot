/**
 * @migrationpilot/collaboration
 * 
 * Real-time SME collaboration, knowledge capture, and team communication integrations.
 */

// Types
export * from './types';

// Notifications
export { NotificationService } from './notifications';

// Knowledge Capture
export { KnowledgeCaptureService, ReviewService } from './capture';

// Integrations
export { SlackIntegration, TeamsIntegration, type SlackConfig, type TeamsConfig } from './integrations';

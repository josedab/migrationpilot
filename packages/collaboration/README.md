# @migrationpilot/collaboration

Real-time SME collaboration, knowledge capture, and team communication integrations for MigrationPilot.

## Overview

This package provides collaboration features that enable Subject Matter Experts (SMEs) to review, annotate, and validate business rules extracted from legacy code. It supports real-time collaboration, knowledge capture, and integrations with team communication tools.

## Features

- **Rule Review Workflow**: Structured review process for business rules
- **Real-time Annotations**: Comments, corrections, and context additions
- **Knowledge Capture**: Document tribal knowledge and edge cases
- **Team Integrations**: Slack, Teams, email notifications
- **Version History**: Track all changes and discussions

## Usage

### Rule Review

```typescript
import { RuleReviewManager } from '@migrationpilot/collaboration';

const reviewManager = new RuleReviewManager();

// Submit rule for review
const review = await reviewManager.submitForReview({
  ruleId: 'BR-001',
  reviewers: ['sme@example.com'],
  priority: 'high',
  dueDate: new Date('2024-02-15'),
});

// Add review comment
await reviewManager.addComment({
  reviewId: review.id,
  userId: 'reviewer-123',
  content: 'This calculation should also handle leap years',
  type: 'correction',
  lineReference: { start: 45, end: 48 },
});

// Approve or reject
await reviewManager.submitDecision({
  reviewId: review.id,
  decision: 'approved',
  modifications: {
    edgeCases: ['Leap year handling for February'],
  },
});
```

### Knowledge Capture

```typescript
import { KnowledgeCapture } from '@migrationpilot/collaboration';

const knowledge = new KnowledgeCapture();

// Document tribal knowledge
await knowledge.capture({
  projectId: 'proj-123',
  topic: 'Overtime Calculation',
  content: `
    The overtime calculation has a special case for the California office.
    Hours over 8 in a single day count as overtime, not just over 40/week.
    This was implemented in 1998 due to CA labor law requirements.
  `,
  source: 'John Smith (retired 2020)',
  relatedRules: ['BR-015', 'BR-016'],
  tags: ['california', 'labor-law', 'overtime'],
});

// Search knowledge base
const results = await knowledge.search('california overtime');
```

### Notifications

```typescript
import { NotificationService } from '@migrationpilot/collaboration';

const notifications = new NotificationService({
  slack: {
    webhookUrl: process.env.SLACK_WEBHOOK_URL,
    channel: '#migration-reviews',
  },
  email: {
    from: 'migration@example.com',
    smtpConfig: { /* ... */ },
  },
});

// Send review request
await notifications.send({
  type: 'review-request',
  recipients: ['sme@example.com'],
  data: {
    ruleName: 'Calculate Overtime Pay',
    confidence: 0.78,
    projectName: 'Payroll Migration',
    reviewUrl: 'https://app.migrationpilot.com/reviews/123',
  },
});
```

### Slack Integration

```typescript
import { SlackIntegration } from '@migrationpilot/collaboration/integrations';

const slack = new SlackIntegration({
  botToken: process.env.SLACK_BOT_TOKEN,
  signingSecret: process.env.SLACK_SIGNING_SECRET,
});

// Register slash commands
slack.registerCommand('/migration-status', async (command) => {
  const status = await getProjectStatus(command.projectId);
  return formatStatusMessage(status);
});

// Send interactive review
await slack.sendReviewCard({
  channel: 'C123456',
  rule: businessRule,
  actions: ['approve', 'reject', 'request-changes'],
});
```

### Microsoft Teams Integration

```typescript
import { TeamsIntegration } from '@migrationpilot/collaboration/integrations';

const teams = new TeamsIntegration({
  tenantId: process.env.TEAMS_TENANT_ID,
  clientId: process.env.TEAMS_CLIENT_ID,
  clientSecret: process.env.TEAMS_CLIENT_SECRET,
});

// Send adaptive card
await teams.sendAdaptiveCard({
  channelId: 'channel-123',
  card: createReviewCard(businessRule),
});
```

## Review Workflow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Pending   │────▶│  In Review  │────▶│   Decision  │
└─────────────┘     └─────────────┘     └─────────────┘
                           │                    │
                           │                    ▼
                           │            ┌─────────────┐
                           │            │  Approved   │
                           │            └─────────────┘
                           │                    │
                           │                    ▼
                           │            ┌─────────────┐
                           └───────────▶│  Rejected   │
                                        └─────────────┘
                                               │
                                               ▼
                                        ┌─────────────┐
                                        │   Revised   │
                                        └─────────────┘
```

## Configuration

```typescript
interface CollaborationConfig {
  // Review settings
  review: {
    defaultReviewers: string[];
    autoAssignThreshold: number;  // Confidence below this triggers review
    reminderInterval: number;     // Hours between reminders
  };
  
  // Notification channels
  notifications: {
    slack?: SlackConfig;
    teams?: TeamsConfig;
    email?: EmailConfig;
  };
  
  // Knowledge capture
  knowledge: {
    enableAutoCapture: boolean;
    tagSuggestions: boolean;
  };
}
```

## Installation

```bash
pnpm add @migrationpilot/collaboration
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Watch mode
pnpm dev
```

## License

MIT

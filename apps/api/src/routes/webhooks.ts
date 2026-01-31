/**
 * Webhook Routes
 * 
 * Manage webhook subscriptions for external integrations
 */

import { Hono } from 'hono';

interface Webhook {
  id: string;
  url: string;
  events: string[];
  secret: string;
  active: boolean;
  createdAt: string;
  lastTriggeredAt?: string;
  failureCount: number;
}

// In-memory store (replace with database in production)
const webhooks = new Map<string, Webhook>();

export const webhookRoutes = new Hono();

// Supported webhook events
const SUPPORTED_EVENTS = [
  'project.created',
  'project.updated',
  'project.deleted',
  'migration.started',
  'migration.completed',
  'migration.failed',
  'migration.progress',
  'rule.extracted',
  'rule.approved',
  'rule.rejected',
  'rule.needs_review',
  'test.completed',
  'test.failed',
  'validation.completed',
];

// List all webhooks
webhookRoutes.get('/', async (c) => {
  const allWebhooks = Array.from(webhooks.values()).map(w => ({
    ...w,
    secret: '****' + w.secret.slice(-4), // Mask secret
  }));
  
  return c.json({
    success: true,
    data: allWebhooks,
    supportedEvents: SUPPORTED_EVENTS,
  });
});

// Create webhook
webhookRoutes.post('/', async (c) => {
  const body = await c.req.json<{ url: string; events: string[]; secret?: string }>();
  
  // Validate URL
  try {
    new URL(body.url);
  } catch {
    return c.json({ success: false, error: 'Invalid URL' }, 400);
  }
  
  // Validate events
  const invalidEvents = body.events.filter(e => !SUPPORTED_EVENTS.includes(e));
  if (invalidEvents.length > 0) {
    return c.json({ 
      success: false, 
      error: `Invalid events: ${invalidEvents.join(', ')}`,
      supportedEvents: SUPPORTED_EVENTS,
    }, 400);
  }
  
  const id = `wh_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
  const secret = body.secret || generateSecret();
  
  const webhook: Webhook = {
    id,
    url: body.url,
    events: body.events,
    secret,
    active: true,
    createdAt: new Date().toISOString(),
    failureCount: 0,
  };
  
  webhooks.set(id, webhook);
  
  return c.json({
    success: true,
    data: {
      ...webhook,
      secret, // Return full secret only on creation
    },
  }, 201);
});

// Get webhook by ID
webhookRoutes.get('/:id', async (c) => {
  const id = c.req.param('id');
  const webhook = webhooks.get(id);
  
  if (!webhook) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  return c.json({
    success: true,
    data: {
      ...webhook,
      secret: '****' + webhook.secret.slice(-4),
    },
  });
});

// Update webhook
webhookRoutes.patch('/:id', async (c) => {
  const id = c.req.param('id');
  const webhook = webhooks.get(id);
  
  if (!webhook) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  const body = await c.req.json<{ url?: string; events?: string[]; active?: boolean }>();
  
  if (body.url) {
    try {
      new URL(body.url);
      webhook.url = body.url;
    } catch {
      return c.json({ success: false, error: 'Invalid URL' }, 400);
    }
  }
  
  if (body.events) {
    const invalidEvents = body.events.filter(e => !SUPPORTED_EVENTS.includes(e));
    if (invalidEvents.length > 0) {
      return c.json({ success: false, error: `Invalid events: ${invalidEvents.join(', ')}` }, 400);
    }
    webhook.events = body.events;
  }
  
  if (typeof body.active === 'boolean') {
    webhook.active = body.active;
  }
  
  webhooks.set(id, webhook);
  
  return c.json({
    success: true,
    data: {
      ...webhook,
      secret: '****' + webhook.secret.slice(-4),
    },
  });
});

// Delete webhook
webhookRoutes.delete('/:id', async (c) => {
  const id = c.req.param('id');
  
  if (!webhooks.has(id)) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  webhooks.delete(id);
  
  return c.json({ success: true });
});

// Test webhook
webhookRoutes.post('/:id/test', async (c) => {
  const id = c.req.param('id');
  const webhook = webhooks.get(id);
  
  if (!webhook) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  const testPayload = {
    event: 'webhook.test',
    timestamp: new Date().toISOString(),
    data: {
      message: 'This is a test webhook delivery',
      webhookId: id,
    },
  };
  
  try {
    const signature = generateSignature(JSON.stringify(testPayload), webhook.secret);
    
    const response = await fetch(webhook.url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'X-MigrationPilot-Signature': signature,
        'X-MigrationPilot-Event': 'webhook.test',
        'X-MigrationPilot-Delivery': `test_${Date.now()}`,
      },
      body: JSON.stringify(testPayload),
      signal: AbortSignal.timeout(10000), // 10 second timeout
    });
    
    return c.json({
      success: response.ok,
      statusCode: response.status,
      message: response.ok ? 'Webhook delivered successfully' : `HTTP ${response.status}`,
    });
  } catch (error) {
    return c.json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

// Rotate webhook secret
webhookRoutes.post('/:id/rotate-secret', async (c) => {
  const id = c.req.param('id');
  const webhook = webhooks.get(id);
  
  if (!webhook) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  const newSecret = generateSecret();
  webhook.secret = newSecret;
  webhooks.set(id, webhook);
  
  return c.json({
    success: true,
    data: {
      id: webhook.id,
      secret: newSecret, // Return new secret
    },
  });
});

// Get delivery logs
webhookRoutes.get('/:id/deliveries', async (c) => {
  const id = c.req.param('id');
  
  if (!webhooks.has(id)) {
    return c.json({ success: false, error: 'Webhook not found' }, 404);
  }
  
  // In production, fetch from database
  return c.json({
    success: true,
    data: [],
    message: 'Delivery logs stored in database',
  });
});

// Helper functions
function generateSecret(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let secret = 'whsec_';
  for (let i = 0; i < 32; i++) {
    secret += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return secret;
}

function generateSignature(payload: string, secret: string): string {
  // In production, use crypto.createHmac('sha256', secret).update(payload).digest('hex')
  return `sha256=${Buffer.from(payload + secret).toString('base64').slice(0, 64)}`;
}

// Export function to trigger webhooks (called from other services)
export async function triggerWebhooks(event: string, data: unknown): Promise<void> {
  const matchingWebhooks = Array.from(webhooks.values())
    .filter(w => w.active && w.events.includes(event));
  
  const payload = {
    event,
    timestamp: new Date().toISOString(),
    data,
  };
  
  for (const webhook of matchingWebhooks) {
    try {
      const signature = generateSignature(JSON.stringify(payload), webhook.secret);
      
      await fetch(webhook.url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-MigrationPilot-Signature': signature,
          'X-MigrationPilot-Event': event,
          'X-MigrationPilot-Delivery': `del_${Date.now()}`,
        },
        body: JSON.stringify(payload),
        signal: AbortSignal.timeout(30000),
      });
      
      webhook.lastTriggeredAt = new Date().toISOString();
      webhook.failureCount = 0;
    } catch (error) {
      webhook.failureCount++;
      console.error(`Webhook ${webhook.id} delivery failed:`, error);
      
      // Disable webhook after 5 consecutive failures
      if (webhook.failureCount >= 5) {
        webhook.active = false;
        console.warn(`Webhook ${webhook.id} disabled after 5 failures`);
      }
    }
    
    webhooks.set(webhook.id, webhook);
  }
}

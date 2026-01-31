/**
 * Strangler Fig Routes
 * 
 * API endpoints for managing incremental migration routing
 */

import { Hono } from 'hono';
import { stranglerFigService, type RoutingRule } from '../services/strangler-fig.js';

export const stranglerFigRoutes = new Hono();

// List routing rules for a project
stranglerFigRoutes.get('/projects/:projectId/rules', async (c) => {
  const projectId = c.req.param('projectId');
  const rules = stranglerFigService.getRules(projectId);
  
  return c.json({
    success: true,
    data: rules,
  });
});

// Create a new routing rule
stranglerFigRoutes.post('/projects/:projectId/rules', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<Omit<RoutingRule, 'id' | 'projectId'>>();
  
  // Validate
  if (!body.name || !body.pathPattern) {
    return c.json({ success: false, error: 'Name and pathPattern are required' }, 400);
  }
  
  if (body.trafficPercentage < 0 || body.trafficPercentage > 100) {
    return c.json({ success: false, error: 'trafficPercentage must be 0-100' }, 400);
  }
  
  const rule = stranglerFigService.createRule({
    ...body,
    projectId,
    targetService: body.targetService || 'legacy',
    trafficPercentage: body.trafficPercentage || 0,
    shadowMode: body.shadowMode || false,
    enabled: body.enabled ?? true,
  });
  
  return c.json({
    success: true,
    data: rule,
  }, 201);
});

// Update a routing rule
stranglerFigRoutes.patch('/rules/:id', async (c) => {
  const id = c.req.param('id');
  const body = await c.req.json<Partial<RoutingRule>>();
  
  const rule = stranglerFigService.updateRule(id, body);
  
  if (!rule) {
    return c.json({ success: false, error: 'Rule not found' }, 404);
  }
  
  return c.json({
    success: true,
    data: rule,
  });
});

// Delete a routing rule
stranglerFigRoutes.delete('/rules/:id', async (c) => {
  const id = c.req.param('id');
  const deleted = stranglerFigService.deleteRule(id);
  
  if (!deleted) {
    return c.json({ success: false, error: 'Rule not found' }, 404);
  }
  
  return c.json({ success: true });
});

// Test routing for a path
stranglerFigRoutes.post('/projects/:projectId/route', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<{ path: string; headers?: Record<string, string> }>();
  
  const decision = stranglerFigService.routeRequest(projectId, body.path, body.headers);
  
  return c.json({
    success: true,
    data: decision,
  });
});

// Execute shadow mode test
stranglerFigRoutes.post('/projects/:projectId/shadow', async (c) => {
  // projectId is in route but shadow test uses explicit endpoints
  c.req.param('projectId'); // Validates projectId presence
  const body = await c.req.json<{
    legacyEndpoint: string;
    modernEndpoint: string;
    request: {
      method: string;
      path: string;
      headers: Record<string, string>;
      body?: unknown;
    };
  }>();
  
  try {
    const result = await stranglerFigService.executeShadow(
      body.legacyEndpoint,
      body.modernEndpoint,
      body.request
    );
    
    return c.json({
      success: true,
      data: result,
    });
  } catch (error) {
    return c.json({
      success: false,
      error: error instanceof Error ? error.message : 'Shadow test failed',
    }, 500);
  }
});

// Generate API facade code
stranglerFigRoutes.get('/projects/:projectId/facade', async (c) => {
  const projectId = c.req.param('projectId');
  const language = c.req.query('language') as 'java' | 'python' | 'typescript' || 'typescript';
  
  const code = stranglerFigService.generateFacade(projectId, language);
  
  return c.json({
    success: true,
    data: {
      language,
      code,
    },
  });
});

// Bulk update traffic percentages (for gradual rollout)
stranglerFigRoutes.post('/projects/:projectId/rollout', async (c) => {
  const projectId = c.req.param('projectId');
  const body = await c.req.json<{
    targetPercentage: number;
    increment?: number;
    ruleIds?: string[];
  }>();
  
  const rules = stranglerFigService.getRules(projectId);
  const targetRules = body.ruleIds 
    ? rules.filter(r => body.ruleIds!.includes(r.id))
    : rules;
  
  const updated: RoutingRule[] = [];
  
  for (const rule of targetRules) {
    if (body.increment) {
      // Incremental rollout
      const newPercentage = Math.min(100, rule.trafficPercentage + body.increment);
      const updatedRule = stranglerFigService.updateRule(rule.id, {
        trafficPercentage: newPercentage,
      });
      if (updatedRule) updated.push(updatedRule);
    } else {
      // Set to target
      const updatedRule = stranglerFigService.updateRule(rule.id, {
        trafficPercentage: body.targetPercentage,
      });
      if (updatedRule) updated.push(updatedRule);
    }
  }
  
  return c.json({
    success: true,
    data: {
      updated: updated.length,
      rules: updated,
    },
  });
});

// Rollback to legacy (set all rules to 0%)
stranglerFigRoutes.post('/projects/:projectId/rollback', async (c) => {
  const projectId = c.req.param('projectId');
  
  const rules = stranglerFigService.getRules(projectId);
  const updated: RoutingRule[] = [];
  
  for (const rule of rules) {
    const updatedRule = stranglerFigService.updateRule(rule.id, {
      trafficPercentage: 0,
      enabled: false,
    });
    if (updatedRule) updated.push(updatedRule);
  }
  
  return c.json({
    success: true,
    message: 'Rolled back to legacy system',
    data: {
      rulesDisabled: updated.length,
    },
  });
});

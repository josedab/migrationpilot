/**
 * MigrationPilot API Server
 */

import { serve } from '@hono/node-server';
import { Hono } from 'hono';
import { cors } from 'hono/cors';
import { logger } from 'hono/logger';
import { prettyJSON } from 'hono/pretty-json';

import { projectRoutes } from './routes/projects.js';
import { analysisRoutes } from './routes/analysis.js';
import { migrationRoutes } from './routes/migration.js';
import { healthRoutes } from './routes/health.js';
import { webhookRoutes } from './routes/webhooks.js';
import { stranglerFigRoutes } from './routes/strangler-fig.js';
import { exportRoutes } from './routes/export.js';
import { metricsRoutes } from './routes/metrics.js';
import { explainerRoutes } from './routes/explainer.js';
import { testGeneratorRoutes } from './routes/test-generator.js';
import { dashboardRoutes } from './routes/dashboard.js';
import { validationRoutes } from './routes/validation.js';
import { costEstimatorRoutes } from './routes/cost-estimator.js';
import { visualizationRoutes } from './routes/visualization.js';
import { nlQueryRoutes } from './routes/nl-query.js';
import { codeReviewerRoutes } from './routes/code-reviewer.js';
import { regressionManagerRoutes } from './routes/regression-manager.js';
import runtimeEmulatorRoutes from './routes/runtime-emulator.js';
import observabilityBridgeRoutes from './routes/observability-bridge.js';
import { graphqlRouter } from './graphql/index.js';
import { rateLimiter, securityHeaders, auditLogger } from './middleware/index.js';

const app = new Hono();

// Security middleware
app.use('*', securityHeaders());
app.use('*', cors());
app.use('*', logger());
app.use('*', prettyJSON());
app.use('/api/*', rateLimiter({ windowMs: 60000, maxRequests: 100 }));
app.use('/api/*', auditLogger());

// Routes
app.route('/api/health', healthRoutes);
app.route('/api/projects', projectRoutes);
app.route('/api/analysis', analysisRoutes);
app.route('/api/migration', migrationRoutes);
app.route('/api/webhooks', webhookRoutes);
app.route('/api/routing', stranglerFigRoutes);
app.route('/api/export', exportRoutes);
app.route('/api/explainer', explainerRoutes);
app.route('/api/tests', testGeneratorRoutes);
app.route('/api/dashboard', dashboardRoutes);
app.route('/api/validation', validationRoutes);
app.route('/api/cost-estimator', costEstimatorRoutes);
app.route('/api/visualization', visualizationRoutes);
app.route('/api/nl-query', nlQueryRoutes);
app.route('/api/code-reviewer', codeReviewerRoutes);
app.route('/api/regression', regressionManagerRoutes);
app.route('/api/emulator', runtimeEmulatorRoutes);
app.route('/api/observability', observabilityBridgeRoutes);

// Metrics endpoint (Prometheus format)
app.route('/metrics', metricsRoutes);

// GraphQL endpoint
app.route('/graphql', graphqlRouter);

// Root
app.get('/', (c) => {
  return c.json({
    name: 'MigrationPilot API',
    version: '0.1.0',
    docs: '/api/docs',
    graphql: '/graphql',
    endpoints: {
      health: '/api/health',
      projects: '/api/projects',
      analysis: '/api/analysis',
      migration: '/api/migration',
      webhooks: '/api/webhooks',
      routing: '/api/routing',
      export: '/api/export',
      explainer: '/api/explainer',
      tests: '/api/tests',
      dashboard: '/api/dashboard',
      validation: '/api/validation',
      'cost-estimator': '/api/cost-estimator',
      visualization: '/api/visualization',
      'nl-query': '/api/nl-query',
      'code-reviewer': '/api/code-reviewer',
      regression: '/api/regression',
      emulator: '/api/emulator',
      observability: '/api/observability',
      metrics: '/metrics',
    },
  });
});

// 404 handler
app.notFound((c) => {
  return c.json({ error: 'Not Found' }, 404);
});

// Error handler
app.onError((err, c) => {
  console.error('Error:', err);
  return c.json(
    {
      error: 'Internal Server Error',
      message: err.message,
    },
    500
  );
});

// Start server
const port = parseInt(process.env.API_PORT || '4000', 10);

console.log(`🚀 MigrationPilot API starting on port ${port}`);

serve({
  fetch: app.fetch,
  port,
});

export default app;

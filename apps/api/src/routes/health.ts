/**
 * Health check routes
 */

import { Hono } from 'hono';

export const healthRoutes = new Hono();

healthRoutes.get('/', (c) => {
  return c.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
  });
});

healthRoutes.get('/ready', (c) => {
  // Check dependencies (database, etc.)
  const checks = {
    api: true,
    // database: await checkDatabase(),
    // redis: await checkRedis(),
  };

  const allHealthy = Object.values(checks).every(Boolean);

  return c.json(
    {
      ready: allHealthy,
      checks,
    },
    allHealthy ? 200 : 503
  );
});

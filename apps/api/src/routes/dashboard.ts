/**
 * Dashboard Data Service
 * 
 * Provides aggregated data for migration progress visualization
 */

import { Hono } from 'hono';
// import { z } from 'zod';
// import { generateId } from '@migrationpilot/core';

export const dashboardRoutes = new Hono();

// Types for dashboard data
interface DependencyNode {
  id: string;
  name: string;
  type: 'program' | 'copybook' | 'file' | 'database' | 'service' | 'module';
  status: 'pending' | 'analyzing' | 'migrating' | 'testing' | 'completed' | 'failed';
  riskLevel: 'low' | 'medium' | 'high' | 'critical';
  metrics: {
    linesOfCode: number;
    businessRules: number;
    complexity: number;
    coverage: number;
  };
  progress: number; // 0-100
  children?: string[];
}

interface DependencyEdge {
  source: string;
  target: string;
  type: 'calls' | 'includes' | 'reads' | 'writes' | 'depends';
  weight: number;
}

interface MigrationDashboardData {
  projectId: string;
  projectName: string;
  status: string;
  overallProgress: number;
  nodes: DependencyNode[];
  edges: DependencyEdge[];
  metrics: {
    totalModules: number;
    completedModules: number;
    inProgressModules: number;
    failedModules: number;
    totalLinesOfCode: number;
    migratedLinesOfCode: number;
    totalBusinessRules: number;
    validatedBusinessRules: number;
    averageComplexity: number;
    estimatedCompletion: string;
  };
  riskHotspots: Array<{
    nodeId: string;
    nodeName: string;
    riskScore: number;
    reasons: string[];
  }>;
  recentActivity: Array<{
    timestamp: string;
    type: 'analysis' | 'migration' | 'validation' | 'review';
    description: string;
    nodeId?: string;
  }>;
}

// In-memory storage for demo (replace with database in production)
const dashboardCache = new Map<string, MigrationDashboardData>();

/**
 * Generate mock dashboard data for demo purposes
 */
function generateMockDashboardData(projectId: string): MigrationDashboardData {
  const nodes: DependencyNode[] = [
    {
      id: 'main-batch',
      name: 'MAINBATCH.cbl',
      type: 'program',
      status: 'completed',
      riskLevel: 'low',
      metrics: { linesOfCode: 2500, businessRules: 15, complexity: 45, coverage: 92 },
      progress: 100,
      children: ['calc-interest', 'validate-account', 'update-balance'],
    },
    {
      id: 'calc-interest',
      name: 'CALCINT.cbl',
      type: 'program',
      status: 'migrating',
      riskLevel: 'high',
      metrics: { linesOfCode: 1200, businessRules: 8, complexity: 72, coverage: 45 },
      progress: 65,
      children: ['interest-copybook'],
    },
    {
      id: 'validate-account',
      name: 'VALIDACC.cbl',
      type: 'program',
      status: 'testing',
      riskLevel: 'medium',
      metrics: { linesOfCode: 800, businessRules: 12, complexity: 38, coverage: 78 },
      progress: 85,
    },
    {
      id: 'update-balance',
      name: 'UPDTBAL.cbl',
      type: 'program',
      status: 'analyzing',
      riskLevel: 'critical',
      metrics: { linesOfCode: 1500, businessRules: 20, complexity: 85, coverage: 0 },
      progress: 25,
      children: ['account-file', 'audit-log'],
    },
    {
      id: 'interest-copybook',
      name: 'INTCPY.cpy',
      type: 'copybook',
      status: 'completed',
      riskLevel: 'low',
      metrics: { linesOfCode: 150, businessRules: 0, complexity: 5, coverage: 100 },
      progress: 100,
    },
    {
      id: 'account-file',
      name: 'ACCTFILE',
      type: 'file',
      status: 'pending',
      riskLevel: 'medium',
      metrics: { linesOfCode: 0, businessRules: 0, complexity: 0, coverage: 0 },
      progress: 0,
    },
    {
      id: 'audit-log',
      name: 'AUDITLOG',
      type: 'database',
      status: 'pending',
      riskLevel: 'low',
      metrics: { linesOfCode: 0, businessRules: 0, complexity: 0, coverage: 0 },
      progress: 0,
    },
  ];

  const edges: DependencyEdge[] = [
    { source: 'main-batch', target: 'calc-interest', type: 'calls', weight: 10 },
    { source: 'main-batch', target: 'validate-account', type: 'calls', weight: 8 },
    { source: 'main-batch', target: 'update-balance', type: 'calls', weight: 12 },
    { source: 'calc-interest', target: 'interest-copybook', type: 'includes', weight: 5 },
    { source: 'update-balance', target: 'account-file', type: 'writes', weight: 15 },
    { source: 'update-balance', target: 'audit-log', type: 'writes', weight: 3 },
  ];

  return {
    projectId,
    projectName: 'Banking Core Migration',
    status: 'in_progress',
    overallProgress: 55,
    nodes,
    edges,
    metrics: {
      totalModules: 7,
      completedModules: 2,
      inProgressModules: 3,
      failedModules: 0,
      totalLinesOfCode: 6150,
      migratedLinesOfCode: 2650,
      totalBusinessRules: 55,
      validatedBusinessRules: 23,
      averageComplexity: 49,
      estimatedCompletion: new Date(Date.now() + 14 * 24 * 60 * 60 * 1000).toISOString(),
    },
    riskHotspots: [
      {
        nodeId: 'update-balance',
        nodeName: 'UPDTBAL.cbl',
        riskScore: 0.85,
        reasons: ['High complexity (85)', 'Critical business logic', '20 business rules'],
      },
      {
        nodeId: 'calc-interest',
        nodeName: 'CALCINT.cbl',
        riskScore: 0.72,
        reasons: ['Complex calculations', 'Low test coverage (45%)'],
      },
    ],
    recentActivity: [
      { timestamp: new Date().toISOString(), type: 'validation', description: 'VALIDACC.cbl passed equivalence tests', nodeId: 'validate-account' },
      { timestamp: new Date(Date.now() - 3600000).toISOString(), type: 'migration', description: 'CALCINT.cbl code generation started', nodeId: 'calc-interest' },
      { timestamp: new Date(Date.now() - 7200000).toISOString(), type: 'analysis', description: 'UPDTBAL.cbl business rules extracted', nodeId: 'update-balance' },
      { timestamp: new Date(Date.now() - 10800000).toISOString(), type: 'review', description: 'SME approved 5 business rules', nodeId: 'calc-interest' },
    ],
  };
}

/**
 * GET /dashboard/:projectId
 * Get dashboard data for a project
 */
dashboardRoutes.get('/:projectId', async (c) => {
  const projectId = c.req.param('projectId');
  
  // Check cache first
  let data = dashboardCache.get(projectId);
  
  if (!data) {
    // Generate mock data for demo
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  return c.json({ data });
});

/**
 * GET /dashboard/:projectId/graph
 * Get dependency graph data only
 */
dashboardRoutes.get('/:projectId/graph', async (c) => {
  const projectId = c.req.param('projectId');
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  return c.json({
    data: {
      nodes: data.nodes,
      edges: data.edges,
    },
  });
});

/**
 * GET /dashboard/:projectId/metrics
 * Get metrics summary only
 */
dashboardRoutes.get('/:projectId/metrics', async (c) => {
  const projectId = c.req.param('projectId');
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  return c.json({ data: data.metrics });
});

/**
 * GET /dashboard/:projectId/risks
 * Get risk hotspots
 */
dashboardRoutes.get('/:projectId/risks', async (c) => {
  const projectId = c.req.param('projectId');
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  return c.json({ data: data.riskHotspots });
});

/**
 * GET /dashboard/:projectId/activity
 * Get recent activity
 */
dashboardRoutes.get('/:projectId/activity', async (c) => {
  const projectId = c.req.param('projectId');
  const limit = parseInt(c.req.query('limit') || '20', 10);
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  return c.json({ data: data.recentActivity.slice(0, limit) });
});

/**
 * GET /dashboard/:projectId/node/:nodeId
 * Get detailed info for a specific node
 */
dashboardRoutes.get('/:projectId/node/:nodeId', async (c) => {
  const projectId = c.req.param('projectId');
  const nodeId = c.req.param('nodeId');
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  const node = data.nodes.find(n => n.id === nodeId);
  if (!node) {
    return c.json({ error: 'Node not found' }, 404);
  }
  
  // Get connected nodes
  const incomingEdges = data.edges.filter(e => e.target === nodeId);
  const outgoingEdges = data.edges.filter(e => e.source === nodeId);
  
  return c.json({
    data: {
      node,
      dependencies: {
        incoming: incomingEdges.map(e => ({
          nodeId: e.source,
          nodeName: data!.nodes.find(n => n.id === e.source)?.name,
          type: e.type,
        })),
        outgoing: outgoingEdges.map(e => ({
          nodeId: e.target,
          nodeName: data!.nodes.find(n => n.id === e.target)?.name,
          type: e.type,
        })),
      },
    },
  });
});

/**
 * POST /dashboard/:projectId/refresh
 * Refresh dashboard data
 */
dashboardRoutes.post('/:projectId/refresh', async (c) => {
  const projectId = c.req.param('projectId');
  
  // Regenerate data
  const data = generateMockDashboardData(projectId);
  dashboardCache.set(projectId, data);
  
  return c.json({ data: { message: 'Dashboard refreshed', timestamp: new Date().toISOString() } });
});

/**
 * POST /dashboard/:projectId/export-report
 * Export dashboard as PDF report data
 */
dashboardRoutes.post('/:projectId/export-report', async (c) => {
  const projectId = c.req.param('projectId');
  
  let data = dashboardCache.get(projectId);
  if (!data) {
    data = generateMockDashboardData(projectId);
    dashboardCache.set(projectId, data);
  }
  
  // Generate report data structure for PDF generation
  const report = {
    title: `Migration Progress Report - ${data.projectName}`,
    generatedAt: new Date().toISOString(),
    summary: {
      status: data.status,
      overallProgress: `${data.overallProgress}%`,
      estimatedCompletion: data.metrics.estimatedCompletion,
    },
    metrics: {
      'Modules Progress': `${data.metrics.completedModules}/${data.metrics.totalModules} completed`,
      'Lines of Code': `${data.metrics.migratedLinesOfCode.toLocaleString()}/${data.metrics.totalLinesOfCode.toLocaleString()} migrated`,
      'Business Rules': `${data.metrics.validatedBusinessRules}/${data.metrics.totalBusinessRules} validated`,
      'Average Complexity': data.metrics.averageComplexity,
    },
    moduleStatus: data.nodes.map(n => ({
      name: n.name,
      type: n.type,
      status: n.status,
      progress: `${n.progress}%`,
      risk: n.riskLevel,
    })),
    risks: data.riskHotspots,
    recentActivity: data.recentActivity.slice(0, 10),
  };
  
  return c.json({ data: report });
});

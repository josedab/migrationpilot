/**
 * Project management routes
 * 
 * Thin route handlers that delegate to ProjectService for business logic.
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { getUserId, getOrganizationId } from '../middleware/index.js';
import {
  type IProjectRepository,
  type IProjectFileRepository,
  InMemoryProjectRepository,
  InMemoryProjectFileRepository,
} from '../repositories/index.js';
import { ProjectService } from '../services/project-service.js';

// Default to in-memory repositories (can be overridden via factory function)
let projectRepository: IProjectRepository = new InMemoryProjectRepository();
let fileRepository: IProjectFileRepository = new InMemoryProjectFileRepository();
let projectService = new ProjectService(projectRepository, fileRepository);

/**
 * Configure repositories for the project routes
 * Call this at startup to inject different implementations (e.g., database-backed)
 */
export function configureRepositories(
  projRepo: IProjectRepository,
  fileRepo: IProjectFileRepository
): void {
  projectRepository = projRepo;
  fileRepository = fileRepo;
  projectService = new ProjectService(projRepo, fileRepo);
}

export const projectRoutes = new Hono();

const CreateProjectSchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().optional(),
  sourceLanguage: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  targetLanguage: z.enum(['java', 'python', 'typescript', 'go', 'csharp']),
  targetFramework: z.string().optional(),
  settings: z.object({
    enableStranglerFig: z.boolean().optional(),
    generateTests: z.boolean().optional(),
    generateDocumentation: z.boolean().optional(),
    humanReviewRequired: z.boolean().optional(),
    confidenceThreshold: z.number().min(0).max(1).optional(),
  }).optional(),
});

const UpdateProjectSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  description: z.string().optional(),
  targetFramework: z.string().optional(),
  settings: z.object({
    enableStranglerFig: z.boolean().optional(),
    generateTests: z.boolean().optional(),
    generateDocumentation: z.boolean().optional(),
    humanReviewRequired: z.boolean().optional(),
    confidenceThreshold: z.number().min(0).max(1).optional(),
  }).optional(),
});

// List all projects
projectRoutes.get('/', async (c) => {
  const orgId = getOrganizationId(c);
  const projects = await projectService.listProjects(orgId);

  return c.json({
    data: projects,
    total: projects.length,
  });
});

// Get single project
projectRoutes.get('/:id', async (c) => {
  const orgId = getOrganizationId(c);
  const project = await projectService.getProject(c.req.param('id'), orgId);
  
  if (!project) {
    return c.json({ error: 'Project not found' }, 404);
  }

  return c.json({ data: project });
});

// Create project
projectRoutes.post('/', async (c) => {
  const body = await c.req.json();
  const parsed = CreateProjectSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const userId = getUserId(c);
  const orgId = getOrganizationId(c);
  
  const project = await projectService.createProject(parsed.data, userId, orgId);

  return c.json({ data: project }, 201);
});

// Update project
projectRoutes.patch('/:id', async (c) => {
  const orgId = getOrganizationId(c);
  const body = await c.req.json();
  const parsed = UpdateProjectSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const project = await projectService.updateProject(c.req.param('id'), orgId, parsed.data);
  
  if (!project) {
    return c.json({ error: 'Project not found' }, 404);
  }

  return c.json({ data: project });
});

// Delete project
projectRoutes.delete('/:id', async (c) => {
  const orgId = getOrganizationId(c);
  const deleted = await projectService.deleteProject(c.req.param('id'), orgId);
  
  if (!deleted) {
    return c.json({ error: 'Project not found' }, 404);
  }

  return c.json({ success: true });
});

// Upload source files - supports both multipart form data and JSON
projectRoutes.post('/:id/files', async (c) => {
  const projectId = c.req.param('id');
  const orgId = getOrganizationId(c);
  
  // Verify project exists and user has access
  const project = await projectService.getProject(projectId, orgId);
  if (!project) {
    return c.json({ error: 'Project not found' }, 404);
  }

  const contentType = c.req.header('content-type') || '';
  const uploadedFiles: Array<{ id: string; filename: string; lines: number; language: string; duplicate: boolean }> = [];

  // Handle multipart form data (file uploads)
  if (contentType.includes('multipart/form-data')) {
    const formData = await c.req.formData();
    const files = formData.getAll('files');
    
    for (const file of files) {
      if (file instanceof File) {
        const content = await file.text();
        const result = await projectService.uploadFile(projectId, orgId, {
          filename: file.name,
          content,
        });
        if (result) {
          uploadedFiles.push({
            id: result.id,
            filename: file.name,
            lines: result.lines,
            language: project.sourceLanguage,
            duplicate: result.duplicate,
          });
        }
      }
    }
    
    // Also check for single file upload
    const singleFile = formData.get('file');
    if (singleFile instanceof File) {
      const content = await singleFile.text();
      const result = await projectService.uploadFile(projectId, orgId, {
        filename: singleFile.name,
        content,
      });
      if (result) {
        uploadedFiles.push({
          id: result.id,
          filename: singleFile.name,
          lines: result.lines,
          language: project.sourceLanguage,
          duplicate: result.duplicate,
        });
      }
    }
  } 
  // Handle JSON body (single file or batch)
  else {
    const body = await c.req.json();
    
    // Support batch upload via array
    const filesToProcess = Array.isArray(body) ? body : [body];
    
    for (const fileInput of filesToProcess) {
      if (!fileInput.filename || !fileInput.content) {
        return c.json({ error: 'Each file requires filename and content' }, 400);
      }
      
      const result = await projectService.uploadFile(projectId, orgId, {
        filename: fileInput.filename,
        content: fileInput.content,
      });
      if (result) {
        uploadedFiles.push({
          id: result.id,
          filename: fileInput.filename,
          lines: result.lines,
          language: project.sourceLanguage,
          duplicate: result.duplicate,
        });
      }
    }
  }

  if (uploadedFiles.length === 0) {
    return c.json({ error: 'No files provided' }, 400);
  }

  // Fetch updated project for statistics
  const updatedProject = await projectService.getProject(projectId, orgId);

  return c.json({
    data: {
      files: uploadedFiles,
      totalUploaded: uploadedFiles.length,
      projectStatistics: updatedProject?.statistics,
    },
  });
});

// Get uploaded files for a project
projectRoutes.get('/:id/files', async (c) => {
  const projectId = c.req.param('id');
  const orgId = getOrganizationId(c);
  
  const files = await projectService.getProjectFiles(projectId, orgId);
  
  if (!files) {
    return c.json({ error: 'Project not found' }, 404);
  }
  
  return c.json({
    data: files.map(f => ({
      id: f.id,
      filename: f.filename,
      lines: f.lines,
      uploadedAt: f.uploadedAt,
    })),
    total: files.length,
  });
});

// Get single file content
projectRoutes.get('/:id/files/:fileId', async (c) => {
  const projectId = c.req.param('id');
  const fileId = c.req.param('fileId');
  const orgId = getOrganizationId(c);
  
  const files = await projectService.getProjectFiles(projectId, orgId);
  
  if (!files) {
    return c.json({ error: 'Project not found' }, 404);
  }

  const file = files.find(f => f.id === fileId);
  
  if (!file) {
    return c.json({ error: 'File not found' }, 404);
  }

  return c.json({
    data: {
      id: file.id,
      filename: file.filename,
      content: file.content,
      lines: file.lines,
      uploadedAt: file.uploadedAt,
    },
  });
});

// Delete a file
projectRoutes.delete('/:id/files/:fileId', async (c) => {
  const projectId = c.req.param('id');
  const fileId = c.req.param('fileId');
  const orgId = getOrganizationId(c);
  
  const result = await projectService.deleteFile(projectId, orgId, fileId);
  
  if (!result) {
    return c.json({ error: 'Project not found' }, 404);
  }

  if (!result.success) {
    return c.json({ error: 'File not found' }, 404);
  }

  return c.json({ success: true });
});

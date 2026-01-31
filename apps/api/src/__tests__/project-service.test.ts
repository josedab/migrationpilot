/**
 * Tests for ProjectService
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { ProjectService } from '../services/project-service.js';
import type { IProjectRepository, IProjectFileRepository, ProjectFile } from '../repositories/index.js';
import type { Project } from '@migrationpilot/core';

interface MockProjectRepository extends IProjectRepository {
  _setProject(project: Project): void;
  _clear(): void;
}

interface MockFileRepository extends IProjectFileRepository {
  _setFiles(projectId: string, files: ProjectFile[]): void;
  _clear(): void;
}

// Mock repositories
function createMockProjectRepository(): MockProjectRepository {
  const projects = new Map<string, Project>();

  return {
    findById: vi.fn(async (id: string) => projects.get(id) || null),
    findByOrganization: vi.fn(async (orgId: string) => 
      Array.from(projects.values()).filter(p => p.organizationId === orgId)
    ),
    create: vi.fn(async (project: Project) => {
      projects.set(project.id, project);
      return project;
    }),
    update: vi.fn(async (id: string, updates: Partial<Project>) => {
      const existing = projects.get(id);
      if (!existing) return null;
      const updated = { ...existing, ...updates };
      projects.set(id, updated);
      return updated;
    }),
    delete: vi.fn(async (id: string) => {
      const existed = projects.has(id);
      projects.delete(id);
      return existed;
    }),
    exists: vi.fn(async (id: string) => projects.has(id)),
    // Test helpers
    _setProject: (project: Project) => projects.set(project.id, project),
    _clear: () => projects.clear(),
  };
}

function createMockFileRepository(): MockFileRepository {
  const files = new Map<string, ProjectFile[]>();

  return {
    findByProjectId: vi.fn(async (projectId: string) => files.get(projectId) || []),
    findById: vi.fn(async (projectId: string, fileId: string) => {
      const projectFiles = files.get(projectId) || [];
      return projectFiles.find(f => f.id === fileId) || null;
    }),
    findByHash: vi.fn(async (projectId: string, hash: string) => {
      const projectFiles = files.get(projectId) || [];
      return projectFiles.find(f => f.hash === hash) || null;
    }),
    create: vi.fn(async (projectId: string, file: ProjectFile) => {
      const projectFiles = files.get(projectId) || [];
      projectFiles.push(file);
      files.set(projectId, projectFiles);
      return file;
    }),
    delete: vi.fn(async (projectId: string, fileId: string) => {
      const projectFiles = files.get(projectId) || [];
      const index = projectFiles.findIndex(f => f.id === fileId);
      if (index === -1) return null;
      const [removed] = projectFiles.splice(index, 1);
      files.set(projectId, projectFiles);
      return removed!;
    }),
    deleteByProjectId: vi.fn(async (projectId: string) => {
      files.delete(projectId);
    }),
    // Test helpers
    _setFiles: (projectId: string, projectFiles: ProjectFile[]) => files.set(projectId, projectFiles),
    _clear: () => files.clear(),
  };
}

function createTestProject(overrides: Partial<Project> = {}): Project {
  return {
    id: 'test-project-id',
    name: 'Test Project',
    description: 'A test project',
    organizationId: 'test-org',
    sourceLanguage: 'cobol',
    targetLanguage: 'java',
    targetFramework: 'spring-boot',
    status: 'draft',
    settings: {
      enableStranglerFig: false,
      generateTests: true,
      generateDocumentation: true,
      humanReviewRequired: true,
      confidenceThreshold: 0.85,
    },
    statistics: {
      totalFiles: 0,
      totalLines: 0,
      analyzedFiles: 0,
      analyzedLines: 0,
      extractedRules: 0,
      generatedFiles: 0,
      generatedLines: 0,
      testsGenerated: 0,
      testsPassed: 0,
      equivalenceScore: 0,
    },
    createdAt: new Date(),
    updatedAt: new Date(),
    createdBy: 'test-user',
    ...overrides,
  };
}

describe('ProjectService', () => {
  let service: ProjectService;
  let projectRepo: MockProjectRepository;
  let fileRepo: MockFileRepository;

  beforeEach(() => {
    projectRepo = createMockProjectRepository();
    fileRepo = createMockFileRepository();
    service = new ProjectService(projectRepo, fileRepo);
  });

  describe('listProjects', () => {
    it('should return empty list when no projects exist', async () => {
      const projects = await service.listProjects('test-org');
      expect(projects).toEqual([]);
    });

    it('should return projects for the organization', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const projects = await service.listProjects('test-org');
      
      expect(projects).toHaveLength(1);
      expect(projects[0]!.id).toBe('test-project-id');
      expect(projects[0]!.name).toBe('Test Project');
    });

    it('should not return projects from other organizations', async () => {
      const project = createTestProject({ organizationId: 'other-org' });
      projectRepo._setProject(project);

      const projects = await service.listProjects('test-org');
      expect(projects).toHaveLength(0);
    });
  });

  describe('getProject', () => {
    it('should return null when project does not exist', async () => {
      const project = await service.getProject('non-existent', 'test-org');
      expect(project).toBeNull();
    });

    it('should return null when organization does not match', async () => {
      const project = createTestProject({ organizationId: 'other-org' });
      projectRepo._setProject(project);

      const result = await service.getProject('test-project-id', 'test-org');
      expect(result).toBeNull();
    });

    it('should return project when id and organization match', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.getProject('test-project-id', 'test-org');
      expect(result).not.toBeNull();
      expect(result!.id).toBe('test-project-id');
    });
  });

  describe('createProject', () => {
    it('should create a new project with defaults', async () => {
      const project = await service.createProject(
        {
          name: 'New Project',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        },
        'user-1',
        'org-1'
      );

      expect(project.name).toBe('New Project');
      expect(project.sourceLanguage).toBe('cobol');
      expect(project.targetLanguage).toBe('java');
      expect(project.targetFramework).toBe('spring-boot');
      expect(project.status).toBe('draft');
      expect(project.organizationId).toBe('org-1');
      expect(project.createdBy).toBe('user-1');
      expect(projectRepo.create).toHaveBeenCalled();
    });

    it('should use provided settings', async () => {
      const project = await service.createProject(
        {
          name: 'Custom Project',
          sourceLanguage: 'fortran',
          targetLanguage: 'python',
          settings: {
            enableStranglerFig: true,
            confidenceThreshold: 0.9,
          },
        },
        'user-1',
        'org-1'
      );

      expect(project.settings.enableStranglerFig).toBe(true);
      expect(project.settings.confidenceThreshold).toBe(0.9);
    });
  });

  describe('updateProject', () => {
    it('should return null when project does not exist', async () => {
      const result = await service.updateProject('non-existent', 'test-org', { name: 'Updated' });
      expect(result).toBeNull();
    });

    it('should update project fields', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.updateProject('test-project-id', 'test-org', {
        name: 'Updated Name',
        description: 'Updated description',
      });

      expect(result).not.toBeNull();
      expect(result!.name).toBe('Updated Name');
      expect(result!.description).toBe('Updated description');
    });

    it('should merge settings instead of replacing', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.updateProject('test-project-id', 'test-org', {
        settings: {
          enableStranglerFig: true,
        },
      });

      expect(result).not.toBeNull();
      expect(result!.settings.enableStranglerFig).toBe(true);
      expect(result!.settings.generateTests).toBe(true); // Original value preserved
    });
  });

  describe('deleteProject', () => {
    it('should return false when project does not exist', async () => {
      const result = await service.deleteProject('non-existent', 'test-org');
      expect(result).toBe(false);
    });

    it('should delete project and its files', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.deleteProject('test-project-id', 'test-org');

      expect(result).toBe(true);
      expect(fileRepo.deleteByProjectId).toHaveBeenCalledWith('test-project-id');
      expect(projectRepo.delete).toHaveBeenCalledWith('test-project-id');
    });
  });

  describe('uploadFile', () => {
    it('should return null when project does not exist', async () => {
      const result = await service.uploadFile('non-existent', 'test-org', {
        filename: 'test.cbl',
        content: 'IDENTIFICATION DIVISION.',
      });
      expect(result).toBeNull();
    });

    it('should upload file and update statistics', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.uploadFile('test-project-id', 'test-org', {
        filename: 'test.cbl',
        content: 'line1\nline2\nline3',
      });

      expect(result).not.toBeNull();
      expect(result!.lines).toBe(3);
      expect(result!.duplicate).toBe(false);
      expect(fileRepo.create).toHaveBeenCalled();
      expect(projectRepo.update).toHaveBeenCalled();
    });

    it('should detect duplicate files by hash', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      // First upload
      await service.uploadFile('test-project-id', 'test-org', {
        filename: 'test.cbl',
        content: 'duplicate content',
      });

      // Second upload with same content
      const result = await service.uploadFile('test-project-id', 'test-org', {
        filename: 'test2.cbl',
        content: 'duplicate content',
      });

      expect(result).not.toBeNull();
      expect(result!.duplicate).toBe(true);
    });
  });

  describe('getProjectFiles', () => {
    it('should return null when project does not exist', async () => {
      const result = await service.getProjectFiles('non-existent', 'test-org');
      expect(result).toBeNull();
    });

    it('should return files for project', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const testFile: ProjectFile = {
        id: 'file-1',
        filename: 'test.cbl',
        content: 'test content',
        lines: 1,
        hash: 'abc123',
        uploadedAt: new Date(),
      };
      fileRepo._setFiles('test-project-id', [testFile]);

      const files = await service.getProjectFiles('test-project-id', 'test-org');

      expect(files).not.toBeNull();
      expect(files).toHaveLength(1);
      expect(files![0]!.filename).toBe('test.cbl');
    });
  });

  describe('deleteFile', () => {
    it('should return null when project does not exist', async () => {
      const result = await service.deleteFile('non-existent', 'test-org', 'file-1');
      expect(result).toBeNull();
    });

    it('should return success false when file does not exist', async () => {
      const project = createTestProject();
      projectRepo._setProject(project);

      const result = await service.deleteFile('test-project-id', 'test-org', 'non-existent');

      expect(result).not.toBeNull();
      expect(result!.success).toBe(false);
    });

    it('should delete file and update statistics', async () => {
      const project = createTestProject({
        statistics: {
          ...createTestProject().statistics,
          totalFiles: 2,
          totalLines: 100,
        },
      });
      projectRepo._setProject(project);

      const testFile: ProjectFile = {
        id: 'file-1',
        filename: 'test.cbl',
        content: 'test content',
        lines: 50,
        hash: 'abc123',
        uploadedAt: new Date(),
      };
      fileRepo._setFiles('test-project-id', [testFile]);

      const result = await service.deleteFile('test-project-id', 'test-org', 'file-1');

      expect(result).not.toBeNull();
      expect(result!.success).toBe(true);
      expect(result!.linesRemoved).toBe(50);
      expect(projectRepo.update).toHaveBeenCalled();
    });
  });
});

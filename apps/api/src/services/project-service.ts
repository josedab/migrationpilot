/**
 * Project Service
 * 
 * Encapsulates business logic for project management.
 * Separates concerns from route handlers.
 */

import type { Project, ProjectSettings, SourceLanguage, TargetLanguage } from '@migrationpilot/core';
import { generateId, DEFAULT_PROJECT_SETTINGS } from '@migrationpilot/core';
import { createHash } from 'crypto';
import type { IProjectRepository, IProjectFileRepository, ProjectFile } from '../repositories/index.js';

export interface CreateProjectInput {
  name: string;
  description?: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  targetFramework?: string;
  settings?: Partial<ProjectSettings>;
}

export interface UploadFileInput {
  filename: string;
  content: string;
}

export interface ProjectListItem {
  id: string;
  name: string;
  status: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  statistics: Project['statistics'];
  createdAt: Date;
  updatedAt: Date;
}

export class ProjectService {
  constructor(
    private projectRepository: IProjectRepository,
    private fileRepository: IProjectFileRepository,
  ) {}

  /**
   * List all projects for an organization
   */
  async listProjects(organizationId: string): Promise<ProjectListItem[]> {
    const projects = await this.projectRepository.findByOrganization(organizationId);
    return projects.map(p => ({
      id: p.id,
      name: p.name,
      status: p.status,
      sourceLanguage: p.sourceLanguage,
      targetLanguage: p.targetLanguage,
      statistics: p.statistics,
      createdAt: p.createdAt,
      updatedAt: p.updatedAt,
    }));
  }

  /**
   * Get a project by ID with authorization check
   */
  async getProject(projectId: string, organizationId: string): Promise<Project | null> {
    const project = await this.projectRepository.findById(projectId);
    if (!project || project.organizationId !== organizationId) {
      return null;
    }
    return project;
  }

  /**
   * Create a new project
   */
  async createProject(
    input: CreateProjectInput,
    userId: string,
    organizationId: string,
  ): Promise<Project> {
    const now = new Date();
    const project: Project = {
      id: generateId(),
      name: input.name,
      description: input.description || '',
      organizationId,
      createdBy: userId,
      sourceLanguage: input.sourceLanguage,
      targetLanguage: input.targetLanguage,
      targetFramework: input.targetFramework || this.getDefaultFramework(input.targetLanguage),
      status: 'draft',
      settings: {
        ...DEFAULT_PROJECT_SETTINGS,
        ...input.settings,
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
      createdAt: now,
      updatedAt: now,
    };

    await this.projectRepository.create(project);
    return project;
  }

  /**
   * Update an existing project
   */
  async updateProject(
    projectId: string,
    organizationId: string,
    updates: {
      name?: string;
      description?: string;
      targetFramework?: string;
      settings?: Partial<ProjectSettings>;
    },
  ): Promise<Project | null> {
    const project = await this.getProject(projectId, organizationId);
    if (!project) {
      return null;
    }

    // Merge settings if provided
    const mergedSettings = updates.settings 
      ? { ...project.settings, ...updates.settings }
      : project.settings;

    return this.projectRepository.update(projectId, {
      ...(updates.name && { name: updates.name }),
      ...(updates.description !== undefined && { description: updates.description }),
      ...(updates.targetFramework && { targetFramework: updates.targetFramework }),
      ...(updates.settings && { settings: mergedSettings }),
      updatedAt: new Date(),
    });
  }

  /**
   * Delete a project and its files
   */
  async deleteProject(projectId: string, organizationId: string): Promise<boolean> {
    const project = await this.getProject(projectId, organizationId);
    if (!project) {
      return false;
    }

    await this.fileRepository.deleteByProjectId(projectId);
    await this.projectRepository.delete(projectId);
    return true;
  }

  /**
   * Upload a file to a project with deduplication
   */
  async uploadFile(
    projectId: string,
    organizationId: string,
    input: UploadFileInput,
  ): Promise<{ id: string; lines: number; hash: string; duplicate: boolean } | null> {
    const project = await this.getProject(projectId, organizationId);
    if (!project) {
      return null;
    }

    const lines = input.content.split('\n').length;
    const hash = this.computeHash(input.content);

    // Check for duplicate
    const existing = await this.fileRepository.findByHash(projectId, hash);
    if (existing) {
      return { id: existing.id, lines: existing.lines, hash: existing.hash, duplicate: true };
    }

    // Create new file (interface requires projectId as first arg)
    const file: ProjectFile = {
      id: generateId(),
      filename: input.filename,
      content: input.content,
      lines,
      hash,
      uploadedAt: new Date(),
    };

    await this.fileRepository.create(projectId, file);

    // Update project statistics
    await this.projectRepository.update(projectId, {
      statistics: {
        ...project.statistics,
        totalFiles: project.statistics.totalFiles + 1,
        totalLines: project.statistics.totalLines + lines,
      },
      updatedAt: new Date(),
    });

    return { id: file.id, lines, hash, duplicate: false };
  }

  /**
   * Get files for a project
   */
  async getProjectFiles(projectId: string, organizationId: string): Promise<ProjectFile[] | null> {
    const project = await this.getProject(projectId, organizationId);
    if (!project) {
      return null;
    }
    return this.fileRepository.findByProjectId(projectId);
  }

  /**
   * Delete a file from a project
   */
  async deleteFile(
    projectId: string,
    organizationId: string,
    fileId: string,
  ): Promise<{ success: boolean; linesRemoved: number } | null> {
    const project = await this.getProject(projectId, organizationId);
    if (!project) {
      return null;
    }

    const removed = await this.fileRepository.delete(projectId, fileId);
    if (!removed) {
      return { success: false, linesRemoved: 0 };
    }

    // Update project statistics
    await this.projectRepository.update(projectId, {
      statistics: {
        ...project.statistics,
        totalFiles: Math.max(0, project.statistics.totalFiles - 1),
        totalLines: Math.max(0, project.statistics.totalLines - removed.lines),
      },
      updatedAt: new Date(),
    });

    return { success: true, linesRemoved: removed.lines };
  }

  /**
   * Get default framework for a target language
   */
  private getDefaultFramework(language: TargetLanguage): string {
    const defaults: Record<TargetLanguage, string> = {
      java: 'spring-boot',
      python: 'fastapi',
      typescript: 'nestjs',
      go: 'gin',
      csharp: 'aspnet-core',
    };
    return defaults[language];
  }

  /**
   * Compute SHA-256 hash of content for deduplication
   */
  private computeHash(content: string): string {
    return createHash('sha256').update(content).digest('hex').slice(0, 16);
  }
}

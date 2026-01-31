/**
 * In-memory implementation of project repositories
 * 
 * Use for development and testing. Replace with database implementation for production.
 */

import type { Project } from '@migrationpilot/core';
import type { IProjectRepository, IProjectFileRepository, ProjectFile } from './interfaces.js';

export class InMemoryProjectRepository implements IProjectRepository {
  private projects = new Map<string, Project>();

  async findById(id: string): Promise<Project | null> {
    return this.projects.get(id) ?? null;
  }

  async findByOrganization(orgId: string): Promise<Project[]> {
    return Array.from(this.projects.values())
      .filter(p => p.organizationId === orgId);
  }

  async create(project: Project): Promise<Project> {
    this.projects.set(project.id, project);
    return project;
  }

  async update(id: string, updates: Partial<Project>): Promise<Project | null> {
    const existing = this.projects.get(id);
    if (!existing) return null;

    const updated = {
      ...existing,
      ...updates,
      id: existing.id, // Prevent ID change
      updatedAt: new Date(),
    };
    this.projects.set(id, updated);
    return updated;
  }

  async delete(id: string): Promise<boolean> {
    return this.projects.delete(id);
  }

  async exists(id: string): Promise<boolean> {
    return this.projects.has(id);
  }
}

export class InMemoryProjectFileRepository implements IProjectFileRepository {
  private files = new Map<string, ProjectFile[]>();

  async findByProjectId(projectId: string): Promise<ProjectFile[]> {
    return this.files.get(projectId) ?? [];
  }

  async findById(projectId: string, fileId: string): Promise<ProjectFile | null> {
    const projectFiles = this.files.get(projectId) ?? [];
    return projectFiles.find(f => f.id === fileId) ?? null;
  }

  async findByHash(projectId: string, hash: string): Promise<ProjectFile | null> {
    const projectFiles = this.files.get(projectId) ?? [];
    return projectFiles.find(f => f.hash === hash) ?? null;
  }

  async create(projectId: string, file: ProjectFile): Promise<ProjectFile> {
    if (!this.files.has(projectId)) {
      this.files.set(projectId, []);
    }
    this.files.get(projectId)!.push(file);
    return file;
  }

  async delete(projectId: string, fileId: string): Promise<ProjectFile | null> {
    const projectFiles = this.files.get(projectId);
    if (!projectFiles) return null;
    
    const index = projectFiles.findIndex(f => f.id === fileId);
    if (index === -1) return null;
    
    const [removed] = projectFiles.splice(index, 1);
    return removed ?? null;
  }

  async deleteByProjectId(projectId: string): Promise<void> {
    this.files.delete(projectId);
  }
}

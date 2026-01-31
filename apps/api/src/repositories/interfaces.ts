/**
 * Repository interfaces for data access abstraction
 * 
 * These interfaces enable swapping between in-memory, database, or other
 * storage implementations without changing the route handlers.
 */

import type { Project } from '@migrationpilot/core';

export interface ProjectFile {
  id: string;
  filename: string;
  content: string;
  lines: number;
  hash: string;
  uploadedAt: Date;
}

export interface IProjectRepository {
  findById(id: string): Promise<Project | null>;
  findByOrganization(orgId: string): Promise<Project[]>;
  create(project: Project): Promise<Project>;
  update(id: string, project: Partial<Project>): Promise<Project | null>;
  delete(id: string): Promise<boolean>;
  exists(id: string): Promise<boolean>;
}

export interface IProjectFileRepository {
  findByProjectId(projectId: string): Promise<ProjectFile[]>;
  findById(projectId: string, fileId: string): Promise<ProjectFile | null>;
  findByHash(projectId: string, hash: string): Promise<ProjectFile | null>;
  create(projectId: string, file: ProjectFile): Promise<ProjectFile>;
  delete(projectId: string, fileId: string): Promise<ProjectFile | null>;
  deleteByProjectId(projectId: string): Promise<void>;
}

/**
 * Project Routes Tests
 * 
 * Tests for file upload and project management endpoints
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Hono } from 'hono';
import { projectRoutes } from '../routes/projects.js';

// Create test app with mocked middleware
function createTestApp() {
  const app = new Hono();
  
  // Mock auth middleware - use type assertion to set context values
  app.use('*', async (c, next) => {
    // @ts-expect-error - mock context values for testing
    c.set('userId', 'test-user-id');
    // @ts-expect-error - mock context values for testing
    c.set('organizationId', 'test-org-id');
    await next();
  });
  
  app.route('/api/projects', projectRoutes);
  return app;
}

describe('Project Routes', () => {
  let app: ReturnType<typeof createTestApp>;

  beforeEach(() => {
    app = createTestApp();
  });

  describe('POST /api/projects', () => {
    it('should create a new project', async () => {
      const res = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Test Project',
          description: 'A test migration project',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });

      expect(res.status).toBe(201);
      const body = await res.json();
      expect(body.data).toBeDefined();
      expect(body.data.name).toBe('Test Project');
      expect(body.data.sourceLanguage).toBe('cobol');
      expect(body.data.targetLanguage).toBe('java');
      expect(body.data.id).toBeDefined();
    });

    it('should reject invalid source language', async () => {
      const res = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Test Project',
          sourceLanguage: 'invalid',
          targetLanguage: 'java',
        }),
      });

      expect(res.status).toBe(400);
    });

    it('should reject missing name', async () => {
      const res = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });

      expect(res.status).toBe(400);
    });
  });

  describe('GET /api/projects', () => {
    it('should return empty list initially', async () => {
      const res = await app.request('/api/projects');

      expect(res.status).toBe(200);
      const body = await res.json();
      expect(body.data).toBeInstanceOf(Array);
      expect(body.total).toBeDefined();
    });
  });

  describe('POST /api/projects/:id/files (JSON)', () => {
    it('should upload a single file via JSON', async () => {
      // First create a project
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'File Upload Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      // Upload a file
      const fileContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO WORLD".
           STOP RUN.`;

      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          filename: 'HELLO.cbl',
          content: fileContent,
        }),
      });

      expect(uploadRes.status).toBe(200);
      const uploadBody = await uploadRes.json();
      expect(uploadBody.data.files).toBeInstanceOf(Array);
      expect(uploadBody.data.files.length).toBe(1);
      expect(uploadBody.data.files[0].filename).toBe('HELLO.cbl');
      expect(uploadBody.data.files[0].lines).toBe(5);
    });

    it('should upload multiple files via JSON array', async () => {
      // Create project
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Batch Upload Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      // Upload multiple files
      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify([
          { filename: 'FILE1.cbl', content: 'CONTENT LINE 1' },
          { filename: 'FILE2.cbl', content: 'CONTENT LINE 1\nCONTENT LINE 2' },
          { filename: 'FILE3.cbl', content: 'CONTENT LINE 1\nCONTENT LINE 2\nCONTENT LINE 3' },
        ]),
      });

      expect(uploadRes.status).toBe(200);
      const uploadBody = await uploadRes.json();
      expect(uploadBody.data.files.length).toBe(3);
      expect(uploadBody.data.totalUploaded).toBe(3);
    });

    it('should deduplicate files with same content', async () => {
      // Create project
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Dedup Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const sameContent = 'DUPLICATE CONTENT';

      // Upload same content twice
      await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'FILE1.cbl', content: sameContent }),
      });

      const secondRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'FILE2.cbl', content: sameContent }),
      });

      expect(secondRes.status).toBe(200);
      // Second upload should return the same file ID (deduplicated)
    });

    it('should reject upload without filename', async () => {
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Validation Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ content: 'some content' }),
      });

      expect(uploadRes.status).toBe(400);
    });

    it('should return 404 for non-existent project', async () => {
      const uploadRes = await app.request('/api/projects/non-existent-id/files', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'test.cbl', content: 'content' }),
      });

      expect(uploadRes.status).toBe(404);
    });
  });

  describe('GET /api/projects/:id/files', () => {
    it('should list uploaded files', async () => {
      // Create project and upload files
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'List Files Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify([
          { filename: 'FILE1.cbl', content: 'content 1' },
          { filename: 'FILE2.cbl', content: 'content 2' },
        ]),
      });

      // List files
      const listRes = await app.request(`/api/projects/${project.id}/files`);

      expect(listRes.status).toBe(200);
      const listBody = await listRes.json();
      expect(listBody.data.length).toBe(2);
      expect(listBody.total).toBe(2);
    });
  });

  describe('GET /api/projects/:id/files/:fileId', () => {
    it('should get file content by ID', async () => {
      // Create project and upload file
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Get File Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const fileContent = 'TEST CONTENT FOR FILE';
      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'TEST.cbl', content: fileContent }),
      });
      const { data: uploadData } = await uploadRes.json();
      const fileId = uploadData.files[0].id;

      // Get file content
      const getRes = await app.request(`/api/projects/${project.id}/files/${fileId}`);

      expect(getRes.status).toBe(200);
      const fileData = await getRes.json();
      expect(fileData.data.content).toBe(fileContent);
      expect(fileData.data.filename).toBe('TEST.cbl');
    });

    it('should return 404 for non-existent file', async () => {
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: '404 File Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const getRes = await app.request(`/api/projects/${project.id}/files/non-existent-id`);

      expect(getRes.status).toBe(404);
    });
  });

  describe('DELETE /api/projects/:id/files/:fileId', () => {
    it('should delete a file', async () => {
      // Create project and upload file
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Delete File Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'DELETE-ME.cbl', content: 'to be deleted' }),
      });
      const { data: uploadData } = await uploadRes.json();
      const fileId = uploadData.files[0].id;

      // Delete file
      const deleteRes = await app.request(`/api/projects/${project.id}/files/${fileId}`, {
        method: 'DELETE',
      });

      expect(deleteRes.status).toBe(200);
      const deleteBody = await deleteRes.json();
      expect(deleteBody.success).toBe(true);

      // Verify file is gone
      const getRes = await app.request(`/api/projects/${project.id}/files/${fileId}`);
      expect(getRes.status).toBe(404);
    });

    it('should update project statistics after delete', async () => {
      // Create project and upload file
      const createRes = await app.request('/api/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: 'Stats Update Test',
          sourceLanguage: 'cobol',
          targetLanguage: 'java',
        }),
      });
      const { data: project } = await createRes.json();

      const content = 'LINE1\nLINE2\nLINE3';
      const uploadRes = await app.request(`/api/projects/${project.id}/files`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ filename: 'STATS.cbl', content }),
      });
      const { data: uploadData } = await uploadRes.json();

      // Check stats after upload
      expect(uploadData.projectStatistics.totalFiles).toBe(1);
      expect(uploadData.projectStatistics.totalLines).toBe(3);

      // Delete and check stats
      const fileId = uploadData.files[0].id;
      await app.request(`/api/projects/${project.id}/files/${fileId}`, {
        method: 'DELETE',
      });

      // Get project to verify stats
      const projectRes = await app.request(`/api/projects/${project.id}`);
      const projectData = await projectRes.json();
      expect(projectData.data.statistics.totalFiles).toBe(0);
      expect(projectData.data.statistics.totalLines).toBe(0);
    });
  });
});

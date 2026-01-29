'use client';

/**
 * Project Detail Page
 * 
 * Shows migration progress and allows navigation to different project views
 */

import Link from 'next/link';
import { useState, useEffect } from 'react';

interface Project {
  id: string;
  name: string;
  sourceLanguage: string;
  targetLanguage: string;
  status: string;
  progress: number;
  createdAt: string;
  stats: {
    files: number;
    linesOfCode: number;
    businessRules: number;
    dataStructures: number;
    testsPassing: number;
    testsFailing: number;
  };
  phases: Array<{
    name: string;
    status: 'pending' | 'in-progress' | 'complete';
    progress: number;
  }>;
}

const mockProject: Project = {
  id: 'proj_001',
  name: 'Banking Core Migration',
  sourceLanguage: 'COBOL',
  targetLanguage: 'Java',
  status: 'in-progress',
  progress: 65,
  createdAt: '2024-01-15',
  stats: {
    files: 70,
    linesOfCode: 125000,
    businessRules: 342,
    dataStructures: 89,
    testsPassing: 183,
    testsFailing: 10,
  },
  phases: [
    { name: 'Analysis', status: 'complete', progress: 100 },
    { name: 'Architecture Design', status: 'complete', progress: 100 },
    { name: 'Code Generation', status: 'in-progress', progress: 65 },
    { name: 'Validation', status: 'pending', progress: 0 },
  ],
};

export default function ProjectPage({ params }: { params: { id: string } }) {
  const [project, setProject] = useState<Project>(mockProject);

  const statusColors = {
    pending: 'bg-gray-200 text-gray-700',
    'in-progress': 'bg-blue-100 text-blue-700',
    complete: 'bg-green-100 text-green-700',
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 py-8">
        {/* Header */}
        <div className="flex items-center justify-between mb-8">
          <div>
            <div className="flex items-center gap-2 text-sm text-gray-500 mb-2">
              <Link href="/dashboard" className="hover:text-gray-700">Dashboard</Link>
              <span>/</span>
              <span>{project.name}</span>
            </div>
            <h1 className="text-3xl font-bold text-gray-900">{project.name}</h1>
            <p className="text-gray-600 mt-1">
              {project.sourceLanguage} → {project.targetLanguage}
            </p>
          </div>
          <span className={`px-4 py-2 rounded-full text-sm font-medium ${
            project.status === 'in-progress' ? 'bg-blue-100 text-blue-700' : 'bg-green-100 text-green-700'
          }`}>
            {project.status}
          </span>
        </div>

        {/* Overall Progress */}
        <div className="bg-white rounded-lg shadow-sm p-6 mb-6">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-lg font-semibold">Overall Progress</h2>
            <span className="text-2xl font-bold text-blue-600">{project.progress}%</span>
          </div>
          <div className="w-full bg-gray-200 rounded-full h-3">
            <div
              className="bg-blue-600 h-3 rounded-full transition-all duration-300"
              style={{ width: `${project.progress}%` }}
            />
          </div>
        </div>

        {/* Stats Cards */}
        <div className="grid grid-cols-6 gap-4 mb-6">
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold">{project.stats.files}</div>
            <div className="text-sm text-gray-500">Files</div>
          </div>
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold">{(project.stats.linesOfCode / 1000).toFixed(0)}K</div>
            <div className="text-sm text-gray-500">Lines of Code</div>
          </div>
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold">{project.stats.businessRules}</div>
            <div className="text-sm text-gray-500">Business Rules</div>
          </div>
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold">{project.stats.dataStructures}</div>
            <div className="text-sm text-gray-500">Data Structures</div>
          </div>
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold text-green-600">{project.stats.testsPassing}</div>
            <div className="text-sm text-gray-500">Tests Passing</div>
          </div>
          <div className="bg-white rounded-lg shadow-sm p-4">
            <div className="text-2xl font-bold text-red-600">{project.stats.testsFailing}</div>
            <div className="text-sm text-gray-500">Tests Failing</div>
          </div>
        </div>

        <div className="grid grid-cols-3 gap-6">
          {/* Migration Phases */}
          <div className="col-span-2">
            <div className="bg-white rounded-lg shadow-sm p-6">
              <h2 className="text-lg font-semibold mb-6">Migration Phases</h2>
              <div className="space-y-6">
                {project.phases.map((phase, index) => (
                  <div key={phase.name} className="flex items-start gap-4">
                    <div className={`w-8 h-8 rounded-full flex items-center justify-center text-sm font-semibold ${
                      phase.status === 'complete'
                        ? 'bg-green-500 text-white'
                        : phase.status === 'in-progress'
                        ? 'bg-blue-500 text-white'
                        : 'bg-gray-200 text-gray-500'
                    }`}>
                      {phase.status === 'complete' ? '✓' : index + 1}
                    </div>
                    <div className="flex-1">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-medium">{phase.name}</span>
                        <span className={`px-2 py-1 rounded text-xs ${statusColors[phase.status]}`}>
                          {phase.status}
                        </span>
                      </div>
                      <div className="w-full bg-gray-200 rounded-full h-2">
                        <div
                          className={`h-2 rounded-full transition-all ${
                            phase.status === 'complete' ? 'bg-green-500' : 'bg-blue-500'
                          }`}
                          style={{ width: `${phase.progress}%` }}
                        />
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </div>

          {/* Quick Actions */}
          <div className="col-span-1">
            <div className="bg-white rounded-lg shadow-sm p-6">
              <h2 className="text-lg font-semibold mb-4">Quick Actions</h2>
              <div className="space-y-3">
                <Link
                  href={`/projects/${params.id}/rules`}
                  className="block w-full p-4 border rounded-lg hover:bg-gray-50 transition-colors"
                >
                  <div className="flex items-center gap-3">
                    <span className="text-2xl">📋</span>
                    <div>
                      <div className="font-medium">Review Business Rules</div>
                      <div className="text-sm text-gray-500">
                        {project.stats.businessRules} rules to review
                      </div>
                    </div>
                  </div>
                </Link>
                <Link
                  href={`/projects/${params.id}/code`}
                  className="block w-full p-4 border rounded-lg hover:bg-gray-50 transition-colors"
                >
                  <div className="flex items-center gap-3">
                    <span className="text-2xl">💻</span>
                    <div>
                      <div className="font-medium">View Generated Code</div>
                      <div className="text-sm text-gray-500">
                        Side-by-side comparison
                      </div>
                    </div>
                  </div>
                </Link>
                <Link
                  href={`/projects/${params.id}/tests`}
                  className="block w-full p-4 border rounded-lg hover:bg-gray-50 transition-colors"
                >
                  <div className="flex items-center gap-3">
                    <span className="text-2xl">🧪</span>
                    <div>
                      <div className="font-medium">Equivalence Tests</div>
                      <div className="text-sm text-gray-500">
                        {project.stats.testsPassing}/{project.stats.testsPassing + project.stats.testsFailing} passing
                      </div>
                    </div>
                  </div>
                </Link>
                <button className="w-full p-4 border rounded-lg hover:bg-blue-50 hover:border-blue-200 transition-colors text-left">
                  <div className="flex items-center gap-3">
                    <span className="text-2xl">📄</span>
                    <div>
                      <div className="font-medium">Export Documentation</div>
                      <div className="text-sm text-gray-500">
                        PDF, Word, or Confluence
                      </div>
                    </div>
                  </div>
                </button>
              </div>
            </div>

            {/* Activity Feed */}
            <div className="bg-white rounded-lg shadow-sm p-6 mt-6">
              <h2 className="text-lg font-semibold mb-4">Recent Activity</h2>
              <div className="space-y-4">
                <div className="flex gap-3 text-sm">
                  <div className="w-2 h-2 rounded-full bg-green-500 mt-1.5" />
                  <div>
                    <div>Business rule BR-003 approved</div>
                    <div className="text-gray-500">2 hours ago</div>
                  </div>
                </div>
                <div className="flex gap-3 text-sm">
                  <div className="w-2 h-2 rounded-full bg-blue-500 mt-1.5" />
                  <div>
                    <div>45 modules generated</div>
                    <div className="text-gray-500">5 hours ago</div>
                  </div>
                </div>
                <div className="flex gap-3 text-sm">
                  <div className="w-2 h-2 rounded-full bg-yellow-500 mt-1.5" />
                  <div>
                    <div>Comment added to BR-002</div>
                    <div className="text-gray-500">1 day ago</div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

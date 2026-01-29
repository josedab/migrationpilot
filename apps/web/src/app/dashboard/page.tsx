'use client';

import { useState } from 'react';
import Link from 'next/link';
import {
  Code2,
  Plus,
  FolderOpen,
  Clock,
  CheckCircle,
  AlertCircle,
  BarChart3,
  Settings,
} from 'lucide-react';

// Mock data for demo
const mockProjects = [
  {
    id: '1',
    name: 'Loan Processing System',
    sourceLanguage: 'COBOL',
    targetLanguage: 'Java',
    status: 'analyzing',
    progress: 45,
    files: 247,
    rules: 156,
    updatedAt: '2 hours ago',
  },
  {
    id: '2',
    name: 'Actuarial Models',
    sourceLanguage: 'Fortran',
    targetLanguage: 'Python',
    status: 'completed',
    progress: 100,
    files: 89,
    rules: 312,
    updatedAt: '1 day ago',
  },
  {
    id: '3',
    name: 'Inventory Management',
    sourceLanguage: 'VB6',
    targetLanguage: 'TypeScript',
    status: 'draft',
    progress: 0,
    files: 0,
    rules: 0,
    updatedAt: '3 days ago',
  },
];

export default function DashboardPage() {
  const [projects] = useState(mockProjects);

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-900">
      {/* Sidebar */}
      <aside className="fixed left-0 top-0 h-full w-64 bg-white dark:bg-slate-800 border-r border-slate-200 dark:border-slate-700">
        <div className="p-4">
          <Link href="/" className="flex items-center gap-2">
            <Code2 className="h-8 w-8 text-blue-600" />
            <span className="text-xl font-bold dark:text-white">MigrationPilot</span>
          </Link>
        </div>
        <nav className="mt-8">
          <NavItem icon={<FolderOpen />} label="Projects" active />
          <NavItem icon={<BarChart3 />} label="Analytics" />
          <NavItem icon={<Settings />} label="Settings" />
        </nav>
      </aside>

      {/* Main content */}
      <main className="ml-64 p-8">
        {/* Header */}
        <div className="flex items-center justify-between mb-8">
          <div>
            <h1 className="text-2xl font-bold dark:text-white">Projects</h1>
            <p className="text-slate-500 dark:text-slate-400">
              Manage your migration projects
            </p>
          </div>
          <Link
            href="/dashboard/new"
            className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg font-medium flex items-center gap-2"
          >
            <Plus className="h-5 w-5" />
            New Project
          </Link>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-4 gap-4 mb-8">
          <StatCard label="Total Projects" value="3" />
          <StatCard label="Lines Analyzed" value="1.2M" />
          <StatCard label="Rules Extracted" value="468" />
          <StatCard label="Avg Equivalence" value="98.7%" />
        </div>

        {/* Project list */}
        <div className="bg-white dark:bg-slate-800 rounded-xl border border-slate-200 dark:border-slate-700">
          <div className="p-4 border-b border-slate-200 dark:border-slate-700">
            <h2 className="font-semibold dark:text-white">Recent Projects</h2>
          </div>
          <div className="divide-y divide-slate-200 dark:divide-slate-700">
            {projects.map((project) => (
              <ProjectRow key={project.id} project={project} />
            ))}
          </div>
        </div>
      </main>
    </div>
  );
}

function NavItem({
  icon,
  label,
  active,
}: {
  icon: React.ReactNode;
  label: string;
  active?: boolean;
}) {
  return (
    <a
      href="#"
      className={`flex items-center gap-3 px-4 py-3 text-sm font-medium ${
        active
          ? 'text-blue-600 bg-blue-50 dark:bg-blue-900/20 border-r-2 border-blue-600'
          : 'text-slate-600 dark:text-slate-400 hover:bg-slate-50 dark:hover:bg-slate-700'
      }`}
    >
      {icon}
      {label}
    </a>
  );
}

function StatCard({ label, value }: { label: string; value: string }) {
  return (
    <div className="bg-white dark:bg-slate-800 rounded-xl p-4 border border-slate-200 dark:border-slate-700">
      <p className="text-sm text-slate-500 dark:text-slate-400">{label}</p>
      <p className="text-2xl font-bold dark:text-white">{value}</p>
    </div>
  );
}

function ProjectRow({ project }: { project: (typeof mockProjects)[0] }) {
  const statusConfig = {
    draft: { icon: Clock, color: 'text-slate-500', bg: 'bg-slate-100' },
    analyzing: { icon: Clock, color: 'text-yellow-500', bg: 'bg-yellow-100' },
    completed: { icon: CheckCircle, color: 'text-green-500', bg: 'bg-green-100' },
    failed: { icon: AlertCircle, color: 'text-red-500', bg: 'bg-red-100' },
  };

  const status = statusConfig[project.status as keyof typeof statusConfig];
  const StatusIcon = status.icon;

  return (
    <Link
      href={`/dashboard/project/${project.id}`}
      className="flex items-center p-4 hover:bg-slate-50 dark:hover:bg-slate-700/50"
    >
      <div className="flex-1">
        <h3 className="font-medium dark:text-white">{project.name}</h3>
        <p className="text-sm text-slate-500 dark:text-slate-400">
          {project.sourceLanguage} → {project.targetLanguage}
        </p>
      </div>
      <div className="flex items-center gap-8">
        <div className="text-right">
          <p className="text-sm font-medium dark:text-white">{project.files} files</p>
          <p className="text-xs text-slate-500">{project.rules} rules</p>
        </div>
        <div className="w-32">
          {project.status === 'analyzing' ? (
            <div className="relative pt-1">
              <div className="flex mb-1 items-center justify-between">
                <span className="text-xs text-slate-500">{project.progress}%</span>
              </div>
              <div className="w-full bg-slate-200 rounded-full h-2">
                <div
                  className="bg-blue-600 h-2 rounded-full"
                  style={{ width: `${project.progress}%` }}
                />
              </div>
            </div>
          ) : (
            <span
              className={`inline-flex items-center gap-1 px-2 py-1 rounded-full text-xs font-medium ${status.bg} ${status.color}`}
            >
              <StatusIcon className="h-3 w-3" />
              {project.status.charAt(0).toUpperCase() + project.status.slice(1)}
            </span>
          )}
        </div>
        <span className="text-sm text-slate-500 w-24">{project.updatedAt}</span>
      </div>
    </Link>
  );
}

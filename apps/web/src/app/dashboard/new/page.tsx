'use client';

import { useState, useRef, useCallback } from 'react';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { Code2, ArrowLeft, Upload, ArrowRight, X, FileText, Loader2, AlertCircle } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001';

const sourceLanguages = [
  { id: 'cobol', name: 'COBOL', description: 'COBOL-85, IBM Enterprise, Micro Focus', extensions: ['.cbl', '.cob', '.cpy'] },
  { id: 'fortran', name: 'Fortran', description: 'F77, F90, F95', extensions: ['.f', '.f90', '.f95', '.for'] },
  { id: 'vb6', name: 'Visual Basic 6', description: 'VB6, VBA', extensions: ['.bas', '.cls', '.frm', '.vba'] },
  { id: 'java-legacy', name: 'Legacy Java', description: 'J2EE, EJB 2.x', extensions: ['.java'] },
];

const targetLanguages = [
  { id: 'java', name: 'Java', frameworks: ['Spring Boot', 'Quarkus', 'Micronaut'] },
  { id: 'python', name: 'Python', frameworks: ['FastAPI', 'Django', 'Flask'] },
  { id: 'typescript', name: 'TypeScript', frameworks: ['NestJS', 'Express', 'Fastify'] },
  { id: 'go', name: 'Go', frameworks: ['Standard Library', 'Gin', 'Echo'] },
  { id: 'csharp', name: 'C#', frameworks: ['.NET Core', 'ASP.NET Core'] },
];

interface FileWithContent {
  name: string;
  content: string;
  size: number;
}

export default function NewProjectPage() {
  const router = useRouter();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [step, setStep] = useState(1);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [formData, setFormData] = useState({
    name: '',
    description: '',
    sourceLanguage: '',
    targetLanguage: '',
    targetFramework: '',
    files: [] as FileWithContent[],
    settings: {
      generateTests: true,
      generateDocumentation: true,
      humanReviewRequired: true,
      enableStranglerFig: false,
    },
  });

  const handleFileSelect = useCallback(async (selectedFiles: FileList | null) => {
    if (!selectedFiles) return;
    
    const newFiles: FileWithContent[] = [];
    const selectedLang = sourceLanguages.find(l => l.id === formData.sourceLanguage);
    const validExtensions = selectedLang?.extensions || [];
    
    for (const file of Array.from(selectedFiles)) {
      const ext = '.' + file.name.split('.').pop()?.toLowerCase();
      if (validExtensions.length === 0 || validExtensions.includes(ext)) {
        const content = await file.text();
        newFiles.push({ name: file.name, content, size: file.size });
      }
    }
    
    setFormData(prev => ({
      ...prev,
      files: [...prev.files, ...newFiles],
    }));
  }, [formData.sourceLanguage]);

  const removeFile = (index: number) => {
    setFormData(prev => ({
      ...prev,
      files: prev.files.filter((_, i) => i !== index),
    }));
  };

  const handleDrop = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    handleFileSelect(e.dataTransfer.files);
  }, [handleFileSelect]);

  const handleSubmit = async () => {
    setIsSubmitting(true);
    setError(null);
    
    try {
      // Step 1: Create the project
      const createResponse = await fetch(`${API_BASE_URL}/api/projects`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          name: formData.name,
          description: formData.description,
          sourceLanguage: formData.sourceLanguage,
          targetLanguage: formData.targetLanguage,
          targetFramework: formData.targetFramework,
          settings: formData.settings,
        }),
      });

      if (!createResponse.ok) {
        const errorData = await createResponse.json();
        throw new Error(errorData.error || 'Failed to create project');
      }

      const { data: project } = await createResponse.json();

      // Step 2: Upload files if any
      if (formData.files.length > 0) {
        const filesPayload = formData.files.map(f => ({
          filename: f.name,
          content: f.content,
        }));

        const uploadResponse = await fetch(`${API_BASE_URL}/api/projects/${project.id}/files`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(filesPayload),
        });

        if (!uploadResponse.ok) {
          console.warn('File upload failed, but project was created');
        }
      }

      // Navigate to the new project
      router.push(`/projects/${project.id}`);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
      setIsSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-900">
      {/* Header */}
      <header className="bg-white dark:bg-slate-800 border-b border-slate-200 dark:border-slate-700">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center gap-4">
            <Link
              href="/dashboard"
              className="text-slate-500 hover:text-slate-700 dark:hover:text-slate-300"
            >
              <ArrowLeft className="h-5 w-5" />
            </Link>
            <div className="flex items-center gap-2">
              <Code2 className="h-6 w-6 text-blue-600" />
              <span className="font-bold dark:text-white">New Migration Project</span>
            </div>
          </div>
        </div>
      </header>

      {/* Progress */}
      <div className="bg-white dark:bg-slate-800 border-b border-slate-200 dark:border-slate-700">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center justify-between max-w-2xl mx-auto">
            {['Project Info', 'Source', 'Target', 'Settings'].map((label, i) => (
              <div key={label} className="flex items-center">
                <div
                  className={`w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium ${
                    step > i + 1
                      ? 'bg-blue-600 text-white'
                      : step === i + 1
                      ? 'bg-blue-600 text-white'
                      : 'bg-slate-200 dark:bg-slate-700 text-slate-500'
                  }`}
                >
                  {i + 1}
                </div>
                <span
                  className={`ml-2 text-sm ${
                    step === i + 1
                      ? 'text-blue-600 font-medium'
                      : 'text-slate-500'
                  }`}
                >
                  {label}
                </span>
                {i < 3 && (
                  <div className="w-12 h-0.5 mx-4 bg-slate-200 dark:bg-slate-700" />
                )}
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Form */}
      <div className="container mx-auto px-4 py-8">
        <div className="max-w-2xl mx-auto">
          {step === 1 && (
            <div className="bg-white dark:bg-slate-800 rounded-xl p-6 border border-slate-200 dark:border-slate-700">
              <h2 className="text-xl font-semibold mb-6 dark:text-white">
                Project Information
              </h2>
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-slate-700 dark:text-slate-300 mb-1">
                    Project Name
                  </label>
                  <input
                    type="text"
                    value={formData.name}
                    onChange={(e) =>
                      setFormData({ ...formData, name: e.target.value })
                    }
                    className="w-full px-3 py-2 border border-slate-300 dark:border-slate-600 rounded-lg focus:ring-2 focus:ring-blue-500 dark:bg-slate-700 dark:text-white"
                    placeholder="e.g., Loan Processing System Migration"
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-slate-700 dark:text-slate-300 mb-1">
                    Description (optional)
                  </label>
                  <textarea
                    value={formData.description}
                    onChange={(e) =>
                      setFormData({ ...formData, description: e.target.value })
                    }
                    className="w-full px-3 py-2 border border-slate-300 dark:border-slate-600 rounded-lg focus:ring-2 focus:ring-blue-500 dark:bg-slate-700 dark:text-white"
                    rows={3}
                    placeholder="Brief description of the project..."
                  />
                </div>
              </div>
            </div>
          )}

          {step === 2 && (
            <div className="bg-white dark:bg-slate-800 rounded-xl p-6 border border-slate-200 dark:border-slate-700">
              <h2 className="text-xl font-semibold mb-6 dark:text-white">
                Source Language
              </h2>
              <div className="grid grid-cols-2 gap-4">
                {sourceLanguages.map((lang) => (
                  <button
                    key={lang.id}
                    onClick={() =>
                      setFormData({ ...formData, sourceLanguage: lang.id })
                    }
                    className={`p-4 rounded-lg border-2 text-left transition-all ${
                      formData.sourceLanguage === lang.id
                        ? 'border-blue-600 bg-blue-50 dark:bg-blue-900/20'
                        : 'border-slate-200 dark:border-slate-700 hover:border-slate-300'
                    }`}
                  >
                    <h3 className="font-medium dark:text-white">{lang.name}</h3>
                    <p className="text-sm text-slate-500">{lang.description}</p>
                  </button>
                ))}
              </div>

              <div className="mt-6">
                <label className="block text-sm font-medium text-slate-700 dark:text-slate-300 mb-2">
                  Upload Source Files
                </label>
                <input
                  ref={fileInputRef}
                  type="file"
                  multiple
                  className="hidden"
                  accept={sourceLanguages.find(l => l.id === formData.sourceLanguage)?.extensions.join(',')}
                  onChange={(e) => handleFileSelect(e.target.files)}
                />
                <div 
                  className="border-2 border-dashed border-slate-300 dark:border-slate-600 rounded-lg p-8 text-center cursor-pointer hover:border-blue-400 transition-colors"
                  onClick={() => fileInputRef.current?.click()}
                  onDrop={handleDrop}
                  onDragOver={(e) => e.preventDefault()}
                >
                  <Upload className="h-10 w-10 text-slate-400 mx-auto mb-4" />
                  <p className="text-slate-600 dark:text-slate-400 mb-2">
                    Drag and drop files here, or click to browse
                  </p>
                  <p className="text-sm text-slate-500">
                    Supported: {sourceLanguages.find(l => l.id === formData.sourceLanguage)?.extensions.join(', ') || '.cbl, .cob, .cpy, .f, .f90, .bas, .cls, .frm'}
                  </p>
                </div>
                
                {/* File list */}
                {formData.files.length > 0 && (
                  <div className="mt-4 space-y-2">
                    <p className="text-sm font-medium text-slate-700 dark:text-slate-300">
                      {formData.files.length} file(s) selected
                    </p>
                    {formData.files.map((file, index) => (
                      <div
                        key={`${file.name}-${index}`}
                        className="flex items-center justify-between p-3 bg-slate-50 dark:bg-slate-700 rounded-lg"
                      >
                        <div className="flex items-center gap-3">
                          <FileText className="h-5 w-5 text-slate-400" />
                          <div>
                            <p className="text-sm font-medium dark:text-white">{file.name}</p>
                            <p className="text-xs text-slate-500">
                              {(file.size / 1024).toFixed(1)} KB • {file.content.split('\n').length} lines
                            </p>
                          </div>
                        </div>
                        <button
                          onClick={(e) => {
                            e.stopPropagation();
                            removeFile(index);
                          }}
                          className="p-1 hover:bg-slate-200 dark:hover:bg-slate-600 rounded"
                        >
                          <X className="h-4 w-4 text-slate-500" />
                        </button>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </div>
          )}

          {step === 3 && (
            <div className="bg-white dark:bg-slate-800 rounded-xl p-6 border border-slate-200 dark:border-slate-700">
              <h2 className="text-xl font-semibold mb-6 dark:text-white">
                Target Language & Framework
              </h2>
              <div className="space-y-6">
                <div className="grid grid-cols-2 gap-4">
                  {targetLanguages.map((lang) => (
                    <button
                      key={lang.id}
                      onClick={() =>
                        setFormData({
                          ...formData,
                          targetLanguage: lang.id,
                          targetFramework: lang.frameworks[0]!,
                        })
                      }
                      className={`p-4 rounded-lg border-2 text-left transition-all ${
                        formData.targetLanguage === lang.id
                          ? 'border-blue-600 bg-blue-50 dark:bg-blue-900/20'
                          : 'border-slate-200 dark:border-slate-700 hover:border-slate-300'
                      }`}
                    >
                      <h3 className="font-medium dark:text-white">{lang.name}</h3>
                      <p className="text-sm text-slate-500">
                        {lang.frameworks.join(', ')}
                      </p>
                    </button>
                  ))}
                </div>

                {formData.targetLanguage && (
                  <div>
                    <label className="block text-sm font-medium text-slate-700 dark:text-slate-300 mb-2">
                      Framework
                    </label>
                    <select
                      value={formData.targetFramework}
                      onChange={(e) =>
                        setFormData({ ...formData, targetFramework: e.target.value })
                      }
                      className="w-full px-3 py-2 border border-slate-300 dark:border-slate-600 rounded-lg focus:ring-2 focus:ring-blue-500 dark:bg-slate-700 dark:text-white"
                    >
                      {targetLanguages
                        .find((l) => l.id === formData.targetLanguage)
                        ?.frameworks.map((fw) => (
                          <option key={fw} value={fw}>
                            {fw}
                          </option>
                        ))}
                    </select>
                  </div>
                )}
              </div>
            </div>
          )}

          {step === 4 && (
            <div className="bg-white dark:bg-slate-800 rounded-xl p-6 border border-slate-200 dark:border-slate-700">
              <h2 className="text-xl font-semibold mb-6 dark:text-white">
                Migration Settings
              </h2>
              <div className="space-y-4">
                <SettingToggle
                  label="Generate Unit Tests"
                  description="Automatically generate test cases for migrated code"
                  checked={formData.settings.generateTests}
                  onChange={(v) =>
                    setFormData({
                      ...formData,
                      settings: { ...formData.settings, generateTests: v },
                    })
                  }
                />
                <SettingToggle
                  label="Generate Documentation"
                  description="Create documentation for business rules and APIs"
                  checked={formData.settings.generateDocumentation}
                  onChange={(v) =>
                    setFormData({
                      ...formData,
                      settings: { ...formData.settings, generateDocumentation: v },
                    })
                  }
                />
                <SettingToggle
                  label="Human Review Required"
                  description="Require SME approval for low-confidence rules"
                  checked={formData.settings.humanReviewRequired}
                  onChange={(v) =>
                    setFormData({
                      ...formData,
                      settings: { ...formData.settings, humanReviewRequired: v },
                    })
                  }
                />
                <SettingToggle
                  label="Enable Strangler Fig Pattern"
                  description="Generate facade for incremental migration"
                  checked={formData.settings.enableStranglerFig}
                  onChange={(v) =>
                    setFormData({
                      ...formData,
                      settings: { ...formData.settings, enableStranglerFig: v },
                    })
                  }
                />
              </div>
            </div>
          )}

          {/* Error message */}
          {error && (
            <div className="mt-4 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg flex items-start gap-3">
              <AlertCircle className="h-5 w-5 text-red-600 dark:text-red-400 flex-shrink-0 mt-0.5" />
              <div>
                <p className="text-red-800 dark:text-red-200 font-medium">Error creating project</p>
                <p className="text-red-600 dark:text-red-400 text-sm">{error}</p>
              </div>
            </div>
          )}

          {/* Navigation */}
          <div className="flex items-center justify-between mt-6">
            {step > 1 ? (
              <button
                onClick={() => setStep(step - 1)}
                disabled={isSubmitting}
                className="px-4 py-2 text-slate-600 dark:text-slate-300 hover:text-slate-800 dark:hover:text-white disabled:opacity-50"
              >
                Back
              </button>
            ) : (
              <div />
            )}
            {step < 4 ? (
              <button
                onClick={() => setStep(step + 1)}
                disabled={
                  (step === 1 && !formData.name) ||
                  (step === 2 && !formData.sourceLanguage) ||
                  (step === 3 && !formData.targetLanguage)
                }
                className="bg-blue-600 hover:bg-blue-700 disabled:bg-slate-300 disabled:cursor-not-allowed text-white px-4 py-2 rounded-lg font-medium flex items-center gap-2"
              >
                Continue <ArrowRight className="h-4 w-4" />
              </button>
            ) : (
              <button
                onClick={handleSubmit}
                disabled={isSubmitting}
                className="bg-blue-600 hover:bg-blue-700 disabled:bg-blue-400 text-white px-6 py-2 rounded-lg font-medium flex items-center gap-2"
              >
                {isSubmitting ? (
                  <>
                    <Loader2 className="h-4 w-4 animate-spin" />
                    Creating...
                  </>
                ) : (
                  'Create Project'
                )}
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

function SettingToggle({
  label,
  description,
  checked,
  onChange,
}: {
  label: string;
  description: string;
  checked: boolean;
  onChange: (value: boolean) => void;
}) {
  return (
    <label className="flex items-start gap-3 p-4 rounded-lg border border-slate-200 dark:border-slate-700 cursor-pointer hover:bg-slate-50 dark:hover:bg-slate-700/50">
      <input
        type="checkbox"
        checked={checked}
        onChange={(e) => onChange(e.target.checked)}
        className="mt-1 h-4 w-4 rounded border-slate-300 text-blue-600 focus:ring-blue-500"
      />
      <div>
        <p className="font-medium dark:text-white">{label}</p>
        <p className="text-sm text-slate-500">{description}</p>
      </div>
    </label>
  );
}

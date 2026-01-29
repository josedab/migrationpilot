'use client';

/**
 * Equivalence Tests Page
 * 
 * Shows validation test results comparing legacy and modern systems
 */

import { useState } from 'react';

interface TestResult {
  id: string;
  name: string;
  category: string;
  status: 'passed' | 'failed' | 'skipped';
  legacyOutput: string;
  modernOutput: string;
  difference?: string;
  duration: number;
  inputs: Record<string, string>;
}

const mockResults: TestResult[] = [
  {
    id: 'T001',
    name: 'Interest Calculation - Standard Rate',
    category: 'Calculation',
    status: 'passed',
    legacyOutput: '125.50',
    modernOutput: '125.50',
    duration: 45,
    inputs: { principal: '10000', rate: '5', years: '3' },
  },
  {
    id: 'T002',
    name: 'Interest Calculation - Zero Balance',
    category: 'Boundary',
    status: 'passed',
    legacyOutput: '0.00',
    modernOutput: '0.00',
    duration: 32,
    inputs: { principal: '0', rate: '5', years: '3' },
  },
  {
    id: 'T003',
    name: 'Account Validation - Negative Balance',
    category: 'Validation',
    status: 'passed',
    legacyOutput: 'ERROR: INVALID-BALANCE',
    modernOutput: 'ERROR: INVALID-BALANCE',
    duration: 28,
    inputs: { balance: '-100' },
  },
  {
    id: 'T004',
    name: 'Date Parsing - Leap Year',
    category: 'Edge Case',
    status: 'failed',
    legacyOutput: '02/29/2000',
    modernOutput: '2000-02-29',
    difference: 'Date format mismatch',
    duration: 55,
    inputs: { date: '29-FEB-2000' },
  },
  {
    id: 'T005',
    name: 'Integer Overflow Handling',
    category: 'Boundary',
    status: 'failed',
    legacyOutput: '2147483647',
    modernOutput: 'ArithmeticException',
    difference: 'Legacy silently caps value, modern throws exception',
    duration: 41,
    inputs: { value: '9999999999' },
  },
  {
    id: 'T006',
    name: 'Loan Eligibility - High Credit Score',
    category: 'Decision',
    status: 'passed',
    legacyOutput: 'ELIGIBLE',
    modernOutput: 'ELIGIBLE',
    duration: 67,
    inputs: { creditScore: '750', income: '100000', debt: '20000' },
  },
  {
    id: 'T007',
    name: 'Loan Eligibility - Low Credit Score',
    category: 'Decision',
    status: 'passed',
    legacyOutput: 'NOT-ELIGIBLE',
    modernOutput: 'NOT-ELIGIBLE',
    duration: 62,
    inputs: { creditScore: '500', income: '50000', debt: '40000' },
  },
];

export default function TestsPage({ params }: { params: { id: string } }) {
  const [results] = useState(mockResults);
  const [filter, setFilter] = useState<'all' | 'passed' | 'failed'>('all');
  const [selectedTest, setSelectedTest] = useState<TestResult | null>(null);
  const [categoryFilter, setCategoryFilter] = useState<string>('all');

  const categories = ['all', ...new Set(results.map(r => r.category))];
  
  const filteredResults = results.filter(r => {
    if (filter !== 'all' && r.status !== filter) return false;
    if (categoryFilter !== 'all' && r.category !== categoryFilter) return false;
    return true;
  });

  const summary = {
    total: results.length,
    passed: results.filter(r => r.status === 'passed').length,
    failed: results.filter(r => r.status === 'failed').length,
    skipped: results.filter(r => r.status === 'skipped').length,
  };

  const passRate = summary.total > 0 
    ? Math.round((summary.passed / summary.total) * 100) 
    : 0;

  const confidenceColor = passRate >= 95 ? 'text-green-600' : passRate >= 80 ? 'text-yellow-600' : 'text-red-600';

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 py-8">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">Equivalence Test Results</h1>
          <p className="text-gray-600 mt-2">
            Comparing legacy and modern system outputs for Project {params.id}
          </p>
        </div>

        {/* Summary Cards */}
        <div className="grid grid-cols-5 gap-4 mb-8">
          <div className="bg-white p-6 rounded-lg shadow-sm">
            <div className="text-3xl font-bold">{summary.total}</div>
            <div className="text-sm text-gray-500">Total Tests</div>
          </div>
          <div className="bg-white p-6 rounded-lg shadow-sm">
            <div className="text-3xl font-bold text-green-600">{summary.passed}</div>
            <div className="text-sm text-gray-500">Passed</div>
          </div>
          <div className="bg-white p-6 rounded-lg shadow-sm">
            <div className="text-3xl font-bold text-red-600">{summary.failed}</div>
            <div className="text-sm text-gray-500">Failed</div>
          </div>
          <div className="bg-white p-6 rounded-lg shadow-sm">
            <div className="text-3xl font-bold text-gray-400">{summary.skipped}</div>
            <div className="text-sm text-gray-500">Skipped</div>
          </div>
          <div className="bg-white p-6 rounded-lg shadow-sm">
            <div className={`text-3xl font-bold ${confidenceColor}`}>{passRate}%</div>
            <div className="text-sm text-gray-500">Confidence Score</div>
          </div>
        </div>

        {/* Progress Bar */}
        <div className="bg-white p-6 rounded-lg shadow-sm mb-8">
          <div className="flex items-center justify-between mb-2">
            <span className="text-sm font-medium">Test Execution Progress</span>
            <span className="text-sm text-gray-500">100% Complete</span>
          </div>
          <div className="flex h-4 rounded-full overflow-hidden">
            <div 
              className="bg-green-500" 
              style={{ width: `${(summary.passed / summary.total) * 100}%` }}
            />
            <div 
              className="bg-red-500" 
              style={{ width: `${(summary.failed / summary.total) * 100}%` }}
            />
            <div 
              className="bg-gray-300" 
              style={{ width: `${(summary.skipped / summary.total) * 100}%` }}
            />
          </div>
          <div className="flex gap-4 mt-2 text-xs text-gray-500">
            <span className="flex items-center gap-1">
              <span className="w-3 h-3 bg-green-500 rounded" /> Passed
            </span>
            <span className="flex items-center gap-1">
              <span className="w-3 h-3 bg-red-500 rounded" /> Failed
            </span>
            <span className="flex items-center gap-1">
              <span className="w-3 h-3 bg-gray-300 rounded" /> Skipped
            </span>
          </div>
        </div>

        <div className="grid grid-cols-3 gap-6">
          {/* Test List */}
          <div className="col-span-2">
            <div className="bg-white rounded-lg shadow-sm">
              <div className="p-4 border-b">
                <div className="flex items-center justify-between">
                  <h2 className="font-semibold">Test Results</h2>
                  <div className="flex gap-2">
                    {(['all', 'passed', 'failed'] as const).map((f) => (
                      <button
                        key={f}
                        onClick={() => setFilter(f)}
                        className={`px-3 py-1 text-xs rounded-full transition-colors ${
                          filter === f
                            ? 'bg-blue-600 text-white'
                            : 'bg-gray-100 hover:bg-gray-200'
                        }`}
                      >
                        {f.charAt(0).toUpperCase() + f.slice(1)}
                      </button>
                    ))}
                  </div>
                </div>
                <div className="flex gap-2 mt-3">
                  {categories.map((cat) => (
                    <button
                      key={cat}
                      onClick={() => setCategoryFilter(cat)}
                      className={`px-2 py-1 text-xs rounded transition-colors ${
                        categoryFilter === cat
                          ? 'bg-gray-800 text-white'
                          : 'bg-gray-100 hover:bg-gray-200'
                      }`}
                    >
                      {cat === 'all' ? 'All Categories' : cat}
                    </button>
                  ))}
                </div>
              </div>
              
              <table className="w-full">
                <thead>
                  <tr className="text-left text-sm text-gray-500 border-b">
                    <th className="px-4 py-3 w-12">Status</th>
                    <th className="px-4 py-3">Test Name</th>
                    <th className="px-4 py-3">Category</th>
                    <th className="px-4 py-3 text-right">Duration</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredResults.map((result) => (
                    <tr
                      key={result.id}
                      onClick={() => setSelectedTest(result)}
                      className={`border-b cursor-pointer hover:bg-gray-50 ${
                        selectedTest?.id === result.id ? 'bg-blue-50' : ''
                      }`}
                    >
                      <td className="px-4 py-3">
                        <span className={`w-3 h-3 rounded-full inline-block ${
                          result.status === 'passed' ? 'bg-green-500' :
                          result.status === 'failed' ? 'bg-red-500' : 'bg-gray-400'
                        }`} />
                      </td>
                      <td className="px-4 py-3">
                        <div className="font-medium">{result.name}</div>
                        <div className="text-xs text-gray-500">{result.id}</div>
                      </td>
                      <td className="px-4 py-3">
                        <span className="px-2 py-1 bg-gray-100 rounded text-xs">
                          {result.category}
                        </span>
                      </td>
                      <td className="px-4 py-3 text-right text-sm text-gray-500">
                        {result.duration}ms
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          {/* Test Details */}
          <div className="col-span-1">
            {selectedTest ? (
              <div className="bg-white rounded-lg shadow-sm sticky top-8">
                <div className="p-4 border-b">
                  <div className="flex items-center gap-2">
                    <span className={`w-3 h-3 rounded-full ${
                      selectedTest.status === 'passed' ? 'bg-green-500' : 'bg-red-500'
                    }`} />
                    <h3 className="font-semibold">{selectedTest.name}</h3>
                  </div>
                  <div className="text-sm text-gray-500 mt-1">
                    {selectedTest.id} • {selectedTest.category}
                  </div>
                </div>

                <div className="p-4 space-y-4">
                  {/* Inputs */}
                  <div>
                    <h4 className="text-sm font-medium text-gray-700 mb-2">Test Inputs</h4>
                    <div className="bg-gray-50 p-3 rounded text-sm font-mono space-y-1">
                      {Object.entries(selectedTest.inputs).map(([key, value]) => (
                        <div key={key}>
                          <span className="text-gray-500">{key}:</span> {value}
                        </div>
                      ))}
                    </div>
                  </div>

                  {/* Legacy Output */}
                  <div>
                    <h4 className="text-sm font-medium text-gray-700 mb-2">Legacy Output</h4>
                    <div className="bg-slate-900 text-slate-100 p-3 rounded font-mono text-sm">
                      {selectedTest.legacyOutput}
                    </div>
                  </div>

                  {/* Modern Output */}
                  <div>
                    <h4 className="text-sm font-medium text-gray-700 mb-2">Modern Output</h4>
                    <div className={`p-3 rounded font-mono text-sm ${
                      selectedTest.status === 'passed'
                        ? 'bg-green-900 text-green-100'
                        : 'bg-red-900 text-red-100'
                    }`}>
                      {selectedTest.modernOutput}
                    </div>
                  </div>

                  {/* Difference (if failed) */}
                  {selectedTest.status === 'failed' && selectedTest.difference && (
                    <div>
                      <h4 className="text-sm font-medium text-red-700 mb-2">Difference</h4>
                      <div className="bg-red-50 border border-red-200 text-red-700 p-3 rounded text-sm">
                        {selectedTest.difference}
                      </div>
                    </div>
                  )}

                  {/* Actions */}
                  <div className="pt-4 border-t space-y-2">
                    <button className="w-full px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
                      Re-run Test
                    </button>
                    {selectedTest.status === 'failed' && (
                      <button className="w-full px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50">
                        Create Issue
                      </button>
                    )}
                  </div>
                </div>
              </div>
            ) : (
              <div className="bg-white rounded-lg shadow-sm p-8 text-center text-gray-500">
                <div className="text-4xl mb-4">🧪</div>
                <p>Select a test to view details</p>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

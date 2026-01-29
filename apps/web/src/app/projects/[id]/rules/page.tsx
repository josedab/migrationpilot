'use client';

/**
 * Business Rules Review Page
 * 
 * Enhanced SME collaboration interface for reviewing extracted business rules
 */

import { useState } from 'react';
import { 
  RuleReviewDashboard, 
  SMEActivityFeed, 
  ConfidenceAlerts,
  type BusinessRule,
  type SMEActivity 
} from '@/components/rules';

// Mock data for demonstration
const mockRules: BusinessRule[] = [
  {
    id: 'BR-001',
    name: 'Interest Rate Calculation',
    description: 'Calculates the interest rate based on account type, balance tier, and customer loyalty status. The rate increases by 0.5% for savings accounts with balances over $10,000.',
    category: 'Financial Calculation',
    confidence: 0.95,
    status: 'pending',
    inputs: [
      { name: 'accountType', type: 'String', description: 'Type of account (SAVINGS, CHECKING, etc.)' },
      { name: 'balance', type: 'BigDecimal', description: 'Current account balance' },
      { name: 'baseRate', type: 'BigDecimal', description: 'Base interest rate' },
    ],
    outputs: [
      { name: 'rate', type: 'BigDecimal', description: 'Calculated interest rate' },
    ],
    logic: {
      calculation: `IF ACCT-TYPE = "SAVINGS"
  IF BALANCE > 10000
    RATE = BASE-RATE + 0.5
  ELSE
    RATE = BASE-RATE
END-IF`,
      conditions: ['Account type must be SAVINGS for bonus rate', 'Balance must exceed $10,000'],
      edgeCases: ['Zero balance accounts', 'Negative balance handling'],
    },
    sourceLocation: {
      file: 'CALCINT.cbl',
      startLine: 245,
      endLine: 280,
    },
    comments: [],
  },
  {
    id: 'BR-002',
    name: 'Account Balance Validation',
    description: 'Validates that account balance is non-negative and within allowed limits for the account type.',
    category: 'Data Validation',
    confidence: 0.88,
    status: 'pending',
    inputs: [
      { name: 'balance', type: 'BigDecimal', description: 'Balance to validate' },
      { name: 'maxBalance', type: 'BigDecimal', description: 'Maximum allowed balance' },
    ],
    outputs: [
      { name: 'isValid', type: 'Boolean', description: 'Validation result' },
      { name: 'errorMessage', type: 'String', description: 'Error message if invalid' },
    ],
    logic: {
      calculation: `IF BALANCE < 0
  SET VALIDATION-ERROR TO TRUE
IF BALANCE > MAX-BALANCE
  SET VALIDATION-ERROR TO TRUE`,
      conditions: ['Balance >= 0', 'Balance <= MAX_BALANCE'],
    },
    sourceLocation: {
      file: 'ACCTVAL.cbl',
      startLine: 120,
      endLine: 145,
    },
    comments: [
      {
        id: 'c1',
        userId: 'user_123',
        userName: 'John Smith (SME)',
        content: 'The MAX-BALANCE limit should be configurable per account type, not a global constant.',
        type: 'suggestion',
        createdAt: '2024-01-28T10:30:00Z',
      },
    ],
  },
  {
    id: 'BR-003',
    name: 'Loan Eligibility Decision',
    description: 'Determines loan eligibility based on credit score, income, and existing debt-to-income ratio.',
    category: 'Business Decision',
    confidence: 0.72,
    status: 'pending',
    inputs: [
      { name: 'creditScore', type: 'Integer', description: 'Applicant credit score' },
      { name: 'income', type: 'BigDecimal', description: 'Annual income' },
      { name: 'existingDebt', type: 'BigDecimal', description: 'Current debt obligations' },
    ],
    outputs: [
      { name: 'eligible', type: 'Boolean', description: 'Eligibility determination' },
      { name: 'maxLoanAmount', type: 'BigDecimal', description: 'Maximum loan amount if eligible' },
    ],
    logic: {
      calculation: `DEBT-RATIO = EXISTING-DEBT / INCOME
IF CREDIT-SCORE >= 700 AND DEBT-RATIO < 0.4
  SET ELIGIBLE TO TRUE
  MAX-LOAN = INCOME * 3`,
      conditions: ['Credit score >= 700', 'Debt-to-income ratio < 40%'],
      edgeCases: ['Zero income applicants', 'Very high income outliers'],
    },
    sourceLocation: {
      file: 'LOANELIG.cbl',
      startLine: 450,
      endLine: 520,
    },
    comments: [
      {
        id: 'c2',
        userId: 'user_456',
        userName: 'Mary Johnson (Finance)',
        content: 'The credit score threshold varies by loan product. We need to parameterize this.',
        type: 'correction',
        createdAt: '2024-01-27T14:15:00Z',
      },
      {
        id: 'c3',
        userId: 'user_789',
        userName: 'Risk Team',
        content: 'What happens for applicants with no credit history?',
        type: 'question',
        createdAt: '2024-01-27T16:00:00Z',
      },
    ],
  },
  {
    id: 'BR-004',
    name: 'Fiscal Year End Processing',
    description: 'Handles special processing rules for fiscal year end dates including interest accrual and account reconciliation.',
    category: 'Temporal Processing',
    confidence: 0.45,
    status: 'pending',
    inputs: [
      { name: 'currentDate', type: 'Date', description: 'Current processing date' },
      { name: 'fiscalYearEnd', type: 'Date', description: 'Fiscal year end date' },
    ],
    outputs: [
      { name: 'isFiscalYearEnd', type: 'Boolean', description: 'Is it fiscal year end' },
      { name: 'processingFlags', type: 'Object', description: 'Special processing flags' },
    ],
    logic: {
      conditions: ['Check if current date equals fiscal year end', 'Apply special accrual rules'],
      edgeCases: ['Leap year handling', 'Weekend fiscal year ends'],
    },
    sourceLocation: {
      file: 'FYEND.cbl',
      startLine: 80,
      endLine: 110,
    },
    comments: [
      {
        id: 'c4',
        userId: 'user_456',
        userName: 'Mary Johnson (Finance)',
        content: 'This rule needs to account for leap years in the fiscal calendar. The original code has a known Y2K-era bug.',
        type: 'correction',
        createdAt: '2024-01-27T14:15:00Z',
      },
    ],
  },
  {
    id: 'BR-005',
    name: 'Transaction Fee Calculation',
    description: 'Calculates transaction fees based on transaction type, amount, and account tier.',
    category: 'Financial Calculation',
    confidence: 0.91,
    status: 'approved',
    inputs: [
      { name: 'transactionType', type: 'String', description: 'Type of transaction' },
      { name: 'amount', type: 'BigDecimal', description: 'Transaction amount' },
      { name: 'accountTier', type: 'String', description: 'Customer account tier' },
    ],
    outputs: [
      { name: 'fee', type: 'BigDecimal', description: 'Calculated fee' },
    ],
    logic: {
      calculation: `FEE = AMOUNT * BASE-FEE-RATE
IF ACCOUNT-TIER = "PREMIUM"
  FEE = FEE * 0.5
IF AMOUNT > 10000
  FEE = FEE + LARGE-TXN-FEE`,
    },
    sourceLocation: {
      file: 'TXNFEE.cbl',
      startLine: 200,
      endLine: 235,
    },
    reviewedBy: 'John Smith',
    reviewedAt: '2024-01-26T09:00:00Z',
    comments: [],
  },
];

const mockActivities: SMEActivity[] = [
  {
    id: 'act-1',
    type: 'approval',
    userId: 'user_123',
    userName: 'John Smith',
    ruleId: 'BR-005',
    ruleName: 'Transaction Fee Calculation',
    description: 'Approved rule after verifying fee calculation logic matches production behavior.',
    timestamp: '2024-01-26T09:00:00Z',
  },
  {
    id: 'act-2',
    type: 'comment',
    userId: 'user_456',
    userName: 'Mary Johnson',
    ruleId: 'BR-004',
    ruleName: 'Fiscal Year End Processing',
    description: 'Identified leap year handling bug in original code.',
    metadata: { commentType: 'correction' },
    timestamp: '2024-01-27T14:15:00Z',
  },
  {
    id: 'act-3',
    type: 'comment',
    userId: 'user_123',
    userName: 'John Smith',
    ruleId: 'BR-002',
    ruleName: 'Account Balance Validation',
    description: 'Suggested making MAX_BALANCE configurable per account type.',
    metadata: { commentType: 'suggestion' },
    timestamp: '2024-01-28T10:30:00Z',
  },
  {
    id: 'act-4',
    type: 'comment',
    userId: 'user_789',
    userName: 'Risk Team',
    ruleId: 'BR-003',
    ruleName: 'Loan Eligibility Decision',
    description: 'Asked about handling applicants with no credit history.',
    metadata: { commentType: 'question' },
    timestamp: '2024-01-27T16:00:00Z',
  },
];

export default function RulesPage({ params }: { params: { id: string } }) {
  const [rules, setRules] = useState<BusinessRule[]>(mockRules);
  const [activities, setActivities] = useState<SMEActivity[]>(mockActivities);
  const [viewMode, setViewMode] = useState<'dashboard' | 'activity'>('dashboard');

  const handleRulesUpdate = (updatedRules: BusinessRule[]) => {
    setRules(updatedRules);
    
    // Add activity for changes (simplified - in production, compare diffs)
    const newActivity: SMEActivity = {
      id: `act-${Date.now()}`,
      type: 'edit',
      userId: 'current_user',
      userName: 'Current User',
      ruleId: updatedRules[0]?.id || 'unknown',
      ruleName: updatedRules[0]?.name || 'Unknown',
      description: 'Updated rule',
      timestamp: new Date().toISOString(),
    };
    setActivities([newActivity, ...activities]);
  };

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Page Header */}
      <div className="bg-white border-b shadow-sm">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-2xl font-bold text-gray-900">
                📋 Business Rules Review
              </h1>
              <p className="text-gray-500">
                Project {params.id} • SME Collaboration Portal
              </p>
            </div>
            <div className="flex items-center gap-2">
              <button
                onClick={() => setViewMode('dashboard')}
                className={`px-4 py-2 rounded-lg ${
                  viewMode === 'dashboard'
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                }`}
              >
                📊 Dashboard
              </button>
              <button
                onClick={() => setViewMode('activity')}
                className={`px-4 py-2 rounded-lg ${
                  viewMode === 'activity'
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                }`}
              >
                👥 Activity
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* Confidence Alerts */}
      <div className="max-w-7xl mx-auto px-4 py-4">
        <ConfidenceAlerts
          rules={rules}
          confidenceThreshold={0.85}
          criticalThreshold={0.5}
        />
      </div>

      {/* Main Content */}
      {viewMode === 'dashboard' ? (
        <RuleReviewDashboard
          rules={rules}
          projectId={params.id}
          projectName={`Project ${params.id}`}
          confidenceThreshold={0.85}
          currentUserId="current_user"
          currentUserName="Current User"
          onRulesUpdate={handleRulesUpdate}
        />
      ) : (
        <div className="max-w-3xl mx-auto px-4 py-6">
          <SMEActivityFeed
            activities={activities}
            maxItems={50}
          />
        </div>
      )}
    </div>
  );
}

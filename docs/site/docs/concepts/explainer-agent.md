---
sidebar_position: 5
---

# Explainer Agent

The Explainer Agent provides natural language explanations of legacy code, enabling interactive Q&A sessions that help SMEs and developers understand complex legacy systems.

## Overview

While the Archeologist extracts business rules programmatically, the Explainer focuses on human understanding. It translates cryptic COBOL paragraphs, obscure Fortran subroutines, and tangled VB6 procedures into clear, accessible explanations.

```typescript
import { ExplainerAgent } from '@migrationpilot/agents';

const explainer = new ExplainerAgent();

// Explain an entire file
const explanation = await explainer.explainCode(
  context,
  cobolSource,
  'cobol',
  'PAYROLL.cbl'
);

console.log(explanation.data.summary);
// "This program calculates employee payroll, including overtime,
//  deductions, and tax withholding based on employee classification."
```

## Key Capabilities

### Full File Explanation

Get a comprehensive breakdown of what a legacy program does:

```typescript
const result = await explainer.explainCode(
  context,
  sourceCode,
  'cobol',
  'INVENTORY.cbl'
);

// Result includes:
// - summary: 1-2 sentence overview
// - detailedExplanation: Full walkthrough
// - businessPurpose: Why this code exists
// - keyOperations: Critical code sections with line numbers
// - dataFlow: How data moves through the program
// - edgeCases: Special handling and conditions
// - assumptions: Implicit requirements
// - confidence: 0.0-1.0 score
// - suggestedQuestions: Follow-up queries
```

### Section-Specific Explanations

Focus on a particular code block:

```typescript
const sectionExplanation = await explainer.explainSection(
  context,
  sourceCode,
  'cobol',
  'PAYROLL.cbl',
  150,  // startLine
  200   // endLine
);
```

The agent automatically includes surrounding context (10 lines before, 5 after) for better understanding.

### Interactive Q&A Sessions

Start a conversation about code that maintains context:

```typescript
// Start a session
const session = explainer.startSession(
  'session-123',
  cobolSource,
  'cobol',
  'CLAIMS.cbl'
);

// Ask questions
const answer1 = await explainer.askQuestion(
  context,
  'session-123',
  'What happens when the claim amount exceeds the policy limit?'
);

console.log(answer1.data.answer);
// "When the claim amount in WS-CLAIM-AMT exceeds WS-POLICY-LIMIT,
//  the program executes paragraph 3200-EXCESS-CLAIM which..."

console.log(answer1.data.suggestedFollowups);
// ["How is the excess amount calculated?",
//  "What notification is sent for excess claims?",
//  "Are there different rules for different policy types?"]

// Follow-up questions maintain context
const answer2 = await explainer.askQuestion(
  context,
  'session-123',
  'How is the excess amount calculated?'
);
```

### COBOL-Specific Features

#### Paragraph Explanation

```typescript
const paragraph = await explainer.explainParagraph(
  context,
  cobolSource,
  '3000-CALCULATE-INTEREST',
  'LOANS.cbl'
);

// Returns when/why paragraph is called, data it modifies, business purpose
```

#### Data Item Analysis

```typescript
const dataItem = await explainer.explainDataItem(
  context,
  cobolSource,
  'WS-ACCT-BALANCE',
  'cobol',
  'BANKING.cbl'
);

// Returns:
// - name, type (from PICTURE clause)
// - purpose in the program
// - where it's read/written with locations
// - related data items
// - business meaning
```

### Executive Summaries

Generate non-technical summaries for stakeholders:

```typescript
const summary = await explainer.generateExecutiveSummary(
  context,
  sourceCode,
  'cobol',
  'BILLING.cbl'
);

console.log(summary.data);
// {
//   title: "Customer Billing Processing System",
//   summary: "Processes monthly billing cycles for all active customers,
//             calculating charges based on usage and applying discounts.",
//   businessFunctions: [
//     "Calculate usage-based charges",
//     "Apply volume discounts",
//     "Generate billing statements",
//     "Update account balances"
//   ],
//   dataProcessed: ["Customer accounts", "Usage records", "Rate tables"],
//   integrations: ["CUSTMAST (Customer Master File)", "RATETBL (Rate Table)"],
//   risks: ["Complex discount logic may have undocumented edge cases"],
//   modernizationNotes: "Consider separating billing calculation from
//                        statement generation for better testability."
// }
```

## Explanation Output Structure

### ExplanationResult

```typescript
interface ExplanationResult {
  summary: string;           // 1-2 sentence overview
  detailedExplanation: string;  // Full walkthrough
  businessPurpose: string;   // Why the code exists
  keyOperations: KeyOperation[];
  dataFlow: DataFlowStep[];
  edgeCases: string[];
  assumptions: string[];
  confidence: number;        // 0.0-1.0
  suggestedQuestions: string[];
}

interface KeyOperation {
  name: string;
  description: string;
  location: { startLine: number; endLine: number };
  importance: 'critical' | 'important' | 'supporting';
}

interface DataFlowStep {
  step: number;
  description: string;
  inputs: string[];
  outputs: string[];
  transformation?: string;
}
```

## Use Cases

### Onboarding New Developers

Help developers unfamiliar with legacy systems get up to speed:

```typescript
// Generate explanations for all files in a module
for (const file of moduleFiles) {
  const explanation = await explainer.explainCode(context, file.content, 'cobol', file.name);
  await saveDocumentation(file.name, explanation.data);
}
```

### SME Knowledge Capture

Use Q&A sessions to capture institutional knowledge:

```typescript
const session = explainer.startSession('sme-session', code, 'cobol', 'LEGACY.cbl');

// SME asks clarifying questions
const q1 = await explainer.askQuestion(context, 'sme-session',
  'Why does this check for account type 7?');

// SME can add corrections: "Actually, type 7 is for VIP customers..."
// These corrections can feed back into business rule documentation
```

### Pre-Migration Assessment

Understand what you're migrating before you start:

```typescript
const assessment = await explainer.generateExecutiveSummary(
  context, code, 'cobol', 'MAINFRAME.cbl'
);

// Use for migration planning, risk assessment, stakeholder communication
```

### Code Review Support

Explain changes during migration review:

```typescript
// Explain what the original code does
const original = await explainer.explainSection(
  context, legacyCode, 'cobol', 'ORIGINAL.cbl', 100, 150
);

// Compare with migrated version explanation
// to verify behavior preservation
```

## Configuration

```typescript
const explainer = new ExplainerAgent();

// The agent uses these defaults:
// - model: 'gpt-4'
// - temperature: 0.3 (slightly creative for natural explanations)
// - maxTokens: 4000
```

## Integration with Other Agents

The Explainer works alongside other agents:

1. **Before Archeologist**: Help SMEs understand code before business rule extraction
2. **After Archeologist**: Explain extracted rules in context
3. **With Validator**: Explain test failures and discrepancies
4. **In Dashboard**: Provide on-demand explanations in the web UI

## API Reference

### explainCode

```typescript
async explainCode(
  context: AgentContext,
  sourceCode: string,
  language: SourceLanguage,
  filename: string
): Promise<AgentResponse<ExplanationResult>>
```

### explainSection

```typescript
async explainSection(
  context: AgentContext,
  sourceCode: string,
  language: SourceLanguage,
  filename: string,
  startLine: number,
  endLine: number
): Promise<AgentResponse<ExplanationResult>>
```

### startSession / askQuestion

```typescript
startSession(
  sessionId: string,
  code: string,
  language: SourceLanguage,
  filename: string
): QASession

async askQuestion(
  context: AgentContext,
  sessionId: string,
  question: string
): Promise<AgentResponse<{
  answer: string;
  relatedCode?: { startLine: number; endLine: number; snippet: string };
  confidence: number;
  suggestedFollowups: string[];
}>>
```

### explainParagraph (COBOL)

```typescript
async explainParagraph(
  context: AgentContext,
  sourceCode: string,
  paragraphName: string,
  filename: string
): Promise<AgentResponse<ExplanationResult>>
```

### explainDataItem

```typescript
async explainDataItem(
  context: AgentContext,
  sourceCode: string,
  dataItemName: string,
  language: SourceLanguage,
  filename: string
): Promise<AgentResponse<{
  name: string;
  type: string;
  purpose: string;
  usedIn: Array<{ location: string; usage: 'read' | 'write' | 'both' }>;
  relatedItems: string[];
  businessMeaning: string;
  confidence: number;
}>>
```

### generateExecutiveSummary

```typescript
async generateExecutiveSummary(
  context: AgentContext,
  sourceCode: string,
  language: SourceLanguage,
  filename: string
): Promise<AgentResponse<{
  title: string;
  summary: string;
  businessFunctions: string[];
  dataProcessed: string[];
  integrations: string[];
  risks: string[];
  modernizationNotes: string;
}>>
```

## Best Practices

1. **Use sessions for related questions**: Maintains context and provides better answers
2. **Start with executive summaries**: Get the big picture before diving into details
3. **Combine with Archeologist output**: Cross-reference explanations with extracted rules
4. **Save explanations**: Generated documentation is valuable for future reference
5. **Review confidence scores**: Lower scores indicate ambiguous code that needs SME input

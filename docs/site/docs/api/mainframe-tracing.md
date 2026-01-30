---
sidebar_position: 9
---

# Mainframe Tracing API

The Mainframe Tracing module provides live execution tracing capabilities for z/OS and other mainframe platforms, enabling capture of CICS transactions, DB2 operations, VSAM file access, and batch job execution.

## Overview

The Mainframe Tracing module includes:
- **MainframeTraceCapture**: Captures live execution traces from mainframe systems
- **TraceNormalizer**: Normalizes raw traces into a standard format
- **BehavioralInferenceEngine**: Infers business rules from trace data
- **SMEValidationWorkflow**: Manages SME review of inferred rules

## MainframeTraceCapture

Captures live execution traces from mainframe systems using various collection methods.

### Constructor

```typescript
import { MainframeTraceCapture } from '@migrationpilot/tracing';

const capture = new MainframeTraceCapture({
  projectId: 'my-project',
  platform: 'zos',              // 'zos' | 'iseries' | 'unisys' | 'tandem'
  traceSource: 'custom_agent',  // See Trace Sources below
  
  connection: {
    host: 'mainframe.example.com',
    port: 3270,
    protocol: 'agent',          // 'tn3270' | 'ssh' | 'mq' | 'rest' | 'agent'
    agentEndpoint: 'http://agent.internal:8080',
    connectionTimeoutMs: 30000,
    readTimeoutMs: 60000,
  },
  
  captureSettings: {
    captureCICS: true,
    captureDB2: true,
    captureIMS: false,
    captureBatch: true,
    captureVSAM: true,
    captureStatements: true,
    captureVariables: true,
    captureWorkingStorage: false,
    captureLinkageSection: false,
    samplingMode: 'all',        // 'all' | 'random' | 'time_based' | 'transaction_based'
    samplingRate: 1.0,
  },
  
  security: {
    authType: 'racf',           // 'racf' | 'acf2' | 'top_secret' | 'api_key' | 'certificate'
    encryptTraces: true,
    maskSensitiveData: true,
    auditLevel: 'detailed',
    sensitivePatterns: [/SSN/, /ACCOUNT/],
    retentionDays: 30,
  },
  
  performance: {
    maxCpuPercent: 5,
    maxMemoryMB: 256,
    maxTraceBufferMB: 100,
    batchSize: 100,
    flushIntervalMs: 1000,
    maxEventsPerSecond: 10000,
  },
});
```

### Trace Sources

| Source | Description |
|--------|-------------|
| `debug_tool` | IBM Debug Tool integration |
| `xpediter` | Compuware Xpediter integration |
| `cedf` | CICS Execution Diagnostic Facility |
| `strobe` | Strobe profiler |
| `smf` | System Management Facilities records |
| `gtf` | Generalized Trace Facility |
| `custom_agent` | Custom trace collection agent |

### Connection Lifecycle

```typescript
// Connect to mainframe
await capture.connect();

// Check connection status
if (capture.isConnected()) {
  console.log('Connected to mainframe');
}

// Disconnect when done
await capture.disconnect();
```

### Starting/Stopping Capture

```typescript
// Start capture with optional filter
const sessionId = await capture.startCapture({
  includeTransactionIds: ['CINQ', 'CUPD'],
  includeProgramNames: ['CUSTINQ', 'CUSTUPD'],
  minResponseTimeMs: 100,  // Only slow transactions
});

// Run your workload...

// Stop capture and get traces
const traces = await capture.stopCapture(sessionId);

console.log(`Captured ${traces.length} traces`);
```

### Real-time Streaming

```typescript
// Subscribe to real-time trace events
capture.onTrace((trace) => {
  console.log(`Trace: ${trace.transactionId}`);
  console.log(`  Program: ${trace.programName}`);
  console.log(`  Events: ${trace.events.length}`);
  console.log(`  Duration: ${trace.metrics.elapsedTimeMs}ms`);
});

// Unsubscribe
capture.offTrace(handler);
```

### Health Monitoring

```typescript
// Get capture health status
const health = await capture.getHealth();

console.log(`Connected: ${health.connected}`);
console.log(`Traces collected: ${health.tracesCollected}`);
console.log(`Events collected: ${health.eventsCollected}`);
console.log(`Bytes collected: ${health.bytesCollected}`);
console.log(`Avg latency: ${health.averageCollectionLatencyMs}ms`);
```

## Trace Event Types

### CICS Events

Captured CICS commands and their attributes:

```typescript
interface CICSCommand {
  commandType: 'LINK' | 'XCTL' | 'CALL' | 'READ' | 'WRITE' | 'SEND' | 'RECEIVE' | ...;
  timestamp: Date;
  programName?: string;
  transactionId?: string;
  fileName?: string;
  mapName?: string;
  response?: string;
  eibresp?: number;
  eibresp2?: number;
  commareaSize?: number;
  commarea?: Record<string, unknown>;
  duration?: number;
}
```

### DB2 Events

SQL operations with full context:

```typescript
interface DB2TraceEvent {
  statementType: 'SELECT' | 'INSERT' | 'UPDATE' | 'DELETE' | 'CALL' | ...;
  sqlStatement?: string;
  table?: string;
  columns?: string[];
  whereClause?: string;
  hostVariables?: Record<string, unknown>;
  rowsAffected?: number;
  sqlCode: number;
  sqlState?: string;
  duration?: number;
  plan?: string;
}
```

### VSAM Events

File operations:

```typescript
interface VSAMOperation {
  operationType: 'READ' | 'WRITE' | 'REWRITE' | 'DELETE' | 'BROWSE' | ...;
  fileName: string;
  fileType: 'KSDS' | 'ESDS' | 'RRDS';
  recordKey?: string;
  recordData?: Record<string, unknown>;
  status: string;
  duration?: number;
}
```

### Batch Events

JCL job and step events:

```typescript
interface BatchJobEvent {
  eventType: 'JOB_START' | 'STEP_START' | 'STEP_END' | 'JOB_END' | 'ABEND';
  jobName: string;
  jobId?: string;
  stepName?: string;
  programName?: string;
  returnCode?: number;
  abendCode?: string;
  cpuTime?: number;
  elapsedTime?: number;
}
```

## TraceNormalizer

Normalizes raw mainframe traces into a standard format for analysis.

### Constructor

```typescript
import { TraceNormalizer } from '@migrationpilot/tracing';

const normalizer = new TraceNormalizer({
  normalizeTimestamps: true,
  extractDataFlows: true,
  detectBranchingPaths: true,
  inferBusinessContext: true,
});
```

### Normalization

```typescript
// Normalize a single trace
const normalized = await normalizer.normalize(rawTrace);

console.log(normalized.id);
console.log(normalized.sourceTraceId);
console.log(normalized.events);         // Normalized events
console.log(normalized.dataFlows);      // Extracted data flows
console.log(normalized.executionPath);  // Execution sequence
console.log(normalized.branchingPoints); // Decision points
console.log(normalized.businessContext); // Inferred context
```

### Merging Traces

```typescript
// Merge related traces (e.g., same transaction across programs)
const merged = await normalizer.merge([trace1, trace2, trace3]);
```

### Normalized Trace Structure

```typescript
interface NormalizedTrace {
  id: string;
  sourceTraceId: string;
  projectId: string;
  transactionId?: string;
  programName: string;
  
  events: NormalizedEvent[];
  dataFlows: DataFlow[];
  executionPath: string[];
  branchingPoints: BranchingPoint[];
  
  businessContext?: {
    domain?: string;
    subdomain?: string;
    process?: string;
  };
  
  normalizedAt: Date;
}
```

## BehavioralInferenceEngine

Infers business rules from normalized trace data.

### Constructor

```typescript
import { BehavioralInferenceEngine } from '@migrationpilot/tracing';

const engine = new BehavioralInferenceEngine({
  projectId: 'my-project',
  minConfidenceThreshold: 0.7,
  maxRulesPerProgram: 100,
  enableStatisticalInference: true,
  enablePatternMatching: true,
  enableDataFlowAnalysis: true,
});
```

### Adding Traces

```typescript
// Add traces for analysis
engine.addTraces(normalizedTraces);
```

### Rule Inference

```typescript
// Infer business rules from all added traces
const rules = await engine.inferRules(traces);

rules.forEach(rule => {
  console.log(`Rule: ${rule.ruleName}`);
  console.log(`  Type: ${rule.ruleType}`);
  console.log(`  Confidence: ${rule.confidence}`);
  console.log(`  Description: ${rule.description}`);
  console.log(`  Evidence: ${rule.evidence.length} observations`);
  console.log(`  Triggered by: ${rule.triggeredBy}`);
  console.log(`  Preconditions: ${rule.preconditions}`);
  console.log(`  Postconditions: ${rule.postconditions}`);
});
```

### Rule Validation

```typescript
// Validate inferred rules against new traces
const validation = await engine.validateRules(rules, newTraces);

console.log(`Validated: ${validation.validated.length}`);
console.log(`Invalidated: ${validation.invalidated.length}`);
console.log(`Inconclusive: ${validation.inconclusive.length}`);
```

### Inferred Rule Structure

```typescript
interface InferredBusinessRule {
  id: string;
  projectId: string;
  programName: string;
  ruleName: string;
  description: string;
  ruleType: 'calculation' | 'validation' | 'transformation' | 'decision' | 'assignment';
  
  confidence: number;
  supportingTraceIds: string[];
  
  triggeredBy: string[];
  preconditions: string[];
  postconditions: string[];
  
  dataAccess: {
    tables: string[];
    files: string[];
  };
  
  evidence: RuleEvidence[];
  inferenceMethod: 'pattern' | 'statistical' | 'formula' | 'hybrid';
  needsValidation: boolean;
  suggestedTestCases: SuggestedTestCase[];
  
  status: 'pending' | 'validated' | 'rejected';
  inferredAt: Date;
}
```

## SMEValidationWorkflow

Manages the SME review process for inferred business rules.

### Constructor

```typescript
import { SMEValidationWorkflow } from '@migrationpilot/tracing';

const workflow = new SMEValidationWorkflow({
  requiredApprovals: 2,
  validationTimeoutDays: 7,
  escalationThreshold: 3,
  notificationEnabled: true,
});
```

### Creating Sessions

```typescript
// Create a validation session for an SME
const session = workflow.createSession(
  'sme-user-id',
  'John Smith',
  inferredRules,
  {
    showTraceEvidence: true,
    showConfidenceScores: true,
    requireComments: true,
  }
);

console.log(`Session: ${session.id}`);
console.log(`Rules to review: ${session.totalRules}`);
```

### Reviewing Rules

```typescript
// SME reviews a rule
const review = await workflow.reviewRule(
  session.id,
  ruleId,
  {
    decision: 'approved',  // 'approved' | 'rejected' | 'needs_clarification' | 'needs_modification'
    comment: 'Verified against business requirements',
    suggestedChanges: null,
  }
);
```

### Session Management

```typescript
// Get session
const session = workflow.getSession(sessionId);

// Get session summary
const summary = workflow.getSessionSummary(sessionId);
console.log(`Reviewed: ${summary.reviewedRules}`);
console.log(`Approved: ${summary.approvedRules}`);
console.log(`Rejected: ${summary.rejectedRules}`);

// Get all pending sessions
const pending = workflow.getPendingSessions();
```

### Exporting Validated Rules

```typescript
// Export validated rules for use in migration
const exportedRules = await workflow.exportValidatedRules(sessionId);

// Export in different formats
const jsonExport = await workflow.exportValidatedRules(sessionId, 'json');
const csvExport = await workflow.exportValidatedRules(sessionId, 'csv');
```

## TypeScript Types

Key types for the Mainframe Tracing module:

```typescript
interface MainframeExecutionTrace {
  id: string;
  projectId: string;
  transactionId?: string;
  programName: string;
  userId?: string;
  terminalId?: string;
  language: SourceLanguage;
  
  startTime: Date;
  endTime?: Date;
  status: 'running' | 'completed' | 'abended';
  
  events: (CICSCommand | DB2TraceEvent | VSAMOperation | BatchJobEvent)[];
  cicsContext?: CICSContext;
  db2Context?: DB2Context;
  
  metrics: MainframeTraceMetrics;
  capturedIO: MainframeCapturedIO[];
}

interface MainframeTraceMetrics {
  totalEvents: number;
  cicsCommands: number;
  db2Statements: number;
  vsamOperations: number;
  cpuTimeMs: number;
  elapsedTimeMs: number;
  storageUsedKB: number;
  ioCount: number;
}
```

## Example: Complete Workflow

```typescript
import {
  MainframeTraceCapture,
  TraceNormalizer,
  BehavioralInferenceEngine,
  SMEValidationWorkflow,
} from '@migrationpilot/tracing';

// 1. Configure and connect to mainframe
const capture = new MainframeTraceCapture({
  projectId: 'banking-migration',
  platform: 'zos',
  traceSource: 'custom_agent',
  connection: {
    host: 'mainframe.bank.com',
    port: 8080,
    protocol: 'agent',
    connectionTimeoutMs: 30000,
    readTimeoutMs: 60000,
  },
  captureSettings: {
    captureCICS: true,
    captureDB2: true,
    captureVSAM: true,
    captureBatch: true,
    captureStatements: true,
    captureVariables: true,
    samplingMode: 'all',
    samplingRate: 1.0,
  },
  security: {
    authType: 'racf',
    encryptTraces: true,
    maskSensitiveData: true,
  },
});

await capture.connect();

// 2. Capture traces for target transactions
const sessionId = await capture.startCapture({
  includeTransactionIds: ['ACCT', 'TRAN', 'LOAN'],
  minResponseTimeMs: 0,
});

// Wait for production traffic...
await sleep(3600000); // 1 hour

const rawTraces = await capture.stopCapture(sessionId);
console.log(`Captured ${rawTraces.length} traces`);

// 3. Normalize traces
const normalizer = new TraceNormalizer({
  extractDataFlows: true,
  detectBranchingPaths: true,
});

const normalizedTraces = await Promise.all(
  rawTraces.map(t => normalizer.normalize(t))
);

// 4. Infer business rules
const inferenceEngine = new BehavioralInferenceEngine({
  projectId: 'banking-migration',
  minConfidenceThreshold: 0.7,
});

const inferredRules = await inferenceEngine.inferRules(normalizedTraces);
console.log(`Inferred ${inferredRules.length} business rules`);

// 5. Send to SME for validation
const workflow = new SMEValidationWorkflow({
  requiredApprovals: 2,
  validationTimeoutDays: 7,
});

const session = workflow.createSession(
  'sme-john',
  'John Smith',
  inferredRules
);

console.log(`Created validation session: ${session.id}`);
console.log(`Rules pending review: ${session.totalRules}`);

// 6. After SME review, export validated rules
const validatedRules = await workflow.exportValidatedRules(session.id);
console.log(`Validated ${validatedRules.length} rules ready for migration`);

// 7. Disconnect
await capture.disconnect();
```

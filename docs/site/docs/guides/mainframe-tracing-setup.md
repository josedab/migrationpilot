---
sidebar_position: 6
---

# Mainframe Tracing Setup Guide

This guide walks through setting up live mainframe trace capture for z/OS systems, including agent deployment, security configuration, and production best practices.

## Prerequisites

- z/OS 2.3 or later
- CICS TS 5.4+ (for CICS tracing)
- DB2 12+ (for DB2 tracing)
- Network connectivity between MigrationPilot and mainframe
- RACF, ACF2, or Top Secret security setup

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                        MigrationPilot                                │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────────────────┐   │
│  │  Trace      │   │  Inference  │   │  SME Validation         │   │
│  │  Collector  │──▶│  Engine     │──▶│  Workflow               │   │
│  └─────────────┘   └─────────────┘   └─────────────────────────┘   │
└────────▲────────────────────────────────────────────────────────────┘
         │
         │ HTTPS / MQ / Agent
         │
┌────────┴────────────────────────────────────────────────────────────┐
│                        z/OS Mainframe                                │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────────────────┐   │
│  │  CICS       │   │  DB2        │   │  Batch                  │   │
│  │  Regions    │   │  Subsystems │   │  Jobs                   │   │
│  └──────┬──────┘   └──────┬──────┘   └───────────┬─────────────┘   │
│         │                 │                       │                  │
│         └────────────┬────┴───────────────────────┘                  │
│                      │                                               │
│  ┌───────────────────▼───────────────────────────────────────────┐  │
│  │                    Trace Agent                                 │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐      │  │
│  │  │ CEDF/GTF │  │ DB2 Perf │  │ SMF      │  │ USS      │      │  │
│  │  │ Hooks    │  │ Trace    │  │ Records  │  │ Agent    │      │  │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘      │  │
│  └───────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

## Deployment Options

### Option 1: Agent-Based (Recommended)

Deploy a lightweight agent in USS (Unix System Services) that collects traces and streams to MigrationPilot.

**Advantages:**
- Real-time streaming
- Minimal mainframe overhead
- Secure TLS communication
- Supports all trace sources

**Installation:**

1. **Upload agent package:**
```bash
# On your workstation
scp migrationpilot-agent-zos.pax.Z user@mainframe:/u/migpilot/

# On mainframe USS
cd /u/migpilot
pax -rf migrationpilot-agent-zos.pax
```

2. **Configure agent:**
```yaml
# /u/migpilot/config.yaml
server:
  port: 8443
  tls:
    enabled: true
    cert: /u/migpilot/certs/server.crt
    key: /u/migpilot/certs/server.key

migrationpilot:
  endpoint: https://migrationpilot.yourcompany.com
  apiKey: ${MP_API_KEY}
  projectId: mainframe-migration

tracing:
  cics:
    enabled: true
    applids: [CICSPROD, CICSTEST]
  db2:
    enabled: true
    subsystems: [DB2P, DB2T]
  batch:
    enabled: true
    jobClasses: [A, B, C]
  vsam:
    enabled: true

security:
  racf:
    enabled: true
    classname: FACILITY
  maskPII: true
  piiFields:
    - SSN
    - ACCOUNT_NUMBER
    - CREDIT_CARD
```

3. **Start agent:**
```bash
/u/migpilot/bin/migpilot-agent start
```

4. **Verify connection:**
```typescript
const capture = new MainframeTraceCapture({
  connection: {
    protocol: 'agent',
    agentEndpoint: 'https://mainframe.yourcompany.com',
    agentPort: 8443,
  },
});

await capture.connect();
const health = await capture.getHealth();
console.log(health.connected); // true
```

### Option 2: MQ-Based

Use IBM MQ to transport traces from mainframe to MigrationPilot.

**Advantages:**
- Guaranteed delivery
- Works with existing MQ infrastructure
- Decoupled architecture

**Setup:**

1. **Define queues:**
```
DEFINE QLOCAL(MIGPILOT.TRACE.INPUT) MAXMSGL(104857600)
DEFINE QLOCAL(MIGPILOT.TRACE.OUTPUT) MAXMSGL(104857600)
```

2. **Configure channel:**
```
DEFINE CHANNEL(MIGPILOT.SVRCONN) CHLTYPE(SVRCONN) +
  SSLCIPH(TLS_RSA_WITH_AES_256_CBC_SHA256)
```

3. **Connect from MigrationPilot:**
```typescript
const capture = new MainframeTraceCapture({
  connection: {
    protocol: 'mq',
    queueManager: 'QMPROD',
    inputQueue: 'MIGPILOT.TRACE.INPUT',
    outputQueue: 'MIGPILOT.TRACE.OUTPUT',
    channel: 'MIGPILOT.SVRCONN',
    host: 'mainframe.yourcompany.com',
    port: 1414,
  },
});
```

### Option 3: REST-Based

Expose a REST endpoint on USS for trace collection.

**Advantages:**
- Simple setup
- Standard HTTP/S
- Easy firewall configuration

**Setup:**

1. **Deploy REST service on USS:**
```bash
/u/migpilot/bin/migpilot-rest start --port 8080
```

2. **Configure MigrationPilot:**
```typescript
const capture = new MainframeTraceCapture({
  connection: {
    protocol: 'rest',
    host: 'mainframe.yourcompany.com',
    port: 8080,
    basePath: '/api/v1/trace',
  },
});
```

## Security Configuration

### RACF Setup

Grant appropriate permissions:

```
/* Define facility class for trace access */
RDEFINE FACILITY MIGPILOT.TRACE.READ UACC(NONE)
RDEFINE FACILITY MIGPILOT.TRACE.WRITE UACC(NONE)

/* Grant to trace agent user */
PERMIT MIGPILOT.TRACE.READ CLASS(FACILITY) ID(MPAGENT) ACCESS(READ)
PERMIT MIGPILOT.TRACE.WRITE CLASS(FACILITY) ID(MPAGENT) ACCESS(UPDATE)

/* Activate */
SETROPTS RACLIST(FACILITY) REFRESH
```

### TLS Configuration

1. **Generate certificates:**
```bash
# On z/OS
gskkyman -c -k /u/migpilot/certs/keyring.kdb -p password123
gskkyman -l /u/migpilot/certs/keyring.kdb -a /u/migpilot/certs/ca.crt
```

2. **Configure agent TLS:**
```yaml
tls:
  enabled: true
  keyring: /u/migpilot/certs/keyring.kdb
  stashfile: /u/migpilot/certs/keyring.sth
  minVersion: TLS1.2
  ciphers:
    - TLS_AES_256_GCM_SHA384
    - TLS_CHACHA20_POLY1305_SHA256
```

### Data Masking

Protect sensitive data in traces:

```typescript
const capture = new MainframeTraceCapture({
  security: {
    maskSensitiveData: true,
    sensitivePatterns: [
      /\d{3}-\d{2}-\d{4}/,      // SSN
      /\d{16}/,                  // Credit card
      /ACCT-?\d{10,12}/i,        // Account numbers
    ],
    maskPattern: '***MASKED***',
    retentionDays: 30,
  },
});
```

## CICS Tracing

### Enable CEDF Tracing

```
CEMT SET PROG(CEDX) ENABLE
CEMT SET TRAN(*) TRACE
```

### Program-Level Tracing

Add to CICS SIT:

```
AUXTR=ON
AUXTRSW=NEXT
```

### Capture Configuration

```typescript
const capture = new MainframeTraceCapture({
  captureSettings: {
    captureCICS: true,
    captureStatements: true,      // Individual COBOL statements
    captureVariables: true,       // Working storage values
    captureWorkingStorage: false, // Full WS dump (expensive)
    captureLinkageSection: true,  // Parameters
    samplingMode: 'transaction_based',
    samplingRate: 0.1,            // 10% of transactions
  },
});
```

### Transaction Filtering

```typescript
const sessionId = await capture.startCapture({
  includeTransactionIds: ['ACCT', 'LOAN', 'PYMT'],
  excludeTransactionIds: ['CSMI', 'CEMT'],
  includeProgramNames: ['ACCTINQ*', 'LOANPROC'],
  minResponseTimeMs: 100,         // Only slow transactions
});
```

## DB2 Tracing

### Enable Performance Trace

```sql
-START TRACE(PERFM) CLASS(*) DEST(GTF)
```

### Capture SQL Statements

```typescript
const capture = new MainframeTraceCapture({
  captureSettings: {
    captureDB2: true,
    captureStatements: true,      // SQL text
    captureBindVariables: true,   // Host variable values
    captureExplain: false,        // Query plans (expensive)
  },
});
```

## Batch Job Tracing

### SMF Recording

Ensure SMF records are enabled:

```
SMFPRMxx:
  SYS(TYPE(30,70,80,89,90))
```

### Capture Configuration

```typescript
const sessionId = await capture.startCapture({
  includeJobNames: ['PAYROLL*', 'BILLING*'],
  excludeJobNames: ['STC*', 'INIT*'],
  includeJobClasses: ['A', 'B'],
  captureAbends: true,
  captureStepInfo: true,
});
```

## Performance Tuning

### Minimize Mainframe Impact

```typescript
const capture = new MainframeTraceCapture({
  performance: {
    maxCpuPercent: 3,            // CPU cap
    maxMemoryMB: 128,            // Memory limit
    maxTraceBufferMB: 50,        // Buffer size
    batchSize: 200,              // Events per batch
    flushIntervalMs: 2000,       // Flush frequency
    maxEventsPerSecond: 5000,    // Rate limiting
    compressionEnabled: true,    // Compress traces
    compressionLevel: 6,
  },
});
```

### Sampling Strategies

| Strategy | Use Case | Overhead |
|----------|----------|----------|
| `all` | Development, small workloads | High |
| `random` (10%) | Production discovery | Low |
| `time_based` (every 5s) | Performance monitoring | Very Low |
| `transaction_based` | Specific transaction types | Medium |

```typescript
captureSettings: {
  samplingMode: 'random',
  samplingRate: 0.1,  // 10%
}
```

## Troubleshooting

### Agent Not Connecting

1. **Check network connectivity:**
```bash
ping migrationpilot.yourcompany.com
telnet migrationpilot.yourcompany.com 443
```

2. **Verify TLS certificates:**
```bash
openssl s_client -connect migrationpilot.yourcompany.com:443
```

3. **Check agent logs:**
```bash
tail -f /u/migpilot/logs/agent.log
```

### Missing Traces

1. **Verify CEDF is active:**
```
CEMT INQ PROG(CEDX)
```

2. **Check transaction filtering:**
```typescript
const health = await capture.getHealth();
console.log(health.tracesCollected);
console.log(health.eventsCollected);
```

3. **Review capture quality:**
```typescript
const traces = await capture.stopCapture(sessionId);
for (const trace of traces) {
  console.log(`Quality: ${trace.captureQuality.completeness}`);
  console.log(`Errors: ${trace.captureQuality.parseErrors}`);
}
```

### High Overhead

1. **Reduce sampling rate:**
```typescript
samplingRate: 0.01  // 1%
```

2. **Increase batch size:**
```typescript
batchSize: 500
flushIntervalMs: 5000
```

3. **Disable expensive options:**
```typescript
captureWorkingStorage: false
captureExplain: false
```

### Data Not Masked

1. **Verify patterns match your data:**
```typescript
sensitivePatterns: [
  /YOUR-SSN-FORMAT/,
  /YOUR-ACCOUNT-FORMAT/,
]
```

2. **Check mask is applied:**
```typescript
const trace = await capture.getTrace(traceId);
console.log(trace.inputs.parameters);
// Should show ***MASKED***
```

## Production Checklist

- [ ] TLS enabled with strong ciphers
- [ ] RACF/ACF2/Top Secret permissions configured
- [ ] PII masking enabled and tested
- [ ] Sampling rate appropriate for workload
- [ ] Performance limits set
- [ ] Log rotation configured
- [ ] Monitoring and alerting set up
- [ ] Backup trace retention policy defined
- [ ] Disaster recovery procedure documented
- [ ] Security review completed

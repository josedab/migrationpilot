/**
 * Mainframe Trace Capture Agent
 * 
 * Core implementation for capturing execution traces from z/OS mainframe systems.
 * Supports multiple collection methods: agent-based, MQ-based, and debug tool integration.
 */

import { generateId } from '@migrationpilot/core';
import type { SourceLanguage } from '@migrationpilot/core';
import type { TraceEvent, TraceEventType, TraceLocation } from '../types.js';
import type {
  MainframeTraceConfig,
  MainframeExecutionTrace,
  MainframeCapturedIO,
  MainframeTraceMetrics,
  CaptureQuality,
  CICSContext,
  CICSCommand,
  CICSCommandType,
  DB2TraceEvent,
  DB2Context,
  VSAMOperation,
  VSAMOperationType,
  TransactionFilter,
  BatchJobFilter,
  IMainframeTracer,
  MainframeTraceFilter,
  MainframeTracerHealth,
  NormalizedTrace,
  MainframePlatform,
} from './types.js';

const DEFAULT_PERFORMANCE_CONFIG = {
  maxCpuPercent: 5,
  maxMemoryMB: 256,
  maxTraceBufferMB: 100,
  batchSize: 100,
  flushIntervalMs: 1000,
  maxEventsPerSecond: 10000,
  backoffMultiplier: 2,
  maxBackoffMs: 30000,
  compressionEnabled: true,
  compressionLevel: 6,
};

export class MainframeTraceCapture implements IMainframeTracer {
  readonly config: MainframeTraceConfig;
  
  private connected = false;
  private activeCaptureSessions = new Map<string, CaptureSession>();
  private traces = new Map<string, MainframeExecutionTrace>();
  private traceListeners = new Set<(trace: MainframeExecutionTrace) => void>();
  private eventBuffer: BufferedEvent[] = [];
  private flushTimer?: NodeJS.Timeout;
  
  // Health metrics
  private tracesCollected = 0;
  private eventsCollected = 0;
  private bytesCollected = 0;
  private errorsEncountered = 0;
  private collectionLatencies: number[] = [];
  private lastTraceAt?: Date;
  private lastErrorAt?: Date;
  private lastError?: string;

  constructor(config: MainframeTraceConfig) {
    this.config = {
      ...config,
      performance: { ...DEFAULT_PERFORMANCE_CONFIG, ...config.performance },
    };
  }

  // ============================================================================
  // LIFECYCLE METHODS
  // ============================================================================

  async connect(): Promise<void> {
    if (this.connected) return;

    try {
      switch (this.config.connection.protocol) {
        case 'agent':
          await this.connectViaAgent();
          break;
        case 'mq':
          await this.connectViaMQ();
          break;
        case 'rest':
          await this.connectViaREST();
          break;
        default:
          throw new Error(`Unsupported protocol: ${this.config.connection.protocol}`);
      }

      this.connected = true;
      this.startFlushTimer();
    } catch (error) {
      this.recordError(error);
      throw error;
    }
  }

  async disconnect(): Promise<void> {
    if (!this.connected) return;

    // Stop all active capture sessions
    for (const sessionId of this.activeCaptureSessions.keys()) {
      await this.stopCapture(sessionId);
    }

    // Flush remaining events
    await this.flushEventBuffer();

    // Stop flush timer
    if (this.flushTimer) {
      clearInterval(this.flushTimer);
      this.flushTimer = undefined;
    }

    // Disconnect based on protocol
    switch (this.config.connection.protocol) {
      case 'agent':
        await this.disconnectFromAgent();
        break;
      case 'mq':
        await this.disconnectFromMQ();
        break;
      case 'rest':
        // REST is stateless, nothing to disconnect
        break;
    }

    this.connected = false;
  }

  isConnected(): boolean {
    return this.connected;
  }

  // ============================================================================
  // TRACE CAPTURE METHODS
  // ============================================================================

  async startCapture(filter?: TransactionFilter | BatchJobFilter): Promise<string> {
    if (!this.connected) {
      throw new Error('Not connected to mainframe');
    }

    const sessionId = generateId();
    const session: CaptureSession = {
      id: sessionId,
      startedAt: new Date(),
      filter,
      events: [],
      traces: [],
      activeTransactions: new Map(),
    };

    this.activeCaptureSessions.set(sessionId, session);

    // Send capture start command based on trace source
    await this.sendCaptureStartCommand(sessionId, filter);

    return sessionId;
  }

  async stopCapture(captureSessionId: string): Promise<MainframeExecutionTrace[]> {
    const session = this.activeCaptureSessions.get(captureSessionId);
    if (!session) {
      throw new Error(`Capture session not found: ${captureSessionId}`);
    }

    // Send capture stop command
    await this.sendCaptureStopCommand(captureSessionId);

    // Finalize any pending traces
    for (const [_transactionId, pendingTrace] of session.activeTransactions) {
      const finalizedTrace = this.finalizeTrace(pendingTrace);
      session.traces.push(finalizedTrace);
      this.traces.set(finalizedTrace.id, finalizedTrace);
    }

    this.activeCaptureSessions.delete(captureSessionId);

    return session.traces;
  }

  // ============================================================================
  // REAL-TIME STREAMING
  // ============================================================================

  onTrace(callback: (trace: MainframeExecutionTrace) => void): void {
    this.traceListeners.add(callback);
  }

  offTrace(callback: (trace: MainframeExecutionTrace) => void): void {
    this.traceListeners.delete(callback);
  }

  private emitTrace(trace: MainframeExecutionTrace): void {
    for (const listener of this.traceListeners) {
      try {
        listener(trace);
      } catch (error) {
        console.error('Error in trace listener:', error);
      }
    }
  }

  // ============================================================================
  // QUERY METHODS
  // ============================================================================

  async getTrace(traceId: string): Promise<MainframeExecutionTrace | null> {
    return this.traces.get(traceId) ?? null;
  }

  async queryTraces(filter: MainframeTraceFilter): Promise<MainframeExecutionTrace[]> {
    let results = Array.from(this.traces.values());

    if (filter.programName) {
      results = results.filter(t => t.programName === filter.programName);
    }
    if (filter.transactionId) {
      results = results.filter(t => t.transactionId === filter.transactionId);
    }
    if (filter.jobName) {
      results = results.filter(t => t.jobName === filter.jobName);
    }
    if (filter.userId) {
      results = results.filter(t => 
        t.cicsContext?.userId === filter.userId ||
        t.batchContext?.submittedBy === filter.userId
      );
    }
    if (filter.startTimeAfter) {
      const afterTime = filter.startTimeAfter.getTime();
      results = results.filter(t => t.startTime >= afterTime);
    }
    if (filter.startTimeBefore) {
      const beforeTime = filter.startTimeBefore.getTime();
      results = results.filter(t => t.startTime <= beforeTime);
    }
    if (filter.minDuration !== undefined) {
      results = results.filter(t => t.durationMs >= filter.minDuration!);
    }
    if (filter.maxDuration !== undefined) {
      results = results.filter(t => t.durationMs <= filter.maxDuration!);
    }
    if (filter.hasErrors !== undefined) {
      results = results.filter(t => 
        (t.captureQuality.parseErrors.length > 0) === filter.hasErrors
      );
    }

    // Apply offset and limit
    if (filter.offset) {
      results = results.slice(filter.offset);
    }
    if (filter.limit) {
      results = results.slice(0, filter.limit);
    }

    return results;
  }

  // ============================================================================
  // NORMALIZATION
  // ============================================================================

  async normalizeTrace(trace: MainframeExecutionTrace): Promise<NormalizedTrace> {
    // Import normalizer dynamically to avoid circular dependency
    const { TraceNormalizer } = await import('./normalizer.js');
    const normalizer = new TraceNormalizer(this.config.projectId);
    return normalizer.normalize(trace);
  }

  // ============================================================================
  // HEALTH
  // ============================================================================

  async getHealth(): Promise<MainframeTracerHealth> {
    const avgLatency = this.collectionLatencies.length > 0
      ? this.collectionLatencies.reduce((a, b) => a + b, 0) / this.collectionLatencies.length
      : 0;

    const recentLatencies = this.collectionLatencies.slice(-100);
    const eventsPerSecond = recentLatencies.length > 0
      ? 1000 / (avgLatency || 1)
      : 0;

    const bufferUtilization = this.eventBuffer.length / this.config.performance.batchSize;

    return {
      connected: this.connected,
      platform: this.config.platform,
      traceSource: this.config.traceSource,
      tracesCollected: this.tracesCollected,
      eventsCollected: this.eventsCollected,
      bytesCollected: this.bytesCollected,
      errorsEncountered: this.errorsEncountered,
      avgCollectionLatencyMs: avgLatency,
      eventsPerSecond,
      bufferUtilization,
      lastTraceAt: this.lastTraceAt,
      lastErrorAt: this.lastErrorAt,
      lastError: this.lastError,
    };
  }

  // ============================================================================
  // EVENT PROCESSING
  // ============================================================================

  /**
   * Process raw trace data from the mainframe
   */
  processRawTraceData(data: RawTraceData): void {
    const startTime = Date.now();

    try {
      // Parse based on trace source format
      const events = this.parseTraceData(data);
      
      // Apply sampling if configured
      const sampledEvents = this.applySampling(events);
      
      // Apply security transformations
      const securedEvents = sampledEvents.map(e => this.applySecurityTransforms(e));
      
      // Buffer events for batching
      this.eventBuffer.push(...securedEvents.map(e => ({
        event: e,
        receivedAt: Date.now(),
      })));

      // Update metrics
      this.eventsCollected += events.length;
      this.bytesCollected += data.raw.length;
      this.collectionLatencies.push(Date.now() - startTime);
      if (this.collectionLatencies.length > 1000) {
        this.collectionLatencies.shift();
      }

      // Process buffered events into traces
      this.processBufferedEvents();

      // Check if buffer is full
      if (this.eventBuffer.length >= this.config.performance.batchSize) {
        this.flushEventBuffer();
      }
    } catch (error) {
      this.recordError(error);
    }
  }

  /**
   * Process a CICS trace event
   */
  processCICSEvent(event: CICSTraceInput): void {
    const traceEvent = this.convertCICSEvent(event);
    
    // Find or create trace for this transaction
    const session = this.findSessionForTransaction(event.transactionId);
    if (!session) return;

    let pendingTrace = session.activeTransactions.get(event.transactionId);
    if (!pendingTrace) {
      pendingTrace = this.createPendingTrace(event.programName, event.transactionId);
      pendingTrace.cicsContext = this.extractCICSContext(event);
      session.activeTransactions.set(event.transactionId, pendingTrace);
    }

    pendingTrace.events.push(traceEvent);

    // Check for transaction end
    if (event.command === 'RETURN' && !event.transactionId) {
      const finalizedTrace = this.finalizeTrace(pendingTrace);
      session.traces.push(finalizedTrace);
      session.activeTransactions.delete(event.transactionId);
      this.traces.set(finalizedTrace.id, finalizedTrace);
      this.emitTrace(finalizedTrace);
      this.tracesCollected++;
      this.lastTraceAt = new Date();
    }
  }

  /**
   * Process a DB2 trace event
   */
  processDB2Event(event: DB2TraceInput): void {
    const traceEvent = this.convertDB2Event(event);
    
    // Find the active trace for this correlation
    for (const session of this.activeCaptureSessions.values()) {
      for (const pendingTrace of session.activeTransactions.values()) {
        if (pendingTrace.db2Operations) {
          pendingTrace.db2Operations.push(traceEvent as DB2TraceEvent);
        }
      }
    }
  }

  /**
   * Process a VSAM operation trace
   */
  processVSAMEvent(event: VSAMTraceInput): void {
    const operation = this.convertVSAMEvent(event);
    
    // Add to active traces
    for (const session of this.activeCaptureSessions.values()) {
      for (const pendingTrace of session.activeTransactions.values()) {
        if (pendingTrace.vsamOperations) {
          pendingTrace.vsamOperations.push(operation);
        }
      }
    }
  }

  // ============================================================================
  // PRIVATE HELPER METHODS
  // ============================================================================

  private async connectViaAgent(): Promise<void> {
    const { agentEndpoint, agentPort } = this.config.connection;
    if (!agentEndpoint || !agentPort) {
      throw new Error('Agent endpoint and port required for agent protocol');
    }
    // In production, would establish connection to the mainframe agent
    console.log(`Connecting to mainframe agent at ${agentEndpoint}:${agentPort}`);
  }

  private async connectViaMQ(): Promise<void> {
    const { queueManager, inputQueue, outputQueue } = this.config.connection;
    if (!queueManager || !inputQueue || !outputQueue) {
      throw new Error('Queue manager and queue names required for MQ protocol');
    }
    // In production, would connect to IBM MQ
    console.log(`Connecting to MQ: ${queueManager}/${inputQueue}`);
  }

  private async connectViaREST(): Promise<void> {
    const { host, port } = this.config.connection;
    // In production, would verify REST endpoint availability
    console.log(`Connecting to REST endpoint: ${host}:${port}`);
  }

  private async disconnectFromAgent(): Promise<void> {
    console.log('Disconnecting from mainframe agent');
  }

  private async disconnectFromMQ(): Promise<void> {
    console.log('Disconnecting from MQ');
  }

  private startFlushTimer(): void {
    this.flushTimer = setInterval(
      () => this.flushEventBuffer(),
      this.config.performance.flushIntervalMs
    );
  }

  private async flushEventBuffer(): Promise<void> {
    if (this.eventBuffer.length === 0) return;

    const events = [...this.eventBuffer];
    this.eventBuffer = [];

    // Process events into traces
    for (const { event } of events) {
      this.routeEventToTrace(event);
    }
  }

  private processBufferedEvents(): void {
    // Group events by transaction/job for trace assembly
    const eventsByTransaction = new Map<string, TraceEvent[]>();

    for (const { event } of this.eventBuffer) {
      const key = this.getEventGroupingKey(event);
      const existing = eventsByTransaction.get(key) || [];
      existing.push(event);
      eventsByTransaction.set(key, existing);
    }

    // Update pending traces
    for (const [key, events] of eventsByTransaction) {
      this.updatePendingTrace(key, events);
    }
  }

  private getEventGroupingKey(event: TraceEvent): string {
    // Group by transaction ID or job name
    const metadata = event.metadata || {};
    return metadata.transactionId as string || 
           metadata.jobName as string || 
           event.sessionId;
  }

  private routeEventToTrace(event: TraceEvent): void {
    const key = this.getEventGroupingKey(event);
    this.updatePendingTrace(key, [event]);
  }

  private updatePendingTrace(key: string, events: TraceEvent[]): void {
    for (const session of this.activeCaptureSessions.values()) {
      let pendingTrace = session.activeTransactions.get(key);
      if (pendingTrace) {
        pendingTrace.events.push(...events);
      }
    }
  }

  private async sendCaptureStartCommand(
    _sessionId: string,
    _filter?: TransactionFilter | BatchJobFilter
  ): Promise<void> {
    // In production, would send command to mainframe trace facility
    console.log('Sending capture start command');
  }

  private async sendCaptureStopCommand(_sessionId: string): Promise<void> {
    // In production, would send command to stop capture
    console.log('Sending capture stop command');
  }

  private parseTraceData(data: RawTraceData): TraceEvent[] {
    switch (data.format) {
      case 'smf':
        return this.parseSMFData(data.raw);
      case 'gtf':
        return this.parseGTFData(data.raw);
      case 'cedf':
        return this.parseCEDFData(data.raw);
      case 'json':
        return this.parseJSONData(data.raw);
      default:
        throw new Error(`Unsupported trace format: ${data.format}`);
    }
  }

  private parseSMFData(raw: string): TraceEvent[] {
    // Parse SMF records (IBM System Management Facility)
    // In production, would parse binary SMF record format
    return this.parseGenericFormat(raw);
  }

  private parseGTFData(raw: string): TraceEvent[] {
    // Parse GTF records (Generalized Trace Facility)
    return this.parseGenericFormat(raw);
  }

  private parseCEDFData(raw: string): TraceEvent[] {
    // Parse CEDF records (CICS Execution Diagnostic Facility)
    return this.parseGenericFormat(raw);
  }

  private parseJSONData(raw: string): TraceEvent[] {
    try {
      const data = JSON.parse(raw);
      if (Array.isArray(data)) {
        return data.map(item => this.convertToTraceEvent(item));
      }
      return [this.convertToTraceEvent(data)];
    } catch {
      return [];
    }
  }

  private parseGenericFormat(raw: string): TraceEvent[] {
    // Generic line-based parsing
    const lines = raw.split('\n').filter(l => l.trim());
    return lines.map(line => this.parseTraceLine(line));
  }

  private parseTraceLine(line: string): TraceEvent {
    // Parse a single trace line into a TraceEvent
    // Format varies by trace source, this is a simplified implementation
    return {
      id: generateId(),
      sessionId: '',
      timestamp: Date.now(),
      sequenceNumber: 0,
      type: 'procedure_entry',
      location: { file: '', line: 0 },
      data: { raw: line },
    };
  }

  private convertToTraceEvent(data: Record<string, unknown>): TraceEvent {
    return {
      id: generateId(),
      sessionId: data.sessionId as string || '',
      timestamp: data.timestamp as number || Date.now(),
      sequenceNumber: data.sequence as number || 0,
      type: data.type as TraceEventType || 'procedure_entry',
      location: data.location as TraceLocation || { file: '', line: 0 },
      data: data.data as Record<string, unknown> || {},
      metadata: data.metadata as Record<string, unknown>,
    };
  }

  private applySampling(events: TraceEvent[]): TraceEvent[] {
    const { samplingMode, samplingRate } = this.config.captureSettings;
    
    if (samplingMode === 'all' || samplingRate >= 1.0) {
      return events;
    }

    if (samplingMode === 'random') {
      return events.filter(() => Math.random() < samplingRate);
    }

    // For time_based and transaction_based, more complex logic needed
    return events;
  }

  private applySecurityTransforms(event: TraceEvent): TraceEvent {
    if (!this.config.security.anonymizePII) {
      return event;
    }

    const masked = { ...event, data: { ...event.data } };
    
    for (const field of this.config.security.piiFields) {
      if (masked.data[field] !== undefined) {
        masked.data[field] = this.maskValue(masked.data[field]);
      }
    }

    return masked;
  }

  private maskValue(value: unknown): string {
    const pattern = this.config.security.maskPattern || '****';
    if (typeof value === 'string') {
      return value.slice(0, 2) + pattern + value.slice(-2);
    }
    return pattern;
  }

  private findSessionForTransaction(transactionId: string): CaptureSession | undefined {
    for (const session of this.activeCaptureSessions.values()) {
      const filter = session.filter as TransactionFilter | undefined;
      if (!filter) return session;
      
      if (filter.includeTransactionIds?.includes(transactionId)) {
        return session;
      }
      if (filter.excludeTransactionIds?.includes(transactionId)) {
        continue;
      }
      if (!filter.includeTransactionIds) {
        return session;
      }
    }
    return undefined;
  }

  private createPendingTrace(
    programName: string,
    transactionId?: string
  ): PendingTrace {
    return {
      id: generateId(),
      projectId: this.config.projectId,
      platform: this.config.platform,
      language: 'cobol' as SourceLanguage,
      programName,
      transactionId,
      startTime: Date.now(),
      events: [],
      cicsCommands: [],
      db2Operations: [],
      vsamOperations: [],
      parseErrors: [],
      warnings: [],
    };
  }

  private extractCICSContext(event: CICSTraceInput): CICSContext {
    return {
      transactionId: event.transactionId,
      taskNumber: event.taskNumber,
      terminalId: event.terminalId,
      userId: event.userId,
      applid: event.applid,
      sysid: event.sysid,
      programName: event.programName,
      eibcalen: event.eibcalen,
      eibtrnid: event.eibtrnid,
      eibtaskn: event.eibtaskn,
      eibtime: event.eibtime,
      eibdate: event.eibdate,
    };
  }

  private convertCICSEvent(event: CICSTraceInput): TraceEvent {
    let eventType: TraceEventType = 'external_call';
    
    switch (event.command) {
      case 'LINK':
      case 'XCTL':
        eventType = 'procedure_entry';
        break;
      case 'RETURN':
        eventType = 'procedure_exit';
        break;
      case 'READ':
      case 'READQ':
        eventType = 'file_read';
        break;
      case 'WRITE':
      case 'WRITEQ':
        eventType = 'file_write';
        break;
    }

    return {
      id: generateId(),
      sessionId: event.transactionId,
      timestamp: event.timestamp,
      sequenceNumber: 0,
      type: eventType,
      location: {
        file: event.programName,
        line: event.statementNumber || 0,
        procedure: event.paragraphName,
      },
      data: {
        name: event.command,
        parameters: event.commandOptions,
        returnValue: event.responseCode,
      },
      metadata: {
        transactionId: event.transactionId,
        taskNumber: event.taskNumber,
      },
    };
  }

  private convertDB2Event(event: DB2TraceInput): TraceEvent {
    return {
      id: generateId(),
      sessionId: event.correlationId,
      timestamp: event.timestamp,
      sequenceNumber: 0,
      type: 'db_query',
      location: {
        file: event.packageName || event.planName,
        line: event.statementId || 0,
      },
      data: {
        query: event.sqlText,
        parameters: event.hostVariables,
        recordCount: event.rowsAffected,
      },
      metadata: {
        db2Context: {
          planName: event.planName,
          packageName: event.packageName,
          sqlCode: event.sqlCode,
          sqlState: event.sqlState,
        },
      },
    };
  }

  private convertVSAMEvent(event: VSAMTraceInput): VSAMOperation {
    return {
      id: generateId(),
      timestamp: event.timestamp,
      filename: event.filename,
      organization: event.organization,
      operation: event.operation as VSAMOperationType,
      key: event.key,
      rba: event.rba,
      rrn: event.rrn,
      record: event.record,
      recordLength: event.recordLength,
      feedbackCode: event.feedbackCode,
      returnCode: event.returnCode,
    };
  }

  private finalizeTrace(pending: PendingTrace): MainframeExecutionTrace {
    const endTime = Date.now();
    
    const metrics = this.calculateMetrics(pending);
    const captureQuality = this.assessCaptureQuality(pending);
    const inputs = this.extractInputs(pending);
    const outputs = this.extractOutputs(pending);

    return {
      id: pending.id,
      projectId: pending.projectId,
      platform: pending.platform,
      language: pending.language,
      programName: pending.programName,
      transactionId: pending.transactionId,
      jobName: pending.jobName,
      startTime: pending.startTime,
      endTime,
      durationMs: endTime - pending.startTime,
      cicsContext: pending.cicsContext,
      db2Context: pending.db2Context,
      batchContext: pending.batchContext,
      events: pending.events,
      cicsCommands: pending.cicsCommands,
      db2Operations: pending.db2Operations,
      vsamOperations: pending.vsamOperations,
      inputs,
      outputs,
      metrics,
      captureQuality,
      capturedAt: new Date(),
    };
  }

  private calculateMetrics(pending: PendingTrace): MainframeTraceMetrics {
    const cicsCommands = pending.cicsCommands || [];
    const db2Operations = pending.db2Operations || [];
    const vsamOps = pending.vsamOperations || [];

    return {
      totalEvents: pending.events.length,
      statementCount: pending.events.filter(e => 
        e.type === 'variable_read' || e.type === 'variable_write'
      ).length,
      proceduresCalled: pending.events.filter(e => e.type === 'procedure_entry').length,
      paragraphsExecuted: new Set(
        pending.events.filter(e => e.location.procedure).map(e => e.location.procedure)
      ).size,
      
      cicsCommandCount: cicsCommands.length,
      linkCount: cicsCommands.filter(c => c.type === 'LINK').length,
      xctlCount: cicsCommands.filter(c => c.type === 'XCTL').length,
      
      db2CallCount: db2Operations.length,
      sqlSelectCount: db2Operations.filter(o => 
        o.db2Context?.statementType === 'SELECT'
      ).length,
      sqlInsertCount: db2Operations.filter(o => 
        o.db2Context?.statementType === 'INSERT'
      ).length,
      sqlUpdateCount: db2Operations.filter(o => 
        o.db2Context?.statementType === 'UPDATE'
      ).length,
      sqlDeleteCount: db2Operations.filter(o => 
        o.db2Context?.statementType === 'DELETE'
      ).length,
      totalRowsProcessed: db2Operations.reduce((sum, o) => 
        sum + (o.db2Context?.rowsAffected || 0), 0
      ),
      
      fileOperations: pending.events.filter(e => 
        e.type.startsWith('file_')
      ).length,
      vsamOperations: vsamOps.length,
      recordsRead: vsamOps.filter(o => 
        o.operation === 'GET' || o.operation === 'READ_NEXT'
      ).length,
      recordsWritten: vsamOps.filter(o => 
        o.operation === 'PUT' || o.operation === 'WRITE'
      ).length,
      
      cpuTimeMicroseconds: 0, // Would be calculated from trace data
      waitTimeMicroseconds: 0,
      
      branchesCovered: pending.events.filter(e => e.type === 'branch_taken').length,
      totalBranches: pending.events.filter(e => 
        e.type === 'branch_taken' || e.type === 'branch_not_taken'
      ).length,
      uniqueCodePaths: new Set(
        pending.events.map(e => `${e.location.file}:${e.location.line}`)
      ).size,
    };
  }

  private assessCaptureQuality(pending: PendingTrace): CaptureQuality {
    const eventCount = pending.events.length;
    const expectedMinEvents = 10;
    const completeness = Math.min(eventCount / expectedMinEvents, 1.0);

    return {
      completeness,
      eventDropCount: 0, // Would track dropped events due to buffer overflow
      truncatedFields: pending.warnings.filter(w => w.includes('truncated')).length,
      parseErrors: pending.parseErrors,
      warnings: pending.warnings,
    };
  }

  private extractInputs(pending: PendingTrace): MainframeCapturedIO {
    const firstEvent = pending.events[0];
    return {
      parameters: firstEvent?.data.parameters as Record<string, unknown> || {},
      files: [],
      database: [],
      screens: [],
      messages: [],
      commarea: pending.cicsContext?.commarea,
      channels: pending.cicsContext?.channels,
    };
  }

  private extractOutputs(pending: PendingTrace): MainframeCapturedIO {
    const lastEvent = pending.events[pending.events.length - 1];
    return {
      parameters: lastEvent?.data.returnValue as Record<string, unknown> || {},
      files: [],
      database: [],
      screens: [],
      messages: [],
    };
  }

  private recordError(error: unknown): void {
    this.errorsEncountered++;
    this.lastErrorAt = new Date();
    this.lastError = error instanceof Error ? error.message : String(error);
    console.error('Mainframe trace capture error:', error);
  }
}

// ============================================================================
// SUPPORTING INTERFACES
// ============================================================================

interface CaptureSession {
  id: string;
  startedAt: Date;
  filter?: TransactionFilter | BatchJobFilter;
  events: TraceEvent[];
  traces: MainframeExecutionTrace[];
  activeTransactions: Map<string, PendingTrace>;
}

interface PendingTrace {
  id: string;
  projectId: string;
  platform: MainframePlatform;
  language: SourceLanguage;
  programName: string;
  transactionId?: string;
  jobName?: string;
  startTime: number;
  events: TraceEvent[];
  cicsContext?: CICSContext;
  db2Context?: DB2Context;
  batchContext?: undefined;
  cicsCommands?: CICSCommand[];
  db2Operations?: DB2TraceEvent[];
  vsamOperations?: VSAMOperation[];
  parseErrors: string[];
  warnings: string[];
}

interface BufferedEvent {
  event: TraceEvent;
  receivedAt: number;
}

interface RawTraceData {
  format: 'smf' | 'gtf' | 'cedf' | 'json' | 'binary';
  raw: string;
  timestamp?: number;
  source?: string;
}

interface CICSTraceInput {
  transactionId: string;
  taskNumber: number;
  terminalId?: string;
  userId: string;
  applid: string;
  sysid: string;
  programName: string;
  command: CICSCommandType;
  commandOptions?: Record<string, unknown>;
  responseCode?: number;
  timestamp: number;
  statementNumber?: number;
  paragraphName?: string;
  eibcalen?: number;
  eibtrnid?: string;
  eibtaskn?: number;
  eibtime?: number;
  eibdate?: number;
}

interface DB2TraceInput {
  planName: string;
  packageName?: string;
  correlationId: string;
  statementId?: number;
  statementType: string;
  sqlText: string;
  hostVariables?: Record<string, unknown>;
  sqlCode: number;
  sqlState?: string;
  rowsAffected?: number;
  timestamp: number;
}

interface VSAMTraceInput {
  filename: string;
  organization: 'KSDS' | 'ESDS' | 'RRDS' | 'LDS';
  operation: string;
  key?: string;
  rba?: number;
  rrn?: number;
  record?: string;
  recordLength?: number;
  feedbackCode: number;
  returnCode: number;
  timestamp: number;
}

export type { CaptureSession, PendingTrace, RawTraceData, CICSTraceInput, DB2TraceInput, VSAMTraceInput };

/**
 * Java Legacy Tracer
 * 
 * Specialized tracer for Legacy Java applications with support for:
 * - EJB 2.x session/entity beans
 * - J2EE servlets and JSP
 * - Struts 1.x actions
 * - JDBC operations
 * - JMS messaging
 */

import type { SourceLanguage } from '@migrationpilot/core';
import { BaseTracer } from './base-tracer.js';
import type { TracerConfig, TraceLocation, DatabaseIO } from '../types.js';

export interface JavaLegacyTracerConfig extends Partial<TracerConfig> {
  projectId: string;
  
  // Java-specific options
  traceEJB: boolean;
  traceServlets: boolean;
  traceStruts: boolean;
  traceJDBC: boolean;
  traceJMS: boolean;
  traceJNDI: boolean;
  
  // Bytecode instrumentation settings
  enableBytecodeInstrumentation: boolean;
  instrumentedPackages?: string[];
  excludedPackages?: string[];
}

const DEFAULT_JAVA_CONFIG: Partial<JavaLegacyTracerConfig> = {
  traceEJB: true,
  traceServlets: true,
  traceStruts: true,
  traceJDBC: true,
  traceJMS: true,
  traceJNDI: true,
  enableBytecodeInstrumentation: false,
  excludedPackages: ['java.', 'javax.', 'sun.', 'com.sun.'],
};

export class JavaLegacyTracer extends BaseTracer {
  readonly language: SourceLanguage = 'java-legacy';
  private javaConfig: JavaLegacyTracerConfig;
  
  constructor(config: JavaLegacyTracerConfig) {
    super({ ...config, language: 'java-legacy' });
    this.javaConfig = { ...DEFAULT_JAVA_CONFIG, ...config };
  }

  protected async onInitialize(): Promise<void> {
    // Initialize Java agent if bytecode instrumentation enabled
    if (this.javaConfig.enableBytecodeInstrumentation) {
      // In production, would attach Java agent for instrumentation
    }
  }

  protected async onShutdown(): Promise<void> {
    // Detach instrumentation
  }

  /**
   * Record method entry
   */
  recordMethodEntry(
    sessionId: string,
    className: string,
    methodName: string,
    parameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: {
        name: `${className}.${methodName}`,
        parameters,
      },
      metadata: { javaType: 'METHOD' },
    });
  }

  /**
   * Record method exit
   */
  recordMethodExit(
    sessionId: string,
    className: string,
    methodName: string,
    returnValue: unknown,
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: 'procedure_exit',
      location,
      data: {
        name: `${className}.${methodName}`,
        returnValue,
      },
      metadata: { javaType: 'METHOD' },
    });
  }

  /**
   * Record EJB session bean method call
   */
  recordEJBSessionBeanCall(
    sessionId: string,
    beanName: string,
    methodName: string,
    parameters: Record<string, unknown>,
    location: TraceLocation,
    isStateful: boolean
  ): void {
    if (!this.javaConfig.traceEJB) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: {
        name: `EJB:${beanName}.${methodName}`,
        parameters,
      },
      metadata: { 
        javaType: 'EJB_SESSION_BEAN',
        beanName,
        methodName,
        isStateful,
      },
    });
  }

  /**
   * Record EJB entity bean operation
   */
  recordEJBEntityBeanOperation(
    sessionId: string,
    beanName: string,
    operation: 'create' | 'find' | 'remove' | 'ejbLoad' | 'ejbStore',
    primaryKey: unknown,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceEJB) return;
    
    this.recordEvent(sessionId, {
      type: operation === 'find' ? 'db_query' : 'db_result',
      location,
      data: {
        name: `EntityBean:${beanName}.${operation}`,
        parameters: { primaryKey },
      },
      metadata: { 
        javaType: 'EJB_ENTITY_BEAN',
        beanName,
        operation,
      },
    });
  }

  /**
   * Record servlet request
   */
  recordServletRequest(
    sessionId: string,
    servletName: string,
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    requestPath: string,
    parameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceServlets) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: {
        name: `Servlet:${servletName}`,
        parameters: { method, requestPath, ...parameters },
      },
      metadata: { 
        javaType: 'SERVLET',
        servletName,
        method,
        requestPath,
      },
    });
  }

  /**
   * Record servlet response
   */
  recordServletResponse(
    sessionId: string,
    servletName: string,
    statusCode: number,
    responseData: unknown,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceServlets) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_exit',
      location,
      data: {
        name: `Servlet:${servletName}`,
        returnValue: { statusCode, data: responseData },
      },
      metadata: { 
        javaType: 'SERVLET',
        statusCode,
      },
    });
  }

  /**
   * Record Struts Action execution
   */
  recordStrutsAction(
    sessionId: string,
    actionClass: string,
    actionPath: string,
    formBean: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceStruts) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: {
        name: `StrutsAction:${actionClass}`,
        parameters: { actionPath, formBean },
      },
      metadata: { 
        javaType: 'STRUTS_ACTION',
        actionClass,
        actionPath,
      },
    });
  }

  /**
   * Record Struts Action forward
   */
  recordStrutsForward(
    sessionId: string,
    actionClass: string,
    forwardName: string,
    forwardPath: string,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceStruts) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_exit',
      location,
      data: {
        name: `StrutsAction:${actionClass}`,
        returnValue: { forward: forwardName, path: forwardPath },
      },
      metadata: { 
        javaType: 'STRUTS_FORWARD',
        forwardName,
        forwardPath,
      },
    });
  }

  /**
   * Record JDBC query
   */
  recordJDBCQuery(
    sessionId: string,
    sql: string,
    parameters: unknown[],
    location: TraceLocation,
    statementType: 'Statement' | 'PreparedStatement' | 'CallableStatement'
  ): void {
    if (!this.javaConfig.traceJDBC) return;
    
    this.recordEvent(sessionId, {
      type: 'db_query',
      location,
      data: {
        query: sql,
        parameters: Object.fromEntries(parameters.map((p, i) => [`param${i}`, p])),
      },
      metadata: { 
        javaType: 'JDBC',
        statementType,
      },
    });
  }

  /**
   * Record JDBC result
   */
  recordJDBCResult(
    sessionId: string,
    sql: string,
    rowCount: number,
    results: unknown[],
    durationMs: number,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceJDBC) return;
    
    this.recordEvent(sessionId, {
      type: 'db_result',
      location,
      data: {
        query: sql,
        returnValue: results,
        recordCount: rowCount,
      },
      metadata: { 
        javaType: 'JDBC_RESULT',
        rowCount,
        durationMs,
      },
    });
    
    this.updateDatabaseIO(sessionId, sql, results, durationMs);
  }

  /**
   * Record JMS message send
   */
  recordJMSSend(
    sessionId: string,
    destination: string,
    destinationType: 'Queue' | 'Topic',
    message: unknown,
    correlationId: string | undefined,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceJMS) return;
    
    this.recordEvent(sessionId, {
      type: 'external_call',
      location,
      data: {
        name: `JMS:${destinationType}:${destination}`,
        parameters: { message, correlationId },
      },
      metadata: { 
        javaType: 'JMS_SEND',
        destination,
        destinationType,
        correlationId,
      },
    });
  }

  /**
   * Record JMS message receive
   */
  recordJMSReceive(
    sessionId: string,
    destination: string,
    destinationType: 'Queue' | 'Topic',
    message: unknown,
    correlationId: string | undefined,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceJMS) return;
    
    this.recordEvent(sessionId, {
      type: 'external_return',
      location,
      data: {
        name: `JMS:${destinationType}:${destination}`,
        returnValue: message,
      },
      metadata: { 
        javaType: 'JMS_RECEIVE',
        destination,
        destinationType,
        correlationId,
      },
    });
  }

  /**
   * Record JNDI lookup
   */
  recordJNDILookup(
    sessionId: string,
    jndiName: string,
    result: unknown,
    location: TraceLocation
  ): void {
    if (!this.javaConfig.traceJNDI) return;
    
    this.recordEvent(sessionId, {
      type: 'external_call',
      location,
      data: {
        name: `JNDI:lookup`,
        parameters: { name: jndiName },
        returnValue: result ? 'found' : 'not found',
      },
      metadata: { 
        javaType: 'JNDI_LOOKUP',
        jndiName,
        found: !!result,
      },
    });
  }

  /**
   * Record exception
   */
  recordException(
    sessionId: string,
    exceptionClass: string,
    message: string,
    stackTrace: string,
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: 'error',
      location,
      data: {
        errorCode: exceptionClass,
        errorMessage: message,
      },
      metadata: { 
        javaType: 'EXCEPTION',
        stackTrace,
      },
    });
  }

  /**
   * Record conditional branch (if/else, switch)
   */
  recordBranch(
    sessionId: string,
    condition: string,
    result: boolean,
    branchType: 'if' | 'else' | 'switch' | 'case',
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: result ? 'branch_taken' : 'branch_not_taken',
      location,
      data: { condition, conditionResult: result },
      metadata: { javaType: branchType.toUpperCase() },
    });
  }

  /**
   * Record loop iteration
   */
  recordLoop(
    sessionId: string,
    loopType: 'for' | 'while' | 'do-while' | 'for-each',
    iterationCount: number,
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: 'loop_iteration',
      location,
      data: {
        name: loopType,
        value: iterationCount,
      },
      metadata: { javaType: loopType.toUpperCase() },
    });
  }

  private updateDatabaseIO(
    sessionId: string,
    sql: string,
    results: unknown[],
    durationMs: number
  ): void {
    const state = this['activeTraces'].get(sessionId);
    if (!state) return;
    
    // Parse SQL to determine operation type
    const sqlUpper = sql.trim().toUpperCase();
    let operation: DatabaseIO['operation'] = 'select';
    if (sqlUpper.startsWith('INSERT')) operation = 'insert';
    else if (sqlUpper.startsWith('UPDATE')) operation = 'update';
    else if (sqlUpper.startsWith('DELETE')) operation = 'delete';
    else if (sqlUpper.startsWith('CALL') || sqlUpper.startsWith('{CALL')) operation = 'call';
    
    const dbIO: DatabaseIO = {
      operation,
      query: sql,
      results,
      rowCount: results.length,
      durationMs,
    };
    
    state.inputs.database.push(dbIO);
  }
}

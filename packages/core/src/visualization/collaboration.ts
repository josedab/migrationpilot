/**
 * Collaboration Manager
 * 
 * Manages collaborative sessions for SME review and annotation
 * of knowledge graphs during migration projects.
 */

import type {
  CollaborationSession,
  RuleDecision,
  CollaborationComment,
  CollaborationActivity,
  NodeAnnotation,
  ActivityType,
  ShareableView,
  ActiveFilters,
  LayoutAlgorithm,
  LayoutOptions,
  ViewState
} from './types.js';
import { generateId } from '../index.js';

/**
 * Manages collaboration sessions for knowledge graph review
 */
export class CollaborationManager {
  private sessions: Map<string, CollaborationSession> = new Map();
  private eventHandlers: Map<string, Set<CollaborationEventHandler>> = new Map();
  
  /**
   * Create a new collaboration session
   */
  createSession(
    projectId: string,
    graphId: string,
    name: string,
    creatorId: string,
    creatorName: string
  ): CollaborationSession {
    const session: CollaborationSession = {
      id: generateId(),
      projectId,
      graphId,
      name,
      participants: [
        {
          userId: creatorId,
          name: creatorName,
          role: 'admin',
          status: 'online',
          joinedAt: new Date(),
          lastSeenAt: new Date(),
        },
      ],
      status: 'active',
      annotations: [],
      decisions: [],
      comments: [],
      activities: [
        {
          id: generateId(),
          type: 'joined',
          actor: creatorId,
          timestamp: new Date(),
          details: { name: creatorName, role: 'admin' },
        },
      ],
      startedAt: new Date(),
      lastActivityAt: new Date(),
    };
    
    this.sessions.set(session.id, session);
    return session;
  }
  
  /**
   * Get a session by ID
   */
  getSession(sessionId: string): CollaborationSession | undefined {
    return this.sessions.get(sessionId);
  }
  
  /**
   * Join an existing session
   */
  joinSession(
    sessionId: string,
    userId: string,
    userName: string,
    role: 'admin' | 'reviewer' | 'viewer' = 'reviewer'
  ): CollaborationSession {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    // Check if user already in session
    const existing = session.participants.find(p => p.userId === userId);
    if (existing) {
      existing.status = 'online';
      existing.lastSeenAt = new Date();
    } else {
      session.participants.push({
        userId,
        name: userName,
        role,
        status: 'online',
        joinedAt: new Date(),
        lastSeenAt: new Date(),
      });
    }
    
    this.addActivity(sessionId, 'joined', userId, { name: userName, role });
    this.emitEvent(sessionId, 'participant-joined', { userId, userName, role });
    
    return session;
  }
  
  /**
   * Leave a session
   */
  leaveSession(sessionId: string, userId: string): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    const participant = session.participants.find(p => p.userId === userId);
    if (participant) {
      participant.status = 'offline';
      this.addActivity(sessionId, 'left', userId, {});
      this.emitEvent(sessionId, 'participant-left', { userId });
    }
  }
  
  /**
   * Update participant cursor position
   */
  updateCursor(sessionId: string, userId: string, x: number, y: number): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    const participant = session.participants.find(p => p.userId === userId);
    if (participant) {
      participant.cursor = { x, y };
      participant.lastSeenAt = new Date();
      this.emitEvent(sessionId, 'cursor-moved', { userId, x, y });
    }
  }
  
  /**
   * Update participant selection
   */
  updateSelection(sessionId: string, userId: string, nodeIds: string[]): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    const participant = session.participants.find(p => p.userId === userId);
    if (participant) {
      participant.selectedNodeIds = nodeIds;
      this.addActivity(sessionId, 'selected-node', userId, { nodeIds });
      this.emitEvent(sessionId, 'selection-changed', { userId, nodeIds });
    }
  }
  
  // ==========================================================================
  // ANNOTATIONS
  // ==========================================================================
  
  /**
   * Add an annotation to a node
   */
  addAnnotation(
    sessionId: string,
    nodeId: string,
    type: NodeAnnotation['type'],
    content: string,
    author: string
  ): NodeAnnotation {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    const annotation: NodeAnnotation = {
      id: generateId(),
      type,
      content,
      author,
      createdAt: new Date(),
      resolved: false,
    };
    
    session.annotations.push(annotation);
    this.addActivity(sessionId, 'added-annotation', author, { 
      nodeId, 
      annotationId: annotation.id,
      type 
    });
    this.emitEvent(sessionId, 'annotation-added', { nodeId, annotation });
    
    return annotation;
  }
  
  /**
   * Resolve an annotation
   */
  resolveAnnotation(
    sessionId: string,
    annotationId: string,
    resolvedBy: string
  ): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    const annotation = session.annotations.find(a => a.id === annotationId);
    if (annotation) {
      annotation.resolved = true;
      this.addActivity(sessionId, 'resolved-annotation', resolvedBy, { annotationId });
      this.emitEvent(sessionId, 'annotation-resolved', { annotationId, resolvedBy });
    }
  }
  
  /**
   * Get annotations for a node
   */
  getNodeAnnotations(sessionId: string, _nodeId: string): NodeAnnotation[] {
    const session = this.sessions.get(sessionId);
    if (!session) return [];
    
    // Note: In a full implementation, annotations would be linked to nodeIds
    // For now, return all annotations (the UI would filter by nodeId)
    return session.annotations;
  }
  
  // ==========================================================================
  // DECISIONS
  // ==========================================================================
  
  /**
   * Make a decision on a node (approve/reject/defer)
   */
  makeDecision(
    sessionId: string,
    nodeId: string,
    decision: RuleDecision['decision'],
    decidedBy: string,
    rationale?: string
  ): RuleDecision {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    // Check if there's an existing decision
    const existingIndex = session.decisions.findIndex(d => d.nodeId === nodeId);
    
    const ruleDecision: RuleDecision = {
      id: generateId(),
      nodeId,
      decision,
      rationale,
      decidedBy,
      decidedAt: new Date(),
    };
    
    if (existingIndex >= 0) {
      // Update existing decision
      const existing = session.decisions[existingIndex];
      if (existing) {
        ruleDecision.reviewedBy = existing.reviewedBy || [];
        if (!ruleDecision.reviewedBy.includes(decidedBy)) {
          ruleDecision.reviewedBy.push(decidedBy);
        }
        session.decisions[existingIndex] = ruleDecision;
        this.addActivity(sessionId, 'changed-decision', decidedBy, { 
          nodeId, 
          from: existing.decision, 
          to: decision 
        });
      }
    } else {
      session.decisions.push(ruleDecision);
      this.addActivity(sessionId, 'made-decision', decidedBy, { nodeId, decision });
    }
    
    this.emitEvent(sessionId, 'decision-made', { nodeId, decision: ruleDecision });
    
    return ruleDecision;
  }
  
  /**
   * Get decision for a node
   */
  getDecision(sessionId: string, nodeId: string): RuleDecision | undefined {
    const session = this.sessions.get(sessionId);
    return session?.decisions.find(d => d.nodeId === nodeId);
  }
  
  /**
   * Get all decisions in a session
   */
  getAllDecisions(sessionId: string): RuleDecision[] {
    const session = this.sessions.get(sessionId);
    return session?.decisions || [];
  }
  
  /**
   * Get decision statistics for a session
   */
  getDecisionStats(sessionId: string): DecisionStats {
    const session = this.sessions.get(sessionId);
    if (!session) {
      return { total: 0, approved: 0, rejected: 0, needsClarification: 0, deferred: 0 };
    }
    
    return {
      total: session.decisions.length,
      approved: session.decisions.filter(d => d.decision === 'approved').length,
      rejected: session.decisions.filter(d => d.decision === 'rejected').length,
      needsClarification: session.decisions.filter(d => d.decision === 'needs_clarification').length,
      deferred: session.decisions.filter(d => d.decision === 'deferred').length,
    };
  }
  
  // ==========================================================================
  // COMMENTS
  // ==========================================================================
  
  /**
   * Add a comment to the session
   */
  addComment(
    sessionId: string,
    content: string,
    author: string,
    nodeId?: string,
    edgeId?: string
  ): CollaborationComment {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    const comment: CollaborationComment = {
      id: generateId(),
      nodeId,
      edgeId,
      content,
      author,
      createdAt: new Date(),
      replies: [],
      resolved: false,
    };
    
    session.comments.push(comment);
    this.addActivity(sessionId, 'added-comment', author, { 
      commentId: comment.id, 
      nodeId, 
      edgeId 
    });
    this.emitEvent(sessionId, 'comment-added', { comment });
    
    return comment;
  }
  
  /**
   * Reply to a comment
   */
  replyToComment(
    sessionId: string,
    commentId: string,
    content: string,
    author: string
  ): CollaborationComment {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    const parentComment = session.comments.find(c => c.id === commentId);
    if (!parentComment) {
      throw new Error(`Comment ${commentId} not found`);
    }
    
    const reply: CollaborationComment = {
      id: generateId(),
      content,
      author,
      createdAt: new Date(),
      replies: [],
      resolved: false,
    };
    
    parentComment.replies.push(reply);
    this.addActivity(sessionId, 'replied-comment', author, { 
      parentCommentId: commentId, 
      replyId: reply.id 
    });
    this.emitEvent(sessionId, 'comment-replied', { parentCommentId: commentId, reply });
    
    return reply;
  }
  
  /**
   * Resolve a comment
   */
  resolveComment(sessionId: string, commentId: string, resolvedBy: string): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    const comment = session.comments.find(c => c.id === commentId);
    if (comment) {
      comment.resolved = true;
      this.addActivity(sessionId, 'resolved-comment', resolvedBy, { commentId });
      this.emitEvent(sessionId, 'comment-resolved', { commentId, resolvedBy });
    }
  }
  
  /**
   * Get comments for a specific node
   */
  getNodeComments(sessionId: string, nodeId: string): CollaborationComment[] {
    const session = this.sessions.get(sessionId);
    return session?.comments.filter(c => c.nodeId === nodeId) || [];
  }
  
  // ==========================================================================
  // SESSION MANAGEMENT
  // ==========================================================================
  
  /**
   * Pause a session
   */
  pauseSession(sessionId: string): void {
    const session = this.sessions.get(sessionId);
    if (session) {
      session.status = 'paused';
      this.emitEvent(sessionId, 'session-paused', {});
    }
  }
  
  /**
   * Resume a paused session
   */
  resumeSession(sessionId: string): void {
    const session = this.sessions.get(sessionId);
    if (session && session.status === 'paused') {
      session.status = 'active';
      this.emitEvent(sessionId, 'session-resumed', {});
    }
  }
  
  /**
   * Complete a session
   */
  completeSession(sessionId: string): CollaborationSession {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    session.status = 'completed';
    session.completedAt = new Date();
    this.emitEvent(sessionId, 'session-completed', {});
    
    return session;
  }
  
  /**
   * Get session summary
   */
  getSessionSummary(sessionId: string): SessionSummary {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }
    
    const decisions = this.getDecisionStats(sessionId);
    
    return {
      id: session.id,
      name: session.name,
      status: session.status,
      participantCount: session.participants.length,
      activeParticipants: session.participants.filter(p => p.status === 'online').length,
      annotationCount: session.annotations.length,
      unresolvedAnnotations: session.annotations.filter(a => !a.resolved).length,
      commentCount: session.comments.length,
      unresolvedComments: session.comments.filter(c => !c.resolved).length,
      decisions,
      duration: session.completedAt 
        ? session.completedAt.getTime() - session.startedAt.getTime()
        : Date.now() - session.startedAt.getTime(),
      startedAt: session.startedAt,
      completedAt: session.completedAt,
    };
  }
  
  // ==========================================================================
  // SHAREABLE VIEWS
  // ==========================================================================
  
  /**
   * Create a shareable view of the graph
   */
  createShareableView(
    name: string,
    description: string | undefined,
    filters: ActiveFilters,
    layout: LayoutAlgorithm,
    layoutOptions: LayoutOptions,
    viewState: ViewState,
    createdBy: string,
    accessType: 'public' | 'link' | 'authenticated' = 'link',
    expiresInDays?: number
  ): ShareableView {
    const view: ShareableView = {
      id: generateId(),
      name,
      description,
      filters,
      layout,
      layoutOptions,
      viewState,
      accessType,
      accessLink: accessType !== 'public' ? generateShareLink() : undefined,
      expiresAt: expiresInDays ? new Date(Date.now() + expiresInDays * 24 * 60 * 60 * 1000) : undefined,
      createdBy,
      createdAt: new Date(),
    };
    
    // Would be persisted in database
    return view;
  }
  
  // ==========================================================================
  // EVENTS
  // ==========================================================================
  
  /**
   * Subscribe to collaboration events
   */
  on(sessionId: string, handler: CollaborationEventHandler): void {
    if (!this.eventHandlers.has(sessionId)) {
      this.eventHandlers.set(sessionId, new Set());
    }
    this.eventHandlers.get(sessionId)!.add(handler);
  }
  
  /**
   * Unsubscribe from collaboration events
   */
  off(sessionId: string, handler: CollaborationEventHandler): void {
    this.eventHandlers.get(sessionId)?.delete(handler);
  }
  
  private emitEvent(sessionId: string, type: string, data: Record<string, unknown>): void {
    const handlers = this.eventHandlers.get(sessionId);
    if (handlers) {
      const event: CollaborationEvent = { type, sessionId, data, timestamp: new Date() };
      handlers.forEach(handler => handler(event));
    }
  }
  
  // ==========================================================================
  // ACTIVITY TRACKING
  // ==========================================================================
  
  private addActivity(
    sessionId: string, 
    type: ActivityType, 
    actor: string, 
    details: Record<string, unknown>
  ): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;
    
    session.activities.push({
      id: generateId(),
      type,
      actor,
      timestamp: new Date(),
      details,
    });
    
    session.lastActivityAt = new Date();
  }
  
  /**
   * Get activity timeline for a session
   */
  getActivityTimeline(
    sessionId: string, 
    limit = 50, 
    offset = 0
  ): CollaborationActivity[] {
    const session = this.sessions.get(sessionId);
    if (!session) return [];
    
    return session.activities
      .slice()
      .reverse()
      .slice(offset, offset + limit);
  }
}

// ==========================================================================
// HELPER TYPES & FUNCTIONS
// ==========================================================================

export interface DecisionStats {
  total: number;
  approved: number;
  rejected: number;
  needsClarification: number;
  deferred: number;
}

export interface SessionSummary {
  id: string;
  name: string;
  status: 'active' | 'paused' | 'completed';
  participantCount: number;
  activeParticipants: number;
  annotationCount: number;
  unresolvedAnnotations: number;
  commentCount: number;
  unresolvedComments: number;
  decisions: DecisionStats;
  duration: number;
  startedAt: Date;
  completedAt?: Date;
}

export interface CollaborationEvent {
  type: string;
  sessionId: string;
  data: Record<string, unknown>;
  timestamp: Date;
}

export type CollaborationEventHandler = (event: CollaborationEvent) => void;

function generateShareLink(): string {
  const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let result = '';
  for (let i = 0; i < 32; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

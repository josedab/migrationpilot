/**
 * Knowledge Capture Service
 * Captures, stores, and retrieves institutional knowledge from SME interactions
 */

import type {
  KnowledgeEntry,
  KnowledgeType,
  KnowledgeSource,
  RelatedEntity,
  KnowledgeSearchQuery,
  IKnowledgeCaptureService,
} from '../types';

export interface KnowledgeStats {
  totalEntries: number;
  byType: Record<KnowledgeType, number>;
  byStatus: Record<string, number>;
  averageConfidence: number;
  verificationRate: number;
}

export class KnowledgeCaptureService implements IKnowledgeCaptureService {
  private entries: Map<string, KnowledgeEntry> = new Map();
  private tagIndex: Map<string, Set<string>> = new Map();
  private entityIndex: Map<string, Set<string>> = new Map();

  /**
   * Create a new knowledge entry
   */
  async create(
    entry: Omit<KnowledgeEntry, 'id' | 'createdAt' | 'updatedAt'>
  ): Promise<KnowledgeEntry> {
    const id = `ke_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const now = new Date().toISOString();

    const fullEntry: KnowledgeEntry = {
      ...entry,
      id,
      createdAt: now,
      updatedAt: now,
    };

    this.entries.set(id, fullEntry);
    this.indexEntry(fullEntry);

    return fullEntry;
  }

  /**
   * Update an existing knowledge entry
   */
  async update(
    id: string,
    updates: Partial<KnowledgeEntry>
  ): Promise<KnowledgeEntry> {
    const entry = this.entries.get(id);
    if (!entry) {
      throw new Error(`Knowledge entry not found: ${id}`);
    }

    // Remove from old indices
    this.removeFromIndices(entry);

    const updatedEntry: KnowledgeEntry = {
      ...entry,
      ...updates,
      id, // Prevent ID changes
      updatedAt: new Date().toISOString(),
    };

    this.entries.set(id, updatedEntry);
    this.indexEntry(updatedEntry);

    return updatedEntry;
  }

  /**
   * Verify a knowledge entry
   */
  async verify(id: string, verifierId: string): Promise<KnowledgeEntry> {
    const entry = this.entries.get(id);
    if (!entry) {
      throw new Error(`Knowledge entry not found: ${id}`);
    }

    return this.update(id, {
      status: 'verified',
      verifiedBy: verifierId,
      verifiedAt: new Date().toISOString(),
    });
  }

  /**
   * Search knowledge entries
   */
  async search(query: KnowledgeSearchQuery): Promise<KnowledgeEntry[]> {
    let results = Array.from(this.entries.values())
      .filter(e => e.projectId === query.projectId);

    // Filter by types
    if (query.types && query.types.length > 0) {
      results = results.filter(e => query.types!.includes(e.type));
    }

    // Filter by tags
    if (query.tags && query.tags.length > 0) {
      results = results.filter(e =>
        query.tags!.some(tag => e.tags.includes(tag))
      );
    }

    // Filter by status
    if (query.status) {
      results = results.filter(e => e.status === query.status);
    }

    // Text search
    if (query.query) {
      const searchTerms = query.query.toLowerCase().split(' ');
      results = results.filter(e => {
        const searchText = `${e.title} ${e.content}`.toLowerCase();
        return searchTerms.every(term => searchText.includes(term));
      });
    }

    // Sort by relevance (confidence * recency)
    results.sort((a, b) => {
      const aScore = a.confidence * (1 / (Date.now() - new Date(a.updatedAt).getTime()));
      const bScore = b.confidence * (1 / (Date.now() - new Date(b.updatedAt).getTime()));
      return bScore - aScore;
    });

    // Apply pagination
    const offset = query.offset || 0;
    const limit = query.limit || 50;
    return results.slice(offset, offset + limit);
  }

  /**
   * Get knowledge entries related to an entity
   */
  async getRelated(entityId: string, _entityType: string): Promise<KnowledgeEntry[]> {
    const entryIds = this.entityIndex.get(entityId);
    if (!entryIds) return [];

    return Array.from(entryIds)
      .map(id => this.entries.get(id))
      .filter((e): e is KnowledgeEntry => e !== undefined)
      .sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Capture knowledge from an SME interview
   */
  async captureFromInterview(
    projectId: string,
    smeId: string,
    topic: string,
    content: string,
    relatedEntities: RelatedEntity[] = [],
    tags: string[] = []
  ): Promise<KnowledgeEntry> {
    const source: KnowledgeSource = {
      type: 'sme-interview',
      capturedAt: new Date().toISOString(),
      capturedBy: smeId,
    };

    // Auto-detect knowledge type based on content
    const type = this.detectKnowledgeType(content);

    return this.create({
      projectId,
      type,
      title: topic,
      content,
      source,
      tags: [...tags, 'interview'],
      relatedEntities,
      confidence: 0.9, // High confidence for direct SME input
      status: 'draft',
      createdBy: smeId,
    });
  }

  /**
   * Capture knowledge from a document
   */
  async captureFromDocument(
    projectId: string,
    capturedBy: string,
    documentRef: string,
    title: string,
    content: string,
    type: KnowledgeType,
    tags: string[] = []
  ): Promise<KnowledgeEntry> {
    const source: KnowledgeSource = {
      type: 'document',
      reference: documentRef,
      capturedAt: new Date().toISOString(),
      capturedBy,
    };

    return this.create({
      projectId,
      type,
      title,
      content,
      source,
      tags: [...tags, 'document'],
      relatedEntities: [],
      confidence: 0.7, // Medium confidence for document extraction
      status: 'draft',
      createdBy: capturedBy,
    });
  }

  /**
   * Capture knowledge from a chat/discussion
   */
  async captureFromChat(
    projectId: string,
    capturedBy: string,
    discussionId: string,
    title: string,
    content: string,
    relatedEntities: RelatedEntity[] = []
  ): Promise<KnowledgeEntry> {
    const source: KnowledgeSource = {
      type: 'chat',
      reference: discussionId,
      capturedAt: new Date().toISOString(),
      capturedBy,
    };

    return this.create({
      projectId,
      type: this.detectKnowledgeType(content),
      title,
      content,
      source,
      tags: ['chat', 'discussion'],
      relatedEntities,
      confidence: 0.6, // Lower confidence for chat extraction
      status: 'draft',
      createdBy: capturedBy,
    });
  }

  /**
   * Link knowledge entry to entities
   */
  async linkToEntities(
    knowledgeId: string,
    entities: RelatedEntity[]
  ): Promise<KnowledgeEntry> {
    const entry = this.entries.get(knowledgeId);
    if (!entry) {
      throw new Error(`Knowledge entry not found: ${knowledgeId}`);
    }

    const existingIds = new Set(entry.relatedEntities.map(e => e.id));
    const newEntities = entities.filter(e => !existingIds.has(e.id));

    return this.update(knowledgeId, {
      relatedEntities: [...entry.relatedEntities, ...newEntities],
    });
  }

  /**
   * Merge duplicate or related knowledge entries
   */
  async merge(
    primaryId: string,
    secondaryIds: string[],
    mergedContent?: string
  ): Promise<KnowledgeEntry> {
    const primary = this.entries.get(primaryId);
    if (!primary) {
      throw new Error(`Primary knowledge entry not found: ${primaryId}`);
    }

    const secondaries = secondaryIds
      .map(id => this.entries.get(id))
      .filter((e): e is KnowledgeEntry => e !== undefined);

    // Merge tags
    const allTags = new Set([
      ...primary.tags,
      ...secondaries.flatMap(s => s.tags),
    ]);

    // Merge related entities
    const allEntities = new Map<string, RelatedEntity>();
    for (const entity of [...primary.relatedEntities, ...secondaries.flatMap(s => s.relatedEntities)]) {
      allEntities.set(entity.id, entity);
    }

    // Merge content if not provided
    const content = mergedContent || [
      primary.content,
      ...secondaries.map(s => `\n---\n${s.content}`),
    ].join('');

    // Update primary with merged data
    const merged = await this.update(primaryId, {
      content,
      tags: Array.from(allTags),
      relatedEntities: Array.from(allEntities.values()),
      confidence: Math.max(primary.confidence, ...secondaries.map(s => s.confidence)),
    });

    // Mark secondaries as deprecated
    for (const secondary of secondaries) {
      await this.update(secondary.id, { status: 'deprecated' });
    }

    return merged;
  }

  /**
   * Get statistics for knowledge entries
   */
  getStats(projectId: string): KnowledgeStats {
    const projectEntries = Array.from(this.entries.values())
      .filter(e => e.projectId === projectId);

    const byType: Record<string, number> = {};
    const byStatus: Record<string, number> = {};
    let totalConfidence = 0;
    let verifiedCount = 0;

    for (const entry of projectEntries) {
      byType[entry.type] = (byType[entry.type] || 0) + 1;
      byStatus[entry.status] = (byStatus[entry.status] || 0) + 1;
      totalConfidence += entry.confidence;
      if (entry.status === 'verified') verifiedCount++;
    }

    return {
      totalEntries: projectEntries.length,
      byType: byType as Record<KnowledgeType, number>,
      byStatus,
      averageConfidence: projectEntries.length > 0 
        ? totalConfidence / projectEntries.length 
        : 0,
      verificationRate: projectEntries.length > 0 
        ? verifiedCount / projectEntries.length 
        : 0,
    };
  }

  /**
   * Export all knowledge for a project
   */
  exportForProject(projectId: string): KnowledgeEntry[] {
    return Array.from(this.entries.values())
      .filter(e => e.projectId === projectId && e.status !== 'deprecated')
      .sort((a, b) => a.type.localeCompare(b.type));
  }

  private indexEntry(entry: KnowledgeEntry): void {
    // Index by tags
    for (const tag of entry.tags) {
      if (!this.tagIndex.has(tag)) {
        this.tagIndex.set(tag, new Set());
      }
      this.tagIndex.get(tag)!.add(entry.id);
    }

    // Index by related entities
    for (const entity of entry.relatedEntities) {
      if (!this.entityIndex.has(entity.id)) {
        this.entityIndex.set(entity.id, new Set());
      }
      this.entityIndex.get(entity.id)!.add(entry.id);
    }
  }

  private removeFromIndices(entry: KnowledgeEntry): void {
    for (const tag of entry.tags) {
      this.tagIndex.get(tag)?.delete(entry.id);
    }
    for (const entity of entry.relatedEntities) {
      this.entityIndex.get(entity.id)?.delete(entry.id);
    }
  }

  private detectKnowledgeType(content: string): KnowledgeType {
    const lower = content.toLowerCase();

    if (lower.includes('calculate') || lower.includes('formula') || lower.includes('computation')) {
      return 'business-rule-explanation';
    }
    if (lower.includes('exception') || lower.includes('edge case') || lower.includes('special case')) {
      return 'exception-case';
    }
    if (lower.includes('workaround') || lower.includes('hack') || lower.includes('temporary')) {
      return 'workaround';
    }
    if (lower.includes('process') || lower.includes('workflow') || lower.includes('steps')) {
      return 'process-flow';
    }
    if (lower.includes('history') || lower.includes('legacy') || lower.includes('originally')) {
      return 'historical-context';
    }
    if (lower.includes('define') || lower.includes('means') || lower.includes('terminology')) {
      return 'terminology';
    }

    return 'domain-concept';
  }
}

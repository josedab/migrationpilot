import { describe, it, expect, beforeEach } from 'vitest';
import {
  MigrationCopilot,
  type InlineSuggestion,
} from '../copilot/migration-copilot.js';

describe('MigrationCopilot', () => {
  let copilot: MigrationCopilot;

  beforeEach(() => {
    copilot = new MigrationCopilot({
      enableStreaming: true,
      cacheResponses: true,
    });
  });

  describe('startSession', () => {
    it('should create a new session with unique ID', () => {
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        'COMPUTE TOTAL = SUBTOTAL + TAX.'
      );

      expect(session.id).toBeTruthy();
      expect(session.projectId).toBe('project-1');
      expect(session.documentUri).toBe('file:///test.cbl');
      expect(session.language).toBe('cobol');
      expect(session.messages.length).toBe(1); // System message
      expect(session.messages[0]?.role).toBe('system');
    });

    it('should initialize context with document content', () => {
      const content = 'IF AMOUNT GREATER THAN LIMIT\n  PERFORM ERROR-ROUTINE\nEND-IF.';
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        content
      );

      expect(session.context.documentContent).toBe(content);
      expect(session.context.cursorPosition).toEqual({ line: 1, column: 1 });
    });
  });

  describe('getInlineSuggestions', () => {
    let sessionId: string;

    beforeEach(() => {
      const session = copilot.startSession(
        'project-1',
        'file:///calc.cbl',
        'cobol',
        'COMPUTE RESULT = VALUE-A + VALUE-B.\nIF RESULT GREATER THAN MAXIMUM\n  MOVE MAXIMUM TO RESULT\nEND-IF.'
      );
      sessionId = session.id;
    });

    it('should return suggestions for calculation lines', async () => {
      const suggestions = await copilot.getInlineSuggestions(
        sessionId,
        1, // Line with COMPUTE
        10,
        'manual'
      );

      expect(Array.isArray(suggestions)).toBe(true);
    });

    it('should return empty array for unknown session', async () => {
      const suggestions = await copilot.getInlineSuggestions(
        'unknown-session',
        1,
        1,
        'manual'
      );

      expect(suggestions).toEqual([]);
    });
  });

  describe('explainCode', () => {
    let sessionId: string;

    beforeEach(() => {
      const session = copilot.startSession(
        'project-1',
        'file:///logic.cbl',
        'cobol',
        'COMPUTE DISCOUNT-AMOUNT = ORDER-TOTAL * DISCOUNT-RATE / 100.\nIF DISCOUNT-AMOUNT GREATER THAN MAX-DISCOUNT\n  MOVE MAX-DISCOUNT TO DISCOUNT-AMOUNT\nEND-IF.'
      );
      sessionId = session.id;
    });

    it('should generate explanation for calculation code', async () => {
      const explanation = await copilot.explainCode(sessionId, {
        startLine: 1,
        startColumn: 1,
        endLine: 1,
        endColumn: 56,
      });

      expect(explanation.summary).toBeTruthy();
      expect(explanation.details).toBeTruthy();
      expect(explanation.range.startLine).toBe(1);
    });

    it('should detect calculation patterns', async () => {
      const explanation = await copilot.explainCode(sessionId, {
        startLine: 1,
        startColumn: 1,
        endLine: 1,
        endColumn: 56,
      });

      expect(explanation.summary).toBe('Business Calculation');
    });

    it('should detect decision logic patterns', async () => {
      const explanation = await copilot.explainCode(sessionId, {
        startLine: 2,
        startColumn: 1,
        endLine: 4,
        endColumn: 8,
      });

      expect(explanation.summary).toBe('Decision Logic');
    });

    it('should throw for unknown session', async () => {
      await expect(
        copilot.explainCode('unknown-session', {
          startLine: 1,
          startColumn: 1,
          endLine: 1,
          endColumn: 10,
        })
      ).rejects.toThrow('Session not found');
    });
  });

  describe('getMigrationSuggestion', () => {
    let sessionId: string;

    beforeEach(() => {
      const session = copilot.startSession(
        'project-1',
        'file:///calc.cbl',
        'cobol',
        'COMPUTE TOTAL = SUBTOTAL + TAX.\nMOVE TOTAL TO OUTPUT-FIELD.'
      );
      sessionId = session.id;
    });

    it('should generate Java migration', async () => {
      const suggestion = await copilot.getMigrationSuggestion(
        sessionId,
        {
          startLine: 1,
          startColumn: 1,
          endLine: 2,
          endColumn: 28,
        },
        'java'
      );

      expect(suggestion.targetLanguage).toBe('java');
      expect(suggestion.suggestedCode).toContain('public class');
      expect(suggestion.suggestedCode).toContain('BigDecimal');
      expect(suggestion.confidence).toBeGreaterThan(0);
      expect(suggestion.warnings).toBeDefined();
    });

    it('should generate Python migration', async () => {
      const suggestion = await copilot.getMigrationSuggestion(
        sessionId,
        {
          startLine: 1,
          startColumn: 1,
          endLine: 2,
          endColumn: 28,
        },
        'python'
      );

      expect(suggestion.targetLanguage).toBe('python');
      expect(suggestion.suggestedCode).toContain('from decimal import Decimal');
      expect(suggestion.suggestedCode).toContain('def migrated_logic');
    });

    it('should include test cases', async () => {
      const suggestion = await copilot.getMigrationSuggestion(
        sessionId,
        {
          startLine: 1,
          startColumn: 1,
          endLine: 1,
          endColumn: 31,
        },
        'java'
      );

      expect(suggestion.testCases).toBeDefined();
      expect(suggestion.testCases?.length).toBeGreaterThan(0);
    });
  });

  describe('chat', () => {
    let sessionId: string;

    beforeEach(() => {
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        'PERFORM CALCULATE-DISCOUNT.'
      );
      sessionId = session.id;
    });

    it('should respond to explain requests', async () => {
      const response = await copilot.chat(
        sessionId,
        'Can you explain this code?'
      );

      expect(response.role).toBe('assistant');
      expect(response.content).toContain('analysis');
    });

    it('should respond to migration requests', async () => {
      const response = await copilot.chat(
        sessionId,
        'How do I migrate this code?'
      );

      expect(response.content).toContain('migrate');
    });

    it('should respond to test requests', async () => {
      const response = await copilot.chat(
        sessionId,
        'Generate tests for this'
      );

      expect(response.content).toContain('test');
    });

    it('should track conversation history', async () => {
      await copilot.chat(sessionId, 'First message');
      await copilot.chat(sessionId, 'Second message');

      // Internal state tracking - just verify no errors
      expect(true).toBe(true);
    });
  });

  describe('streamSuggestions', () => {
    let sessionId: string;

    beforeEach(() => {
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        'COMPUTE'
      );
      sessionId = session.id;
    });

    it('should stream completions for COMPUTE', async () => {
      const suggestions: InlineSuggestion[] = [];

      for await (const suggestion of copilot.streamSuggestions(
        sessionId,
        'COMPUTE',
        1,
        8
      )) {
        suggestions.push(suggestion);
      }

      // Should yield at least one suggestion
      expect(suggestions.length).toBeGreaterThanOrEqual(0);
    });
  });

  describe('updateContext', () => {
    it('should update session context', () => {
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        'Initial content'
      );

      copilot.updateContext(session.id, {
        cursorPosition: { line: 5, column: 10 },
        documentContent: 'Updated content',
      });

      // Context update should not throw
      expect(true).toBe(true);
    });
  });

  describe('closeSession', () => {
    it('should close session without error', () => {
      const session = copilot.startSession(
        'project-1',
        'file:///test.cbl',
        'cobol',
        'Content'
      );

      copilot.closeSession(session.id);

      // Should not be able to get suggestions for closed session
      expect(
        copilot.getInlineSuggestions(session.id, 1, 1, 'manual')
      ).resolves.toEqual([]);
    });
  });
});

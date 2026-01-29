/**
 * Tests for ExplainerAgent
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { ExplainerAgent } from '../explainer/agent.js';

// Mock the Copilot client
vi.mock('../common/copilot-client.js', () => ({
  getCopilotClient: () => ({
    executeWithTools: vi.fn().mockResolvedValue({
      content: JSON.stringify({
        summary: 'This COBOL program calculates loan interest.',
        detailedExplanation: 'The program reads customer records and applies interest calculations.',
        businessPurpose: 'Monthly interest calculation for customer loans.',
        keyOperations: [
          { name: 'CALC-INTEREST', description: 'Calculate interest', location: { startLine: 50, endLine: 80 }, importance: 'critical' }
        ],
        dataFlow: [
          { step: 1, description: 'Read customer record', inputs: ['CUSTOMER-FILE'], outputs: ['WS-CUSTOMER-REC'] }
        ],
        edgeCases: ['Zero balance accounts', 'Negative interest rates'],
        assumptions: ['Interest rate is annual'],
        confidence: 0.92,
        suggestedQuestions: ['What happens with zero balance?', 'How is compound interest calculated?']
      }),
      toolCalls: []
    })
  })
}));

describe('ExplainerAgent', () => {
  let agent: ExplainerAgent;

  beforeEach(() => {
    agent = new ExplainerAgent();
  });

  describe('explainCode', () => {
    it('should explain COBOL code and return structured result', async () => {
      const context = {
        projectId: 'test-project',
        sessionId: 'test-session',
        userId: 'test-user',
      };

      const cobolCode = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL    PIC 9(9)V99.
       01 WS-RATE         PIC 9(2)V9(4).
       01 WS-INTEREST     PIC 9(9)V99.
       
       PROCEDURE DIVISION.
       CALC-INTEREST.
           COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE.
           STOP RUN.
      `;

      const result = await agent.explainCode(context, cobolCode, 'cobol', 'CALCINT.cbl');

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data).toHaveProperty('summary');
      expect(result.data).toHaveProperty('businessPurpose');
      expect(result.data).toHaveProperty('confidence');
    });

    it('should return error for uninitialized agent', async () => {
      const uninitializedAgent = new ExplainerAgent();
      
      // Calling send without initialize should throw
      await expect(uninitializedAgent['send']('test')).rejects.toThrow();
    });
  });

  describe('Q&A Session Management', () => {
    it('should start a new Q&A session', () => {
      const session = agent.startSession('session-1', 'COBOL code', 'cobol', 'test.cbl');

      expect(session.sessionId).toBe('session-1');
      expect(session.language).toBe('cobol');
      expect(session.filename).toBe('test.cbl');
      expect(session.conversationHistory).toHaveLength(0);
    });

    it('should retrieve an existing session', () => {
      agent.startSession('session-2', 'COBOL code', 'cobol', 'test.cbl');
      
      const retrieved = agent.getQASession('session-2');
      expect(retrieved).toBeDefined();
      expect(retrieved?.sessionId).toBe('session-2');
    });

    it('should return undefined for non-existent session', () => {
      const retrieved = agent.getQASession('non-existent');
      expect(retrieved).toBeUndefined();
    });
  });

  describe('explainSection', () => {
    it('should explain a specific code section with context', async () => {
      const context = {
        projectId: 'test-project',
        sessionId: 'test-session',
        userId: 'test-user',
      };

      const cobolCode = `
       01 LINE-1.
       02 LINE-2.
       03 LINE-3.
       04 LINE-4.
       05 LINE-5.
       06 LINE-6.
       07 LINE-7.
       08 LINE-8.
       09 LINE-9.
       10 LINE-10.
       11 TARGET-START.
       12 TARGET-MID.
       13 TARGET-END.
       14 LINE-14.
       15 LINE-15.
      `;

      const result = await agent.explainSection(
        context,
        cobolCode,
        'cobol',
        'test.cbl',
        11,
        13
      );

      expect(result.success).toBe(true);
    });
  });

  describe('agent type', () => {
    it('should return archeologist type', () => {
      expect(agent.type).toBe('archeologist');
    });
  });
});

describe('ExplainerAgent - Executive Summary', () => {
  let agent: ExplainerAgent;

  beforeEach(() => {
    agent = new ExplainerAgent();
  });

  it('should generate executive summary for non-technical stakeholders', async () => {
    const context = {
      projectId: 'test-project',
      sessionId: 'test-session',
      userId: 'test-user',
    };

    const result = await agent.generateExecutiveSummary(
      context,
      'COBOL code here',
      'cobol',
      'MAINPROG.cbl'
    );

    expect(result.success).toBe(true);
  });
});

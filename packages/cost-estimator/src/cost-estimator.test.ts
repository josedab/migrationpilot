/**
 * Cost Estimator Tests
 */

import { describe, it, expect } from 'vitest';
import { CostEstimatorService } from './cost-estimator.js';
import { LanguageDetector } from './analyzers/language-detector.js';
import { CodeAnalyzer } from './analyzers/code-analyzer.js';

const SAMPLE_COBOL = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL    PIC 9(10)V99.
       01 WS-RATE         PIC 9(3)V9(4).
       01 WS-YEARS        PIC 9(2).
       01 WS-INTEREST     PIC 9(12)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM CALCULATE-INTEREST
           DISPLAY "Interest: " WS-INTEREST
           STOP RUN.

       CALCULATE-INTEREST.
           IF WS-YEARS > 0
               COMPUTE WS-INTEREST =
                   WS-PRINCIPAL * WS-RATE * WS-YEARS
           ELSE
               MOVE 0 TO WS-INTEREST
           END-IF.
`;

const SAMPLE_FORTRAN = `
      PROGRAM INTEREST
      REAL PRINCIPAL, RATE, YEARS, RESULT

      PRINCIPAL = 1000.0
      RATE = 0.05
      YEARS = 5.0

      IF (YEARS .GT. 0) THEN
          RESULT = PRINCIPAL * RATE * YEARS
      ELSE
          RESULT = 0.0
      END IF

      WRITE(*,*) 'Interest:', RESULT

      END PROGRAM
`;

const SAMPLE_VB6 = `
Option Explicit

Private Sub CalculateInterest()
    Dim principal As Double
    Dim rate As Double
    Dim years As Integer
    Dim interest As Double

    principal = 1000
    rate = 0.05
    years = 5

    If years > 0 Then
        interest = principal * rate * years
    Else
        interest = 0
    End If

    MsgBox "Interest: " & interest
End Sub
`;

describe('LanguageDetector', () => {
  const detector = new LanguageDetector();

  it('should detect COBOL code', () => {
    const result = detector.detect(SAMPLE_COBOL);
    expect(result.language).toBe('cobol');
    expect(result.confidence).toBeGreaterThan(0.5);
  });

  it('should detect Fortran code', () => {
    const result = detector.detect(SAMPLE_FORTRAN);
    expect(result.language).toBe('fortran');
    expect(result.confidence).toBeGreaterThan(0.5);
  });

  it('should detect VB6 code', () => {
    const result = detector.detect(SAMPLE_VB6);
    expect(result.language).toBe('vb6');
    expect(result.confidence).toBeGreaterThan(0.5);
  });
});

describe('CodeAnalyzer', () => {
  const analyzer = new CodeAnalyzer();

  it('should analyze COBOL code metrics', () => {
    const metrics = analyzer.analyze({ content: SAMPLE_COBOL, language: 'cobol' });

    expect(metrics.totalLines).toBeGreaterThan(0);
    expect(metrics.codeLines).toBeGreaterThan(0);
    expect(metrics.procedures).toBeGreaterThan(0);
    expect(metrics.dataStructures).toBeGreaterThan(0);
  });

  it('should calculate complexity for COBOL', () => {
    const metrics = analyzer.analyze({ content: SAMPLE_COBOL, language: 'cobol' });
    const complexity = analyzer.calculateComplexity(metrics, 'cobol');

    expect(complexity.overall).toBeGreaterThan(0);
    expect(complexity.level).toBeDefined();
    expect(['trivial', 'low', 'medium', 'high', 'very_high', 'extreme']).toContain(complexity.level);
  });

  it('should analyze Fortran code metrics', () => {
    const metrics = analyzer.analyze({ content: SAMPLE_FORTRAN, language: 'fortran' });

    expect(metrics.totalLines).toBeGreaterThan(0);
    expect(metrics.codeLines).toBeGreaterThan(0);
    expect(metrics.cyclomaticComplexity).toBeGreaterThan(0);
  });
});

describe('CostEstimatorService', () => {
  const service = new CostEstimatorService();

  describe('quickEstimate', () => {
    it('should provide quick estimate for COBOL', async () => {
      const result = await service.quickEstimate({ code: SAMPLE_COBOL });

      expect(result.linesOfCode).toBeGreaterThan(0);
      expect(result.detectedLanguage).toBe('cobol');
      expect(result.complexityScore).toBeGreaterThan(0);
      expect(result.estimatedDays.min).toBeGreaterThan(0);
      expect(result.estimatedDays.max).toBeGreaterThanOrEqual(result.estimatedDays.min);
      expect(result.estimatedCost.min).toBeGreaterThan(0);
      expect(result.recommendation).toBeDefined();
    });

    it('should respect explicit language parameter', async () => {
      const result = await service.quickEstimate({
        code: SAMPLE_COBOL,
        sourceLanguage: 'cobol',
      });

      expect(result.detectedLanguage).toBe('cobol');
    });
  });

  describe('estimate', () => {
    it('should provide full estimate with breakdown', async () => {
      const result = await service.estimate({
        code: { content: SAMPLE_COBOL },
      });

      expect(result.id).toBeDefined();
      expect(result.timestamp).toBeDefined();
      expect(result.metrics).toBeDefined();
      expect(result.complexity).toBeDefined();
      expect(result.effort).toBeDefined();
      expect(result.cost).toBeDefined();
      expect(result.timeline).toBeDefined();
      expect(result.comparisons).toHaveLength(4);
      expect(result.recommendations).toBeDefined();
      expect(result.confidence).toBeGreaterThan(0);
    });

    it('should include phase breakdown in effort', async () => {
      const result = await service.estimate({
        code: { content: SAMPLE_COBOL },
      });

      expect(result.effort.phases).toHaveLength(7);
      expect(result.effort.phases.map(p => p.phase)).toContain('Implementation');
      expect(result.effort.phases.map(p => p.phase)).toContain('Testing');
    });

    it('should include role breakdown in effort', async () => {
      const result = await service.estimate({
        code: { content: SAMPLE_COBOL },
      });

      expect(result.effort.byRole.length).toBeGreaterThan(0);
      expect(result.effort.byRole.some(r => r.role === 'Senior Developer')).toBe(true);
    });

    it('should respect estimation options', async () => {
      // Use a larger code sample to see meaningful cost differences
      const largerSample = SAMPLE_COBOL.repeat(10);

      const resultMinimal = await service.estimate({
        code: { content: largerSample },
        options: {
          humanReviewLevel: 'minimal',
          teamExperience: 'expert',
        },
      });

      const resultComprehensive = await service.estimate({
        code: { content: largerSample },
        options: {
          humanReviewLevel: 'comprehensive',
          teamExperience: 'novice',
        },
      });

      // Comprehensive review with novice team should cost more
      expect(resultComprehensive.cost.total).toBeGreaterThan(resultMinimal.cost.total);
    });
  });

  describe('batchEstimate', () => {
    it('should estimate multiple files', async () => {
      const result = await service.batchEstimate({
        files: [
          { content: SAMPLE_COBOL, filename: 'CALCINT.cbl' },
          { content: SAMPLE_FORTRAN, filename: 'interest.f' },
        ],
      });

      expect(result.fileCount).toBe(2);
      expect(result.fileResults).toHaveLength(2);
      expect(result.totalLines).toBeGreaterThan(0);
      expect(result.aggregateComplexity).toBeDefined();
      expect(result.aggregateCost).toBeDefined();
    });
  });
});

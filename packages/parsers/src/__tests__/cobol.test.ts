/**
 * COBOL Parser Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { CobolParser } from '../cobol/parser.js';

describe('CobolParser', () => {
  let parser: CobolParser;

  beforeEach(() => {
    parser = new CobolParser();
  });

  describe('parse', () => {
    it('should parse simple COBOL program', () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER    PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
           ADD 1 TO WS-COUNTER.
           STOP RUN.
      `;

      const result = parser.parse(source, 'TESTPROG.cbl');

      expect(result.success).toBe(true);
      expect(result.dataStructures.length).toBeGreaterThan(0);
      expect(result.procedures.length).toBeGreaterThan(0);
      expect(result.metadata?.language).toBe('cobol');
    });

    it('should extract data structures with correct types', () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATATYPES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING     PIC X(20).
       01 WS-NUMBER     PIC 9(10).
       01 WS-DECIMAL    PIC 9(5)V99.
      `;

      const result = parser.parse(source, 'DATATYPES.cbl');

      expect(result.success).toBe(true);
      expect(result.dataStructures).toContainEqual(
        expect.objectContaining({ name: 'WS-STRING', type: 'string' })
      );
      expect(result.dataStructures).toContainEqual(
        expect.objectContaining({ name: 'WS-NUMBER', type: 'integer' })
      );
      expect(result.dataStructures).toContainEqual(
        expect.objectContaining({ name: 'WS-DECIMAL', type: 'decimal' })
      );
    });

    it('should handle group items', () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GROUPS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ADDRESS.
          05 WS-STREET    PIC X(30).
          05 WS-CITY      PIC X(20).
          05 WS-ZIP       PIC 9(5).
      `;

      const result = parser.parse(source, 'GROUPS.cbl');

      expect(result.success).toBe(true);
      const addressGroup = result.dataStructures.find(d => d.name === 'WS-ADDRESS');
      expect(addressGroup).toBeDefined();
      expect(addressGroup?.type).toBe('group');
    });

    it('should extract procedures with complexity', () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEX.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           IF X > 0
               PERFORM PROCESS-PARA
           ELSE
               PERFORM ERROR-PARA
           END-IF.
           STOP RUN.
       
       PROCESS-PARA.
           ADD 1 TO COUNTER.
       
       ERROR-PARA.
           DISPLAY "ERROR".
      `;

      const result = parser.parse(source, 'COMPLEX.cbl');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBe(3);
      
      const mainPara = result.procedures.find(p => p.name === 'MAIN-PARA');
      expect(mainPara).toBeDefined();
      expect(mainPara?.complexity).toBeGreaterThan(1); // Has IF statement
    });

    it('should report parsing errors gracefully', () => {
      const invalidSource = `
       NOT VALID COBOL
       RANDOM TEXT
      `;

      const result = parser.parse(invalidSource, 'INVALID.cbl');

      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.metadata?.language).toBe('cobol');
    });
  });

  describe('getSupportedDialects', () => {
    it('should return supported COBOL dialects', () => {
      const dialects = parser.getSupportedDialects();

      expect(dialects).toContain('cobol85');
      expect(dialects).toContain('ibm-enterprise');
      expect(dialects).toContain('microfocus');
      expect(dialects).toContain('gnucobol');
    });
  });
});

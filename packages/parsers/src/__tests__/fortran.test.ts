/**
 * Fortran Parser Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { FortranParser } from '../fortran/parser.js';

describe('FortranParser', () => {
  let parser: FortranParser;

  beforeEach(() => {
    parser = new FortranParser();
  });

  describe('parse', () => {
    it('should parse simple Fortran program', () => {
      const source = `
      PROGRAM HELLO
      IMPLICIT NONE
      INTEGER :: X
      X = 10
      PRINT *, 'Hello, World!'
      END PROGRAM HELLO
      `;

      const result = parser.parse(source, 'hello.f90');

      expect(result.success).toBe(true);
      expect(result.metadata?.language).toBe('fortran');
    });

    it('should extract subroutines', () => {
      const source = `
      PROGRAM MAIN
      IMPLICIT NONE
      CALL MYSUB(10)
      END PROGRAM MAIN
      
      SUBROUTINE MYSUB(N)
      INTEGER, INTENT(IN) :: N
      PRINT *, N
      END SUBROUTINE MYSUB
      `;

      const result = parser.parse(source, 'sub.f90');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBeGreaterThanOrEqual(2);
    });

    it('should extract functions', () => {
      const source = `
      PROGRAM MAIN
      IMPLICIT NONE
      REAL :: RESULT
      RESULT = CALC(5.0, 3.0)
      END PROGRAM MAIN
      
      REAL FUNCTION CALC(A, B)
      REAL, INTENT(IN) :: A, B
      CALC = A * B
      END FUNCTION CALC
      `;

      const result = parser.parse(source, 'func.f90');

      expect(result.success).toBe(true);
      const calcFunc = result.procedures.find(p => p.name === 'CALC');
      expect(calcFunc).toBeDefined();
      expect(calcFunc?.type).toBe('function');
    });

    it('should handle COMMON blocks', () => {
      const source = `
      PROGRAM COMMON_TEST
      IMPLICIT NONE
      COMMON /BLOCK1/ X, Y, Z
      REAL :: X, Y, Z
      X = 1.0
      Y = 2.0
      Z = 3.0
      END PROGRAM COMMON_TEST
      `;

      const result = parser.parse(source, 'common.f');

      expect(result.success).toBe(true);
      // COMMON blocks should be detected
    });
  });

  describe('getSupportedDialects', () => {
    it('should return supported Fortran dialects', () => {
      const dialects = parser.getSupportedDialects();

      expect(dialects).toContain('f77');
      expect(dialects).toContain('f90');
      expect(dialects).toContain('f95');
    });
  });
});

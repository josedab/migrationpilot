/**
 * VB6 Parser Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { VB6Parser } from '../vb6/parser.js';

describe('VB6Parser', () => {
  let parser: VB6Parser;

  beforeEach(() => {
    parser = new VB6Parser();
  });

  describe('parse', () => {
    it('should parse simple VB6 module', () => {
      const source = `
Option Explicit

Dim gCounter As Integer

Public Sub Main()
    gCounter = 0
    Call IncrementCounter
    MsgBox "Counter: " & gCounter
End Sub

Private Sub IncrementCounter()
    gCounter = gCounter + 1
End Sub
      `;

      const result = parser.parse(source, 'Module1.bas');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBe(2);
      expect(result.metadata?.language).toBe('vb6');
    });

    it('should extract functions with return types', () => {
      const source = `
Option Explicit

Public Function Add(a As Integer, b As Integer) As Integer
    Add = a + b
End Function

Public Function Multiply(a As Double, b As Double) As Double
    Multiply = a * b
End Function
      `;

      const result = parser.parse(source, 'Math.bas');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBe(2);
      
      const addFunc = result.procedures.find(p => p.name === 'Add');
      expect(addFunc).toBeDefined();
      expect(addFunc?.parameters.length).toBe(2);
    });

    it('should extract variable declarations', () => {
      const source = `
Option Explicit

Dim strName As String
Dim intAge As Integer
Dim dblSalary As Double
Dim blnActive As Boolean

Public Sub Test()
    strName = "John"
End Sub
      `;

      const result = parser.parse(source, 'Vars.bas');

      expect(result.success).toBe(true);
      expect(result.dataStructures.length).toBe(4);
      
      expect(result.dataStructures).toContainEqual(
        expect.objectContaining({ name: 'strName', type: 'string' })
      );
      expect(result.dataStructures).toContainEqual(
        expect.objectContaining({ name: 'intAge', type: 'integer' })
      );
    });

    it('should handle class modules', () => {
      const source = `
Option Explicit

Private mName As String
Private mValue As Double

Public Property Get Name() As String
    Name = mName
End Property

Public Property Let Name(value As String)
    mName = value
End Property

Public Function Calculate() As Double
    Calculate = mValue * 2
End Function
      `;

      const result = parser.parse(source, 'MyClass.cls');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('getSupportedDialects', () => {
    it('should return supported VB dialects', () => {
      const dialects = parser.getSupportedDialects();

      expect(dialects).toContain('vb6');
      expect(dialects).toContain('vba');
    });
  });
});

/**
 * Legacy Java Parser Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { JavaLegacyParser } from '../java-legacy/parser.js';

describe('JavaLegacyParser', () => {
  let parser: JavaLegacyParser;

  beforeEach(() => {
    parser = new JavaLegacyParser();
  });

  describe('parse', () => {
    it('should parse simple Java class', () => {
      const source = `
package com.example;

public class Calculator {
    private int value;
    
    public int add(int a, int b) {
        return a + b;
    }
    
    public int getValue() {
        return value;
    }
}
      `;

      const result = parser.parse(source, 'Calculator.java');

      expect(result.success).toBe(true);
      expect(result.procedures.length).toBe(2);
      expect(result.dataStructures.length).toBe(1);
      expect(result.metadata?.language).toBe('java-legacy');
    });

    it('should detect EJB annotations', () => {
      const source = `
package com.example;

import javax.ejb.Stateless;

@Stateless
public class UserService {
    public String findUser(String id) {
        return "User: " + id;
    }
}
      `;

      const result = parser.parse(source, 'UserService.java');

      expect(result.success).toBe(true);
      expect(result.warnings.some(w => w.code === 'EJB_DETECTED')).toBe(true);
    });

    it('should extract method parameters', () => {
      const source = `
package com.example;

public class Service {
    public void process(String name, int count, boolean active) {
        System.out.println(name);
    }
}
      `;

      const result = parser.parse(source, 'Service.java');

      expect(result.success).toBe(true);
      const processMethod = result.procedures.find(p => p.name === 'process');
      expect(processMethod).toBeDefined();
      expect(processMethod?.parameters.length).toBe(3);
    });

    it('should calculate method complexity', () => {
      const source = `
package com.example;

public class Complex {
    public int calculate(int x) {
        if (x > 0) {
            if (x > 10) {
                return x * 2;
            } else {
                return x + 1;
            }
        } else if (x < 0) {
            return -x;
        }
        return 0;
    }
}
      `;

      const result = parser.parse(source, 'Complex.java');

      expect(result.success).toBe(true);
      const calcMethod = result.procedures.find(p => p.name === 'calculate');
      expect(calcMethod).toBeDefined();
      expect(calcMethod?.complexity).toBeGreaterThan(1);
    });

    it('should handle interfaces', () => {
      const source = `
package com.example;

public interface Repository {
    Object find(String id);
    void save(Object entity);
    void delete(String id);
}
      `;

      const result = parser.parse(source, 'Repository.java');

      expect(result.success).toBe(true);
      expect(result.ast?.type).toBe('Interface');
    });
  });

  describe('getSupportedDialects', () => {
    it('should return supported Java dialects', () => {
      const dialects = parser.getSupportedDialects();

      expect(dialects).toContain('j2ee');
      expect(dialects).toContain('ejb2');
      expect(dialects).toContain('struts1');
    });
  });
});

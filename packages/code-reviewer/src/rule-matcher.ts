/**
 * Rule Matcher
 * Matches code changes against business rules
 */

import type { BusinessRule } from '@migrationpilot/core';
import type { ChangedFile, RuleMatch, AffectedRule } from './types.js';

export class RuleMatcher {
  /**
   * Find rules that might be affected by the changed files
   */
  findAffectedRules(files: ChangedFile[], rules: BusinessRule[]): AffectedRule[] {
    const affected: AffectedRule[] = [];

    for (const rule of rules) {
      const matchingFiles: string[] = [];
      let bestConfidence = 0;
      let changeType: AffectedRule['changeType'] = 'modified';
      let explanation = '';

      for (const file of files) {
        const match = this.matchRuleToFile(rule, file);
        if (match) {
          matchingFiles.push(file.path);
          if (match.confidence > bestConfidence) {
            bestConfidence = match.confidence;
            explanation = match.reason;
          }

          // Determine change type based on file status and match
          if (file.status === 'deleted') {
            changeType = 'removed';
          } else if (match.matchType === 'direct') {
            changeType = this.determineChangeType(rule, file);
          }
        }
      }

      if (matchingFiles.length > 0) {
        affected.push({
          rule,
          changeType,
          affectedFiles: matchingFiles,
          confidence: bestConfidence,
          explanation,
        });
      }
    }

    // Sort by confidence
    return affected.sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Match a rule to a specific file
   */
  matchRuleToFile(rule: BusinessRule, file: ChangedFile): RuleMatch | null {
    // Check if the file path matches the rule's source file
    if (this.filePathMatches(file.path, rule.sourceFile)) {
      return this.analyzeDirectMatch(rule, file);
    }

    // Check for indirect matches based on rule keywords
    const indirectMatch = this.analyzeIndirectMatch(rule, file);
    if (indirectMatch) {
      return indirectMatch;
    }

    return null;
  }

  /**
   * Find all rules that match specific code content
   */
  findMatchingRules(code: string, rules: BusinessRule[]): RuleMatch[] {
    const matches: RuleMatch[] = [];

    for (const rule of rules) {
      const match = this.matchRuleToCode(rule, code);
      if (match) {
        matches.push(match);
      }
    }

    return matches.sort((a, b) => b.confidence - a.confidence);
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private filePathMatches(filePath: string, rulePath: string): boolean {
    const normalizedFile = filePath.toLowerCase().replace(/\\/g, '/');
    const normalizedRule = rulePath.toLowerCase().replace(/\\/g, '/');

    // Direct match
    if (normalizedFile.includes(normalizedRule) || normalizedRule.includes(normalizedFile)) {
      return true;
    }

    // Check basename match
    const fileBase = normalizedFile.split('/').pop() || '';
    const ruleBase = normalizedRule.split('/').pop() || '';

    return fileBase === ruleBase;
  }

  private analyzeDirectMatch(rule: BusinessRule, file: ChangedFile): RuleMatch {
    let confidence = 0.9;
    let reason = `File ${file.path} is the source of rule "${rule.name}"`;

    // Check if changes are in the rule's line range
    const ruleLines = rule.sourceLines;
    let affectsRuleLines = false;

    for (const hunk of file.hunks) {
      if (this.rangesOverlap(
        hunk.oldStart, hunk.oldStart + hunk.oldLines,
        ruleLines[0], ruleLines[1]
      )) {
        affectsRuleLines = true;
        confidence = 0.95;
        reason = `Changes directly affect lines ${ruleLines[0]}-${ruleLines[1]} where rule "${rule.name}" is implemented`;
        break;
      }
    }

    const matchedCode = file.hunks.map(h => h.content).join('\n');

    return {
      rule,
      matchType: affectsRuleLines ? 'direct' : 'indirect',
      matchedCode,
      location: {
        file: file.path,
        startLine: file.hunks[0]?.newStart || 1,
        endLine: file.hunks[file.hunks.length - 1]?.newStart || 1,
      },
      confidence,
      reason,
    };
  }

  private analyzeIndirectMatch(rule: BusinessRule, file: ChangedFile): RuleMatch | null {
    // Get all changed content
    const changedContent = file.hunks.map(h => h.content).join('\n').toLowerCase();

    // Check for rule-related keywords
    const keywords = this.extractRuleKeywords(rule);
    const matchedKeywords: string[] = [];

    for (const keyword of keywords) {
      if (changedContent.includes(keyword.toLowerCase())) {
        matchedKeywords.push(keyword);
      }
    }

    if (matchedKeywords.length === 0) {
      return null;
    }

    const confidence = Math.min(0.8, 0.3 + matchedKeywords.length * 0.15);

    return {
      rule,
      matchType: 'potential',
      matchedCode: changedContent.substring(0, 500),
      location: {
        file: file.path,
        startLine: file.hunks[0]?.newStart || 1,
        endLine: file.hunks[file.hunks.length - 1]?.newStart || 1,
      },
      confidence,
      reason: `Code contains keywords related to rule "${rule.name}": ${matchedKeywords.join(', ')}`,
    };
  }

  private matchRuleToCode(rule: BusinessRule, code: string): RuleMatch | null {
    const lowerCode = code.toLowerCase();
    const keywords = this.extractRuleKeywords(rule);
    const matched: string[] = [];

    for (const keyword of keywords) {
      if (lowerCode.includes(keyword.toLowerCase())) {
        matched.push(keyword);
      }
    }

    if (matched.length === 0) {
      return null;
    }

    return {
      rule,
      matchType: matched.length >= 3 ? 'direct' : 'potential',
      matchedCode: code.substring(0, 200),
      location: { file: 'inline', startLine: 1, endLine: 1 },
      confidence: Math.min(0.9, 0.3 + matched.length * 0.15),
      reason: `Matches keywords: ${matched.join(', ')}`,
    };
  }

  private extractRuleKeywords(rule: BusinessRule): string[] {
    const keywords: string[] = [];

    // Add rule name parts
    const nameParts = rule.name.split(/[\s_-]+/);
    keywords.push(...nameParts.filter((p: string) => p.length > 3));

    // Add input/output names
    for (const input of rule.inputs) {
      keywords.push(input.name);
    }
    for (const output of rule.outputs) {
      keywords.push(output.name);
    }

    // Add formula variables if present
    if (rule.formula) {
      const varMatches = rule.formula.match(/[A-Z_][A-Z0-9_-]*/gi);
      if (varMatches) {
        keywords.push(...varMatches.filter((v: string) => v.length > 2));
      }
    }

    // Add category-specific keywords
    const categoryKeywords: Record<string, string[]> = {
      calculation: ['calculate', 'compute', 'formula', 'total', 'sum'],
      validation: ['validate', 'check', 'verify', 'assert', 'error'],
      decision: ['if', 'when', 'condition', 'else', 'then'],
      transformation: ['convert', 'transform', 'map', 'format'],
    };

    const catKeywords = categoryKeywords[rule.category];
    if (catKeywords) {
      keywords.push(...catKeywords);
    }

    // Deduplicate and filter
    return [...new Set(keywords)].filter(k => k.length > 2);
  }

  private determineChangeType(rule: BusinessRule, file: ChangedFile): AffectedRule['changeType'] {
    // Analyze the changes to determine what kind of modification
    for (const hunk of file.hunks) {
      const addedContent = hunk.changes
        .filter(c => c.type === 'add')
        .map(c => c.content)
        .join('\n');
      const deletedContent = hunk.changes
        .filter(c => c.type === 'delete')
        .map(c => c.content)
        .join('\n');

      // Check for formula/calculation changes
      if (rule.category === 'calculation') {
        const calcPatterns = [/compute/i, /calculate/i, /=.*\*/i, /=.*\+/i, /=.*\//i];
        for (const pattern of calcPatterns) {
          if (pattern.test(deletedContent) && pattern.test(addedContent)) {
            return 'modified';
          }
          if (pattern.test(deletedContent) && !pattern.test(addedContent)) {
            return 'potentially_broken';
          }
        }
      }

      // Check for validation changes
      if (rule.category === 'validation') {
        const valPatterns = [/if\s*\(/i, /check/i, /valid/i, />=?|<=?|==|!=/];
        for (const pattern of valPatterns) {
          if (pattern.test(deletedContent) && !pattern.test(addedContent)) {
            return 'potentially_broken';
          }
        }
      }
    }

    return 'modified';
  }

  private rangesOverlap(start1: number, end1: number, start2: number, end2: number): boolean {
    return start1 <= end2 && end1 >= start2;
  }
}

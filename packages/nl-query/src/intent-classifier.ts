/**
 * Intent Classifier
 * Classifies natural language questions into query types
 */

import type { IntentClassification, QueryType, ExtractedEntity } from './types.js';

interface IntentPattern {
  intent: QueryType;
  patterns: RegExp[];
  keywords: string[];
  entityPatterns?: RegExp[];
}

const INTENT_PATTERNS: IntentPattern[] = [
  {
    intent: 'code-location',
    patterns: [
      /where\s+(is|are|can\s+i\s+find)/i,
      /which\s+file/i,
      /locate\s+/i,
      /find\s+the\s+(code|function|procedure|program)/i,
      /what\s+file\s+(contains|has)/i,
    ],
    keywords: ['where', 'locate', 'find', 'file', 'location', 'which file'],
  },
  {
    intent: 'data-flow',
    patterns: [
      /what\s+(uses|reads|writes|modifies|updates|accesses)/i,
      /how\s+is\s+(\w+)\s+(used|modified|processed)/i,
      /where\s+does\s+(\w+)\s+(come\s+from|go)/i,
      /trace\s+(the\s+)?data/i,
      /data\s+flow/i,
      /who\s+(produces|consumes)/i,
    ],
    keywords: ['uses', 'reads', 'writes', 'modifies', 'data flow', 'trace', 'produces', 'consumes'],
  },
  {
    intent: 'business-rule',
    patterns: [
      /what\s+(rule|logic|calculation)\s+(handles|governs|controls)/i,
      /how\s+is\s+(\w+)\s+calculated/i,
      /business\s+rule/i,
      /what\s+are\s+the\s+(rules|validations)/i,
      /how\s+does\s+the\s+system\s+(validate|calculate|determine)/i,
    ],
    keywords: ['rule', 'logic', 'calculation', 'validate', 'business rule', 'formula'],
  },
  {
    intent: 'dependency',
    patterns: [
      /what\s+(depends\s+on|relies\s+on)/i,
      /dependencies\s+of/i,
      /what\s+does\s+(\w+)\s+(call|use|depend)/i,
      /show\s+(me\s+)?(the\s+)?dependencies/i,
      /what\s+are\s+the\s+(prerequisites|requirements)/i,
    ],
    keywords: ['depends', 'dependency', 'dependencies', 'relies', 'calls', 'uses'],
  },
  {
    intent: 'impact',
    patterns: [
      /what\s+(would|will)\s+(be\s+)?affected/i,
      /impact\s+(of|analysis)/i,
      /if\s+(i|we)\s+(change|modify|update)/i,
      /what\s+breaks\s+if/i,
      /ripple\s+effect/i,
      /downstream\s+(effects|impact)/i,
    ],
    keywords: ['impact', 'affected', 'change', 'breaks', 'downstream', 'ripple'],
  },
  {
    intent: 'explanation',
    patterns: [
      /explain\s+(how|what|why)/i,
      /what\s+does\s+(\w+)\s+do/i,
      /how\s+does\s+(\w+)\s+work/i,
      /describe\s+/i,
      /tell\s+me\s+about/i,
      /what\s+is\s+the\s+purpose/i,
    ],
    keywords: ['explain', 'how does', 'what does', 'describe', 'purpose', 'work'],
  },
  {
    intent: 'comparison',
    patterns: [
      /compare\s+(\w+)\s+(and|with|to)/i,
      /difference\s+between/i,
      /how\s+(is|are)\s+(\w+)\s+different/i,
      /versus|vs\.?/i,
      /similar\s+to/i,
    ],
    keywords: ['compare', 'difference', 'versus', 'vs', 'similar', 'different'],
  },
  {
    intent: 'search',
    patterns: [
      /find\s+all/i,
      /list\s+(all\s+)?(the\s+)?/i,
      /show\s+(me\s+)?(all\s+)?/i,
      /search\s+for/i,
      /get\s+(all|every)/i,
      /what\s+are\s+all\s+the/i,
    ],
    keywords: ['find all', 'list', 'show all', 'search', 'every', 'all the'],
  },
  {
    intent: 'summary',
    patterns: [
      /summarize/i,
      /give\s+(me\s+)?(a\s+)?summary/i,
      /overview\s+of/i,
      /brief(ly)?\s+(describe|explain)/i,
      /high[\s-]level/i,
      /in\s+brief/i,
    ],
    keywords: ['summarize', 'summary', 'overview', 'brief', 'high-level'],
  },
];

const ENTITY_PATTERNS: { type: ExtractedEntity['type']; patterns: RegExp[] }[] = [
  {
    type: 'procedure',
    patterns: [
      /procedure\s+["']?(\w+)["']?/i,
      /function\s+["']?(\w+)["']?/i,
      /program\s+["']?(\w+)["']?/i,
      /subroutine\s+["']?(\w+)["']?/i,
      /method\s+["']?(\w+)["']?/i,
      /["']?([A-Z][A-Z0-9_-]+)["']?\s+procedure/i,
    ],
  },
  {
    type: 'data',
    patterns: [
      /data\s+["']?(\w+)["']?/i,
      /field\s+["']?(\w+)["']?/i,
      /variable\s+["']?(\w+)["']?/i,
      /["']?(\w+-?\w*)\s+(field|data|variable)/i,
      /customer\s+(\w+)/i,
      /["']?([A-Z][A-Z0-9_-]+)["']?\s+record/i,
    ],
  },
  {
    type: 'rule',
    patterns: [
      /rule\s+["']?(\w+)["']?/i,
      /business\s+rule\s+["']?(\w+)["']?/i,
      /validation\s+["']?(\w+)["']?/i,
      /["']?(\w+)["']?\s+(rule|validation)/i,
    ],
  },
  {
    type: 'file',
    patterns: [
      /file\s+["']?(\w+\.\w+)["']?/i,
      /["']?(\w+\.(cbl|cob|f|f90|bas|java))["']?/i,
      /in\s+["']?(\w+\.\w+)["']?/i,
    ],
  },
  {
    type: 'system',
    patterns: [
      /system\s+["']?(\w+)["']?/i,
      /database\s+["']?(\w+)["']?/i,
      /service\s+["']?(\w+)["']?/i,
      /api\s+["']?(\w+)["']?/i,
    ],
  },
  {
    type: 'concept',
    patterns: [
      /interest/i,
      /loan/i,
      /payment/i,
      /customer/i,
      /account/i,
      /balance/i,
      /transaction/i,
      /credit/i,
      /eligibility/i,
      /risk/i,
    ],
  },
];

export class IntentClassifier {
  /**
   * Classify the intent of a natural language question
   */
  classify(question: string): IntentClassification {
    const normalizedQuestion = question.toLowerCase().trim();

    // Score each intent
    const scores: { intent: QueryType; score: number }[] = [];

    for (const pattern of INTENT_PATTERNS) {
      let score = 0;

      // Check regex patterns
      for (const regex of pattern.patterns) {
        if (regex.test(question)) {
          score += 10;
        }
      }

      // Check keywords
      for (const keyword of pattern.keywords) {
        if (normalizedQuestion.includes(keyword.toLowerCase())) {
          score += 5;
        }
      }

      if (score > 0) {
        scores.push({ intent: pattern.intent, score });
      }
    }

    // Sort by score
    scores.sort((a, b) => b.score - a.score);

    // Get best match
    const bestMatch = scores[0];
    const intent: QueryType = bestMatch?.intent || 'unknown';
    const maxScore = bestMatch?.score || 0;

    // Calculate confidence
    const totalScore = scores.reduce((sum, s) => sum + s.score, 0) || 1;
    const confidence = Math.min(0.95, maxScore / totalScore + (maxScore > 15 ? 0.2 : 0));

    // Extract entities
    const entities = this.extractEntities(question);

    // Extract keywords
    const keywords = this.extractKeywords(question);

    return {
      intent,
      confidence,
      entities,
      keywords,
    };
  }

  /**
   * Extract entities from the question
   */
  private extractEntities(question: string): ExtractedEntity[] {
    const entities: ExtractedEntity[] = [];

    for (const entityDef of ENTITY_PATTERNS) {
      for (const pattern of entityDef.patterns) {
        const match = question.match(pattern);
        if (match && match[1]) {
          entities.push({
            type: entityDef.type,
            value: match[1],
            position: [match.index || 0, (match.index || 0) + match[0].length],
            confidence: 0.8,
          });
        }
      }
    }

    // Deduplicate by value
    const seen = new Set<string>();
    return entities.filter(e => {
      if (seen.has(e.value.toLowerCase())) return false;
      seen.add(e.value.toLowerCase());
      return true;
    });
  }

  /**
   * Extract relevant keywords from the question
   */
  private extractKeywords(question: string): string[] {
    // Remove common stop words
    const stopWords = new Set([
      'a', 'an', 'the', 'is', 'are', 'was', 'were', 'be', 'been', 'being',
      'have', 'has', 'had', 'do', 'does', 'did', 'will', 'would', 'could',
      'should', 'may', 'might', 'can', 'must', 'shall', 'to', 'of', 'in',
      'for', 'on', 'with', 'at', 'by', 'from', 'as', 'into', 'through',
      'during', 'before', 'after', 'above', 'below', 'between', 'under',
      'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where',
      'why', 'how', 'all', 'each', 'few', 'more', 'most', 'other', 'some',
      'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than',
      'too', 'very', 'just', 'and', 'but', 'if', 'or', 'because', 'until',
      'while', 'i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'you', 'your',
      'he', 'him', 'his', 'she', 'her', 'it', 'its', 'they', 'them', 'their',
      'what', 'which', 'who', 'whom', 'this', 'that', 'these', 'those', 'am',
    ]);

    const words = question.toLowerCase()
      .replace(/[^\w\s-]/g, ' ')
      .split(/\s+/)
      .filter(w => w.length > 2 && !stopWords.has(w));

    // Return unique keywords
    return [...new Set(words)];
  }
}

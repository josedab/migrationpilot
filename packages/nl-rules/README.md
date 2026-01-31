# @migrationpilot/nl-rules

Natural language interface for business rule editing.

## Overview

This package provides natural language capabilities for editing, querying, and validating business rules, making it easier for non-technical SMEs to participate in the migration process.

## Features

- **Natural Language Editing**: Modify rules using plain English
- **Rule Querying**: Ask questions about business logic
- **Validation**: Check rule consistency
- **Translation**: Convert rules to/from natural language
- **Suggestions**: AI-powered rule improvements

## Usage

### Edit Rules with Natural Language

```typescript
import { NLRuleEditor } from '@migrationpilot/nl-rules';

const editor = new NLRuleEditor();

// Modify a rule using natural language
const updated = await editor.edit(rule, {
  instruction: "Change the overtime threshold from 40 hours to 35 hours for part-time employees",
});

// Result includes the modified rule and explanation
console.log(updated.rule);
console.log(updated.explanation);
console.log(updated.changes);
```

### Query Business Logic

```typescript
import { NLRuleQuery } from '@migrationpilot/nl-rules';

const query = new NLRuleQuery();

// Ask questions about rules
const answer = await query.ask(rules, {
  question: "How is overtime calculated for salaried employees?",
});

console.log(answer.response);
console.log(answer.relevantRules);
```

### Translate Rules

```typescript
import { RuleTranslator } from '@migrationpilot/nl-rules';

const translator = new RuleTranslator();

// Convert rule to natural language
const description = await translator.toNaturalLanguage(rule);
// "When an employee works more than 40 hours in a week, 
//  their overtime pay is calculated as 1.5 times their 
//  hourly rate for each hour over 40."

// Convert natural language to rule
const rule = await translator.fromNaturalLanguage({
  description: "Employees get a 10% bonus if they've been with the company for more than 5 years",
  context: { category: 'compensation' },
});
```

### Validate with Natural Language

```typescript
import { NLValidator } from '@migrationpilot/nl-rules';

const validator = new NLValidator();

// Validate rule consistency
const validation = await validator.validate(rules, {
  check: "Are there any conflicting rules about overtime calculation?",
});

if (validation.issues.length > 0) {
  for (const issue of validation.issues) {
    console.log(`Conflict: ${issue.description}`);
    console.log(`Rules: ${issue.affectedRules.join(', ')}`);
  }
}
```

### Get Suggestions

```typescript
import { RuleSuggester } from '@migrationpilot/nl-rules';

const suggester = new RuleSuggester();

// Get improvement suggestions
const suggestions = await suggester.suggest(rule);

for (const suggestion of suggestions) {
  console.log(`
    Type: ${suggestion.type}
    Description: ${suggestion.description}
    Confidence: ${suggestion.confidence}
  `);
}
```

## Installation

```bash
pnpm add @migrationpilot/nl-rules
```

## License

MIT

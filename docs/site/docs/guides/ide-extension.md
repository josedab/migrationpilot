---
sidebar_position: 5
---

# IDE Extension

MigrationPilot provides IDE extensions for VS Code and IntelliJ that bring AI-powered migration assistance directly into your development environment.

## Features

### Real-Time Migration Copilot

Get intelligent assistance while working with legacy code:

- **Inline Suggestions**: Context-aware completions and refactoring hints
- **Code Explanations**: Select any code block and get plain-language explanations
- **Migration Previews**: See what modern code would look like before migrating
- **Interactive Chat**: Ask questions about code with full context awareness

### Confidence Visualization

Visual feedback on AI analysis certainty:

- **Color-coded highlights**: Green (high), amber (medium), red (low) confidence
- **Inline badges**: Percentage scores on extracted rules
- **Hover tooltips**: Detailed explanations of confidence levels
- **Review flags**: Automatic marking of code needing human review

## Installation

### VS Code

```bash
# Install from marketplace
code --install-extension migrationpilot.vscode-migrationpilot

# Or search "MigrationPilot" in VS Code Extensions
```

### IntelliJ IDEA

1. Open **Settings** → **Plugins** → **Marketplace**
2. Search for "MigrationPilot"
3. Click **Install** and restart IDE

## Configuration

### Extension Settings

```json
{
  "migrationpilot.apiEndpoint": "http://localhost:3001",
  "migrationpilot.defaultTargetLanguage": "java",
  "migrationpilot.enableInlineSuggestions": true,
  "migrationpilot.enableConfidenceVisualization": true,
  "migrationpilot.confidenceThresholds": {
    "high": 0.85,
    "medium": 0.60
  },
  "migrationpilot.autoAnalyzeOnOpen": true
}
```

### Project Configuration

Create `.migrationpilot.json` in your project root:

```json
{
  "projectId": "my-migration-project",
  "sourceLanguage": "cobol",
  "targetLanguage": "java",
  "excludePatterns": ["**/test/**", "**/copybooks/**"],
  "customRules": "./migration-rules.json"
}
```

## Usage

### Starting a Session

1. Open a legacy code file (`.cbl`, `.cob`, `.f90`, `.bas`, etc.)
2. The extension automatically starts a copilot session
3. You'll see confidence highlights appear as analysis completes

### Getting Code Explanations

**Method 1: Context Menu**
1. Select code in the editor
2. Right-click → **MigrationPilot** → **Explain Selection**

**Method 2: Keyboard Shortcut**
- `Ctrl+Shift+E` (Windows/Linux)
- `Cmd+Shift+E` (macOS)

**Method 3: Command Palette**
1. `Ctrl+Shift+P` / `Cmd+Shift+P`
2. Type "MigrationPilot: Explain Code"

### Interactive Chat

Open the MigrationPilot chat panel:

```
Ctrl+Shift+M / Cmd+Shift+M
```

Ask questions in natural language:

```
> What does the 3000-CALCULATE-INTEREST paragraph do?

This paragraph calculates compound interest based on the principal
amount (WS-PRINCIPAL), rate (WS-RATE), and time period (WS-YEARS).
It uses the formula: Interest = Principal × (1 + Rate)^Years - Principal

Key observations:
- Uses COMPUTE for calculation (line 145)
- Rounds to 2 decimal places (line 147)
- Stores result in WS-INTEREST

Related business rule: BR-015 (Interest Calculation)
Confidence: 94%
```

### Migration Previews

1. Select code you want to migrate
2. Right-click → **MigrationPilot** → **Preview Migration**
3. Choose target language
4. Review side-by-side comparison

```
┌─────────────────────────┬─────────────────────────┐
│ Original (COBOL)        │ Migrated (Java)         │
├─────────────────────────┼─────────────────────────┤
│ COMPUTE WS-INTEREST =   │ BigDecimal interest =   │
│   WS-PRINCIPAL *        │   principal             │
│   WS-RATE *             │     .multiply(rate)     │
│   WS-YEARS.             │     .multiply(years);   │
└─────────────────────────┴─────────────────────────┘
```

### Inline Suggestions

As you navigate code, you'll see lightbulb suggestions:

- 💡 **Extract as business rule** - For calculation patterns
- 📖 **Explain this condition** - For complex IF statements
- 🔄 **View migration** - For refactorable code
- ⚠️ **Review needed** - For low-confidence sections

Click the lightbulb or press `Ctrl+.` / `Cmd+.` to see actions.

## Confidence Visualization

### Understanding Confidence Levels

| Level | Score | Color | Meaning |
|-------|-------|-------|---------|
| High | ≥ 85% | Green | Reliable extraction, safe to proceed |
| Medium | 60-84% | Amber | Generally accurate, review recommended |
| Low | < 60% | Red | Uncertain, manual review required |
| Unknown | N/A | Gray | Could not analyze |

### Viewing Confidence Details

Hover over highlighted code to see:

```
✅ HIGH confidence (92%)
Category: Business Rule Extraction

This calculation implements interest computation based on
principal, rate, and time variables.

Extracted as: BR-015 Interest Calculation
```

### Finding Code Needing Review

1. Open Command Palette (`Ctrl+Shift+P`)
2. Run "MigrationPilot: Show Low Confidence Items"
3. Navigate through flagged sections

### Confidence Summary

View project-wide confidence statistics:

```
📊 Confidence Summary: PAYROLL.cbl
────────────────────────────────────
Total analyzed: 47 sections
Average confidence: 87%

By Level:
  ✅ High:    38 (81%)
  ⚠️ Medium:  7 (15%)
  🔴 Low:     2 (4%)

By Category:
  Rule Extraction:    23
  Type Inference:     12
  Relationships:      8
  Migration Ready:    4
```

## Keyboard Shortcuts

| Action | Windows/Linux | macOS |
|--------|---------------|-------|
| Explain selection | `Ctrl+Shift+E` | `Cmd+Shift+E` |
| Open chat panel | `Ctrl+Shift+M` | `Cmd+Shift+M` |
| Preview migration | `Ctrl+Shift+G` | `Cmd+Shift+G` |
| Show quick actions | `Ctrl+.` | `Cmd+.` |
| Go to low confidence | `Ctrl+Shift+L` | `Cmd+Shift+L` |
| Toggle confidence view | `Ctrl+Shift+C` | `Cmd+Shift+C` |

## Copilot Session API

For extension developers, the copilot session API is available:

```typescript
import { MigrationCopilot } from '@migrationpilot/ide-extension';

const copilot = new MigrationCopilot({
  apiEndpoint: 'http://localhost:3001',
  enableStreaming: true,
});

// Start session
const session = copilot.startSession(
  'project-123',
  'file:///path/to/PAYROLL.cbl',
  'cobol',
  cobolSourceCode
);

// Get inline suggestions
const suggestions = await copilot.getInlineSuggestions(
  session.id,
  50,  // line
  10,  // column
  'manual'
);

// Explain selected code
const explanation = await copilot.explainCode(session.id, {
  startLine: 100,
  startColumn: 1,
  endLine: 120,
  endColumn: 50
});

// Get migration suggestion
const migration = await copilot.getMigrationSuggestion(
  session.id,
  { startLine: 100, startColumn: 1, endLine: 120, endColumn: 50 },
  'java'
);

// Interactive chat
const response = await copilot.chat(
  session.id,
  'What happens when the account balance is negative?'
);
```

## Confidence Visualizer API

```typescript
import {
  ConfidenceVisualizer,
  createConfidenceVisualizer
} from '@migrationpilot/ide-extension';

const visualizer = createConfidenceVisualizer({
  thresholds: { high: 0.85, medium: 0.6 },
  colors: {
    high: '#22c55e',
    medium: '#f59e0b',
    low: '#ef4444',
    unknown: '#6b7280',
  },
  showScores: true,
  showTooltips: true,
});

// Set confidence data from analysis
visualizer.setConfidenceRanges('file:///PAYROLL.cbl', [
  {
    range: { startLine: 100, startColumn: 1, endLine: 110, endColumn: 1 },
    confidence: 0.92,
    level: 'high',
    category: 'rule-extraction',
    explanation: 'Interest calculation rule extracted successfully',
  },
]);

// Generate IDE decorations
const highlights = visualizer.generateHighlights('file:///PAYROLL.cbl');
const badges = visualizer.generateInlineDecorations('file:///PAYROLL.cbl');

// Get items needing review
const needsReview = visualizer.getRangesNeedingReview('file:///PAYROLL.cbl');

// Get summary statistics
const summary = visualizer.getSummaryStats('file:///PAYROLL.cbl');
```

## Troubleshooting

### Extension Not Activating

1. Ensure file extension is supported (`.cbl`, `.cob`, `.f90`, `.f77`, `.bas`, `.vb`)
2. Check API endpoint is reachable
3. Verify project has `.migrationpilot.json` or use default settings

### Slow Analysis

- Large files (>5000 lines) may take longer
- Check network connectivity to API
- Try increasing `maxTokens` in settings

### Missing Confidence Highlights

1. Run "MigrationPilot: Analyze Current File" manually
2. Check for parse errors in the Problems panel
3. Ensure the language is supported

### Chat Not Responding

- Verify API server is running
- Check for rate limiting messages
- Review extension output logs

## Best Practices

1. **Start with analysis**: Let the extension analyze files before interacting
2. **Review low confidence first**: Address uncertain areas early
3. **Use chat for complex questions**: Better than repeated explain commands
4. **Save sessions**: Chat history is preserved per file
5. **Customize thresholds**: Adjust confidence thresholds to your risk tolerance

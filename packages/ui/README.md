# @migrationpilot/ui

Shared UI components for MigrationPilot applications.

## Overview

This package provides reusable React components built with Radix UI primitives and styled with Tailwind CSS. These components are used across the MigrationPilot web dashboard and other frontend applications.

## Features

- **Accessible**: Built on Radix UI primitives with full ARIA support
- **Themeable**: Dark/light mode support with CSS variables
- **Type-Safe**: Full TypeScript support
- **Responsive**: Mobile-first responsive design
- **Customizable**: Easily extend with Tailwind CSS

## Components

### Core Components

| Component | Description |
|-----------|-------------|
| `Button` | Primary, secondary, and ghost button variants |
| `Card` | Content container with header and footer |
| `Dialog` | Modal dialogs and confirmations |
| `Dropdown` | Dropdown menus and selects |
| `Input` | Text inputs with validation states |
| `Progress` | Progress bars and indicators |
| `Tabs` | Tabbed navigation |
| `Table` | Data tables with sorting and pagination |
| `Tooltip` | Informational tooltips |

### Migration-Specific Components

| Component | Description |
|-----------|-------------|
| `RuleCard` | Business rule display with confidence indicator |
| `CodeDiff` | Side-by-side code comparison |
| `PipelineStatus` | Migration pipeline progress |
| `ConfidenceScore` | Visual confidence score display |
| `FileTree` | Source file navigation |
| `TracabilityMap` | Legacy to modern code mapping |

## Usage

### Installation

```bash
pnpm add @migrationpilot/ui
```

### Setup

Add the styles to your application:

```tsx
// app/layout.tsx or _app.tsx
import '@migrationpilot/ui/styles';
```

Configure Tailwind to include the component styles:

```js
// tailwind.config.js
module.exports = {
  content: [
    './src/**/*.{ts,tsx}',
    './node_modules/@migrationpilot/ui/dist/**/*.{js,ts,jsx,tsx}',
  ],
  // ... rest of config
};
```

### Basic Usage

```tsx
import { Button, Card, Progress } from '@migrationpilot/ui';

function MigrationStatus() {
  return (
    <Card>
      <Card.Header>
        <Card.Title>Migration Progress</Card.Title>
        <Card.Description>Payroll System Migration</Card.Description>
      </Card.Header>
      <Card.Content>
        <Progress value={65} />
        <p className="mt-2 text-sm text-muted-foreground">
          65% complete - Generating code...
        </p>
      </Card.Content>
      <Card.Footer>
        <Button variant="outline">Cancel</Button>
        <Button>View Details</Button>
      </Card.Footer>
    </Card>
  );
}
```

### Business Rule Card

```tsx
import { RuleCard } from '@migrationpilot/ui';

function RuleList({ rules }) {
  return (
    <div className="space-y-4">
      {rules.map((rule) => (
        <RuleCard
          key={rule.id}
          rule={rule}
          onApprove={() => handleApprove(rule.id)}
          onReject={() => handleReject(rule.id)}
          onEdit={() => handleEdit(rule.id)}
        />
      ))}
    </div>
  );
}
```

### Code Diff

```tsx
import { CodeDiff } from '@migrationpilot/ui';

function MigrationComparison() {
  return (
    <CodeDiff
      legacy={{
        language: 'cobol',
        code: legacyCode,
        filename: 'PAYROLL.cbl',
      }}
      modern={{
        language: 'java',
        code: modernCode,
        filename: 'PayrollService.java',
      }}
      highlights={[
        { legacy: [150, 165], modern: [45, 62], ruleId: 'BR-001' },
      ]}
    />
  );
}
```

### Pipeline Status

```tsx
import { PipelineStatus } from '@migrationpilot/ui';

function MigrationPipeline({ project }) {
  return (
    <PipelineStatus
      stages={[
        { name: 'Analysis', status: 'complete', duration: '5m 23s' },
        { name: 'Review', status: 'complete', duration: '2h 15m' },
        { name: 'Design', status: 'complete', duration: '12m 45s' },
        { name: 'Generation', status: 'in-progress', progress: 65 },
        { name: 'Validation', status: 'pending' },
      ]}
    />
  );
}
```

### Confidence Score

```tsx
import { ConfidenceScore } from '@migrationpilot/ui';

function RuleConfidence({ confidence }) {
  return (
    <ConfidenceScore
      value={confidence}
      showLabel
      size="lg"
      thresholds={{
        high: 0.9,    // Green
        medium: 0.7,  // Yellow
        low: 0,       // Red
      }}
    />
  );
}
```

## Theming

### CSS Variables

```css
:root {
  --background: 0 0% 100%;
  --foreground: 222.2 84% 4.9%;
  --card: 0 0% 100%;
  --card-foreground: 222.2 84% 4.9%;
  --primary: 222.2 47.4% 11.2%;
  --primary-foreground: 210 40% 98%;
  --secondary: 210 40% 96.1%;
  --secondary-foreground: 222.2 47.4% 11.2%;
  --muted: 210 40% 96.1%;
  --muted-foreground: 215.4 16.3% 46.9%;
  --accent: 210 40% 96.1%;
  --accent-foreground: 222.2 47.4% 11.2%;
  --destructive: 0 84.2% 60.2%;
  --destructive-foreground: 210 40% 98%;
  --border: 214.3 31.8% 91.4%;
  --ring: 222.2 84% 4.9%;
  --radius: 0.5rem;
}

.dark {
  --background: 222.2 84% 4.9%;
  --foreground: 210 40% 98%;
  /* ... dark mode values */
}
```

### Custom Theme

```tsx
import { ThemeProvider } from '@migrationpilot/ui';

function App() {
  return (
    <ThemeProvider defaultTheme="system" storageKey="migrationpilot-theme">
      <YourApp />
    </ThemeProvider>
  );
}
```

## Development

```bash
# Build
pnpm build

# Watch mode
pnpm dev

# Lint
pnpm lint

# Type check
pnpm typecheck
```

## Storybook

```bash
# Start Storybook
pnpm storybook

# Build Storybook
pnpm build-storybook
```

## License

MIT

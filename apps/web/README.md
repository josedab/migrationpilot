# @migrationpilot/web

MigrationPilot web dashboard built with Next.js.

## Overview

This is the main web interface for MigrationPilot, providing a visual dashboard for managing migration projects, reviewing business rules, and monitoring migration progress.

## Features

- **Project Management**: Create and manage migration projects
- **Code Analysis**: Upload and analyze legacy code
- **Rule Review**: SME collaboration for business rule validation
- **Pipeline Visualization**: Track migration progress in real-time
- **Code Comparison**: Side-by-side legacy vs modern code view
- **Traceability**: Navigate between legacy and generated code
- **Reports**: Export migration reports and documentation

## Quick Start

```bash
# Development
pnpm dev

# Production build
pnpm build
pnpm start
```

The dashboard runs on `http://localhost:3000` by default.

## Screenshots

### Dashboard
View all projects, recent activity, and migration statistics.

### Analysis View
Upload legacy code and view extracted business rules with confidence scores.

### Rule Review
Collaborate with SMEs to validate and annotate business rules.

### Migration Progress
Monitor real-time progress through the migration pipeline.

### Code Comparison
Compare legacy code with generated modern code side-by-side.

## Project Structure

```
src/
├── app/
│   ├── layout.tsx           # Root layout
│   ├── page.tsx             # Dashboard home
│   ├── projects/
│   │   ├── page.tsx         # Projects list
│   │   ├── new/page.tsx     # Create project
│   │   └── [id]/
│   │       ├── page.tsx     # Project details
│   │       ├── analysis/    # Analysis views
│   │       ├── rules/       # Business rules
│   │       ├── migration/   # Migration progress
│   │       └── validation/  # Test results
│   └── settings/
│       └── page.tsx         # User settings
├── components/
│   ├── layout/              # Layout components
│   ├── projects/            # Project components
│   ├── rules/               # Rule review components
│   ├── migration/           # Migration components
│   └── ui/                  # Base UI components
├── hooks/
│   ├── useProject.ts        # Project data hook
│   ├── useRules.ts          # Business rules hook
│   └── useMigration.ts      # Migration status hook
├── lib/
│   ├── api.ts               # API client
│   └── utils.ts             # Utilities
└── styles/
    └── globals.css          # Global styles
```

## Key Components

### ProjectCard

Display project summary with status and progress.

```tsx
import { ProjectCard } from '@/components/projects';

<ProjectCard
  project={project}
  onClick={() => router.push(`/projects/${project.id}`)}
/>
```

### RuleReview

Interactive business rule review interface.

```tsx
import { RuleReview } from '@/components/rules';

<RuleReview
  rule={rule}
  onApprove={(comment) => approveRule(rule.id, comment)}
  onReject={(reason) => rejectRule(rule.id, reason)}
  onEdit={(changes) => editRule(rule.id, changes)}
/>
```

### PipelineProgress

Migration pipeline visualization.

```tsx
import { PipelineProgress } from '@/components/migration';

<PipelineProgress
  projectId={projectId}
  onStageClick={(stage) => showStageDetails(stage)}
/>
```

### CodeComparison

Side-by-side code viewer with traceability.

```tsx
import { CodeComparison } from '@/components/migration';

<CodeComparison
  legacy={{ code: cobolCode, language: 'cobol' }}
  modern={{ code: javaCode, language: 'java' }}
  mappings={tracabilityMappings}
  onMappingClick={(mapping) => highlightRelated(mapping)}
/>
```

## API Integration

The dashboard uses React Query for API data fetching:

```tsx
import { useQuery, useMutation } from '@tanstack/react-query';
import { api } from '@/lib/api';

// Fetch projects
const { data: projects } = useQuery({
  queryKey: ['projects'],
  queryFn: () => api.projects.list(),
});

// Start migration
const startMigration = useMutation({
  mutationFn: (projectId: string) => api.migration.start(projectId),
  onSuccess: () => {
    queryClient.invalidateQueries(['projects']);
  },
});
```

## Environment Variables

```env
# API
NEXT_PUBLIC_API_URL=http://localhost:3001

# Authentication
NEXTAUTH_SECRET=your-nextauth-secret
NEXTAUTH_URL=http://localhost:3000

# Features
NEXT_PUBLIC_ENABLE_ANALYTICS=false
```

## Styling

The dashboard uses Tailwind CSS with a custom theme:

```js
// tailwind.config.js
module.exports = {
  darkMode: 'class',
  theme: {
    extend: {
      colors: {
        primary: { /* custom primary colors */ },
        // Confidence score colors
        confidence: {
          high: '#22c55e',
          medium: '#eab308',
          low: '#ef4444',
        },
      },
    },
  },
};
```

## Authentication

Authentication is handled via NextAuth.js:

```tsx
import { signIn, signOut, useSession } from 'next-auth/react';

function LoginButton() {
  const { data: session } = useSession();
  
  if (session) {
    return <Button onClick={() => signOut()}>Sign out</Button>;
  }
  return <Button onClick={() => signIn()}>Sign in</Button>;
}
```

## Development

```bash
# Run development server
pnpm dev

# Build for production
pnpm build

# Start production server
pnpm start

# Lint
pnpm lint

# Type check
pnpm typecheck
```

## Testing

```bash
# Run unit tests
pnpm test

# Run e2e tests
pnpm test:e2e

# Run component tests
pnpm test:components
```

## Docker

```bash
# Build image
docker build -t migrationpilot-web .

# Run container
docker run -p 3000:3000 --env-file .env migrationpilot-web
```

## License

MIT

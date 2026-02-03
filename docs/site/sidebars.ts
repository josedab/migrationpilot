import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  tutorialSidebar: [
    'intro',
    'why-migrationpilot',
    'benchmarks',
    {
      type: 'category',
      label: 'Getting Started',
      items: [
        'getting-started/installation',
        'getting-started/quickstart',
        'getting-started/your-first-migration',
      ],
    },
    {
      type: 'category',
      label: 'Concepts',
      items: [
        'concepts/how-it-works',
        'concepts/ai-agents',
        'concepts/explainer-agent',
        'concepts/business-rules',
        'concepts/equivalence-testing',
      ],
    },
    {
      type: 'category',
      label: 'Guides',
      items: [
        'guides/cobol-to-java',
        'guides/fortran-to-python',
        'guides/vb6-to-typescript',
        'guides/strangler-fig-pattern',
        'guides/ide-extension',
        'guides/mainframe-tracing-setup',
      ],
    },
    {
      type: 'category',
      label: 'Architecture',
      items: [
        'architecture/overview',
        'architecture/agents',
        'architecture/parsers',
        'architecture/generators',
        'architecture/testing',
        'architecture/differential-testing',
        'architecture/tracing',
        'architecture/rollout',
        'architecture/self-healing',
        'architecture/collaboration',
        'architecture/database',
      ],
    },
    {
      type: 'category',
      label: 'Deployment',
      items: [
        'deployment/cloud',
        'deployment/on-premises',
        'deployment/kubernetes',
        'deployment/security',
        'deployment/search-setup',
      ],
    },
    {
      type: 'category',
      label: 'API Reference',
      items: [
        'api/overview',
        'api/authentication',
        'api/projects',
        'api/analysis',
        'api/migration',
        'api/validation',
      ],
    },
    {
      type: 'category',
      label: 'CLI Reference',
      items: [
        'cli/overview',
        'cli/analyze',
        'cli/migrate',
        'cli/validate',
        'cli/project',
        'cli/rules',
        'cli/config',
      ],
    },
    'faq',
    'troubleshooting',
    'contributing',
    'changelog',
  ],
};

export default sidebars;

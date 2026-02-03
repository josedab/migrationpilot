import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'MigrationPilot',
  tagline: 'AI-Powered Legacy Code Modernization',
  favicon: 'img/favicon.svg',

  url: 'https://docs.migrationpilot.dev',
  baseUrl: '/',

  organizationName: 'migrationpilot',
  projectName: 'migrationpilot',

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  // OpenGraph and Twitter meta tags
  headTags: [
    {
      tagName: 'meta',
      attributes: {
        property: 'og:type',
        content: 'website',
      },
    },
    {
      tagName: 'meta',
      attributes: {
        property: 'og:site_name',
        content: 'MigrationPilot',
      },
    },
    {
      tagName: 'meta',
      attributes: {
        name: 'twitter:card',
        content: 'summary_large_image',
      },
    },
    {
      tagName: 'meta',
      attributes: {
        name: 'twitter:site',
        content: '@migrationpilot',
      },
    },
    {
      tagName: 'meta',
      attributes: {
        name: 'keywords',
        content: 'legacy code modernization, COBOL migration, Fortran migration, code transformation, AI code migration, mainframe modernization',
      },
    },
    {
      tagName: 'meta',
      attributes: {
        name: 'author',
        content: 'MigrationPilot Team',
      },
    },
  ],

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/migrationpilot/migrationpilot/tree/main/docs/site/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/migrationpilot-social-card.svg',
    navbar: {
      title: 'MigrationPilot',
      logo: {
        alt: 'MigrationPilot Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'tutorialSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {to: '/docs/api/overview', label: 'API Reference', position: 'left'},
        {
          href: 'https://github.com/migrationpilot/migrationpilot',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Documentation',
          items: [
            {label: 'Getting Started', to: '/docs/getting-started/quickstart'},
            {label: 'Architecture', to: '/docs/architecture/overview'},
            {label: 'API Reference', to: '/docs/api/overview'},
          ],
        },
        {
          title: 'Community',
          items: [
            {label: 'Stack Overflow', href: 'https://stackoverflow.com/questions/tagged/migrationpilot'},
            {label: 'Discord', href: 'https://discord.gg/migrationpilot'},
            {label: 'Twitter', href: 'https://twitter.com/migrationpilot'},
          ],
        },
        {
          title: 'More',
          items: [
            {label: 'Contributing', to: '/docs/contributing'},
            {label: 'GitHub', href: 'https://github.com/migrationpilot/migrationpilot'},
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} MigrationPilot. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['cobol', 'fortran', 'java', 'bash'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

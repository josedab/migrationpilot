---
sidebar_position: 4
---

# Search Configuration

Set up search functionality for your MigrationPilot documentation using Algolia DocSearch.

## Prerequisites

- Deployed MigrationPilot documentation site
- Algolia account (free for open source)

## Setting Up Algolia DocSearch

### 1. Apply for DocSearch

For open source projects, apply for free DocSearch at [docsearch.algolia.com/apply](https://docsearch.algolia.com/apply).

Requirements:
- Public documentation website
- Open source project
- Technical documentation (not marketing content)

### 2. Configure Algolia Credentials

Once approved, you'll receive:
- `appId`: Your Algolia application ID
- `apiKey`: Search-only API key (safe to expose)
- `indexName`: Your documentation index name

### 3. Install Dependencies

```bash
npm install @docusaurus/theme-search-algolia
```

### 4. Update Configuration

Add to `docusaurus.config.ts`:

```typescript
const config: Config = {
  // ... other config

  themeConfig: {
    // ... other theme config

    algolia: {
      appId: 'YOUR_APP_ID',
      apiKey: 'YOUR_SEARCH_API_KEY',
      indexName: 'migrationpilot',

      // Optional: Algolia search parameters
      contextualSearch: true,

      // Optional: path for search page
      searchPagePath: 'search',

      // Optional: insights for analytics
      insights: false,
    },
  } satisfies Preset.ThemeConfig,
};
```

### 5. Environment Variables

For security, use environment variables:

```bash
# .env
ALGOLIA_APP_ID=your_app_id
ALGOLIA_API_KEY=your_api_key
ALGOLIA_INDEX_NAME=migrationpilot
```

```typescript
// docusaurus.config.ts
algolia: {
  appId: process.env.ALGOLIA_APP_ID || 'YOUR_APP_ID',
  apiKey: process.env.ALGOLIA_API_KEY || 'YOUR_SEARCH_API_KEY',
  indexName: process.env.ALGOLIA_INDEX_NAME || 'migrationpilot',
},
```

## Self-Hosted Search

For air-gapped or self-hosted deployments, you can run your own Algolia crawler.

### Using DocSearch Scraper

```bash
# Install the scraper
docker pull algolia/docsearch-scraper

# Create config file
cat > docsearch-config.json << 'EOF'
{
  "index_name": "migrationpilot",
  "start_urls": ["https://docs.migrationpilot.dev/"],
  "sitemap_urls": ["https://docs.migrationpilot.dev/sitemap.xml"],
  "selectors": {
    "lvl0": {
      "selector": ".menu__link--active",
      "global": true,
      "default_value": "Documentation"
    },
    "lvl1": "article h1",
    "lvl2": "article h2",
    "lvl3": "article h3",
    "lvl4": "article h4",
    "content": "article p, article li"
  }
}
EOF

# Run the scraper
docker run -it --rm \
  -e APPLICATION_ID=YOUR_APP_ID \
  -e API_KEY=YOUR_ADMIN_API_KEY \
  -v $(pwd)/docsearch-config.json:/root/config.json \
  algolia/docsearch-scraper
```

### Automated Indexing

Add to your CI/CD pipeline:

```yaml
# .github/workflows/search-index.yml
name: Update Search Index

on:
  push:
    branches: [main]
    paths:
      - 'docs/site/docs/**'

jobs:
  index:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Run DocSearch Scraper
        run: |
          docker run --rm \
            -e APPLICATION_ID=${{ secrets.ALGOLIA_APP_ID }} \
            -e API_KEY=${{ secrets.ALGOLIA_ADMIN_KEY }} \
            -v ${{ github.workspace }}/docsearch-config.json:/root/config.json \
            algolia/docsearch-scraper
```

## Alternative: Local Search

For simpler setups without external dependencies, consider these alternatives when they support Docusaurus 3:

### Option 1: Built-in Search (Future)

Docusaurus may add built-in search in future versions. Check the [Docusaurus roadmap](https://docusaurus.io/community/team) for updates.

### Option 2: lunr.js Integration

For fully offline search:

```javascript
// Custom search implementation
// See: https://docusaurus.io/docs/search#using-your-own-search
```

## Keyboard Shortcuts

Once search is configured, users can:

| Shortcut | Action |
|----------|--------|
| `Ctrl+K` / `⌘+K` | Open search |
| `↑` / `↓` | Navigate results |
| `Enter` | Go to result |
| `Escape` | Close search |

## Troubleshooting

### Search Not Working

1. **Check API keys**: Ensure you're using the search-only key, not admin key
2. **Verify index**: Check your Algolia dashboard for indexed records
3. **Clear cache**: Run `npm run clear && npm run build`

### Missing Content

1. **Re-run scraper**: Index may be outdated
2. **Check selectors**: Ensure CSS selectors match your content structure
3. **Review sitemap**: Verify all pages are in `sitemap.xml`

### Slow Search

1. **Optimize index**: Remove unnecessary attributes
2. **Enable caching**: Use Algolia's built-in caching
3. **Review query rules**: Simplify search configuration

## Related Topics

- [Deployment Guide](/docs/deployment/cloud) - Deploy documentation site
- [Kubernetes Setup](/docs/deployment/kubernetes) - Production deployment
- [Security Configuration](/docs/deployment/security) - Secure your deployment

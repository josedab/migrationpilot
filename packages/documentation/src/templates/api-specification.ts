/**
 * API Specification Template
 * 
 * Generates API documentation for the modern implementation
 */

import type { DocumentTemplate } from '../types.js';

export const apiSpecificationTemplate: DocumentTemplate = {
  type: 'api-specification',
  name: 'API Specification',
  description: 'REST API documentation for the modernized system based on extracted business rules',
  
  sections: [
    {
      id: 'overview',
      title: 'API Overview',
      description: 'Introduction to the API',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'apiOverview',
      subsections: [
        {
          id: 'base-url',
          title: 'Base URL',
          description: 'API base endpoint',
          level: 2,
          required: true,
          contentType: 'static',
          staticContent: '```\nhttps://api.example.com/v1\n```',
        },
        {
          id: 'authentication',
          title: 'Authentication',
          description: 'How to authenticate',
          level: 2,
          required: true,
          contentType: 'static',
          staticContent: `All API requests require authentication using a Bearer token in the Authorization header:

\`\`\`
Authorization: Bearer <your-token>
\`\`\`

Tokens can be obtained through the authentication endpoint.`,
        },
        {
          id: 'rate-limits',
          title: 'Rate Limits',
          description: 'API rate limiting policy',
          level: 2,
          required: false,
          contentType: 'static',
          staticContent: `| Tier | Requests/Minute | Requests/Day |
|------|-----------------|--------------|
| Free | 60 | 1,000 |
| Standard | 600 | 50,000 |
| Enterprise | Unlimited | Unlimited |`,
        },
      ],
    },
    {
      id: 'endpoints-summary',
      title: 'Endpoints Summary',
      description: 'Quick reference of all endpoints',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'endpointsSummary',
    },
    {
      id: 'endpoints-detail',
      title: 'Endpoint Details',
      description: 'Detailed documentation for each endpoint',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'endpointsDetail',
    },
    {
      id: 'data-models',
      title: 'Data Models',
      description: 'Request and response schemas',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'dataModels',
    },
    {
      id: 'error-handling',
      title: 'Error Handling',
      description: 'Error codes and responses',
      level: 1,
      required: true,
      contentType: 'static',
      staticContent: `## Error Response Format

All errors return a consistent JSON structure:

\`\`\`json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": {}
  }
}
\`\`\`

## Common Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| INVALID_REQUEST | 400 | Request validation failed |
| UNAUTHORIZED | 401 | Missing or invalid authentication |
| FORBIDDEN | 403 | Insufficient permissions |
| NOT_FOUND | 404 | Resource not found |
| RATE_LIMITED | 429 | Too many requests |
| INTERNAL_ERROR | 500 | Server error |`,
    },
    {
      id: 'business-rule-mapping',
      title: 'Business Rule Mapping',
      description: 'Mapping of endpoints to business rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'businessRuleMapping',
    },
    {
      id: 'examples',
      title: 'Examples',
      description: 'Example API calls',
      level: 1,
      required: true,
      contentType: 'ai-generated',
      aiPrompt: 'Generate practical API usage examples based on the business rules. Include curl commands and expected responses for common use cases.',
    },
    {
      id: 'changelog',
      title: 'Changelog',
      description: 'API version history',
      level: 1,
      required: false,
      contentType: 'static',
      staticContent: `## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | TBD | Initial release |`,
    },
  ],
  
  variables: [
    { name: 'projectName', type: 'string', description: 'Name of the project', required: true },
    { name: 'apiVersion', type: 'string', description: 'API version', required: true, defaultValue: '1.0.0' },
    { name: 'baseUrl', type: 'string', description: 'API base URL', required: true },
    { name: 'totalEndpoints', type: 'number', description: 'Total number of endpoints', required: true },
  ],
  
  outputFormats: ['markdown', 'html', 'pdf', 'confluence'],
};

export default apiSpecificationTemplate;

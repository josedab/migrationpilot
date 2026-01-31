/**
 * GraphQL Server Setup
 * 
 * Integrates GraphQL with Hono using graphql-yoga
 */

import { Hono } from 'hono';
import { createYoga, createSchema } from 'graphql-yoga';
import { typeDefs } from './schema.js';
import { resolvers } from './resolvers.js';

// Create GraphQL schema
const schema = createSchema({
  typeDefs,
  resolvers,
});

// Create Yoga instance
const yoga = createYoga({
  schema,
  graphqlEndpoint: '/graphql',
  landingPage: true,
  graphiql: {
    title: 'MigrationPilot GraphQL',
  },
});

// Create Hono router for GraphQL
export const graphqlRouter = new Hono();

// Handle GraphQL requests
graphqlRouter.all('/', async (c) => {
  const response = await yoga.handle(c.req.raw, {});
  return new Response(response.body, {
    status: response.status,
    headers: response.headers,
  });
});

export { schema, yoga };

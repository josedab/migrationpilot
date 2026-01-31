import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['packages/**/*.test.ts', 'apps/**/*.test.ts'],
    exclude: ['**/node_modules/**', '**/dist/**'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      exclude: [
        'node_modules/',
        'dist/',
        '**/*.d.ts',
        '**/*.test.ts',
        '**/index.ts',
        'infrastructure/**',
        'docs/**',
        'examples/**',
        'scripts/**',
        // Exclude UI components (need separate React test environment)
        'packages/ui/**',
        // Exclude packages without tests yet (mark for future coverage)
        'packages/planner/**',
        'packages/tracing/src/capture/**',
        'packages/tracing/src/learning/**',
        'packages/tracing/src/tracers/**',
      ],
      thresholds: {
        // Thresholds for tested code paths
        // Target: progressively increase as tests are added
        lines: 20,
        functions: 30,
        branches: 25,
        statements: 20,
      },
    },
    testTimeout: 10000,
  },
});

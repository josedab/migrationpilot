/**
 * Copilot Module Index
 * 
 * Exports all copilot-related functionality for the IDE extension.
 */

export {
  MigrationCopilot,
  type CopilotConfig,
  type CopilotSession,
  type CopilotMessage,
  type SessionContext,
  type ExtractedRule,
  type RecentEdit,
  type InlineSuggestion,
  type CodeExplanation,
  type DataFlowInfo,
  type MigrationSuggestion as CopilotMigrationSuggestion,
  type SuggestedTestCase,
} from './migration-copilot.js';

export * from './confidence-visualizer.js';

export {
  IntelliJAdapter,
  BaseMigrationAction,
  ExplainCodeAction,
  MigrateCodeAction,
  ExtractRuleAction,
  createQuickFix,
  createMockDocument,
  type IntelliJPosition,
  type IntelliJRange,
  type IntelliJDocument,
  type IntelliJEditor,
  type IntelliJCaretModel,
  type IntelliJSelectionModel,
  type IntelliJProject,
  type IntelliJVirtualFile,
  type IntelliJAnAction,
  type IntelliJAnActionEvent,
  type IntelliJProblemDescriptor,
  type IntelliJHighlightType,
  type IntelliJQuickFix,
  type VSCodeRange,
  type VSCodeDiagnostic,
  type IntelliJMigrationService,
  type MigrationAnalysisResult,
  type MigrationSuggestion as IntelliJMigrationSuggestion,
  type MigrationResult,
  type InlineHint,
  type IntelliJPluginConfig,
  MigrationPilotIntelliJPlugin,
} from './intellij-adapter.js';

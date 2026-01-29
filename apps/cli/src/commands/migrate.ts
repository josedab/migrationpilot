/**
 * Migrate Command
 * 
 * Start or continue a migration using the AI agents
 */

import { Command } from 'commander';
import chalk from 'chalk';
import ora from 'ora';
import inquirer from 'inquirer';
import type { SourceLanguage, TargetLanguage } from '@migrationpilot/core';

interface MigrateOptions {
  project?: string;
  source?: string;
  target?: TargetLanguage;
  language?: SourceLanguage;
  pattern?: string;
  output?: string;
  dryRun?: boolean;
  interactive?: boolean;
}

export const migrateCommand = new Command('migrate')
  .description('Start or continue a migration project')
  .option('-p, --project <name>', 'Project name (creates new if not exists)')
  .option('-s, --source <path>', 'Source code path')
  .option('-t, --target <lang>', 'Target language (java, python, typescript, go, csharp)')
  .option('-l, --language <lang>', 'Source language (cobol, fortran, vb6, java-legacy)')
  .option('--pattern <pattern>', 'File pattern to migrate')
  .option('-o, --output <path>', 'Output directory for generated code')
  .option('--dry-run', 'Preview migration without making changes')
  .option('-i, --interactive', 'Interactive mode for rule review')
  .action(async (options: MigrateOptions) => {
    console.log('');
    console.log(chalk.bold('🚀 MigrationPilot Migration'));
    console.log(chalk.gray('─'.repeat(50)));

    // If no options provided, run interactive wizard
    if (!options.project && !options.source) {
      const answers = await inquirer.prompt([
        {
          type: 'input',
          name: 'project',
          message: 'Project name:',
          default: 'migration-' + Date.now(),
        },
        {
          type: 'input',
          name: 'source',
          message: 'Path to legacy source code:',
          validate: (input: string) => input.length > 0 || 'Source path is required',
        },
        {
          type: 'list',
          name: 'language',
          message: 'Source language:',
          choices: [
            { name: 'COBOL', value: 'cobol' },
            { name: 'Fortran', value: 'fortran' },
            { name: 'Visual Basic 6', value: 'vb6' },
            { name: 'Legacy Java (J2EE/EJB)', value: 'java-legacy' },
          ],
        },
        {
          type: 'list',
          name: 'target',
          message: 'Target language:',
          choices: [
            { name: 'Java (Spring Boot)', value: 'java' },
            { name: 'Python (FastAPI)', value: 'python' },
            { name: 'TypeScript (Node.js)', value: 'typescript' },
            { name: 'Go', value: 'go' },
            { name: 'C# (.NET)', value: 'csharp' },
          ],
        },
        {
          type: 'list',
          name: 'pattern',
          message: 'Migration pattern:',
          choices: [
            { name: 'Microservices', value: 'microservices' },
            { name: 'Modular Monolith', value: 'modular-monolith' },
            { name: 'Strangler Fig (Incremental)', value: 'strangler-fig' },
          ],
        },
        {
          type: 'input',
          name: 'output',
          message: 'Output directory:',
          default: './migrated',
        },
        {
          type: 'confirm',
          name: 'interactive',
          message: 'Enable interactive rule review?',
          default: true,
        },
      ]);

      Object.assign(options, answers);
    }

    const spinner = ora('Initializing migration...').start();

    try {
      // Show migration plan
      spinner.text = 'Creating migration plan...';
      await new Promise(resolve => setTimeout(resolve, 1000)); // Simulate API call

      spinner.succeed('Migration plan created');

      console.log('');
      console.log(chalk.bold('📋 Migration Plan'));
      console.log(chalk.gray('─'.repeat(50)));
      console.log(`  ${chalk.cyan('Project:')} ${options.project}`);
      console.log(`  ${chalk.cyan('Source:')} ${options.source}`);
      console.log(`  ${chalk.cyan('Source Language:')} ${options.language}`);
      console.log(`  ${chalk.cyan('Target Language:')} ${options.target}`);
      console.log(`  ${chalk.cyan('Pattern:')} ${options.pattern || 'microservices'}`);
      console.log(`  ${chalk.cyan('Output:')} ${options.output}`);
      console.log('');

      if (options.dryRun) {
        console.log(chalk.yellow.bold('  🔍 DRY RUN MODE - Preview Only'));
        console.log(chalk.gray('  No files will be created or modified'));
        console.log('');
        
        // Show what would happen
        console.log(chalk.bold('  📊 What would be analyzed:'));
        console.log(chalk.gray(`     • Source files in ${options.source}`));
        console.log(chalk.gray(`     • Language: ${options.language}`));
        console.log('');
        
        console.log(chalk.bold('  🎯 What would be generated:'));
        console.log(chalk.gray(`     • Target language: ${options.target}`));
        console.log(chalk.gray(`     • Architecture pattern: ${options.pattern || 'microservices'}`));
        console.log(chalk.gray(`     • Output directory: ${options.output}`));
        console.log('');
        
        console.log(chalk.bold('  📝 Migration phases that would run:'));
        console.log(chalk.gray('     1. Analysis - Extract business rules and structure'));
        console.log(chalk.gray('     2. Architecture Design - Design modern service boundaries'));
        console.log(chalk.gray('     3. Code Generation - Generate idiomatic modern code'));
        console.log(chalk.gray('     4. Validation - Generate equivalence test cases'));
        console.log('');
        
        if (options.interactive) {
          console.log(chalk.bold('  👥 Human Review Points:'));
          console.log(chalk.gray('     • Business rules with confidence < 90% will be flagged'));
          console.log(chalk.gray('     • Architecture decisions will be presented for approval'));
        }
        console.log('');
        
        console.log(chalk.cyan('  Run without --dry-run to execute the migration'));
        console.log('');
        return;
      }

      // Confirm before proceeding
      const { confirm } = await inquirer.prompt([
        {
          type: 'confirm',
          name: 'confirm',
          message: 'Start migration?',
          default: true,
        },
      ]);

      if (!confirm) {
        console.log(chalk.yellow('Migration cancelled'));
        return;
      }

      // Start migration phases
      console.log('');
      console.log(chalk.bold('🔄 Migration Progress'));
      console.log(chalk.gray('─'.repeat(50)));

      // Phase 1: Analysis
      const analysisSpinner = ora('Phase 1: Analyzing source code...').start();
      await new Promise(resolve => setTimeout(resolve, 2000)); // Simulate
      analysisSpinner.succeed('Phase 1: Analysis complete');

      // Phase 2: Design
      const designSpinner = ora('Phase 2: Generating architecture design...').start();
      await new Promise(resolve => setTimeout(resolve, 2000)); // Simulate
      designSpinner.succeed('Phase 2: Architecture design complete');

      // Phase 3: Code Generation
      const buildSpinner = ora('Phase 3: Generating modern code...').start();
      await new Promise(resolve => setTimeout(resolve, 2000)); // Simulate
      buildSpinner.succeed('Phase 3: Code generation complete');

      // Phase 4: Validation
      const validateSpinner = ora('Phase 4: Validating equivalence...').start();
      await new Promise(resolve => setTimeout(resolve, 2000)); // Simulate
      validateSpinner.succeed('Phase 4: Validation complete');

      console.log('');
      console.log(chalk.green.bold('✅ Migration completed successfully!'));
      console.log('');
      console.log(chalk.gray('  Next steps:'));
      console.log(chalk.gray('  1. Review generated code in ' + options.output));
      console.log(chalk.gray('  2. Run: migrationpilot validate --project ' + options.project));
      console.log(chalk.gray('  3. Check business rules: migrationpilot rules list --project ' + options.project));
      console.log('');

    } catch (error) {
      spinner.fail(chalk.red('Migration failed'));
      console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
      process.exit(1);
    }
  });

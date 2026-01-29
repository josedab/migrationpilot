#!/usr/bin/env node
/**
 * MigrationPilot CLI
 * 
 * Command-line interface for legacy code modernization
 */

import { Command } from 'commander';
import chalk from 'chalk';
import { config } from 'dotenv';
import { analyzeCommand } from './commands/analyze.js';
import { migrateCommand } from './commands/migrate.js';
import { validateCommand } from './commands/validate.js';
import { configCommand } from './commands/config.js';
import { projectCommand } from './commands/project.js';
import { rulesCommand } from './commands/rules.js';
import { traceCommand } from './commands/trace.js';
import { oracleCommand } from './commands/oracle.js';
import { graphCommand } from './commands/graph.js';

// Load environment variables
config();

const program = new Command();

program
  .name('migrationpilot')
  .description(chalk.bold('🚀 MigrationPilot - AI-powered legacy code modernization'))
  .version('0.1.0')
  .option('-v, --verbose', 'Enable verbose output')
  .option('-q, --quiet', 'Suppress all output except errors')
  .option('--api-url <url>', 'MigrationPilot API URL', process.env.MIGRATIONPILOT_API_URL || 'http://localhost:3001')
  .option('--api-key <key>', 'MigrationPilot API Key', process.env.MIGRATIONPILOT_API_KEY);

// Register commands
program.addCommand(analyzeCommand);
program.addCommand(migrateCommand);
program.addCommand(validateCommand);
program.addCommand(configCommand);
program.addCommand(projectCommand);
program.addCommand(rulesCommand);
program.addCommand(traceCommand);
program.addCommand(oracleCommand);
program.addCommand(graphCommand);

// Custom help
program.on('--help', () => {
  console.log('');
  console.log(chalk.gray('Examples:'));
  console.log(chalk.cyan('  $ migrationpilot analyze ./legacy-code --language cobol'));
  console.log(chalk.cyan('  $ migrationpilot migrate --project my-migration --target java'));
  console.log(chalk.cyan('  $ migrationpilot validate --project my-migration'));
  console.log(chalk.cyan('  $ migrationpilot trace capture host:3270 --platform zos'));
  console.log(chalk.cyan('  $ migrationpilot oracle train executions.json --name "Interest Model"'));
  console.log(chalk.cyan('  $ migrationpilot graph export graph.json --format graphml'));
  console.log('');
  console.log(chalk.gray('Documentation: https://docs.migrationpilot.dev'));
});

// Error handling
program.exitOverride((err) => {
  if (err.code === 'commander.help') {
    process.exit(0);
  }
  console.error(chalk.red(`Error: ${err.message}`));
  process.exit(1);
});

program.parse();

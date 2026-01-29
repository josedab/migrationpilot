/**
 * Analyze Command
 * 
 * Analyze legacy code to extract structure and business rules
 */

import { Command } from 'commander';
import chalk from 'chalk';
import ora from 'ora';
import { glob } from 'glob';
import { readFile } from 'fs/promises';
import { table } from 'table';
import { createParser } from '@migrationpilot/parsers';
import type { SourceLanguage, BusinessRule } from '@migrationpilot/core';

interface AnalyzeOptions {
  language: SourceLanguage;
  output?: string;
  format?: 'json' | 'yaml' | 'table';
  rules?: boolean;
  verbose?: boolean;
}

export const analyzeCommand = new Command('analyze')
  .description('Analyze legacy code to extract structure and business rules')
  .argument('<path>', 'Path to file or directory to analyze')
  .option('-l, --language <lang>', 'Source language (cobol, fortran, vb6, java-legacy)', 'cobol')
  .option('-o, --output <file>', 'Output file path')
  .option('-f, --format <format>', 'Output format (json, yaml, table)', 'table')
  .option('-r, --rules', 'Extract business rules')
  .option('--verbose', 'Show detailed output')
  .action(async (path: string, options: AnalyzeOptions) => {
    const spinner = ora('Discovering files...').start();

    try {
      // Discover files
      const patterns: Record<SourceLanguage, string[]> = {
        cobol: ['**/*.cbl', '**/*.cob', '**/*.CBL', '**/*.COB'],
        fortran: ['**/*.f', '**/*.for', '**/*.f90', '**/*.f95', '**/*.F', '**/*.FOR'],
        vb6: ['**/*.bas', '**/*.cls', '**/*.frm'],
        vba: ['**/*.bas', '**/*.cls'],
        'java-legacy': ['**/*.java'],
        'pl1': ['**/*.pl1', '**/*.pli'],
        'rpg': ['**/*.rpg', '**/*.rpgle'],
      };

      const filePatterns = patterns[options.language] || ['**/*'];
      const files = await glob(filePatterns, { cwd: path, nodir: true, absolute: true });

      if (files.length === 0) {
        spinner.fail(chalk.red(`No ${options.language} files found in ${path}`));
        process.exit(1);
      }

      spinner.text = `Found ${files.length} files. Analyzing...`;

      // Create parser
      const parser = createParser(options.language);

      // Analyze each file
      const results = [];
      let totalDataStructures = 0;
      let totalProcedures = 0;
      let totalErrors = 0;
      let totalWarnings = 0;

      for (const file of files) {
        const content = await readFile(file, 'utf-8');
        const result = parser.parse(content, file);

        totalDataStructures += result.dataStructures.length;
        totalProcedures += result.procedures.length;
        totalErrors += result.errors.length;
        totalWarnings += result.warnings.length;

        results.push({
          file,
          ...result,
        });
      }

      spinner.succeed(chalk.green(`Analyzed ${files.length} files`));

      // Display summary
      console.log('');
      console.log(chalk.bold('📊 Analysis Summary'));
      console.log(chalk.gray('─'.repeat(50)));
      
      const summaryData = [
        ['Metric', 'Count'],
        ['Files', files.length.toString()],
        ['Data Structures', totalDataStructures.toString()],
        ['Procedures', totalProcedures.toString()],
        ['Errors', totalErrors.toString()],
        ['Warnings', totalWarnings.toString()],
      ];

      console.log(table(summaryData, {
        border: {
          topBody: `─`,
          topJoin: `┬`,
          topLeft: `┌`,
          topRight: `┐`,
          bottomBody: `─`,
          bottomJoin: `┴`,
          bottomLeft: `└`,
          bottomRight: `┘`,
          bodyLeft: `│`,
          bodyRight: `│`,
          bodyJoin: `│`,
          joinBody: `─`,
          joinLeft: `├`,
          joinRight: `┤`,
          joinJoin: `┼`,
        },
      }));

      // Show procedures if verbose
      if (options.verbose) {
        console.log('');
        console.log(chalk.bold('📝 Procedures Found'));
        console.log(chalk.gray('─'.repeat(50)));

        for (const result of results) {
          for (const proc of result.procedures) {
            const complexity = proc.complexity || 0;
            const complexityColor = complexity < 10 ? 'green' : complexity < 20 ? 'yellow' : 'red';
            console.log(`  ${chalk.cyan(proc.name)} (${chalk[complexityColor](`complexity: ${complexity}`)})`);
          }
        }
      }

      // Extract business rules if requested
      if (options.rules) {
        console.log('');
        console.log(chalk.bold('📋 Business Rules Detected'));
        console.log(chalk.gray('─'.repeat(50)));
        console.log(chalk.yellow('  Note: Full rule extraction requires AI analysis via the API'));
        console.log(chalk.gray('  Use: migrationpilot migrate --project <name> for AI-powered extraction'));
      }

      // Output to file if requested
      if (options.output) {
        const { writeFile } = await import('fs/promises');
        const outputData = {
          summary: {
            files: files.length,
            dataStructures: totalDataStructures,
            procedures: totalProcedures,
            errors: totalErrors,
            warnings: totalWarnings,
          },
          results,
        };

        if (options.format === 'json') {
          await writeFile(options.output, JSON.stringify(outputData, null, 2));
        } else if (options.format === 'yaml') {
          const YAML = await import('yaml');
          await writeFile(options.output, YAML.stringify(outputData));
        }

        console.log('');
        console.log(chalk.green(`✓ Results saved to ${options.output}`));
      }

    } catch (error) {
      spinner.fail(chalk.red('Analysis failed'));
      console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
      process.exit(1);
    }
  });

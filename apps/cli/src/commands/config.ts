/**
 * Config Command
 * 
 * Manage MigrationPilot configuration
 */

import { Command } from 'commander';
import chalk from 'chalk';
import { homedir } from 'os';
import { join } from 'path';
import { mkdir, readFile, writeFile } from 'fs/promises';

interface ConfigData {
  apiUrl?: string;
  apiKey?: string;
  defaultLanguage?: string;
  defaultTarget?: string;
  outputFormat?: string;
}

const CONFIG_DIR = join(homedir(), '.migrationpilot');
const CONFIG_FILE = join(CONFIG_DIR, 'config.json');

export const configCommand = new Command('config')
  .description('Manage MigrationPilot configuration');

configCommand
  .command('init')
  .description('Initialize configuration')
  .action(async () => {
    const inquirer = await import('inquirer');

    const answers = await inquirer.default.prompt([
      {
        type: 'input',
        name: 'apiUrl',
        message: 'MigrationPilot API URL:',
        default: 'http://localhost:3001',
      },
      {
        type: 'password',
        name: 'apiKey',
        message: 'API Key (optional):',
      },
      {
        type: 'list',
        name: 'defaultLanguage',
        message: 'Default source language:',
        choices: ['cobol', 'fortran', 'vb6', 'java-legacy'],
        default: 'cobol',
      },
      {
        type: 'list',
        name: 'defaultTarget',
        message: 'Default target language:',
        choices: ['java', 'python', 'typescript', 'go', 'csharp'],
        default: 'java',
      },
    ]);

    await mkdir(CONFIG_DIR, { recursive: true });
    await writeFile(CONFIG_FILE, JSON.stringify(answers, null, 2));

    console.log('');
    console.log(chalk.green('✓ Configuration saved to ' + CONFIG_FILE));
  });

configCommand
  .command('get <key>')
  .description('Get a configuration value')
  .action(async (key: string) => {
    try {
      const config = JSON.parse(await readFile(CONFIG_FILE, 'utf-8')) as ConfigData;
      const value = config[key as keyof ConfigData];
      
      if (value !== undefined) {
        console.log(value);
      } else {
        console.log(chalk.yellow(`Configuration key '${key}' not found`));
      }
    } catch {
      console.log(chalk.red('No configuration found. Run: migrationpilot config init'));
    }
  });

configCommand
  .command('set <key> <value>')
  .description('Set a configuration value')
  .action(async (key: string, value: string) => {
    let config: ConfigData = {};
    
    try {
      config = JSON.parse(await readFile(CONFIG_FILE, 'utf-8'));
    } catch {
      await mkdir(CONFIG_DIR, { recursive: true });
    }

    config[key as keyof ConfigData] = value;
    await writeFile(CONFIG_FILE, JSON.stringify(config, null, 2));

    console.log(chalk.green(`✓ Set ${key} = ${value}`));
  });

configCommand
  .command('list')
  .description('List all configuration values')
  .action(async () => {
    try {
      const config = JSON.parse(await readFile(CONFIG_FILE, 'utf-8')) as ConfigData;
      
      console.log('');
      console.log(chalk.bold('MigrationPilot Configuration'));
      console.log(chalk.gray('─'.repeat(40)));
      
      for (const [key, value] of Object.entries(config)) {
        if (key === 'apiKey' && value) {
          console.log(`  ${chalk.cyan(key)}: ${chalk.gray('****' + (value as string).slice(-4))}`);
        } else {
          console.log(`  ${chalk.cyan(key)}: ${value}`);
        }
      }
      console.log('');
    } catch {
      console.log(chalk.yellow('No configuration found. Run: migrationpilot config init'));
    }
  });

configCommand
  .command('path')
  .description('Show configuration file path')
  .action(() => {
    console.log(CONFIG_FILE);
  });

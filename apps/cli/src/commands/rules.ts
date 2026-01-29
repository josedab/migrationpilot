/**
 * Rules Command
 * 
 * Manage extracted business rules
 */

import { Command } from 'commander';
import chalk from 'chalk';
import { table } from 'table';

export const rulesCommand = new Command('rules')
  .description('Manage extracted business rules');

rulesCommand
  .command('list')
  .description('List business rules')
  .option('-p, --project <name>', 'Project name')
  .option('-t, --type <type>', 'Filter by type (calculation, validation, decision, temporal)')
  .option('-c, --confidence <min>', 'Minimum confidence score', '0')
  .option('--pending', 'Show only rules pending review')
  .action(async (options: { project?: string; type?: string; confidence?: string; pending?: boolean }) => {
    console.log('');
    console.log(chalk.bold('📋 Business Rules'));
    console.log(chalk.gray('─'.repeat(70)));

    // Simulated rules data
    const rules = [
      {
        id: 'BR-001',
        name: 'Interest Rate Calculation',
        type: 'calculation',
        confidence: 95,
        status: 'approved',
        source: 'CALCINT.cbl:245',
      },
      {
        id: 'BR-002',
        name: 'Account Balance Validation',
        type: 'validation',
        confidence: 88,
        status: 'pending',
        source: 'ACCTVAL.cbl:120',
      },
      {
        id: 'BR-003',
        name: 'Loan Eligibility Decision',
        type: 'decision',
        confidence: 92,
        status: 'approved',
        source: 'LOANELIG.cbl:450',
      },
      {
        id: 'BR-004',
        name: 'Fiscal Year End Processing',
        type: 'temporal',
        confidence: 75,
        status: 'needs-review',
        source: 'FYEND.cbl:80',
      },
      {
        id: 'BR-005',
        name: 'Overdraft Fee Calculation',
        type: 'calculation',
        confidence: 98,
        status: 'approved',
        source: 'OVRDFT.cbl:200',
      },
    ];

    let filtered = rules;
    
    if (options.type) {
      filtered = filtered.filter(r => r.type === options.type);
    }
    if (options.confidence) {
      filtered = filtered.filter(r => r.confidence >= parseInt(options.confidence));
    }
    if (options.pending) {
      filtered = filtered.filter(r => r.status !== 'approved');
    }

    const statusColors: Record<string, string> = {
      'approved': 'green',
      'pending': 'yellow',
      'needs-review': 'red',
      'rejected': 'gray',
    };

    const confidenceColor = (conf: number) => 
      conf >= 90 ? 'green' : conf >= 70 ? 'yellow' : 'red';

    const data = [
      ['ID', 'Name', 'Type', 'Confidence', 'Status', 'Source'],
      ...filtered.map(r => [
        chalk.cyan(r.id),
        r.name,
        r.type,
        chalk[confidenceColor(r.confidence)](`${r.confidence}%`),
        chalk[statusColors[r.status] || 'white'](r.status),
        chalk.gray(r.source),
      ]),
    ];

    console.log(table(data, {
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

    console.log(chalk.gray(`  Showing ${filtered.length} of ${rules.length} rules`));
    console.log('');
  });

rulesCommand
  .command('show <id>')
  .description('Show rule details')
  .action(async (id: string) => {
    console.log('');
    console.log(chalk.bold(`📋 Business Rule: ${chalk.cyan(id)}`));
    console.log(chalk.gray('─'.repeat(50)));

    // Simulated rule detail
    console.log(`  ${chalk.gray('Name:')} Interest Rate Calculation`);
    console.log(`  ${chalk.gray('Type:')} calculation`);
    console.log(`  ${chalk.gray('Confidence:')} ${chalk.green('95%')}`);
    console.log(`  ${chalk.gray('Status:')} ${chalk.green('approved')}`);
    console.log('');
    
    console.log(chalk.bold('  Description'));
    console.log(chalk.gray('  Calculates the interest rate based on account type,'));
    console.log(chalk.gray('  balance tier, and customer loyalty status.'));
    console.log('');
    
    console.log(chalk.bold('  Source Reference'));
    console.log(`  ${chalk.cyan('File:')} CALCINT.cbl`);
    console.log(`  ${chalk.cyan('Lines:')} 245-280`);
    console.log('');
    
    console.log(chalk.bold('  Formula'));
    console.log(chalk.yellow('  IF ACCT-TYPE = "SAVINGS"'));
    console.log(chalk.yellow('    IF BALANCE > 10000'));
    console.log(chalk.yellow('      RATE = BASE-RATE + 0.5'));
    console.log(chalk.yellow('    ELSE'));
    console.log(chalk.yellow('      RATE = BASE-RATE'));
    console.log(chalk.yellow('  END-IF'));
    console.log('');
    
    console.log(chalk.bold('  Generated Code (Java)'));
    console.log(chalk.green('  public BigDecimal calculateInterestRate(Account account) {'));
    console.log(chalk.green('    BigDecimal rate = baseRate;'));
    console.log(chalk.green('    if ("SAVINGS".equals(account.getType())) {'));
    console.log(chalk.green('      if (account.getBalance().compareTo(BigDecimal.valueOf(10000)) > 0) {'));
    console.log(chalk.green('        rate = rate.add(BigDecimal.valueOf(0.5));'));
    console.log(chalk.green('      }'));
    console.log(chalk.green('    }'));
    console.log(chalk.green('    return rate;'));
    console.log(chalk.green('  }'));
    console.log('');
  });

rulesCommand
  .command('approve <id>')
  .description('Approve a business rule')
  .option('-c, --comment <text>', 'Add a comment')
  .action(async (id: string, options: { comment?: string }) => {
    console.log(chalk.green(`✓ Approved rule: ${id}`));
    if (options.comment) {
      console.log(chalk.gray(`  Comment: ${options.comment}`));
    }
  });

rulesCommand
  .command('reject <id>')
  .description('Reject a business rule')
  .option('-r, --reason <text>', 'Rejection reason', 'Requires manual review')
  .action(async (id: string, options: { reason: string }) => {
    console.log(chalk.yellow(`✗ Rejected rule: ${id}`));
    console.log(chalk.gray(`  Reason: ${options.reason}`));
  });

rulesCommand
  .command('export')
  .description('Export rules to file')
  .option('-p, --project <name>', 'Project name')
  .option('-f, --format <format>', 'Export format (json, yaml, csv, pdf)', 'json')
  .option('-o, --output <file>', 'Output file')
  .action(async (options: { project?: string; format: string; output?: string }) => {
    const output = options.output || `rules-export.${options.format}`;
    console.log(chalk.green(`✓ Exported rules to ${output}`));
  });

rulesCommand
  .command('import <file>')
  .description('Import rules from file')
  .option('-p, --project <name>', 'Project name')
  .option('--merge', 'Merge with existing rules')
  .action(async (file: string, options: { project?: string; merge?: boolean }) => {
    console.log(chalk.green(`✓ Imported rules from ${file}`));
    if (options.merge) {
      console.log(chalk.gray('  Mode: Merged with existing rules'));
    }
  });

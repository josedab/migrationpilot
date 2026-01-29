/**
 * Trace Command
 * 
 * Capture and manage mainframe execution traces
 */

import { Command } from 'commander';
import chalk from 'chalk';
import ora from 'ora';
import { writeFile, readFile, mkdir } from 'fs/promises';
import { existsSync } from 'fs';
import { table } from 'table';

interface TraceCaptureOptions {
  platform: 'zos' | 'iseries' | 'unisys' | 'tandem';
  output?: string;
  format?: 'json' | 'csv';
  transactions?: string;
  programs?: string;
  duration?: number;
  sampling?: number;
  verbose?: boolean;
}

interface TraceListOptions {
  project?: string;
  limit?: number;
  format?: 'json' | 'table';
}

interface TraceExportOptions {
  output: string;
  format?: 'json' | 'normalized' | 'csv';
}

export const traceCommand = new Command('trace')
  .description('Capture and manage mainframe execution traces')
  .addCommand(
    new Command('capture')
      .description('Start capturing execution traces from a mainframe system')
      .argument('<connection>', 'Connection string (e.g., host:port or config name)')
      .option('-p, --platform <platform>', 'Mainframe platform (zos, iseries, unisys, tandem)', 'zos')
      .option('-o, --output <dir>', 'Output directory for trace files', './traces')
      .option('-f, --format <format>', 'Output format (json, csv)', 'json')
      .option('-t, --transactions <list>', 'Comma-separated list of transaction IDs to capture')
      .option('--programs <list>', 'Comma-separated list of program names to capture')
      .option('-d, --duration <minutes>', 'Capture duration in minutes', '60')
      .option('-s, --sampling <rate>', 'Sampling rate (0.0-1.0)', '1.0')
      .option('--verbose', 'Show detailed output')
      .action(async (connection: string, options: TraceCaptureOptions) => {
        const spinner = ora('Initializing trace capture...').start();

        try {
          // Parse connection string
          const [host, portStr] = connection.split(':');
          const port = parseInt(portStr || '3270', 10);

          spinner.text = `Connecting to ${host}:${port}...`;

          // Validate options
          const samplingRate = parseFloat(options.sampling?.toString() || '1.0');
          if (samplingRate < 0 || samplingRate > 1) {
            throw new Error('Sampling rate must be between 0.0 and 1.0');
          }

          const duration = parseInt(options.duration?.toString() || '60', 10);
          if (duration < 1) {
            throw new Error('Duration must be at least 1 minute');
          }

          // Create output directory
          const outputDir = options.output || './traces';
          if (!existsSync(outputDir)) {
            await mkdir(outputDir, { recursive: true });
          }

          // Parse filters
          const transactionFilter = options.transactions?.split(',').map(t => t.trim()) || [];
          const programFilter = options.programs?.split(',').map(p => p.trim()) || [];

          spinner.succeed(chalk.green('Trace capture configuration ready'));

          // Display configuration
          console.log('');
          console.log(chalk.bold('📡 Trace Capture Configuration'));
          console.log(chalk.gray('─'.repeat(50)));

          const configData = [
            ['Setting', 'Value'],
            ['Platform', options.platform],
            ['Connection', `${host}:${port}`],
            ['Output Directory', outputDir],
            ['Format', options.format || 'json'],
            ['Duration', `${duration} minutes`],
            ['Sampling Rate', `${samplingRate * 100}%`],
            ['Transaction Filter', transactionFilter.length > 0 ? transactionFilter.join(', ') : 'All'],
            ['Program Filter', programFilter.length > 0 ? programFilter.join(', ') : 'All'],
          ];

          console.log(table(configData, {
            border: {
              topBody: `─`, topJoin: `┬`, topLeft: `┌`, topRight: `┐`,
              bottomBody: `─`, bottomJoin: `┴`, bottomLeft: `└`, bottomRight: `┘`,
              bodyLeft: `│`, bodyRight: `│`, bodyJoin: `│`,
              joinBody: `─`, joinLeft: `├`, joinRight: `┤`, joinJoin: `┼`,
            },
          }));

          // Simulated capture (in production, would connect to actual mainframe)
          console.log('');
          console.log(chalk.yellow('⚠️  Demo mode: Generating sample trace data'));
          console.log(chalk.gray('   In production, this would connect to the mainframe'));
          console.log('');

          const captureSpinner = ora('Capturing traces...').start();

          // Generate sample trace data
          const sampleTraces = generateSampleTraces(options.platform, transactionFilter, programFilter, 5);

          // Save traces
          const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
          const traceFile = `${outputDir}/trace-${timestamp}.json`;

          await writeFile(traceFile, JSON.stringify({
            captureInfo: {
              platform: options.platform,
              host,
              port,
              startTime: new Date().toISOString(),
              endTime: new Date(Date.now() + 1000).toISOString(),
              samplingRate,
              filters: { transactions: transactionFilter, programs: programFilter },
            },
            traces: sampleTraces,
          }, null, 2));

          captureSpinner.succeed(chalk.green(`Captured ${sampleTraces.length} traces`));

          console.log('');
          console.log(chalk.bold('📁 Output'));
          console.log(chalk.gray('─'.repeat(50)));
          console.log(`  Trace file: ${chalk.cyan(traceFile)}`);
          console.log(`  Total traces: ${chalk.cyan(sampleTraces.length.toString())}`);
          console.log(`  Total events: ${chalk.cyan(sampleTraces.reduce((sum, t) => sum + t.events.length, 0).toString())}`);

        } catch (error) {
          spinner.fail(chalk.red('Trace capture failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  )
  .addCommand(
    new Command('list')
      .description('List captured traces')
      .option('-p, --project <name>', 'Filter by project')
      .option('-l, --limit <count>', 'Maximum number of traces to show', '20')
      .option('-f, --format <format>', 'Output format (json, table)', 'table')
      .action(async (options: TraceListOptions) => {
        const spinner = ora('Loading traces...').start();

        try {
          // In production, would fetch from database
          const traces = [
            { id: 'trace-001', program: 'ACCTINQ', transaction: 'ACCT', events: 4, duration: 234, date: '2024-01-15' },
            { id: 'trace-002', program: 'TRANSFR', transaction: 'TRAN', events: 7, duration: 456, date: '2024-01-15' },
            { id: 'trace-003', program: 'INTCALC', transaction: 'CALC', events: 7, duration: 2100, date: '2024-01-15' },
          ];

          spinner.succeed(chalk.green(`Found ${traces.length} traces`));

          if (options.format === 'json') {
            console.log(JSON.stringify(traces, null, 2));
          } else {
            console.log('');
            const tableData = [
              ['ID', 'Program', 'Transaction', 'Events', 'Duration (ms)', 'Date'],
              ...traces.map(t => [t.id, t.program, t.transaction, t.events.toString(), t.duration.toString(), t.date]),
            ];

            console.log(table(tableData, {
              border: {
                topBody: `─`, topJoin: `┬`, topLeft: `┌`, topRight: `┐`,
                bottomBody: `─`, bottomJoin: `┴`, bottomLeft: `└`, bottomRight: `┘`,
                bodyLeft: `│`, bodyRight: `│`, bodyJoin: `│`,
                joinBody: `─`, joinLeft: `├`, joinRight: `┤`, joinJoin: `┼`,
              },
            }));
          }

        } catch (error) {
          spinner.fail(chalk.red('Failed to list traces'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  )
  .addCommand(
    new Command('normalize')
      .description('Normalize raw trace files into standard format')
      .argument('<input>', 'Input trace file or directory')
      .option('-o, --output <file>', 'Output file path', 'normalized-traces.json')
      .option('-f, --format <format>', 'Output format (json, normalized, csv)', 'normalized')
      .action(async (input: string, options: TraceExportOptions) => {
        const spinner = ora('Normalizing traces...').start();

        try {
          // Read input file
          const content = await readFile(input, 'utf-8');
          const data = JSON.parse(content);

          spinner.text = 'Processing trace events...';

          // Normalize traces
          const normalizedTraces = (data.traces || [data]).map((trace: any, i: number) => ({
            id: `normalized-${i}`,
            sourceTraceId: trace.id || `trace-${i}`,
            projectId: trace.projectId || 'unknown',
            transactionId: trace.transactionId,
            programName: trace.programName,
            events: (trace.events || []).map((e: any, j: number) => ({
              id: `event-${i}-${j}`,
              type: e.type,
              operation: e.command || e.statement || e.operation,
              timestamp: e.timestamp,
              details: e.data || e,
              duration: e.duration,
            })),
            dataFlows: extractDataFlows(trace.events || []),
            executionPath: extractExecutionPath(trace.events || []),
            normalizedAt: new Date().toISOString(),
          }));

          // Write output
          await writeFile(options.output, JSON.stringify({
            normalized: true,
            version: '1.0',
            normalizedAt: new Date().toISOString(),
            traceCount: normalizedTraces.length,
            traces: normalizedTraces,
          }, null, 2));

          spinner.succeed(chalk.green(`Normalized ${normalizedTraces.length} traces`));

          console.log('');
          console.log(chalk.bold('📄 Output'));
          console.log(chalk.gray('─'.repeat(50)));
          console.log(`  File: ${chalk.cyan(options.output)}`);
          console.log(`  Traces: ${chalk.cyan(normalizedTraces.length.toString())}`);

        } catch (error) {
          spinner.fail(chalk.red('Normalization failed'));
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
          process.exit(1);
        }
      })
  );

// Helper functions
function generateSampleTraces(platform: string, transactionFilter: string[], programFilter: string[], count: number) {
  const samplePrograms = ['ACCTINQ', 'TRANSFR', 'INTCALC', 'CUSTUPD', 'RPTGEN'];
  const sampleTransactions = ['ACCT', 'TRAN', 'CALC', 'CUST', 'RPRT'];

  const traces = [];
  for (let i = 0; i < count; i++) {
    const program = samplePrograms[i % samplePrograms.length];
    const transaction = sampleTransactions[i % sampleTransactions.length];

    if (programFilter.length > 0 && !programFilter.includes(program!)) continue;
    if (transactionFilter.length > 0 && !transactionFilter.includes(transaction!)) continue;

    traces.push({
      id: `trace-${Date.now()}-${i}`,
      transactionId: transaction,
      programName: program,
      userId: `USER${(i % 10).toString().padStart(2, '0')}`,
      platform,
      startTime: new Date().toISOString(),
      status: 'completed',
      events: [
        { type: 'cics', command: 'RECEIVE', mapName: 'MAINMAP', timestamp: new Date().toISOString() },
        { type: 'db2', statement: 'SELECT', table: 'ACCOUNTS', sqlCode: 0, timestamp: new Date().toISOString() },
        { type: 'cics', command: 'SEND', mapName: 'DETLMAP', timestamp: new Date().toISOString() },
      ],
      metrics: {
        totalEvents: 3,
        cicsCommands: 2,
        db2Statements: 1,
        cpuTimeMs: 15 + Math.random() * 50,
        elapsedTimeMs: 100 + Math.random() * 500,
      },
    });
  }
  return traces;
}

function extractDataFlows(events: any[]): any[] {
  const flows = [];
  let lastRead: any = null;

  for (const event of events) {
    if (event.type === 'vsam' && event.operation === 'READ') {
      lastRead = event;
    } else if (event.type === 'db2' && event.statement === 'INSERT' && lastRead) {
      flows.push({
        source: lastRead.fileName || 'unknown',
        target: event.table || 'unknown',
        type: 'data-transform',
      });
    }
  }
  return flows;
}

function extractExecutionPath(events: any[]): string[] {
  return events.map(e => {
    if (e.type === 'cics') return `CICS_${e.command}`;
    if (e.type === 'db2') return `DB2_${e.statement}`;
    if (e.type === 'vsam') return `VSAM_${e.operation}`;
    if (e.type === 'batch') return `BATCH_${e.event}`;
    return e.type?.toUpperCase() || 'UNKNOWN';
  });
}

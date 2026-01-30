/**
 * Behavioral Learning Engine
 * 
 * Learns expected behavior from historical execution data by building
 * statistical models, discovering invariants, and identifying edge cases.
 */

import { randomUUID } from 'crypto';
import type {
  BehavioralModel,
  BehavioralModelType,
  FieldSignature,
  ModelStatistics,
  Distribution,
  CorrelationMatrix,
  LearnedInvariant,
  LearnedConstraint,
  DiscoveredEdgeCase,
  ModelQuality,
  HistoricalExecution,
  OracleConfig,
} from './types.js';

const DEFAULT_CONFIG: Partial<OracleConfig> = {
  minSamplesForModel: 50,
  confidenceThreshold: 0.8,
  maxInvariantsPerModel: 100,
  strictMode: false,
  enableContinuousLearning: true,
  retrainThreshold: 0.1,
  cacheEnabled: true,
  maxCacheSize: 1000,
};

export class BehavioralLearningEngine {
  private config: OracleConfig;
  private models = new Map<string, BehavioralModel>();

  constructor(config: Partial<OracleConfig> & { projectId: string }) {
    this.config = { 
      ...DEFAULT_CONFIG, 
      ...config,
      defaultTolerance: config.defaultTolerance || { numeric: 0.001, string: 'exact' },
    } as OracleConfig;
  }

  /**
   * Train a behavioral model from historical execution data
   */
  async trainModel(
    data: HistoricalExecution[],
    name: string,
    ruleId?: string
  ): Promise<BehavioralModel> {
    if (data.length < this.config.minSamplesForModel) {
      throw new Error(
        `Insufficient training data: ${data.length} samples (minimum: ${this.config.minSamplesForModel})`
      );
    }

    const modelId = `model_${randomUUID()}`;
    const startTime = Date.now();

    // Phase 1: Learn field signatures
    const inputSignature = this.learnFieldSignatures(data, 'inputs');
    const outputSignature = this.learnFieldSignatures(data, 'outputs');

    // Phase 2: Compute statistics
    const statistics = this.computeStatistics(data, inputSignature, outputSignature);

    // Phase 3: Discover invariants
    const invariants = this.discoverInvariants(data, inputSignature, outputSignature);

    // Phase 4: Learn constraints
    const constraints = this.learnConstraints(data, inputSignature, outputSignature);

    // Phase 5: Identify edge cases
    const edgeCases = this.identifyEdgeCases(data, statistics, invariants);

    // Phase 6: Assess model quality
    const quality = this.assessModelQuality(data, invariants, constraints, edgeCases);

    // Determine model type
    const modelType = this.determineModelType(statistics, invariants);

    const model: BehavioralModel = {
      id: modelId,
      projectId: this.config.projectId,
      ruleId,
      name,
      description: `Behavioral model trained on ${data.length} executions`,
      modelType,
      version: 1,
      inputSignature,
      outputSignature,
      statistics,
      invariants,
      constraints,
      edgeCases,
      quality,
      trainingInfo: {
        dataSource: 'historical_executions',
        startDate: new Date(Math.min(...data.map(d => d.timestamp.getTime()))),
        endDate: new Date(Math.max(...data.map(d => d.timestamp.getTime()))),
        sampleCount: data.length,
        parameters: {
          confidenceThreshold: this.config.confidenceThreshold,
          minSamples: this.config.minSamplesForModel,
        },
        trainingDuration: Date.now() - startTime,
      },
      createdAt: new Date(),
      updatedAt: new Date(),
    };

    this.models.set(modelId, model);
    return model;
  }

  /**
   * Update an existing model with new data
   */
  async updateModel(
    modelId: string,
    newData: HistoricalExecution[]
  ): Promise<BehavioralModel> {
    const existingModel = this.models.get(modelId);
    if (!existingModel) {
      throw new Error(`Model not found: ${modelId}`);
    }

    // Combine with existing training data (in production, would store data separately)
    const updatedModel = await this.trainModel(
      newData,
      existingModel.name,
      existingModel.ruleId
    );

    // Preserve model ID and increment version
    updatedModel.id = modelId;
    updatedModel.version = existingModel.version + 1;
    
    this.models.set(modelId, updatedModel);
    return updatedModel;
  }

  /**
   * Get a model by ID
   */
  getModel(modelId: string): BehavioralModel | null {
    return this.models.get(modelId) || null;
  }

  // ============================================================================
  // FIELD SIGNATURE LEARNING
  // ============================================================================

  private learnFieldSignatures(
    data: HistoricalExecution[],
    fieldType: 'inputs' | 'outputs'
  ): FieldSignature[] {
    const signatures: FieldSignature[] = [];
    const fieldValues = new Map<string, unknown[]>();

    // Collect all values for each field
    for (const execution of data) {
      const fields = execution[fieldType];
      for (const [name, value] of Object.entries(fields)) {
        const values = fieldValues.get(name) || [];
        values.push(value);
        fieldValues.set(name, values);
      }
    }

    // Analyze each field
    for (const [name, values] of fieldValues) {
      const nonNullValues = values.filter(v => v !== null && v !== undefined);
      const dataType = this.inferDataType(nonNullValues);
      const distribution = this.computeDistribution(nonNullValues, dataType);
      
      signatures.push({
        name,
        dataType,
        nullable: nonNullValues.length < values.length,
        observedDistribution: distribution,
        observedRange: this.computeRange(nonNullValues, dataType),
        observedValues: dataType === 'string' ? this.getUniqueValues(nonNullValues, 100) : undefined,
        cardinality: new Set(values.map(v => JSON.stringify(v))).size,
        nullRate: (values.length - nonNullValues.length) / values.length,
      });
    }

    return signatures;
  }

  private inferDataType(values: unknown[]): FieldSignature['dataType'] {
    if (values.length === 0) return 'string';
    
    const sample = values[0];
    if (typeof sample === 'number') return 'number';
    if (typeof sample === 'boolean') return 'boolean';
    if (sample instanceof Date) return 'date';
    if (Array.isArray(sample)) return 'array';
    if (typeof sample === 'object') return 'object';
    
    // Check if string looks like a date
    if (typeof sample === 'string' && !isNaN(Date.parse(sample))) {
      const allDates = values.every(v => typeof v === 'string' && !isNaN(Date.parse(v as string)));
      if (allDates) return 'date';
    }
    
    return 'string';
  }

  private computeDistribution(values: unknown[], dataType: string): Distribution {
    if (values.length === 0) {
      return { type: 'unknown' };
    }

    if (dataType === 'number') {
      return this.computeNumericDistribution(values as number[]);
    }

    if (dataType === 'string') {
      return this.computeCategoricalDistribution(values as string[]);
    }

    return { type: 'unknown' };
  }

  private computeNumericDistribution(values: number[]): Distribution {
    const n = values.length;
    const mean = values.reduce((a, b) => a + b, 0) / n;
    const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / n;
    const stdDev = Math.sqrt(variance);

    // Check for normal distribution using skewness
    const skewness = values.reduce((sum, v) => 
      sum + Math.pow((v - mean) / stdDev, 3), 0
    ) / n;

    const kurtosis = values.reduce((sum, v) => 
      sum + Math.pow((v - mean) / stdDev, 4), 0
    ) / n - 3;

    let type: Distribution['type'] = 'unknown';
    if (Math.abs(skewness) < 0.5 && Math.abs(kurtosis) < 1) {
      type = 'normal';
    } else if (Math.abs(skewness) < 0.1) {
      type = 'uniform';
    } else {
      type = 'skewed';
    }

    // Create histogram
    const bucketCount = Math.min(20, Math.ceil(Math.sqrt(n)));
    const min = Math.min(...values);
    const max = Math.max(...values);
    const bucketSize = (max - min) / bucketCount || 1;
    
    const histogram: { bucket: string; count: number }[] = [];
    for (let i = 0; i < bucketCount; i++) {
      const bucketMin = min + i * bucketSize;
      const bucketMax = min + (i + 1) * bucketSize;
      const count = values.filter(v => v >= bucketMin && v < bucketMax).length;
      histogram.push({
        bucket: `${bucketMin.toFixed(2)}-${bucketMax.toFixed(2)}`,
        count,
      });
    }

    return {
      type,
      parameters: { mean, stdDev, skewness, kurtosis },
      histogram,
    };
  }

  private computeCategoricalDistribution(values: string[]): Distribution {
    const counts = new Map<string, number>();
    for (const v of values) {
      counts.set(v, (counts.get(v) || 0) + 1);
    }

    const histogram = Array.from(counts.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 20)
      .map(([bucket, count]) => ({ bucket, count }));

    const uniqueCount = counts.size;
    const type: Distribution['type'] = uniqueCount <= 10 ? 'categorical' : 'unknown';

    return {
      type,
      parameters: { mode: histogram[0]?.bucket },
      histogram,
    };
  }

  private computeRange(values: unknown[], dataType: string): { min: unknown; max: unknown } | undefined {
    if (dataType !== 'number' || values.length === 0) return undefined;
    
    const numbers = values as number[];
    return {
      min: Math.min(...numbers),
      max: Math.max(...numbers),
    };
  }

  private getUniqueValues(values: unknown[], limit: number): unknown[] {
    const unique = [...new Set(values.map(v => JSON.stringify(v)))];
    return unique.slice(0, limit).map(v => JSON.parse(v));
  }

  // ============================================================================
  // STATISTICS COMPUTATION
  // ============================================================================

  private computeStatistics(
    data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): ModelStatistics {
    // Input distributions
    const inputDistributions: Record<string, Distribution> = {};
    for (const sig of inputSignature) {
      inputDistributions[sig.name] = sig.observedDistribution || { type: 'unknown' };
    }

    // Output distributions
    const outputDistributions: Record<string, Distribution> = {};
    for (const sig of outputSignature) {
      outputDistributions[sig.name] = sig.observedDistribution || { type: 'unknown' };
    }

    // Input correlations
    const inputCorrelations = this.computeCorrelationMatrix(
      data.map(d => d.inputs),
      inputSignature.filter(s => s.dataType === 'number').map(s => s.name)
    );

    // Input-output correlations
    const inputOutputCorrelations: Record<string, number> = {};
    const numericInputs = inputSignature.filter(s => s.dataType === 'number');
    const numericOutputs = outputSignature.filter(s => s.dataType === 'number');

    for (const input of numericInputs) {
      for (const output of numericOutputs) {
        const inputValues = data.map(d => d.inputs[input.name] as number);
        const outputValues = data.map(d => d.outputs[output.name] as number);
        const correlation = this.pearsonCorrelation(inputValues, outputValues);
        if (!isNaN(correlation)) {
          inputOutputCorrelations[`${input.name}->${output.name}`] = correlation;
        }
      }
    }

    // Unique patterns
    const uniqueInputPatterns = new Set(data.map(d => JSON.stringify(d.inputs))).size;
    const uniqueOutputPatterns = new Set(data.map(d => JSON.stringify(d.outputs))).size;

    return {
      sampleCount: data.length,
      uniqueInputPatterns,
      uniqueOutputPatterns,
      inputCorrelations,
      inputDistributions,
      outputDistributions,
      inputOutputCorrelations,
    };
  }

  private computeCorrelationMatrix(
    records: Record<string, unknown>[],
    fields: string[]
  ): CorrelationMatrix {
    const n = fields.length;
    const values: number[][] = Array(n).fill(null).map(() => Array(n).fill(0));

    for (let i = 0; i < n; i++) {
      for (let j = i; j < n; j++) {
        if (i === j) {
          values[i]![j] = 1;
        } else {
          const x = records.map(r => r[fields[i]!] as number);
          const y = records.map(r => r[fields[j]!] as number);
          const corr = this.pearsonCorrelation(x, y);
          values[i]![j] = corr;
          values[j]![i] = corr;
        }
      }
    }

    return { fields, values };
  }

  private pearsonCorrelation(x: number[], y: number[]): number {
    const n = x.length;
    if (n === 0) return NaN;

    const sumX = x.reduce((a, b) => a + b, 0);
    const sumY = y.reduce((a, b) => a + b, 0);
    const sumXY = x.reduce((acc, xi, i) => acc + xi * (y[i] || 0), 0);
    const sumX2 = x.reduce((acc, xi) => acc + xi * xi, 0);
    const sumY2 = y.reduce((acc, yi) => acc + yi * yi, 0);

    const numerator = n * sumXY - sumX * sumY;
    const denominator = Math.sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY));

    if (denominator === 0) return NaN;
    return numerator / denominator;
  }

  // ============================================================================
  // INVARIANT DISCOVERY
  // ============================================================================

  private discoverInvariants(
    data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): LearnedInvariant[] {
    const invariants: LearnedInvariant[] = [];

    // Equality invariants (output.X == input.Y)
    invariants.push(...this.discoverEqualityInvariants(data, inputSignature, outputSignature));

    // Formula invariants (output.X = f(inputs))
    invariants.push(...this.discoverFormulaInvariants(data, inputSignature, outputSignature));

    // Range invariants (output.X in [a, b])
    invariants.push(...this.discoverRangeInvariants(data, outputSignature));

    // Conditional invariants
    invariants.push(...this.discoverConditionalInvariants(data, inputSignature, outputSignature));

    // Sort by confidence and limit
    return invariants
      .filter(inv => inv.confidence >= this.config.confidenceThreshold)
      .sort((a, b) => b.confidence - a.confidence)
      .slice(0, this.config.maxInvariantsPerModel);
  }

  private discoverEqualityInvariants(
    data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): LearnedInvariant[] {
    const invariants: LearnedInvariant[] = [];

    for (const output of outputSignature) {
      for (const input of inputSignature) {
        if (output.dataType !== input.dataType) continue;

        let matches = 0;
        for (const exec of data) {
          if (exec.outputs[output.name] === exec.inputs[input.name]) {
            matches++;
          }
        }

        const holdRate = matches / data.length;
        if (holdRate >= this.config.confidenceThreshold) {
          invariants.push({
            id: `inv_${randomUUID()}`,
            name: `${output.name} equals ${input.name}`,
            type: 'equality',
            description: `Output ${output.name} always equals input ${input.name}`,
            expression: `output.${output.name} == input.${input.name}`,
            inputFields: [input.name],
            outputFields: [output.name],
            holdRate,
            violationCount: data.length - matches,
            totalObservations: data.length,
            knownExceptions: [],
            confidence: holdRate,
            discoveredAt: new Date(),
          });
        }
      }
    }

    return invariants;
  }

  private discoverFormulaInvariants(
    data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): LearnedInvariant[] {
    const invariants: LearnedInvariant[] = [];
    const numericInputs = inputSignature.filter(s => s.dataType === 'number');
    const numericOutputs = outputSignature.filter(s => s.dataType === 'number');

    for (const output of numericOutputs) {
      // Try to find linear relationships
      for (const input of numericInputs) {
        const formula = this.tryLinearFit(data, input.name, output.name);
        if (formula) {
          invariants.push({
            id: `inv_${randomUUID()}`,
            name: `${output.name} formula`,
            type: 'formula',
            description: `${output.name} = ${formula.expression}`,
            expression: formula.expression,
            inputFields: [input.name],
            outputFields: [output.name],
            holdRate: formula.accuracy,
            violationCount: Math.floor(data.length * (1 - formula.accuracy)),
            totalObservations: data.length,
            knownExceptions: [],
            confidence: formula.accuracy,
            discoveredAt: new Date(),
          });
        }
      }

      // Try sum of two inputs
      if (numericInputs.length >= 2) {
        for (let i = 0; i < numericInputs.length; i++) {
          for (let j = i + 1; j < numericInputs.length; j++) {
            const input1 = numericInputs[i]!;
            const input2 = numericInputs[j]!;
            
            let matches = 0;
            for (const exec of data) {
              const sum = (exec.inputs[input1.name] as number) + (exec.inputs[input2.name] as number);
              const outputVal = exec.outputs[output.name] as number;
              if (Math.abs(sum - outputVal) < 0.001) {
                matches++;
              }
            }

            const holdRate = matches / data.length;
            if (holdRate >= this.config.confidenceThreshold) {
              invariants.push({
                id: `inv_${randomUUID()}`,
                name: `${output.name} = ${input1.name} + ${input2.name}`,
                type: 'formula',
                description: `${output.name} equals sum of ${input1.name} and ${input2.name}`,
                expression: `output.${output.name} == input.${input1.name} + input.${input2.name}`,
                inputFields: [input1.name, input2.name],
                outputFields: [output.name],
                holdRate,
                violationCount: data.length - matches,
                totalObservations: data.length,
                knownExceptions: [],
                confidence: holdRate,
                discoveredAt: new Date(),
              });
            }
          }
        }
      }
    }

    return invariants;
  }

  private tryLinearFit(
    data: HistoricalExecution[],
    inputField: string,
    outputField: string
  ): { expression: string; accuracy: number } | null {
    const x = data.map(d => d.inputs[inputField] as number);
    const y = data.map(d => d.outputs[outputField] as number);
    
    const n = x.length;
    const sumX = x.reduce((a, b) => a + b, 0);
    const sumY = y.reduce((a, b) => a + b, 0);
    const sumXY = x.reduce((acc, xi, i) => acc + xi * (y[i] || 0), 0);
    const sumX2 = x.reduce((acc, xi) => acc + xi * xi, 0);

    const denominator = n * sumX2 - sumX * sumX;
    if (Math.abs(denominator) < 0.0001) return null;

    const slope = (n * sumXY - sumX * sumY) / denominator;
    const intercept = (sumY - slope * sumX) / n;

    // Check accuracy
    let totalError = 0;
    for (let i = 0; i < n; i++) {
      const predicted = slope * x[i]! + intercept;
      const actual = y[i]!;
      totalError += Math.abs(predicted - actual);
    }
    
    const avgError = totalError / n;
    const maxY = Math.max(...y);
    const accuracy = maxY > 0 ? 1 - avgError / maxY : 0;

    if (accuracy < this.config.confidenceThreshold) return null;

    // Format expression
    let expression = `input.${inputField}`;
    if (Math.abs(slope - 1) > 0.001) {
      expression = `${slope.toFixed(4)} * input.${inputField}`;
    }
    if (Math.abs(intercept) > 0.001) {
      expression += intercept > 0 ? ` + ${intercept.toFixed(4)}` : ` - ${Math.abs(intercept).toFixed(4)}`;
    }

    return { expression, accuracy };
  }

  private discoverRangeInvariants(
    data: HistoricalExecution[],
    outputSignature: FieldSignature[]
  ): LearnedInvariant[] {
    const invariants: LearnedInvariant[] = [];

    for (const output of outputSignature) {
      if (output.dataType !== 'number') continue;
      if (!output.observedRange) continue;

      const { min, max } = output.observedRange as { min: number; max: number };
      
      invariants.push({
        id: `inv_${randomUUID()}`,
        name: `${output.name} range`,
        type: 'range',
        description: `${output.name} is always in range [${min}, ${max}]`,
        expression: `${min} <= output.${output.name} <= ${max}`,
        inputFields: [],
        outputFields: [output.name],
        holdRate: 1.0,
        violationCount: 0,
        totalObservations: data.length,
        knownExceptions: [],
        confidence: 0.95, // Slightly lower since range might expand
        discoveredAt: new Date(),
      });
    }

    return invariants;
  }

  private discoverConditionalInvariants(
    data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): LearnedInvariant[] {
    const invariants: LearnedInvariant[] = [];

    // Look for categorical inputs that determine output behavior
    const categoricalInputs = inputSignature.filter(s => 
      s.dataType === 'string' && s.cardinality && s.cardinality <= 10
    );

    for (const input of categoricalInputs) {
      const valueGroups = new Map<string, HistoricalExecution[]>();
      
      for (const exec of data) {
        const value = String(exec.inputs[input.name]);
        const group = valueGroups.get(value) || [];
        group.push(exec);
        valueGroups.set(value, group);
      }

      // Check if each group has consistent output
      for (const output of outputSignature) {
        for (const [inputValue, group] of valueGroups) {
          const outputValues = group.map(e => e.outputs[output.name]);
          const uniqueOutputs = new Set(outputValues.map(v => JSON.stringify(v)));
          
          if (uniqueOutputs.size === 1 && group.length >= 5) {
            const constantOutput = outputValues[0];
            invariants.push({
              id: `inv_${randomUUID()}`,
              name: `${output.name} when ${input.name}=${inputValue}`,
              type: 'conditional',
              description: `When ${input.name}="${inputValue}", ${output.name} always equals ${JSON.stringify(constantOutput)}`,
              expression: `if (input.${input.name} == "${inputValue}") then output.${output.name} == ${JSON.stringify(constantOutput)}`,
              inputFields: [input.name],
              outputFields: [output.name],
              holdRate: 1.0,
              violationCount: 0,
              totalObservations: group.length,
              knownExceptions: [],
              confidence: Math.min(group.length / data.length * 2, 0.95),
              discoveredAt: new Date(),
            });
          }
        }
      }
    }

    return invariants;
  }

  // ============================================================================
  // CONSTRAINT LEARNING
  // ============================================================================

  private learnConstraints(
    _data: HistoricalExecution[],
    inputSignature: FieldSignature[],
    outputSignature: FieldSignature[]
  ): LearnedConstraint[] {
    const constraints: LearnedConstraint[] = [];

    // Required field constraints
    for (const sig of [...inputSignature, ...outputSignature]) {
      if (!sig.nullable) {
        constraints.push({
          id: `const_${randomUUID()}`,
          name: `${sig.name} required`,
          type: 'required',
          description: `${sig.name} is always present (never null)`,
          expression: `${sig.name} != null`,
          fields: [sig.name],
          validationRate: 1 - (sig.nullRate || 0),
          severity: 'error',
          confidence: 1 - (sig.nullRate || 0),
        });
      }
    }

    // Type constraints
    for (const sig of outputSignature) {
      constraints.push({
        id: `const_${randomUUID()}`,
        name: `${sig.name} type`,
        type: 'type',
        description: `${sig.name} is always of type ${sig.dataType}`,
        expression: `typeof ${sig.name} == "${sig.dataType}"`,
        fields: [sig.name],
        validationRate: 1.0,
        severity: 'error',
        confidence: 0.99,
      });
    }

    // Range constraints for numeric fields
    for (const sig of outputSignature) {
      if (sig.dataType === 'number' && sig.observedRange) {
        const range = sig.observedRange as { min: number; max: number };
        constraints.push({
          id: `const_${randomUUID()}`,
          name: `${sig.name} range constraint`,
          type: 'range',
          description: `${sig.name} must be between ${range.min} and ${range.max}`,
          expression: `${range.min} <= ${sig.name} <= ${range.max}`,
          fields: [sig.name],
          validationRate: 1.0,
          severity: 'warning',
          confidence: 0.9,
        });
      }
    }

    return constraints;
  }

  // ============================================================================
  // EDGE CASE IDENTIFICATION
  // ============================================================================

  private identifyEdgeCases(
    data: HistoricalExecution[],
    statistics: ModelStatistics,
    invariants: LearnedInvariant[]
  ): DiscoveredEdgeCase[] {
    const edgeCases: DiscoveredEdgeCase[] = [];

    // Boundary value edge cases
    edgeCases.push(...this.findBoundaryEdgeCases(data, statistics));

    // Rare value edge cases
    edgeCases.push(...this.findRareValueEdgeCases(data, statistics));

    // Invariant exception edge cases
    edgeCases.push(...this.findInvariantExceptions(data, invariants));

    return edgeCases;
  }

  private findBoundaryEdgeCases(
    data: HistoricalExecution[],
    statistics: ModelStatistics
  ): DiscoveredEdgeCase[] {
    const edgeCases: DiscoveredEdgeCase[] = [];

    for (const [field, dist] of Object.entries(statistics.inputDistributions)) {
      if (dist.type !== 'normal' && dist.type !== 'uniform') continue;
      if (!dist.parameters?.mean || !dist.parameters?.stdDev) continue;

      const { mean, stdDev } = dist.parameters;
      
      // Find executions at boundaries (> 2 std devs from mean)
      const boundaryExecutions = data.filter(e => {
        const value = e.inputs[field] as number;
        return Math.abs(value - mean) > 2 * stdDev;
      });

      if (boundaryExecutions.length > 0 && boundaryExecutions.length < data.length * 0.05) {
        edgeCases.push({
          id: `edge_${randomUUID()}`,
          name: `${field} boundary values`,
          description: `Executions with ${field} values far from the mean (±2σ)`,
          trigger: {
            type: 'boundary',
            condition: `|${field} - ${mean.toFixed(2)}| > ${(2 * stdDev).toFixed(2)}`,
            fields: [field],
          },
          expectedBehavior: 'May exhibit different behavior at boundaries',
          frequency: boundaryExecutions.length,
          exampleInputs: boundaryExecutions.slice(0, 3).map(e => e.inputs),
          riskLevel: 'medium',
        });
      }
    }

    return edgeCases;
  }

  private findRareValueEdgeCases(
    data: HistoricalExecution[],
    statistics: ModelStatistics
  ): DiscoveredEdgeCase[] {
    const edgeCases: DiscoveredEdgeCase[] = [];
    const rareThreshold = 0.01; // Less than 1% of data

    for (const [field, dist] of Object.entries(statistics.inputDistributions)) {
      if (!dist.histogram) continue;

      const totalCount = dist.histogram.reduce((sum, h) => sum + h.count, 0);
      const rareBuckets = dist.histogram.filter(h => h.count / totalCount < rareThreshold);

      for (const bucket of rareBuckets) {
        const executions = data.filter(e => {
          const value = String(e.inputs[field]);
          return bucket.bucket.includes(value) || value.includes(bucket.bucket);
        });

        if (executions.length > 0) {
          edgeCases.push({
            id: `edge_${randomUUID()}`,
            name: `Rare ${field} value: ${bucket.bucket}`,
            description: `Very infrequent value for ${field}`,
            trigger: {
              type: 'rare',
              condition: `${field} == "${bucket.bucket}"`,
              fields: [field],
            },
            expectedBehavior: 'May have untested behavior paths',
            frequency: executions.length,
            exampleInputs: executions.slice(0, 3).map(e => e.inputs),
            riskLevel: 'low',
          });
        }
      }
    }

    return edgeCases;
  }

  private findInvariantExceptions(
    data: HistoricalExecution[],
    invariants: LearnedInvariant[]
  ): DiscoveredEdgeCase[] {
    const edgeCases: DiscoveredEdgeCase[] = [];

    for (const invariant of invariants) {
      if (invariant.holdRate >= 0.99) continue; // Skip very stable invariants

      const violations = data.filter(e => !this.checkInvariant(e, invariant));
      
      if (violations.length > 0 && violations.length < data.length * 0.1) {
        edgeCases.push({
          id: `edge_${randomUUID()}`,
          name: `Exception to: ${invariant.name}`,
          description: `Cases where invariant "${invariant.expression}" doesn't hold`,
          trigger: {
            type: 'combination',
            condition: `NOT (${invariant.expression})`,
            fields: [...invariant.inputFields, ...invariant.outputFields],
          },
          expectedBehavior: `Invariant violation: ${invariant.description}`,
          frequency: violations.length,
          exampleInputs: violations.slice(0, 3).map(e => e.inputs),
          riskLevel: invariant.confidence > 0.9 ? 'high' : 'medium',
        });
      }
    }

    return edgeCases;
  }

  private checkInvariant(execution: HistoricalExecution, invariant: LearnedInvariant): boolean {
    // Simplified invariant checking - in production would use expression parser
    if (invariant.type === 'equality') {
      const outputVal = execution.outputs[invariant.outputFields[0]!];
      const inputVal = execution.inputs[invariant.inputFields[0]!];
      return outputVal === inputVal;
    }
    return true;
  }

  // ============================================================================
  // MODEL QUALITY ASSESSMENT
  // ============================================================================

  private assessModelQuality(
    data: HistoricalExecution[],
    invariants: LearnedInvariant[],
    constraints: LearnedConstraint[],
    edgeCases: DiscoveredEdgeCase[]
  ): ModelQuality {
    // Calculate accuracy based on invariant hold rates
    const avgInvariantHoldRate = invariants.length > 0
      ? invariants.reduce((sum, inv) => sum + inv.holdRate, 0) / invariants.length
      : 0;

    // Input space coverage estimate
    const uniqueInputs = new Set(data.map(d => JSON.stringify(d.inputs))).size;
    const inputSpaceCoverage = Math.min(uniqueInputs / (data.length * 0.5), 1.0);

    // Edge case coverage
    const edgeCaseCoverage = edgeCases.length > 0
      ? edgeCases.filter(e => e.frequency >= 3).length / edgeCases.length
      : 1.0;

    return {
      accuracy: avgInvariantHoldRate,
      precision: avgInvariantHoldRate * 0.95, // Estimate
      recall: avgInvariantHoldRate * 0.9,     // Estimate
      f1Score: avgInvariantHoldRate * 0.92,   // Estimate
      inputSpaceCoverage,
      outputSpaceCoverage: inputSpaceCoverage * 0.95,
      edgeCaseCoverage,
      consistencyScore: constraints.length > 0
        ? constraints.reduce((sum, c) => sum + c.validationRate, 0) / constraints.length
        : 1.0,
      confidenceCalibration: 0.85, // Would need validation data to compute
      dataFreshness: 0, // Days since last data
      driftDetected: false,
    };
  }

  // ============================================================================
  // MODEL TYPE DETERMINATION
  // ============================================================================

  private determineModelType(
    _statistics: ModelStatistics,
    invariants: LearnedInvariant[]
  ): BehavioralModelType {
    // Check for strong formula-based relationships
    const formulaInvariants = invariants.filter(inv => inv.type === 'formula');
    if (formulaInvariants.length > 0 && formulaInvariants[0]!.confidence > 0.95) {
      return 'regression';
    }

    // Check for categorical patterns
    const conditionalInvariants = invariants.filter(inv => inv.type === 'conditional');
    if (conditionalInvariants.length > invariants.length / 2) {
      return 'decision_tree';
    }

    // Check for simple patterns
    const equalityInvariants = invariants.filter(inv => inv.type === 'equality');
    if (equalityInvariants.length > 0) {
      return 'pattern';
    }

    // Default to statistical
    return 'statistical';
  }
}

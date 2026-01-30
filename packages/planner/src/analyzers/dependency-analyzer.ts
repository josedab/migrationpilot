/**
 * Dependency Analyzer
 * Analyzes module dependencies and coupling metrics for migration planning
 */

import type { KnowledgeNode, KnowledgeEdge } from '@migrationpilot/core';
import type { DependencyAnalysis, IDependencyAnalyzer } from '../types';

interface GraphContext {
  nodes: Map<string, KnowledgeNode>;
  edges: Map<string, KnowledgeEdge>;
}

export class DependencyAnalyzer implements IDependencyAnalyzer {
  
  /**
   * Analyze dependencies for a specific module
   */
  analyze(moduleId: string, graph: GraphContext): DependencyAnalysis {
    const directDeps = this.findDirectDependencies(moduleId, graph);
    const transitiveDeps = this.findTransitiveDependencies(moduleId, directDeps, graph);
    const directDependents = this.findDirectDependents(moduleId, graph);
    const transitiveDependents = this.findTransitiveDependents(moduleId, directDependents, graph);
    
    const allModuleIds = Array.from(graph.nodes.keys())
      .filter(id => graph.nodes.get(id)?.type === 'program');
    
    const dependencyMap = new Map<string, string[]>();
    for (const id of allModuleIds) {
      dependencyMap.set(id, this.findDirectDependencies(id, graph));
    }
    
    const circularDeps = this.findCircularDependencies(allModuleIds, dependencyMap);
    const moduleCircular = circularDeps.filter(cycle => cycle.includes(moduleId));
    
    const afferent = directDependents.length;
    const efferent = directDeps.length;
    const instability = afferent + efferent > 0 ? efferent / (afferent + efferent) : 0;
    
    const analysis: DependencyAnalysis = {
      moduleId,
      directDependencies: directDeps,
      transitiveDependencies: transitiveDeps,
      directDependents,
      transitiveDependents,
      circularDependencies: moduleCircular,
      couplingScore: this.calculateCoupling(moduleId, {
        moduleId,
        directDependencies: directDeps,
        transitiveDependencies: transitiveDeps,
        directDependents,
        transitiveDependents,
        circularDependencies: moduleCircular,
        afferentCoupling: afferent,
        efferentCoupling: efferent,
        instability,
        couplingScore: 0,
      }),
      afferentCoupling: afferent,
      efferentCoupling: efferent,
      instability,
    };
    
    return analysis;
  }
  
  /**
   * Find direct dependencies of a module
   */
  private findDirectDependencies(moduleId: string, graph: GraphContext): string[] {
    const dependencies: string[] = [];
    
    for (const edge of graph.edges.values()) {
      if (edge.source === moduleId && 
          ['calls', 'uses', 'depends-on', 'reads', 'imports'].includes(edge.type)) {
        const targetNode = graph.nodes.get(edge.target);
        if (targetNode && ['program', 'procedure', 'library'].includes(targetNode.type)) {
          dependencies.push(edge.target);
        }
      }
    }
    
    return [...new Set(dependencies)];
  }
  
  /**
   * Find transitive dependencies (all dependencies recursively)
   */
  private findTransitiveDependencies(
    moduleId: string, 
    directDeps: string[], 
    graph: GraphContext
  ): string[] {
    const visited = new Set<string>([moduleId]);
    const transitive = new Set<string>();
    const queue = [...directDeps];
    
    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current)) continue;
      
      visited.add(current);
      transitive.add(current);
      
      const deps = this.findDirectDependencies(current, graph);
      for (const dep of deps) {
        if (!visited.has(dep)) {
          queue.push(dep);
        }
      }
    }
    
    return Array.from(transitive);
  }
  
  /**
   * Find modules that directly depend on this module
   */
  private findDirectDependents(moduleId: string, graph: GraphContext): string[] {
    const dependents: string[] = [];
    
    for (const edge of graph.edges.values()) {
      if (edge.target === moduleId && 
          ['calls', 'uses', 'depends-on', 'reads', 'imports'].includes(edge.type)) {
        const sourceNode = graph.nodes.get(edge.source);
        if (sourceNode && ['program', 'procedure', 'library'].includes(sourceNode.type)) {
          dependents.push(edge.source);
        }
      }
    }
    
    return [...new Set(dependents)];
  }
  
  /**
   * Find transitive dependents (all modules that depend on this one recursively)
   */
  private findTransitiveDependents(
    moduleId: string, 
    directDependents: string[], 
    graph: GraphContext
  ): string[] {
    const visited = new Set<string>([moduleId]);
    const transitive = new Set<string>();
    const queue = [...directDependents];
    
    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current)) continue;
      
      visited.add(current);
      transitive.add(current);
      
      const dependents = this.findDirectDependents(current, graph);
      for (const dep of dependents) {
        if (!visited.has(dep)) {
          queue.push(dep);
        }
      }
    }
    
    return Array.from(transitive);
  }
  
  /**
   * Find all circular dependencies in the module graph
   */
  findCircularDependencies(modules: string[], dependencies: Map<string, string[]>): string[][] {
    const cycles: string[][] = [];
    const visited = new Set<string>();
    const recursionStack = new Set<string>();
    const path: string[] = [];
    
    const dfs = (node: string): void => {
      visited.add(node);
      recursionStack.add(node);
      path.push(node);
      
      const deps = dependencies.get(node) || [];
      for (const dep of deps) {
        if (!visited.has(dep)) {
          dfs(dep);
        } else if (recursionStack.has(dep)) {
          // Found a cycle
          const cycleStart = path.indexOf(dep);
          if (cycleStart !== -1) {
            const cycle = path.slice(cycleStart);
            // Normalize cycle to avoid duplicates
            const minIdx = cycle.indexOf(cycle.reduce((a, b) => a < b ? a : b));
            const normalizedCycle = [...cycle.slice(minIdx), ...cycle.slice(0, minIdx)];
            
            // Check if this cycle is already recorded
            const cycleKey = normalizedCycle.join('->');
            const existingKeys = cycles.map(c => {
              const minI = c.indexOf(c.reduce((a, b) => a < b ? a : b));
              return [...c.slice(minI), ...c.slice(0, minI)].join('->');
            });
            
            if (!existingKeys.includes(cycleKey)) {
              cycles.push(normalizedCycle);
            }
          }
        }
      }
      
      path.pop();
      recursionStack.delete(node);
    };
    
    for (const module of modules) {
      if (!visited.has(module)) {
        dfs(module);
      }
    }
    
    return cycles;
  }
  
  /**
   * Calculate coupling score for a module
   */
  calculateCoupling(_moduleId: string, deps: DependencyAnalysis): number {
    // Coupling score formula:
    // Base: (afferent + efferent) / 2
    // Penalty for circular dependencies
    // Penalty for high instability
    
    const baseCoupling = (deps.afferentCoupling + deps.efferentCoupling) / 2;
    const circularPenalty = deps.circularDependencies.length * 2;
    const instabilityFactor = deps.instability > 0.7 || deps.instability < 0.3 ? 1.5 : 1.0;
    
    return Math.min(10, (baseCoupling + circularPenalty) * instabilityFactor);
  }
  
  /**
   * Analyze all modules and return a map of dependency analyses
   */
  analyzeAll(graph: GraphContext): Map<string, DependencyAnalysis> {
    const analyses = new Map<string, DependencyAnalysis>();
    
    for (const [nodeId, node] of graph.nodes) {
      if (node.type === 'program') {
        analyses.set(nodeId, this.analyze(nodeId, graph));
      }
    }
    
    return analyses;
  }
  
  /**
   * Get modules sorted by their coupling score (most coupled first)
   */
  getMostCoupledModules(analyses: Map<string, DependencyAnalysis>, limit: number = 10): string[] {
    return Array.from(analyses.entries())
      .sort((a, b) => b[1].couplingScore - a[1].couplingScore)
      .slice(0, limit)
      .map(([id]) => id);
  }
  
  /**
   * Get leaf modules (no dependents, good candidates for early migration)
   */
  getLeafModules(analyses: Map<string, DependencyAnalysis>): string[] {
    return Array.from(analyses.entries())
      .filter(([_, analysis]) => analysis.directDependents.length === 0)
      .map(([id]) => id);
  }
  
  /**
   * Get root modules (no dependencies, foundation modules)
   */
  getRootModules(analyses: Map<string, DependencyAnalysis>): string[] {
    return Array.from(analyses.entries())
      .filter(([_, analysis]) => analysis.directDependencies.length === 0)
      .map(([id]) => id);
  }
}

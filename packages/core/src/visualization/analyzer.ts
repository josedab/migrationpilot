/**
 * Graph Analyzer
 * 
 * Advanced analysis of knowledge graphs for structural metrics,
 * cluster detection, critical path analysis, and risk identification.
 */

import type { 
  KnowledgeGraph,
  KnowledgeNode,
  KnowledgeEdge,
  NodeType 
} from '../knowledge-graph.js';
import type {
  GraphAnalysis,
  StructuralMetrics,
  ClusterAnalysis,
  CriticalPathAnalysis,
  RiskAnalysis,
  AnalysisRecommendation
} from './types.js';

/**
 * Analyzes knowledge graphs to provide structural insights,
 * identify clusters, find critical paths, and assess risks.
 */
export class GraphAnalyzer {
  
  /**
   * Perform comprehensive analysis of a knowledge graph
   */
  analyze(graph: KnowledgeGraph): GraphAnalysis {
    const nodes = Array.from(graph.nodes.values());
    const edges = Array.from(graph.edges.values());
    
    // Build adjacency structures
    const adjacency = this.buildAdjacency(nodes, edges);
    
    return {
      graphId: graph.id,
      analyzedAt: new Date(),
      structural: this.analyzeStructure(nodes, edges, adjacency),
      clusters: this.analyzeClusters(nodes, edges, adjacency),
      criticalPaths: this.analyzeCriticalPaths(nodes, edges, adjacency),
      risks: this.analyzeRisks(nodes, edges, adjacency),
      recommendations: this.generateRecommendations(nodes, edges, adjacency),
    };
  }
  
  // ==========================================================================
  // STRUCTURAL ANALYSIS
  // ==========================================================================
  
  private analyzeStructure(
    nodes: KnowledgeNode[],
    edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): StructuralMetrics {
    const n = nodes.length;
    const m = edges.length;
    
    // Density: actual edges / possible edges
    const maxEdges = n * (n - 1);
    const density = maxEdges > 0 ? m / maxEdges : 0;
    
    // Degree analysis
    const degrees = Array.from(adjacency.degrees.values());
    const avgDegree = degrees.length > 0 
      ? degrees.reduce((a, b) => a + b, 0) / degrees.length 
      : 0;
    const maxDegree = Math.max(0, ...degrees);
    
    // Connected components
    const components = this.findConnectedComponents(nodes, adjacency);
    
    // Diameter and average path length (for largest component)
    const { diameter, avgPathLength } = this.calculatePathMetrics(nodes, adjacency);
    
    // Clustering coefficient
    const clusteringCoeff = this.calculateClusteringCoefficient(nodes, adjacency);
    
    return {
      nodeCount: n,
      edgeCount: m,
      density,
      averageDegree: avgDegree,
      maxDegree,
      diameter,
      averagePathLength: avgPathLength,
      clusteringCoefficient: clusteringCoeff,
      connectedComponents: components.length,
    };
  }
  
  private findConnectedComponents(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): Set<string>[] {
    const visited = new Set<string>();
    const components: Set<string>[] = [];
    
    for (const node of nodes) {
      if (visited.has(node.id)) continue;
      
      const component = new Set<string>();
      const queue = [node.id];
      
      while (queue.length > 0) {
        const current = queue.shift()!;
        if (visited.has(current)) continue;
        
        visited.add(current);
        component.add(current);
        
        const neighbors = adjacency.undirected.get(current) || new Set();
        for (const neighbor of neighbors) {
          if (!visited.has(neighbor)) {
            queue.push(neighbor);
          }
        }
      }
      
      components.push(component);
    }
    
    return components;
  }
  
  private calculatePathMetrics(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): { diameter: number; avgPathLength: number } {
    if (nodes.length === 0) {
      return { diameter: 0, avgPathLength: 0 };
    }
    
    // For large graphs, sample
    const sampleSize = Math.min(100, nodes.length);
    const sampledNodes = nodes.slice(0, sampleSize);
    
    let maxDistance = 0;
    let totalDistance = 0;
    let pathCount = 0;
    
    for (const source of sampledNodes) {
      const distances = this.bfs(source.id, adjacency);
      
      for (const [, distance] of distances) {
        if (distance !== Infinity && distance > 0) {
          maxDistance = Math.max(maxDistance, distance);
          totalDistance += distance;
          pathCount++;
        }
      }
    }
    
    return {
      diameter: maxDistance,
      avgPathLength: pathCount > 0 ? totalDistance / pathCount : 0,
    };
  }
  
  private bfs(startId: string, adjacency: AdjacencyData): Map<string, number> {
    const distances = new Map<string, number>();
    const queue: { id: string; distance: number }[] = [{ id: startId, distance: 0 }];
    
    while (queue.length > 0) {
      const { id, distance } = queue.shift()!;
      
      if (distances.has(id)) continue;
      distances.set(id, distance);
      
      const neighbors = adjacency.undirected.get(id) || new Set();
      for (const neighbor of neighbors) {
        if (!distances.has(neighbor)) {
          queue.push({ id: neighbor, distance: distance + 1 });
        }
      }
    }
    
    return distances;
  }
  
  private calculateClusteringCoefficient(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): number {
    if (nodes.length === 0) return 0;
    
    let totalCoeff = 0;
    let countable = 0;
    
    for (const node of nodes) {
      const neighbors = Array.from(adjacency.undirected.get(node.id) || new Set());
      const k = neighbors.length;
      
      if (k < 2) continue;
      
      // Count edges between neighbors
      let edgesBetweenNeighbors = 0;
      for (let i = 0; i < neighbors.length; i++) {
        for (let j = i + 1; j < neighbors.length; j++) {
          const neighbor1 = neighbors[i] as string;
          const neighbor2 = neighbors[j] as string;
          if (neighbor1 && neighbor2 && adjacency.undirected.get(neighbor1)?.has(neighbor2)) {
            edgesBetweenNeighbors++;
          }
        }
      }
      
      // Local clustering coefficient
      const maxPossible = (k * (k - 1)) / 2;
      totalCoeff += edgesBetweenNeighbors / maxPossible;
      countable++;
    }
    
    return countable > 0 ? totalCoeff / countable : 0;
  }
  
  // ==========================================================================
  // CLUSTER ANALYSIS
  // ==========================================================================
  
  private analyzeClusters(
    nodes: KnowledgeNode[],
    edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): ClusterAnalysis {
    // Use Louvain-inspired community detection
    const clusters = this.detectCommunities(nodes, edges, adjacency);
    const modularity = this.calculateModularity(clusters, edges, adjacency);
    
    // Analyze each cluster
    const clusterDetails = clusters.map((nodeIds, index) => {
      const clusterNodes = nodes.filter(n => nodeIds.includes(n.id));
      
      // Find dominant type
      const typeCounts = new Map<NodeType, number>();
      clusterNodes.forEach(n => {
        typeCounts.set(n.type, (typeCounts.get(n.type) || 0) + 1);
      });
      let dominantType: NodeType = 'program';
      let maxCount = 0;
      for (const [type, count] of typeCounts) {
        if (count > maxCount) {
          maxCount = count;
          dominantType = type;
        }
      }
      
      // Calculate cohesion and coupling
      const { cohesion, coupling } = this.calculateClusterMetrics(
        nodeIds, edges, adjacency
      );
      
      return {
        id: `cluster-${index}`,
        nodeIds,
        size: nodeIds.length,
        dominantType,
        cohesion,
        couplingToOthers: coupling,
      };
    });
    
    return {
      algorithm: 'louvain-inspired',
      clusterCount: clusters.length,
      clusters: clusterDetails,
      modularity,
    };
  }
  
  private detectCommunities(
    nodes: KnowledgeNode[],
    _edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): string[][] {
    // Simple label propagation algorithm
    const labels = new Map<string, string>();
    
    // Initialize: each node in its own community
    nodes.forEach(n => labels.set(n.id, n.id));
    
    // Iterate until convergence
    const maxIterations = 10;
    for (let iter = 0; iter < maxIterations; iter++) {
      let changed = false;
      
      // Shuffle nodes
      const shuffled = [...nodes].sort(() => Math.random() - 0.5);
      
      for (const node of shuffled) {
        const neighbors = adjacency.undirected.get(node.id) || new Set();
        if (neighbors.size === 0) continue;
        
        // Find most common label among neighbors
        const labelCounts = new Map<string, number>();
        for (const neighbor of neighbors) {
          const label = labels.get(neighbor)!;
          labelCounts.set(label, (labelCounts.get(label) || 0) + 1);
        }
        
        let maxLabel = labels.get(node.id)!;
        let maxCount = 0;
        for (const [label, count] of labelCounts) {
          if (count > maxCount || (count === maxCount && label < maxLabel)) {
            maxCount = count;
            maxLabel = label;
          }
        }
        
        if (maxLabel !== labels.get(node.id)) {
          labels.set(node.id, maxLabel);
          changed = true;
        }
      }
      
      if (!changed) break;
    }
    
    // Group by label
    const communities = new Map<string, string[]>();
    for (const [nodeId, label] of labels) {
      if (!communities.has(label)) communities.set(label, []);
      communities.get(label)!.push(nodeId);
    }
    
    return Array.from(communities.values());
  }
  
  private calculateModularity(
    clusters: string[][],
    edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): number {
    const m = edges.length;
    if (m === 0) return 0;
    
    // Build cluster membership map
    const membership = new Map<string, number>();
    clusters.forEach((cluster, index) => {
      cluster.forEach(nodeId => membership.set(nodeId, index));
    });
    
    // Calculate modularity
    let q = 0;
    for (const edge of edges) {
      const ci = membership.get(edge.source);
      const cj = membership.get(edge.target);
      if (ci === undefined || cj === undefined) continue;
      
      const ki = adjacency.degrees.get(edge.source) || 0;
      const kj = adjacency.degrees.get(edge.target) || 0;
      
      const expected = (ki * kj) / (2 * m);
      const actual = ci === cj ? 1 : 0;
      
      q += actual - expected;
    }
    
    return q / (2 * m);
  }
  
  private calculateClusterMetrics(
    nodeIds: string[],
    edges: KnowledgeEdge[],
    _adjacency: AdjacencyData
  ): { cohesion: number; coupling: number } {
    const nodeSet = new Set(nodeIds);
    
    let internalEdges = 0;
    let externalEdges = 0;
    
    for (const edge of edges) {
      const sourceIn = nodeSet.has(edge.source);
      const targetIn = nodeSet.has(edge.target);
      
      if (sourceIn && targetIn) {
        internalEdges++;
      } else if (sourceIn || targetIn) {
        externalEdges++;
      }
    }
    
    // Cohesion: internal density
    const maxInternal = (nodeIds.length * (nodeIds.length - 1)) / 2;
    const cohesion = maxInternal > 0 ? internalEdges / maxInternal : 0;
    
    // Coupling: ratio of external to total edges
    const totalClusterEdges = internalEdges + externalEdges;
    const coupling = totalClusterEdges > 0 ? externalEdges / totalClusterEdges : 0;
    
    return { cohesion, coupling };
  }
  
  // ==========================================================================
  // CRITICAL PATH ANALYSIS
  // ==========================================================================
  
  private analyzeCriticalPaths(
    nodes: KnowledgeNode[],
    edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): CriticalPathAnalysis {
    // Calculate betweenness centrality to find critical nodes
    const betweenness = this.calculateBetweenness(nodes, adjacency);
    
    // Sort by criticality
    const sortedNodes = Array.from(betweenness.entries())
      .sort((a, b) => b[1] - a[1]);
    
    // Top critical nodes
    const criticalNodes = sortedNodes.slice(0, 10).map(([nodeId, centrality]) => {
      const inDegree = adjacency.inDegree.get(nodeId) || 0;
      const outDegree = adjacency.outDegree.get(nodeId) || 0;
      
      let reason = 'High betweenness centrality';
      if (outDegree > inDegree * 2) {
        reason = 'Major source/provider of functionality';
      } else if (inDegree > outDegree * 2) {
        reason = 'Critical dependency hub';
      } else if (inDegree > 5 && outDegree > 5) {
        reason = 'Central integration point';
      }
      
      return {
        nodeId,
        criticality: centrality,
        reason,
        dependentCount: this.countDependents(nodeId, adjacency),
      };
    });
    
    // Identify bottlenecks (high in-degree, limited alternatives)
    const bottlenecks = this.findBottlenecks(nodes, edges, adjacency);
    
    return {
      criticalNodes,
      bottlenecks,
    };
  }
  
  private calculateBetweenness(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): Map<string, number> {
    const betweenness = new Map<string, number>();
    nodes.forEach(n => betweenness.set(n.id, 0));
    
    // Sample for large graphs
    const sampleSize = Math.min(50, nodes.length);
    const sampledNodes = nodes.slice(0, sampleSize);
    
    for (const source of sampledNodes) {
      // BFS from source
      const distances = new Map<string, number>();
      const paths = new Map<string, number>();
      const predecessors = new Map<string, string[]>();
      const stack: string[] = [];
      
      const queue: string[] = [source.id];
      distances.set(source.id, 0);
      paths.set(source.id, 1);
      
      while (queue.length > 0) {
        const v = queue.shift()!;
        stack.push(v);
        
        const neighbors = adjacency.undirected.get(v) || new Set();
        for (const w of neighbors) {
          if (!distances.has(w)) {
            distances.set(w, distances.get(v)! + 1);
            queue.push(w);
          }
          
          if (distances.get(w) === distances.get(v)! + 1) {
            paths.set(w, (paths.get(w) || 0) + (paths.get(v) || 0));
            if (!predecessors.has(w)) predecessors.set(w, []);
            predecessors.get(w)!.push(v);
          }
        }
      }
      
      // Accumulate betweenness
      const delta = new Map<string, number>();
      nodes.forEach(n => delta.set(n.id, 0));
      
      while (stack.length > 0) {
        const w = stack.pop()!;
        const preds = predecessors.get(w) || [];
        for (const v of preds) {
          const contribution = ((paths.get(v) || 0) / (paths.get(w) || 1)) * 
                              (1 + (delta.get(w) || 0));
          delta.set(v, (delta.get(v) || 0) + contribution);
        }
        
        if (w !== source.id) {
          betweenness.set(w, (betweenness.get(w) || 0) + (delta.get(w) || 0));
        }
      }
    }
    
    // Normalize
    const maxBetweenness = Math.max(...Array.from(betweenness.values()));
    if (maxBetweenness > 0) {
      for (const [id, value] of betweenness) {
        betweenness.set(id, value / maxBetweenness);
      }
    }
    
    return betweenness;
  }
  
  private countDependents(nodeId: string, adjacency: AdjacencyData): number {
    // Count all nodes that transitively depend on this node
    const visited = new Set<string>();
    const queue = [nodeId];
    
    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current)) continue;
      visited.add(current);
      
      // Find nodes that depend on this one (have edges pointing to it)
      const dependents = adjacency.inbound.get(current) || new Set();
      for (const dep of dependents) {
        if (!visited.has(dep)) {
          queue.push(dep);
        }
      }
    }
    
    return visited.size - 1; // Exclude self
  }
  
  private findBottlenecks(
    nodes: KnowledgeNode[],
    _edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): { nodeId: string; throughputImpact: number; alternatives: string[] }[] {
    const bottlenecks: { nodeId: string; throughputImpact: number; alternatives: string[] }[] = [];
    
    for (const node of nodes) {
      const inDegree = adjacency.inDegree.get(node.id) || 0;
      const outDegree = adjacency.outDegree.get(node.id) || 0;
      
      // High in-degree relative to out-degree suggests bottleneck
      if (inDegree > 3 && inDegree > outDegree * 1.5) {
        // Find if there are alternatives
        const provides = adjacency.outbound.get(node.id) || new Set();
        
        // Look for nodes that could provide similar functionality
        const alternatives: string[] = [];
        for (const target of provides) {
          const providers = adjacency.inbound.get(target) || new Set();
          for (const provider of providers) {
            if (provider !== node.id && !alternatives.includes(provider)) {
              alternatives.push(provider);
            }
          }
        }
        
        // Impact based on lack of alternatives
        const throughputImpact = alternatives.length === 0 ? 1 : 1 / (alternatives.length + 1);
        
        if (throughputImpact > 0.3) {
          bottlenecks.push({
            nodeId: node.id,
            throughputImpact,
            alternatives,
          });
        }
      }
    }
    
    return bottlenecks
      .sort((a, b) => b.throughputImpact - a.throughputImpact)
      .slice(0, 10);
  }
  
  // ==========================================================================
  // RISK ANALYSIS
  // ==========================================================================
  
  private analyzeRisks(
    nodes: KnowledgeNode[],
    edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): RiskAnalysis {
    const highRiskNodes = this.identifyHighRiskNodes(nodes, edges, adjacency);
    const singlePointsOfFailure = this.findSinglePointsOfFailure(nodes, adjacency);
    const orphanedNodes = this.findOrphanedNodes(nodes, adjacency);
    const circularDependencies = this.findCircularDependencies(nodes, edges, adjacency);
    
    return {
      highRiskNodes,
      singlePointsOfFailure,
      orphanedNodes,
      circularDependencies,
    };
  }
  
  private identifyHighRiskNodes(
    nodes: KnowledgeNode[],
    _edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): { nodeId: string; riskScore: number; riskFactors: string[]; mitigations: string[] }[] {
    const risks: { nodeId: string; riskScore: number; riskFactors: string[]; mitigations: string[] }[] = [];
    
    for (const node of nodes) {
      const riskFactors: string[] = [];
      const mitigations: string[] = [];
      let riskScore = 0;
      
      const inDegree = adjacency.inDegree.get(node.id) || 0;
      const outDegree = adjacency.outDegree.get(node.id) || 0;
      const totalDegree = inDegree + outDegree;
      
      // High coupling risk
      if (totalDegree > 10) {
        riskScore += 0.3;
        riskFactors.push('High coupling - many dependencies');
        mitigations.push('Consider interface abstraction');
      }
      
      // Low confidence risk
      const confidence = node.metadata?.confidence || 1;
      if (confidence < 0.5) {
        riskScore += 0.3;
        riskFactors.push('Low confidence in extracted logic');
        mitigations.push('Require SME validation');
      }
      
      // Business rule with many dependents
      if (node.type === 'business-rule' && inDegree > 5) {
        riskScore += 0.2;
        riskFactors.push('Critical business rule with many consumers');
        mitigations.push('Extensive testing required');
      }
      
      // Single point of failure (no alternatives)
      if (outDegree === 0 && inDegree > 3) {
        riskScore += 0.2;
        riskFactors.push('Potential single point of failure');
        mitigations.push('Consider redundancy or fallback');
      }
      
      if (riskScore > 0.3) {
        risks.push({
          nodeId: node.id,
          riskScore: Math.min(1, riskScore),
          riskFactors,
          mitigations,
        });
      }
    }
    
    return risks
      .sort((a, b) => b.riskScore - a.riskScore)
      .slice(0, 20);
  }
  
  private findSinglePointsOfFailure(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): string[] {
    const spof: string[] = [];
    
    for (const node of nodes) {
      const outDegree = adjacency.outDegree.get(node.id) || 0;
      const dependents = this.countDependents(node.id, adjacency);
      
      // Node that many depend on but provides unique functionality
      if (dependents > nodes.length * 0.1 && outDegree <= 2) {
        // Check if removal disconnects graph significantly
        spof.push(node.id);
      }
    }
    
    return spof.slice(0, 10);
  }
  
  private findOrphanedNodes(
    nodes: KnowledgeNode[],
    adjacency: AdjacencyData
  ): string[] {
    return nodes
      .filter(n => {
        const degree = adjacency.degrees.get(n.id) || 0;
        return degree === 0;
      })
      .map(n => n.id);
  }
  
  private findCircularDependencies(
    nodes: KnowledgeNode[],
    _edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): string[][] {
    const cycles: string[][] = [];
    const visited = new Set<string>();
    const recursionStack = new Set<string>();
    
    const dfs = (nodeId: string, path: string[]): void => {
      if (recursionStack.has(nodeId)) {
        // Found cycle
        const cycleStart = path.indexOf(nodeId);
        if (cycleStart !== -1) {
          cycles.push(path.slice(cycleStart));
        }
        return;
      }
      
      if (visited.has(nodeId)) return;
      
      visited.add(nodeId);
      recursionStack.add(nodeId);
      path.push(nodeId);
      
      const neighbors = adjacency.outbound.get(nodeId) || new Set();
      for (const neighbor of neighbors) {
        dfs(neighbor, [...path]);
      }
      
      recursionStack.delete(nodeId);
    };
    
    for (const node of nodes) {
      if (!visited.has(node.id)) {
        dfs(node.id, []);
      }
    }
    
    // Deduplicate cycles
    const seen = new Set<string>();
    return cycles.filter(cycle => {
      const key = [...cycle].sort().join(',');
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    }).slice(0, 10);
  }
  
  // ==========================================================================
  // RECOMMENDATIONS
  // ==========================================================================
  
  private generateRecommendations(
    nodes: KnowledgeNode[],
    _edges: KnowledgeEdge[],
    adjacency: AdjacencyData
  ): AnalysisRecommendation[] {
    const recommendations: AnalysisRecommendation[] = [];
    
    // Check for high coupling
    const highCouplingNodes = nodes.filter(n => 
      (adjacency.degrees.get(n.id) || 0) > 15
    );
    if (highCouplingNodes.length > 0) {
      recommendations.push({
        type: 'structure',
        severity: 'high',
        title: 'High coupling detected',
        description: `${highCouplingNodes.length} nodes have more than 15 connections, indicating tight coupling that may complicate migration.`,
        affectedNodes: highCouplingNodes.map(n => n.id),
        suggestedAction: 'Consider introducing abstraction layers or interfaces to reduce direct dependencies.',
      });
    }
    
    // Check for orphaned nodes
    const orphaned = nodes.filter(n => (adjacency.degrees.get(n.id) || 0) === 0);
    if (orphaned.length > 0) {
      recommendations.push({
        type: 'quality',
        severity: 'medium',
        title: 'Orphaned components found',
        description: `${orphaned.length} nodes have no connections. These may be dead code or missing relationship extraction.`,
        affectedNodes: orphaned.map(n => n.id),
        suggestedAction: 'Review orphaned nodes to determine if they are unused or if relationships need to be discovered.',
      });
    }
    
    // Check for low confidence rules
    const lowConfidence = nodes.filter(n => 
      n.type === 'business-rule' && (n.metadata?.confidence || 1) < 0.5
    );
    if (lowConfidence.length > 0) {
      recommendations.push({
        type: 'quality',
        severity: 'high',
        title: 'Low confidence business rules',
        description: `${lowConfidence.length} business rules have confidence scores below 50%. SME validation is critical.`,
        affectedNodes: lowConfidence.map(n => n.id),
        suggestedAction: 'Schedule SME review sessions to validate or correct low-confidence rules before migration.',
      });
    }
    
    // Check for missing coverage (simplified - just count business rules)
    const businessRules = nodes.filter(n => n.type === 'business-rule');
    if (businessRules.length > 10) {
      recommendations.push({
        type: 'coverage',
        severity: 'medium',
        title: 'Consider test coverage review',
        description: `${businessRules.length} business rules found. Ensure adequate test coverage for all rules.`,
        affectedNodes: businessRules.map(n => n.id),
        suggestedAction: 'Use the test oracle to generate test cases for business rules.',
      });
    }
    
    // Check for circular dependencies
    const cycles = this.findCircularDependencies(nodes, [], adjacency);
    if (cycles.length > 0) {
      recommendations.push({
        type: 'risk',
        severity: 'high',
        title: 'Circular dependencies detected',
        description: `${cycles.length} circular dependency chains found, which complicate incremental migration.`,
        affectedNodes: [...new Set(cycles.flat())],
        suggestedAction: 'Refactor to break circular dependencies before migration, or migrate entire cycles together.',
      });
    }
    
    return recommendations.sort((a, b) => {
      const severityOrder = { high: 0, medium: 1, low: 2 };
      return severityOrder[a.severity] - severityOrder[b.severity];
    });
  }
  
  // ==========================================================================
  // UTILITIES
  // ==========================================================================
  
  private buildAdjacency(nodes: KnowledgeNode[], edges: KnowledgeEdge[]): AdjacencyData {
    const undirected = new Map<string, Set<string>>();
    const outbound = new Map<string, Set<string>>();
    const inbound = new Map<string, Set<string>>();
    const degrees = new Map<string, number>();
    const inDegree = new Map<string, number>();
    const outDegree = new Map<string, number>();
    
    // Initialize
    nodes.forEach(n => {
      undirected.set(n.id, new Set());
      outbound.set(n.id, new Set());
      inbound.set(n.id, new Set());
      degrees.set(n.id, 0);
      inDegree.set(n.id, 0);
      outDegree.set(n.id, 0);
    });
    
    // Build from edges
    edges.forEach(e => {
      undirected.get(e.source)?.add(e.target);
      undirected.get(e.target)?.add(e.source);
      outbound.get(e.source)?.add(e.target);
      inbound.get(e.target)?.add(e.source);
      
      degrees.set(e.source, (degrees.get(e.source) || 0) + 1);
      degrees.set(e.target, (degrees.get(e.target) || 0) + 1);
      outDegree.set(e.source, (outDegree.get(e.source) || 0) + 1);
      inDegree.set(e.target, (inDegree.get(e.target) || 0) + 1);
    });
    
    return {
      undirected,
      outbound,
      inbound,
      degrees,
      inDegree,
      outDegree,
    };
  }
}

interface AdjacencyData {
  undirected: Map<string, Set<string>>;
  outbound: Map<string, Set<string>>;
  inbound: Map<string, Set<string>>;
  degrees: Map<string, number>;
  inDegree: Map<string, number>;
  outDegree: Map<string, number>;
}

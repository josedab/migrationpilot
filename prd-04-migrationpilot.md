# Product Requirements Document: MigrationPilot

## AI-Powered Legacy Code Modernization

**Document Version:** 1.0  
**Last Updated:** January 28, 2026  
**Author:** Jose David Baena  
**Status:** Draft  

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Problem Statement](#2-problem-statement)
3. [Market Analysis](#3-market-analysis)
4. [Target Users & Personas](#4-target-users--personas)
5. [Product Vision & Strategy](#5-product-vision--strategy)
6. [Features & Requirements](#6-features--requirements)
7. [Technical Architecture](#7-technical-architecture)
8. [User Stories & Use Cases](#8-user-stories--use-cases)
9. [User Experience & Design](#9-user-experience--design)
10. [Success Metrics & KPIs](#10-success-metrics--kpis)
11. [Competitive Analysis](#11-competitive-analysis)
12. [Go-to-Market Strategy](#12-go-to-market-strategy)
13. [Monetization Strategy](#13-monetization-strategy)
14. [Risks & Mitigations](#14-risks--mitigations)
15. [Roadmap & Milestones](#15-roadmap--milestones)
16. [Dependencies & Constraints](#16-dependencies--constraints)
17. [Appendices](#17-appendices)

---

## 1. Executive Summary

### 1.1 Product Overview

MigrationPilot is an AI-powered platform that automates the modernization of legacy codebases by understanding undocumented business logic, generating modern implementations, and validating behavioral equivalence. Using a multi-agent architecture built on the GitHub Copilot SDK, MigrationPilot transforms COBOL, Fortran, legacy Java, Visual Basic, and other aging systems into modern, maintainable code while preserving critical business rules.

### 1.2 Value Proposition

**For enterprises trapped by legacy code**, MigrationPilot dramatically reduces the cost, time, and risk of modernization projects—transforming multi-year, multi-million-dollar initiatives into months-long, predictable engagements with automated validation that business logic is preserved.

### 1.3 Key Differentiators

- **Multi-Agent Architecture:** Specialized AI agents for archaeology (understanding), architecture (design), building (generation), and validation (testing)
- **Behavioral Extraction:** Understands code through execution tracing, not just static analysis
- **Incremental Migration:** Strangler fig pattern enables gradual, low-risk modernization
- **Formal Equivalence Testing:** Property-based testing proves behavioral preservation
- **Business Logic Documentation:** Generates human-readable specifications as a byproduct

### 1.4 Business Opportunity

- **Target Market Size:** $8-15B legacy modernization market by 2030
- **Revenue Model:** Project-based ($100K-500K per engagement) or platform licensing ($50K+/year)
- **Primary Customers:** Large enterprises with COBOL, Fortran, legacy Java/VB codebases

---

## 2. Problem Statement

### 2.1 The Legacy Code Crisis

The world runs on legacy code that nobody fully understands:

**The Scale of the Problem:**
- **240 billion lines of COBOL** still in active use globally
- **95% of ATM transactions** processed by COBOL systems
- **80% of in-person transactions** rely on COBOL
- **43% of banking systems** built on COBOL
- Average age of core banking systems: **25-30 years**

**The Human Capital Crisis:**
- Average COBOL programmer age: **55-60 years**
- COBOL developer workforce shrinking by **5% annually**
- Universities stopped teaching COBOL decades ago
- Knowledge leaving with retiring workforce

### 2.2 Why Migration Projects Fail

Legacy modernization has a notoriously high failure rate:

**Common Failure Modes:**

| Failure Mode | Frequency | Consequence |
|--------------|-----------|-------------|
| Undocumented Business Logic | 70% | Critical rules lost in translation |
| Big Bang Approach | 60% | Catastrophic failures at cutover |
| Underestimated Complexity | 80% | Budget/timeline overruns |
| Testing Gaps | 65% | Production defects post-migration |
| Knowledge Silos | 55% | Dependency on retiring experts |

**Case Study: Failed Migration**
A major US bank attempted a COBOL-to-Java migration in 2019:
- Original estimate: $50M, 3 years
- Actual spend before cancellation: $180M, 5 years
- Result: Project abandoned, legacy system still running
- Root cause: Undocumented business rules in 40-year-old code

### 2.3 Current Approaches Fall Short

| Approach | Limitation |
|----------|------------|
| **Manual Rewrite** | $1M+ per application, 2-5 years, high risk |
| **Transpilers** | Syntax translation only; no understanding of intent |
| **Lift and Shift** | Moves problems to cloud without solving them |
| **Strangler Fig (Manual)** | Sound approach but still requires understanding legacy code |
| **Consultants** | Expensive, variable quality, knowledge doesn't stay |

### 2.4 The Opportunity

MigrationPilot leverages AI to solve the core challenges:

1. **Understanding:** AI can read and comprehend legacy code patterns
2. **Extraction:** Behavioral analysis captures actual business logic
3. **Generation:** Modern code generated following best practices
4. **Validation:** Automated equivalence testing ensures correctness
5. **Documentation:** Business logic documented as migration byproduct

---

## 3. Market Analysis

### 3.1 Market Size & Growth

**Total Addressable Market (TAM):**
- Application modernization services: $24.8B by 2025, growing to $40B+ by 2030
- CAGR: 16.8%

**Serviceable Addressable Market (SAM):**
- Legacy code migration (COBOL, Fortran, VB, legacy Java): $8-15B by 2030
- Subset requiring AI-assisted migration: $5-10B

**Serviceable Obtainable Market (SOM):**
- AI-powered migration platform: $500M-1B by 2028
- Based on: 10,000 enterprises with legacy systems, 5% adoption, $100K average engagement

### 3.2 Market Trends

**Favorable Trends:**

1. **Mainframe Workforce Crisis:** Accelerating retirements creating urgency
2. **Cloud Mandates:** Organizations required to modernize for cloud adoption
3. **AI Capabilities:** LLMs now capable of understanding legacy languages
4. **Regulatory Pressure:** Auditors questioning sustainability of legacy systems
5. **Security Concerns:** Legacy systems increasingly targeted by attackers

**A16Z Perspective:**
> "Legacy code migration consistently emerges as one of the most successful AI coding use cases. The combination of repetitive patterns, well-defined inputs/outputs, and high value per successful migration makes it ideal for AI assistance."

### 3.3 Industry Analysis

**Market Segments:**

| Segment | Size | Migration Urgency | Budget |
|---------|------|-------------------|--------|
| Banking/Finance | 5,000+ enterprises | Critical | $10M+ |
| Insurance | 3,000+ enterprises | High | $5-10M |
| Government | 10,000+ agencies | Medium-High | $2-20M |
| Healthcare | 4,000+ systems | High | $3-8M |
| Manufacturing | 8,000+ enterprises | Medium | $1-5M |

**Competitive Dynamics:**
- Consulting firms (Accenture, IBM, Deloitte) dominate with services
- Few product-based solutions exist
- AI-native approaches emerging but immature

### 3.4 Regulatory Environment

- **DORA (EU):** Digital Operational Resilience Act requires financial institutions to address IT risks including legacy systems
- **Federal Modernization Mandates:** US government agencies required to modernize aging systems
- **SOX/PCI Compliance:** Auditors increasingly scrutinizing legacy system risks
- **Cyber Insurance:** Premiums increasing for organizations with unpatched legacy systems

---

## 4. Target Users & Personas

### 4.1 Primary Personas

#### Persona 1: Richard - CIO / VP of Technology

**Demographics:**
- Title: CIO, VP Technology, Chief Digital Officer
- Company: Fortune 1000 enterprise
- Budget Authority: $10M+ annually for technology initiatives

**Goals:**
- Reduce technical debt and operational risk
- Enable digital transformation initiatives
- Control IT costs while modernizing
- Satisfy board/regulatory requirements

**Pain Points:**
- Legacy systems blocking cloud migration
- Retiring workforce taking knowledge with them
- Failed migration projects in the past
- Can't justify multi-year, uncertain investments

**Behavior:**
- Relies on consultants for major decisions
- Attends Gartner conferences
- Reports to CEO and board on technology risk
- Risk-averse due to past failures

**Quote:** *"I need to modernize our core systems, but I can't afford another failed migration project."*

---

#### Persona 2: Margaret - Enterprise Architect

**Demographics:**
- Title: Enterprise Architect / Principal Architect
- Experience: 20+ years, often started on legacy systems
- Role: Technical strategy, migration planning

**Goals:**
- Design target architecture for modernization
- Ensure business logic preservation
- Minimize disruption during transition
- Build internal capabilities

**Pain Points:**
- Can't fully document existing system behavior
- Dependent on retiring SMEs
- Previous migration tools produced unmaintainable code
- Testing coverage for legacy systems is poor

**Behavior:**
- Deep technical evaluator
- Creates architecture decision records
- Influences vendor selection
- Bridges business and technology

**Quote:** *"The hardest part isn't writing new code—it's understanding what the old code actually does."*

---

#### Persona 3: David - Legacy System SME

**Demographics:**
- Title: Senior Developer / System Analyst
- Experience: 25-35 years, deep legacy expertise
- Status: Often approaching retirement

**Goals:**
- Transfer knowledge before retiring
- See systems modernized properly
- Reduce maintenance burden
- Leave a positive legacy (pun intended)

**Pain Points:**
- Only person who understands certain systems
- Documentation is incomplete or wrong
- Constantly firefighting production issues
- No time for proper knowledge transfer

**Behavior:**
- Protective of "their" systems
- Skeptical of automated tools
- Invaluable for edge case knowledge
- Often works around formal processes

**Quote:** *"I've been maintaining this system for 30 years. There are things in here that aren't written down anywhere."*

---

#### Persona 4: Jennifer - Program Manager

**Demographics:**
- Title: Program Manager / Transformation Lead
- Background: IT delivery, change management
- Responsibility: Migration program execution

**Goals:**
- Deliver migration on time and budget
- Manage stakeholder expectations
- Minimize business disruption
- Demonstrate measurable progress

**Pain Points:**
- Unpredictable timelines for understanding legacy code
- Scope creep from discovered complexity
- Difficulty measuring progress
- Coordination across multiple teams

**Quote:** *"Every time we dig into the legacy code, we find something unexpected that blows our timeline."*

---

### 4.2 Secondary Personas

#### Persona 5: Consulting Partner

**Demographics:**
- Partner at Accenture, Deloitte, IBM, etc.
- Sells and delivers migration engagements
- Measured on revenue and client satisfaction

**Goals:**
- Differentiate firm's migration offerings
- Reduce delivery risk
- Improve margins on migration projects
- Build repeatable practice

---

#### Persona 6: Quality Assurance Lead

**Demographics:**
- Responsible for testing migrated systems
- Often lacks legacy system expertise
- Critical for go/no-go decisions

**Goals:**
- Comprehensive test coverage
- Confidence in behavioral equivalence
- Automated regression testing
- Clear defect attribution

---

### 4.3 Organizational Buying Dynamics

```
┌─────────────────────────────────────────────────────────────────┐
│                    Enterprise Buying Center                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │   Economic   │    │   Technical  │    │     User     │       │
│  │    Buyer     │    │   Evaluator  │    │   Champion   │       │
│  │    (CIO)     │    │  (Architect) │    │    (SME)     │       │
│  │              │    │              │    │              │       │
│  │ • Budget     │    │ • Technical  │    │ • Day-to-day │       │
│  │ • Risk       │    │   fit        │    │   usage      │       │
│  │ • ROI        │    │ • Security   │    │ • Adoption   │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│         │                   │                   │                │
│         └───────────────────┴───────────────────┘                │
│                             │                                    │
│                    ┌────────▼────────┐                          │
│                    │   Procurement   │                          │
│                    │   • Legal       │                          │
│                    │   • Security    │                          │
│                    │   • Compliance  │                          │
│                    └─────────────────┘                          │
│                                                                  │
│  Sales Cycle: 6-12 months                                       │
│  Decision Process: Committee-based                              │
│  Key Influencer: Enterprise Architect                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## 5. Product Vision & Strategy

### 5.1 Vision Statement

**"Unlock the $240 billion lines of legacy code holding enterprises hostage—transforming technical debt into modern, maintainable assets while preserving decades of encoded business knowledge."**

### 5.2 Mission

To make legacy code migration predictable, affordable, and safe by using AI to understand, translate, and validate business logic that has been locked in aging systems for decades.

### 5.3 Strategic Pillars

#### Pillar 1: Deep Understanding
Go beyond syntax translation—truly understand what legacy code does and why.

#### Pillar 2: Provable Correctness
Don't trust, verify—automated testing proves behavioral equivalence.

#### Pillar 3: Incremental Safety
Enable gradual migration that reduces risk at every step.

#### Pillar 4: Knowledge Capture
Extract and document business logic that exists only in code.

### 5.4 Product Principles

1. **Correctness Over Speed:** Better to migrate slowly and correctly than fast and broken
2. **Human-in-the-Loop:** AI assists experts, doesn't replace them
3. **Transparency:** Show reasoning, not just results
4. **Reversibility:** Every migration step can be validated and rolled back

### 5.5 Success Criteria

**Year 1:**
- 10 completed migration engagements
- $3M in project revenue
- 95% behavioral equivalence in migrated code
- 3 reference customers

**Year 3:**
- 100 active enterprise customers
- $25M ARR (platform + services)
- Partner ecosystem with 5+ consulting firms
- Industry recognition as migration leader

---

## 6. Features & Requirements

### 6.1 Feature Overview

| Feature | Priority | Phase | Description |
|---------|----------|-------|-------------|
| Legacy Code Analysis | P0 | MVP | Parse and understand legacy languages |
| Business Logic Extraction | P0 | MVP | Identify and document business rules |
| Modern Code Generation | P0 | MVP | Generate target language implementations |
| Equivalence Testing | P0 | MVP | Validate behavioral preservation |
| Incremental Migration | P1 | V1.1 | Strangler fig pattern support |
| Progress Dashboard | P1 | V1.1 | Track migration progress and quality |
| SME Collaboration | P1 | V1.1 | Tools for expert knowledge capture |
| Multi-Language Support | P2 | V1.2 | Expand source/target language pairs |
| CI/CD Integration | P2 | V1.2 | Automated migration pipelines |
| Enterprise Security | P2 | V2.0 | On-premises, air-gapped deployment |

### 6.2 Functional Requirements

#### FR-001: Legacy Code Parsing

**Description:** Parse and create semantic representation of legacy code across multiple languages.

**Acceptance Criteria:**
- Support COBOL (COBOL-85, COBOL-2002, Enterprise COBOL)
- Support Fortran (F77, F90, F95)
- Support Visual Basic 6 and VBA
- Support legacy Java (J2EE, EJB 2.x)
- Create abstract syntax tree (AST) representation
- Identify data structures, control flow, and dependencies
- Handle dialect variations and vendor extensions

**Supported Languages (MVP):**

| Language | Dialects | Complexity |
|----------|----------|------------|
| COBOL | IBM Enterprise, Micro Focus, GnuCOBOL | High |
| Fortran | F77, F90 | Medium |
| VB6/VBA | Standard | Medium |
| Legacy Java | J2EE, EJB 2.x | Medium |

**Technical Notes:**
- Use ANTLR grammars for parsing
- Handle copybooks and includes
- Resolve cross-file dependencies

---

#### FR-002: Business Logic Extraction

**Description:** Identify, extract, and document business rules embedded in legacy code.

**Acceptance Criteria:**
- Identify business rule patterns (calculations, validations, decisions)
- Extract rule logic with context
- Generate human-readable rule documentation
- Map rules to code locations
- Detect implicit rules from code patterns

**Business Rule Categories:**

| Category | Examples | Extraction Method |
|----------|----------|-------------------|
| Calculations | Interest rates, fees, taxes | Formula extraction |
| Validations | Input checks, range limits | Condition analysis |
| Decisions | Eligibility, routing, classification | Decision tree extraction |
| Transformations | Data formatting, conversions | Pattern matching |
| Workflows | Process sequences, state machines | Control flow analysis |

**Rule Documentation Format:**

```yaml
rule:
  id: BR-001
  name: "Late Payment Fee Calculation"
  source_file: "PAYMENT.CBL"
  source_lines: [1234, 1267]
  
  description: |
    Calculates late payment fee based on days overdue and account type.
    Premium accounts receive 50% fee reduction.
  
  logic:
    inputs:
      - name: days_overdue
        type: integer
        source: WS-DAYS-LATE
      - name: account_type
        type: string
        source: WS-ACCT-TYPE
      - name: balance
        type: decimal
        source: WS-BALANCE
    
    calculation: |
      base_fee = balance * 0.015 * days_overdue
      if account_type == 'PREMIUM':
        fee = base_fee * 0.5
      else:
        fee = base_fee
      fee = min(fee, 500.00)  # Maximum cap
    
    output:
      - name: late_fee
        type: decimal
  
  edge_cases:
    - "Zero balance returns zero fee"
    - "Maximum fee capped at $500"
    - "Premium accounts get 50% reduction"
  
  confidence: 0.94
  requires_sme_review: false
```

---

#### FR-003: Modern Code Generation

**Description:** Generate modern, idiomatic code in target language that implements extracted business logic.

**Acceptance Criteria:**
- Generate code in target languages (Java, Python, TypeScript, Go, C#)
- Follow language-specific best practices and idioms
- Produce well-structured, maintainable code
- Include comprehensive comments referencing source
- Generate corresponding unit tests

**Target Language Support:**

| Target | Framework Options | Status |
|--------|-------------------|--------|
| Java | Spring Boot, Quarkus | MVP |
| Python | FastAPI, Django | MVP |
| TypeScript | Node.js, NestJS | V1.1 |
| Go | Standard library | V1.2 |
| C# | .NET Core | V1.2 |

**Code Generation Principles:**

1. **Traceability:** Every generated line links back to source
2. **Readability:** Optimize for human understanding, not just correctness
3. **Testability:** Design for easy unit testing
4. **Modularity:** Break monoliths into services where appropriate

---

#### FR-004: Behavioral Equivalence Testing

**Description:** Automatically validate that migrated code behaves identically to original.

**Acceptance Criteria:**
- Generate test cases from code analysis
- Execute tests against both legacy and modern systems
- Compare outputs for equivalence
- Report discrepancies with detailed diagnostics
- Support property-based testing for comprehensive coverage

**Testing Strategies:**

| Strategy | Description | Coverage |
|----------|-------------|----------|
| Trace Replay | Record legacy execution, replay on modern | Historical scenarios |
| Property-Based | Generate random inputs, compare outputs | Edge cases |
| Boundary Analysis | Test at value boundaries | Numeric precision |
| Scenario Extraction | Extract test cases from code paths | All code paths |
| Production Shadow | Mirror production traffic to both systems | Real-world scenarios |

**Equivalence Validation:**

```python
class EquivalenceValidator:
    def validate(
        self,
        legacy_system: LegacyAdapter,
        modern_system: ModernAdapter,
        test_cases: List[TestCase]
    ) -> EquivalenceReport:
        results = []
        
        for test_case in test_cases:
            # Execute on both systems
            legacy_result = legacy_system.execute(test_case.inputs)
            modern_result = modern_system.execute(test_case.inputs)
            
            # Compare with tolerance for floating point
            equivalent = self.compare_results(
                legacy_result,
                modern_result,
                tolerance=test_case.tolerance
            )
            
            results.append(EquivalenceResult(
                test_case=test_case,
                legacy_output=legacy_result,
                modern_output=modern_result,
                equivalent=equivalent,
                differences=self.diff(legacy_result, modern_result) if not equivalent else None
            ))
        
        return EquivalenceReport(
            total_tests=len(results),
            passed=sum(1 for r in results if r.equivalent),
            failed=[r for r in results if not r.equivalent],
            confidence=self.calculate_confidence(results)
        )
```

---

#### FR-005: Incremental Migration Support

**Description:** Enable gradual migration using strangler fig pattern with traffic routing.

**Acceptance Criteria:**
- Generate API facades for legacy systems
- Support traffic splitting between legacy and modern
- Enable feature-by-feature migration
- Provide rollback capabilities
- Track migration progress

**Strangler Fig Architecture:**

```
┌─────────────────────────────────────────────────────────────────┐
│                    Strangler Fig Pattern                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────┐                                                │
│  │   Client    │                                                │
│  │  Requests   │                                                │
│  └──────┬──────┘                                                │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Router / Facade                       │   │
│  │  ┌─────────────────────────────────────────────────┐    │   │
│  │  │  Feature A: 100% Modern  ✓                      │    │   │
│  │  │  Feature B: 80% Modern / 20% Legacy (shadow)    │    │   │
│  │  │  Feature C: 100% Legacy                         │    │   │
│  │  └─────────────────────────────────────────────────┘    │   │
│  └─────────────────────────────────────────────────────────┘   │
│         │                                   │                    │
│         ▼                                   ▼                    │
│  ┌─────────────┐                    ┌─────────────┐             │
│  │   Modern    │                    │   Legacy    │             │
│  │   Service   │                    │   System    │             │
│  │  (Feature A,│                    │  (Feature B,│             │
│  │   B shadow) │                    │   C)        │             │
│  └─────────────┘                    └─────────────┘             │
│                                                                  │
│  Migration Progress:                                            │
│  ████████████████░░░░░░░░░░ 65% Complete                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

#### FR-006: Knowledge Capture Interface

**Description:** Tools for SMEs to review, correct, and enrich AI-extracted business logic.

**Acceptance Criteria:**
- Present extracted rules for review
- Allow corrections and additions
- Capture rationale and edge cases
- Version control for rule changes
- Export documentation in multiple formats

---

#### FR-007: Migration Dashboard

**Description:** Track migration progress, quality metrics, and project health.

**Acceptance Criteria:**
- Visualize migration progress by module/feature
- Show equivalence test results
- Track rule extraction coverage
- Display quality metrics over time
- Export reports for stakeholders

---

### 6.3 Non-Functional Requirements

#### NFR-001: Performance

| Metric | Requirement |
|--------|-------------|
| Code Analysis | 100K lines/hour |
| Code Generation | 10K lines/hour |
| Test Execution | 1000 test cases/minute |
| Dashboard Response | <2s for any view |

#### NFR-002: Security

| Requirement | Description |
|-------------|-------------|
| Data Encryption | AES-256 at rest, TLS 1.3 in transit |
| Code Isolation | Customer code never leaves their environment (on-prem option) |
| Access Control | Role-based access, audit logging |
| Compliance | SOC 2 Type II, support for regulated industries |

#### NFR-003: Reliability

| Metric | Requirement |
|--------|-------------|
| Analysis Accuracy | >95% correct rule extraction |
| Generation Quality | >90% compilable first-pass |
| Equivalence Detection | <1% false negatives (missed differences) |
| Platform Availability | 99.9% for cloud version |

---

## 7. Technical Architecture

### 7.1 System Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                       MigrationPilot Architecture                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                         Agent Orchestrator                            │   │
│  │                      (GitHub Copilot SDK)                             │   │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐     │   │
│  │  │Archeologist│  │  Architect │  │  Builder   │  │ Validator  │     │   │
│  │  │   Agent    │  │   Agent    │  │   Agent    │  │   Agent    │     │   │
│  │  │  (GPT-5)   │  │ (Claude)   │  │  (Codex)   │  │  (GPT-5)   │     │   │
│  │  │            │  │            │  │            │  │            │     │   │
│  │  │ • Parse    │  │ • Design   │  │ • Generate │  │ • Test     │     │   │
│  │  │ • Analyze  │  │ • Structure│  │ • Document │  │ • Compare  │     │   │
│  │  │ • Extract  │  │ • Map      │  │ • Refactor │  │ • Validate │     │   │
│  │  └────────────┘  └────────────┘  └────────────┘  └────────────┘     │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                      │                                       │
│                                      ▼                                       │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                        Processing Pipeline                            │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐            │   │
│  │  │  Parse   │─▶│  Analyze │─▶│ Generate │─▶│ Validate │            │   │
│  │  │  Legacy  │  │  Logic   │  │  Modern  │  │  Equiv   │            │   │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘            │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                      │                                       │
│  ┌───────────────────────────────────┼───────────────────────────────────┐  │
│  │                     Infrastructure Layer                               │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐             │  │
│  │  │ Postgres │  │   S3     │  │  Redis   │  │  Kafka   │             │  │
│  │  │(Metadata)│  │ (Code)   │  │ (Cache)  │  │ (Events) │             │  │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘             │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 7.2 Multi-Agent Architecture

#### 7.2.1 Agent Orchestrator

**Responsibility:** Coordinate specialized agents through the migration pipeline

**Technology:**
- GitHub Copilot SDK for agent management
- Async orchestration with state management
- Human-in-the-loop integration points

**Implementation:**

```python
from copilot import CopilotClient
from copilot.tools import define_tool

class MigrationOrchestrator:
    def __init__(self):
        self.client = CopilotClient()
    
    async def migrate_module(
        self,
        source_path: str,
        source_lang: str,
        target_lang: str,
        config: MigrationConfig
    ) -> MigrationResult:
        
        # Phase 1: Archeologist Agent - Understand Legacy Code
        archeologist = await self.client.create_session({
            "model": "gpt-5",
            "tools": [
                self.cobol_parser,
                self.fortran_parser,
                self.vb_parser,
                self.control_flow_analyzer,
                self.data_flow_analyzer,
                self.dependency_mapper
            ],
            "systemMessage": {
                "mode": "append",
                "content": """You are a legacy code archeologist with deep expertise in 
                COBOL, Fortran, and legacy systems. Your job is to thoroughly understand 
                legacy code: its structure, business logic, data flows, and hidden rules.
                
                Extract every business rule, no matter how obscure. Pay special attention to:
                - Implicit rules encoded in variable names or comments
                - Edge cases handled by specific code paths
                - Temporal logic (end of month, fiscal year, etc.)
                - Industry-specific calculations (interest, amortization, etc.)
                """
            }
        })
        
        analysis_result = await archeologist.sendAndWait({
            "prompt": f"""
            Analyze this {source_lang} code module:
            
            File: {source_path}
            Code:
            ```
            {await self.load_source(source_path)}
            ```
            
            Provide:
            1. Complete structural analysis (programs, paragraphs, sections)
            2. All data structures with their purposes
            3. Every business rule extracted with confidence scores
            4. Control flow and decision logic
            5. External dependencies (files, databases, other programs)
            6. Potential edge cases and special handling
            """
        })
        
        # Phase 2: Architect Agent - Design Modern Structure
        architect = await self.client.create_session({
            "model": "claude-sonnet-4.5",
            "tools": [
                self.module_designer,
                self.api_specifier,
                self.database_mapper,
                self.service_decomposer
            ],
            "systemMessage": {
                "content": """You are a software architect specializing in modernization.
                Design clean, modern architectures that preserve business logic while
                following current best practices.
                
                Principles:
                - Favor composition over inheritance
                - Design for testability
                - Apply domain-driven design where appropriate
                - Consider microservices boundaries carefully
                """
            }
        })
        
        architecture = await architect.sendAndWait({
            "prompt": f"""
            Based on this legacy code analysis:
            {analysis_result}
            
            Design the modern {target_lang} architecture:
            1. Module/service structure
            2. Class/function organization
            3. Data models and database schema
            4. API contracts
            5. Integration points with other systems
            6. Testing strategy
            """
        })
        
        # Phase 3: Builder Agent - Generate Modern Code
        builder = await self.client.create_session({
            "model": "gpt-5.2-codex",
            "tools": [
                self.code_generator,
                self.test_generator,
                self.documentation_generator
            ],
            "systemMessage": {
                "content": f"""You are an expert {target_lang} developer.
                Generate clean, idiomatic, well-documented code.
                
                Every generated function must:
                - Include docstring with business rule reference
                - Have corresponding unit tests
                - Follow {target_lang} best practices
                - Be traceable to source legacy code
                """
            }
        })
        
        generated_code = await builder.sendAndWait({
            "prompt": f"""
            Generate {target_lang} implementation based on:
            
            Legacy Analysis:
            {analysis_result}
            
            Target Architecture:
            {architecture}
            
            Generate:
            1. All source code files
            2. Unit tests for each component
            3. Integration tests
            4. API documentation
            5. Mapping document (legacy line -> modern line)
            """
        })
        
        # Phase 4: Validator Agent - Verify Equivalence
        validator = await self.client.create_session({
            "model": "gpt-5",
            "tools": [
                self.test_case_generator,
                self.equivalence_checker,
                self.property_tester,
                self.coverage_analyzer
            ],
            "mcpServers": {
                "legacy_executor": {
                    "type": "http",
                    "url": config.legacy_executor_url
                },
                "modern_executor": {
                    "type": "local",
                    "command": f"{target_lang}-runner"
                }
            }
        })
        
        validation_result = await validator.sendAndWait({
            "prompt": f"""
            Validate behavioral equivalence between:
            
            Legacy: {source_path}
            Modern: {generated_code.main_file}
            
            Business Rules to Verify:
            {analysis_result.business_rules}
            
            Execute:
            1. Generate comprehensive test cases covering all rules
            2. Run tests on both legacy and modern systems
            3. Compare outputs with appropriate tolerances
            4. Report any discrepancies with root cause analysis
            5. Calculate overall equivalence confidence
            """
        })
        
        return MigrationResult(
            analysis=analysis_result,
            architecture=architecture,
            generated_code=generated_code,
            validation=validation_result,
            confidence=validation_result.confidence,
            requires_review=validation_result.confidence < 0.95
        )
```

---

#### 7.2.2 Legacy Code Parsers

**Responsibility:** Parse legacy languages into analyzable AST representations

**Technology:**
- ANTLR for grammar-based parsing
- Custom parsers for dialects
- Tree-sitter for incremental parsing

**COBOL Parser Example:**

```python
class COBOLParser:
    def __init__(self):
        self.grammar = load_antlr_grammar("cobol85")
        self.copybook_resolver = CopybookResolver()
    
    def parse(self, source: str, copybook_paths: List[str]) -> COBOLProgram:
        # Resolve copybooks (COPY statements)
        expanded_source = self.copybook_resolver.expand(source, copybook_paths)
        
        # Parse with ANTLR
        lexer = COBOL85Lexer(InputStream(expanded_source))
        tokens = CommonTokenStream(lexer)
        parser = COBOL85Parser(tokens)
        tree = parser.compilationUnit()
        
        # Build semantic model
        visitor = COBOLSemanticVisitor()
        program = visitor.visit(tree)
        
        # Analyze data division
        program.data_structures = self.analyze_data_division(program)
        
        # Analyze procedure division
        program.procedures = self.analyze_procedure_division(program)
        
        # Extract business rules
        program.business_rules = self.extract_business_rules(program)
        
        return program
    
    def extract_business_rules(self, program: COBOLProgram) -> List[BusinessRule]:
        rules = []
        
        # Pattern: COMPUTE statements often encode business calculations
        for compute in program.find_all("COMPUTE"):
            rule = self.analyze_computation(compute)
            if rule:
                rules.append(rule)
        
        # Pattern: EVALUATE statements often encode business decisions
        for evaluate in program.find_all("EVALUATE"):
            rule = self.analyze_decision(evaluate)
            if rule:
                rules.append(rule)
        
        # Pattern: Condition-guarded logic often encodes validations
        for if_stmt in program.find_all("IF"):
            rule = self.analyze_condition(if_stmt)
            if rule:
                rules.append(rule)
        
        return rules
```

---

#### 7.2.3 Equivalence Testing Engine

**Responsibility:** Validate that modern code behaves identically to legacy

**Technology:**
- Property-based testing (Hypothesis)
- Execution trace comparison
- Differential testing

**Testing Implementation:**

```python
from hypothesis import given, strategies as st

class EquivalenceTestEngine:
    def __init__(
        self,
        legacy_adapter: LegacySystemAdapter,
        modern_adapter: ModernSystemAdapter
    ):
        self.legacy = legacy_adapter
        self.modern = modern_adapter
    
    async def generate_test_cases(
        self,
        business_rules: List[BusinessRule]
    ) -> List[TestCase]:
        test_cases = []
        
        for rule in business_rules:
            # Boundary value analysis
            test_cases.extend(self.generate_boundary_tests(rule))
            
            # Equivalence partitioning
            test_cases.extend(self.generate_partition_tests(rule))
            
            # Historical data replay
            test_cases.extend(await self.extract_historical_cases(rule))
        
        return test_cases
    
    def generate_boundary_tests(self, rule: BusinessRule) -> List[TestCase]:
        tests = []
        
        for input_param in rule.inputs:
            if input_param.type == "decimal":
                # Test at boundaries
                tests.append(TestCase(
                    inputs={input_param.name: Decimal("0")},
                    description=f"Zero value for {input_param.name}"
                ))
                tests.append(TestCase(
                    inputs={input_param.name: input_param.max_value},
                    description=f"Max value for {input_param.name}"
                ))
                tests.append(TestCase(
                    inputs={input_param.name: input_param.max_value + Decimal("0.01")},
                    description=f"Just over max for {input_param.name}"
                ))
        
        return tests
    
    @given(st.decimals(min_value=0, max_value=1000000))
    async def property_test_calculation(self, amount: Decimal):
        """Property-based test: outputs should match for any valid input"""
        legacy_result = await self.legacy.execute({"amount": amount})
        modern_result = await self.modern.execute({"amount": amount})
        
        assert self.results_equivalent(legacy_result, modern_result), \
            f"Mismatch for amount={amount}: legacy={legacy_result}, modern={modern_result}"
    
    def results_equivalent(
        self,
        legacy: Any,
        modern: Any,
        tolerance: Decimal = Decimal("0.01")
    ) -> bool:
        if isinstance(legacy, Decimal) and isinstance(modern, Decimal):
            return abs(legacy - modern) <= tolerance
        return legacy == modern
```

---

### 7.3 Data Flow

```
1. Legacy source code ingested
         │
         ▼
2. Copybooks/includes resolved, code expanded
         │
         ▼
3. Archeologist Agent parses and analyzes
         │
         ├── Control flow extraction
         ├── Data flow analysis
         ├── Business rule identification
         └── Dependency mapping
         │
         ▼
4. Human review of extracted rules (optional)
         │
         ▼
5. Architect Agent designs target structure
         │
         ├── Module decomposition
         ├── API design
         └── Data model mapping
         │
         ▼
6. Builder Agent generates modern code
         │
         ├── Source code
         ├── Unit tests
         └── Documentation
         │
         ▼
7. Validator Agent tests equivalence
         │
         ├── Test case generation
         ├── Execution on both systems
         └── Difference analysis
         │
         ▼
8. Results reviewed, migration approved/refined
```

### 7.4 Infrastructure

#### 7.4.1 Deployment Options

| Option | Description | Use Case |
|--------|-------------|----------|
| Cloud (SaaS) | Fully managed, multi-tenant | Standard deployments |
| Private Cloud | Single-tenant, customer's cloud | Data sovereignty |
| On-Premises | Air-gapped, behind firewall | Highly regulated |
| Hybrid | Cloud orchestration, on-prem execution | Sensitive code |

#### 7.4.2 On-Premises Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                Customer Data Center                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │              MigrationPilot On-Prem                      │   │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐           │   │
│  │  │ Agents    │  │ Parsers   │  │ Validators│           │   │
│  │  │(Local LLM │  │           │  │           │           │   │
│  │  │ or API)   │  │           │  │           │           │   │
│  │  └───────────┘  └───────────┘  └───────────┘           │   │
│  │         │              │              │                 │   │
│  │         └──────────────┴──────────────┘                 │   │
│  │                        │                                │   │
│  │                ┌───────▼───────┐                        │   │
│  │                │   Local DB    │                        │   │
│  │                │  (PostgreSQL) │                        │   │
│  │                └───────────────┘                        │   │
│  └─────────────────────────────────────────────────────────┘   │
│                           │                                     │
│                           │ (Code never leaves)                │
│                           ▼                                     │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │              Customer Legacy Systems                     │   │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐           │   │
│  │  │ Mainframe │  │ AS/400    │  │ Legacy DB │           │   │
│  │  │  (COBOL)  │  │ (RPG)     │  │  (IMS)    │           │   │
│  │  └───────────┘  └───────────┘  └───────────┘           │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## 8. User Stories & Use Cases

### 8.1 Epic: Legacy Code Understanding

#### US-001: Parse COBOL Program
**As an** architect  
**I want** to parse a COBOL program and see its structure  
**So that** I can understand what it does before migration  

**Acceptance Criteria:**
- Upload or connect to COBOL source
- See program structure visualization
- View data division analysis
- See procedure division breakdown

---

#### US-002: Extract Business Rules
**As a** business analyst  
**I want** to see business rules extracted from code  
**So that** I can verify they're understood correctly  

**Acceptance Criteria:**
- List all identified business rules
- See rule logic in plain language
- Review confidence scores
- Add corrections or clarifications

---

### 8.2 Epic: Code Migration

#### US-003: Generate Modern Code
**As a** developer  
**I want** to generate Java code from COBOL  
**So that** I can have a modern implementation  

**Acceptance Criteria:**
- Select target language and framework
- Generate compilable code
- Include unit tests
- See mapping from legacy to modern

---

#### US-004: Validate Equivalence
**As a** QA engineer  
**I want** to verify migrated code behaves identically  
**So that** I can approve the migration  

**Acceptance Criteria:**
- Run automated equivalence tests
- See test results with pass/fail
- Investigate any differences
- Export test report

---

### 8.3 Use Case Scenarios

#### Scenario 1: Banking COBOL Migration

**Context:** Regional bank needs to migrate 2M lines of COBOL loan processing to Java.

**Flow:**
1. **Week 1-2:** Connect MigrationPilot to mainframe, index all COBOL programs
2. **Week 3-4:** Archeologist agent analyzes code, extracts 847 business rules
3. **Week 5:** SMEs review rules, add 23 corrections from institutional knowledge
4. **Week 6-8:** Architect agent designs microservices architecture (12 services)
5. **Week 9-16:** Builder agent generates Java code incrementally
6. **Week 17-20:** Validator runs 50,000 test cases, achieves 99.2% equivalence
7. **Week 21-24:** Address remaining discrepancies, achieve 100% equivalence
8. **Week 25-30:** Strangler fig deployment, gradual traffic migration

**Outcome:**
- 6-month timeline vs. typical 3-year manual project
- 70% cost reduction vs. traditional approach
- Business logic documented for first time in 30 years

---

#### Scenario 2: Insurance Fortran Migration

**Context:** Insurance company has Fortran actuarial models from 1980s.

**Challenge:**
- Original developers all retired
- No documentation
- Models certified by regulators—any change requires recertification

**Flow:**
1. Parse Fortran code, extract mathematical models
2. Generate formal specifications from code
3. Create Python implementations with NumPy
4. Property-based testing with 1M scenarios
5. Regulator review of specifications and test results
6. Parallel run for 6 months before cutover

**Outcome:**
- Actuarial models preserved exactly
- New platform 100x faster for simulations
- Documentation enables future modifications

---

## 9. User Experience & Design

### 9.1 Design Principles

1. **Expert-Friendly:** Designed for technical users, not dumbed down
2. **Evidence-Based:** Show reasoning, not just conclusions
3. **Reviewable:** Every AI decision can be examined and overridden
4. **Progress-Oriented:** Clear milestones and measurable advancement

### 9.2 Key Interfaces

#### Migration Dashboard

```
┌─────────────────────────────────────────────────────────────────────────────┐
│  MigrationPilot — Acme Bank Loan Processing Migration                       │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Project Status: Phase 3 - Code Generation                                  │
│  ████████████████████░░░░░░░░░░ 65% Complete                               │
│                                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐   │
│  │   247,832    │  │     847      │  │     612      │  │    99.2%     │   │
│  │ Lines Parsed │  │Rules Extracted│  │Files Generated│  │ Equivalence  │   │
│  │   ✓ Done     │  │  ✓ Reviewed  │  │  In Progress │  │  Validated   │   │
│  └──────────────┘  └──────────────┘  └──────────────┘  └──────────────┘   │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│  Module Progress                                                            │
│                                                                              │
│  ├── LOANPROC.CBL ███████████████████████████████████████████ 100% ✓      │
│  ├── INTEREST.CBL ███████████████████████████████████████████ 100% ✓      │
│  ├── PAYMENT.CBL  █████████████████████████████░░░░░░░░░░░░░ 72%          │
│  ├── ESCROW.CBL   ████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 28%          │
│  └── REPORTS.CBL  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 0%  Queued   │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│  Recent Activity                                                            │
│                                                                              │
│  10:23 AM  Generated PaymentService.java (342 lines, 12 tests)             │
│  10:15 AM  SME approved rule BR-234 "Escrow calculation"                   │
│  09:58 AM  Equivalence test passed for InterestCalculator (1,247 cases)    │
│  09:45 AM  Warning: Rule BR-189 has low confidence (72%) - needs review    │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

#### Business Rule Review

```
┌─────────────────────────────────────────────────────────────────────────────┐
│  Business Rule Review — BR-234: Escrow Calculation                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Source: ESCROW.CBL, lines 1456-1523                                        │
│  Confidence: 89% │ Status: Pending Review                                   │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│  Extracted Rule                                                             │
│  ─────────────────────────────────────────────────────────────────────────  │
│                                                                              │
│  Name: Monthly Escrow Calculation                                           │
│                                                                              │
│  Description:                                                               │
│  Calculates monthly escrow payment based on annual property tax and         │
│  insurance premiums, with a 2-month cushion as required by RESPA.           │
│                                                                              │
│  Formula:                                                                    │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │ monthly_escrow = (annual_tax + annual_insurance) / 12                │   │
│  │ cushion = monthly_escrow * 2                                         │   │
│  │ if account_age < 12 months:                                          │   │
│  │     initial_deposit = cushion                                        │   │
│  │ else:                                                                 │   │
│  │     initial_deposit = 0                                              │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
│  Edge Cases Detected:                                                       │
│  • Zero tax/insurance returns zero escrow                                   │
│  • Negative values trigger error condition                                  │
│  • Accounts over 30 years have special handling (line 1498)                │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│  Original COBOL                          │  Generated Java                  │
│  ─────────────────────────────────────── │ ──────────────────────────────── │
│  COMPUTE WS-MONTHLY-ESCROW =             │ public Money calculateEscrow(    │
│      (WS-ANNUAL-TAX +                    │     Money annualTax,             │
│       WS-ANNUAL-INS) / 12                │     Money annualInsurance,       │
│  COMPUTE WS-CUSHION =                    │     int accountAgeMonths) {      │
│      WS-MONTHLY-ESCROW * 2               │   var monthly = annualTax        │
│  IF WS-ACCT-AGE < 12                     │     .add(annualInsurance)        │
│      MOVE WS-CUSHION TO                  │     .divide(12);                 │
│          WS-INITIAL-DEP                  │   var cushion = monthly          │
│  ELSE                                    │     .multiply(2);                │
│      MOVE ZERO TO WS-INITIAL-DEP         │   return accountAgeMonths < 12   │
│  END-IF                                  │     ? cushion : Money.ZERO;      │
│                                          │ }                                │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│  SME Comments                                                               │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │ Add comment here...                                                  │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
│  [✓ Approve Rule]  [✎ Edit Rule]  [⚠ Flag for Discussion]  [✗ Reject]     │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 10. Success Metrics & KPIs

### 10.1 Product Metrics

| Metric | Definition | Target (Y1) |
|--------|------------|-------------|
| Lines Migrated | Total legacy lines processed | 10M |
| Rules Extracted | Business rules identified | 50,000 |
| Equivalence Rate | % passing equivalence tests | >98% |
| Time Savings | Vs. manual migration estimate | 70% |

### 10.2 Business Metrics

| Metric | Definition | Target (Y1) |
|--------|------------|-------------|
| Project Revenue | Completed engagement revenue | $3M |
| Platform ARR | Recurring platform licenses | $500K |
| Customers | Enterprises with active projects | 10 |
| NPS | Customer satisfaction | >50 |

### 10.3 Quality Metrics

| Metric | Definition | Target |
|--------|------------|--------|
| Rule Extraction Accuracy | SME-validated correctness | >95% |
| Code Compilation Rate | First-pass compilable | >90% |
| Test Generation Coverage | Code paths covered | >85% |
| False Negative Rate | Missed behavioral differences | <1% |

---

## 11. Competitive Analysis

### 11.1 Competitive Landscape

| Competitor | Approach | Strengths | Weaknesses |
|------------|----------|-----------|------------|
| **Accenture/IBM** | Consulting services | Relationships, scale | Expensive, slow |
| **Micro Focus** | COBOL modernization tools | Mature tooling | Limited AI |
| **Modern Systems** | Automated translation | Speed | Syntax-only, no semantics |
| **TSRI** | Automated migration | Proven at scale | Limited languages |

### 11.2 Differentiation

**MigrationPilot vs. Consulting:**
- 70% faster, 50% cheaper
- AI captures knowledge permanently
- Repeatable process

**MigrationPilot vs. Transpilers:**
- Understands semantics, not just syntax
- Generates idiomatic modern code
- Validates behavioral equivalence

---

## 12. Go-to-Market Strategy

### 12.1 GTM Model

**Primary:** Direct enterprise sales with services component

**Secondary:** Partner through consulting firms

### 12.2 Sales Motion

| Phase | Duration | Activities |
|-------|----------|------------|
| Discovery | 2-4 weeks | Assess legacy portfolio, identify pilot |
| Pilot | 4-8 weeks | Migrate single module, prove value |
| Expansion | Ongoing | Broader migration program |

### 12.3 Partnership Strategy

**Target Partners:**
- Accenture, Deloitte, IBM (delivery partners)
- Micro Focus, BMC (technology partners)
- AWS, Azure, GCP (cloud migration programs)

**Partner Model:**
- Referral fees: 15-20%
- Co-delivery: Revenue share
- Technology integration: Joint marketing

---

## 13. Monetization Strategy

### 13.1 Pricing Model

**Project-Based (Services):**

| Tier | Scope | Price Range |
|------|-------|-------------|
| Pilot | Single module, <50K lines | $50K-100K |
| Department | Multiple modules, <500K lines | $200K-500K |
| Enterprise | Full system, >500K lines | $500K-2M |

**Platform License (Annual):**

| Tier | Includes | Price |
|------|----------|-------|
| Standard | Cloud platform, 5 users | $50K/year |
| Professional | Private cloud, 20 users | $150K/year |
| Enterprise | On-premises, unlimited | $500K/year |

### 13.2 Revenue Projections

| Year | Project Revenue | Platform ARR | Total |
|------|-----------------|--------------|-------|
| Y1 | $3M | $500K | $3.5M |
| Y2 | $10M | $3M | $13M |
| Y3 | $20M | $10M | $30M |

---

## 14. Risks & Mitigations

### 14.1 Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| COBOL dialect complexity | High | Medium | Extensive grammar library, dialect detection |
| Undocumented behavior | High | High | Execution tracing, SME collaboration |
| LLM hallucination | Medium | High | Validation agent, equivalence testing |
| Scale limitations | Medium | Medium | Chunking strategies, incremental processing |

### 14.2 Business Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Long sales cycles | High | Medium | Pilot-first approach |
| Competition from incumbents | Medium | Medium | Differentiate on AI, speed |
| Customer readiness | Medium | Medium | Change management support |

---

## 15. Roadmap & Milestones

### 15.1 Development Timeline

| Phase | Duration | Focus |
|-------|----------|-------|
| MVP | M1-4 | COBOL→Java, core pipeline |
| V1.0 | M5-8 | Fortran, VB, testing suite |
| V1.5 | M9-12 | Enterprise features, partners |
| V2.0 | M13-18 | Multi-target, advanced AI |

### 15.2 Key Milestones

| Milestone | Date | Criteria |
|-----------|------|----------|
| First Pilot | M4 | Complete migration of customer module |
| Production Customer | M6 | Customer in production with migrated code |
| Partner Launch | M9 | First consulting partner signed |
| 10 Customers | M12 | 10 enterprises with active projects |

---

## 16. Dependencies & Constraints

### 16.1 Dependencies

| Dependency | Risk | Mitigation |
|------------|------|------------|
| Copilot SDK | Medium | Build abstraction layer |
| LLM quality for COBOL | Medium | Fine-tuning, prompt engineering |
| Legacy system access | High | On-prem deployment option |

### 16.2 Constraints

- Customer procurement cycles: 3-6 months typical
- Mainframe access requires security approvals
- Regulatory requirements for financial services

---

## 17. Appendices

### 17.1 Glossary

| Term | Definition |
|------|------------|
| **COBOL** | Common Business-Oriented Language; legacy programming language |
| **Copybook** | COBOL equivalent of header files; reusable data definitions |
| **Strangler Fig** | Migration pattern of gradually replacing legacy with new |
| **Behavioral Equivalence** | Modern system produces same outputs as legacy for all inputs |

### 17.2 References

1. COBOL Language Specification
2. Strangler Fig Pattern (Martin Fowler)
3. A16Z: "AI in Software Development" (2025)
4. Gartner: "Application Modernization Market Guide" (2025)

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-28 | Jose David Baena | Initial draft |

---

*This PRD is a living document and will be updated as product development progresses and market conditions evolve.*

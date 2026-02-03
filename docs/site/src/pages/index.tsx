import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import styles from './index.module.css';

function HeroSection() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className={styles.heroTitle}>
          {siteConfig.title}
        </Heading>
        <p className={styles.heroSubtitle}>
          Transform legacy COBOL, Fortran, and VB6 into modern code—without losing decades of business logic.
        </p>
        <div className={styles.heroButtons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/getting-started/quickstart">
            Get Started
          </Link>
          <Link
            className="button button--outline button--lg"
            to="https://github.com/migrationpilot/migrationpilot">
            View on GitHub
          </Link>
        </div>
        <div className={styles.installCommand}>
          <code>npm install -g @migrationpilot/cli</code>
          <button
            className={styles.copyButton}
            onClick={() => navigator.clipboard.writeText('npm install -g @migrationpilot/cli')}
            title="Copy to clipboard"
          >
            📋
          </button>
        </div>
      </div>
    </header>
  );
}

const features = [
  {
    title: 'AI-Powered Understanding',
    icon: '🔍',
    description: 'Four specialized AI agents analyze, design, build, and validate—understanding intent, not just syntax.',
  },
  {
    title: 'Business Logic Preserved',
    icon: '💼',
    description: 'Extract business rules with confidence scores. Human-in-the-loop review ensures nothing gets lost.',
  },
  {
    title: 'Clean, Idiomatic Code',
    icon: '✨',
    description: 'Generate code developers actually want to maintain—not unreadable transpiler output.',
  },
  {
    title: 'Proven Equivalence',
    icon: '✅',
    description: 'Automated testing proves the new code behaves exactly like the old. 100% confidence or fix it.',
  },
  {
    title: 'Incremental Migration',
    icon: '🔄',
    description: 'Strangler fig pattern support lets you migrate piece by piece with zero downtime.',
  },
  {
    title: 'Enterprise Ready',
    icon: '🏢',
    description: 'On-premises deployment, SOC 2 compliance, multi-tenancy, and air-gapped installation.',
  },
];

function FeatureGrid() {
  return (
    <section className={styles.features}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          Why MigrationPilot?
        </Heading>
        <div className={styles.featureGrid}>
          {features.map((feature, idx) => (
            <div key={idx} className={styles.featureCard}>
              <div className={styles.featureIcon}>{feature.icon}</div>
              <Heading as="h3">{feature.title}</Heading>
              <p>{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function CodeTransformDemo() {
  return (
    <section className={styles.demoSection}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          See It In Action
        </Heading>
        <p className={styles.sectionSubtitle}>
          Watch a 50-year-old COBOL program become modern Java in seconds.
        </p>
        <div className={styles.codeComparison}>
          <div className={styles.codeBlock}>
            <div className={styles.codeHeader}>
              <span className={styles.codeLabel}>Before: COBOL</span>
              <span className={styles.fileName}>CALCINT.cbl</span>
            </div>
            <pre className={styles.codeContent}>
{`       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL    PIC 9(10)V99.
       01 WS-RATE         PIC 9(2)V9(4).
       01 WS-YEARS        PIC 9(2).
       01 WS-INTEREST     PIC 9(10)V99.

       PROCEDURE DIVISION.
           COMPUTE WS-INTEREST =
               WS-PRINCIPAL * WS-RATE * WS-YEARS.
           STOP RUN.`}
            </pre>
          </div>
          <div className={styles.arrow}>→</div>
          <div className={styles.codeBlock}>
            <div className={styles.codeHeader}>
              <span className={styles.codeLabel}>After: Java</span>
              <span className={styles.fileName}>InterestCalculator.java</span>
            </div>
            <pre className={styles.codeContent}>
{`public class InterestCalculator {

    /**
     * Calculate simple interest.
     * Business Rule: BR-001
     */
    public BigDecimal calculateInterest(
            BigDecimal principal,
            BigDecimal rate,
            int years) {

        return principal
            .multiply(rate)
            .multiply(BigDecimal.valueOf(years))
            .setScale(2, RoundingMode.HALF_UP);
    }
}`}
            </pre>
          </div>
        </div>
      </div>
    </section>
  );
}

function ArchitectureDiagram() {
  return (
    <section className={styles.architectureSection}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          How It Works
        </Heading>
        <p className={styles.sectionSubtitle}>
          A multi-agent AI architecture where specialized agents collaborate through an orchestrator.
        </p>
        <div className={styles.architectureDiagram}>
          <div className={styles.pipelineFlow}>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>📥</div>
              <div className={styles.stepLabel}>Legacy Code</div>
              <div className={styles.stepLanguages}>COBOL • Fortran • VB6 • Java</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>🔍</div>
              <div className={styles.stepLabel}>Archeologist</div>
              <div className={styles.stepLanguages}>Extract Business Rules</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>👥</div>
              <div className={styles.stepLabel}>SME Review</div>
              <div className={styles.stepLanguages}>Human Validation</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>📐</div>
              <div className={styles.stepLabel}>Architect</div>
              <div className={styles.stepLanguages}>Design Modern System</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>🔨</div>
              <div className={styles.stepLabel}>Builder</div>
              <div className={styles.stepLanguages}>Generate Code</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>✅</div>
              <div className={styles.stepLabel}>Validator</div>
              <div className={styles.stepLanguages}>Prove Equivalence</div>
            </div>
            <div className={styles.pipelineArrow}>→</div>
            <div className={styles.pipelineStep}>
              <div className={styles.stepIcon}>📤</div>
              <div className={styles.stepLabel}>Modern Code</div>
              <div className={styles.stepLanguages}>Java • Python • TypeScript • Go</div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

function LanguageSupport() {
  const sourceLanguages = [
    { name: 'COBOL', dialects: 'COBOL-85, IBM, Micro Focus' },
    { name: 'Fortran', dialects: 'F77, F90, F95' },
    { name: 'Visual Basic', dialects: 'VB6, VBA' },
    { name: 'Legacy Java', dialects: 'J2EE, EJB 2.x, Struts' },
    { name: 'RPG', dialects: 'RPG II, III, ILE' },
    { name: 'PL/I', dialects: 'Enterprise PL/I' },
  ];

  const targetLanguages = [
    { name: 'Java', frameworks: 'Spring Boot, Quarkus' },
    { name: 'Python', frameworks: 'FastAPI, Django' },
    { name: 'TypeScript', frameworks: 'NestJS, Express' },
    { name: 'Go', frameworks: 'Gin, Standard Library' },
    { name: 'C#', frameworks: '.NET Core, ASP.NET' },
  ];

  return (
    <section className={styles.languageSection}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          From Legacy to Modern
        </Heading>
        <div className={styles.languageGrid}>
          <div className={styles.languageColumn}>
            <Heading as="h3">Source Languages</Heading>
            <ul className={styles.languageList}>
              {sourceLanguages.map((lang, idx) => (
                <li key={idx}>
                  <strong>{lang.name}</strong>
                  <span>{lang.dialects}</span>
                </li>
              ))}
            </ul>
          </div>
          <div className={styles.languageArrow}>→</div>
          <div className={styles.languageColumn}>
            <Heading as="h3">Target Languages</Heading>
            <ul className={styles.languageList}>
              {targetLanguages.map((lang, idx) => (
                <li key={idx}>
                  <strong>{lang.name}</strong>
                  <span>{lang.frameworks}</span>
                </li>
              ))}
            </ul>
          </div>
        </div>
      </div>
    </section>
  );
}

function CTASection() {
  return (
    <section className={styles.ctaSection}>
      <div className="container">
        <Heading as="h2">Ready to Modernize?</Heading>
        <p>Start your first migration in under 5 minutes.</p>
        <div className={styles.ctaButtons}>
          <Link
            className="button button--primary button--lg"
            to="/docs/getting-started/quickstart">
            Get Started Free
          </Link>
          <Link
            className="button button--secondary button--lg"
            to="/docs/intro">
            Read the Docs
          </Link>
        </div>
      </div>
    </section>
  );
}

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title="AI-Powered Legacy Code Modernization"
      description="Transform COBOL, Fortran, and VB6 into modern code while preserving decades of business logic.">
      <HeroSection />
      <main>
        <FeatureGrid />
        <CodeTransformDemo />
        <ArchitectureDiagram />
        <LanguageSupport />
        <CTASection />
      </main>
    </Layout>
  );
}

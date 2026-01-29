import Link from 'next/link';
import { ArrowRight, Code2, Shield, Zap, FileCode, CheckCircle } from 'lucide-react';

export default function Home() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-900 to-slate-800 text-white">
      {/* Header */}
      <header className="container mx-auto px-4 py-6">
        <nav className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <Code2 className="h-8 w-8 text-blue-400" />
            <span className="text-xl font-bold">MigrationPilot</span>
          </div>
          <div className="flex items-center gap-4">
            <Link href="/dashboard" className="text-slate-300 hover:text-white">
              Dashboard
            </Link>
            <Link
              href="/dashboard/new"
              className="bg-blue-600 hover:bg-blue-700 px-4 py-2 rounded-lg font-medium"
            >
              Start Migration
            </Link>
          </div>
        </nav>
      </header>

      {/* Hero */}
      <section className="container mx-auto px-4 py-20 text-center">
        <h1 className="text-5xl font-bold mb-6">
          Modernize Legacy Code with{' '}
          <span className="text-blue-400">AI-Powered Precision</span>
        </h1>
        <p className="text-xl text-slate-300 max-w-3xl mx-auto mb-8">
          Transform your COBOL, Fortran, and Visual Basic systems into modern,
          maintainable code while preserving decades of business logic.
        </p>
        <div className="flex items-center justify-center gap-4">
          <Link
            href="/dashboard/new"
            className="bg-blue-600 hover:bg-blue-700 px-6 py-3 rounded-lg font-medium text-lg flex items-center gap-2"
          >
            Start Free Trial <ArrowRight className="h-5 w-5" />
          </Link>
          <Link
            href="#demo"
            className="border border-slate-600 hover:border-slate-500 px-6 py-3 rounded-lg font-medium text-lg"
          >
            Watch Demo
          </Link>
        </div>
      </section>

      {/* Features */}
      <section className="container mx-auto px-4 py-20">
        <h2 className="text-3xl font-bold text-center mb-12">
          Why MigrationPilot?
        </h2>
        <div className="grid md:grid-cols-3 gap-8">
          <FeatureCard
            icon={<Zap className="h-8 w-8 text-yellow-400" />}
            title="70% Faster Migration"
            description="AI-powered analysis and code generation dramatically accelerates modernization projects."
          />
          <FeatureCard
            icon={<Shield className="h-8 w-8 text-green-400" />}
            title="99% Behavioral Equivalence"
            description="Automated testing ensures your business logic is preserved exactly."
          />
          <FeatureCard
            icon={<FileCode className="h-8 w-8 text-purple-400" />}
            title="Human-Readable Output"
            description="Generated code follows modern best practices and is easy to maintain."
          />
        </div>
      </section>

      {/* Supported Languages */}
      <section className="container mx-auto px-4 py-20">
        <h2 className="text-3xl font-bold text-center mb-12">
          Supported Languages
        </h2>
        <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6">
          <LanguageCard name="COBOL" versions="COBOL-85, IBM Enterprise, Micro Focus" />
          <LanguageCard name="Fortran" versions="F77, F90, F95" />
          <LanguageCard name="Visual Basic" versions="VB6, VBA" />
          <LanguageCard name="Legacy Java" versions="J2EE, EJB 2.x" />
        </div>
        <div className="text-center mt-8">
          <ArrowRight className="h-6 w-6 mx-auto text-slate-500 rotate-90 mb-4" />
          <p className="text-slate-400 mb-4">Transform to</p>
          <div className="flex justify-center gap-4 flex-wrap">
            {['Java', 'Python', 'TypeScript', 'Go', 'C#'].map((lang) => (
              <span
                key={lang}
                className="bg-slate-700 px-4 py-2 rounded-lg font-medium"
              >
                {lang}
              </span>
            ))}
          </div>
        </div>
      </section>

      {/* Process */}
      <section className="container mx-auto px-4 py-20">
        <h2 className="text-3xl font-bold text-center mb-12">How It Works</h2>
        <div className="grid md:grid-cols-4 gap-8">
          <ProcessStep
            number={1}
            title="Upload Code"
            description="Upload your legacy source files or connect to your repository."
          />
          <ProcessStep
            number={2}
            title="AI Analysis"
            description="Our AI agents analyze structure and extract business rules."
          />
          <ProcessStep
            number={3}
            title="Generate & Test"
            description="Modern code is generated and validated for equivalence."
          />
          <ProcessStep
            number={4}
            title="Deploy"
            description="Migrate incrementally with strangler fig pattern support."
          />
        </div>
      </section>

      {/* CTA */}
      <section className="container mx-auto px-4 py-20">
        <div className="bg-blue-600 rounded-2xl p-12 text-center">
          <h2 className="text-3xl font-bold mb-4">Ready to Modernize?</h2>
          <p className="text-blue-100 mb-8 max-w-2xl mx-auto">
            Start with a free pilot project. Upload a single module and see the
            magic of AI-powered migration.
          </p>
          <Link
            href="/dashboard/new"
            className="bg-white text-blue-600 hover:bg-blue-50 px-8 py-3 rounded-lg font-medium text-lg inline-flex items-center gap-2"
          >
            Start Free Pilot <ArrowRight className="h-5 w-5" />
          </Link>
        </div>
      </section>

      {/* Footer */}
      <footer className="container mx-auto px-4 py-8 border-t border-slate-700">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <Code2 className="h-6 w-6 text-blue-400" />
            <span className="font-bold">MigrationPilot</span>
          </div>
          <p className="text-slate-400">
            © 2026 MigrationPilot. All rights reserved.
          </p>
        </div>
      </footer>
    </div>
  );
}

function FeatureCard({
  icon,
  title,
  description,
}: {
  icon: React.ReactNode;
  title: string;
  description: string;
}) {
  return (
    <div className="bg-slate-800 rounded-xl p-6 border border-slate-700">
      <div className="mb-4">{icon}</div>
      <h3 className="text-xl font-semibold mb-2">{title}</h3>
      <p className="text-slate-400">{description}</p>
    </div>
  );
}

function LanguageCard({ name, versions }: { name: string; versions: string }) {
  return (
    <div className="bg-slate-800 rounded-xl p-6 border border-slate-700 text-center">
      <h3 className="text-xl font-semibold mb-2">{name}</h3>
      <p className="text-slate-400 text-sm">{versions}</p>
    </div>
  );
}

function ProcessStep({
  number,
  title,
  description,
}: {
  number: number;
  title: string;
  description: string;
}) {
  return (
    <div className="text-center">
      <div className="w-12 h-12 bg-blue-600 rounded-full flex items-center justify-center mx-auto mb-4 text-xl font-bold">
        {number}
      </div>
      <h3 className="text-lg font-semibold mb-2">{title}</h3>
      <p className="text-slate-400 text-sm">{description}</p>
    </div>
  );
}

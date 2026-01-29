'use client';

/**
 * Code Comparison Page
 * 
 * Side-by-side view of legacy and generated modern code with line mapping
 */

import { useState } from 'react';

interface CodeFile {
  name: string;
  language: string;
  content: string;
}

interface CodeMapping {
  legacyLines: number[];
  modernLines: number[];
  description: string;
}

const mockLegacyCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL    PIC 9(10)V99.
       01 WS-RATE         PIC 9(2)V9(4).
       01 WS-YEARS        PIC 9(2).
       01 WS-INTEREST     PIC 9(10)V99.
       01 WS-ACCT-TYPE    PIC X(10).
       
       PROCEDURE DIVISION.
       CALCULATE-INTEREST.
           IF WS-ACCT-TYPE = "SAVINGS"
               IF WS-PRINCIPAL > 10000
                   COMPUTE WS-RATE = WS-RATE + 0.005
               END-IF
           END-IF
           
           COMPUTE WS-INTEREST = 
               WS-PRINCIPAL * WS-RATE * WS-YEARS.
           
           STOP RUN.`;

const mockModernCode = `package com.bank.interest;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Interest Calculator Service
 * Migrated from CALCINT.cbl
 */
public class InterestCalculator {
    
    private static final BigDecimal SAVINGS_BONUS_RATE = 
        new BigDecimal("0.005");
    private static final BigDecimal SAVINGS_THRESHOLD = 
        new BigDecimal("10000");
    
    /**
     * Calculate interest based on account type and balance.
     * 
     * @param principal The principal amount
     * @param rate The base interest rate
     * @param years Number of years
     * @param accountType The account type
     * @return Calculated interest amount
     */
    public BigDecimal calculateInterest(
            BigDecimal principal,
            BigDecimal rate,
            int years,
            String accountType) {
        
        BigDecimal effectiveRate = rate;
        
        // Apply savings bonus for high balances
        if ("SAVINGS".equals(accountType)) {
            if (principal.compareTo(SAVINGS_THRESHOLD) > 0) {
                effectiveRate = rate.add(SAVINGS_BONUS_RATE);
            }
        }
        
        // Calculate interest
        return principal
            .multiply(effectiveRate)
            .multiply(BigDecimal.valueOf(years))
            .setScale(2, RoundingMode.HALF_UP);
    }
}`;

const mockMappings: CodeMapping[] = [
  {
    legacyLines: [6, 7, 8, 9, 10],
    modernLines: [11, 12, 13, 14],
    description: 'Data declarations → Class constants',
  },
  {
    legacyLines: [14, 15, 16, 17, 18],
    modernLines: [32, 33, 34, 35, 36],
    description: 'Savings rate calculation logic',
  },
  {
    legacyLines: [20, 21],
    modernLines: [39, 40, 41, 42],
    description: 'Interest computation',
  },
];

export default function CodePage({ params }: { params: { id: string } }) {
  const [selectedMapping, setSelectedMapping] = useState<number | null>(null);
  const [showMappings, setShowMappings] = useState(true);

  const legacyLines = mockLegacyCode.split('\n');
  const modernLines = mockModernCode.split('\n');

  const isLegacyHighlighted = (lineNum: number) =>
    selectedMapping !== null && mockMappings[selectedMapping]?.legacyLines.includes(lineNum);
  
  const isModernHighlighted = (lineNum: number) =>
    selectedMapping !== null && mockMappings[selectedMapping]?.modernLines.includes(lineNum);

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-[1800px] mx-auto px-4 py-8">
        {/* Header */}
        <div className="flex items-center justify-between mb-6">
          <div>
            <h1 className="text-3xl font-bold text-gray-900">Code Comparison</h1>
            <p className="text-gray-600 mt-1">
              CALCINT.cbl → InterestCalculator.java
            </p>
          </div>
          <div className="flex items-center gap-4">
            <label className="flex items-center gap-2">
              <input
                type="checkbox"
                checked={showMappings}
                onChange={(e) => setShowMappings(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Show Line Mappings</span>
            </label>
            <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
              Download Mapping Report
            </button>
          </div>
        </div>

        {/* Mapping Legend */}
        {showMappings && (
          <div className="bg-white rounded-lg shadow-sm p-4 mb-6">
            <h3 className="text-sm font-medium text-gray-700 mb-3">Line Mappings</h3>
            <div className="flex flex-wrap gap-2">
              {mockMappings.map((mapping, index) => (
                <button
                  key={index}
                  onClick={() => setSelectedMapping(selectedMapping === index ? null : index)}
                  className={`px-3 py-2 rounded-lg text-sm transition-colors ${
                    selectedMapping === index
                      ? 'bg-yellow-400 text-yellow-900'
                      : 'bg-gray-100 hover:bg-gray-200'
                  }`}
                >
                  <span className="font-mono">
                    L{mapping.legacyLines[0]}-{mapping.legacyLines[mapping.legacyLines.length - 1]}
                  </span>
                  {' → '}
                  <span className="font-mono">
                    M{mapping.modernLines[0]}-{mapping.modernLines[mapping.modernLines.length - 1]}
                  </span>
                  <span className="ml-2 text-gray-500">({mapping.description})</span>
                </button>
              ))}
              {selectedMapping !== null && (
                <button
                  onClick={() => setSelectedMapping(null)}
                  className="px-3 py-2 text-sm text-gray-500 hover:text-gray-700"
                >
                  Clear selection
                </button>
              )}
            </div>
          </div>
        )}

        {/* Code Viewers */}
        <div className="grid grid-cols-2 gap-6">
          {/* Legacy Code */}
          <div className="bg-white rounded-lg shadow-sm overflow-hidden">
            <div className="flex items-center justify-between px-4 py-3 bg-gray-100 border-b">
              <div className="flex items-center gap-2">
                <span className="w-3 h-3 rounded-full bg-yellow-500" />
                <span className="font-medium">CALCINT.cbl</span>
                <span className="text-sm text-gray-500">(COBOL)</span>
              </div>
              <span className="text-xs text-gray-500">{legacyLines.length} lines</span>
            </div>
            <div className="overflow-x-auto">
              <pre className="p-0 text-sm">
                <code>
                  {legacyLines.map((line, index) => {
                    const lineNum = index + 1;
                    const highlighted = isLegacyHighlighted(lineNum);
                    return (
                      <div
                        key={index}
                        className={`flex ${highlighted ? 'bg-yellow-200' : 'hover:bg-gray-50'}`}
                      >
                        <span className="select-none w-12 text-right pr-4 py-1 text-gray-400 bg-gray-50 border-r">
                          {lineNum}
                        </span>
                        <span className={`flex-1 pl-4 py-1 ${highlighted ? 'text-yellow-900' : 'text-gray-800'}`}>
                          {line || ' '}
                        </span>
                      </div>
                    );
                  })}
                </code>
              </pre>
            </div>
          </div>

          {/* Modern Code */}
          <div className="bg-white rounded-lg shadow-sm overflow-hidden">
            <div className="flex items-center justify-between px-4 py-3 bg-gray-100 border-b">
              <div className="flex items-center gap-2">
                <span className="w-3 h-3 rounded-full bg-green-500" />
                <span className="font-medium">InterestCalculator.java</span>
                <span className="text-sm text-gray-500">(Java)</span>
              </div>
              <span className="text-xs text-gray-500">{modernLines.length} lines</span>
            </div>
            <div className="overflow-x-auto">
              <pre className="p-0 text-sm">
                <code>
                  {modernLines.map((line, index) => {
                    const lineNum = index + 1;
                    const highlighted = isModernHighlighted(lineNum);
                    return (
                      <div
                        key={index}
                        className={`flex ${highlighted ? 'bg-yellow-200' : 'hover:bg-gray-50'}`}
                      >
                        <span className="select-none w-12 text-right pr-4 py-1 text-gray-400 bg-gray-50 border-r">
                          {lineNum}
                        </span>
                        <span className={`flex-1 pl-4 py-1 ${highlighted ? 'text-yellow-900' : 'text-gray-800'}`}>
                          {line || ' '}
                        </span>
                      </div>
                    );
                  })}
                </code>
              </pre>
            </div>
          </div>
        </div>

        {/* Mapping Details */}
        {selectedMapping !== null && (
          <div className="mt-6 bg-white rounded-lg shadow-sm p-6">
            <h3 className="font-semibold text-lg mb-4">
              Mapping Details: {mockMappings[selectedMapping]!.description}
            </h3>
            <div className="grid grid-cols-2 gap-6">
              <div>
                <h4 className="text-sm font-medium text-gray-700 mb-2">Legacy Code (Lines {mockMappings[selectedMapping]!.legacyLines.join(', ')})</h4>
                <pre className="bg-slate-900 text-slate-100 p-4 rounded-lg text-sm overflow-x-auto">
                  {mockMappings[selectedMapping]!.legacyLines
                    .map(ln => legacyLines[ln - 1])
                    .join('\n')}
                </pre>
              </div>
              <div>
                <h4 className="text-sm font-medium text-gray-700 mb-2">Modern Code (Lines {mockMappings[selectedMapping]!.modernLines.join(', ')})</h4>
                <pre className="bg-green-900 text-green-100 p-4 rounded-lg text-sm overflow-x-auto">
                  {mockMappings[selectedMapping]!.modernLines
                    .map(ln => modernLines[ln - 1])
                    .join('\n')}
                </pre>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

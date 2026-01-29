/**
 * Explainer Agent Tools
 * 
 * Tools available to the Code Explainer agent for analyzing and explaining code.
 */

import type { AgentTool } from '@migrationpilot/core';

export const explainerTools: AgentTool[] = [
  {
    name: 'find_code_section',
    description: 'Find a specific code section by name (paragraph, function, subroutine)',
    parameters: {
      sectionName: {
        type: 'string',
        description: 'Name of the section to find',
        required: true,
      },
      sectionType: {
        type: 'string',
        description: 'Type of section: paragraph, section, function, subroutine',
        required: false,
      },
    },
    execute: async (args) => {
      // This would typically search through parsed AST
      // For now, returns a placeholder indicating the tool was called
      return {
        found: true,
        sectionName: args.sectionName,
        sectionType: args.sectionType || 'paragraph',
        message: 'Section search executed - implementation depends on AST availability',
      };
    },
  },
  {
    name: 'find_data_references',
    description: 'Find all references to a data item/variable in the code',
    parameters: {
      dataItemName: {
        type: 'string',
        description: 'Name of the data item to find references for',
        required: true,
      },
    },
    execute: async (args) => {
      return {
        dataItemName: args.dataItemName,
        message: 'Reference search executed - implementation depends on AST availability',
      };
    },
  },
  {
    name: 'get_call_hierarchy',
    description: 'Get the call hierarchy for a procedure (what calls it and what it calls)',
    parameters: {
      procedureName: {
        type: 'string',
        description: 'Name of the procedure to analyze',
        required: true,
      },
    },
    execute: async (args) => {
      return {
        procedureName: args.procedureName,
        calledBy: [],
        calls: [],
        message: 'Call hierarchy search executed - implementation depends on AST availability',
      };
    },
  },
  {
    name: 'explain_picture_clause',
    description: 'Explain a COBOL PICTURE clause format',
    parameters: {
      picture: {
        type: 'string',
        description: 'The PICTURE clause to explain (e.g., "9(5)V99", "X(10)")',
        required: true,
      },
    },
    execute: async (args) => {
      const picture = args.picture as string;
      
      // Parse the picture clause
      let explanation = `PICTURE ${picture}:\n`;
      let totalLength = 0;

      // Simple parsing for common patterns
      const nineMatch = picture.match(/9\((\d+)\)/);
      const xMatch = picture.match(/X\((\d+)\)/);
      const vMatch = picture.includes('V');

      if (nineMatch && nineMatch[1]) {
        explanation += `- ${nineMatch[1]} numeric digits\n`;
        totalLength += parseInt(nineMatch[1], 10);
      }
      if (xMatch && xMatch[1]) {
        explanation += `- ${xMatch[1]} alphanumeric characters\n`;
        totalLength += parseInt(xMatch[1], 10);
      }
      if (vMatch) {
        explanation += '- Contains implied decimal point\n';
        const afterV = picture.split('V')[1];
        const decimalPlaces = (afterV?.match(/9/g) || []).length;
        if (decimalPlaces > 0) {
          explanation += `- ${decimalPlaces} decimal places\n`;
        }
      }

      return {
        picture,
        explanation,
        totalLength: totalLength || 'varies',
        dataType: nineMatch ? 'numeric' : xMatch ? 'alphanumeric' : 'mixed',
      };
    },
  },
  {
    name: 'search_comments',
    description: 'Search for comments in the code that might provide documentation',
    parameters: {
      searchTerm: {
        type: 'string',
        description: 'Term to search for in comments',
        required: false,
      },
    },
    execute: async (args) => {
      return {
        searchTerm: args.searchTerm || '*',
        message: 'Comment search executed - implementation depends on source availability',
      };
    },
  },
];

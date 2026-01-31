/**
 * Data Dictionary Template
 * 
 * Generates comprehensive documentation of all data structures
 */

import type { DocumentTemplate } from '../types.js';

export const dataDictionaryTemplate: DocumentTemplate = {
  type: 'data-dictionary',
  name: 'Data Dictionary',
  description: 'Complete reference of all data structures, variables, and their relationships',
  
  sections: [
    {
      id: 'introduction',
      title: 'Introduction',
      description: 'Overview of the data dictionary',
      level: 1,
      required: true,
      contentType: 'static',
      staticContent: `This data dictionary provides a comprehensive reference of all data structures, variables, and constants used in the system. Each entry includes:

- **Name**: The identifier used in the code
- **Type**: The data type and format
- **Description**: Business meaning and usage
- **Source**: Where the data originates
- **Constraints**: Validation rules and limits
- **Related Rules**: Business rules that use this data`,
    },
    {
      id: 'summary-statistics',
      title: 'Summary Statistics',
      description: 'Overview of data inventory',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'dataStatistics',
    },
    {
      id: 'data-structures',
      title: 'Data Structures',
      description: 'Grouped data items and records',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'dataStructures',
      subsections: [
        {
          id: 'input-structures',
          title: 'Input Structures',
          description: 'Data received from external sources',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'inputStructures',
        },
        {
          id: 'output-structures',
          title: 'Output Structures',
          description: 'Data sent to external destinations',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'outputStructures',
        },
        {
          id: 'working-storage',
          title: 'Working Storage',
          description: 'Internal processing variables',
          level: 2,
          required: true,
          contentType: 'dynamic',
          dynamicContentKey: 'workingStorage',
        },
      ],
    },
    {
      id: 'file-layouts',
      title: 'File Layouts',
      description: 'Record definitions for files',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'fileLayouts',
    },
    {
      id: 'database-tables',
      title: 'Database Tables',
      description: 'Table and column definitions',
      level: 1,
      required: false,
      contentType: 'dynamic',
      dynamicContentKey: 'databaseTables',
    },
    {
      id: 'constants',
      title: 'Constants and Literals',
      description: 'Fixed values used in the system',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'constants',
    },
    {
      id: 'enumerations',
      title: 'Enumerations and Code Values',
      description: 'Valid values for coded fields',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'enumerations',
    },
    {
      id: 'data-flows',
      title: 'Data Flows',
      description: 'How data moves through the system',
      level: 1,
      required: false,
      contentType: 'ai-generated',
      aiPrompt: 'Describe the main data flows in this system based on the data structures and business rules. Include input sources, transformations, and output destinations.',
    },
    {
      id: 'data-quality-rules',
      title: 'Data Quality Rules',
      description: 'Validation and integrity rules',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'dataQualityRules',
    },
    {
      id: 'modern-mapping',
      title: 'Modern Type Mapping',
      description: 'Recommended types in target language',
      level: 1,
      required: true,
      contentType: 'dynamic',
      dynamicContentKey: 'modernMapping',
    },
  ],
  
  variables: [
    { name: 'projectName', type: 'string', description: 'Name of the project', required: true },
    { name: 'sourceLanguage', type: 'string', description: 'Source language', required: true },
    { name: 'targetLanguage', type: 'string', description: 'Target language', required: true },
    { name: 'totalDataItems', type: 'number', description: 'Total number of data items', required: true },
    { name: 'totalStructures', type: 'number', description: 'Total number of data structures', required: true },
  ],
  
  outputFormats: ['markdown', 'html', 'pdf', 'confluence'],
};

export default dataDictionaryTemplate;

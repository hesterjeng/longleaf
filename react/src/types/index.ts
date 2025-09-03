// Central type definitions for the Longleaf dashboard

// OCaml variant type - can be string or array format
export type OCamlVariant = string | string[];

// OCaml Target type from the backend
export type OCamlTarget = ['Download'] | ['File', string];

// Parsed target for UI consumption
export interface ParsedTarget {
  type: 'Download' | 'File';
  file: string;
}

// Base CLI field types for better type safety
export interface CLIBooleanFields {
  stacktrace?: boolean;
  no_gui?: boolean;
  save_received?: boolean;
}

export interface CLINumericFields {
  start?: number;
  random_drop_chance?: number;
}

// Unknown fields that need proper typing when we understand them better
export interface CLIUnknownFields {
  save_to_file?: unknown;
  nowait_market_open?: unknown;
  print_tick_arg?: unknown;
  precompute_indicators_arg?: unknown;
  compare_preloaded?: unknown;
}

// CLI settings from the backend - now composed of smaller interfaces
export interface CLISettings extends CLIBooleanFields, CLINumericFields, CLIUnknownFields {
  runtype: OCamlVariant;
  strategy_arg: string;
  [key: string]: unknown; // For any additional fields
}

// Settings data structure from /settings endpoint
export interface ServerSettings {
  cli_vars: CLISettings;
  target: OCamlTarget;
}

// Server data structure that comes from various endpoints
export interface ServerData {
  status: string | null;
  settings: ServerSettings | null;
  dataFiles: string[] | null;
  strategies: string[] | null;
  symbols: string[] | null;
}

// Chart data structure for Plotly
export interface ChartTrace {
  x: (string | number)[];
  y: number[];
  type: string;
  mode: string;
  name: string;
  line?: {
    color: string;
    dash?: string;
    width?: number;
  };
  visible?: boolean;
  yaxis?: string;
  marker?: object;
  hovertext?: string[];
  hoverinfo?: string;
}

export interface ChartData {
  traces: ChartTrace[];
  layout?: {
    title?: string;
    xaxis?: { title?: string };
    yaxis?: { title?: string };
    [key: string]: unknown;
  };
  title?: string;
  symbol?: string;
}

// Base form fields
export interface FormCLIFields extends CLIBooleanFields, CLINumericFields, CLIUnknownFields {
  runtype: string;
  strategy_arg: string;
}

export interface FormTargetFields {
  target_type: 'Download' | 'File';
  target_file?: string;
}

// Form values for settings update (composed of base fields)
export interface SettingsFormValues extends FormCLIFields, FormTargetFields {
  [key: string]: unknown; // For any additional form fields
}

// Pure CLI data structure (simplified)
export interface CLIFormData extends FormCLIFields {}

// API response wrapper
export interface ApiResponse<T = unknown> {
  data: T;
  status: number;
  statusText: string;
}

// Error structure for consistent error handling
export interface APIError {
  response?: {
    status: number;
    data: string | object;
  };
  message: string;
}
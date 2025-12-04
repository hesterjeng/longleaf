// Utility functions for converting between React and OCaml yojson formats

import type { OCamlVariant, OCamlTarget, ParsedTarget, CLISettings, CLIFormData, APIError } from '../types';

/**
 * Convert OCaml variant array format to simple string
 * ["Backtest"] -> "Backtest"
 * ["Live"] -> "Live" 
 */
export const parseOCamlVariant = (variant: OCamlVariant): string => {
  if (Array.isArray(variant) && variant.length === 1) {
    return variant[0];
  }
  return variant as string;
};

/**
 * Convert simple string to OCaml variant array format
 * "Backtest" -> ["Backtest"]
 * "Live" -> ["Live"]
 */
export const toOCamlVariant = (value: string): string[] => {
  return [value];
};

/**
 * Parse OCaml Target.t format
 * ["Download"] -> { type: "Download", file: "" }
 * ["File", "path"] -> { type: "File", file: "path" }
 */
export const parseTarget = (target: OCamlTarget | null | undefined): ParsedTarget => {
  if (!target) {
    return { type: 'Download', file: '' };
  }
  if (Array.isArray(target)) {
    if (target.length === 1 && target[0] === 'Download') {
      return { type: 'Download', file: '' };
    } else if (target.length === 2 && target[0] === 'File') {
      return { type: 'File', file: target[1] };
    }
  }
  return { type: 'Download', file: '' };
};

/**
 * Convert target settings to OCaml Target.t format
 * { type: "Download", file: "" } -> ["Download"]
 * { type: "File", file: "path" } -> ["File", "path"]
 */
export const toOCamlTarget = (targetSettings: ParsedTarget): OCamlTarget => {
  if (targetSettings.type === 'Download') {
    return ['Download'];
  } else {
    return ['File', targetSettings.file];
  }
};

/**
 * Get display string for target
 * ["Download"] -> "Download"
 * ["File", "path"] -> "File: path"
 */
export const getTargetDisplay = (target: OCamlTarget | null): string => {
  if (!target) return 'None';
  
  if (target.length === 1 && target[0] === 'Download') {
    return 'Download';
  } else if (target.length === 2 && target[0] === 'File') {
    return `File: ${target[1]}`;
  }
  
  // Fallback case - shouldn't happen with well-formed OCamlTarget
  return (target as string[]).join(' ');
};

/**
 * Get active data file from target (null if Download)
 * ["Download"] -> null
 * ["File", "path"] -> "path"
 */
export const getActiveDataFile = (target: OCamlTarget | null): string | null => {
  if (target && target.length === 2 && target[0] === 'File') {
    return target[1];
  }
  return null;
};

/**
 * Convert CLI settings from OCaml format to React format
 */
export const parseOCamlCLI = (oclCLI: CLISettings) => {
  if (!oclCLI || typeof oclCLI !== 'object') {
    throw new Error(`parseOCamlCLI: expected object, got ${typeof oclCLI}: ${JSON.stringify(oclCLI)}`);
  }
  return {
    ...oclCLI,
    runtype: parseOCamlVariant(oclCLI.runtype)
  };
};

/**
 * Convert CLI settings from React format to OCaml format
 */
export const toOCamlCLI = (cliSettings: CLIFormData): CLISettings => {
  return {
    ...cliSettings,
    runtype: toOCamlVariant(cliSettings.runtype),
    // Ensure all required OCaml CLI fields are present with defaults
    stacktrace: cliSettings.stacktrace ?? false,
    no_gui: cliSettings.no_gui ?? false,
    save_received: cliSettings.save_received ?? false,
    save_to_file: cliSettings.save_to_file ?? false,
    nowait_market_open: cliSettings.nowait_market_open ?? false,
    print_tick_arg: cliSettings.print_tick_arg ?? false,
    precompute_indicators_arg: cliSettings.precompute_indicators_arg ?? false,
    compare_preloaded: cliSettings.compare_preloaded ?? false,
    // Ensure numeric fields are present with defaults
    start: cliSettings.start ?? 0,
    random_drop_chance: cliSettings.random_drop_chance ?? 0,
    slippage_pct: cliSettings.slippage_pct ?? 0.0,
  };
};

/**
 * Standard error message formatting
 */
export const formatError = (error: APIError, operation: string): string => {
  if (error.response?.status === 406) {
    return `Server rejected ${operation} (406): ${error.response.data}`;
  } else {
    return `Failed to ${operation}: ${error.message}`;
  }
};
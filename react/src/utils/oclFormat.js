// Utility functions for converting between React and OCaml yojson formats

/**
 * Convert OCaml variant array format to simple string
 * ["Backtest"] -> "Backtest"
 * ["Live"] -> "Live" 
 */
export const parseOCamlVariant = (variant) => {
  if (Array.isArray(variant) && variant.length === 1) {
    return variant[0];
  }
  return variant;
};

/**
 * Convert simple string to OCaml variant array format
 * "Backtest" -> ["Backtest"]
 * "Live" -> ["Live"]
 */
export const toOCamlVariant = (value) => {
  return [value];
};

/**
 * Parse OCaml Target.t format
 * ["Download"] -> { type: "Download", file: "" }
 * ["File", "path"] -> { type: "File", file: "path" }
 */
export const parseTarget = (target) => {
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
export const toOCamlTarget = (targetSettings) => {
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
export const getTargetDisplay = (target) => {
  if (Array.isArray(target)) {
    if (target.length === 1 && target[0] === 'Download') {
      return 'Download';
    } else if (target.length === 2 && target[0] === 'File') {
      return `File: ${target[1]}`;
    } else {
      return target.join(' ');
    }
  }
  return String(target || 'None');
};

/**
 * Get active data file from target (null if Download)
 * ["Download"] -> null
 * ["File", "path"] -> "path"
 */
export const getActiveDataFile = (target) => {
  if (Array.isArray(target) && target.length === 2 && target[0] === 'File') {
    return target[1];
  }
  return null;
};

/**
 * Convert CLI settings from OCaml format to React format
 */
export const parseOCamlCLI = (oclCLI) => {
  return {
    ...oclCLI,
    runtype: parseOCamlVariant(oclCLI.runtype)
  };
};

/**
 * Convert CLI settings from React format to OCaml format
 */
export const toOCamlCLI = (cliSettings) => {
  return {
    ...cliSettings,
    runtype: toOCamlVariant(cliSettings.runtype)
  };
};

/**
 * Standard error message formatting
 */
export const formatError = (error, operation) => {
  if (error.response?.status === 406) {
    return `Server rejected ${operation} (406): ${error.response.data}`;
  } else {
    return `Failed to ${operation}: ${error.message}`;
  }
};
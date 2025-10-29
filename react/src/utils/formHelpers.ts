// Form utilities to eliminate repetitive form handling patterns
import type { SettingsFormValues, CLIFormData, ParsedTarget } from '../types';

/**
 * Extracts CLI data from form values, removing target-specific fields
 * Eliminates manual field extraction in components
 */
export const extractCLIData = (formValues: SettingsFormValues): CLIFormData => {
  // Use destructuring to separate CLI fields from target fields
  const { target_type, target_file, ...cliFields } = formValues;
  return cliFields as CLIFormData;
};

/**
 * Extracts target data from form values
 * Provides consistent target data structure
 */
export const extractTargetData = (formValues: SettingsFormValues): ParsedTarget => {
  return {
    type: formValues.target_type || 'Download',
    file: formValues.target_file || ''
  };
};

/**
 * Creates default form values with proper types
 * Eliminates hardcoded defaults across components
 */
export const createDefaultFormValues = (
  overrides: Partial<SettingsFormValues> = {}
): SettingsFormValues => {
  const defaults: SettingsFormValues = {
    runtype: 'Backtest',
    strategy_arg: '',
    target_type: 'Download',
    target_file: '',
    stacktrace: false,
    no_gui: false,
    save_received: false,
    start: 0,
    random_drop_chance: 0,
    slippage_pct: 0.0
  };

  return { ...defaults, ...overrides };
};

/**
 * Validates form values and returns validation errors
 * Centralizes form validation logic
 */
export const validateFormValues = (values: SettingsFormValues): Record<string, string> => {
  const errors: Record<string, string> = {};

  if (!values.runtype) {
    errors.runtype = 'Run type is required';
  }

  if (!values.strategy_arg) {
    errors.strategy_arg = 'Strategy is required';
  }

  if (values.target_type === 'File' && !values.target_file) {
    errors.target_file = 'Target file is required when type is "File"';
  }

  if (values.start !== undefined && values.start < 0) {
    errors.start = 'Start value must be non-negative';
  }

  if (values.random_drop_chance !== undefined && 
      (values.random_drop_chance < 0 || values.random_drop_chance > 100)) {
    errors.random_drop_chance = 'Random drop chance must be between 0 and 100';
  }

  if (values.slippage_pct !== undefined &&
      (values.slippage_pct < 0 || values.slippage_pct > 1)) {
    errors.slippage_pct = 'Slippage factor must be between 0 and 1 (e.g., 0.01 for ±1% slippage)';
  }

  return errors;
};

/**
 * Checks if form has validation errors
 */
export const hasValidationErrors = (values: SettingsFormValues): boolean => {
  return Object.keys(validateFormValues(values)).length > 0;
};
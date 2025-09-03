// Common type definitions - eliminates duplication and simplifies types
import type { ServerData } from './index';

// Base props that all tab components share
export interface BaseTabProps {
  serverData: ServerData;
  refreshData: () => void;
  loading: boolean;
}

// Common server response wrapper
export interface ServerResponse<T = unknown> {
  data: T;
  status: number;
  message?: string;
}

// Simplified error handling
export interface AppError {
  message: string;
  code?: string | number;
  response?: {
    status: number;
    data: unknown;
  };
}

// Form field base type
export interface FormField<T = unknown> {
  name: string;
  label: string;
  value: T;
  required?: boolean;
  tooltip?: string;
}

// Generic list item for reusable components
export interface ListItem {
  key: string;
  label: string;
  value?: string | number;
  description?: string;
  isActive?: boolean;
  disabled?: boolean;
  icon?: string;
  color?: string;
}

// Status types for consistent status handling
export type StatusType = 'success' | 'error' | 'warning' | 'info' | 'processing';

export interface StatusInfo {
  type: StatusType;
  message: string;
  description?: string;
  timestamp?: Date;
}

// Re-export main types to maintain compatibility
export type { 
  ServerData,
  OCamlTarget,
  OCamlVariant,
  ParsedTarget,
  CLISettings,
  SettingsFormValues,
  CLIFormData,
  ChartData,
  ChartTrace,
  ApiResponse,
  APIError
} from './index';
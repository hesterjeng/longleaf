// Custom hook for server data management - eliminates duplication across tabs
import { useState, useCallback } from 'react';
import type { ServerData, APIError } from '../types';
import { formatError } from '../utils/oclFormat';

interface UseServerDataOptions {
  onError?: (error: string) => void;
  autoRefresh?: boolean;
  refreshInterval?: number;
}

interface UseServerDataReturn {
  loading: boolean;
  error: string | null;
  executeAction: <T>(
    action: () => Promise<T>, 
    options?: { 
      successMessage?: string;
      errorPrefix?: string;
      onSuccess?: (result: T) => void;
    }
  ) => Promise<void>;
}

/**
 * Centralized hook for handling common server operations
 * Eliminates repeated error handling and loading state management
 */
export const useServerData = (
  refreshData: () => void,
  options: UseServerDataOptions = {}
): UseServerDataReturn => {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const executeAction = useCallback(async <T>(
    action: () => Promise<T>,
    actionOptions: {
      successMessage?: string;
      errorPrefix?: string;
      onSuccess?: (result: T) => void;
    } = {}
  ) => {
    const { successMessage, errorPrefix = 'operation', onSuccess } = actionOptions;
    
    setLoading(true);
    setError(null);
    
    try {
      const result = await action();
      
      if (onSuccess) {
        onSuccess(result);
      }
      
      if (successMessage && typeof window !== 'undefined') {
        // Dynamic import to avoid SSR issues
        const { message } = await import('antd');
        message.success(successMessage);
      }
      
      refreshData();
    } catch (err) {
      const errorMessage = formatError(err as APIError, errorPrefix);
      setError(errorMessage);
      
      if (options.onError) {
        options.onError(errorMessage);
      }
      
      if (typeof window !== 'undefined') {
        const { message } = await import('antd');
        message.error(errorMessage);
      }
    } finally {
      setLoading(false);
    }
  }, [refreshData, options]);

  return {
    loading,
    error,
    executeAction
  };
};
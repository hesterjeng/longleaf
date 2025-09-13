// Custom hook for chart data fetching - eliminates duplication in ChartTab
import { useState, useCallback } from 'react';
import axios from 'axios';
import type { ChartData, APIError } from '../types';

interface UseChartDataReturn {
  chartData: ChartData | null;
  loading: boolean;
  error: string | null;
  selectedSymbol: string;
  fetchChartData: (symbol: string) => Promise<void>;
  setSelectedSymbol: (symbol: string) => void;
  clearError: () => void;
}

/**
 * Specialized hook for chart data management
 * Handles symbol selection, data fetching, and error states
 */
export const useChartData = (): UseChartDataReturn => {
  const [selectedSymbol, setSelectedSymbol] = useState<string>('');
  const [chartData, setChartData] = useState<ChartData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);

  const fetchChartData = useCallback(async (symbol: string) => {
    if (!symbol) return;

    setLoading(true);
    setError(null);
    setChartData(null);

    try {
      const response = await axios.get(`/data/${symbol.toUpperCase()}/json`, { 
        timeout: 10000 
      });
      setChartData(response.data);
      setSelectedSymbol(symbol);
    } catch (err) {
      const error = err as APIError;
      if (error.response?.status === 404) {
        setError(`Chart data not yet available for ${symbol}`);
      } else {
        setError(`Error fetching chart data for ${symbol}: ${error.message}`);
      }
    } finally {
      setLoading(false);
    }
  }, []);

  const clearError = useCallback(() => {
    setError(null);
  }, []);

  return {
    chartData,
    loading,
    error,
    selectedSymbol,
    fetchChartData,
    setSelectedSymbol,
    clearError
  };
};
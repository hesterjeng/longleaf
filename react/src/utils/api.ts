import axios, { Method, AxiosRequestConfig } from 'axios';
import type { CLISettings, OCamlTarget, ApiResponse } from '../types';

/**
 * Standard API call wrapper with error handling
 */
export const apiCall = async <T = unknown>(method: Method, url: string, data: unknown = null, timeout: number = 5000): Promise<ApiResponse<T>> => {
  const config: AxiosRequestConfig = {
    method,
    url,
    timeout,
    headers: { 'Content-Type': 'application/json' }
  };
  
  if (data) {
    config.data = JSON.stringify(data);
  }
  
  return axios(config);
};

/**
 * GET request helper
 */
export const apiGet = <T = unknown>(url: string, timeout: number = 5000): Promise<ApiResponse<T>> => {
  return apiCall<T>('GET', url, null, timeout);
};

/**
 * POST request helper  
 */
export const apiPost = <T = unknown>(url: string, data: unknown, timeout: number = 5000): Promise<ApiResponse<T>> => {
  return apiCall<T>('POST', url, data, timeout);
};

/**
 * Standard server status update
 */
export const updateServerStatus = (status: string) => {
  return apiPost('/set_status', [status]);
};

/**
 * Standard CLI settings update
 */
export const updateCLI = async (cliSettings: CLISettings): Promise<ApiResponse> => {
  console.log('Sending CLI data:', JSON.stringify(cliSettings, null, 2));
  try {
    const response = await apiPost('/set_cli', cliSettings);
    console.log('CLI response:', response);
    return response;
  } catch (error) {
    console.error('CLI error:', (error as any).response?.data || (error as any).message);
    throw error;
  }
};

/**
 * Standard target update
 */
export const updateTarget = async (targetData: OCamlTarget): Promise<ApiResponse> => {
  console.log('Sending target data:', JSON.stringify(targetData, null, 2));
  try {
    const response = await apiPost('/set_target', targetData);
    console.log('Target response:', response);
    return response;
  } catch (error) {
    console.error('Target error:', (error as any).response?.data || (error as any).message);
    throw error;
  }
};

/**
 * Execute strategy - uses longer timeout for potentially long-running backtests
 */
export const executeStrategy = async (): Promise<ApiResponse> => {
  console.log('Executing strategy...');
  try {
    // 60 second timeout for strategy execution (backtests can take time)
    const response = await apiGet('/execute', 60000);
    console.log('Execute response:', response);
    return response;
  } catch (error) {
    console.error('Execute error:', (error as any).response?.data || (error as any).message);
    throw error;
  }
};
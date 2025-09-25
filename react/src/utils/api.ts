import axios, { Method, AxiosRequestConfig } from 'axios';
import type { CLISettings, OCamlTarget, ApiResponse, APIError } from '../types';

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
 * POST request helper for endpoints that return plain text instead of JSON
 */
export const apiPostText = async (url: string, data: unknown, timeout: number = 5000): Promise<ApiResponse<string>> => {
  const config: AxiosRequestConfig = {
    method: 'POST',
    url,
    timeout,
    headers: { 'Content-Type': 'application/json' },
    responseType: 'text', // Expect plain text response
    data: JSON.stringify(data)
  };
  
  return axios(config);
};

/**
 * Standard server status update
 */
export const updateServerStatus = (status: string) => {
  return apiPostText('/set_status', [status]);
};

/**
 * Standard CLI settings update
 */
export const updateCLI = async (cliSettings: CLISettings): Promise<ApiResponse<string>> => {
  console.log('Sending CLI data:', JSON.stringify(cliSettings, null, 2));
  try {
    const response = await apiPostText('/set_cli', cliSettings);
    console.log('CLI response:', response);
    return response;
  } catch (error) {
    console.error('CLI error:', (error as APIError).response?.data || (error as APIError).message);
    throw error;
  }
};

/**
 * Standard target update
 */
export const updateTarget = async (targetData: OCamlTarget): Promise<ApiResponse<string>> => {
  console.log('Sending target data:', JSON.stringify(targetData, null, 2));
  try {
    const response = await apiPostText('/set_target', targetData);
    console.log('Target response:', response);
    return response;
  } catch (error) {
    console.error('Target error:', (error as APIError).response?.data || (error as APIError).message);
    throw error;
  }
};

/**
 * Execute strategy - server now responds immediately and runs strategy in background
 */
export const executeStrategy = async (): Promise<ApiResponse> => {
  console.log('📡 API: Starting strategy execution request...');
  try {
    console.log('📡 API: Making GET request to /execute...');
    // Normal timeout since server responds immediately
    const response = await apiGet('/execute', 60000);
    console.log('✅ API: Got response from /execute:', response);
    return response;
  } catch (error) {
    console.error('❌ API: Execute request failed:', error);
    console.error('❌ API: Error details:', (error as APIError).response?.data || (error as APIError).message);
    console.error('❌ API: Error code:', (error as Error).message);
    throw error;
  }
};
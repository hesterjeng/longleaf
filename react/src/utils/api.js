import axios from 'axios';

/**
 * Standard API call wrapper with error handling
 */
export const apiCall = async (method, url, data = null, timeout = 5000) => {
  const config = {
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
export const apiGet = (url, timeout = 5000) => {
  return apiCall('GET', url, null, timeout);
};

/**
 * POST request helper  
 */
export const apiPost = (url, data, timeout = 5000) => {
  return apiCall('POST', url, data, timeout);
};

/**
 * Standard server status update
 */
export const updateServerStatus = (status) => {
  return apiPost('/set_status', [status]);
};

/**
 * Standard CLI settings update
 */
export const updateCLI = async (cliSettings) => {
  console.log('Sending CLI data:', JSON.stringify(cliSettings, null, 2));
  try {
    const response = await apiPost('/set_cli', cliSettings);
    console.log('CLI response:', response);
    return response;
  } catch (error) {
    console.error('CLI error:', error.response?.data || error.message);
    throw error;
  }
};

/**
 * Standard target update
 */
export const updateTarget = async (targetData) => {
  console.log('Sending target data:', JSON.stringify(targetData, null, 2));
  try {
    const response = await apiPost('/set_target', targetData);
    console.log('Target response:', response);
    return response;
  } catch (error) {
    console.error('Target error:', error.response?.data || error.message);
    throw error;
  }
};

/**
 * Execute strategy
 */
export const executeStrategy = async () => {
  console.log('Executing strategy...');
  try {
    const response = await apiGet('/execute');
    console.log('Execute response:', response);
    return response;
  } catch (error) {
    console.error('Execute error:', error.response?.data || error.message);
    throw error;
  }
};
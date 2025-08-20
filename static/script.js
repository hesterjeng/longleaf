// Longleaf C&C Dashboard - Refactored
// ===================================

// Constants and Configuration
const CONFIG = {
  ENDPOINTS: {
    STATUS: '/status',
    SETTINGS: '/settings', 
    STRATEGIES: '/strategies',
    DATA: '/data',
    OPTIONS: '/options',
    SET_STRATEGY: '/set_strategy',
    SET_STATUS: '/set_status',
    SET_TARGET: '/set_target'
  },
  
  STATUS_OPTIONS: ['Ready', 'Started', 'Error'],
  
  MESSAGES: {
    LOADING: 'Loading...',
    TESTING: 'Testing endpoint...',
    SETTING: 'Setting...',
    SELECT_STRATEGY: 'Please select a strategy',
    SELECT_STATUS: 'Please select a status', 
    SELECT_TARGET: 'Please select a data file'
  }
};

// Utility Functions
// ================

const Utils = {
  // Generic API call handler
  async apiCall(endpoint, options = {}) {
    const config = {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' },
      ...options
    };
    
    const startTime = performance.now();
    const response = await fetch(endpoint, config);
    const duration = Math.round(performance.now() - startTime);
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }
    
    const contentType = response.headers.get('content-type') || 'unknown';
    
    let content;
    if (contentType.includes('application/json')) {
      content = await response.json();
    } else {
      content = await response.text();
    }
    
    return { content, contentType, duration, status: response.status };
  },

  // Create result state object
  createResultState() {
    return {
      loading: false,
      success: null,
      error: null
    };
  },

  // Create data state object  
  createDataState() {
    return {
      loading: false,
      data: null,
      error: null
    };
  },

  // Format API test result
  formatTestResult(method, endpoint, body, result, error) {
    const lines = [`${method} ${endpoint}`];
    
    if (body) {
      lines.push(`Body: ${JSON.stringify(body)}`);
    }
    
    if (error) {
      lines.push(`Error: ${error.message}`);
    } else {
      lines.push(
        `Status: ${result.status}`,
        `Content-Type: ${result.contentType}`,
        `Response Time: ${result.duration}ms`,
        '',
        typeof result.content === 'object' 
          ? JSON.stringify(result.content, null, 2) 
          : result.content
      );
    }
    
    return lines.join('\n');
  }
};

// CSS Styles
// ==========

const STYLES = `
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    margin: 0;
    padding: 20px;
    background: #f5f7fa;
  }
  
  .dashboard {
    max-width: 1200px;
    margin: 0 auto;
  }
  
  .header {
    background: white;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
  
  .grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 20px;
    margin-bottom: 20px;
  }
  
  .panel {
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    overflow: hidden;
  }
  
  .panel-header {
    background: #f8f9fa;
    padding: 15px 20px;
    border-bottom: 1px solid #e9ecef;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .panel-content {
    padding: 20px;
  }
  
  .btn {
    background: #007bff;
    color: white;
    border: none;
    padding: 8px 16px;
    border-radius: 4px;
    cursor: pointer;
    margin: 2px;
    font-size: 14px;
  }
  
  .btn:hover { background: #0056b3; }
  .btn:disabled { background: #6c757d; cursor: not-allowed; }
  .btn-sm { padding: 4px 8px; font-size: 12px; }
  .btn-success { background: #28a745; }
  .btn-success:hover { background: #1e7e34; }
  
  pre {
    background: #f8f9fa;
    padding: 15px;
    border-radius: 4px;
    overflow-x: auto;
    font-size: 12px;
    margin: 0;
    border: 1px solid #e9ecef;
  }
  
  .form-group {
    margin-bottom: 15px;
  }
  
  .form-group label {
    display: block;
    margin-bottom: 5px;
    font-weight: 500;
  }
  
  select, input {
    width: 100%;
    padding: 8px 12px;
    border: 1px solid #ced4da;
    border-radius: 4px;
    box-sizing: border-box;
  }
  
  .text-success { color: #28a745; }
  .text-danger { color: #dc3545; }
  .loading { color: #6c757d; font-style: italic; }
  
  .status-indicator {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    display: inline-block;
    margin-right: 8px;
    vertical-align: middle;
  }
  
  .status-connected { background: #28a745; }
  .status-disconnected { background: #dc3545; }
  
  .endpoint-buttons {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-bottom: 15px;
  }
  
  .status-display {
    font-size: 18px;
    font-weight: bold;
    margin-bottom: 10px;
  }
`;

// HTML Generation Functions
// ========================

const HTMLGen = {
  header() {
    return `
      <div class="header">
        <h1>
          <span class="status-indicator" :class="connected ? 'status-connected' : 'status-disconnected'"></span>
          Longleaf Trading Platform C&C
        </h1>
        <p>Command & Control Dashboard for algorithmic trading operations</p>
      </div>
    `;
  },

  dataPanel(id, title, dataKey) {
    return `
      <div class="panel">
        <div class="panel-header">
          <h3>${title}</h3>
          <button class="btn btn-sm" @click="fetch${dataKey}()">Refresh</button>
        </div>
        <div class="panel-content">
          <div x-show="${id}.loading" class="loading">${CONFIG.MESSAGES.LOADING}</div>
          <div x-show="${id}.error" class="text-danger" x-text="${id}.error"></div>
          <div x-show="${id}.data && !${id}.loading">
            ${id === 'status' ? this.statusDisplay() : this.dataDisplay(id)}
          </div>
        </div>
      </div>
    `;
  },

  statusDisplay() {
    return `
      <div x-show="status.data && typeof status.data === 'object'" class="text-success status-display">
        Current Status: <span x-text="status.data.status || 'Unknown'"></span>
      </div>
      <pre x-text="JSON.stringify(status.data, null, 2)"></pre>
    `;
  },

  dataDisplay(id) {
    if (id === 'strategies' || id === 'dataFiles') {
      return `
        <div x-text="\`\${${id}.data.length} ${id === 'strategies' ? 'strategies' : 'data files'} available\`" class="text-success"></div>
        <pre x-text="JSON.stringify(${id}.data, null, 2)"></pre>
      `;
    }
    return `<pre x-text="JSON.stringify(${id}.data, null, 2)"></pre>`;
  },

  controlPanel(title, selectId, options, actionFn, resultKey) {
    return `
      <div class="panel">
        <div class="panel-header">
          <h3>${title}</h3>
        </div>
        <div class="panel-content">
          <div class="form-group">
            <label>Select ${title.replace(' Control', '')}:</label>
            ${this.selectDropdown(selectId, options)}
          </div>
          <button class="btn btn-success" @click="${actionFn}()" :disabled="!${selectId} || ${resultKey}.loading">
            <span x-show="${resultKey}.loading">${CONFIG.MESSAGES.SETTING}</span>
            <span x-show="!${resultKey}.loading">Set ${title.replace(' Control', '')}</span>
          </button>
          <div x-show="${resultKey}.success" class="text-success" x-text="${resultKey}.success"></div>
          <div x-show="${resultKey}.error" class="text-danger" x-text="${resultKey}.error"></div>
        </div>
      </div>
    `;
  },

  selectDropdown(selectId, options) {
    if (options.static) {
      const opts = options.values.map(val => `<option value="${val}">${val}</option>`).join('');
      return `
        <select x-model="${selectId}">
          <option value="">-- Select ${options.placeholder} --</option>
          ${opts}
        </select>
      `;
    } else {
      return `
        <select x-model="${selectId}" :disabled="${options.dataSource}.loading || !${options.dataSource}.data">
          <option value="">-- Select ${options.placeholder} --</option>
          <template x-for="item in ${options.dataSource}.data" :key="item">
            <option :value="item" x-text="item"></option>
          </template>
        </select>
      `;
    }
  },

  apiTestingPanel() {
    return `
      <div class="panel">
        <div class="panel-header">
          <h3>API Endpoint Testing</h3>
        </div>
        <div class="panel-content">
          <div class="endpoint-buttons">
            <button class="btn btn-sm" @click="testEndpoint('GET', '${CONFIG.ENDPOINTS.STATUS}')">GET /status</button>
            <button class="btn btn-sm" @click="testEndpoint('GET', '${CONFIG.ENDPOINTS.SETTINGS}')">GET /settings</button>
            <button class="btn btn-sm" @click="testEndpoint('GET', '${CONFIG.ENDPOINTS.STRATEGIES}')">GET /strategies</button>
            <button class="btn btn-sm" @click="testEndpoint('GET', '${CONFIG.ENDPOINTS.DATA}')">GET /data</button>
            <button class="btn btn-sm" @click="testEndpoint('GET', '${CONFIG.ENDPOINTS.OPTIONS}')">GET /options</button>
            <button class="btn btn-sm" @click="testEndpoint('POST', '${CONFIG.ENDPOINTS.SET_STATUS}', 'Ready')">POST /set_status</button>
            <button class="btn btn-sm" @click="testEndpoint('POST', '${CONFIG.ENDPOINTS.SET_TARGET}', 'test.json')">POST /set_target</button>
          </div>
          <div x-show="apiTest.loading" class="loading">${CONFIG.MESSAGES.TESTING}</div>
          <div x-show="apiTest.result">
            <pre x-text="apiTest.result"></pre>
          </div>
        </div>
      </div>
    `;
  }
};

// Dashboard Creation
// =================

function injectStyles() {
  const style = document.createElement('style');
  style.textContent = STYLES;
  document.head.appendChild(style);
}

function createDashboard() {
  const app = document.getElementById('app');
  
  app.innerHTML = `
    <div class="dashboard" x-data="dashboard()" x-init="init()">
      ${HTMLGen.header()}
      
      <!-- Main Data Grid -->
      <div class="grid">
        ${HTMLGen.dataPanel('status', 'Server Status', 'Status')}
        ${HTMLGen.dataPanel('settings', 'Current Settings', 'Settings')}
        ${HTMLGen.dataPanel('strategies', 'Available Strategies', 'Strategies')}
        ${HTMLGen.dataPanel('dataFiles', 'Data Files', 'Data')}
      </div>

      <!-- Controls Grid -->
      <div class="grid">
        ${HTMLGen.controlPanel('Strategy Control', 'selectedStrategy', 
          { dataSource: 'strategies', placeholder: 'Strategy' }, 
          'setControl', 'strategyResult')}
        ${HTMLGen.controlPanel('Status Control', 'selectedStatus', 
          { static: true, values: CONFIG.STATUS_OPTIONS, placeholder: 'Status' }, 
          'setControl', 'statusResult')}
        ${HTMLGen.controlPanel('Target Control', 'selectedTarget', 
          { dataSource: 'dataFiles', placeholder: 'Data File' }, 
          'setControl', 'targetResult')}
      </div>

      ${HTMLGen.apiTestingPanel()}
    </div>
  `;
}

// Alpine.js Dashboard Logic
// ========================

function dashboard() {
  return {
    // Connection status
    connected: false,

    // Data states
    status: Utils.createDataState(),
    settings: Utils.createDataState(),
    strategies: Utils.createDataState(),
    dataFiles: Utils.createDataState(),

    // Control selections
    selectedStrategy: '',
    selectedStatus: '',
    selectedTarget: '',

    // Control results
    strategyResult: Utils.createResultState(),
    statusResult: Utils.createResultState(),
    targetResult: Utils.createResultState(),

    // API testing
    apiTest: {
      loading: false,
      result: null
    },

    // Initialization
    async init() {
      console.log('Initializing Longleaf C&C Dashboard...');
      await this.loadAllData();
    },

    async loadAllData() {
      await Promise.all([
        this.fetchStatus(),
        this.fetchSettings(),
        this.fetchStrategies(),
        this.fetchData()
      ]);
    },

    // Generic data fetching
    async fetchFromEndpoint(endpoint, stateKey) {
      this[stateKey].loading = true;
      this[stateKey].error = null;
      
      try {
        const result = await Utils.apiCall(endpoint);
        this[stateKey].data = result.content;
        this.connected = true;
      } catch (error) {
        console.error(`Error fetching ${endpoint}:`, error);
        this[stateKey].error = error.message;
        this.connected = false;
      } finally {
        this[stateKey].loading = false;
      }
    },

    // Specific fetch methods
    async fetchStatus() { await this.fetchFromEndpoint(CONFIG.ENDPOINTS.STATUS, 'status'); },
    async fetchSettings() { await this.fetchFromEndpoint(CONFIG.ENDPOINTS.SETTINGS, 'settings'); },
    async fetchStrategies() { await this.fetchFromEndpoint(CONFIG.ENDPOINTS.STRATEGIES, 'strategies'); },
    async fetchData() { await this.fetchFromEndpoint(CONFIG.ENDPOINTS.DATA, 'dataFiles'); },

    // Generic control setter
    async setControl() {
      // Determine which control based on what's selected
      let endpoint, value, resultKey, refreshFn, validationMsg;
      
      if (this.selectedStrategy) {
        endpoint = CONFIG.ENDPOINTS.SET_STRATEGY;
        value = this.selectedStrategy;
        resultKey = 'strategyResult';
        refreshFn = () => this.fetchSettings();
        validationMsg = CONFIG.MESSAGES.SELECT_STRATEGY;
      } else if (this.selectedStatus) {
        endpoint = CONFIG.ENDPOINTS.SET_STATUS;
        value = this.selectedStatus;
        resultKey = 'statusResult';
        refreshFn = () => this.fetchStatus();
        validationMsg = CONFIG.MESSAGES.SELECT_STATUS;
      } else if (this.selectedTarget) {
        endpoint = CONFIG.ENDPOINTS.SET_TARGET;
        value = this.selectedTarget;
        resultKey = 'targetResult';
        refreshFn = () => this.fetchSettings();
        validationMsg = CONFIG.MESSAGES.SELECT_TARGET;
      } else {
        return; // No control selected
      }

      await this.executeControlSet(endpoint, value, resultKey, refreshFn);
    },

    async executeControlSet(endpoint, value, resultKey, refreshFn) {
      this[resultKey].loading = true;
      this[resultKey].error = null;
      this[resultKey].success = null;

      try {
        const result = await Utils.apiCall(endpoint, {
          method: 'POST',
          body: JSON.stringify(value)
        });

        this[resultKey].success = typeof result.content === 'string' 
          ? result.content 
          : 'Success';
        
        await refreshFn();
        
      } catch (error) {
        console.error(`Error setting control:`, error);
        this[resultKey].error = error.message;
      } finally {
        this[resultKey].loading = false;
      }
    },

    // API endpoint testing
    async testEndpoint(method, endpoint, body = null) {
      this.apiTest.loading = true;
      this.apiTest.result = null;

      try {
        const options = method === 'POST' && body ? {
          method: 'POST',
          body: JSON.stringify(body)
        } : { method };

        const result = await Utils.apiCall(endpoint, options);
        this.apiTest.result = Utils.formatTestResult(method, endpoint, body, result);
        
      } catch (error) {
        this.apiTest.result = Utils.formatTestResult(method, endpoint, body, null, error);
      } finally {
        this.apiTest.loading = false;
      }
    }
  };
}

// Initialize Dashboard
// ===================

document.addEventListener('DOMContentLoaded', function() {
  injectStyles();
  createDashboard();
});
// Longleaf C&C Dashboard
// Initialize everything when DOM loads
document.addEventListener('DOMContentLoaded', function() {
  injectStyles();
  createDashboard();
});

// Inject CSS styles into the page
function injectStyles() {
  const style = document.createElement('style');
  style.textContent = `
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
    .btn:hover {
      background: #0056b3;
    }
    .btn:disabled {
      background: #6c757d;
      cursor: not-allowed;
    }
    .btn-sm {
      padding: 4px 8px;
      font-size: 12px;
    }
    .btn-success {
      background: #28a745;
    }
    .btn-success:hover {
      background: #1e7e34;
    }
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
    .text-success {
      color: #28a745;
    }
    .text-danger {
      color: #dc3545;
    }
    .loading {
      color: #6c757d;
      font-style: italic;
    }
    .status-indicator {
      width: 10px;
      height: 10px;
      border-radius: 50%;
      display: inline-block;
      margin-right: 8px;
      vertical-align: middle;
    }
    .status-connected {
      background: #28a745;
    }
    .status-disconnected {
      background: #dc3545;
    }
    .endpoint-buttons {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-bottom: 15px;
    }
  `;
  document.head.appendChild(style);
}

// Create the dashboard HTML and attach Alpine.js
function createDashboard() {
  const app = document.getElementById('app');
  
  app.innerHTML = `
    <div class="dashboard" x-data="dashboard()" x-init="init()">
      
      <!-- Header -->
      <div class="header">
        <h1>
          <span class="status-indicator" :class="connected ? 'status-connected' : 'status-disconnected'"></span>
          Longleaf Trading Platform C&C
        </h1>
        <p>Command & Control Dashboard for algorithmic trading operations</p>
      </div>

      <!-- Main Grid -->
      <div class="grid">
        
        <!-- Server Status -->
        <div class="panel">
          <div class="panel-header">
            <h3>Server Status</h3>
            <button class="btn btn-sm" @click="fetchStatus()">Refresh</button>
          </div>
          <div class="panel-content">
            <div x-show="status.loading" class="loading">Loading...</div>
            <div x-show="status.error" class="text-danger" x-text="status.error"></div>
            <div x-show="status.data && !status.loading">
              <pre x-text="status.data"></pre>
            </div>
          </div>
        </div>

        <!-- Current Settings -->
        <div class="panel">
          <div class="panel-header">
            <h3>Current Settings</h3>
            <button class="btn btn-sm" @click="fetchSettings()">Refresh</button>
          </div>
          <div class="panel-content">
            <div x-show="settings.loading" class="loading">Loading...</div>
            <div x-show="settings.error" class="text-danger" x-text="settings.error"></div>
            <div x-show="settings.data && !settings.loading">
              <pre x-text="JSON.stringify(settings.data, null, 2)"></pre>
            </div>
          </div>
        </div>

        <!-- Available Strategies -->
        <div class="panel">
          <div class="panel-header">
            <h3>Available Strategies</h3>
            <button class="btn btn-sm" @click="fetchStrategies()">Refresh</button>
          </div>
          <div class="panel-content">
            <div x-show="strategies.loading" class="loading">Loading...</div>
            <div x-show="strategies.error" class="text-danger" x-text="strategies.error"></div>
            <div x-show="strategies.data && !strategies.loading">
              <div x-text="\`\${strategies.data.length} strategies available\`" class="text-success"></div>
              <pre x-text="JSON.stringify(strategies.data, null, 2)"></pre>
            </div>
          </div>
        </div>

        <!-- Data Files -->
        <div class="panel">
          <div class="panel-header">
            <h3>Data Files</h3>
            <button class="btn btn-sm" @click="fetchData()">Refresh</button>
          </div>
          <div class="panel-content">
            <div x-show="dataFiles.loading" class="loading">Loading...</div>
            <div x-show="dataFiles.error" class="text-danger" x-text="dataFiles.error"></div>
            <div x-show="dataFiles.data && !dataFiles.loading">
              <div x-text="\`\${dataFiles.data.length} data files found\`" class="text-success"></div>
              <pre x-text="JSON.stringify(dataFiles.data, null, 2)"></pre>
            </div>
          </div>
        </div>

      </div>

      <!-- Strategy Control -->
      <div class="panel">
        <div class="panel-header">
          <h3>Strategy Control</h3>
        </div>
        <div class="panel-content">
          <div class="form-group">
            <label>Select Strategy:</label>
            <select x-model="selectedStrategy" :disabled="strategies.loading || !strategies.data">
              <option value="">-- Select Strategy --</option>
              <template x-for="strategy in strategies.data" :key="strategy">
                <option :value="strategy" x-text="strategy"></option>
              </template>
            </select>
          </div>
          <button class="btn btn-success" @click="setStrategy()" :disabled="!selectedStrategy || strategyResult.loading">
            <span x-show="strategyResult.loading">Setting...</span>
            <span x-show="!strategyResult.loading">Set Strategy</span>
          </button>
          <div x-show="strategyResult.success" class="text-success" x-text="strategyResult.success"></div>
          <div x-show="strategyResult.error" class="text-danger" x-text="strategyResult.error"></div>
        </div>
      </div>

      <!-- API Testing -->
      <div class="panel">
        <div class="panel-header">
          <h3>API Endpoint Testing</h3>
        </div>
        <div class="panel-content">
          <div class="endpoint-buttons">
            <button class="btn btn-sm" @click="testEndpoint('/status')">GET /status</button>
            <button class="btn btn-sm" @click="testEndpoint('/settings')">GET /settings</button>
            <button class="btn btn-sm" @click="testEndpoint('/strategies')">GET /strategies</button>
            <button class="btn btn-sm" @click="testEndpoint('/data')">GET /data</button>
            <button class="btn btn-sm" @click="testEndpoint('/options')">GET /options</button>
          </div>
          <div x-show="apiTest.loading" class="loading">Testing endpoint...</div>
          <div x-show="apiTest.result">
            <pre x-text="apiTest.result"></pre>
          </div>
        </div>
      </div>

    </div>
  `;
}

// Alpine.js Dashboard Data
function dashboard() {
  return {
    // Connection status
    connected: false,

    // Data states for each endpoint
    status: {
      loading: false,
      data: null,
      error: null
    },
    
    settings: {
      loading: false,
      data: null,
      error: null
    },
    
    strategies: {
      loading: false,
      data: null,
      error: null
    },
    
    dataFiles: {
      loading: false,
      data: null,
      error: null
    },

    // Strategy control
    selectedStrategy: '',
    strategyResult: {
      loading: false,
      success: null,
      error: null
    },

    // API testing
    apiTest: {
      loading: false,
      result: null
    },

    // Initialize dashboard
    async init() {
      console.log('Initializing Longleaf C&C Dashboard...');
      await this.loadAllData();
    },

    // Load all data on startup
    async loadAllData() {
      await Promise.all([
        this.fetchStatus(),
        this.fetchSettings(),
        this.fetchStrategies(),
        this.fetchData()
      ]);
    },

    // Generic fetch helper
    async fetchEndpoint(endpoint, stateKey, isJson = true) {
      this[stateKey].loading = true;
      this[stateKey].error = null;
      
      try {
        const response = await fetch(endpoint);
        
        if (!response.ok) {
          throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }

        if (isJson) {
          this[stateKey].data = await response.json();
        } else {
          this[stateKey].data = await response.text();
        }
        
        this.connected = true;
      } catch (error) {
        console.error(`Error fetching ${endpoint}:`, error);
        this[stateKey].error = error.message;
        this.connected = false;
      } finally {
        this[stateKey].loading = false;
      }
    },

    // Fetch server status
    async fetchStatus() {
      await this.fetchEndpoint('/status', 'status', false);
    },

    // Fetch current settings
    async fetchSettings() {
      await this.fetchEndpoint('/settings', 'settings', true);
    },

    // Fetch available strategies
    async fetchStrategies() {
      await this.fetchEndpoint('/strategies', 'strategies', true);
    },

    // Fetch data files
    async fetchData() {
      await this.fetchEndpoint('/data', 'dataFiles', true);
    },

    // Set strategy via POST
    async setStrategy() {
      if (!this.selectedStrategy) {
        this.strategyResult.error = 'Please select a strategy';
        return;
      }

      this.strategyResult.loading = true;
      this.strategyResult.error = null;
      this.strategyResult.success = null;

      try {
        const response = await fetch('/set_strategy', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(this.selectedStrategy)
        });

        if (!response.ok) {
          throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }

        const result = await response.text();
        this.strategyResult.success = result;
        
        // Refresh settings to show the update
        await this.fetchSettings();
        
      } catch (error) {
        console.error('Error setting strategy:', error);
        this.strategyResult.error = error.message;
      } finally {
        this.strategyResult.loading = false;
      }
    },

    // Test any endpoint
    async testEndpoint(endpoint) {
      this.apiTest.loading = true;
      this.apiTest.result = null;

      try {
        const startTime = performance.now();
        const response = await fetch(endpoint);
        const endTime = performance.now();
        const duration = Math.round(endTime - startTime);

        const contentType = response.headers.get('content-type') || 'unknown';
        
        let content;
        if (contentType.includes('application/json')) {
          content = await response.json();
          content = JSON.stringify(content, null, 2);
        } else {
          content = await response.text();
        }

        this.apiTest.result = [
          `GET ${endpoint}`,
          `Status: ${response.status} ${response.statusText}`,
          `Content-Type: ${contentType}`,
          `Response Time: ${duration}ms`,
          ``,
          content
        ].join('\n');

      } catch (error) {
        this.apiTest.result = [
          `GET ${endpoint}`,
          `Error: ${error.message}`
        ].join('\n');
      } finally {
        this.apiTest.loading = false;
      }
    }
  }
}

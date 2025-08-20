// Longleaf C&C Dashboard - Alpine.js Data
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
    },

    // Clear strategy result messages after a delay
    clearStrategyResult() {
      setTimeout(() => {
        this.strategyResult.success = null;
        this.strategyResult.error = null;
      }, 5000);
    }
  }
}

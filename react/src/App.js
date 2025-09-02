import React, { useState, useEffect, useCallback } from 'react';
import axios from 'axios';
import Sidebar from './components/Sidebar';
import OverviewTab from './components/OverviewTab';
import ChartTab from './components/ChartTab';
import ControlTab from './components/ControlTab';
import DataTab from './components/DataTab';
import StrategiesTab from './components/StrategiesTab';

const App = () => {
  const [serverUrl, setServerUrl] = useState('http://localhost:8080');
  const [serverOnline, setServerOnline] = useState(false);
  const [activeTab, setActiveTab] = useState('overview');
  const [serverData, setServerData] = useState({
    status: null,
    settings: null,
    dataFiles: null,
    strategies: null,
    symbols: null
  });
  const [loading, setLoading] = useState(false);
  const [lastUpdate, setLastUpdate] = useState(new Date());

  const fetchAllData = useCallback(async () => {
    if (!serverOnline) return;
    
    setLoading(true);
    try {
      const [statusRes, settingsRes, dataRes, strategiesRes] = await Promise.all([
        axios.get('/status', { timeout: 5000 }).catch(() => null),
        axios.get('/settings', { timeout: 5000 }).catch(() => null),
        axios.get('/data', { timeout: 5000 }).catch(() => null),
        axios.get('/strategies', { timeout: 5000 }).catch(() => null),
      ]);

      let symbolsData = [];
      try {
        const symbolsRes = await axios.get('/symbols', { timeout: 5000 });
        symbolsData = Array.isArray(symbolsRes.data) ? symbolsRes.data : [];
      } catch (error) {
        // Symbols endpoint may not be implemented
        symbolsData = [];
      }

      setServerData({
        status: statusRes?.data || null,
        settings: settingsRes?.data || null,
        dataFiles: dataRes?.data || null,
        strategies: strategiesRes?.data || null,
        symbols: symbolsData
      });
      setLastUpdate(new Date());
    } catch (error) {
      console.error('Error fetching data:', error);
    } finally {
      setLoading(false);
    }
  }, [serverOnline]);

  const checkServerConnection = useCallback(async () => {
    try {
      const response = await axios.get('/status', { timeout: 2000 });
      if (response.status === 200) {
        setServerOnline(true);
        fetchAllData();
      } else {
        setServerOnline(false);
      }
    } catch (error) {
      setServerOnline(false);
    }
  }, [fetchAllData]);

  // Check server connection
  useEffect(() => {
    checkServerConnection();
  }, [checkServerConnection]);

  const refreshData = useCallback(() => {
    fetchAllData();
  }, [fetchAllData]);

  const tabs = [
    { id: 'overview', label: '📊 Overview' },
    { id: 'charts', label: '📈 Charts' },
    { id: 'control', label: '🎛️ Control' },
    { id: 'data', label: '📁 Data Files' },
    { id: 'strategies', label: '🧠 Strategies' }
  ];

  const renderTabContent = () => {
    const commonProps = {
      serverData,
      refreshData,
      loading
    };

    switch (activeTab) {
      case 'overview':
        return <OverviewTab {...commonProps} lastUpdate={lastUpdate} />;
      case 'charts':
        return <ChartTab {...commonProps} />;
      case 'control':
        return <ControlTab {...commonProps} />;
      case 'data':
        return <DataTab {...commonProps} />;
      case 'strategies':
        return <StrategiesTab {...commonProps} />;
      default:
        return <OverviewTab {...commonProps} lastUpdate={lastUpdate} />;
    }
  };

  return (
    <div className="app">
      <Sidebar
        serverUrl={serverUrl}
        setServerUrl={setServerUrl}
        serverOnline={serverOnline}
        checkServerConnection={checkServerConnection}
      />
      
      <div className="main-content">
        <div className="header">
          <h1>🍃 Longleaf Control Dashboard</h1>
          <p>Algorithmic Trading Platform Control Interface</p>
        </div>

        <div className="tabs">
          {tabs.map(tab => (
            <button
              key={tab.id}
              className={`tab ${activeTab === tab.id ? 'active' : ''}`}
              onClick={() => setActiveTab(tab.id)}
            >
              {tab.label}
            </button>
          ))}
        </div>

        <div className="tab-content">
          {serverOnline ? (
            renderTabContent()
          ) : (
            <div className="card">
              <div className="alert alert-danger">
                <strong>🔴 Cannot connect to server.</strong> Please check:
                <ul style={{ marginTop: '10px', paddingLeft: '20px' }}>
                  <li>Server is running: <code>longleaf_server</code></li>
                  <li>Correct URL (default: http://localhost:8080)</li>
                  <li>Network connectivity</li>
                  <li>Firewall settings</li>
                </ul>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default App;
import React, { useState, useEffect, useCallback } from 'react';
import axios from 'axios';
import { Layout, Tabs, Alert } from 'antd';
import type { TabsProps } from 'antd';
import OverviewTab from './components/OverviewTab';
import ChartTab from './components/ChartTab';
import DataTab from './components/DataTab';
import StrategiesTab from './components/StrategiesTab';
import type { ServerData } from './types';
import { API_CONFIG } from './utils/constants';

const { Content } = Layout;

const App: React.FC = () => {
  const [serverUrl, setServerUrl] = useState<string>(API_CONFIG.baseURL);
  const [serverOnline, setServerOnline] = useState<boolean>(false);
  const [serverData, setServerData] = useState<ServerData>({
    status: null,
    settings: null,
    dataFiles: null,
    strategies: null,
    symbols: null
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [lastUpdate, setLastUpdate] = useState<Date>(new Date());

  const fetchAllData = useCallback(async () => {
    if (!serverOnline) return;

    setLoading(true);
    console.log('[API-POLL] 🔄 Starting fetchAllData...');
    try {
      console.log('[API-POLL] 📡 Fetching: /status, /settings, /data, /strategies');
      const [statusRes, settingsRes, dataRes, strategiesRes] = await Promise.all([
        axios.get('/status', { timeout: 5000 })
          .then(res => { console.log('[API-POLL] ✅ /status success'); return res; })
          .catch(err => { console.log('[API-POLL] ❌ /status failed:', err.message); return null; }),
        axios.get('/settings', { timeout: 5000 })
          .then(res => { console.log('[API-POLL] ✅ /settings success'); return res; })
          .catch(err => { console.log('[API-POLL] ❌ /settings failed:', err.message); return null; }),
        axios.get('/data', { timeout: 5000 })
          .then(res => { console.log('[API-POLL] ✅ /data success'); return res; })
          .catch(err => { console.log('[API-POLL] ❌ /data failed:', err.message); return null; }),
        axios.get('/strategies', { timeout: 5000 })
          .then(res => { console.log('[API-POLL] ✅ /strategies success'); return res; })
          .catch(err => { console.log('[API-POLL] ❌ /strategies failed:', err.message); return null; }),
      ]);

      let symbolsData = [];
      try {
        console.log('[API-POLL] 📡 Fetching: /symbols');
        const symbolsRes = await axios.get('/symbols', { timeout: 5000 });
        console.log('[API-POLL] ✅ /symbols success');
        symbolsData = Array.isArray(symbolsRes.data) ? symbolsRes.data : [];
      } catch (error) {
        console.log('[API-POLL] ❌ /symbols failed:', (error as Error).message);
        symbolsData = [];
      }

      console.log('[API-POLL] Raw responses:');
      console.log('[API-POLL]   statusRes?.data:', JSON.stringify(statusRes?.data));
      console.log('[API-POLL]   settingsRes?.data:', JSON.stringify(settingsRes?.data)?.substring(0, 100));
      console.log('[API-POLL]   dataRes?.data:', JSON.stringify(dataRes?.data));
      console.log('[API-POLL]   strategiesRes?.data:', JSON.stringify(strategiesRes?.data)?.substring(0, 100));

      setServerData({
        status: statusRes?.data || null,
        settings: settingsRes?.data || null,
        dataFiles: dataRes?.data || null,
        strategies: strategiesRes?.data || null,
        symbols: symbolsData
      });
      setLastUpdate(new Date());
      console.log('[API-POLL] ✅ fetchAllData completed successfully');
    } catch (error) {
      console.error('[API-POLL] ❌ Error in fetchAllData:', error);
    } finally {
      setLoading(false);
    }
  }, [serverOnline]);

  const checkServerConnection = useCallback(async () => {
    try {
      console.log('[API-CONNECT] 🔌 Checking server connection...');
      const response = await axios.get('/status', { timeout: 2000 });
      if (response.status === 200) {
        console.log('[API-CONNECT] ✅ Server is online');
        setServerOnline(true);
        fetchAllData();
      } else {
        console.log('[API-CONNECT] ⚠️ Server returned non-200 status:', response.status);
        setServerOnline(false);
      }
    } catch (error) {
      console.log('[API-CONNECT] ❌ Server connection failed:', (error as Error).message);
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

  const tabItems: TabsProps['items'] = [
    {
      key: 'overview',
      label: 'Overview & Control',
      children: <OverviewTab 
        serverData={serverData} 
        refreshData={refreshData} 
        loading={loading} 
        lastUpdate={lastUpdate}
        serverUrl={serverUrl}
        setServerUrl={setServerUrl}
        serverOnline={serverOnline}
        checkServerConnection={checkServerConnection}
      />
    },
    {
      key: 'charts', 
      label: 'Charts',
      children: <ChartTab serverData={serverData} refreshData={refreshData} loading={loading} />
    },
    {
      key: 'data',
      label: 'Data Files',
      children: <DataTab serverData={serverData} refreshData={refreshData} loading={loading} />
    },
    {
      key: 'strategies',
      label: 'Strategies',
      children: <StrategiesTab serverData={serverData} refreshData={refreshData} loading={loading} />
    }
  ];


  return (
    <Layout>
      <Content>
          {serverOnline ? (
            <Tabs 
              items={tabItems} 
              defaultActiveKey="overview"
              size="large"
            />
          ) : (
            <Alert
              type="error"
              showIcon
              message="Cannot connect to server"
              description={
                <div>
                  <p>Please check:</p>
                  <ul>
                    <li>Server is running: <code>longleaf_server</code></li>
                    <li>Correct URL (configured via LONGLEAF_PORT env var)</li>
                    <li>Network connectivity</li>
                    <li>Firewall settings</li>
                  </ul>
                </div>
              }
            />
          )}
      </Content>
    </Layout>
  );
};

export default App;
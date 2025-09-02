import React, { useState, useEffect, useCallback } from 'react';
import axios from 'axios';
import { Layout, Tabs, Alert, Typography } from 'antd';
import Sidebar from './components/Sidebar';
import OverviewTab from './components/OverviewTab';
import ChartTab from './components/ChartTab';
import DataTab from './components/DataTab';
import StrategiesTab from './components/StrategiesTab';

const { Header, Sider, Content } = Layout;
const { Title, Text } = Typography;

const App = () => {
  const [serverUrl, setServerUrl] = useState('http://localhost:8080');
  const [serverOnline, setServerOnline] = useState(false);
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

  const tabItems = [
    {
      key: 'overview',
      label: 'Overview & Control',
      children: <OverviewTab serverData={serverData} refreshData={refreshData} loading={loading} lastUpdate={lastUpdate} />
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
    <Layout style={{ minHeight: '100vh' }}>
      <Sider width={300} theme="light" style={{ borderRight: '1px solid #f0f0f0' }}>
        <Sidebar
          serverUrl={serverUrl}
          setServerUrl={setServerUrl}
          serverOnline={serverOnline}
          checkServerConnection={checkServerConnection}
        />
      </Sider>
      
      <Layout>
        <Header style={{ background: '#fff', padding: '16px 24px', borderBottom: '1px solid #f0f0f0', height: 'auto' }}>
          <Title level={2} style={{ margin: '0 0 4px 0' }}>Longleaf Control Dashboard</Title>
          <Text type="secondary">Algorithmic Trading Platform Control Interface</Text>
        </Header>

        <Content style={{ padding: '24px' }}>
          {serverOnline ? (
            <Tabs items={tabItems} defaultActiveKey="overview" />
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
                    <li>Correct URL (default: http://localhost:8080)</li>
                    <li>Network connectivity</li>
                    <li>Firewall settings</li>
                  </ul>
                </div>
              }
            />
          )}
        </Content>
      </Layout>
    </Layout>
  );
};

export default App;
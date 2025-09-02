import React from 'react';
import { Card, Input, Button, Badge, Typography, Divider } from 'antd';
import { ReloadOutlined, CheckCircleOutlined, CloseCircleOutlined } from '@ant-design/icons';

const { Title, Paragraph, Text } = Typography;

const Sidebar = ({ serverUrl, setServerUrl, serverOnline, checkServerConnection }) => {
  const handleUrlChange = (e) => {
    setServerUrl(e.target.value);
  };

  return (
    <div style={{ padding: '16px' }}>
      <Card title="Server Controls" size="small" style={{ marginBottom: '16px' }}>
        <div style={{ marginBottom: '16px' }}>
          <Text strong>Server URL</Text>
          <Input
            value={serverUrl}
            onChange={handleUrlChange}
            placeholder="http://localhost:8080"
            style={{ marginTop: '8px' }}
          />
          <Text type="secondary" style={{ fontSize: '12px' }}>Longleaf server URL</Text>
        </div>

        <div style={{ marginBottom: '16px' }}>
          <Badge 
            status={serverOnline ? 'success' : 'error'}
            text={serverOnline ? 'Server Online' : 'Server Offline'}
          />
        </div>

        <Button 
          type="primary"
          icon={<ReloadOutlined />}
          onClick={checkServerConnection}
          block
        >
          Check Connection
        </Button>
      </Card>
      
      <Card title="Instructions" size="small">
        <div>
          <Paragraph>
            <Text strong>Quick Start:</Text>
          </Paragraph>
          <ol>
            <li>Ensure server is running</li>
            <li>Check server status in Overview tab</li>
            <li>Set target data file in Control tab</li>
            <li>Select trading strategy</li>
            <li>Start server when ready</li>
          </ol>

          <Paragraph>
            <Text strong>Status Meanings:</Text>
          </Paragraph>
          <ul>
            <li><Text strong>Ready</Text>: Server ready for commands</li>
            <li><Text strong>Started</Text>: Server is running strategy</li>
            <li><Text strong>Error</Text>: Server encountered an error</li>
          </ul>
        </div>
      </Card>
    </div>
  );
};

export default Sidebar;
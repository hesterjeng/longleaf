// Reusable container for tab content - eliminates repeated patterns
import React from 'react';
import { Card, Alert, Spin, Typography } from 'antd';
import type { ServerData } from '../../types';

const { Title } = Typography;

interface TabContainerProps {
  title: string;
  serverData: ServerData;
  loading?: boolean;
  error?: string | null;
  requiredData?: (keyof ServerData)[];
  children: React.ReactNode;
  extra?: React.ReactNode;
}

/**
 * Common container for tab content with consistent loading/error states
 * Reduces boilerplate across all tabs
 */
export const TabContainer: React.FC<TabContainerProps> = ({ 
  title, 
  serverData, 
  loading = false, 
  error,
  requiredData = [],
  children,
  extra
}) => {
  // Check if required data is available
  const missingData = requiredData.filter(key => !serverData[key]);
  const hasRequiredData = missingData.length === 0;

  if (loading) {
    return (
      <div style={{ textAlign: 'center', padding: '50px' }}>
        <Spin size="large" />
        <div style={{ marginTop: '16px' }}>Loading {title.toLowerCase()}...</div>
      </div>
    );
  }

  if (error) {
    return (
      <Alert
        type="error"
        showIcon
        message="Error"
        description={error}
        style={{ margin: '16px 0' }}
      />
    );
  }

  if (!hasRequiredData) {
    return (
      <Alert
        type="warning"
        showIcon
        message="Data Not Available"
        description={`Required data (${missingData.join(', ')}) is not available from the server.`}
        style={{ margin: '16px 0' }}
      />
    );
  }

  return (
    <div>
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between', 
        alignItems: 'center',
        marginBottom: '16px' 
      }}>
        <Title level={2} style={{ margin: 0 }}>{title}</Title>
        {extra}
      </div>
      {children}
    </div>
  );
};
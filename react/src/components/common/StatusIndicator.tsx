// Reusable status indicator - eliminates repeated alert patterns
import React from 'react';
import { Alert, Tag } from 'antd';
import { CheckCircleOutlined, CloseCircleOutlined, ExclamationCircleOutlined } from '@ant-design/icons';

interface StatusIndicatorProps {
  status: string | null;
  successStates?: string[];
  errorStates?: string[];
  warningStates?: string[];
  showIcon?: boolean;
}

/**
 * Standardized status indicator component
 * Provides consistent status display across the app
 */
export const StatusIndicator: React.FC<StatusIndicatorProps> = ({
  status,
  successStates = ['Ready', 'Online', 'Connected', 'Success'],
  errorStates = ['Error', 'Failed', 'Offline', 'Disconnected'],
  warningStates = ['Started', 'Pending', 'Warning'],
  showIcon = true
}) => {
  if (!status) {
    return (
      <Alert
        type="info"
        message="Status Unknown"
        showIcon={showIcon}
      />
    );
  }

  const statusStr = String(status);
  let type: 'success' | 'error' | 'warning' | 'info' = 'info';
  let icon = <ExclamationCircleOutlined />;

  if (successStates.some(state => statusStr.includes(state))) {
    type = 'success';
    icon = <CheckCircleOutlined />;
  } else if (errorStates.some(state => statusStr.includes(state))) {
    type = 'error';
    icon = <CloseCircleOutlined />;
  } else if (warningStates.some(state => statusStr.includes(state))) {
    type = 'warning';
    icon = <ExclamationCircleOutlined />;
  }

  return (
    <Alert
      type={type}
      message={`Status: ${statusStr}`}
      showIcon={showIcon}
      icon={showIcon ? icon : undefined}
    />
  );
};

/**
 * Simple status tag variant
 */
export const StatusTag: React.FC<StatusIndicatorProps> = ({
  status,
  successStates = ['Ready', 'Online', 'Connected'],
  errorStates = ['Error', 'Failed', 'Offline'],
  warningStates = ['Started', 'Pending']
}) => {
  if (!status) return <Tag>Unknown</Tag>;

  const statusStr = String(status);
  
  if (successStates.some(state => statusStr.includes(state))) {
    return <Tag color="success" icon={<CheckCircleOutlined />}>{statusStr}</Tag>;
  } else if (errorStates.some(state => statusStr.includes(state))) {
    return <Tag color="error" icon={<CloseCircleOutlined />}>{statusStr}</Tag>;
  } else if (warningStates.some(state => statusStr.includes(state))) {
    return <Tag color="warning" icon={<ExclamationCircleOutlined />}>{statusStr}</Tag>;
  }
  
  return <Tag>{statusStr}</Tag>;
};
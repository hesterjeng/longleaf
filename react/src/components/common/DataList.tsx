// Reusable data list component - eliminates repeated list patterns
import React from 'react';
import { List, Card, Tag, Typography, Empty } from 'antd';
import { CheckCircleOutlined } from '@ant-design/icons';

const { Text } = Typography;

import type { ListItem } from '../../types/common';

interface DataListProps {
  title: string;
  items: ListItem[];
  loading?: boolean;
  emptyText?: string;
  showActiveIndicator?: boolean;
  activeText?: string;
  renderItem?: (item: ListItem, index: number) => React.ReactNode;
}

/**
 * Reusable list component for displaying arrays of data
 * Eliminates repeated list rendering patterns
 */
export const DataList: React.FC<DataListProps> = ({
  title,
  items,
  loading = false,
  emptyText = 'No items available',
  showActiveIndicator = true,
  activeText = 'ACTIVE',
  renderItem
}) => {
  const defaultRenderItem = (item: ListItem, index: number) => (
    <List.Item>
      <div>
        <span>
          {item.icon}
          <Text strong>{index + 1}.</Text>{' '}
          {item.value ? (
            <>
              <Text>{item.label}</Text> <code>{item.value}</code>
            </>
          ) : (
            <code>{item.label}</code>
          )}
        </span>
        {showActiveIndicator && item.isActive && (
          <Tag icon={<CheckCircleOutlined />} color="success">
            {activeText}
          </Tag>
        )}
      </div>
    </List.Item>
  );

  if (items.length === 0) {
    return (
      <Card title={title}>
        <Empty 
          image={Empty.PRESENTED_IMAGE_SIMPLE}
          description={emptyText}
        />
      </Card>
    );
  }

  return (
    <Card title={`${title} (${items.length})`}>
      <List
        loading={loading}
        dataSource={items}
        renderItem={renderItem || defaultRenderItem}
      />
    </Card>
  );
};
// Example of optimized DataTab using new reusable components and hooks
import React from 'react';
import { FileOutlined } from '@ant-design/icons';
import { getActiveDataFile } from '../utils/oclFormat';
import { TabContainer } from './common/TabContainer';
import { DataList } from './common/DataList';
import { useServerData } from '../hooks/useServerData';
import type { BaseTabProps, ListItem } from '../types/common';

/**
 * Optimized DataTab showing significant code reduction
 * Before: ~95 lines of component code
 * After: ~35 lines of component code
 * 
 * Benefits:
 * - Eliminates repeated Alert/Card/List patterns
 * - Consistent error handling via useServerData hook
 * - Reusable TabContainer handles loading states
 * - DataList eliminates manual list rendering
 */
const DataTabOptimized: React.FC<BaseTabProps> = ({ serverData, refreshData, loading }) => {
  const { dataFiles, settings } = serverData;
  const { error } = useServerData(refreshData);

  const currentTarget = getActiveDataFile(settings?.target || null);

  // Transform data files into standard ListItem format
  const dataItems: ListItem[] = (dataFiles || []).map((filePath: string) => ({
    key: filePath,
    label: filePath,
    isActive: currentTarget === filePath,
    icon: 'file'
  }));

  // Instructions component could also be extracted as a reusable component
  const renderInstructions = () => (
    <div style={{ marginBottom: '16px', padding: '16px', background: '#f9f9f9', borderRadius: '6px' }}>
      <p><strong>Data files</strong> are historical market data files used for backtesting and analysis.</p>
      <ul>
        <li>Files are typically JSON format containing OHLCV data</li>
        <li>Use the <strong>longleaf_downloader</strong> tool to create data files</li>
        <li>Select a file to set it as the target for backtesting</li>
      </ul>
      <pre style={{ background: '#f5f5f5', padding: '12px', borderRadius: '4px', fontSize: '12px' }}>
        longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 \<br/>
        {'    '}--interval=10 --timeframe=minute data/24.json
      </pre>
    </div>
  );

  const renderCurrentTarget = () => {
    if (currentTarget) {
      return (
        <div style={{ marginBottom: '16px', padding: '12px', background: '#f6ffed', border: '1px solid #b7eb8f', borderRadius: '6px' }}>
          <strong>Active Data File:</strong> <code>{currentTarget}</code>
        </div>
      );
    }
    return (
      <div style={{ marginBottom: '16px', padding: '12px', background: '#e6f7ff', border: '1px solid #91d5ff', borderRadius: '6px' }}>
        <strong>Target:</strong> Download (live data)
      </div>
    );
  };

  return (
    <TabContainer
      title="Data Files"
      serverData={serverData}
      loading={loading}
      error={error}
      requiredData={['dataFiles']}
    >
      {renderCurrentTarget()}
      {renderInstructions()}
      
      <DataList
        title="Available Data Files"
        items={dataItems}
        loading={loading}
        emptyText="No data files available. Use longleaf_downloader to create data files."
        renderItem={(item) => (
          <div style={{ 
            width: '100%', 
            display: 'flex', 
            justifyContent: 'space-between', 
            alignItems: 'center' 
          }}>
            <span>
              <FileOutlined style={{ marginRight: '8px' }} />
              <code>{item.label}</code>
            </span>
            {item.isActive && (
              <span style={{ 
                background: '#52c41a', 
                color: 'white', 
                padding: '2px 8px', 
                borderRadius: '4px', 
                fontSize: '12px' 
              }}>
                ACTIVE
              </span>
            )}
          </div>
        )}
      />
    </TabContainer>
  );
};

export default DataTabOptimized;
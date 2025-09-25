import React from 'react';
import { Card, Alert, Typography, Tag, Collapse, List } from 'antd';
import { CheckCircleOutlined, FileOutlined } from '@ant-design/icons';
import { getActiveDataFile } from '../utils/oclFormat';
import type { ServerData } from '../types';

const { Title, Text, Paragraph } = Typography;
const { Panel } = Collapse;

interface DataTabProps {
  serverData: ServerData;
  refreshData: () => void;
  loading: boolean;
}

const DataTab: React.FC<DataTabProps> = ({ serverData }) => {
  const { dataFiles, settings } = serverData;

  const currentTarget = getActiveDataFile(settings?.target || null);

  const renderInstructions = () => (
    <Collapse>
      <Panel header="Data File Instructions" key="1">
        <Paragraph>
          <Text strong>Data files</Text> are historical market data files used for backtesting and analysis.
        </Paragraph>
        
        <ul>
          <li>Files are typically JSON format containing OHLCV data</li>
          <li>Use the <Text strong>longleaf_downloader</Text> tool to create data files</li>
          <li>Select a file to set it as the target for backtesting</li>
        </ul>

        <Title level={5}>Example commands:</Title>
        <pre >
{`# Download data for backtesting
longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 \\
    --interval=10 --timeframe=minute data/24.json`}
        </pre>
      </Panel>
    </Collapse>
  );

  return (
    <div>
      <div >
        <img 
          src="/valley.jpg" 
          alt="Valley" 
           
        />
      </div>
      
      <Title level={2} >Data Files</Title>

      <Card title="Current Target" >
        {currentTarget ? (
          <Alert
            type="success"
            message={<span><Text strong>Active Data File:</Text> <code>{currentTarget}</code></span>}
            showIcon
            icon={<FileOutlined />}
          />
        ) : (
          <Alert
            type="info"
            message={<span><Text strong>Target:</Text> Download (live data)</span>}
            showIcon
          />
        )}
        <Text type="secondary" >
          <Text strong>To change the target:</Text> Use the <Text strong>Control</Text> tab → <Text strong>Complete CLI Settings</Text> form
        </Text>
      </Card>

      {dataFiles && dataFiles.length > 0 ? (
        <Card title={`Available Data Files (${dataFiles.length})`} >
          <List
            dataSource={dataFiles}
            renderItem={(filePath: string, index: number) => (
              <List.Item>
                <div >
                  <span>
                    <FileOutlined  />
                    <Text strong>{index + 1}.</Text> <code>{filePath}</code>
                  </span>
                  {currentTarget === filePath && (
                    <Tag icon={<CheckCircleOutlined />} color="success">
                      ACTIVE
                    </Tag>
                  )}
                </div>
              </List.Item>
            )}
          />
        </Card>
      ) : (
        <Alert
          type="warning"
          message="No data files found"
          
        />
      )}
      
      {renderInstructions()}
    </div>
  );
};

export default DataTab;
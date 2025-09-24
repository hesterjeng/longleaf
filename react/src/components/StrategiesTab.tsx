import React from 'react';
import { Card, Alert, Typography, Tag, Collapse, List } from 'antd';
import { CheckCircleOutlined } from '@ant-design/icons';
import type { ServerData } from '../types';

const { Title, Text, Paragraph } = Typography;
const { Panel } = Collapse;

interface StrategiesTabProps {
  serverData: ServerData;
  refreshData: () => void;
  loading: boolean;
}

const StrategiesTab: React.FC<StrategiesTabProps> = ({ serverData }) => {
  const { strategies, settings } = serverData;
  const currentStrategy = settings?.cli_vars?.strategy_arg || null;

  const renderDevelopmentGuide = () => (
    <Collapse>
      <Panel header="Strategy Development Guide" key="1">
        <Paragraph>
          <Text strong>Strategies</Text> are trading algorithms implemented in OCaml using the Longleaf framework.
        </Paragraph>

        <Title level={5}>Key Components:</Title>
        <ul>
          <li><Text strong>Buy Trigger</Text>: Defines entry conditions</li>
          <li><Text strong>Sell Trigger</Text>: Defines exit conditions</li>
          <li><Text strong>Template System</Text>: Use <code>Template.Make</code> functor for consistency</li>
        </ul>

        <Title level={5}>Development Workflow:</Title>
        <ol>
          <li>Create strategy file in <code>strategies/</code> directory</li>
          <li>Implement using Template.Make functor</li>
          <li>Add to <code>longleaf_strategies.ml</code> registry</li>
          <li>Test with backtesting</li>
          <li>Deploy to paper/live trading</li>
        </ol>

        <Title level={5}>Example Strategy Structure:</Title>
        <pre >
{`module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass state symbol = (* entry logic *)
  let score state symbol = (* ranking logic *)
  let num_positions = 5
end

module Sell : Template.Sell_trigger.S = struct
  let make state ~buying_order = (* exit logic *)
end

module Make : Strategy.BUILDER = Template.Make (Buy_inp) (Sell)`}
        </pre>
      </Panel>
    </Collapse>
  );

  return (
    <div>
      <div >
        <img 
          src="/cole.jpg" 
          alt="Cole" 
           
        />
      </div>
      
      <Title level={2} >Available Strategies</Title>

      <Card title="Current Strategy" >
        {currentStrategy ? (
          <Alert
            type="success"
            message={<span><Text strong>Active Strategy:</Text> <code>{currentStrategy}</code></span>}
            showIcon
          />
        ) : (
          <Alert
            type="warning"
            message="No strategy selected"
            showIcon
          />
        )}
        <Text type="secondary" >
          <Text strong>To change the strategy:</Text> Use the <Text strong>Control</Text> tab → <Text strong>Complete CLI Settings</Text> form
        </Text>
      </Card>

      {strategies && strategies.length > 0 ? (
        <Card title={`Available Strategies (${strategies.length})`} >
          <List
            dataSource={strategies}
            renderItem={(strategy: string, index: number) => (
              <List.Item>
                <div >
                  <span>
                    <Text strong>{index + 1}.</Text> <code>{strategy}</code>
                  </span>
                  {currentStrategy === strategy && (
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
          message="No strategies found"
          
        />
      )}
      
      {renderDevelopmentGuide()}
    </div>
  );
};

export default StrategiesTab;
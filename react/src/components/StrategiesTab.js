import React from 'react';
import { Card, Alert, Typography, Tag, Collapse, List } from 'antd';
import { CheckCircleOutlined } from '@ant-design/icons';

const { Title, Text, Paragraph } = Typography;
const { Panel } = Collapse;

const StrategiesTab = ({ serverData }) => {
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
        <pre style={{ background: '#f5f5f5', padding: '16px', borderRadius: '6px' }}>
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
      <Title level={2}>Available Strategies</Title>

      <Card title="Current Strategy" style={{ marginBottom: '16px' }}>
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
        <Text type="secondary" style={{ marginTop: '12px', display: 'block' }}>
          <Text strong>To change the strategy:</Text> Use the <Text strong>Control</Text> tab → <Text strong>Complete CLI Settings</Text> form
        </Text>
      </Card>

      {strategies && strategies.length > 0 ? (
        <Card title={`Available Strategies (${strategies.length})`} style={{ marginBottom: '16px' }}>
          <List
            dataSource={strategies}
            renderItem={(strategy, index) => (
              <List.Item>
                <div style={{ width: '100%', display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
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
          style={{ marginBottom: '16px' }}
        />
      )}
      
      {renderDevelopmentGuide()}
    </div>
  );
};

export default StrategiesTab;
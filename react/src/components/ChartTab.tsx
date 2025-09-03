import React, { useState } from 'react';
import Plot from 'react-plotly.js';
import axios from 'axios';
import { Card, Button, Alert, Select, Typography, Row, Col, Spin, Statistic, Collapse } from 'antd';
import { LineChartOutlined, ReloadOutlined } from '@ant-design/icons';
import type { ServerData, ChartData, APIError } from '../types';

const { Title, Text } = Typography;
const { Option } = Select;
const { Panel } = Collapse;

interface ChartTabProps {
  serverData: ServerData;
  refreshData: () => void;
  loading: boolean;
}

const ChartTab: React.FC<ChartTabProps> = ({ serverData }) => {
  const [selectedSymbol, setSelectedSymbol] = useState<string>('');
  const [chartData, setChartData] = useState<ChartData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);

  const { symbols } = serverData;

  const fetchChartData = async (symbol: string) => {
    if (!symbol) return;

    setLoading(true);
    setError(null);
    
    try {
      const response = await axios.get(`/data/${symbol.toUpperCase()}/json`, { 
        timeout: 10000 
      });
      setChartData(response.data);
    } catch (err) {
      const error = err as APIError;
      if (error.response?.status === 404) {
        setError(`Chart data not yet available for ${symbol}`);
      } else {
        setError(`Error fetching chart data for ${symbol}: ${error.message}`);
      }
      setChartData(null);
    } finally {
      setLoading(false);
    }
  };

  const handleLoadChart = () => {
    if (selectedSymbol) {
      fetchChartData(selectedSymbol);
    }
  };

  const renderChart = () => {
    if (!chartData || !chartData.traces || !chartData.layout) {
      return null;
    }

    // Convert server chart data to Plotly format
    const traces = chartData.traces.map((trace) => ({
      x: trace.x || [],
      y: trace.y || [],
      type: trace.type === 'scatter' ? 'scatter' : 'scatter',
      mode: trace.mode || 'lines',
      name: trace.name || '',
      line: {
        color: trace.line?.color || '#1f77b4',
        dash: trace.line?.dash || 'solid',
        width: trace.line?.width || 1
      },
      visible: trace.visible !== false,
      yaxis: trace.yaxis || 'y',
      marker: trace.marker || {},
      hovertext: trace.hovertext || [],
      hoverinfo: trace.hoverinfo || 'x+y+name'
    }));

    const layout = {
      title: chartData.layout.title || `${selectedSymbol} Price Chart`,
      xaxis: chartData.layout.xaxis || {},
      yaxis: chartData.layout.yaxis || {},
      yaxis2: chartData.layout.yaxis2 || {},
      hovermode: chartData.layout.hovermode || 'x',
      height: 600,
      showlegend: true,
      ...chartData.layout
    };

    return (
      <div>
        <Plot
          data={traces}
          layout={layout}
          style={{ width: '100%', height: '600px' }}
          config={{ responsive: true }}
        />
        
        <Row gutter={16} style={{ marginTop: '20px' }}>
          <Col span={8}>
            <Card>
              <Statistic title="Symbol" value={selectedSymbol} />
            </Card>
          </Col>
          <Col span={8}>
            <Card>
              <Statistic title="Indicators" value={traces.length - 1} />
            </Card>
          </Col>
          <Col span={8}>
            <Card>
              <Statistic title="Data Points" value={traces.length > 0 ? traces[0].x.length : 0} />
            </Card>
          </Col>
        </Row>

        <Collapse style={{ marginTop: '20px' }}>
          <Panel header="Raw Chart Data" key="1">
            <pre style={{ background: '#f5f5f5', padding: '16px', borderRadius: '6px' }}>
              {JSON.stringify(chartData, null, 2)}
            </pre>
          </Panel>
        </Collapse>
      </div>
    );
  };

  const renderInstructions = () => (
    <Collapse>
      <Panel header="Chart Instructions" key="1">
        <Title level={5}>How to use the charts:</Title>
        <ol>
          <li><Text strong>Select Symbol</Text>: Choose from available symbols in the current data file</li>
          <li><Text strong>Load Chart</Text>: Click to fetch and display the price chart</li>
          <li><Text strong>Interactive Features</Text>:
            <ul>
              <li>Zoom: Click and drag to zoom into specific time periods</li>
              <li>Pan: Hold shift and drag to pan across the chart</li>
              <li>Legend: Click legend items to show/hide indicators</li>
              <li>Hover: Hover over data points for detailed information</li>
            </ul>
          </li>
        </ol>

        <Title level={5}>Chart Components:</Title>
        <ul>
          <li><Text strong>Price Line</Text>: Main price data (usually in blue)</li>
          <li><Text strong>Technical Indicators</Text>: Additional analysis overlays</li>
          <li><Text strong>Buy/Sell Markers</Text>: Trading signals if available</li>
          <li><Text strong>Secondary Y-axis</Text>: For oscillators and normalized indicators</li>
        </ul>

        <Text><Text strong>Note</Text>: Charts display data from the currently selected target file.
        Set a data file in the Control tab to enable charting.</Text>
      </Panel>
    </Collapse>
  );

  return (
    <div>
      <Title level={2}>Price Charts</Title>
      
      {symbols && symbols.length > 0 ? (
        <div>
          <Card style={{ marginBottom: '16px' }}>
            <Row gutter={16} align="bottom">
              <Col span={18}>
                <Text strong>Select Symbol</Text>
                <Select
                  style={{ width: '100%', marginTop: '8px' }}
                  placeholder="Choose a symbol..."
                  value={selectedSymbol}
                  onChange={setSelectedSymbol}
                  size="large"
                >
                  {symbols.map(symbol => (
                    <Option key={symbol} value={symbol}>
                      {symbol}
                    </Option>
                  ))}
                </Select>
                <Text type="secondary" style={{ fontSize: '12px' }}>
                  Choose a symbol to view its price chart with technical indicators
                </Text>
              </Col>
              
              <Col span={6}>
                <Button
                  type="primary"
                  icon={<LineChartOutlined />}
                  onClick={handleLoadChart}
                  disabled={!selectedSymbol || loading}
                  size="large"
                  block
                >
                  {loading ? 'Loading...' : 'Load Chart'}
                </Button>
              </Col>
            </Row>
          </Card>

          {error && (
            <Alert
              type="warning"
              message={error}
              style={{ marginBottom: '16px' }}
            />
          )}

          {loading && (
            <Card style={{ textAlign: 'center', padding: '50px', marginBottom: '16px' }}>
              <Spin size="large" />
              <Text style={{ marginTop: '16px', display: 'block' }}>
                Loading chart for {selectedSymbol}...
              </Text>
            </Card>
          )}

          {chartData && renderChart()}
          
          {renderInstructions()}
        </div>
      ) : (
        <div>
          <Alert
            type="warning"
            message="Chart functionality not yet available"
            style={{ marginBottom: '16px' }}
          />
          
          <Alert
            type="info"
            message="Chart Implementation Status"
            description={
              <div>
                <p>The chart endpoints are currently under development:</p>
                <ul>
                  <li><Text strong>/symbols</Text> endpoint: Not yet implemented for File targets</li>
                  <li><Text strong>/data/:symbol/json</Text> endpoint: Returns 404 (not yet implemented)</li>
                </ul>
                
                <p><Text strong>When implemented, you will be able to:</Text></p>
                <ol>
                  <li>Set a data file target in the <Text strong>Control</Text> tab</li>
                  <li>View available symbols from the data file</li>
                  <li>Generate interactive price charts with technical indicators</li>
                </ol>
                
                <p><Text strong>Available data files can be found in the Data Files tab</Text></p>
              </div>
            }
          />
        </div>
      )}
    </div>
  );
};

export default ChartTab;
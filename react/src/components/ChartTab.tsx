import React, { useState } from 'react';
import Plot from 'react-plotly.js';
import axios from 'axios';
import { Card, Alert, Typography, Row, Col, Spin, Statistic, Collapse, Radio } from 'antd';
import type { ServerData, ChartData, APIError } from '../types';

const { Title, Text } = Typography;
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
        timeout: 60000 
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

  // Auto-load chart when symbol is selected
  React.useEffect(() => {
    if (selectedSymbol) {
      fetchChartData(selectedSymbol);
    }
  }, [selectedSymbol]);

  const renderChart = () => {
    if (!chartData || !chartData.traces || !Array.isArray(chartData.traces) || !chartData.layout) {
      return null;
    }

    // Convert server chart data to Plotly format
    console.log('[DEBUG] ChartTab.tsx:63 - About to map chartData.traces:', chartData.traces, 'Type:', typeof chartData.traces, 'IsArray:', Array.isArray(chartData.traces));
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
      showlegend: true,
      autosize: true,
      dragmode: 'zoom',
      selectdirection: 'diagonal',
      transition: { duration: 0 },
      ...chartData.layout
    };

    return (
      <div>
        <Plot
          data={traces}
          layout={layout}
          config={{ 
            responsive: true, 
            displayModeBar: true,
            plotlyServerURL: false,
            showTips: false,
            staticPlot: false,
            editable: false,
            autosizable: true,
            queueLength: 0,
            globalTransforms: [],
            locale: 'en-US'
          }}
        />
        
        <Row gutter={16}>
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

        <Collapse>
          <Panel header="Raw Chart Data" key="1">
            <pre>
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
      <div>
        <img 
          src="/niagara.jpg" 
          alt="Niagara" 
        />
      </div>
      
      <Title level={2}>Price Charts</Title>
      
      {symbols === null || symbols === undefined ? (
        // Case 1: symbols endpoint not available/implemented
        <div>
          <Alert
            type="warning"
            message="Chart functionality not yet available"
          />
          
          <Alert
            type="info"
            message="Chart Implementation Status"
            description={
              <div>
                <p>The chart endpoints are currently under development:</p>
                <ul>
                  <li><Text strong>/symbols</Text> endpoint: Not yet implemented for current target type</li>
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
          {renderInstructions()}
        </div>
      ) : symbols.length === 0 ? (
        // Case 2: symbols available but empty
        <div>
          <Alert
            type="info"
            message="No symbols available"
            description="No symbols found in the current data source. Please check that your data file contains symbol data or select a different data file in the Control tab."
          />
          {renderInstructions()}
        </div>
      ) : (
        // Case 3: symbols available and populated
        <div>
          <Card title="Select Symbol">
            <Radio.Group value={selectedSymbol} onChange={(e) => setSelectedSymbol(e.target.value)} buttonStyle="solid">
              <Row gutter={[8, 8]}>
                {Array.isArray(symbols) && symbols.map(symbol => (
                  <Col xs={12} sm={8} md={6} lg={4} key={symbol}>
                    <Radio.Button value={symbol} style={{ width: '100%', textAlign: 'center' }}>
                      {symbol}
                    </Radio.Button>
                  </Col>
                ))}
              </Row>
            </Radio.Group>
            <div style={{ marginTop: 8 }}>
              <Text type="secondary">
                Choose a symbol to automatically load its price chart with technical indicators
              </Text>
            </div>
          </Card>

          {error && (
            <Alert
              type="warning"
              message={error}
            />
          )}

          {loading && (
            <Card>
              <Spin size="large" />
              <Text>
                Loading chart for {selectedSymbol}...
              </Text>
            </Card>
          )}

          {chartData && renderChart()}
          
          {renderInstructions()}
        </div>
      )}
    </div>
  );
};

export default ChartTab;
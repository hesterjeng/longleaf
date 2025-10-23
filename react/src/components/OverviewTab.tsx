import React, { useState } from 'react';
import { Card, Button, Alert, Typography, Row, Col, Spin, Form, InputNumber, Input, Badge, message, Tooltip, Radio } from 'antd';
import { PlayCircleOutlined, ReloadOutlined, CloseOutlined, SaveOutlined, FileTextOutlined } from '@ant-design/icons';
import Plot from 'react-plotly.js';
import axios from 'axios';
import { formatError, parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget } from '../utils/oclFormat';
import { executeStrategy, updateCLI, updateTarget } from '../utils/api';
import type { ServerData, SettingsFormValues, CLIFormData, APIError, ParsedTarget, StrategyDetails, PerformanceData } from '../types';

const { Title, Text } = Typography;

interface OverviewTabProps {
  serverData: ServerData;
  lastUpdate: Date;
  refreshData: () => void;
  loading: boolean;
  serverUrl: string;
  setServerUrl: (url: string) => void;
  serverOnline: boolean;
  checkServerConnection: () => void;
}

const OverviewTab: React.FC<OverviewTabProps> = ({ serverData, lastUpdate, refreshData, loading, serverUrl, setServerUrl, serverOnline, checkServerConnection }) => {
  const { status, settings, dataFiles, strategies } = serverData;
  const [executing, setExecuting] = useState<boolean>(false);
  const [executeResult, setExecuteResult] = useState<string | null>(null);
  const [executeError, setExecuteError] = useState<string | null>(null);
  const [settingsForm] = Form.useForm();
  const [settingsLoading, setSettingsLoading] = useState<boolean>(false);
  const [performanceData, setPerformanceData] = useState<PerformanceData | null>(null);
  const [performanceLoading, setPerformanceLoading] = useState<boolean>(false);
  const [performanceError, setPerformanceError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState<number>(0);
  const [showServerUrlInput, setShowServerUrlInput] = useState<boolean>(false);
  const [stacktraceActive, setStacktraceActive] = useState<boolean>(false);
  const [printTickActive, setPrintTickActive] = useState<boolean>(false);
  const [saveReceivedActive, setSaveReceivedActive] = useState<boolean>(false);
  const [saveToFileActive, setSaveToFileActive] = useState<boolean>(false);
  const [precomputeIndicatorsActive, setPrecomputeIndicatorsActive] = useState<boolean>(false);
  const [nowaitMarketOpenActive, setNowaitMarketOpenActive] = useState<boolean>(false);
  const [strategyTooltips, setStrategyTooltips] = useState<Record<string, StrategyDetails>>({});
  const strategiesPerPage = 12; // 3 columns × 4 rows = 12 strategies per page

  const { displayedStrategies, totalPages } = React.useMemo(() => {
    if (!strategies || strategies.length === 0) return { displayedStrategies: [], totalPages: 0 };
    
    const total = Math.ceil(strategies.length / strategiesPerPage);
    const start = currentPage * strategiesPerPage;
    const end = start + strategiesPerPage;
    const displayed = strategies.slice(start, end);
    
    return { displayedStrategies: displayed, totalPages: total };
  }, [strategies, currentPage]);

  const runtypeOptions = [
    'Live', 'Paper', 'Backtest', 'Manual', 'Multitest', 'Montecarlo',
    'MultiMontecarlo', 'RandomSliceBacktest', 'MultiRandomSliceBacktest',
    'RandomTickerBacktest', 'MultiRandomTickerBacktest'
  ];

  const fetchStrategyDetails = async (strategyName: string) => {
    if (strategyTooltips[strategyName]) {
      return strategyTooltips[strategyName];
    }

    try {
      console.log(`[API-STRATEGY] 📡 Fetching details for: ${strategyName}`);
      const response = await axios.get(`/strategy/${encodeURIComponent(strategyName)}`, { timeout: 5000 });
      console.log(`[API-STRATEGY] ✅ Got details for: ${strategyName}`);
      const details = response.data;
      setStrategyTooltips(prev => ({ ...prev, [strategyName]: details }));
      return details;
    } catch (error) {
      console.error(`[API-STRATEGY] ❌ Failed to fetch ${strategyName}:`, error);
      return null;
    }
  };

  const renderStrategyTooltip = (strategyName: string) => {
    const details = strategyTooltips[strategyName];
    
    if (!details) {
      return (
        <div>
          <div>Loading strategy details...</div>
        </div>
      );
    }

    return (
      <div>
        <div><strong>Name:</strong> {details.name}</div>
        <div><strong>Max Positions:</strong> {details.max_positions}</div>
        <div><strong>Position Size:</strong> {(details.position_size * 100).toFixed(1)}%</div>
        <div>
          <strong>Buy Trigger:</strong>
          <pre>
            {details.buy_trigger}
          </pre>
        </div>
        <div>
          <strong>Sell Trigger:</strong>
          <pre>
            {details.sell_trigger}
          </pre>
        </div>
      </div>
    );
  };

  const fetchPerformanceData = async (includeOrders = true) => {
    setPerformanceLoading(true);
    setPerformanceError(null);

    try {
      const endpoint = includeOrders ? '/performance?orders=true' : '/performance';
      console.log(`[API-PERFORMANCE] 📡 Fetching: ${endpoint}`);
      const response = await axios.get(endpoint, { timeout: 60000 });
      console.log('[API-PERFORMANCE] ✅ Performance data received');
      setPerformanceData(response.data);
    } catch (error) {
      console.error('[API-PERFORMANCE] ❌ Failed:', error);
      setPerformanceError(formatError(error as APIError, 'fetch performance data'));
      setPerformanceData(null);
    } finally {
      setPerformanceLoading(false);
    }
  };

  // Fetch performance data on component mount and data refresh
  React.useEffect(() => {
    fetchPerformanceData();
  }, []);

  // Monitor status changes to detect errors and hung executions
  React.useEffect(() => {
    if (executing && status === 'Error') {
      console.log('🔥 Server status changed to Error during execution');
      setExecuteError('Strategy execution failed - check server logs');
      setExecuting(false);
    }
  }, [status, executing]);

  // Note: Server blocks completely during execution - cannot respond to any requests
  // No point in trying to check server status while strategy is running

  // Update form when settings change
  React.useEffect(() => {
    if (settings?.cli_vars && settings?.target) {
      const cliData = parseOCamlCLI(settings.cli_vars);
      const targetData = parseTarget(settings.target);
      console.log('Setting form values:', { cliData, targetData, strategies, dataFiles });
      settingsForm.setFieldsValue({
        ...cliData,
        runtype: cliData.runtype || 'Backtest',
        strategy_arg: cliData.strategy_arg || 'Listener',
        target_file: targetData.type === 'Download' ? 'download' : targetData.file
      });
    } else {
      // Set defaults when no settings are available
      settingsForm.setFieldsValue({
        runtype: 'Backtest',
        strategy_arg: 'Listener'
      });
    }
  }, [settings, settingsForm, strategies, dataFiles]);

  const executeStrategyHandler = async () => {
    console.log('[API-EXECUTE] 🚀 Starting strategy execution...');
    setExecuting(true);
    setExecuteResult(null);
    setExecuteError(null);

    try {
      console.log('[API-EXECUTE] 📡 Calling /execute endpoint...');
      const startTime = Date.now();
      const result = await executeStrategy();
      const elapsed = Date.now() - startTime;
      console.log(`[API-EXECUTE] ✅ /execute responded in ${elapsed}ms with:`, result);

      // Server now responds immediately with "strategy execution started"
      if (typeof result.data === 'string' && result.data.includes('started')) {
        setExecuteResult('Strategy execution started - running in background');
        console.log('[API-EXECUTE] 🎯 Strategy started in background, will check status via refresh');
      } else {
        setExecuteResult('Strategy execution initiated');
      }

      setExecuting(false);
      console.log('[API-EXECUTE] 🔄 Triggering data refresh...');
      // Refresh data to get updated status and last_value
      refreshData();
      console.log('[API-EXECUTE] 📊 Fetching performance data...');
      // Also refresh performance data after execution
      fetchPerformanceData();
    } catch (error) {
      console.error('[API-EXECUTE] ❌ Strategy execution error:', error);
      console.error('[API-EXECUTE] ❌ Error type:', (error as Error).name);
      console.error('[API-EXECUTE] ❌ Error message:', (error as Error).message);
      setExecuteError(formatError(error as APIError, 'execute strategy'));
      setExecuting(false);
    }
  };

  // Shutdown handler - currently not exposed in UI but kept for future use
  // const stopServerHandler = async () => {
  //   setSettingsLoading(true);
  //   try {
  //     const response = await fetch('/shutdown');
  //     if (response.ok) {
  //       message.success('Server shutdown initiated');
  //       refreshData();
  //     } else {
  //       throw new Error('Failed to shutdown server');
  //     }
  //   } catch (error) {
  //     message.error(formatError(error as APIError, 'shutdown server'));
  //   } finally {
  //     setSettingsLoading(false);
  //   }
  // };

  const openTearsheet = () => {
    // Open tearsheet in a new tab
    window.open(`${serverUrl}/tearsheet`, '_blank', 'noopener,noreferrer');
  };

  const onFinishSettings = async (values: SettingsFormValues) => {
    setSettingsLoading(true);
    try {
      const cliData: CLIFormData = {
        runtype: values.runtype,
        stacktrace: values.stacktrace,
        strategy_arg: values.strategy_arg,
        no_gui: values.no_gui,
        save_received: values.save_received,
        save_to_file: values.save_to_file,
        nowait_market_open: values.nowait_market_open,
        print_tick_arg: values.print_tick_arg,
        precompute_indicators_arg: values.precompute_indicators_arg,
        compare_preloaded: values.compare_preloaded,
        start: values.start || 0,
        random_drop_chance: values.random_drop_chance || 0,
        slippage_pct: values.slippage_pct || 0.0
      };
      
      const targetData: ParsedTarget = {
        type: values.target_file === 'download' ? 'Download' : 'File',
        file: values.target_file === 'download' ? '' : (values.target_file || '')
      };

      await updateCLI(toOCamlCLI(cliData));
      await updateTarget(toOCamlTarget(targetData));
      
      message.success('Settings updated successfully');
      refreshData();
    } catch (error) {
      message.error(formatError(error as APIError, 'update settings'));
    } finally {
      setSettingsLoading(false);
    }
  };

  const renderStatusDisplay = (statusData: string | null) => {
    if (!statusData) {
      return <Alert type="error" message="No status data available" />;
    }

    const statusStr = typeof statusData === 'string' ? statusData : String(statusData);
    let alertType: 'warning' | 'success' | 'info' | 'error' = 'warning';

    if (statusStr.includes('Ready')) {
      alertType = 'success';
    } else if (statusStr.includes('Started')) {
      alertType = 'info';
    } else if (statusStr.includes('Error')) {
      alertType = 'error';
    }

    const lastValue = settings?.last_value;
    
    // Debug: Log the settings to see if last_value is present
    if (settings) {
      console.log('Settings received:', settings);
      console.log('Last value:', lastValue);
    }

    return (
      <div>
        <Alert
          type={alertType}
          message={`Server Status: ${statusStr}`}
          showIcon
        />
        {lastValue !== undefined && (
          <Alert
            type="info"
            message="Last Execution Result"
            description={
              <Text strong>
                Final Portfolio Value: ${lastValue.toLocaleString('en-US', { 
                  minimumFractionDigits: 2, 
                  maximumFractionDigits: 2 
                })}
              </Text>
            }
          />
        )}
      </div>
    );
  };

  const renderPerformanceChart = () => {
    if (performanceError) {
      return (
        <Alert
          type="warning"
          message="Performance Chart Unavailable"
          description={performanceError}
          action={
            <Button size="small" icon={<ReloadOutlined />} onClick={() => fetchPerformanceData()}>
              Retry
            </Button>
          }
        />
      );
    }

    if (performanceLoading) {
      return (
        <div>
          <Spin size="large" />
          <Text>
            Loading performance data...
          </Text>
        </div>
      );
    }

    if (!performanceData || !performanceData.traces || !Array.isArray(performanceData.traces) || performanceData.traces.length === 0) {
      return (
        <Alert
          type="info"
          message="No Performance Data Available"
          description="Performance data will appear here after running a strategy"
          action={
            <Button size="small" icon={<ReloadOutlined />} onClick={() => fetchPerformanceData()}>
              Refresh
            </Button>
          }
        />
      );
    }

    // Convert server data to Plotly format
    console.log('[DEBUG] OverviewTab.tsx:346 - About to map performanceData.traces:', performanceData.traces, 'Type:', typeof performanceData.traces, 'IsArray:', Array.isArray(performanceData.traces));
    const traces = performanceData.traces.map((trace, index: number) => ({
      x: trace.x || [],
      y: trace.y || [],
      type: 'scatter',
      mode: trace.mode || (index === 0 ? 'lines' : 'markers'),
      name: trace.name || `Trace ${index}`,
      line: trace.line || (index === 0 ? { color: '#1890ff', width: 3 } : undefined),
      marker: trace.marker || {},
      hovertext: trace.hovertext || [],
      hoverinfo: trace.hoverinfo || (index === 0 ? 'x+y+name' : 'text')
    }));

    const layout = {
      title: performanceData.layout?.title || 'Portfolio Performance',
      xaxis: {
        title: 'Time',
        ...performanceData.layout?.xaxis
      },
      yaxis: {
        title: 'Portfolio Value ($)',
        tickformat: ',.0f',
        ...performanceData.layout?.yaxis
      },
      height: 400,
      showlegend: true, // Enable legend for order traces
      hovermode: 'closest', // Better for mixed line/marker data
      autosize: true,
      margin: { l: 60, r: 40, t: 50, b: 50 },
      // Optimizations for large datasets
      dragmode: 'zoom',
      selectdirection: 'diagonal',
      // Reduce animation for better performance with large datasets
      transition: { duration: 0 },
      ...performanceData.layout
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
      </div>
    );
  };



  if (loading) {
    return (
      <div>
        <Spin size="large" />
      </div>
    );
  }

  return (
    <div>
      <div>
        <img 
          src="/hairpine.jpg" 
          alt="Hairpine" 
 
        />
      </div>
      
      <Title level={2}>Longleaf System Overview</Title>
      
      <Row gutter={6} >
        <Col span={8}>
          <Card title="Server Status">
            {renderStatusDisplay(status)}
            <div>
              <Badge 
                status={serverOnline ? 'success' : 'error'}
                text={serverOnline ? 'Server Online' : 'Server Offline'}
              />
            </div>
            <Text type="secondary">
              Last updated: {lastUpdate.toLocaleString()}
            </Text>
          </Card>
        </Col>
        
        <Col span={16}>
          <Card title="Control Panel">
            <Row gutter={[8, 8]}>
              <Col span={8}>
                <Button
                  type="primary"
                  danger
                  size="large"
                  icon={<PlayCircleOutlined />}
                  onClick={executeStrategyHandler}
                  disabled={executing || loading || settingsLoading}
                >
                  {executing ? 'Executing...' : 'Execute Strategy'}
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  type="primary"
                  size="large"
                  icon={<SaveOutlined />}
                  onClick={() => settingsForm.submit()}
                  loading={settingsLoading}
                >
                  Save Settings
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  icon={<ReloadOutlined />}
                  onClick={() => {
                    refreshData();
                    fetchPerformanceData();
                  }}
                  disabled={loading}
                >
                  Refresh Data
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  icon={<ReloadOutlined />}
                  onClick={checkServerConnection}
                >
                  Check Connection
                </Button>
              </Col>
              
              {(executeResult !== null || executeError) && (
                <Col span={8}>
                  <Button
                    size="large"
                    icon={<CloseOutlined />}
                    onClick={() => {
                      setExecuteResult(null);
                      setExecuteError(null);
                    }}
                    >
                    Clear Result
                  </Button>
                </Col>
              )}
              
              {/* Empty buttons to fill out the control panel */}
              <Col span={8}>
                <Button
                  size="large"
                  onClick={() => setShowServerUrlInput(!showServerUrlInput)}
                  type={showServerUrlInput ? 'primary' : 'default'}
                >
                  Server URL
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={stacktraceActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !stacktraceActive;
                    setStacktraceActive(newValue);
                    settingsForm.setFieldsValue({ stacktrace: newValue });
                  }}
                >
                  Stacktrace
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={printTickActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !printTickActive;
                    setPrintTickActive(newValue);
                    settingsForm.setFieldsValue({ print_tick_arg: newValue });
                  }}
                >
                  Print Tick
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  icon={<FileTextOutlined />}
                  onClick={openTearsheet}
                >
                  Tearsheet
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={saveReceivedActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !saveReceivedActive;
                    setSaveReceivedActive(newValue);
                    settingsForm.setFieldsValue({ save_received: newValue });
                  }}
                >
                  Save Received
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={saveToFileActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !saveToFileActive;
                    setSaveToFileActive(newValue);
                    settingsForm.setFieldsValue({ save_to_file: newValue });
                  }}
                >
                  Save to File
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={precomputeIndicatorsActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !precomputeIndicatorsActive;
                    setPrecomputeIndicatorsActive(newValue);
                    settingsForm.setFieldsValue({ precompute_indicators_arg: newValue });
                  }}
                >
                  Precompute Indicators
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  type={nowaitMarketOpenActive ? 'primary' : 'default'}
                  onClick={() => {
                    const newValue = !nowaitMarketOpenActive;
                    setNowaitMarketOpenActive(newValue);
                    settingsForm.setFieldsValue({ nowait_market_open: newValue });
                  }}
                >
                  No Wait Market Open
                </Button>
              </Col>
            </Row>
            
            {showServerUrlInput && (
              <div>
                <Text strong>Server URL</Text>
                <Input
                  value={serverUrl}
                  onChange={(e) => setServerUrl(e.target.value)}
                  placeholder="http://localhost:8080"
                  addonBefore="Server"
                />
                <Text type="secondary">
                  Longleaf server connection URL
                </Text>
              </div>
            )}

            {(executeResult !== null || executeError) && (
              <div>
                {executeResult !== null && (
                  <Alert
                    type="success"
                    message="Execution Result"
                    description={<code>{executeResult}</code>}
                  />
                )}
                
                {executeError && (
                  <Alert
                    type="error"
                    message="Execution Error"
                    description={executeError}
                  />
                )}
              </div>
            )}
          </Card>
        </Col>
      </Row>

      <Card 
        title="Portfolio Performance" 
                      >
        {renderPerformanceChart()}
      </Card>

      <Form
        form={settingsForm}
        layout="vertical"
        onFinish={onFinishSettings}
        onValuesChange={(changedValues, allValues) => {
          // This ensures form state is properly managed
        }}
      >

        <Row gutter={6}>
          <Col span={24}>
            <Card title="Run Type">
              <Form.Item name="runtype" noStyle>
                <Radio.Group buttonStyle="solid">
                  <Row gutter={[8, 8]}>
                    {runtypeOptions.map(type => (
                      <Col xs={12} sm={8} md={6} lg={4} key={type}>
                        <Radio.Button value={type} style={{ width: '100%', textAlign: 'center' }}>
                          {type}
                        </Radio.Button>
                      </Col>
                    ))}
                  </Row>
                </Radio.Group>
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={6}>
          <Col span={24}>
            <Card title="Strategy">
              {strategies && strategies.length > 0 ? (
                <>
                  <Form.Item name="strategy_arg" noStyle>
                    <Radio.Group buttonStyle="solid">
                      <Row gutter={[8, 8]}>
                        {displayedStrategies.map((strategy: string) => (
                          <Col xs={12} sm={8} md={6} lg={4} key={strategy}>
                            <Tooltip
                              title={renderStrategyTooltip(strategy)}
                              placement="top"
                              mouseEnterDelay={0.5}
                              onOpenChange={(visible) => {
                                if (visible && !strategyTooltips[strategy]) {
                                  fetchStrategyDetails(strategy);
                                }
                              }}
                            >
                              <Radio.Button value={strategy} style={{ width: '100%', textAlign: 'center' }}>
                                {strategy}
                              </Radio.Button>
                            </Tooltip>
                          </Col>
                        ))}
                      </Row>
                    </Radio.Group>
                  </Form.Item>

                  {totalPages > 1 && (
                    <Row justify="center" style={{ marginTop: 16 }}>
                      <Col>
                        <Button
                          size="small"
                          onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
                          disabled={currentPage === 0}
                        >
                          ← Previous
                        </Button>
                      </Col>
                      <Col style={{ margin: '0 16px', lineHeight: '32px' }}>
                        <Text>
                          Page {currentPage + 1} of {totalPages} ({strategies.length} strategies)
                        </Text>
                      </Col>
                      <Col>
                        <Button
                          size="small"
                          onClick={() => setCurrentPage(prev => Math.min(totalPages - 1, prev + 1))}
                          disabled={currentPage === totalPages - 1}
                        >
                          Next →
                        </Button>
                      </Col>
                    </Row>
                  )}
                </>
              ) : (
                <Alert type="info" message="No strategies available" />
              )}
            </Card>
          </Col>
        </Row>

        <Row gutter={6}>
          <Col span={24}>
            <Card title="Target">
              <Form.Item name="target_file" noStyle>
                <Radio.Group buttonStyle="solid">
                  <Row gutter={[8, 8]}>
                    {['download', '', ...(dataFiles || [])].map((file: string) => (
                      <Col xs={24} sm={12} md={8} lg={6} key={file || 'none'}>
                        <Radio.Button value={file} style={{ width: '100%', textAlign: 'center' }}>
                          {file === 'download' ? 'Download' : file === '' ? 'None' : file}
                        </Radio.Button>
                      </Col>
                    ))}
                  </Row>
                </Radio.Group>
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={6} >
          <Col span={12}>
            <Card title="Start Index" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="start">
                <InputNumber min={0} size="large"  />
              </Form.Item>
            </Card>
          </Col>
          
          <Col span={12}>
            <Card title="Random Drop %" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="random_drop_chance">
                <InputNumber min={0} max={100} size="large"  />
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={[16, 16]} >
          <Col span={12}>
            <Card title="Slippage %" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="slippage_pct">
                <InputNumber 
                  min={0} 
                  max={1} 
                  step={0.001} 
                  precision={3}
                  size="large" 
                   
                  placeholder="0.010 (1%)"
                />
              </Form.Item>
            </Card>
          </Col>
        </Row>


      </Form>
    </div>
  );
};

export default OverviewTab;
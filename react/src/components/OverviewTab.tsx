import React, { useState } from 'react';
import { Card, Button, Alert, Typography, Row, Col, Spin, Form, Switch, InputNumber, Input, Badge, message, Tooltip } from 'antd';
import { PlayCircleOutlined, ReloadOutlined, CloseOutlined, SaveOutlined, StopOutlined, LineChartOutlined, FileTextOutlined } from '@ant-design/icons';
import Plot from 'react-plotly.js';
import axios from 'axios';
import { formatError, parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget } from '../utils/oclFormat';
import { executeStrategy, updateCLI, updateTarget } from '../utils/api';
import type { ServerData, SettingsFormValues, CLIFormData, APIError, ParsedTarget, StrategyDetails } from '../types';

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
  const [performanceData, setPerformanceData] = useState<any>(null);
  const [performanceLoading, setPerformanceLoading] = useState<boolean>(false);
  const [performanceError, setPerformanceError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState<number>(0);
  const [showServerUrlInput, setShowServerUrlInput] = useState<boolean>(false);
  const [stacktraceActive, setStacktraceActive] = useState<boolean>(false);
  const [printTickActive, setPrintTickActive] = useState<boolean>(false);
  const [comparePreloadedActive, setComparePreloadedActive] = useState<boolean>(false);
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
      const response = await axios.get(`/strategy/${encodeURIComponent(strategyName)}`, { timeout: 5000 });
      const details = response.data;
      setStrategyTooltips(prev => ({ ...prev, [strategyName]: details }));
      return details;
    } catch (error) {
      console.error('Error fetching strategy details:', error);
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
      <div style={{ maxWidth: '400px' }}>
        <div><strong>Name:</strong> {details.name}</div>
        <div><strong>Max Positions:</strong> {details.max_positions}</div>
        <div><strong>Position Size:</strong> {(details.position_size * 100).toFixed(1)}%</div>
        <div style={{ marginTop: '8px' }}>
          <strong>Buy Trigger:</strong>
          <pre style={{ 
            fontSize: '12px', 
            margin: '4px 0', 
            padding: '8px', 
            border: '1px solid #d9d9d9',
            borderRadius: '4px',
            whiteSpace: 'pre-wrap',
            wordBreak: 'break-all',
            fontFamily: 'monospace'
          }}>
            {details.buy_trigger}
          </pre>
        </div>
        <div>
          <strong>Sell Trigger:</strong>
          <pre style={{ 
            fontSize: '12px', 
            margin: '4px 0', 
            padding: '8px', 
            border: '1px solid #d9d9d9',
            borderRadius: '4px',
            whiteSpace: 'pre-wrap',
            wordBreak: 'break-all',
            fontFamily: 'monospace'
          }}>
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
      const response = await axios.get(endpoint, { timeout: 10000 });
      setPerformanceData(response.data);
    } catch (error) {
      console.error('Error fetching performance data:', error);
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
    console.log('🚀 Starting strategy execution...');
    setExecuting(true);
    setExecuteResult(null);
    setExecuteError(null);
    
    try {
      console.log('📡 Calling executeStrategy API...');
      const result = await executeStrategy();
      console.log('✅ Got result from server:', result);
      
      // Server now responds immediately with "strategy execution started"
      if (typeof result.data === 'string' && result.data.includes('started')) {
        setExecuteResult('Strategy execution started - running in background');
        console.log('🎯 Strategy started in background, will check status via refresh');
      } else {
        setExecuteResult('Strategy execution initiated');
      }
      
      setExecuting(false);
      // Refresh data to get updated status and last_value
      refreshData();
      // Also refresh performance data after execution
      fetchPerformanceData();
    } catch (error) {
      console.error('❌ Strategy execution error:', error);
      setExecuteError(formatError(error as APIError, 'execute strategy'));
      setExecuting(false);
    }
  };

  const stopServerHandler = async () => {
    setSettingsLoading(true);
    try {
      const response = await fetch('/shutdown');
      if (response.ok) {
        message.success('Server shutdown initiated');
        refreshData();
      } else {
        throw new Error('Failed to shutdown server');
      }
    } catch (error) {
      message.error(formatError(error as APIError, 'shutdown server'));
    } finally {
      setSettingsLoading(false);
    }
  };

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
        random_drop_chance: values.random_drop_chance || 0
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
            style={{ marginTop: '8px' }}
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
        <div style={{ textAlign: 'center', padding: '50px' }}>
          <Spin size="large" />
          <Text style={{ marginTop: '16px', display: 'block' }}>
            Loading performance data...
          </Text>
        </div>
      );
    }

    if (!performanceData || !performanceData.traces || performanceData.traces.length === 0) {
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
    const traces = performanceData.traces.map((trace: any, index: number) => ({
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
      ...performanceData.layout
    };

    return (
      <div style={{ width: '100%', overflow: 'hidden' }}>
        <Plot
          data={traces}
          layout={layout}
          style={{ width: '100%', height: '400px' }}
          config={{ responsive: true, displayModeBar: true }}
        />
      </div>
    );
  };



  if (loading) {
    return (
      <div style={{ textAlign: 'center', padding: '50px' }}>
        <Spin size="large" />
      </div>
    );
  }

  return (
    <div>
      <div style={{ 
        textAlign: 'center', 
        marginBottom: '12px'
      }}>
        <img 
          src="/hairpine.jpg" 
          alt="Hairpine" 
          style={{ 
            width: '60%',
            height: 'auto',
            borderRadius: '8px'
          }} 
        />
      </div>
      
      <Title level={2} style={{ textAlign: 'center' }}>Longleaf System Overview</Title>
      
      <Row gutter={6} style={{ marginBottom: '6px' }}>
        <Col span={8}>
          <Card title="Server Status" style={{ height: '100%' }} styles={{ body: { padding: '8px' } }}>
            {renderStatusDisplay(status)}
            <div style={{ marginTop: '12px', marginBottom: '8px' }}>
              <Badge 
                status={serverOnline ? 'success' : 'error'}
                text={serverOnline ? 'Server Online' : 'Server Offline'}
              />
            </div>
            <Text type="secondary" style={{ marginTop: '8px', display: 'block' }}>
              Last updated: {lastUpdate.toLocaleString()}
            </Text>
          </Card>
        </Col>
        
        <Col span={16}>
          <Card title="Control Panel" style={{ height: '100%' }} styles={{ body: { padding: '8px' } }}>
            <Row gutter={[8, 8]}>
              <Col span={8}>
                <Button
                  type="primary"
                  danger
                  size="large"
                  icon={<PlayCircleOutlined />}
                  onClick={executeStrategyHandler}
                  disabled={executing || loading || settingsLoading}
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
                >
                  Refresh Data
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  icon={<ReloadOutlined />}
                  onClick={checkServerConnection}
                  style={{ width: '100%' }}
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
                    style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
                >
                  Print Tick
                </Button>
              </Col>
              
              <Col span={8}>
                <Button
                  size="large"
                  icon={<FileTextOutlined />}
                  onClick={openTearsheet}
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
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
                  style={{ width: '100%' }}
                >
                  No Wait Market Open
                </Button>
              </Col>
            </Row>
            
            {showServerUrlInput && (
              <div style={{ marginTop: '16px', marginBottom: '16px' }}>
                <Text strong>Server URL</Text>
                <Input
                  value={serverUrl}
                  onChange={(e) => setServerUrl(e.target.value)}
                  placeholder="http://localhost:8080"
                  style={{ marginTop: '4px' }}
                  addonBefore="Server"
                />
                <Text type="secondary" style={{ fontSize: '12px', display: 'block', marginTop: '2px' }}>
                  Longleaf server connection URL
                </Text>
              </div>
            )}

            {(executeResult !== null || executeError) && (
              <div style={{ marginTop: '16px' }}>
                {executeResult !== null && (
                  <Alert
                    type="success"
                    message="Execution Result"
                    description={<code>{executeResult}</code>}
                    style={{ marginBottom: '16px' }}
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
        style={{ marginBottom: '6px' }}
        styles={{ body: { padding: '8px' } }}
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

        <Row gutter={6} style={{ marginBottom: '6px' }}>
          <Col span={24}>
            <Card title="Run Type" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="runtype">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => (
                    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '6px' }}>
                      {runtypeOptions.map(type => (
                        <Button
                          key={type}
                          size="small"
                          type={getFieldValue('runtype') === type ? 'primary' : 'default'}
                          onClick={() => setFieldsValue({ runtype: type })}
                          style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                        >
                          {type}
                        </Button>
                      ))}
                    </div>
                  )}
                </Form.Item>
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={6} style={{ marginBottom: '6px' }}>
          <Col span={24}>
            <Card title="Strategy" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="strategy_arg">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => {
                    return (
                      <div>
                        {strategies && strategies.length > 0 ? (
                          <>
                            <div style={{ 
                              display: 'grid', 
                              gridTemplateColumns: 'repeat(4, 1fr)', 
                              gap: '6px',
                              marginBottom: '12px'
                            }}>
                              {displayedStrategies.map((strategy: string) => (
                                <Tooltip
                                  key={strategy}
                                  title={renderStrategyTooltip(strategy)}
                                  placement="right"
                                  onOpenChange={(visible) => {
                                    if (visible && !strategyTooltips[strategy]) {
                                      fetchStrategyDetails(strategy);
                                    }
                                  }}
                                >
                                  <Button
                                    size="small"
                                    type={getFieldValue('strategy_arg') === strategy ? 'primary' : 'default'}
                                    onClick={() => setFieldsValue({ strategy_arg: strategy })}
                                    style={{ 
                                      textAlign: 'left', 
                                      justifyContent: 'flex-start',
                                      minHeight: '32px',
                                      width: '100%'
                                    }}
                                  >
                                    {strategy}
                                  </Button>
                                </Tooltip>
                              ))}
                            </div>
                            
                            {totalPages > 1 && (
                              <div style={{ 
                                display: 'flex', 
                                justifyContent: 'center', 
                                alignItems: 'center', 
                                gap: '8px',
                                marginTop: '8px'
                              }}>
                                <Button
                                  size="small"
                                  onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
                                  disabled={currentPage === 0}
                                >
                                  ← Previous
                                </Button>
                                <span style={{ fontSize: '12px', color: '#666' }}>
                                  Page {currentPage + 1} of {totalPages} ({strategies.length} strategies)
                                </span>
                                <Button
                                  size="small"
                                  onClick={() => setCurrentPage(prev => Math.min(totalPages - 1, prev + 1))}
                                  disabled={currentPage === totalPages - 1}
                                >
                                  Next →
                                </Button>
                              </div>
                            )}
                          </>
                        ) : (
                          <Button disabled size="small">No strategies available</Button>
                        )}
                      </div>
                    );
                  }}
                </Form.Item>
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={6} style={{ marginBottom: '6px' }}>
          <Col span={24}>
            <Card title="Target" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="target_file">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => (
                    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '6px' }}>
                      <Button
                        size="small"
                        type={getFieldValue('target_file') === 'download' ? 'primary' : 'default'}
                        onClick={() => setFieldsValue({ target_file: 'download' })}
                        style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                      >
                        Download (Live Data)
                      </Button>
                      <Button
                        size="small"
                        type={getFieldValue('target_file') === '' ? 'primary' : 'default'}
                        onClick={() => setFieldsValue({ target_file: '' })}
                        style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                      >
                        None
                      </Button>
                      {dataFiles && dataFiles.length > 0 ? dataFiles.map((file: string) => (
                        <Button
                          key={file}
                          size="small"
                          type={getFieldValue('target_file') === file ? 'primary' : 'default'}
                          onClick={() => setFieldsValue({ target_file: file })}
                          style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                        >
                          {file}
                        </Button>
                      )) : null}
                    </div>
                  )}
                </Form.Item>
              </Form.Item>
            </Card>
          </Col>
        </Row>

        <Row gutter={6} style={{ marginBottom: '6px' }}>
          <Col span={12}>
            <Card title="Start Index" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="start">
                <InputNumber min={0} size="large" style={{ width: '100%' }} />
              </Form.Item>
            </Card>
          </Col>
          
          <Col span={12}>
            <Card title="Random Drop %" styles={{ body: { padding: '8px' } }}>
              <Form.Item label="" name="random_drop_chance">
                <InputNumber min={0} max={100} size="large" style={{ width: '100%' }} />
              </Form.Item>
            </Card>
          </Col>
        </Row>


      </Form>
    </div>
  );
};

export default OverviewTab;
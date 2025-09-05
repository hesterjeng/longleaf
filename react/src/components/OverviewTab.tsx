import React, { useState } from 'react';
import { Card, Button, Alert, Typography, Row, Col, Spin, Form, Switch, InputNumber, message } from 'antd';
import { PlayCircleOutlined, ReloadOutlined, CloseOutlined, SaveOutlined, StopOutlined, LineChartOutlined } from '@ant-design/icons';
import Plot from 'react-plotly.js';
import axios from 'axios';
import { formatError, parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget } from '../utils/oclFormat';
import { executeStrategy, updateCLI, updateTarget } from '../utils/api';
import type { ServerData, SettingsFormValues, CLIFormData, APIError } from '../types';

const { Title, Text } = Typography;

interface OverviewTabProps {
  serverData: ServerData;
  lastUpdate: Date;
  refreshData: () => void;
  loading: boolean;
}

const OverviewTab: React.FC<OverviewTabProps> = ({ serverData, lastUpdate, refreshData, loading }) => {
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

  const fetchPerformanceData = async () => {
    setPerformanceLoading(true);
    setPerformanceError(null);
    
    try {
      const response = await axios.get('/performance', { timeout: 10000 });
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
        target_type: targetData.type,
        target_file: targetData.file
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
      
      const targetData = {
        type: values.target_type || 'Download',
        file: values.target_file || ''
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
            <Button size="small" icon={<ReloadOutlined />} onClick={fetchPerformanceData}>
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
            <Button size="small" icon={<ReloadOutlined />} onClick={fetchPerformanceData}>
              Refresh
            </Button>
          }
        />
      );
    }

    // Convert server data to Plotly format
    const traces = performanceData.traces.map((trace: any) => ({
      x: trace.x || [],
      y: trace.y || [],
      type: 'scatter',
      mode: 'lines',
      name: trace.name || 'Portfolio Value',
      line: {
        color: '#1890ff',
        width: 3
      }
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
      showlegend: false,
      hovermode: 'x',
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
          config={{ responsive: true, displayModeBar: false }}
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
      <Title level={2}>System Overview</Title>
      
      <Row gutter={16}>
        <Col span={8}>
          <Card title="Server Status">
            {renderStatusDisplay(status)}
            <Text type="secondary" style={{ marginTop: '16px', display: 'block' }}>
              Last updated: {lastUpdate.toLocaleString()}
            </Text>
          </Card>
        </Col>
        
        <Col span={16}>
          <Card title="Server Controls" style={{ marginBottom: '16px' }}>
            <div style={{ marginBottom: '16px', display: 'flex', gap: '8px', alignItems: 'flex-start' }}>
              <Button
                type="primary"
                danger
                size="large"
                icon={<PlayCircleOutlined />}
                onClick={executeStrategyHandler}
                disabled={executing || loading || settingsLoading}
                style={{ minWidth: '150px' }}
              >
                {executing ? 'Executing...' : 'Execute Strategy'}
              </Button>
              
              <Button
                size="large"
                icon={<StopOutlined />}
                onClick={stopServerHandler}
                disabled={settingsLoading || loading}
                style={{ minWidth: '150px' }}
              >
                Shutdown Server
              </Button>
              
              <Button
                size="large"
                icon={<ReloadOutlined />}
                onClick={refreshData}
                disabled={loading}
                style={{ minWidth: '150px' }}
              >
                Refresh Data
              </Button>
              
              {(executeResult !== null || executeError) && (
                <Button
                  size="large"
                  icon={<CloseOutlined />}
                  onClick={() => {
                    setExecuteResult(null);
                    setExecuteError(null);
                  }}
                  style={{ minWidth: '150px' }}
                >
                  Clear Result
                </Button>
              )}
            </div>
            
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
                style={{ marginBottom: '16px' }}
              />
            )}
          </Card>
        </Col>
      </Row>

      <Card 
        title="Portfolio Performance" 
        style={{ marginBottom: '16px' }}
        extra={
          <Button 
            icon={<LineChartOutlined />}
            onClick={fetchPerformanceData}
            disabled={performanceLoading}
            size="small"
          >
            Refresh Chart
          </Button>
        }
      >
        {renderPerformanceChart()}
      </Card>

      <Card 
        title="Configuration" 
        extra={
          <Button 
            type="primary" 
            icon={<SaveOutlined />}
            loading={settingsLoading}
            onClick={() => settingsForm.submit()}
          >
            Save Settings
          </Button>
        }
      >
        <Form
          form={settingsForm}
          layout="vertical"
          onFinish={onFinishSettings}
          onValuesChange={(changedValues, allValues) => {
            // This ensures form state is properly managed
          }}
        >
          <Row gutter={24}>
            <Col span={8}>
              <Form.Item label="Run Type" name="runtype">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => (
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
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
            </Col>
            
            <Col span={16}>
              <Form.Item label="Strategy" name="strategy_arg">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => {
                    
                    return (
                      <div>
                        {strategies && strategies.length > 0 ? (
                          <>
                            {/* Strategy Grid - 3 columns, no scrollbars */}
                            <div style={{ 
                              display: 'grid', 
                              gridTemplateColumns: 'repeat(3, 1fr)', 
                              gap: '6px',
                              marginBottom: '12px'
                            }}>
                              {displayedStrategies.map((strategy: string) => (
                                <Button
                                  key={strategy}
                                  size="small"
                                  type={getFieldValue('strategy_arg') === strategy ? 'primary' : 'default'}
                                  onClick={() => setFieldsValue({ strategy_arg: strategy })}
                                  style={{ 
                                    textAlign: 'left', 
                                    justifyContent: 'flex-start',
                                    minHeight: '32px' // Ensure consistent button height
                                  }}
                                >
                                  {strategy}
                                </Button>
                              ))}
                            </div>
                            
                            {/* Pagination Controls */}
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
            </Col>
          </Row>
          
          <Row gutter={24}>
            <Col span={6}>
              <Form.Item label="Target Type" name="target_type">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => (
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
                      <Button
                        size="small"
                        type={getFieldValue('target_type') === 'Download' ? 'primary' : 'default'}
                        onClick={() => setFieldsValue({ target_type: 'Download' })}
                        style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                      >
                        Download (Live Data)
                      </Button>
                      <Button
                        size="small"
                        type={getFieldValue('target_type') === 'File' ? 'primary' : 'default'}
                        onClick={() => setFieldsValue({ target_type: 'File' })}
                        style={{ textAlign: 'left', justifyContent: 'flex-start' }}
                      >
                        Data File
                      </Button>
                    </div>
                  )}
                </Form.Item>
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Target File" name="target_file">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldValue, setFieldsValue }) => (
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '6px', maxHeight: '300px', overflowY: 'auto' }}>
                      <Button
                        size="small"
                        type={!getFieldValue('target_file') ? 'primary' : 'default'}
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
                      )) : (
                        <Button disabled size="small">No data files available</Button>
                      )}
                    </div>
                  )}
                </Form.Item>
              </Form.Item>
            </Col>
            
            <Col span={12}>
              <div style={{ display: 'flex', gap: '24px' }}>
                <Form.Item label="Start Index" name="start" style={{ flex: 1 }}>
                  <InputNumber min={0} size="large" style={{ width: '100%' }} />
                </Form.Item>
                
                <Form.Item label="Random Drop %" name="random_drop_chance" style={{ flex: 1 }}>
                  <InputNumber min={0} max={100} size="large" style={{ width: '100%' }} />
                </Form.Item>
              </div>
            </Col>
          </Row>
          
          <Typography.Title level={5} style={{ marginTop: '24px', marginBottom: '16px' }}>
            Options
          </Typography.Title>
          
          <Row gutter={16}>
            <Col span={6}>
              <Form.Item label="Stacktrace" name="stacktrace" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Print Tick" name="print_tick_arg" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="No GUI" name="no_gui" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Compare Preloaded" name="compare_preloaded" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
          </Row>
          
          <Row gutter={16}>
            <Col span={6}>
              <Form.Item label="Save Received Data" name="save_received" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Save to File" name="save_to_file" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Precompute Indicators" name="precompute_indicators_arg" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="No Wait Market Open" name="nowait_market_open" valuePropName="checked">
                <Switch />
              </Form.Item>
            </Col>
          </Row>
        </Form>
      </Card>
    </div>
  );
};

export default OverviewTab;
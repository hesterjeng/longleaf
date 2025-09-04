import React, { useState } from 'react';
import { Card, Button, Alert, Typography, Row, Col, Spin, Form, Select, Switch, InputNumber, message } from 'antd';
import { PlayCircleOutlined, ReloadOutlined, CloseOutlined, SaveOutlined, StopOutlined } from '@ant-design/icons';
import { formatError, parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget } from '../utils/oclFormat';
import { executeStrategy, updateServerStatus, updateCLI, updateTarget } from '../utils/api';
import type { ServerData, SettingsFormValues, CLIFormData, APIError } from '../types';

const { Title, Text } = Typography;
const { Option } = Select;

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

  const runtypeOptions = [
    'Live', 'Paper', 'Backtest', 'Manual', 'Multitest', 'Montecarlo',
    'MultiMontecarlo', 'RandomSliceBacktest', 'MultiRandomSliceBacktest',
    'RandomTickerBacktest', 'MultiRandomTickerBacktest'
  ];

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
        target_type: targetData.type,
        target_file: targetData.file
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
    } catch (error) {
      console.error('❌ Strategy execution error:', error);
      setExecuteError(formatError(error as APIError, 'execute strategy'));
      setExecuting(false);
    }
  };

  const stopServerHandler = async () => {
    setSettingsLoading(true);
    try {
      await updateServerStatus('Ready');
      message.success('Server stopped successfully');
      refreshData();
    } catch (error) {
      message.error(formatError(error as APIError, 'stop server'));
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
            <div style={{ marginBottom: '16px' }}>
              <Button
                type="primary"
                danger
                size="large"
                icon={<PlayCircleOutlined />}
                onClick={executeStrategyHandler}
                disabled={executing || loading || settingsLoading}
                style={{ marginRight: '8px' }}
              >
                {executing ? 'Executing...' : 'Execute Strategy'}
              </Button>
              
              <Button
                icon={<StopOutlined />}
                onClick={stopServerHandler}
                disabled={settingsLoading || loading}
                style={{ marginRight: '8px' }}
              >
                Stop Server
              </Button>
              
              {(executeResult !== null || executeError) && (
                <Button
                  size="small"
                  icon={<CloseOutlined />}
                  onClick={() => {
                    setExecuteResult(null);
                    setExecuteError(null);
                  }}
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
        >
          <Row gutter={24}>
            <Col span={6}>
              <Form.Item label="Run Type" name="runtype">
                <Select size="large">
                  {runtypeOptions.map(type => (
                    <Option key={type} value={type}>{type}</Option>
                  ))}
                </Select>
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Strategy" name="strategy_arg">
                <Select placeholder="Select strategy" showSearch size="large">
                  {strategies && strategies.length > 0 ? strategies.map((strategy: string) => (
                    <Option key={strategy} value={strategy}>{strategy}</Option>
                  )) : <Option disabled value="">No strategies available</Option>}
                </Select>
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Start Index" name="start">
                <InputNumber min={0} size="large" style={{ width: '100%' }} />
              </Form.Item>
            </Col>
            
            <Col span={6}>
              <Form.Item label="Random Drop %" name="random_drop_chance">
                <InputNumber min={0} max={100} size="large" style={{ width: '100%' }} />
              </Form.Item>
            </Col>
          </Row>
          
          <Row gutter={24}>
            <Col span={12}>
              <Form.Item label="Target Type" name="target_type">
                <Select size="large">
                  <Option value="Download">Download (Live Data)</Option>
                  <Option value="File">Data File</Option>
                </Select>
              </Form.Item>
            </Col>
            
            <Col span={12}>
              <Form.Item label="Target File" name="target_file">
                <Select placeholder="Select data file" allowClear showSearch size="large">
                  {dataFiles && dataFiles.length > 0 ? dataFiles.map((file: string) => (
                    <Option key={file} value={file}>{file}</Option>
                  )) : <Option disabled value="">No data files available</Option>}
                </Select>
              </Form.Item>
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

      <Card style={{ marginTop: '16px' }}>
        <Button type="primary" icon={<ReloadOutlined />} onClick={refreshData}>
          Refresh Data
        </Button>
      </Card>
    </div>
  );
};

export default OverviewTab;
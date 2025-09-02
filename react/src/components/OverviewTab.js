import React, { useState } from 'react';
import { Card, Button, Alert, Table, Typography, Row, Col, Spin, Tag, Collapse, Form, Select, Switch, InputNumber, Input, message } from 'antd';
import { PlayCircleOutlined, ReloadOutlined, CloseOutlined, SettingOutlined, SaveOutlined, StopOutlined } from '@ant-design/icons';
import { parseOCamlVariant, getTargetDisplay, formatError, parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget } from '../utils/oclFormat';
import { executeStrategy, updateServerStatus, updateCLI, updateTarget } from '../utils/api';

const { Title, Text } = Typography;
const { Panel } = Collapse;
const { Option } = Select;

const OverviewTab = ({ serverData, lastUpdate, refreshData, loading }) => {
  const { status, settings, dataFiles, strategies } = serverData;
  const [executing, setExecuting] = useState(false);
  const [executeResult, setExecuteResult] = useState(null);
  const [executeError, setExecuteError] = useState(null);
  const [settingsForm] = Form.useForm();
  const [settingsLoading, setSettingsLoading] = useState(false);

  const runtypeOptions = [
    'Live', 'Paper', 'Backtest', 'Manual', 'Multitest', 'Montecarlo',
    'MultiMontecarlo', 'RandomSliceBacktest', 'MultiRandomSliceBacktest',
    'RandomTickerBacktest', 'MultiRandomTickerBacktest'
  ];

  // Monitor status changes to detect completion
  React.useEffect(() => {
    if (executing) {
      if (status === 'Ready' && executeResult === null && executeError === null) {
        setExecuteResult('Strategy execution completed');
        setExecuting(false);
      } else if (status === 'Error') {
        setExecuteError('Strategy execution failed - check server logs');
        setExecuting(false);
      }
    }
  }, [status, executing, executeResult, executeError]);

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
    setExecuting(true);
    setExecuteResult(null);
    setExecuteError(null);
    
    try {
      executeStrategy().catch(error => {
        if (!error.code || error.code !== 'ECONNABORTED') {
          setExecuteError(formatError(error, 'execute strategy'));
          setExecuting(false);
        }
      });
      refreshData();
    } catch (error) {
      setExecuteError(formatError(error, 'execute strategy'));
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
      message.error(formatError(error, 'stop server'));
    } finally {
      setSettingsLoading(false);
    }
  };

  const onFinishSettings = async (values) => {
    setSettingsLoading(true);
    try {
      const cliData = {
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
      message.error(formatError(error, 'update settings'));
    } finally {
      setSettingsLoading(false);
    }
  };

  const renderStatusDisplay = (statusData) => {
    if (!statusData) {
      return <Alert type="error" message="No status data available" />;
    }

    const statusStr = typeof statusData === 'string' ? statusData : String(statusData);
    let alertType = 'warning';

    if (statusStr.includes('Ready')) {
      alertType = 'success';
    } else if (statusStr.includes('Started')) {
      alertType = 'info';
    } else if (statusStr.includes('Error')) {
      alertType = 'error';
    }

    return (
      <Alert
        type={alertType}
        message={`Server Status: ${statusStr}`}
        showIcon
      />
    );
  };

  const renderSettingsDisplay = (settingsData) => {
    if (!settingsData) {
      return <Alert type="error" message="No settings data available" />;
    }

    const cliVars = settingsData.cli_vars || {};
    const target = settingsData.target;
    const targetDisplay = getTargetDisplay(target);

    const coreColumns = [
      {
        title: 'Setting',
        dataIndex: 'setting',
        key: 'setting',
        width: '40%'
      },
      {
        title: 'Value',
        dataIndex: 'value',
        key: 'value',
        render: (text) => <code>{text}</code>
      }
    ];

    const coreData = [
      { key: '1', setting: 'Run Type', value: parseOCamlVariant(cliVars.runtype) || 'Not set' },
      { key: '2', setting: 'Strategy', value: cliVars.strategy_arg || 'Not set' },
      { key: '3', setting: 'Target', value: targetDisplay },
      { key: '4', setting: 'Start Index', value: cliVars.start || 0 }
    ];

    return (
      <div>
        <Title level={4}>Current Configuration</Title>
        
        <Row gutter={16}>
          <Col span={12}>
            <Card title="Core Settings" size="small">
              <Table
                columns={coreColumns}
                dataSource={coreData}
                pagination={false}
                size="small"
              />
            </Card>
          </Col>
          
          <Col span={12}>
            <Card title="Options" size="small">
              <div>
                <Text strong>Debug:</Text>
                <div style={{ marginBottom: '12px' }}>
                  <Tag color={cliVars.stacktrace ? 'green' : 'default'}>Stacktrace</Tag>
                  <Tag color={cliVars.print_tick_arg ? 'green' : 'default'}>Print Tick</Tag>
                  <Tag color={cliVars.compare_preloaded ? 'green' : 'default'}>Compare Preloaded</Tag>
                </div>
                
                <Text strong>Processing:</Text>
                <div style={{ marginBottom: '12px' }}>
                  <Tag color={cliVars.no_gui ? 'green' : 'default'}>No GUI</Tag>
                  <Tag color={cliVars.precompute_indicators_arg ? 'green' : 'default'}>Precompute Indicators</Tag>
                  <Tag color={cliVars.nowait_market_open ? 'green' : 'default'}>No Wait Market Open</Tag>
                </div>
                
                <Text strong>Storage:</Text>
                <div style={{ marginBottom: '12px' }}>
                  <Tag color={cliVars.save_received ? 'green' : 'default'}>Save Received</Tag>
                  <Tag color={cliVars.save_to_file ? 'green' : 'default'}>Save to File</Tag>
                </div>
                
                {cliVars.random_drop_chance > 0 && (
                  <div>
                    <Text strong>Random Drop:</Text> <Tag>{cliVars.random_drop_chance}%</Tag>
                  </div>
                )}
              </div>
            </Card>
          </Col>
        </Row>

        <Collapse style={{ marginTop: '16px' }}>
          <Panel header="Raw Settings JSON" key="1">
            <pre style={{ background: '#f5f5f5', padding: '16px', borderRadius: '6px' }}>
              {JSON.stringify(settingsData, null, 2)}
            </pre>
          </Panel>
        </Collapse>
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
                  {strategies && strategies.length > 0 ? strategies.map(strategy => (
                    <Option key={strategy} value={strategy}>{strategy}</Option>
                  )) : <Option disabled>No strategies available</Option>}
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
                  {dataFiles && dataFiles.length > 0 ? dataFiles.map(file => (
                    <Option key={file} value={file}>{file}</Option>
                  )) : <Option disabled>No data files available</Option>}
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
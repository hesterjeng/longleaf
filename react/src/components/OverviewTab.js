import React, { useState } from 'react';
import { parseOCamlVariant, getTargetDisplay, formatError } from '../utils/oclFormat';
import { executeStrategy } from '../utils/api';

const OverviewTab = ({ serverData, lastUpdate, refreshData, loading }) => {
  const { status, settings } = serverData;
  const [executing, setExecuting] = useState(false);
  const [executeResult, setExecuteResult] = useState(null);
  const [executeError, setExecuteError] = useState(null);

  // Monitor status changes to detect completion
  React.useEffect(() => {
    if (executing) {
      if (status === 'Ready' && executeResult === null && executeError === null) {
        // Execution completed successfully - we'd need to get the result from somewhere
        setExecuteResult('Strategy execution completed');
        setExecuting(false);
      } else if (status === 'Error') {
        // Execution failed
        setExecuteError('Strategy execution failed - check server logs');
        setExecuting(false);
      }
      // If status is 'Started', keep executing state
    }
  }, [status, executing, executeResult, executeError]);

  const executeStrategyHandler = async () => {
    setExecuting(true);
    setExecuteResult(null);
    setExecuteError(null);
    
    try {
      // Fire and forget - don't wait for completion
      executeStrategy().catch(error => {
        // Only handle immediate errors (not timeout)
        if (!error.code || error.code !== 'ECONNABORTED') {
          setExecuteError(formatError(error, 'execute strategy'));
          setExecuting(false);
        }
      });
      
      // Start polling status immediately
      refreshData();
      
    } catch (error) {
      setExecuteError(formatError(error, 'execute strategy'));
      setExecuting(false);
    }
  };

  const renderStatusDisplay = (statusData) => {
    if (!statusData) {
      return <div className="alert alert-danger">No status data available</div>;
    }

    const statusStr = typeof statusData === 'string' ? statusData : String(statusData);
    let alertClass = 'alert-warning';
    let icon = '🟡';

    if (statusStr.includes('Ready')) {
      alertClass = 'alert-success';
      icon = '🟢';
    } else if (statusStr.includes('Started')) {
      alertClass = 'alert-info';
      icon = '🔵';
    } else if (statusStr.includes('Error')) {
      alertClass = 'alert-danger';
      icon = '🔴';
    }

    return (
      <div className={`alert ${alertClass}`}>
        <strong>{icon} Server Status: {statusStr}</strong>
      </div>
    );
  };

  const renderSettingsDisplay = (settingsData) => {
    if (!settingsData) {
      return <div className="alert alert-danger">No settings data available</div>;
    }

    const cliVars = settingsData.cli_vars || {};
    const target = settingsData.target;
    const targetDisplay = getTargetDisplay(target);

    return (
      <div>
        <h4>Current Configuration</h4>
        
        <div className="grid grid-2">
          <div>
            <h5>🎯 Core Settings</h5>
            <table style={{ width: '100%', fontSize: '14px' }}>
              <tbody>
                <tr><td><strong>Run Type:</strong></td><td><code>{parseOCamlVariant(cliVars.runtype) || 'Not set'}</code></td></tr>
                <tr><td><strong>Strategy:</strong></td><td><code>{cliVars.strategy_arg || 'Not set'}</code></td></tr>
                <tr><td><strong>Target:</strong></td><td><code>{targetDisplay}</code></td></tr>
                <tr><td><strong>Start Index:</strong></td><td><code>{cliVars.start || 0}</code></td></tr>
              </tbody>
            </table>
          </div>
          
          <div>
            <h5>⚙️ Options</h5>
            <div className="grid grid-2" style={{ gap: '10px', fontSize: '14px' }}>
              <div>
                <strong>Debug:</strong>
                <ul style={{ listStyle: 'none', padding: '0', margin: '5px 0' }}>
                  <li>{cliVars.stacktrace ? '✅' : '❌'} Stacktrace</li>
                  <li>{cliVars.print_tick_arg ? '✅' : '❌'} Print Tick</li>
                  <li>{cliVars.compare_preloaded ? '✅' : '❌'} Compare Preloaded</li>
                </ul>
              </div>
              <div>
                <strong>Processing:</strong>
                <ul style={{ listStyle: 'none', padding: '0', margin: '5px 0' }}>
                  <li>{cliVars.no_gui ? '✅' : '❌'} No GUI</li>
                  <li>{cliVars.precompute_indicators_arg ? '✅' : '❌'} Precompute Indicators</li>
                  <li>{cliVars.nowait_market_open ? '✅' : '❌'} No Wait Market Open</li>
                </ul>
              </div>
            </div>
            
            <div style={{ marginTop: '10px', fontSize: '14px' }}>
              <strong>Storage:</strong>
              <ul style={{ listStyle: 'none', padding: '0', margin: '5px 0' }}>
                <li>{cliVars.save_received ? '✅' : '❌'} Save Received Data</li>
                <li>{cliVars.save_to_file ? '✅' : '❌'} Save to File</li>
              </ul>
            </div>
            
            {cliVars.random_drop_chance > 0 && (
              <div style={{ marginTop: '10px', fontSize: '14px' }}>
                <strong>Random Drop:</strong> <code>{cliVars.random_drop_chance}%</code>
              </div>
            )}
          </div>
        </div>

        <details className="expandable">
          <summary className="expandable-header">
            🔍 Raw Settings JSON
          </summary>
          <div className="expandable-content">
            <pre className="code-block">
              {JSON.stringify(settingsData, null, 2)}
            </pre>
          </div>
        </details>
      </div>
    );
  };

  if (loading) {
    return (
      <div className="loading">
        <div className="spinner"></div>
      </div>
    );
  }

  return (
    <div>
      <h2>📊 System Overview</h2>
      
      <div className="grid grid-2">
        <div className="card">
          <h3>Server Status</h3>
          {renderStatusDisplay(status)}
          <p className="text-muted">
            Last updated: {lastUpdate.toLocaleString()}
          </p>
        </div>
        
        <div className="card">
          {renderSettingsDisplay(settings)}
          
          <div style={{ marginTop: '20px', borderTop: '1px solid #eee', paddingTop: '20px' }}>
            <h4>🚀 Strategy Execution</h4>
            <div style={{ display: 'flex', alignItems: 'center', gap: '15px', marginBottom: '15px' }}>
              <button
                className="btn"
                onClick={executeStrategyHandler}
                disabled={executing || loading}
                style={{
                  backgroundColor: '#dc3545',
                  borderColor: '#dc3545',
                  color: 'white',
                  fontSize: '16px',
                  padding: '10px 20px',
                  fontWeight: 'bold'
                }}
              >
                {executing ? '⏳ Executing...' : '🚀 Execute Strategy'}
              </button>
              {(executeResult !== null || executeError) && (
                <button
                  className="btn btn-secondary btn-sm"
                  onClick={() => {
                    setExecuteResult(null);
                    setExecuteError(null);
                  }}
                  style={{ fontSize: '12px' }}
                >
                  🗑️ Clear Result
                </button>
              )}
            </div>
            
            {executeResult !== null && (
              <div className="alert alert-success">
                <strong>✅ Execution Result:</strong>
                <div style={{ marginTop: '8px', fontFamily: 'monospace', fontSize: '14px' }}>
                  {executeResult}
                </div>
              </div>
            )}
            
            {executeError && (
              <div className="alert alert-danger">
                <strong>❌ Execution Error:</strong>
                <div style={{ marginTop: '8px', fontSize: '14px' }}>
                  {executeError}
                </div>
              </div>
            )}
          </div>
        </div>
      </div>

      <div className="card">
        <button className="btn btn-primary" onClick={refreshData}>
          🔄 Refresh Data
        </button>
      </div>
    </div>
  );
};

export default OverviewTab;
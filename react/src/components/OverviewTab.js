import React from 'react';

const OverviewTab = ({ serverData, lastUpdate, refreshData, loading }) => {
  const { status, settings } = serverData;

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

    let targetDisplay = '';
    if (Array.isArray(target)) {
      if (target.length === 1 && target[0] === 'Download') {
        targetDisplay = 'Download';
      } else if (target.length === 2 && target[0] === 'File') {
        targetDisplay = `File: ${target[1]}`;
      } else {
        targetDisplay = target.join(' ');
      }
    } else {
      targetDisplay = String(target || 'None');
    }

    return (
      <div>
        <h4>Current Configuration</h4>
        
        <div className="grid grid-2">
          <div>
            <h5>🎯 Core Settings</h5>
            <table style={{ width: '100%', fontSize: '14px' }}>
              <tbody>
                <tr><td><strong>Run Type:</strong></td><td><code>{cliVars.runtype || 'Not set'}</code></td></tr>
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
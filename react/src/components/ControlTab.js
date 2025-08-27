import React, { useState, useEffect } from 'react';
import { parseOCamlCLI, toOCamlCLI, parseTarget, toOCamlTarget, formatError } from '../utils/oclFormat';
import { updateServerStatus, updateCLI, updateTarget } from '../utils/api';

const ControlTab = ({ serverData, refreshData }) => {
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState(null);
  
  // CLI Settings form state
  const [cliSettings, setCLISettings] = useState({
    runtype: 'Backtest',
    stacktrace: false,
    strategy_arg: '',
    no_gui: false,
    save_received: false,
    save_to_file: false,
    nowait_market_open: false,
    print_tick_arg: false,
    precompute_indicators_arg: false,
    compare_preloaded: false,
    start: 0,
    random_drop_chance: 0
  });
  
  // Target settings (separate from CLI but managed together)
  const [targetSettings, setTargetSettings] = useState({
    type: 'Download',
    file: ''
  });

  const runtypeOptions = [
    'Live', 'Paper', 'Backtest', 'Manual', 'Multitest', 'Montecarlo',
    'MultiMontecarlo', 'RandomSliceBacktest', 'MultiRandomSliceBacktest',
    'RandomTickerBacktest', 'MultiRandomTickerBacktest'
  ];

  const { dataFiles, strategies } = serverData;

  // Update CLI and target settings when serverData changes
  useEffect(() => {
    if (serverData.settings?.cli_vars) {
      setCLISettings(parseOCamlCLI(serverData.settings.cli_vars));
    }
    
    if (serverData.settings?.target) {
      setTargetSettings(parseTarget(serverData.settings.target));
    }
  }, [serverData.settings]);

  const showMessage = (msg, type = 'info') => {
    setMessage({ text: msg, type });
    setTimeout(() => setMessage(null), 5000);
  };

  const setServerStatus = async (status) => {
    setLoading(true);
    try {
      await updateServerStatus(status);
      showMessage(`Server ${status.toLowerCase()} successfully`, 'success');
      refreshData();
    } catch (error) {
      showMessage(formatError(error, `set server status to ${status}`), 'danger');
    } finally {
      setLoading(false);
    }
  };

  const setTarget = async (target) => {
    setLoading(true);
    try {
      await updateTarget(target);
      const targetDisplay = Array.isArray(target) ? target.join(' -> ') : target;
      showMessage(`Target set to: ${targetDisplay}`, 'success');
      refreshData();
    } catch (error) {
      showMessage(formatError(error, 'set target'), 'danger');
    } finally {
      setLoading(false);
    }
  };

  const setCLI = async (cliData) => {
    setLoading(true);
    try {
      await updateCLI(toOCamlCLI(cliData));
      showMessage('CLI settings updated successfully', 'success');
      refreshData();
    } catch (error) {
      showMessage(formatError(error, 'update CLI settings'), 'danger');
    } finally {
      setLoading(false);
    }
  };

  const handleCLIChange = (field, value) => {
    setCLISettings(prev => ({
      ...prev,
      [field]: value
    }));
  };

  const handleCLISubmit = async (e) => {
    e.preventDefault();
    
    try {
      await setCLI(cliSettings);
      await setTarget(toOCamlTarget(targetSettings));
    } catch (error) {
      // Error handling is done in individual functions
    }
  };

  return (
    <div>
      <h2>🎛️ Server Control</h2>

      {message && (
        <div className={`alert alert-${message.type}`}>
          {message.text}
        </div>
      )}

      <div className="card">
        <h3>Status Management</h3>
        <div className="grid grid-3">
          <button
            className="btn btn-success"
            onClick={() => setServerStatus('Started')}
            disabled={loading}
          >
            ▶️ Start Server
          </button>
          
          <button
            className="btn btn-secondary"
            onClick={() => setServerStatus('Ready')}
            disabled={loading}
          >
            ⏸️ Set Ready
          </button>
          
          <button
            className="btn btn-danger"
            onClick={() => setServerStatus('Error')}
            disabled={loading}
          >
            ⚠️ Set Error
          </button>
        </div>
      </div>



      <div className="card">
        <h3>Complete CLI Settings</h3>
        <p className="text-muted">Configure all CLI parameters and submit as a complete set</p>
        
        <form onSubmit={handleCLISubmit}>
          <div className="grid grid-2">
            <div className="form-group">
              <label htmlFor="runtype">Run Type</label>
              <select
                id="runtype"
                className="form-control"
                value={cliSettings.runtype}
                onChange={(e) => handleCLIChange('runtype', e.target.value)}
              >
                {runtypeOptions.map(type => (
                  <option key={type} value={type}>{type}</option>
                ))}
              </select>
              <small>Type of run (Live, Paper, Backtest, etc.)</small>
            </div>

            <div className="form-group">
              <label htmlFor="strategy_arg">Strategy</label>
              <select
                id="strategy_arg"
                className="form-control"
                value={cliSettings.strategy_arg}
                onChange={(e) => handleCLIChange('strategy_arg', e.target.value)}
              >
                <option value="">Select strategy...</option>
                {strategies && strategies.map(strategy => (
                  <option key={strategy} value={strategy}>{strategy}</option>
                ))}
              </select>
              <small>Trading strategy to use</small>
            </div>

            <div className="form-group">
              <label htmlFor="target_type">Target Type</label>
              <select
                id="target_type"
                className="form-control"
                value={targetSettings.type}
                onChange={(e) => setTargetSettings(prev => ({ ...prev, type: e.target.value, file: '' }))}
              >
                <option value="Download">Download</option>
                <option value="File">File</option>
              </select>
              <small>Data source for the strategy</small>
            </div>

            {targetSettings.type === 'File' && (
              <div className="form-group">
                <label htmlFor="target_file">Data File</label>
                <select
                  id="target_file"
                  className="form-control"
                  value={targetSettings.file}
                  onChange={(e) => setTargetSettings(prev => ({ ...prev, file: e.target.value }))}
                >
                  <option value="">Select data file...</option>
                  {dataFiles && dataFiles.map(file => (
                    <option key={file} value={file}>{file}</option>
                  ))}
                </select>
                <small>Historical data file for backtesting</small>
              </div>
            )}

            <div className="form-group">
              <label htmlFor="start">Start Index</label>
              <input
                id="start"
                type="number"
                className="form-control"
                value={cliSettings.start}
                onChange={(e) => handleCLIChange('start', parseInt(e.target.value) || 0)}
                min="0"
              />
              <small>Starting index for backtest</small>
            </div>

            <div className="form-group">
              <label htmlFor="random_drop_chance">Random Drop Chance (%)</label>
              <input
                id="random_drop_chance"
                type="number"
                className="form-control"
                value={cliSettings.random_drop_chance}
                onChange={(e) => handleCLIChange('random_drop_chance', parseInt(e.target.value) || 0)}
                min="0"
                max="100"
              />
              <small>Chance that an order will be randomly ignored</small>
            </div>
          </div>

          <div className="grid grid-2">
            <div>
              <h4>Debugging Options</h4>
              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.stacktrace}
                    onChange={(e) => handleCLIChange('stacktrace', e.target.checked)}
                  />
                  Print stacktrace on exceptions
                </label>
              </div>

              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.print_tick_arg}
                    onChange={(e) => handleCLIChange('print_tick_arg', e.target.checked)}
                  />
                  Print current tick
                </label>
              </div>

              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.compare_preloaded}
                    onChange={(e) => handleCLIChange('compare_preloaded', e.target.checked)}
                  />
                  Compare live with preloaded indicators
                </label>
              </div>
            </div>

            <div>
              <h4>Processing Options</h4>
              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.no_gui}
                    onChange={(e) => handleCLIChange('no_gui', e.target.checked)}
                  />
                  Disable GUI process
                </label>
              </div>

              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.precompute_indicators_arg}
                    onChange={(e) => handleCLIChange('precompute_indicators_arg', e.target.checked)}
                  />
                  Precompute indicators
                </label>
              </div>

              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.nowait_market_open}
                    onChange={(e) => handleCLIChange('nowait_market_open', e.target.checked)}
                  />
                  Don't wait for market open
                </label>
              </div>
            </div>
          </div>

          <div className="grid grid-2">
            <div>
              <h4>Data Storage Options</h4>
              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.save_received}
                    onChange={(e) => handleCLIChange('save_received', e.target.checked)}
                  />
                  Save received data
                </label>
              </div>

              <div className="form-group">
                <label>
                  <input
                    type="checkbox"
                    checked={cliSettings.save_to_file}
                    onChange={(e) => handleCLIChange('save_to_file', e.target.checked)}
                  />
                  Save data to files
                </label>
              </div>
            </div>
            
            <div style={{ display: 'flex', alignItems: 'end' }}>
              <button
                type="submit"
                className="btn btn-primary"
                disabled={loading}
                style={{ height: 'fit-content', fontSize: '18px', padding: '12px 24px' }}
              >
                📤 Update All CLI Settings
              </button>
            </div>
          </div>
        </form>

        <details className="expandable" style={{ marginTop: '20px' }}>
          <summary className="expandable-header">
            📋 Current CLI Settings JSON
          </summary>
          <div className="expandable-content">
            <pre className="code-block">
              {JSON.stringify(cliSettings, null, 2)}
            </pre>
          </div>
        </details>
      </div>

      {loading && (
        <div className="loading">
          <div className="spinner"></div>
        </div>
      )}
    </div>
  );
};

export default ControlTab;
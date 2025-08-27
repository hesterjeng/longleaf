import React, { useState } from 'react';
import axios from 'axios';

const DataTab = ({ serverData, refreshData }) => {
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState(null);

  const { dataFiles } = serverData;

  const showMessage = (msg, type = 'info') => {
    setMessage({ text: msg, type });
    setTimeout(() => setMessage(null), 5000);
  };

  const selectFile = async (filePath) => {
    setLoading(true);
    try {
      const response = await axios.post(
        '/set_target',
        JSON.stringify(['File', filePath]),
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 5000
        }
      );

      if (response.status === 406) {
        showMessage(`Server rejected request (406): ${response.data}`, 'danger');
      } else {
        showMessage(`Target set to: ${filePath}`, 'success');
        refreshData();
      }
    } catch (error) {
      if (error.response?.status === 406) {
        showMessage(`Server error (406 Not Acceptable): ${error.response.data}`, 'danger');
      } else {
        showMessage(`Failed to set target: ${error.message}`, 'danger');
      }
    } finally {
      setLoading(false);
    }
  };

  const renderInstructions = () => (
    <details className="expandable">
      <summary className="expandable-header">
        📖 Data File Instructions
      </summary>
      <div className="expandable-content">
        <p><strong>Data files</strong> are historical market data files used for backtesting and analysis.</p>
        
        <ul>
          <li>Files are typically JSON format containing OHLCV data</li>
          <li>Use the <strong>longleaf_downloader</strong> tool to create data files</li>
          <li>Select a file to set it as the target for backtesting</li>
        </ul>

        <h4>Example commands:</h4>
        <pre className="code-block">
{`# Download data for backtesting
longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 \\
    --interval=10 --timeframe=minute data/24.json`}
        </pre>
      </div>
    </details>
  );

  return (
    <div>
      <h2>📁 Data Files</h2>

      {message && (
        <div className={`alert alert-${message.type}`}>
          {message.text}
        </div>
      )}

      {dataFiles && dataFiles.length > 0 ? (
        <div>
          <div className="card">
            <h3>Available Data Files</h3>
            
            <div className="file-list">
              {dataFiles.map((filePath, index) => (
                <div key={filePath} className="file-item">
                  <div style={{ flex: 1 }}>
                    <strong>{index + 1}.</strong> <code>{filePath}</code>
                  </div>
                  <button
                    className="btn btn-primary btn-sm"
                    onClick={() => selectFile(filePath)}
                    disabled={loading}
                  >
                    Select
                  </button>
                </div>
              ))}
            </div>

            <div className="alert alert-info" style={{ marginTop: '20px' }}>
              Total data files: {dataFiles.length}
            </div>
          </div>
          
          {renderInstructions()}
        </div>
      ) : (
        <div>
          <div className="alert alert-warning">
            No data files found
          </div>
          
          {renderInstructions()}
        </div>
      )}

      {loading && (
        <div className="loading">
          <div className="spinner"></div>
        </div>
      )}
    </div>
  );
};

export default DataTab;
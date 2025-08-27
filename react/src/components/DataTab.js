import React from 'react';
import { getActiveDataFile } from '../utils/oclFormat';

const DataTab = ({ serverData }) => {
  const { dataFiles, settings } = serverData;

  const currentTarget = getActiveDataFile(settings?.target);

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

      <div className="card">
        <h3>Current Target</h3>
        {currentTarget ? (
          <div className="alert alert-success">
            <strong>📄 Active Data File:</strong> <code>{currentTarget}</code>
          </div>
        ) : (
          <div className="alert alert-info">
            <strong>📥 Target:</strong> Download (live data)
          </div>
        )}
        <p className="text-muted">
          <strong>💡 To change the target:</strong> Use the <strong>Control</strong> tab → <strong>Complete CLI Settings</strong> form
        </p>
      </div>

      {dataFiles && dataFiles.length > 0 ? (
        <div className="card">
          <h3>Available Data Files ({dataFiles.length})</h3>
          
          <div className="file-list">
            {dataFiles.map((filePath, index) => (
              <div key={filePath} className="file-item">
                <div style={{ flex: 1 }}>
                  <strong>{index + 1}.</strong> <code>{filePath}</code>
                  {currentTarget === filePath && (
                    <span style={{ marginLeft: '10px', color: '#28a745', fontWeight: 'bold' }}>
                      ✅ ACTIVE
                    </span>
                  )}
                </div>
              </div>
            ))}
          </div>
        </div>
      ) : (
        <div className="alert alert-warning">
          No data files found
        </div>
      )}
      
      {renderInstructions()}
    </div>
  );
};

export default DataTab;
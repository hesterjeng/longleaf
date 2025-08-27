import React, { useState } from 'react';
import axios from 'axios';

const StrategiesTab = ({ serverData, refreshData }) => {
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState(null);

  const { strategies } = serverData;

  const showMessage = (msg, type = 'info') => {
    setMessage({ text: msg, type });
    setTimeout(() => setMessage(null), 5000);
  };

  const selectStrategy = async (strategy) => {
    setLoading(true);
    try {
      const response = await axios.post(
        '/set_strategy',
        `"${strategy}"`,
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 5000
        }
      );

      if (response.status === 406) {
        showMessage(`Server rejected request (406): ${response.data}`, 'danger');
      } else {
        showMessage(`Strategy selected: ${strategy}`, 'success');
        refreshData();
      }
    } catch (error) {
      if (error.response?.status === 406) {
        showMessage(`Server error (406 Not Acceptable): ${error.response.data}`, 'danger');
      } else {
        showMessage(`Failed to select strategy: ${error.message}`, 'danger');
      }
    } finally {
      setLoading(false);
    }
  };

  const renderDevelopmentGuide = () => (
    <details className="expandable">
      <summary className="expandable-header">
        📖 Strategy Development Guide
      </summary>
      <div className="expandable-content">
        <p><strong>Strategies</strong> are trading algorithms implemented in OCaml using the Longleaf framework.</p>

        <h4>Key Components:</h4>
        <ul>
          <li><strong>Buy Trigger</strong>: Defines entry conditions</li>
          <li><strong>Sell Trigger</strong>: Defines exit conditions</li>
          <li><strong>Template System</strong>: Use <code>Template.Make</code> functor for consistency</li>
        </ul>

        <h4>Development Workflow:</h4>
        <ol>
          <li>Create strategy file in <code>strategies/</code> directory</li>
          <li>Implement using Template.Make functor</li>
          <li>Add to <code>longleaf_strategies.ml</code> registry</li>
          <li>Test with backtesting</li>
          <li>Deploy to paper/live trading</li>
        </ol>

        <h4>Example Strategy Structure:</h4>
        <pre className="code-block">
{`module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass state symbol = (* entry logic *)
  let score state symbol = (* ranking logic *)
  let num_positions = 5
end

module Sell : Template.Sell_trigger.S = struct
  let make state ~buying_order = (* exit logic *)
end

module Make : Strategy.BUILDER = Template.Make (Buy_inp) (Sell)`}
        </pre>
      </div>
    </details>
  );

  return (
    <div>
      <h2>🧠 Available Strategies</h2>

      {message && (
        <div className={`alert alert-${message.type}`}>
          {message.text}
        </div>
      )}

      {strategies && strategies.length > 0 ? (
        <div>
          <div className="card">
            <h3>Strategy List</h3>
            
            <div className="file-list">
              {strategies.map((strategy, index) => (
                <div key={strategy} className="file-item">
                  <div style={{ flex: 1 }}>
                    <strong>{index + 1}.</strong> <code>{strategy}</code>
                  </div>
                  <button
                    className="btn btn-primary btn-sm"
                    onClick={() => selectStrategy(strategy)}
                    disabled={loading}
                  >
                    Select
                  </button>
                </div>
              ))}
            </div>

            <div className="alert alert-info" style={{ marginTop: '20px' }}>
              Total strategies: {strategies.length}
            </div>
          </div>

          {renderDevelopmentGuide()}
        </div>
      ) : (
        <div>
          <div className="alert alert-warning">
            No strategies found
          </div>
          
          {renderDevelopmentGuide()}
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

export default StrategiesTab;
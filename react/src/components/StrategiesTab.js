import React from 'react';

const StrategiesTab = ({ serverData }) => {
  const { strategies, settings } = serverData;
  const currentStrategy = settings?.cli_vars?.strategy_arg || null;

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

      <div className="card">
        <h3>Current Strategy</h3>
        {currentStrategy ? (
          <div className="alert alert-success">
            <strong>🎯 Active Strategy:</strong> <code>{currentStrategy}</code>
          </div>
        ) : (
          <div className="alert alert-warning">
            <strong>⚠️ No strategy selected</strong>
          </div>
        )}
        <p className="text-muted">
          <strong>💡 To change the strategy:</strong> Use the <strong>Control</strong> tab → <strong>Complete CLI Settings</strong> form
        </p>
      </div>

      {strategies && strategies.length > 0 ? (
        <div className="card">
          <h3>Available Strategies ({strategies.length})</h3>
          
          <div className="file-list">
            {strategies.map((strategy, index) => (
              <div key={strategy} className="file-item">
                <div style={{ flex: 1 }}>
                  <strong>{index + 1}.</strong> <code>{strategy}</code>
                  {currentStrategy === strategy && (
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
          No strategies found
        </div>
      )}
      
      {renderDevelopmentGuide()}
    </div>
  );
};

export default StrategiesTab;
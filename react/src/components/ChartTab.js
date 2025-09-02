import React, { useState } from 'react';
import Plot from 'react-plotly.js';
import axios from 'axios';

const ChartTab = ({ serverData }) => {
  const [selectedSymbol, setSelectedSymbol] = useState('');
  const [chartData, setChartData] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const { symbols } = serverData;

  const fetchChartData = async (symbol) => {
    if (!symbol) return;

    setLoading(true);
    setError(null);
    
    try {
      const response = await axios.get(`/data/${symbol.toUpperCase()}/json`, { 
        timeout: 10000 
      });
      setChartData(response.data);
    } catch (err) {
      if (err.response?.status === 404) {
        setError(`Chart data not yet available for ${symbol}`);
      } else {
        setError(`Error fetching chart data for ${symbol}: ${err.message}`);
      }
      setChartData(null);
    } finally {
      setLoading(false);
    }
  };

  const handleLoadChart = () => {
    if (selectedSymbol) {
      fetchChartData(selectedSymbol);
    }
  };

  const renderChart = () => {
    if (!chartData || !chartData.traces || !chartData.layout) {
      return null;
    }

    // Convert server chart data to Plotly format
    const traces = chartData.traces.map(trace => ({
      x: trace.x || [],
      y: trace.y || [],
      type: trace.type === 'scatter' ? 'scatter' : 'scatter',
      mode: trace.mode || 'lines',
      name: trace.name || '',
      line: {
        color: trace.line?.color || '#1f77b4',
        dash: trace.line?.dash || 'solid',
        width: trace.line?.width || 1
      },
      visible: trace.visible !== false,
      yaxis: trace.yaxis || 'y',
      marker: trace.marker || {},
      hovertext: trace.hovertext || [],
      hoverinfo: trace.hoverinfo || 'x+y+name'
    }));

    const layout = {
      title: chartData.layout.title || `${selectedSymbol} Price Chart`,
      xaxis: chartData.layout.xaxis || {},
      yaxis: chartData.layout.yaxis || {},
      yaxis2: chartData.layout.yaxis2 || {},
      hovermode: chartData.layout.hovermode || 'x',
      height: 600,
      showlegend: true,
      ...chartData.layout
    };

    return (
      <div>
        <Plot
          data={traces}
          layout={layout}
          style={{ width: '100%', height: '600px' }}
          config={{ responsive: true }}
        />
        
        <div className="grid grid-3" style={{ marginTop: '20px' }}>
          <div className="card">
            <div className="metric">
              <div className="metric-value">{selectedSymbol}</div>
              <div className="metric-label">Symbol</div>
            </div>
          </div>
          <div className="card">
            <div className="metric">
              <div className="metric-value">{traces.length - 1}</div>
              <div className="metric-label">Indicators</div>
            </div>
          </div>
          <div className="card">
            <div className="metric">
              <div className="metric-value">
                {traces.length > 0 ? traces[0].x.length : 0}
              </div>
              <div className="metric-label">Data Points</div>
            </div>
          </div>
        </div>

        <details className="expandable" style={{ marginTop: '20px' }}>
          <summary className="expandable-header">
            🔍 Raw Chart Data
          </summary>
          <div className="expandable-content">
            <pre className="code-block">
              {JSON.stringify(chartData, null, 2)}
            </pre>
          </div>
        </details>
      </div>
    );
  };

  const renderInstructions = () => (
    <details className="expandable">
      <summary className="expandable-header">
        📖 Chart Instructions
      </summary>
      <div className="expandable-content">
        <h4>How to use the charts:</h4>
        <ol>
          <li><strong>Select Symbol</strong>: Choose from available symbols in the current data file</li>
          <li><strong>Load Chart</strong>: Click to fetch and display the price chart</li>
          <li><strong>Interactive Features</strong>:
            <ul>
              <li>Zoom: Click and drag to zoom into specific time periods</li>
              <li>Pan: Hold shift and drag to pan across the chart</li>
              <li>Legend: Click legend items to show/hide indicators</li>
              <li>Hover: Hover over data points for detailed information</li>
            </ul>
          </li>
        </ol>

        <h4>Chart Components:</h4>
        <ul>
          <li><strong>Price Line</strong>: Main price data (usually in blue)</li>
          <li><strong>Technical Indicators</strong>: Additional analysis overlays</li>
          <li><strong>Buy/Sell Markers</strong>: Trading signals if available</li>
          <li><strong>Secondary Y-axis</strong>: For oscillators and normalized indicators</li>
        </ul>

        <p><strong>Note</strong>: Charts display data from the currently selected target file.
        Set a data file in the Control tab to enable charting.</p>
      </div>
    </details>
  );

  return (
    <div>
      <h2>📈 Price Charts</h2>
      
      {symbols && symbols.length > 0 ? (
        <div>
          <div className="grid grid-2" style={{ marginBottom: '20px' }}>
            <div className="form-group">
              <label htmlFor="symbol-select">Select Symbol</label>
              <select
                id="symbol-select"
                className="form-control"
                value={selectedSymbol}
                onChange={(e) => setSelectedSymbol(e.target.value)}
              >
                <option value="">Choose a symbol...</option>
                {symbols.map(symbol => (
                  <option key={symbol} value={symbol}>
                    {symbol}
                  </option>
                ))}
              </select>
              <small>Choose a symbol to view its price chart with technical indicators</small>
            </div>
            
            <div style={{ display: 'flex', alignItems: 'end' }}>
              <button
                className="btn btn-primary"
                onClick={handleLoadChart}
                disabled={!selectedSymbol || loading}
                style={{ height: 'fit-content' }}
              >
                {loading ? '⏳ Loading...' : '📊 Load Chart'}
              </button>
            </div>
          </div>

          {error && (
            <div className="alert alert-warning">
              {error}
            </div>
          )}

          {loading && (
            <div className="loading">
              <div className="spinner"></div>
              <p>Loading chart for {selectedSymbol}...</p>
            </div>
          )}

          {chartData && renderChart()}
          
          {renderInstructions()}
        </div>
      ) : (
        <div>
          <div className="alert alert-warning">
            ⚠️ Chart functionality not yet available
          </div>
          
          <div className="alert alert-info">
            <strong>💡 Chart Implementation Status:</strong>
            <p>The chart endpoints are currently under development:</p>
            <ul>
              <li><strong>/symbols</strong> endpoint: Not yet implemented for File targets</li>
              <li><strong>/data/:symbol/json</strong> endpoint: Returns 404 (not yet implemented)</li>
            </ul>
            
            <p><strong>When implemented, you will be able to:</strong></p>
            <ol>
              <li>Set a data file target in the <strong>Control</strong> tab</li>
              <li>View available symbols from the data file</li>
              <li>Generate interactive price charts with technical indicators</li>
            </ol>
            
            <p><strong>Available data files can be found in the Data Files tab</strong></p>
          </div>
        </div>
      )}
    </div>
  );
};

export default ChartTab;
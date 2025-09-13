from flask import Flask, request, jsonify, send_from_directory
import quantstats as qs
import pandas as pd
import tempfile
import os

app = Flask(__name__)

@app.route('/')
def index():
    return '''
    <h1>QuantStats Server</h1>
    <p>Available endpoints:</p>
    <ul>
        <li><code>POST /analyze</code> - Analyze portfolio returns</li>
        <li><code>POST /report</code> - Generate HTML report</li>
        <li><code>GET /metrics/&lt;symbol&gt;</code> - Get basic metrics for a symbol</li>
    </ul>
    '''

@app.route('/analyze', methods=['POST'])
def analyze():
    try:
        data = request.json
        returns = pd.Series(data['returns'], index=pd.to_datetime(data['dates']))
        
        metrics = {
            'sharpe': qs.stats.sharpe(returns),
            'sortino': qs.stats.sortino(returns),
            'max_drawdown': qs.stats.max_drawdown(returns),
            'cagr': qs.stats.cagr(returns),
            'volatility': qs.stats.volatility(returns),
            'calmar': qs.stats.calmar(returns)
        }
        
        return jsonify(metrics)
    except Exception as e:
        return jsonify({'error': str(e)}), 400

@app.route('/report', methods=['POST'])
def generate_report():
    try:
        data = request.json
        returns = pd.Series(data['returns'], index=pd.to_datetime(data['dates']))
        benchmark = data.get('benchmark', 'SPY')
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            qs.reports.html(returns, benchmark, output=f.name, title=data.get('title', 'Portfolio Analysis'))
            
        with open(f.name, 'r') as report_file:
            html_content = report_file.read()
            
        os.unlink(f.name)
        
        return html_content, 200, {'Content-Type': 'text/html'}
    except Exception as e:
        return jsonify({'error': str(e)}), 400

@app.route('/metrics/<symbol>')
def get_symbol_metrics(symbol):
    try:
        returns = qs.utils.download_returns(symbol)
        
        metrics = {
            'symbol': symbol,
            'sharpe': qs.stats.sharpe(returns),
            'sortino': qs.stats.sortino(returns),
            'max_drawdown': qs.stats.max_drawdown(returns),
            'cagr': qs.stats.cagr(returns),
            'volatility': qs.stats.volatility(returns)
        }
        
        return jsonify(metrics)
    except Exception as e:
        return jsonify({'error': str(e)}), 400

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)

    # run with uv --with...

from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import List, Optional
import pandas as pd
import tempfile
import os
import sys

# Import quantstats
import quantstats as qs

app = FastAPI(title="QuantStats Server", description="Portfolio analytics service")

class AnalyzeRequest(BaseModel):
    returns: List[float]
    dates: List[str]
    periods: Optional[int] = 9828  # Default for 10-minute ticks (6 * 6.5 * 252)

class ReportRequest(BaseModel):
    returns: List[float]
    dates: List[str]
    benchmark_returns: Optional[List[float]] = None
    benchmark_dates: Optional[List[str]] = None
    benchmark_name: Optional[str] = "SPY"
    title: Optional[str] = "Portfolio Analysis"
    periods: Optional[int] = 9828  # Default for 10-minute ticks (6 * 6.5 * 252)

class TearsheetRequest(BaseModel):
    returns: List[float]
    dates: List[str]
    benchmark_returns: Optional[List[float]] = None
    benchmark_dates: Optional[List[str]] = None
    benchmark_name: Optional[str] = "SPY"
    title: Optional[str] = "Longleaf Trading Strategy Tearsheet"
    periods: Optional[int] = 9828  # Default for 10-minute ticks (6 * 6.5 * 252)

@app.get("/")
async def root():
    return {
        "message": "QuantStats Server",
        "endpoints": {
            "POST /analyze": "Analyze portfolio returns",
            "POST /report": "Generate HTML report", 
            "POST /tearsheet": "Generate complete tearsheet report",
            "GET /metrics/{symbol}": "Get basic metrics for a symbol"
        }
    }

@app.post("/analyze")
async def analyze(request: AnalyzeRequest):
    try:
        returns = pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        metrics = {
            'sharpe': float(qs.stats.sharpe(returns, periods=request.periods)),
            'sortino': float(qs.stats.sortino(returns, periods=request.periods)),
            'max_drawdown': float(qs.stats.max_drawdown(returns)),
            'cagr': float(qs.stats.cagr(returns, periods=request.periods)),
            'volatility': float(qs.stats.volatility(returns, periods=request.periods)),
            'calmar': float(qs.stats.calmar(returns, periods=request.periods))
        }
        
        return metrics
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/report", response_class=HTMLResponse)
async def generate_report(request: ReportRequest):
    try:
        returns = pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        # Prepare benchmark data if provided
        benchmark = None
        if request.benchmark_returns and request.benchmark_dates:
            benchmark = pd.Series(request.benchmark_returns, index=pd.to_datetime(request.benchmark_dates))
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            # Use the correct quantstats API with periods parameter and benchmark data
            qs.reports.html(returns, benchmark=benchmark, output=f.name, title=request.title, periods_per_year=request.periods)
            
        with open(f.name, 'r') as report_file:
            html_content = report_file.read()
            
        os.unlink(f.name)
        
        return HTMLResponse(content=html_content)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/tearsheet", response_class=HTMLResponse)
async def generate_tearsheet(request: TearsheetRequest):
    try:
        returns = pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        # Prepare benchmark data if provided
        benchmark = None
        if request.benchmark_returns and request.benchmark_dates:
            benchmark = pd.Series(request.benchmark_returns, index=pd.to_datetime(request.benchmark_dates))
        
        # Generate full tearsheet with quantstats
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            # Use the correct quantstats API with explicit parameters including periods and benchmark data
            qs.reports.html(returns, benchmark=benchmark, output=f.name, title=request.title, periods_per_year=request.periods)
            
        with open(f.name, 'r') as report_file:
            html_content = report_file.read()
            
        os.unlink(f.name)
        
        return HTMLResponse(content=html_content)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.get("/metrics/{symbol}")
async def get_symbol_metrics(symbol: str):
    try:
        returns = qs.utils.download_returns(symbol)
        
        metrics = {
            'symbol': symbol,
            'sharpe': float(qs.stats.sharpe(returns)),
            'sortino': float(qs.stats.sortino(returns)),
            'max_drawdown': float(qs.stats.max_drawdown(returns)),
            'cagr': float(qs.stats.cagr(returns)),
            'volatility': float(qs.stats.volatility(returns))
        }
        
        return metrics
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

if __name__ == '__main__':
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=5000)

    # run with uv --with...

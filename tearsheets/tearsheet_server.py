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

def align_benchmark_to_strategy(strategy_returns, strategy_dates, benchmark_returns, benchmark_dates):
    """
    Align benchmark data to strategy data by simple length matching.
    Returns both aligned strategy and benchmark series with matching lengths.
    """
    if not benchmark_returns or not benchmark_dates:
        return None, None
    
    # Debug logging
    print(f"Strategy: {len(strategy_returns)} returns, {len(strategy_dates)} dates")
    print(f"Benchmark: {len(benchmark_returns)} returns, {len(benchmark_dates)} dates")
    
    # Ensure input data is consistent first, fix if needed
    if len(strategy_returns) != len(strategy_dates):
        raise ValueError(f"Strategy data mismatch: {len(strategy_returns)} returns vs {len(strategy_dates)} dates")
    
    # Fix benchmark data mismatch by truncating longer array
    if len(benchmark_returns) != len(benchmark_dates):
        print(f"Warning: Benchmark data mismatch, truncating to match")
        min_benchmark_length = min(len(benchmark_returns), len(benchmark_dates))
        benchmark_returns = benchmark_returns[:min_benchmark_length]
        benchmark_dates = benchmark_dates[:min_benchmark_length]
        print(f"Fixed benchmark lengths - Returns: {len(benchmark_returns)}, Dates: {len(benchmark_dates)}")
    
    # Take the shorter length to ensure alignment
    min_length = min(len(strategy_returns), len(benchmark_returns))
    print(f"Using min_length: {min_length}")
    
    # Truncate both to same length (from the end, assuming benchmark starts earlier)
    aligned_strategy_returns = strategy_returns[-min_length:]
    aligned_strategy_dates = strategy_dates[-min_length:]
    aligned_benchmark_returns = benchmark_returns[-min_length:]
    aligned_benchmark_dates = benchmark_dates[-min_length:]
    
    # Create series with matching lengths
    strategy_series = pd.Series(aligned_strategy_returns, index=pd.to_datetime(aligned_strategy_dates))
    benchmark_series = pd.Series(aligned_benchmark_returns, index=pd.to_datetime(aligned_benchmark_dates))
    
    print(f"Final lengths - Strategy: {len(strategy_series)}, Benchmark: {len(benchmark_series)}")
    
    return strategy_series, benchmark_series

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
        # Align strategy and benchmark data
        aligned_returns, benchmark = align_benchmark_to_strategy(
            request.returns, request.dates, 
            request.benchmark_returns, request.benchmark_dates
        )
        
        # Use aligned returns or fall back to original if no benchmark
        returns = aligned_returns if aligned_returns is not None else pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            # Use the correct quantstats API with periods parameter and benchmark data
            if benchmark is not None:
                qs.reports.html(returns, benchmark=benchmark, output=f.name, title=request.title, periods_per_year=request.periods)
            else:
                qs.reports.html(returns, output=f.name, title=request.title, periods_per_year=request.periods)
            
        with open(f.name, 'r') as report_file:
            html_content = report_file.read()
            
        os.unlink(f.name)
        
        return HTMLResponse(content=html_content)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/tearsheet", response_class=HTMLResponse)
async def generate_tearsheet(request: TearsheetRequest):
    try:
        # Align strategy and benchmark data  
        aligned_returns, benchmark = align_benchmark_to_strategy(
            request.returns, request.dates, 
            request.benchmark_returns, request.benchmark_dates
        )
        
        # Use aligned returns or fall back to original if no benchmark
        returns = aligned_returns if aligned_returns is not None else pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        # Generate full tearsheet with quantstats
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            # Use the correct quantstats API with explicit parameters including periods and benchmark data
            if benchmark is not None:
                qs.reports.html(returns, benchmark=benchmark, output=f.name, title=request.title, periods_per_year=request.periods)
            else:
                qs.reports.html(returns, output=f.name, title=request.title, periods_per_year=request.periods)
            
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

from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import List, Optional
import quantstats as qs
import pandas as pd
import tempfile
import os

app = FastAPI(title="QuantStats Server", description="Portfolio analytics service")

class AnalyzeRequest(BaseModel):
    returns: List[float]
    dates: List[str]

class ReportRequest(BaseModel):
    returns: List[float]
    dates: List[str]
    benchmark: Optional[str] = "SPY"
    title: Optional[str] = "Portfolio Analysis"

class TearsheetRequest(BaseModel):
    returns: List[float]
    dates: List[str]
    benchmark: Optional[str] = "SPY"
    title: Optional[str] = "Longleaf Trading Strategy Tearsheet"

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
            'sharpe': float(qs.stats.sharpe(returns)),
            'sortino': float(qs.stats.sortino(returns)),
            'max_drawdown': float(qs.stats.max_drawdown(returns)),
            'cagr': float(qs.stats.cagr(returns)),
            'volatility': float(qs.stats.volatility(returns)),
            'calmar': float(qs.stats.calmar(returns))
        }
        
        return metrics
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/report", response_class=HTMLResponse)
async def generate_report(request: ReportRequest):
    try:
        returns = pd.Series(request.returns, index=pd.to_datetime(request.dates))
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            qs.reports.html(returns, request.benchmark, output=f.name, title=request.title)
            
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
        
        # Generate full tearsheet with quantstats
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            qs.reports.full(returns, request.benchmark, output=f.name, title=request.title)
            
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

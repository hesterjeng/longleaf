import streamlit as st
import requests
import plotly.graph_objects as go
import pandas as pd
import hashlib
import os
import json

st.set_page_config(layout="wide")

# Indicator metadata structure
INDICATORS = {
    "Unit Constructors": {
        "Price Indicators": {
            "Avgprice": {"format": "Avgprice ()", "description": "Average Price"},
            "Medprice": {"format": "Medprice ()", "description": "Median Price"},
            "Typprice": {"format": "Typprice ()", "description": "Typical Price"},
            "Wclprice": {"format": "Wclprice ()", "description": "Weighted Close Price"},
            "Ad": {"format": "Ad ()", "description": "Accumulation/Distribution Line"},
            "Obv": {"format": "Obv ()", "description": "On Balance Volume"},
            "Bop": {"format": "Bop ()", "description": "Balance of Power"},
            "Trange": {"format": "Trange ()", "description": "True Range"},
        },
        "Hilbert Transform": {
            "Ht_dcperiod": {"format": "Ht_dcperiod ()", "description": "Dominant Cycle Period"},
            "Ht_dcphase": {"format": "Ht_dcphase ()", "description": "Dominant Cycle Phase"},
            "Ht_trendline": {"format": "Ht_trendline ()", "description": "Trend Line"},
            "Ht_trendmode": {"format": "Ht_trendmode ()", "description": "Trend Mode"},
            "Ht_phasor": {"format": "Ht_phasor ()", "description": "Phasor Components"},
            "Ht_sine": {"format": "Ht_sine ()", "description": "Sine Wave"},
        }
    },
    "Timeperiod Indicators": {
        "Moving Averages": {
            "Sma": {"params": ["timeperiod"], "defaults": [20], "description": "Simple Moving Average"},
            "Ema": {"params": ["timeperiod"], "defaults": [20], "description": "Exponential Moving Average"},
            "Dema": {"params": ["timeperiod"], "defaults": [30], "description": "Double Exponential Moving Average"},
            "Tema": {"params": ["timeperiod"], "defaults": [30], "description": "Triple Exponential Moving Average"},
            "Trima": {"params": ["timeperiod"], "defaults": [30], "description": "Triangular Moving Average"},
            "Wma": {"params": ["timeperiod"], "defaults": [30], "description": "Weighted Moving Average"},
            "Kama": {"params": ["timeperiod"], "defaults": [30], "description": "Kaufman Adaptive Moving Average"},
        },
        "Oscillators": {
            "Rsi": {"params": ["timeperiod"], "defaults": [14], "description": "Relative Strength Index"},
            "Cci": {"params": ["timeperiod"], "defaults": [20], "description": "Commodity Channel Index"},
            "Cmo": {"params": ["timeperiod"], "defaults": [14], "description": "Chande Momentum Oscillator"},
            "Willr": {"params": ["timeperiod"], "defaults": [14], "description": "Williams' %R"},
            "Mfi": {"params": ["timeperiod"], "defaults": [14], "description": "Money Flow Index"},
        },
        "Trend Indicators": {
            "Adx": {"params": ["timeperiod"], "defaults": [14], "description": "Average Directional Movement Index"},
            "Adxr": {"params": ["timeperiod"], "defaults": [14], "description": "Average Directional Movement Index Rating"},
            "Aroon": {"params": ["timeperiod"], "defaults": [14], "description": "Aroon"},
            "Aroonosc": {"params": ["timeperiod"], "defaults": [14], "description": "Aroon Oscillator"},
            "Dx": {"params": ["timeperiod"], "defaults": [14], "description": "Directional Movement Index"},
            "Minus_di": {"params": ["timeperiod"], "defaults": [14], "description": "Minus Directional Indicator"},
            "Plus_di": {"params": ["timeperiod"], "defaults": [14], "description": "Plus Directional Indicator"},
        },
        "Volatility": {
            "Atr": {"params": ["timeperiod"], "defaults": [14], "description": "Average True Range"},
            "Natr": {"params": ["timeperiod"], "defaults": [14], "description": "Normalized Average True Range"},
        },
        "Momentum": {
            "Mom": {"params": ["timeperiod"], "defaults": [10], "description": "Momentum"},
            "Roc": {"params": ["timeperiod"], "defaults": [10], "description": "Rate of Change"},
            "Rocp": {"params": ["timeperiod"], "defaults": [10], "description": "Rate of Change Percentage"},
            "Rocr": {"params": ["timeperiod"], "defaults": [10], "description": "Rate of Change Ratio"},
            "Rocr100": {"params": ["timeperiod"], "defaults": [10], "description": "Rate of Change Ratio 100 Scale"},
        }
    },
    "Advanced Indicators": {
        "Multi-Parameter": {
            "Ma": {"params": ["timeperiod", "ma_type"], "defaults": [14, "SMA"], "description": "Moving Average with Type"},
            "Macd": {"params": ["fast_period", "slow_period", "signal_period"], "defaults": [12, 26, 9], "description": "MACD"},
            "Apo": {"params": ["fast_period", "slow_period", "ma_type"], "defaults": [12, 26, "SMA"], "description": "Absolute Price Oscillator"},
            "Ppo": {"params": ["fast_period", "slow_period", "ma_type"], "defaults": [12, 26, "SMA"], "description": "Percentage Price Oscillator"},
            "Adosc": {"params": ["fast_period", "slow_period"], "defaults": [3, 10], "description": "Accumulation/Distribution Oscillator"},
            "Bbands": {"params": ["timeperiod", "nb_dev_up", "nb_dev_dn", "ma_type"], "defaults": [20, 2.0, 2.0, "SMA"], "description": "Bollinger Bands"},
            "Sar": {"params": ["acceleration", "maximum"], "defaults": [0.02, 0.2], "description": "Parabolic SAR"},
            "Stoch": {"params": ["fast_k_period", "slow_k_period", "slow_k_ma_type", "slow_d_period", "slow_d_ma_type"], 
                     "defaults": [5, 3, "SMA", 3, "SMA"], "description": "Stochastic"},
            "Stochf": {"params": ["fast_k_period", "fast_d_period", "fast_d_ma_type"], "defaults": [5, 3, "SMA"], "description": "Stochastic Fast"},
            "Stochrsi": {"params": ["timeperiod", "fast_k_period", "fast_d_period", "fast_d_ma_type"], 
                        "defaults": [14, 5, 3, "SMA"], "description": "Stochastic RSI"},
            "T3": {"params": ["timeperiod", "v_factor"], "defaults": [5, 0.7], "description": "T3 Moving Average"},
            "Ultosc": {"params": ["timeperiod1", "timeperiod2", "timeperiod3"], "defaults": [7, 14, 28], "description": "Ultimate Oscillator"},
            "Stddev": {"params": ["timeperiod", "nb_dev"], "defaults": [5, 1.0], "description": "Standard Deviation"},
            "Var": {"params": ["timeperiod", "nb_dev"], "defaults": [5, 1.0], "description": "Variance"},
        }
    }
}

MA_TYPES = ["SMA", "EMA", "WMA", "DEMA", "TEMA", "TRIMA", "KAMA", "MAMA", "T3"]

def generate_tacaml_string(indicator_name, params_dict):
    """Generate proper Tacaml string from indicator name and parameters"""
    if not params_dict:
        return f"{indicator_name} ()"
    
    # Build parameter string
    param_parts = []
    for key, value in params_dict.items():
        if isinstance(value, str):
            param_parts.append(f"{key} = {value}")
        elif isinstance(value, float):
            param_parts.append(f"{key} = {value}")
        else:
            param_parts.append(f"{key} = {value}")
    
    param_string = "; ".join(param_parts)
    return f"{indicator_name} {{ {param_string} }}"

def render_indicator_form(category, subcategory, indicator_name, config):
    """Render form for a specific indicator"""
    if "format" in config:
        # Unit constructor - no parameters
        st.write(f"**{indicator_name}**: {config['description']}")
        st.code(config["format"])
        return config["format"]
    
    # Parameterized indicator
    st.write(f"**{indicator_name}**: {config['description']}")
    
    params = config.get("params", [])
    defaults = config.get("defaults", [])
    
    if not params:
        return f"{indicator_name} ()"
    
    params_dict = {}
    
    for i, param in enumerate(params):
        default_val = defaults[i] if i < len(defaults) else None
        
        if param == "ma_type":
            params_dict[param] = st.selectbox(
                f"{param.replace('_', ' ').title()}", 
                MA_TYPES, 
                index=MA_TYPES.index(default_val) if default_val in MA_TYPES else 0,
                key=f"{indicator_name}_{param}"
            )
        elif "period" in param.lower():
            params_dict[param] = st.number_input(
                f"{param.replace('_', ' ').title()}", 
                min_value=1, max_value=200, 
                value=int(default_val) if default_val else 14,
                key=f"{indicator_name}_{param}"
            )
        elif any(x in param.lower() for x in ["acceleration", "maximum", "factor", "limit", "dev"]):
            params_dict[param] = st.number_input(
                f"{param.replace('_', ' ').title()}", 
                min_value=0.0, max_value=10.0, 
                value=float(default_val) if default_val else 1.0,
                step=0.01,
                key=f"{indicator_name}_{param}"
            )
        else:
            # Generic parameter
            if isinstance(default_val, (int, float)):
                params_dict[param] = st.number_input(
                    f"{param.replace('_', ' ').title()}", 
                    value=default_val,
                    key=f"{indicator_name}_{param}"
                )
            else:
                params_dict[param] = st.text_input(
                    f"{param.replace('_', ' ').title()}", 
                    value=str(default_val) if default_val else "",
                    key=f"{indicator_name}_{param}"
                )
    
    tacaml_string = generate_tacaml_string(indicator_name, params_dict)
    st.code(tacaml_string)
    return tacaml_string

# Authentication
def check_password():
    if "password_correct" not in st.session_state:
        expected_hash = os.getenv("DASHBOARD_PASSWORD_HASH")
        if expected_hash:
            st.session_state["password_correct"] = True
            return True
        else:
            st.session_state["password_correct"] = False
    
    if st.session_state["password_correct"]:
        return True
    else:
        st.text_input("Password", type="password", key="password")
        st.info("Set DASHBOARD_PASSWORD_HASH environment variable to skip password prompt")
        return False

if not check_password():
    st.stop()

# Helper functions for state info
def fetch_state_info(server_url):
    """Fetch state information from the server"""
    try:
        response = requests.get(f"{server_url}/state-info", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching state info: {e}")
        return None

def fetch_stats_info(server_url):
    """Fetch statistics information from the server"""
    try:
        response = requests.get(f"{server_url}/stats", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching stats info: {e}")
        return None

def fetch_symbols(server_url):
    """Fetch available symbols from the server"""
    try:
        response = requests.get(f"{server_url}/symbols", timeout=5)
        response.raise_for_status()
        data = response.json()
        symbols_str = data.get("symbols", "")
        if symbols_str:
            symbols = [s.strip() for s in symbols_str.split(",") if s.strip()]
            return sorted(symbols)  # Alphabetize the symbols
        return []
    except Exception as e:
        st.error(f"Error fetching symbols: {e}")
        return ["AAPL"]  # Fallback to a default symbol

def render_state_tab(state_info, stats_info=None):
    """Render the state information tab"""
    if not state_info:
        st.error("No state information available")
        return
    
    st.subheader("System State Information")
    
    # Key metrics in columns
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric("Current State", state_info.get("current_state", "Unknown"))
    
    with col2:
        st.metric("Current Tick", state_info.get("current_tick", 0))
    
    with col3:
        st.metric("Orders Placed", state_info.get("orders_placed", 0))
    
    with col4:
        st.metric("Cash Available", f"${state_info.get('cash', 0):,.2f}")
    
    # Trading Statistics
    if stats_info:
        st.subheader("Trading Statistics")
        
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric("Total Orders", stats_info.get("num_orders", 0))
        
        with col2:
            buy_orders = stats_info.get("num_buy_orders", 0)
            sell_orders = stats_info.get("num_sell_orders", 0)
            st.metric("Buy Orders", buy_orders)
            st.metric("Sell Orders", sell_orders)
        
        with col3:
            st.metric("Total Volume", f"{stats_info.get('total_volume', 0):,}")
            st.metric("Symbols Traded", stats_info.get("symbols_traded", 0))
        
        with col4:
            st.metric("Cash Traded", f"${stats_info.get('total_cash_traded', 0):,.2f}")
            profit_loss = stats_info.get("profit_loss", 0)
            st.metric("Profit/Loss", f"${profit_loss:,.2f}", 
                     delta=f"{profit_loss:+.2f}" if profit_loss != 0 else None)
    
    # Configuration info
    st.subheader("Configuration")
    config_info = state_info.get("config", {})
    col1, col2 = st.columns(2)
    
    with col1:
        st.write(f"**Placeholder Mode**: {config_info.get('placeholder', 'Unknown')}")
    
    with col2:
        st.write(f"**Active Indicators**: {config_info.get('indicators_count', 0)}")

def render_portfolio_tab(state_info):
    """Render the portfolio information tab"""
    if not state_info:
        st.error("No state information available")
        return
    
    st.subheader("Portfolio Overview")
    
    # Cash and basic info
    col1, col2 = st.columns(2)
    
    with col1:
        st.metric("Cash Balance", f"${state_info.get('cash', 0):,.2f}")
    
    with col2:
        positions = state_info.get("positions", [])
        st.metric("Active Positions", len(positions))
    
    # Positions table
    if positions:
        st.subheader("Current Positions")
        
        # Convert to DataFrame for better display
        df = pd.DataFrame(positions)
        
        # Add value column (would need current prices for accuracy)
        st.dataframe(df, use_container_width=True)
        
        # Position summary
        long_positions = [p for p in positions if p.get("side") == "long"]
        short_positions = [p for p in positions if p.get("side") == "short"]
        
        col1, col2 = st.columns(2)
        with col1:
            st.write(f"**Long Positions**: {len(long_positions)}")
        with col2:
            st.write(f"**Short Positions**: {len(short_positions)}")
    else:
        st.info("No active positions")

# Main app
st.markdown("<h1 style='text-align: center;'>Longleaf Trading Dashboard</h1>", unsafe_allow_html=True)

# Sidebar controls
st.sidebar.header("Controls")

# Initialize server URL in session state if not present
if "server_url" not in st.session_state:
    st.session_state.server_url = "http://localhost:8080"

# Fetch available symbols and create dropdown (appears first)
available_symbols = fetch_symbols(st.session_state.server_url)
symbol = st.sidebar.selectbox("Symbol", options=available_symbols, help="Select stock symbol")
if not symbol:
    symbol = "AAPL"  # Fallback if no symbols available

# Server URL input (appears below symbol dropdown)
server_url = st.sidebar.text_input("Server URL", value=st.session_state.server_url, help="OCaml server URL")
if server_url != st.session_state.server_url:
    st.session_state.server_url = server_url
    st.rerun()  # Refresh to fetch symbols from new URL

# Custom indicator section
st.sidebar.markdown("---")
st.sidebar.subheader("Custom Indicator")
custom_indicator_enabled = st.sidebar.checkbox("Enable Custom Indicator")

custom_tacaml = None
if custom_indicator_enabled:
    st.sidebar.markdown("### Quick Select")
    quick_indicators = {
        "SMA (20)": "Sma { timeperiod = 20 }",
        "EMA (20)": "Ema { timeperiod = 20 }",
        "RSI (14)": "Rsi { timeperiod = 14 }",
        "MACD (12,26,9)": "Macd { fast_period = 12; slow_period = 26; signal_period = 9 }",
        "Bollinger Bands": "Bbands { timeperiod = 20; nb_dev_up = 2.0; nb_dev_dn = 2.0; ma_type = SMA }",
        "Custom...": None
    }
    
    quick_select = st.sidebar.selectbox("Quick Indicators", list(quick_indicators.keys()))
    
    if quick_select != "Custom...":
        custom_tacaml = quick_indicators[quick_select]
        st.sidebar.code(custom_tacaml)
    else:
        # Show comprehensive indicator builder in main area
        st.markdown("## Comprehensive Indicator Builder")
        
        # Search functionality
        search_term = st.text_input("🔍 Search Indicators", placeholder="Type indicator name...")
        
        selected_tacaml = None
        
        for category, subcategories in INDICATORS.items():
            with st.expander(f"📊 {category}", expanded=(category == "Timeperiod Indicators")):
                
                for subcategory, indicators in subcategories.items():
                    st.markdown(f"### {subcategory}")
                    
                    # Filter indicators based on search
                    filtered_indicators = indicators
                    if search_term:
                        filtered_indicators = {
                            name: config for name, config in indicators.items() 
                            if search_term.lower() in name.lower() or 
                               search_term.lower() in config.get('description', '').lower()
                        }
                    
                    if not filtered_indicators:
                        continue
                    
                    # Create columns for layout
                    cols = st.columns(2)
                    
                    for idx, (indicator_name, config) in enumerate(filtered_indicators.items()):
                        with cols[idx % 2]:
                            with st.container():
                                tacaml_result = render_indicator_form(category, subcategory, indicator_name, config)
                                
                                if st.button(f"Use {indicator_name}", key=f"use_{indicator_name}"):
                                    selected_tacaml = tacaml_result
                                    st.session_state.selected_tacaml = tacaml_result
                                    st.rerun()
        
        # Manual Tacaml input
        st.markdown("### Manual Tacaml Input")
        manual_tacaml = st.text_area(
            "Enter Tacaml Expression Manually", 
            value=st.session_state.get('selected_tacaml', ''),
            height=100,
            help="Enter any valid Tacaml expression directly"
        )
        
        custom_tacaml = manual_tacaml if manual_tacaml.strip() else selected_tacaml
    
    # Visualization options
    if custom_tacaml:
        st.sidebar.markdown("### Visualization Options")
        indicator_color = st.sidebar.color_picker("Indicator Color", value="#FF6B6B")
        indicator_yaxis = st.sidebar.selectbox("Y-Axis", ["y1 (Main)", "y2 (Secondary)"], help="Which y-axis to plot the indicator on")

# Create tabs
chart_tab, portfolio_tab, state_tab = st.tabs(["📈 Trading Chart", "💰 Portfolio", "⚙️ System State"])

# Data fetching and display
if st.sidebar.button("Fetch Data") or symbol:
    if symbol:
        # Fetch state info and stats for all tabs
        state_info = fetch_state_info(server_url)
        stats_info = fetch_stats_info(server_url)
        
        with chart_tab:
            try:
                with st.spinner(f"Fetching chart data for {symbol}..."):
                    if custom_indicator_enabled and custom_tacaml:
                        # POST request to custom indicator endpoint
                        payload = {
                            "tacaml": custom_tacaml,
                            "color": indicator_color,
                            "yaxis": "y2" if indicator_yaxis == "y2 (Secondary)" else "y1"
                        }
                        response = requests.post(f"{server_url}/custom-indicator/{symbol.upper()}", 
                                               json=payload,
                                               headers={"Content-Type": "application/json"})
                    else:
                        # Regular GET request to data endpoint
                        response = requests.get(f"{server_url}/data/{symbol.upper()}")
                    
                    response.raise_for_status()
                    
                    # Parse JSON response containing plotly layout and traces
                    plot_data = response.json()
                    
                    # Create plotly figure from server response
                    if "layout" in plot_data and "data" in plot_data:
                        fig = go.Figure(data=plot_data["data"], layout=plot_data["layout"])
                        st.plotly_chart(fig, use_container_width=True)
                        
                        with st.expander("Raw Data"):
                            st.json(plot_data)
                            
                    elif "traces" in plot_data:
                        fig = go.Figure()
                        for trace in plot_data["traces"]:
                            fig.add_trace(go.Scatter(**trace))
                        
                        if "layout" in plot_data:
                            fig.update_layout(plot_data["layout"])
                        
                        st.plotly_chart(fig, use_container_width=True)
                        
                        with st.expander("Raw Data"):
                            st.json(plot_data)
                    else:
                        st.error("Invalid data format. Expected 'layout' and 'data' or 'traces' fields.")
                        st.json(plot_data)
                        
            except requests.exceptions.RequestException as e:
                st.error(f"Error connecting to server: {e}")
            except json.JSONDecodeError as e:
                st.error(f"Error parsing JSON response: {e}")
            except Exception as e:
                st.error(f"Unexpected error: {e}")
        
        with portfolio_tab:
            render_portfolio_tab(state_info)
        
        with state_tab:
            render_state_tab(state_info, stats_info)
    else:
        st.warning("Please enter a symbol")

# Status section
st.sidebar.markdown("---")
st.sidebar.subheader("Server Status")

try:
    health_response = requests.get(f"{server_url}/health", timeout=2)
    if health_response.status_code == 200:
        st.sidebar.success("Server Online")
    else:
        st.sidebar.error(f"Server Error: {health_response.status_code}")
except:
    st.sidebar.error("Server Offline")

# Shutdown button
st.sidebar.markdown("---")
if st.sidebar.button("Shutdown Server", type="primary", use_container_width=True):
    try:
        response = requests.get(f"{server_url}/shutdown")
        if response.status_code == 200:
            st.sidebar.success("Server shutdown initiated")
        else:
            st.sidebar.error(f"Shutdown failed: {response.status_code}")
    except Exception as e:
        st.sidebar.error(f"Error: {e}")

# Instructions
st.sidebar.markdown("---")
st.sidebar.subheader("Instructions")
st.sidebar.markdown("""
1. Enter a stock symbol (e.g., AAPL, TSLA)
2. Enable Custom Indicator for technical analysis
3. Choose from Quick Select or build custom indicator
4. Click 'Fetch Data' to visualize

**Formats Supported**: All Tacaml.of_string parseable expressions
""")
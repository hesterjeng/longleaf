import streamlit as st
import requests
import json
import os
from datetime import datetime
import plotly.graph_objects as go

st.set_page_config(
    page_title="Longleaf Control Dashboard",
    page_icon="🍃",
    layout="wide",
    initial_sidebar_state="expanded"
)

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

# Helper functions
def fetch_server_status(server_url):
    """Fetch server status"""
    try:
        response = requests.get(f"{server_url}/status", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching status: {e}")
        return None

def fetch_server_settings(server_url):
    """Fetch server settings"""
    try:
        response = requests.get(f"{server_url}/settings", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching settings: {e}")
        return None

def fetch_available_data(server_url):
    """Fetch available data files"""
    try:
        response = requests.get(f"{server_url}/data", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching data files: {e}")
        return None

def fetch_available_strategies(server_url):
    """Fetch available strategies"""
    try:
        response = requests.get(f"{server_url}/strategies", timeout=5)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching strategies: {e}")
        return None

def fetch_available_symbols(server_url):
    """Fetch available symbols from current target data"""
    try:
        response = requests.get(f"{server_url}/symbols", timeout=5)
        if response.status_code == 404:
            # Symbols endpoint not yet implemented for File targets
            return []
        response.raise_for_status()
        symbols_list = response.json()
        if isinstance(symbols_list, list):
            return symbols_list
        return []
    except Exception as e:
        # Don't show error for not yet implemented endpoint
        return []

def fetch_chart_data(server_url, symbol):
    """Fetch chart data for a specific symbol"""
    try:
        response = requests.get(f"{server_url}/data/{symbol.upper()}/json", timeout=10)
        if response.status_code == 404:
            # Chart endpoint not yet fully implemented
            st.warning(f"Chart data not yet available for {symbol}")
            return None
        response.raise_for_status()
        return response.json()
    except Exception as e:
        st.error(f"Error fetching chart data for {symbol}: {e}")
        return None

def set_server_status(server_url, status):
    """Set server status"""
    try:
        response = requests.post(f"{server_url}/set_status", json=status, timeout=5)
        response.raise_for_status()
        return True, response.text
    except Exception as e:
        return False, str(e)

def set_target(server_url, target):
    """Set target (Download or File path)"""
    try:
        response = requests.post(f"{server_url}/set_target", json=target, timeout=5)
        response.raise_for_status()
        return True, response.text
    except Exception as e:
        return False, str(e)

def set_strategy(server_url, strategy):
    """Set strategy"""
    try:
        response = requests.post(f"{server_url}/set_strategy", json=strategy, timeout=5)
        response.raise_for_status()
        return True, response.text
    except Exception as e:
        return False, str(e)

def render_status_display(status_data):
    """Render server status display"""
    if not status_data:
        st.error("No status data available")
        return
    
    status_str = status_data if isinstance(status_data, str) else str(status_data)
    
    # Create status indicator with color
    if "Ready" in status_str:
        st.success(f"🟢 Server Status: {status_str}")
    elif "Started" in status_str:
        st.info(f"🔵 Server Status: {status_str}")
    elif "Error" in status_str:
        st.error(f"🔴 Server Status: {status_str}")
    else:
        st.warning(f"🟡 Server Status: {status_str}")

def render_settings_display(settings_data):
    """Render server settings display"""
    if not settings_data:
        st.error("No settings data available")
        return
    
    st.subheader("Current Configuration")
    
    # Create columns for better layout
    col1, col2 = st.columns(2)
    
    with col1:
        st.write("**CLI Variables:**")
        cli_vars = settings_data.get("cli_vars", {})
        for key, value in cli_vars.items():
            st.write(f"- {key}: `{value}`")
    
    with col2:
        st.write("**Target:**")
        target = settings_data.get("target")
        if isinstance(target, dict):
            target_type = list(target.keys())[0] if target else "Unknown"
            target_value = target.get(target_type, "")
            st.write(f"- Type: `{target_type}`")
            if target_value:
                st.write(f"- Value: `{target_value}`")
        else:
            st.write(f"- `{target}`")
    
    # Raw JSON in expander
    with st.expander("Raw Settings JSON"):
        st.json(settings_data)

# Main app
st.markdown("<h1 style='text-align: center;'>🍃 Longleaf Control Dashboard</h1>", unsafe_allow_html=True)
st.markdown("<p style='text-align: center;'>Algorithmic Trading Platform Control Interface</p>", unsafe_allow_html=True)

# Sidebar controls
st.sidebar.header("🔧 Server Controls")

# Initialize server URL in session state if not present
if "server_url" not in st.session_state:
    st.session_state.server_url = "http://localhost:8080"

# Server URL input
server_url = st.sidebar.text_input("Server URL", value=st.session_state.server_url, help="Longleaf server URL")
if server_url != st.session_state.server_url:
    st.session_state.server_url = server_url
    st.rerun()

# Test server connection
try:
    health_response = requests.get(f"{server_url}/status", timeout=2)
    if health_response.status_code == 200:
        st.sidebar.success("🟢 Server Online")
        server_online = True
    else:
        st.sidebar.error(f"🔴 Server Error: {health_response.status_code}")
        server_online = False
except:
    st.sidebar.error("🔴 Server Offline")
    server_online = False

# Create tabs
overview_tab, chart_tab, control_tab, data_tab, strategies_tab = st.tabs([
    "📊 Overview", 
    "📈 Charts",
    "🎛️ Control", 
    "📁 Data Files", 
    "🧠 Strategies"
])

if server_online:
    # Fetch all data
    status_data = fetch_server_status(server_url)
    settings_data = fetch_server_settings(server_url)
    data_files = fetch_available_data(server_url)
    strategies = fetch_available_strategies(server_url)
    available_symbols = fetch_available_symbols(server_url)
    
    with overview_tab:
        st.header("📊 System Overview")
        
        # Status and settings in columns
        col1, col2 = st.columns(2)
        
        with col1:
            st.subheader("Server Status")
            render_status_display(status_data)
            
            # Last update timestamp
            st.caption(f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        with col2:
            render_settings_display(settings_data)
        
        # Auto-refresh option
        if st.button("🔄 Refresh Data"):
            st.rerun()
    
    with chart_tab:
        st.header("📈 Price Charts")
        
        if available_symbols:
            # Symbol selection
            col1, col2 = st.columns([2, 1])
            
            with col1:
                selected_symbol = st.selectbox(
                    "Select Symbol", 
                    available_symbols,
                    help="Choose a symbol to view its price chart with technical indicators"
                )
            
            with col2:
                if st.button("📊 Load Chart", type="primary"):
                    if selected_symbol:
                        with st.spinner(f"Loading chart for {selected_symbol}..."):
                            chart_data = fetch_chart_data(server_url, selected_symbol)
                            
                            if chart_data:
                                # Parse the chart data from the server
                                if "traces" in chart_data and "layout" in chart_data:
                                    # Create Plotly figure from server response
                                    fig = go.Figure()
                                    
                                    # Add all traces
                                    for trace_data in chart_data["traces"]:
                                        if trace_data.get("type") == "scatter":
                                            fig.add_trace(go.Scatter(
                                                x=trace_data.get("x", []),
                                                y=trace_data.get("y", []),
                                                name=trace_data.get("name", ""),
                                                line=dict(
                                                    color=trace_data.get("line", {}).get("color", "#1f77b4"),
                                                    dash=trace_data.get("line", {}).get("dash", "solid"),
                                                    width=trace_data.get("line", {}).get("width", 1)
                                                ),
                                                visible=trace_data.get("visible", True),
                                                yaxis=trace_data.get("yaxis", "y"),
                                                mode=trace_data.get("mode", "lines"),
                                                marker=trace_data.get("marker", {}),
                                                hovertext=trace_data.get("hovertext", []),
                                                hoverinfo=trace_data.get("hoverinfo", "x+y+name")
                                            ))
                                    
                                    # Update layout
                                    layout = chart_data["layout"]
                                    fig.update_layout(
                                        title=layout.get("title", f"{selected_symbol} Price Chart"),
                                        xaxis=layout.get("xaxis", {}),
                                        yaxis=layout.get("yaxis", {}),
                                        yaxis2=layout.get("yaxis2", {}),
                                        hovermode=layout.get("hovermode", "x"),
                                        height=600,
                                        showlegend=True
                                    )
                                    
                                    # Display the chart
                                    st.plotly_chart(fig, use_container_width=True)
                                    
                                    # Chart information
                                    st.subheader("📊 Chart Information")
                                    col1, col2, col3 = st.columns(3)
                                    
                                    with col1:
                                        st.metric("Symbol", selected_symbol)
                                    
                                    with col2:
                                        num_traces = len(chart_data["traces"])
                                        st.metric("Indicators", num_traces - 1)  # Subtract price trace
                                    
                                    with col3:
                                        # Count data points from first trace
                                        if chart_data["traces"]:
                                            data_points = len(chart_data["traces"][0].get("x", []))
                                            st.metric("Data Points", data_points)
                                    
                                    # Raw data expander
                                    with st.expander("🔍 Raw Chart Data"):
                                        st.json(chart_data)
                                        
                                else:
                                    st.error("Invalid chart data format received from server")
                                    st.json(chart_data)
            
            # Chart instructions
            with st.expander("📖 Chart Instructions"):
                st.markdown("""
                **How to use the charts:**
                
                1. **Select Symbol**: Choose from available symbols in the current data file
                2. **Load Chart**: Click to fetch and display the price chart
                3. **Interactive Features**: 
                   - Zoom: Click and drag to zoom into specific time periods
                   - Pan: Hold shift and drag to pan across the chart
                   - Legend: Click legend items to show/hide indicators
                   - Hover: Hover over data points for detailed information
                
                **Chart Components:**
                - **Price Line**: Main price data (usually in blue)
                - **Technical Indicators**: Additional analysis overlays
                - **Buy/Sell Markers**: Trading signals if available
                - **Secondary Y-axis**: For oscillators and normalized indicators
                
                **Note**: Charts display data from the currently selected target file.
                Set a data file in the Control tab to enable charting.
                """)
        else:
            st.warning("⚠️ Chart functionality not yet available")
            st.info("💡 **Chart Implementation Status:**")
            st.markdown("""
            The chart endpoints are currently under development:
            
            - **`/symbols`** endpoint: Not yet implemented for File targets
            - **`/data/:symbol/json`** endpoint: Returns 404 (not yet implemented)
            
            **When implemented, you will be able to:**
            1. Set a data file target in the **Control** tab
            2. View available symbols from the data file
            3. Generate interactive price charts with technical indicators
            
            **Available data files can be found in the Data Files tab**
            """)
    
    with control_tab:
        st.header("🎛️ Server Control")
        
        # Status control section
        st.subheader("Status Management")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("▶️ Start Server", type="primary"):
                success, message = set_server_status(server_url, "Started")
                if success:
                    st.success("Server started successfully")
                    st.rerun()
                else:
                    st.error(f"Failed to start server: {message}")
        
        with col2:
            if st.button("⏸️ Set Ready", type="secondary"):
                success, message = set_server_status(server_url, "Ready")
                if success:
                    st.success("Server set to ready")
                    st.rerun()
                else:
                    st.error(f"Failed to set ready: {message}")
        
        with col3:
            if st.button("⚠️ Set Error", type="secondary"):
                success, message = set_server_status(server_url, "Error")
                if success:
                    st.warning("Server set to error state")
                    st.rerun()
                else:
                    st.error(f"Failed to set error: {message}")
        
        st.markdown("---")
        
        # Target management
        st.subheader("Target Configuration")
        
        target_type = st.radio("Target Type", ["Download", "File"], horizontal=True)
        
        if target_type == "Download":
            if st.button("Set Download Target"):
                success, message = set_target(server_url, "Download")
                if success:
                    st.success("Target set to Download")
                    st.rerun()
                else:
                    st.error(f"Failed to set target: {message}")
        else:
            # Show available files for selection
            if data_files:
                selected_file = st.selectbox("Select Data File", data_files)
                if st.button("Set File Target"):
                    success, message = set_target(server_url, {"File": selected_file})
                    if success:
                        st.success(f"Target set to file: {selected_file}")
                        st.rerun()
                    else:
                        st.error(f"Failed to set target: {message}")
            else:
                st.warning("No data files available")
        
        st.markdown("---")
        
        # Strategy management
        st.subheader("Strategy Selection")
        
        if strategies:
            selected_strategy = st.selectbox("Select Strategy", strategies)
            if st.button("Set Strategy"):
                success, message = set_strategy(server_url, selected_strategy)
                if success:
                    st.success(f"Strategy set to: {selected_strategy}")
                    st.rerun()
                else:
                    st.error(f"Failed to set strategy: {message}")
        else:
            st.warning("No strategies available")
    
    with data_tab:
        st.header("📁 Data Files")
        
        if data_files:
            st.subheader("Available Data Files")
            
            # Display files in a nice format
            for i, file_path in enumerate(data_files, 1):
                col1, col2 = st.columns([3, 1])
                with col1:
                    st.write(f"**{i}.** `{file_path}`")
                with col2:
                    if st.button(f"Select", key=f"select_file_{i}"):
                        success, message = set_target(server_url, {"File": file_path})
                        if success:
                            st.success(f"Target set to: {file_path}")
                            st.rerun()
                        else:
                            st.error(f"Failed to set target: {message}")
            
            st.info(f"Total data files: {len(data_files)}")
        else:
            st.warning("No data files found")
        
        # Instructions
        with st.expander("📖 Data File Instructions"):
            st.markdown("""
            **Data files** are historical market data files used for backtesting and analysis.
            
            - Files are typically JSON format containing OHLCV data
            - Use the **longleaf_downloader** tool to create data files
            - Select a file to set it as the target for backtesting
            
            **Example commands:**
            ```bash
            # Download data for backtesting
            longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 \\
                --interval=10 --timeframe=minute data/24.json
            ```
            """)
    
    with strategies_tab:
        st.header("🧠 Available Strategies")
        
        if strategies:
            st.subheader("Strategy List")
            
            # Display strategies with descriptions (if available)
            for i, strategy in enumerate(strategies, 1):
                col1, col2 = st.columns([3, 1])
                with col1:
                    st.write(f"**{i}.** `{strategy}`")
                with col2:
                    if st.button(f"Select", key=f"select_strategy_{i}"):
                        success, message = set_strategy(server_url, strategy)
                        if success:
                            st.success(f"Strategy selected: {strategy}")
                            st.rerun()
                        else:
                            st.error(f"Failed to select strategy: {message}")
            
            st.info(f"Total strategies: {len(strategies)}")
        else:
            st.warning("No strategies found")
        
        # Strategy development instructions
        with st.expander("📖 Strategy Development Guide"):
            st.markdown("""
            **Strategies** are trading algorithms implemented in OCaml using the Longleaf framework.
            
            **Key Components:**
            - **Buy Trigger**: Defines entry conditions
            - **Sell Trigger**: Defines exit conditions
            - **Template System**: Use `Template.Make` functor for consistency
            
            **Development Workflow:**
            1. Create strategy file in `strategies/` directory
            2. Implement using Template.Make functor
            3. Add to `longleaf_strategies.ml` registry
            4. Test with backtesting
            5. Deploy to paper/live trading
            
            **Example Strategy Structure:**
            ```ocaml
            module Buy_inp : Template.Buy_trigger.INPUT = struct
              let pass state symbol = (* entry logic *)
              let score state symbol = (* ranking logic *)
              let num_positions = 5
            end
            
            module Sell : Template.Sell_trigger.S = struct
              let make state ~buying_order = (* exit logic *)
            end
            
            module Make : Strategy.BUILDER = Template.Make (Buy_inp) (Sell)
            ```
            """)

else:
    # Server is offline
    st.error("🔴 Cannot connect to server. Please check:")
    st.markdown("""
    1. Server is running: `longleaf_server`
    2. Correct URL (default: http://localhost:8080)
    3. Network connectivity
    4. Firewall settings
    """)

# Sidebar instructions
st.sidebar.markdown("---")
st.sidebar.subheader("📖 Instructions")
st.sidebar.markdown("""
**Quick Start:**
1. Ensure server is running
2. Check server status in Overview tab
3. Set target data file in Control tab
4. Select trading strategy
5. Start server when ready

**Status Meanings:**
- 🟢 **Ready**: Server ready for commands
- 🔵 **Started**: Server is running strategy
- 🔴 **Error**: Server encountered an error
""")

# Footer
st.markdown("---")
st.markdown(
    "<p style='text-align: center; color: gray;'>Longleaf Algorithmic Trading Platform - Control Dashboard</p>",
    unsafe_allow_html=True
)
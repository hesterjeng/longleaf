import streamlit as st
import requests
import plotly.graph_objects as go
import hashlib
import os
import json

st.set_page_config(layout="wide")

# Authentication
def check_password():
    # Auto-login if environment variable is set
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

# Main app
st.markdown("<h1 style='text-align: center;'>Longleaf Trading Dashboard</h1>", unsafe_allow_html=True)

# Sidebar controls
st.sidebar.header("Controls")
symbol = st.sidebar.text_input("Symbol", value="AAPL", help="Enter stock symbol (e.g., AAPL, TSLA)")
server_url = st.sidebar.text_input("Server URL", value="http://localhost:8080", help="OCaml server URL")

# Custom indicator section
st.sidebar.markdown("---")
st.sidebar.subheader("Custom Indicator")
custom_indicator_enabled = st.sidebar.checkbox("Enable Custom Indicator")

if custom_indicator_enabled:
    # Dropdown for indicator type
    indicator_type = st.sidebar.selectbox(
        "Indicator Type",
        ["Sma", "Ema", "Rsi", "Macd", "Atr", "Cci", "Stoch", "Willr", "Adx"],
        help="Select the type of technical indicator"
    )
    
    # Parameters based on indicator type
    if indicator_type in ["Sma", "Ema", "Rsi", "Atr", "Cci", "Willr", "Adx"]:
        period = st.sidebar.number_input("Period", min_value=1, max_value=200, value=14, help="Number of periods for calculation")
        custom_tacaml = f"{indicator_type} {period}"
    elif indicator_type == "Macd":
        fast = st.sidebar.number_input("Fast Period", min_value=1, max_value=50, value=12)
        slow = st.sidebar.number_input("Slow Period", min_value=1, max_value=50, value=26)
        signal = st.sidebar.number_input("Signal Period", min_value=1, max_value=20, value=9)
        custom_tacaml = f"MACD {fast} {slow} {signal}"
    elif indicator_type == "Stoch":
        k_period = st.sidebar.number_input("K Period", min_value=1, max_value=50, value=14)
        d_period = st.sidebar.number_input("D Period", min_value=1, max_value=20, value=3)
        custom_tacaml = f"Stoch {k_period} {d_period}"
    
    # Display the generated Tacaml expression
    st.sidebar.text_area("Generated Tacaml Expression", value=custom_tacaml, height=68, help="This is the Tacaml expression that will be sent to the server")
    
    # Custom indicator visualization options
    st.sidebar.subheader("Visualization Options")
    indicator_color = st.sidebar.color_picker("Indicator Color", value="#FF6B6B")
    indicator_yaxis = st.sidebar.selectbox("Y-Axis", ["y1 (Main)", "y2 (Secondary)"], help="Which y-axis to plot the indicator on")
else:
    custom_tacaml = None

if st.sidebar.button("Fetch Data") or symbol:
    if symbol:
        try:
            with st.spinner(f"Fetching data for {symbol}..."):
                # Choose endpoint based on whether custom indicator is enabled
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
                    
                    # Display the graph
                    st.plotly_chart(fig, use_container_width=True)
                    
                    # Show raw data in expander
                    with st.expander("Raw Data"):
                        st.json(plot_data)
                        
                elif "traces" in plot_data:
                    # Alternative format: traces array
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

# Shutdown button with styling
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
2. Click 'Fetch Data' or press Enter
3. View the plotly graph generated by the Longleaf server

**Server Endpoint**: `GET /data/{symbol}`
""")

# Longleaf Streamlit Dashboard

A web dashboard for visualizing data from the Longleaf trading platform.

## Setup

1. Install dependencies:
```bash
pip install -r requirements.txt
```

2. Set password (optional but recommended):
```bash
# Generate password hash
python3 -c "import hashlib; print(hashlib.sha256('your_secure_password'.encode()).hexdigest())"

# Set environment variable
export DASHBOARD_PASSWORD_HASH="your_generated_hash"
```

3. Start your OCaml server on port 8080

4. Run the dashboard:
```bash
streamlit run dashboard.py
```

## Usage

- Enter a stock symbol (e.g., AAPL, TSLA)
- The app fetches data from `localhost:8080/data/{symbol}`
- Displays plotly graphs returned by your OCaml server

## For Digital Ocean Deployment

Run with external access:
```bash
streamlit run dashboard.py --server.address 0.0.0.0 --server.port 8501
```

Open firewall:
```bash
sudo ufw allow 8501
```

Access via: `http://your-droplet-ip:8501`
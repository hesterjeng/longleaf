# Longleaf React Dashboard

A React-based control dashboard for the Longleaf algorithmic trading platform.

## Features

- **Real-time server monitoring** - Check server status and connectivity
- **Interactive charts** - View price data with technical indicators using Plotly
- **Server control** - Start/stop server, set status, configure targets
- **Data file management** - Browse and select historical data files
- **Strategy management** - Select and configure trading strategies
- **Responsive design** - Works on desktop and mobile devices

## Quick Start

1. **Install dependencies:**
   ```bash
   cd react
   npm install
   ```

2. **Start the development server:**
   ```bash
   npm start
   ```

3. **Open browser:**
   Navigate to `http://localhost:3000`

4. **Configure server URL:**
   The dashboard connects to `http://localhost:8080` by default. Ensure your Longleaf server is running:
   ```bash
   longleaf_server
   ```

## Architecture

### Components

- **App.js** - Main application component with state management
- **Sidebar.js** - Server connection controls and instructions
- **OverviewTab.js** - System status and configuration overview
- **ChartTab.js** - Interactive price charts with Plotly integration
- **ControlTab.js** - Server control interface (start/stop, targets, strategies)
- **DataTab.js** - Data file browser and selection
- **StrategiesTab.js** - Strategy browser and selection

### Key Features

#### Server Integration
- Connects to Longleaf server REST API
- Real-time status monitoring
- Automatic reconnection attempts
- Error handling and user feedback

#### Chart Visualization
- Interactive price charts using react-plotly.js
- Technical indicators overlay
- Zoom, pan, and hover interactions
- Multi-axis support for different indicator types

#### Responsive Design
- Mobile-friendly sidebar that collapses on smaller screens
- Grid layouts that adapt to screen size
- Touch-friendly controls and buttons

## API Integration

The dashboard communicates with the Longleaf server using these endpoints:

- `GET /status` - Server status
- `GET /settings` - Server configuration
- `GET /data` - Available data files
- `GET /strategies` - Available strategies  
- `GET /symbols` - Available symbols in current data
- `GET /data/:symbol/json` - Chart data for specific symbol
- `POST /set_status` - Set server status (Ready/Started/Error)
- `POST /set_target` - Set data target (Download or File)
- `POST /set_strategy` - Select trading strategy

## Development

### Available Scripts

- `npm start` - Start development server (port 3000)
- `npm build` - Build production version
- `npm test` - Run tests
- `npm run eject` - Eject from Create React App (irreversible)

### Dependencies

- **React 18** - Component framework
- **react-plotly.js** - Interactive charts
- **plotly.js** - Chart rendering library  
- **axios** - HTTP client for API calls

### Styling

The dashboard uses vanilla CSS with a clean, modern design:
- Responsive grid system
- Card-based layout
- Color-coded status indicators
- Mobile-first responsive design
- Custom button and form styles

## Dashboard Features

This React dashboard provides:

- **Better performance** - No Python backend required
- **More responsive** - Native web technologies
- **Better mobile support** - Touch-friendly interface
- **Customizable** - Easy to extend and modify
- **Professional look** - Modern web UI components

## Deployment

### Development
```bash
npm start
```

### Production Build
```bash
npm run build
```

The build folder will contain the static files ready for deployment to any web server.

### Docker (Optional)

Create a simple Dockerfile for containerized deployment:

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build
EXPOSE 3000
CMD ["npm", "start"]
```

## Troubleshooting

### Server Connection Issues

1. **Check server is running:**
   ```bash
   longleaf_server
   ```

2. **Verify server URL in sidebar** (default: http://localhost:8080)

3. **Check firewall/network settings**

### Chart Loading Issues

- Ensure data file is set as target in Control tab
- Verify symbols endpoint is implemented in server
- Check browser console for API errors

### Build Issues

- Clear node_modules and reinstall:
  ```bash
  rm -rf node_modules package-lock.json
  npm install
  ```

## Contributing

1. Fork the repository
2. Create feature branch
3. Make changes and test
4. Submit pull request

## License

Same as main Longleaf project.
const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

// Configuration priority: CLI args > env vars > defaults
const PORT = parseInt(process.argv[2]) || parseInt(process.env.REACT_PORT) || 3000;
const LONGLEAF_PORT = process.env.LONGLEAF_PORT || '8080';
const BACKEND_URL = process.argv[3] || process.env.LONGLEAF_URL || `http://localhost:${LONGLEAF_PORT}`;

console.log('Configuration:');
console.log(`  REACT_PORT env: ${process.env.REACT_PORT}`);
console.log(`  LONGLEAF_PORT env: ${process.env.LONGLEAF_PORT}`);
console.log(`  argv[2] (port): ${process.argv[2]}`);
console.log(`  argv[3] (backend): ${process.argv[3]}`);
console.log(`  Resolved PORT: ${PORT}`);
console.log(`  Resolved BACKEND_URL: ${BACKEND_URL}`);

const app = express();

// API routes to proxy to OCaml server
const apiRoutes = [
  '/status', '/settings', '/data', '/strategies', '/symbols',
  '/performance', '/execute', '/shutdown', '/strategy',
  '/set_status', '/set_target', '/set_strategy', '/set_runtype', '/set_cli'
];

// Create proxy middleware
const proxyMiddleware = createProxyMiddleware({
  target: BACKEND_URL,
  changeOrigin: true,
  onProxyReq: (proxyReq, req) => {
    console.log(`[PROXY] ${req.method} ${req.originalUrl} -> ${BACKEND_URL}${req.originalUrl}`);
  },
  onError: (err, req, res) => {
    console.error(`[PROXY ERROR] ${err.message}`);
    res.status(502).json({ error: 'Proxy error', message: err.message });
  }
});

// Apply proxy to all API routes
apiRoutes.forEach(route => {
  app.all(route, proxyMiddleware);
});

// Also handle dynamic routes like /data/:symbol/json
app.all('/data/:symbol/json', proxyMiddleware);

// Serve static files from build directory
app.use(express.static(path.join(__dirname, 'build')));

// Fallback: serve index.html for any non-API routes (React Router)
app.use((req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
  console.log(`Proxying API requests to ${BACKEND_URL}`);
});
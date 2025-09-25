const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

const app = express();
const PORT = 3000;

// API routes to proxy to OCaml server
const apiRoutes = [
  '/status', '/settings', '/data', '/strategies', '/symbols', 
  '/performance', '/execute', '/shutdown', '/strategy', 
  '/set_status', '/set_target', '/set_strategy', '/set_runtype', '/set_cli'
];

// Create proxy middleware for OCaml server
const proxyMiddleware = createProxyMiddleware({
  target: 'http://localhost:8080',
  changeOrigin: true,
});

// Apply proxy to all API routes
apiRoutes.forEach(route => {
  app.use(route, proxyMiddleware);
});

// Also handle dynamic routes like /data/:symbol/json
app.use('/data/:symbol/json', proxyMiddleware);

// Serve static files from build directory
app.use(express.static(path.join(__dirname, 'build')));

// Fallback: serve index.html for any non-API routes (React Router)
app.use((req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
  console.log(`Proxying API requests to http://localhost:8080`);
});
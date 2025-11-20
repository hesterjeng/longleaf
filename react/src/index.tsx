import React from 'react';
import ReactDOM from 'react-dom/client';
import axios from 'axios';
import 'antd/dist/reset.css';
import './styles.css';
import App from './App';
import { API_CONFIG } from './utils/constants';

// Configure axios baseURL from environment or default
axios.defaults.baseURL = API_CONFIG.baseURL;

const root = ReactDOM.createRoot(document.getElementById('root')!);
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
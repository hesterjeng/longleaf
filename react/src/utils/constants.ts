// Application constants - eliminates hardcoded values across components

// Runtype options for strategy execution
export const RUNTYPE_OPTIONS = [
  'Live', 
  'Paper', 
  'Backtest', 
  'Manual', 
  'Multitest', 
  'Montecarlo',
  'MultiMontecarlo', 
  'RandomSliceBacktest', 
  'MultiRandomSliceBacktest',
  'RandomTickerBacktest', 
  'MultiRandomTickerBacktest'
] as const;

// Status classifications for consistent status handling
export const STATUS_CLASSIFICATIONS = {
  success: ['Ready', 'Online', 'Connected', 'Success', 'Completed'],
  error: ['Error', 'Failed', 'Offline', 'Disconnected', 'Crashed'],
  warning: ['Started', 'Pending', 'Warning', 'Processing'],
  info: ['Unknown', 'Initializing', 'Loading']
} as const;

// API configuration
export const API_CONFIG = {
  timeout: 5000,
  chartTimeout: 60000, // Increased to 60 seconds for large datasets
  retryAttempts: 3,
  baseURL: process.env.REACT_APP_API_URL || 'http://localhost:8080'
} as const;

// UI configuration
export const UI_CONFIG = {
  refreshInterval: 30000, // 30 seconds
  maxTableRows: 100,
  debounceDelay: 300,
  animationDuration: 200
} as const;

// File extensions and types
export const FILE_TYPES = {
  data: ['.json', '.csv'],
  config: ['.toml', '.yaml', '.yml', '.json'],
  logs: ['.log', '.txt']
} as const;

// Chart configuration
export const CHART_CONFIG = {
  defaultLayout: {
    showlegend: true,
    hovermode: 'x unified',
    dragmode: 'zoom',
    margin: { t: 30, r: 30, b: 60, l: 60 }
  },
  defaultConfig: {
    responsive: true,
    displayModeBar: true,
    displaylogo: false,
    modeBarButtonsToRemove: ['pan2d', 'lasso2d', 'select2d']
  },
  defaultColors: [
    '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', 
    '#9467bd', '#8c564b', '#e377c2', '#7f7f7f'
  ]
} as const;

// Form field labels - for consistency
export const FORM_LABELS = {
  runtype: 'Execution Mode',
  strategy_arg: 'Strategy',
  target_type: 'Data Source',
  target_file: 'Target File',
  stacktrace: 'Enable Stack Trace',
  no_gui: 'Disable GUI',
  save_received: 'Save Received Data',
  start: 'Start Position',
  random_drop_chance: 'Random Drop Chance (%)',
  slippage_pct: 'Slippage Percentage (%)'
} as const;

// Tooltip descriptions
export const FORM_TOOLTIPS = {
  runtype: 'Select how the strategy should be executed',
  strategy_arg: 'Choose the trading strategy to run',
  target_type: 'Use live data download or historical file',
  target_file: 'Historical data file for backtesting',
  stacktrace: 'Show detailed error information',
  no_gui: 'Run without graphical interface',
  save_received: 'Save incoming market data to disk',
  start: 'Position to start processing from',
  random_drop_chance: 'Percentage of data to randomly drop for testing',
  slippage_pct: 'Price slippage factor for realistic order simulation (0.01 = 1%)'
} as const;
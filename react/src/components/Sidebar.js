import React from 'react';

const Sidebar = ({ serverUrl, setServerUrl, serverOnline, checkServerConnection }) => {
  const handleUrlChange = (e) => {
    setServerUrl(e.target.value);
  };

  return (
    <div className="sidebar">
      <h3>🔧 Server Controls</h3>
      
      <div className="form-group">
        <label htmlFor="server-url">Server URL</label>
        <input
          id="server-url"
          type="text"
          className="form-control"
          value={serverUrl}
          onChange={handleUrlChange}
          placeholder="http://localhost:8080"
        />
        <small>Longleaf server URL</small>
      </div>

      <div className={`status-indicator ${serverOnline ? 'status-online' : 'status-offline'}`}>
        {serverOnline ? '🟢 Server Online' : '🔴 Server Offline'}
      </div>

      <button 
        className="btn btn-primary" 
        onClick={checkServerConnection}
        style={{ width: '100%', marginTop: '10px' }}
      >
        🔄 Check Connection
      </button>

      <hr />
      
      <h4>📖 Instructions</h4>
      <div className="instructions">
        <p><strong>Quick Start:</strong></p>
        <ol>
          <li>Ensure server is running</li>
          <li>Check server status in Overview tab</li>
          <li>Set target data file in Control tab</li>
          <li>Select trading strategy</li>
          <li>Start server when ready</li>
        </ol>

        <p><strong>Status Meanings:</strong></p>
        <ul>
          <li>🟢 <strong>Ready</strong>: Server ready for commands</li>
          <li>🔵 <strong>Started</strong>: Server is running strategy</li>
          <li>🔴 <strong>Error</strong>: Server encountered an error</li>
        </ul>
      </div>
    </div>
  );
};

export default Sidebar;
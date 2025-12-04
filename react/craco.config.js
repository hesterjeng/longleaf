module.exports = {
  devServer: (devServerConfig) => {
    devServerConfig.host = 'localhost';
    devServerConfig.allowedHosts = ['localhost', '.localhost'];
    return devServerConfig;
  },
  webpack: {
    configure: (webpackConfig) => {
      // Enable source maps in production for debugging
      if (process.env.NODE_ENV === 'production') {
        webpackConfig.devtool = 'source-map';

        // Completely disable minification to debug Terser bugs
        webpackConfig.optimization.minimize = false;
      }
      return webpackConfig;
    }
  }
};
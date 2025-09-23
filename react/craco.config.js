module.exports = {
  devServer: (devServerConfig) => {
    devServerConfig.host = 'localhost';
    devServerConfig.allowedHosts = ['localhost', '.localhost'];
    return devServerConfig;
  }
};
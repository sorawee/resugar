var path = require('path');

module.exports = {
    entry: './src/Main.bs.js',
    output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, 'dist'),
        library: 'resugar/resugar',
        libraryTarget: 'amd'
    }
};

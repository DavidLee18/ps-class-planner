const path = require('path');
const autoprefixer = require('autoprefixer');

module.exports = [
    {
      entry: './pure.js',
      output: {
        filename: 'main.js',
        path: path.resolve(__dirname, 'public'),
      },
      mode: 'production',
    },
    {
      entry: ['./src/app.scss', './src/import.js'],
      output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, 'public')
      },
      module: {
        rules: [
          {
            test: /\.scss$/,
            use: [
              {
                loader: 'file-loader',
                options: {
                  name: 'bundle.css',
                  outputPath: path.resolve(__dirname, 'public')
                }
              },
              { loader: 'extract-loader' },
              { loader: 'css-loader' },
              {
                loader: 'postcss-loader',
                options: {
                  postcssOptions: {
                    plugins: [ autoprefixer() ]
                  }
                } 
              },
              {
                loader: 'sass-loader',
                options: {
                  // Prefer Dart Sass
                  implementation: require('sass'),
    
                  // See https://github.com/webpack-contrib/sass-loader/issues/804
                  webpackImporter: false,
                  sassOptions: {
                    importer: materialImporter,
                    includePaths: ['./node_modules']
                  }
                }
              }
            ]
          },
          {
            test: /\.js$/,
            loader: 'babel-loader',
            options: {
              presets: ['@babel/preset-env'],
            },
          }
        ]
      },
      mode: 'production'
    }
  ];


  function tryResolve_(url, sourceFilename) {
    // Put require.resolve in a try/catch to avoid node-sass failing with cryptic libsass errors
    // when the importer throws
    try {
      return require.resolve(url, {paths: [path.dirname(sourceFilename)]});
    } catch (e) {
      return '';
    }
  }
  
  function tryResolveScss(url, sourceFilename) {
    // Support omission of .scss and leading _
    const normalizedUrl = url.endsWith('.scss') ? url : `${url}.scss`;
    return tryResolve_(normalizedUrl, sourceFilename) ||
      tryResolve_(path.join(path.dirname(normalizedUrl), `_${path.basename(normalizedUrl)}`),
        sourceFilename);
  }
  
  function materialImporter(url, prev) {
    if (url.startsWith('@material')) {
      const resolved = tryResolveScss(url, prev);
      return {file: resolved || url};
    }
    return {file: url};
  }
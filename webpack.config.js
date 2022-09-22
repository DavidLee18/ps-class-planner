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
      }
    }
  ];
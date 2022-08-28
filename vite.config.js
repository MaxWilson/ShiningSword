import { defineConfig } from 'vite';
import path from 'path';
import envCompatible from 'vite-plugin-env-compatible';
import { createHtmlPlugin } from 'vite-plugin-html';
import { viteCommonjs } from '@originjs/vite-plugin-commonjs';
import mkcert from 'vite-plugin-mkcert'
import react from '@vitejs/plugin-react';
import fs from 'fs';

var CONFIG = {
    // The tags to include the generated JS and CSS will be automatically injected in the HTML template
    // See https://github.com/jantimon/html-webpack-plugin
    indexHtmlTemplate: "./src/ShiningSword/UI/index.html",
    devHtmlTemplate: "./src/ShiningSword/UI/dev.html",
    fsharpEntry: "./src/ShiningSword/UI/App.fs.js",
    fsharpDevEntry: "./src/ShiningSword/Dev/App.fs.js",
    outputDir: "./dist",
    assetsDir: "./src/ShiningSword/public",
    devServerPort: 8080,
    // When using webpack-dev-server, you may need to redirect some calls
    // to a external API server. See https://webpack.js.org/configuration/dev-server/#devserver-proxy
    devServerProxy: {
        '/api/*': {
            // assuming the backend is running on port 5000
            target: "http://localhost:5000",
            changeOrigin: true
        }
    }
}

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}

export default defineConfig({
  resolve: {
    alias: [
      {
        find: '@',
        replacement: path.resolve(__dirname,'src')
      },
      {
        find: 'core-js/es6',
        replacement: path.resolve(__dirname,'core-js/es')
      }
    ],
    extensions: [
      '.mjs',
      '.js',
      '.ts',
      '.jsx',
      '.tsx',
      '.json',
      '.vue'
    ]
  },
  plugins: [
    viteCommonjs(),
    envCompatible(),
    createHtmlPlugin({
      minify: 'auto',
      inject: {
        tags: [],
        data: {
          title: 'Webpack App'
        }
      }
    })
  ],
  mode: 'production',
  build: {
    rollupOptions: {
      input: {
        app: CONFIG.fsharpEntry,
        dev: CONFIG.fsharpDevEntry
      },
      output: {
        entryFileNames: '[name].[hash].js'
      }
    },
    outDir: path.resolve(__dirname, 'dist')
  },
  server: {
    strictPort: false,
    port: 8080,
    proxy: {
      '/api/*': {
        target: 'http://localhost:5000',
        changeOrigin: true
      }
    },
    base: path.resolve(__dirname, 'src/ShiningSword/public'),
    plugins: [ react() ]
  },
  define: {
   
  }
})

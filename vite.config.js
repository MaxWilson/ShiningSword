import { defineConfig } from 'vite'

export default defineConfig({
  root: "src/ShiningSword",
  build: {
    outDir: "publish",
    emptyOutDir: true,
    sourcemap: true
  }
});

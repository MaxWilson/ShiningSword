import { defineConfig } from 'vite'

export default defineConfig({
  root: "src/ShiningSword/UI/",
  build: {
    outDir: "publish",
    emptyOutDir: true,
    sourcemap: true
  }
});

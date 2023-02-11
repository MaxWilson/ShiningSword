import { defineConfig } from 'vite'

export default defineConfig({
  root: "ShiningSword/UI/",
  base: '/ShiningSword/',  
  build: {
    outDir: "publish",
    emptyOutDir: true,
    sourcemap: true
  }
});

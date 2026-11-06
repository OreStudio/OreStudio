import { build } from 'esbuild';
await build({ entryPoints: ['src/main.ts'], outfile: 'build/agent.mjs', bundle: true, platform: 'node', target: 'node22', format: 'esm', banner: { js: "import { createRequire } from 'node:module'; const require = createRequire(import.meta.url);" } });

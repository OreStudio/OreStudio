import { loadConfig } from './config.js';
import { createApp } from './server.js';

const index = process.argv.indexOf('--config');
const path = index >= 0 ? process.argv[index + 1] : process.env.ORES_AGENT_CONFIG;
if (!path) throw new Error('Pass --config <path> or ORES_AGENT_CONFIG. OreStudio .env is never loaded.');
const config = await loadConfig(path);
const server = createApp(config);
server.listen(config.port, config.host, () => console.log(JSON.stringify({ event: 'listening', address: server.address() })));
for (const signal of ['SIGINT', 'SIGTERM'] as const) process.on(signal, () => {
    server.close(() => process.exit(0));
    setTimeout(() => { server.closeAllConnections(); process.exit(1); }, 5000).unref();
});

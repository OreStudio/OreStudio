import { loadConfig } from './config.js';
import { createApp } from './server.js';
import { NatsBackend } from './backend.js';

const index = process.argv.indexOf('--config');
const path = index >= 0 ? process.argv[index + 1] : process.env.ORES_AGENT_CONFIG;
if (!path) throw new Error('Pass --config <path> or ORES_AGENT_CONFIG. OreStudio .env is never loaded.');
const config = await loadConfig(path);
const backend = config.nats ? await NatsBackend.open(config.nats) : undefined;
const server = createApp(config, backend);
server.listen(config.port, config.host, () => console.log(JSON.stringify({ event: 'listening', address: server.address() })));
for (const signal of ['SIGINT', 'SIGTERM'] as const) process.on(signal, () => {
    server.close(() => { void backend?.close().finally(() => process.exit(0)); if (!backend) process.exit(0); });
    setTimeout(() => { server.closeAllConnections(); process.exit(1); }, 5000).unref();
});

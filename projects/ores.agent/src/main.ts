// SPDX-License-Identifier: GPL-3.0-or-later
import { loadConfig } from './config.js';
import { createApp } from './server.js';
import { NatsBackend } from './backend.js';

const index = process.argv.indexOf('--config');
const path = index >= 0 ? process.argv[index + 1] : process.env.ORES_AGENT_CONFIG;
if (!path)
    throw new Error('Pass --config <path> or ORES_AGENT_CONFIG. OreStudio .env is never loaded.');
const config = await loadConfig(path);
const backend = config.nats ? await NatsBackend.open(config.nats) : undefined;
const server = createApp(config, backend);
server.listen(config.port, config.host, () =>
    console.log(JSON.stringify({ event: 'listening', address: server.address() })),
);
let stopping = false;
async function stop() {
    if (stopping) return;
    stopping = true;
    const deadline = setTimeout(() => {
        server.closeAllConnections();
        process.exit(1);
    }, 15000).unref();
    try {
        try {
            await server.shutdown();
        } finally {
            await backend?.close();
        }
        clearTimeout(deadline);
        process.exit(0);
    } catch {
        console.error(JSON.stringify({ event: 'shutdown_failed' }));
        process.exit(1);
    }
}
for (const signal of ['SIGINT', 'SIGTERM'] as const)
    process.on(signal, () => {
        void stop();
    });

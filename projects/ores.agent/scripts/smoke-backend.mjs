// Private credentials are read only from the explicit file, never logged or persisted.
import { readFile } from 'node:fs/promises';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { ToolExecutor } from '../dist/tools.js';
const [configPath, credentialsPath, partyId] = process.argv.slice(2);
if (!configPath || !credentialsPath) throw Error('Pass config and private credentials JSON paths.');
const config = await loadConfig(configPath);
const credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const backend = await NatsBackend.open(config.nats), sessions = new Sessions(backend);
try {
    const login = await sessions.login({ principal: credentials.tenantUsername, password: credentials.tenantPassword, ...(partyId ? { partyId } : {}) });
    const session = sessions.require('Bearer ' + login.accessToken);
    const executor = new ToolExecutor(backend);
    const currencies = await executor.execute('list_currencies', { limit: 3 }, session);
    const series = await executor.execute('list_market_series', { limit: 3 }, session);
    console.log(JSON.stringify({ stage: 'authenticated_backend', username: login.username, tenantId: login.tenantId, partyId: login.partyId, currencies, series }, null, 2));
    if (series.rows[0]) console.log(JSON.stringify(await executor.execute('read_market_observations', { seriesId: series.rows[0].id, limit: 1 }, session), null, 2));
    await sessions.logout('Bearer ' + login.accessToken);
} finally { await sessions.close(); await backend.close(); }

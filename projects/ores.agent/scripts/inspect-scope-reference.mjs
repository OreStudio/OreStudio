// Read-only raw evidence capture for the independent Python decimal reference.
import assert from 'node:assert/strict';
import { readFile, writeFile } from 'node:fs/promises';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
const [configPath, credentialsPath, partyId, runId, outputPath] = process.argv.slice(2);
if (!outputPath || !configPath || !credentialsPath || !partyId || !runId) throw Error('Pass config, private credentials JSON, party UUID, run UUID and private output path.');
const config = await loadConfig(configPath), credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const backend = await NatsBackend.open(config.nats), sessions = new Sessions(backend);
try {
    const login = await sessions.login({ principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId });
    const session = sessions.require('Bearer ' + login.accessToken);
    const read = async (operation, key, filter, asOf) => {
        const rows = []; let total;
        for (let offset = 0; offset < 2000; offset += 50) {
            const reply = await backend.request(operation, { offset, limit: 50, order: { field: '', descending: false }, filter, ...(asOf ? { as_of: asOf } : {}) }, session);
            assert.equal(reply.result.outcome, 'ok'); assert.ok(Array.isArray(reply[key]));
            assert.ok(total === undefined || total === reply.total); total = reply.total;
            assert.equal(reply[key].length, Math.min(50, Math.max(0, total - offset))); rows.push(...reply[key]);
            if (rows.length >= total) return rows;
        }
        throw Error('Raw evidence exceeds the 2,000-row ceiling.');
    };
    const runs = await read('reportInstances', 'instances', { id_one_of: [runId] }, new Date().toISOString());
    assert.equal(runs.length, 1); const run = runs[0]; assert.equal(run.id, runId); assert.equal(run.party_id, partyId); assert.equal(run.tenant_id, session.tenantId); assert.ok(run.started_at);
    const health = await backend.request('runHealth', { report_instance_id: runId }, session); assert.equal(health.result.outcome, 'ok'); assert.equal(health.health, 'completed clean');
    const output = { run };
    for (const [op, key] of [['books', 'books'], ['portfolios', 'portfolios'], ['trades', 'trades'], ['tradeBookings', 'trade_bookings']]) output[op] = await read(op, key, null, run.started_at);
    output.metrics = await read('reportMetrics', 'metrics', { report_instance_id: runId, measure: 'NPV(Base)', family: 'npv_report' });
    await writeFile(outputPath, JSON.stringify(output, null, 2) + '\n', { mode: 0o600 });
    console.log(JSON.stringify({ outputPath, runId, rawMetricCount: output.metrics.length }));
} finally { try { await sessions.close(); } finally { await backend.close(); } }

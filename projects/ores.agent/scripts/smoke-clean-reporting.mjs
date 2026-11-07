// Read-only reconciliation of an existing clean run; this script never triggers work.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';

const [base, configPath, credentialsPath, partyId, tradeId, runId, otherPartyId] = process.argv.slice(2);
if (!base || !configPath || !credentialsPath || !partyId || !tradeId || !runId) throw Error('Pass agent URL, config, private credentials JSON, party, trade, clean run and optional second party UUIDs.');
const config = await loadConfig(configPath), credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const backend = await NatsBackend.open(config.nats), sessions = new Sessions(backend);
const client = new Client({ name: 'clean-reporting-reconciliation', version: '0.1.3' });
const post = async (path, data, token) => {
    const response = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { authorization: 'Bearer ' + token } : {}) }, body: JSON.stringify(data), signal: AbortSignal.timeout(125000) });
    const body = await response.json(); assert.equal(response.status, 200, JSON.stringify(body)); return body;
};
const login = p => post('/auth/login', { principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId: p });
let token;
try {
    const auth = await sessions.login({ principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId });
    const session = sessions.require('Bearer ' + auth.accessToken);
    const health = await backend.request('runHealth', { report_instance_id: runId }, session);
    assert.equal(health.result.outcome, 'ok'); assert.equal(health.health, 'completed clean');
    const filter = Object.fromEntries(['party_id', 'report_instance_id', 'report_definition_id', 'subject_type', 'subject_id', 'measure', 'currency', 'counterparty_id', 'id_one_of', 'party_id_one_of', 'report_instance_id_one_of', 'report_definition_id_one_of', 'subject_type_one_of', 'subject_id_one_of', 'measure_one_of', 'currency_one_of', 'counterparty_id_one_of'].map(key => [key, null]));
    Object.assign(filter, { party_id: partyId, report_instance_id: runId, subject_type: 'trade', subject_id: tradeId, measure: 'npv' });
    const stored = await backend.request('runResults', { offset: 0, limit: 50, order: { field: '', descending: false }, filter }, session);
    assert.equal(stored.result.outcome, 'ok'); assert.equal(stored.total, 1); assert.equal(stored.results.length, 1);
    const row = stored.results[0];
    assert.equal(row.tenant_id, session.tenantId); assert.equal(row.party_id, partyId); assert.equal(row.subject_id, tradeId); assert.equal(row.report_instance_id, runId);
    assert.equal(typeof row.figure, 'number'); assert.ok(Number.isFinite(row.figure)); assert.match(row.currency, /^[A-Z]{3}$/); assert.equal(row.source, 'npv.csv');
    token = (await login(partyId)).accessToken;
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers: { authorization: 'Bearer ' + token } } }));
    const reply = await client.callTool({ name: 'read_trade_results', arguments: { tradeId, runId } });
    assert.ok(!reply.isError, JSON.stringify(reply)); const result = JSON.parse(reply.content[0].text);
    assert.equal(result.status, 'ok'); assert.equal(result.health, 'completed clean');
    for (const key of ['id', 'subject_id', 'report_instance_id', 'report_definition_id', 'measure', 'currency', 'figure', 'as_of', 'source']) assert.equal(result.result[key], row[key]);
    assert.equal(result.run.definition_id, row.report_definition_id); assert.equal(result.run.priced_at.slice(0, 10), row.as_of.slice(0, 10)); assert.ok(result.run.completed_at);
    if (otherPartyId) {
        const other = await login(otherPartyId);
        try { assert.equal((await post('/tools/read_trade_results', { tradeId, runId }, other.accessToken)).status, 'not_found'); }
        finally { await post('/auth/logout', {}, other.accessToken); }
    }
    const chat = await post('/chat', { message: `What is the NPV of trade ${tradeId}?` }, token);
    assert.ok(chat.evidence.some(e => e.tool === 'read_trade_results' && e.status === 'ok'), JSON.stringify(chat));
    // Numerical text is rendered from authoritative evidence, not rewritten by the model.
    const text = chat.answer ?? '';
    assert.ok(text.includes(`${row.figure} ${row.currency}`), JSON.stringify(chat));
    assert.ok(text.includes(runId), JSON.stringify(chat));
    console.log(JSON.stringify({ reconciled: true, externalMcp: true, gpuChat: true, secondPartyIsolation: Boolean(otherPartyId), tradeId, runId, definitionId: row.report_definition_id, measure: row.measure, figure: row.figure, currency: row.currency, pricedAt: result.run.priced_at, completedAt: result.run.completed_at, source: row.source, chatDurationMs: chat.durationMs }));
} finally {
    try { await client.close(); }
    finally {
        try { if (token) await post('/auth/logout', {}, token); }
        finally { try { await sessions.close(); } finally { await backend.close(); } }
    }
}

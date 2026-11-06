// Read-only Acme demo validation; existing errored runs stay explicitly partial.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
const [base, credentialsPath, partyId, otherPartyId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId) throw Error('Pass agent URL, private credentials JSON, selected party and optional second party.');
const c = JSON.parse(await readFile(credentialsPath, 'utf8'));
async function post(path, data, token) { const r = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { authorization: 'Bearer ' + token } : {}) }, body: JSON.stringify(data), signal: AbortSignal.timeout(125000) }); const body = await r.json(); assert.equal(r.status, 200, JSON.stringify(body)); return body; }
const login = p => post('/auth/login', { principal: c.tenantUsername, password: c.tenantPassword, partyId: p });
const auth = await login(partyId), token = auth.accessToken;
const tradeId = '05dcc4f0-4af9-597f-8e20-94c3018c6b4c', runId = 'bdfd79f3-0089-4512-8fb6-fd8a1e783291';
const client = new Client({ name: 'reporting-smoke', version: '0.1.3' });
async function call(name, args) { const r = await client.callTool({ name, arguments: args }); assert.ok(!r.isError, JSON.stringify(r)); return JSON.parse(r.content[0].text); }
try {
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers: { authorization: 'Bearer ' + token } } }));
    assert.ok((await call('find_report_runs', { tradeId })).rows.some(r => r.id === runId));
    assert.equal((await call('read_run_status', { runId })).health, 'completed with errors');
    const withheld = await call('read_trade_results', { tradeId }); assert.equal(withheld.status, 'unavailable'); assert.equal(withheld.result, undefined);
    const partial = await call('read_trade_results', { tradeId, runId, allowPartial: true }); assert.equal(partial.status, 'partial'); assert.equal(partial.result.source, 'npv.csv'); assert.match(partial.run.priced_at, /^2026-11-06/);
    if (otherPartyId) { const other = await login(otherPartyId); try { assert.equal((await post('/tools/read_trade_results', { tradeId, runId, allowPartial: true }, other.accessToken)).status, 'not_found'); } finally { await post('/auth/logout', {}, other.accessToken); } }
    for (const [message, tool, status] of [
        [`What is the NPV of trade ${tradeId}?`, 'read_trade_results', 'unavailable'],
        [`Show the status of report run ${runId}`, 'read_run_status', 'ok'],
        [`Show the partial NPV of trade ${tradeId} from run ${runId}`, 'read_trade_results', 'partial'],
    ]) { const r = await post('/chat', { message }, token); assert.ok(r.evidence.some(e => e.tool === tool && e.status === status), JSON.stringify(r)); console.log(JSON.stringify({ message, tool, status, durationMs: r.durationMs })); }
    console.log(JSON.stringify({ externalMcp: true, defaultWithheld: true, explicitPartial: true, source: partial.result.source, pricingDate: partial.run.priced_at, secondPartyIsolation: Boolean(otherPartyId), cleanLiveExample: false }));
} finally { await client.close(); await post('/auth/logout', {}, token); }

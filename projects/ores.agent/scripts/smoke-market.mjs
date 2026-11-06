// Read-only smoke for the existing Acme demo dataset, including its historical (not current) FX observations.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
const [base, credentialsPath, partyId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId) throw Error('Pass agent URL, private credentials JSON and selected Acme party UUID.');
const credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
let authorization;
async function post(path, data) {
    const response = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(authorization ? { authorization } : {}) }, body: JSON.stringify(data), signal: AbortSignal.timeout(125000) });
    const result = await response.json(); assert.equal(response.status, 200, JSON.stringify(result)); return result;
}
const login = await post('/auth/login', { principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId });
authorization = 'Bearer ' + login.accessToken;
const client = new Client({ name: 'market-lookup-smoke', version: '0.1.1' });
try {
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers: { authorization } } }));
    const catalog = await client.listTools(); assert.ok(catalog.tools.some(t => t.name === 'read_ir_rate'));
    const read = await client.callTool({ name: 'read_fx_spot', arguments: { baseCurrency: 'EUR', quoteCurrency: 'USD', provider: 'ore' } });
    assert.ok(!read.isError, JSON.stringify(read)); const fx = JSON.parse(read.content[0].text);
    assert.equal(fx.status, 'stale'); assert.equal(fx.observation.value, '1.0552'); assert.match(fx.observation.observedAt, /^2016-12-30/);
    for (const [message, status] of [
        ['What is EURUSD spot?', 'ambiguous'],
        ['ore', 'stale'],
        ['What is EURUSD spot from ore?', 'stale'],
        ['What was EURUSD spot from ore on 2016-12-30?', 'ok'],
        ['What is the USD 5Y swap rate from bloomberg?', 'no_observation'],
    ]) {
        const result = await post('/chat', { message });
        assert.ok(result.evidence.some(e => e.status === status), JSON.stringify(result));
        console.log(JSON.stringify({ question: message, answer: result.answer, status, durationMs: result.durationMs }));
    }
    const currencies = await post('/chat', { message: 'List the first three configured currencies. Use limit 3 and offset 0.' });
    assert.ok(currencies.evidence.some(e => e.tool === 'list_currencies' && e.rows.length === 3));
    console.log(JSON.stringify({ externalMcpHistoricalFx: true, currencyRegression: true }));
} finally { await client.close(); await post('/auth/logout', {}); }

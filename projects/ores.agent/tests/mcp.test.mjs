import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
import { createApp } from '../dist/server.js';
const party = '11111111-1111-4111-8111-111111111111';
const config = { host: '127.0.0.1', port: 0, allowedOrigins: [], nats: null, inference: { baseUrl: 'http://127.0.0.1:20010/v1', model: 'lfm3-1.7b', timeoutMs: 60000 } };
test('independent official MCP client discovers/calls tools; denied calls and revoked tokens fail', async () => {
    const calls = [];
    const backend = { ready: () => true, close: async () => {}, request: async (op, args, s) => {
        calls.push({ op, token: s?.token });
        if (op === 'login') return { success: true, token: 'private-backend-token', session_id: 'session', username: 'alice', tenant_id: 'tenant', selected_party_id: party };
        if (op === 'logout') return {};
        return { result: { outcome: 'ok' }, currencies: [{ iso_code: 'GBP', name: 'test fixture' }], total: 1 };
    } };
    const server = createApp(config, backend); server.listen(0, '127.0.0.1'); await once(server, 'listening');
    const base = `http://127.0.0.1:${server.address().port}`;
    const client = new Client({ name: 'independent-test-client', version: '1.0.0' });
    try {
        assert.equal((await fetch(base + '/mcp', { method: 'POST', body: '{}' })).status, 401);
        const login = await (await fetch(base + '/auth/login', { method: 'POST', body: JSON.stringify({ principal: 'alice', password: 'fixture' }) })).json();
        const headers = { Authorization: 'Bearer ' + login.accessToken };
        await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers } }));
        const catalog = await client.listTools(); assert.equal(catalog.tools.length, 3);
        const result = await client.callTool({ name: 'list_currencies', arguments: { limit: 1 } });
        assert.equal(JSON.parse(result.content[0].text).rows[0].iso_code, 'GBP');
        assert.ok(!JSON.stringify(result).includes('private-backend-token'));
        assert.equal(calls.find(c => c.op === 'currencies').token, 'private-backend-token');
        const denied = await client.callTool({ name: 'list_currencies', arguments: { tenantId: 'other' } });
        assert.equal(denied.isError, true);
        await fetch(base + '/auth/logout', { method: 'POST', headers });
        await assert.rejects(() => client.listTools());
    } finally { await client.close(); server.closeAllConnections(); await new Promise(r => server.close(r)); }
});

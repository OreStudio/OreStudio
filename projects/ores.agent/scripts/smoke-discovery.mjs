// Read-only integration check for the existing Acme demo environment.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
const [base, credentialsPath, partyId, otherPartyId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId) throw Error('Pass agent URL, private credentials JSON, selected party UUID and optional second party UUID.');
const c = JSON.parse(await readFile(credentialsPath, 'utf8'));
async function post(path, args, token) { const r = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { authorization: 'Bearer ' + token } : {}) }, body: JSON.stringify(args), signal: AbortSignal.timeout(120000) }); const data = await r.json(); assert.equal(r.status, 200, JSON.stringify(data)); return data; }
const login = id => post('/auth/login', { principal: c.tenantUsername, password: c.tenantPassword, partyId: id });
const auth = await login(partyId), token = auth.accessToken;
const client = new Client({ name: 'discovery-smoke', version: '0.1.2' });
async function call(name, args) { const r = await client.callTool({ name, arguments: args }); assert.ok(!r.isError, JSON.stringify(r)); return JSON.parse(r.content[0].text); }
try {
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers: { authorization: 'Bearer ' + token } } }));
    const books = await call('find_books', {}); assert.ok(books.rows.some(b => b.name === 'Group FX Hedges'));
    const trades = await call('find_trades', { book: 'Group FX Hedges', limit: 1 }); assert.ok(trades.totalMatches > 0);
    const detail = await call('read_trade', { tradeId: trades.rows[0].id });
    assert.equal(detail.economicsStatus, 'available'); assert.equal(detail.trade.party_id, partyId); assert.equal(detail.book.name, 'Group FX Hedges');
    assert.ok(detail.bookingPortfolioHierarchy.some(p => p.name === 'Group Treasury Portfolio'));
    assert.equal((await call('find_counterparties', { query: detail.counterparty.short_code })).rows[0].id, detail.counterparty.id);
    assert.ok((await call('find_portfolios', {})).rows.length > 0);
    if (otherPartyId) {
        const other = await login(otherPartyId);
        try { assert.equal((await post('/tools/read_trade', { tradeId: detail.trade.id }, other.accessToken)).status, 'not_found'); }
        finally { await post('/auth/logout', {}, other.accessToken); }
    }
    for (const [message, tool, status] of [
        ['Show my books', 'find_books', 'ok'],
        ['Show trades in book Group', 'find_trades', 'ambiguous'],
        ['Group FX Hedges', 'find_trades', 'ok'],
        [`Read trade ${detail.trade.id}`, 'read_trade', 'ok'],
        [`Find counterparty ${detail.counterparty.short_code}`, 'find_counterparties', 'ok'],
    ]) {
        const answer = await post('/chat', { message }, token);
        assert.ok(answer.evidence.some(e => e.tool === tool && e.status === status), JSON.stringify(answer));
        console.log(JSON.stringify({ question: message, tool, status, durationMs: answer.durationMs }));
    }
    console.log(JSON.stringify({ independentMcp: true, tradeId: detail.trade.id, book: detail.book.name, economics: detail.economicsStatus, secondPartyIsolation: Boolean(otherPartyId) }));
} finally { await client.close(); await post('/auth/logout', {}, token); }

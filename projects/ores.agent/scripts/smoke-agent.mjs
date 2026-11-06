import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
const [base, credentialsPath, partyId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId) throw Error('Pass agent URL, private credentials JSON and party UUID.');
const c = JSON.parse(await readFile(credentialsPath, 'utf8'));
let authorization;
async function post(path, data) {
    const r = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(authorization ? { authorization } : {}) }, body: JSON.stringify(data), signal: AbortSignal.timeout(125000) });
    const body = await r.json(); if (!r.ok) throw Error(JSON.stringify(body)); return body;
}
const login = await post('/auth/login', { principal: c.tenantUsername, password: c.tenantPassword, partyId });
authorization = 'Bearer ' + login.accessToken;
try {
    const currencies = await post('/chat', { message: 'List the first three configured currencies from OreStudio, with their codes and names. Use limit 3 and offset 0.' });
    assert.equal(currencies.dataRetrieved, true);
    assert.ok(currencies.evidence.some(e => e.tool === 'list_currencies' && e.rows.length === 3));
    console.log(JSON.stringify({ check: 'real_currency_tool_loop', ...currencies }, null, 2));
    const empty = await post('/chat', { message: 'Read the latest observation for this exact market series ID: 00625843-8550-4829-9738-915cf5178271. Use limit 1. If no data exists, say that there is no available observation; do not invent a price.' });
    assert.ok(empty.evidence.some(e => e.tool === 'read_market_observations'));
    console.log(JSON.stringify({ check: 'real_observation_tool_loop', ...empty }, null, 2));
} finally { await post('/auth/logout', {}); }

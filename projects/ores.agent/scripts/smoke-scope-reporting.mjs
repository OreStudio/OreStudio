// Read-only deployed MCP/chat reconciliation against an independent Decimal reference.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
const [base, credentialsPath, partyId, referencePath, otherPartyId, partialRunId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId || !referencePath) throw Error('Pass agent URL, private credentials JSON, party UUID, private Python-reference JSON, optional other party and partial run UUID.');
const credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const reference = JSON.parse(await readFile(referencePath, 'utf8')); assert.ok(reference.length >= 2);
const post = async (path, data, token) => {
    const response = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { authorization: 'Bearer ' + token } : {}) }, body: JSON.stringify(data), signal: AbortSignal.timeout(125000) });
    const body = await response.json(); assert.equal(response.status, 200, JSON.stringify(body)); return body;
};
const login = p => post('/auth/login', { principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId: p });
const auth = await login(partyId), token = auth.accessToken;
const client = new Client({ name: 'scope-reporting-reconciliation', version: '0.1.4' });
const call = async args => { const reply = await client.callTool({ name: 'summarize_scope_results', arguments: args }); assert.ok(!reply.isError, JSON.stringify(reply)); return JSON.parse(reply.content[0].text); };
try {
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers: { authorization: 'Bearer ' + token } } }));
    for (const ref of reference) {
        const args = { scopeType: ref.scopeType, scope: ref.scope, runId: ref.runId, limit: 25 };
        const found = new Map(); let offset = 0; const started = Date.now();
        do {
            const result = await call({ ...args, offset });
            assert.equal(result.status, 'ok'); assert.equal(result.scope.id, ref.scopeId);
            assert.equal(result.membershipAt, ref.membershipAt);
            assert.equal(result.total.figure, ref.total); assert.equal(result.total.currency, ref.currency); assert.equal(result.total.currencyBasis, 'typed_metric.BaseCurrency');
            assert.equal(result.coverage.expectedTradeCount, ref.expectedTradeCount); assert.equal(result.coverage.valuedTradeCount, ref.expectedTradeCount);
            assert.equal(result.coverage.missingTradeCount, 0); assert.equal(result.coverage.duplicateTradeCount, 0);
            for (const contributor of result.contributors) { assert.ok(!found.has(contributor.tradeId)); assert.equal(contributor.figure, ref.contributors[contributor.tradeId]); assert.equal(contributor.currency, ref.currency); found.set(contributor.tradeId, contributor.figure); }
            offset = result.nextOffset;
        } while (offset !== null);
        assert.equal(found.size, Object.keys(ref.contributors).length);
        console.log(JSON.stringify({ externalMcp: true, independentDecimalReference: true, scope: ref.scope, total: ref.total, currency: ref.currency, reconciledContributors: found.size, durationMs: Date.now() - started }));
    }
    const first = reference[0], selected = { scopeType: first.scopeType, scope: first.scope, runId: first.runId };
    const greek = await call({ ...selected, measure: 'delta' }); assert.equal(greek.status, 'unsupported'); assert.equal(greek.total, undefined);
    const wrongCurrency = await call({ ...selected, currency: first.currency === 'USD' ? 'GBP' : 'USD' }); assert.equal(wrongCurrency.total, undefined);
    if (partialRunId) { const partial = await call({ ...selected, runId: partialRunId }); assert.equal(partial.status, 'unavailable'); assert.equal(partial.total, undefined); }
    if (otherPartyId) {
        const other = await login(otherPartyId);
        try { assert.equal((await post('/tools/summarize_scope_results', selected, other.accessToken)).status, 'not_found'); }
        finally { await post('/auth/logout', {}, other.accessToken); }
    }
    for (const ref of [reference.find(r => r.scopeType === 'book'), reference.find(r => r.scopeType === 'portfolio')].filter(Boolean)) {
        const result = await post('/chat', { message: `What is the base-currency NPV for ${ref.scopeType} "${ref.scope}" from run ${ref.runId}?` }, token);
        assert.ok(result.evidence.some(e => e.tool === 'summarize_scope_results' && e.status === 'ok' && e.total.figure === ref.total), JSON.stringify(result));
        assert.ok(result.answer.includes(ref.total + ' ' + ref.currency)); assert.ok(result.answer.includes(ref.runId));
        console.log(JSON.stringify({ gpuChat: true, scope: ref.scope, total: ref.total, durationMs: result.durationMs }));
    }
    const clarification = await post('/chat', { message: `What is the base-currency NPV for ${first.scopeType} "${first.scope}"?` }, token);
    assert.ok(clarification.selectionRequired === 'runId' || clarification.evidence.some(e => e.status === 'selection_required'), JSON.stringify(clarification));
    const selectedReply = await post('/chat', { message: first.runId }, token);
    assert.ok(selectedReply.evidence.some(e => e.total?.figure === first.total));
    const unsupported = await post('/chat', { message: `Show delta for ${first.scopeType} "${first.scope}" from run ${first.runId}` }, token);
    assert.ok(unsupported.evidence.some(e => e.tool === 'summarize_scope_results' && e.status === 'unsupported'), JSON.stringify(unsupported));
    console.log(JSON.stringify({ runClarification: true, greekSubstitutionPrevented: true, otherPartyDenied: Boolean(otherPartyId), partialRunWithheld: Boolean(partialRunId) }));
} finally { try { await client.close(); } finally { await post('/auth/logout', {}, token); } }

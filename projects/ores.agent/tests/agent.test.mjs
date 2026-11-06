import test from 'node:test';
import assert from 'node:assert/strict';
import { Agent } from '../dist/agent.js';
const makeSession = () => ({ active: true, expires: Date.now() + 100000, chatBusy: false });
const toolCall = (args = '{}', name = 'list_currencies') => ({ role: 'assistant', content: null, tool_calls: [{ id: 'call1', type: 'function', function: { name, arguments: args } }] });
test('agent directly executes tools and returns attributable evidence; no MCP loopback', async () => {
    let turns = 0, executed = 0;
    const inference = { complete: async messages => {
        if (++turns === 1) return toolCall('{"limit":1}');
        assert.equal(messages.at(-1).role, 'tool'); assert.match(messages.at(-1).content, /GBP/);
        return { role: 'assistant', content: 'GBP from the fixture.' };
    } };
    const agent = new Agent(inference, { execute: async (name, args, session) => { executed++; assert.equal(name, 'list_currencies'); assert.equal(args.limit, 1); assert.equal(session.active, true); return { rows: [{ iso_code: 'GBP' }], source: 'labelled fixture' }; } });
    const result = await agent.chat({ message: 'Currencies?' }, makeSession(), new AbortController().signal);
    assert.equal(executed, 1); assert.equal(result.evidence.length, 1); assert.equal(result.dataRetrieved, true);
});
test('malformed calls, failures and repeated calls cannot run indefinitely', async () => {
    let executed = 0, turn = 0;
    const agent = new Agent({ complete: async () => { turn++; return toolCall(turn === 1 ? '{broken' : '{}'); } }, { execute: async () => { executed++; throw Error('private internal error'); } });
    const session = makeSession();
    await assert.rejects(() => agent.chat({ message: 'test' }, session, new AbortController().signal), /turn limit/);
    assert.equal(executed, 1); assert.equal(session.chatBusy, false);
});
test('cancelled, expired and concurrent requests are bounded; model failure releases slot', async () => {
    const session = makeSession(), abort = new AbortController(); abort.abort();
    const agent = new Agent({ complete: async () => { throw Error('model down'); } }, { execute: async () => assert.fail() });
    await assert.rejects(() => agent.chat({ message: 'test' }, session, abort.signal));
    assert.equal(session.chatBusy, false);
    await assert.rejects(() => agent.chat({ message: 'test' }, session, new AbortController().signal), /model down/);
    session.expires = 0; await assert.rejects(() => agent.chat({ message: 'test' }, session, new AbortController().signal), /Sign in/);
    session.chatBusy = true; await assert.rejects(() => agent.chat({ message: 'test' }, session, new AbortController().signal), /busy/);
});

test('market facts are rendered from evidence; provider clarification stays in its own session', async () => {
    const calls = []; let modelCalls = 0;
    const ambiguous = { tool: 'read_fx_spot', status: 'ambiguous', asOf: '2026-11-06T12:00:00Z', historicalRequest: false,
        request: { baseCurrency: 'EUR', quoteCurrency: 'USD' }, reason: 'Choose provider.', candidates: [{ provider: 'ore', seriesId: 'id-ore', instrument: 'fixture' }, { provider: 'vendor', seriesId: 'id-vendor', instrument: 'fixture' }] };
    const resolved = { ...ambiguous, status: 'stale', units: 'USD per 1 EUR', series: { provider: 'ore', seriesId: 'id-ore', instrument: 'fixture' },
        observation: { value: '1.0552', observedAt: '2016-12-30T00:00:00Z', source: 'labelled.fixture' } };
    const agent = new Agent({ complete: async () => { modelCalls++; return { role: 'assistant', content: null, tool_calls: [{ id: 'm', type: 'function', function: { name: 'read_fx_spot', arguments: '{"baseCurrency":"EUR","quoteCurrency":"USD"}' } }] }; } }, {
        execute: async (name, args) => { calls.push(args); return args.provider ? resolved : ambiguous; },
    });
    const s = makeSession();
    assert.match((await agent.chat({ message: 'What is EURUSD spot?' }, s, new AbortController().signal)).answer, /specify the provider/);
    const reply = await agent.chat({ message: 'ore' }, s, new AbortController().signal);
    assert.match(reply.answer, /not a current market quote/); assert.match(reply.answer, /1.0552/); assert.equal(modelCalls, 1);
    assert.equal(calls.at(-1).provider, 'ore');
    await agent.chat({ message: 'ore' }, makeSession(), new AbortController().signal); assert.equal(modelCalls, 2);
    await agent.chat({ message: 'What is EURUSD spot from ore?' }, s, new AbortController().signal);
    assert.equal(calls.at(-1).provider, 'ore');
});
test('book clarification is bound to the requesting session and resumes the original trade search', async () => {
    let modelCalls = 0; const calls = [];
    const agent = new Agent({ complete: async (_, catalog) => { modelCalls++; assert.ok(catalog.length <= 6); return { role: 'assistant', content: null, tool_calls: [{ id: 'b', type: 'function', function: { name: 'find_trades', arguments: '{"book":"Group"}' } }] }; } }, {
        execute: async (name, args) => { calls.push(args); return args.book === 'id-fx' ? { tool: name, status: 'ok', rows: [], totalMatches: 0, catalogueScope: 'selected_party', offset: 0 } : { tool: name, status: 'ambiguous', request: args, field: 'book', reason: 'Choose a book.', candidates: [{ id: 'id-fx', name: 'Group FX' }, { id: 'id-loans', name: 'Group Loans' }] }; },
    });
    const s = makeSession(); await agent.chat({ message: 'Show trades in book Group' }, s, new AbortController().signal);
    await agent.chat({ message: 'Group FX' }, s, new AbortController().signal); assert.equal(modelCalls, 1); assert.equal(calls.at(-1).book, 'id-fx');
    await agent.chat({ message: 'Group FX' }, makeSession(), new AbortController().signal); assert.equal(modelCalls, 2);
});

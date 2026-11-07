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
test('exact partial inspection preserves UUIDs and explicit opt-in without a model round trip', async () => {
    const tradeId = '11111111-1111-4111-8111-111111111111', runId = '22222222-2222-4222-8222-222222222222';
    const agent = new Agent({ complete: async () => assert.fail('Exact command must not need model transcription') }, { execute: async (name, args) => {
        assert.equal(name, 'read_trade_results'); assert.equal(args.tradeId, tradeId); assert.equal(args.runId, runId); assert.equal(args.allowPartial, true);
        return { tool: name, status: 'unavailable', reason: 'labelled fixture unavailable' };
    } });
    assert.match((await agent.chat({ message: `Show the partial NPV of trade ${tradeId} from run ${runId}` }, makeSession(), new AbortController().signal)).answer, /unavailable/);
});
test('the model cannot opt into partial results when the user did not request them', async () => {
    let turn = 0;
    const agent = new Agent({ complete: async () => ++turn === 1 ? toolCall('{"allowPartial":true}', 'read_trade_results') : { role: 'assistant', content: 'No verified value available.' } }, { execute: async () => assert.fail('Unrequested partial read must not execute') });
    const r = await agent.chat({ message: 'What is the NPV of my trade?' }, makeSession(), new AbortController().signal);
    assert.equal(r.evidence.length, 0);
});

test('scope run selection is private to its session and resumes the literal historical scope', async () => {
    const runId = '22222222-2222-4222-8222-222222222222'; let modelCalls = 0; const calls = [];
    const agent = new Agent({ complete: async (messages, catalogue) => {
        modelCalls++; assert.ok(catalogue.length <= 6);
        if (messages.at(-1).content === runId) return { role: 'assistant', content: 'No pending scope for this session.' };
        return toolCall(JSON.stringify({ scopeType: 'book', scope: 'FX Book', runId: messages.at(-1).content.includes(runId) ? runId : undefined }), 'summarize_scope_results');
    } }, { execute: async (name, args) => { calls.push(args); return { tool: name, request: args, status: args.runId ? 'unavailable' : 'selection_required', field: 'runId', reason: args.runId ? 'labelled fixture unavailable' : 'Choose a run.' }; } });
    const session = makeSession(); const prompt = await agent.chat({ message: 'What is the NPV of book FX Book?' }, session, new AbortController().signal);
    assert.equal(prompt.selectionRequired, 'runId'); assert.equal(modelCalls, 0);
    await agent.chat({ message: runId }, makeSession(), new AbortController().signal); assert.equal(calls.length, 0);
    assert.match((await agent.chat({ message: runId }, session, new AbortController().signal)).answer, /fixture unavailable/);
    assert.equal(modelCalls, 2); assert.equal(calls.at(-1).scope, 'FX Book'); assert.equal(calls.at(-1).runId, runId);
});

test('scope Greeks cannot be replaced by an NPV measure proposed by the model', async () => {
    const agent = new Agent({ complete: async () => toolCall('{"scopeType":"book","scope":"FX Book","measure":"npv_base"}', 'summarize_scope_results') }, {
        execute: async (name, args) => { assert.equal(args.measure, 'delta'); return { tool: name, status: 'unsupported', reason: 'Greek units are not verified.' }; },
    });
    const r = await agent.chat({ message: 'Show delta for book FX Book from run 11111111-1111-4111-8111-111111111111' }, makeSession(), new AbortController().signal);
    assert.match(r.answer, /not verified/); assert.equal(r.evidence[0].status, 'unsupported');
});

test('a model-invented scope run cannot execute without a user selection', async () => {
    let round = 0;
    const agent = new Agent({ complete: async () => ++round === 1 ? toolCall('{"scopeType":"book","scope":"FX Book","runId":"22222222-2222-4222-8222-222222222222"}', 'summarize_scope_results') : { role: 'assistant', content: 'Select a report run.' } }, { execute: async () => assert.fail('Invented run selection must not execute') });
    assert.equal((await agent.chat({ message: 'Show NPV for book FX Book from run 44444444-4444-4444-8444-444444444444' }, makeSession(), new AbortController().signal)).evidence.length, 0);
});

test('Greek questions ask for a run before inference and cannot invent factor selection', async () => {
    let modelCalls=0;
    const agent = new Agent({ complete: async () => { modelCalls++; return toolCall('{"scopeType":"book","scope":"FX Book","measure":"delta","factor":"DiscountCurve/GBP/0/1Y"}', 'summarize_scope_results'); } }, { execute: async () => assert.fail('Unrequested factor must not execute') });
    const missing=await agent.chat({message:'Show Greeks for book FX Book'},makeSession(),new AbortController().signal);
    assert.equal(missing.selectionRequired,'runId');assert.equal(modelCalls,0);
    await assert.rejects(()=>agent.chat({message:'Show delta for book FX Book from run 11111111-1111-4111-8111-111111111111'},makeSession(),new AbortController().signal));
});

test('model-supplied empty Greek selectors cannot silently narrow stress or cross buckets', async () => {
    const agent=new Agent({complete:async()=>toolCall('{"scopeType":"book","scope":"FX Book","measure":"greeks","scenario":"","factor2":""}','summarize_scope_results')},{execute:async(name,args)=>{
        assert.equal(Object.hasOwn(args,'scenario'),false);assert.equal(Object.hasOwn(args,'factor2'),false);
        return {tool:name,status:'unavailable',reason:'No stored figures in this fixture.'};
    }});
    const result=await agent.chat({message:'Show Greeks for book FX Book from run 11111111-1111-4111-8111-111111111111'},makeSession(),new AbortController().signal);
    assert.equal(result.evidence[0].status,'unavailable');
});

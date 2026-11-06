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

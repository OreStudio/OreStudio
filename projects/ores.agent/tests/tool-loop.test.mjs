import test from 'node:test';
import assert from 'node:assert/strict';
import { Agent } from '../dist/agent.js';
import { Conversations } from '../dist/conversation.js';
import { ToolReferences } from '../dist/tool-references.js';
import { AppError } from '../dist/backend.js';
import { fixtureSession } from '../evals/fixtures.mjs';

const id = n => '11111111-1111-4111-8111-' + String(n).padStart(12, '0');
const call = (name, args = {}) => ({ role: 'assistant', content: null,
    tool_calls: [{ id: name, type: 'function', function: { name, arguments: JSON.stringify(args) } }] });
const final = content => ({ role: 'assistant', content });
const page = (tool, rows) => ({ tool, status: 'ok', rows, totalMatches: rows.length });
const ask = (agent, message, session = fixtureSession()) => agent.chat({ message }, session, new AbortController().signal);
const last = messages => JSON.parse(messages.at(-1).content);

function scripted(steps) {
    let step = 0;
    return { complete: async (messages, tools) => {
        assert.ok(step < steps.length, 'Unexpected extra inference call');
        const next = steps[step++];
        return typeof next === 'function' ? next(messages, tools) : next;
    } };
}

test('ordinary conjunctions continue through discovery, detail and final answer', async () => {
    for (const wording of ['and', 'then', 'followed by']) {
        const executed = [];
        const model = scripted([
            call('find_trades'),
            messages => { assert.equal(last(messages).rows[0].id, id(1)); return call('read_trade', { tradeId: id(1) }); },
            messages => { assert.equal(last(messages).economics.sold_amount, '144'); return final('The first trade sold 144 GBP.'); },
        ]);
        const agent = new Agent(model, { execute: async (name, args) => {
            executed.push(name);
            if (name === 'find_trades') return page(name, [{ id: id(1) }]);
            assert.equal(args.tradeId, id(1));
            return { tool: name, status: 'ok', trade: { id: id(1) }, economicsStatus: 'available',
                bookingPortfolioHierarchy: [], directPortfolios: [], economics: { sold_amount: '144', sold_currency: 'GBP' } };
        } });
        const result = await ask(agent, `Find trades ${wording} show the details of the first match`);
        assert.deepEqual(executed, ['find_trades', 'read_trade']);
        assert.equal(result.answer, 'The first trade sold 144 GBP.');
    }
});

test('tool references accept typed discovery IDs but reject invented, echoed and ambiguous IDs', () => {
    const refs = new ToolReferences();
    refs.observe(page('find_trades', [{ id: id(1), note: id(9) }]));
    refs.observe({ ...page('find_report_runs', [{ id: id(2) }]), request: { seriesId: id(9) } });
    refs.observe(page('list_market_series', [{ id: id(3) }]));
    refs.observe({ tool: 'find_trades', status: 'ambiguous', candidates: [{ id: id(4) }] });
    refs.observe(page('run_python', [{ id: id(5) }]));
    assert.equal(refs.has('tradeId', id(1)), true);
    assert.equal(refs.has('runId', id(2)), true);
    assert.equal(refs.has('seriesId', id(3)), true);
    for (const key of ['tradeId', 'runId', 'seriesId']) {
        for (const n of [4, 5, 9]) assert.equal(refs.has(key, id(n)), false);
    }
    assert.equal(refs.has('runId', id(1)), false);
});

test('run and series discovery feed their identifiers to dependent reads', async () => {
    for (const [message, find, read, key] of [
        ['List report runs and show the status of the first', 'find_report_runs', 'read_run_status', 'runId'],
        ['List market series and show observations for the first', 'list_market_series', 'read_market_observations', 'seriesId'],
    ]) {
        const executed = [];
        const agent = new Agent(scripted([call(find), call(read, { [key]: id(1) }), final('Verified dependent read.')]), {
            execute: async (name, args) => {
                executed.push(name);
                if (name === find) return page(name, [{ id: id(1) }]);
                assert.equal(args[key], id(1));
                return { tool: name, status: 'ok', run: { id: id(1) }, rows: [] };
            },
        });
        await ask(agent, message);
        assert.deepEqual(executed, [find, read]);
    }
});

test('unseen IDs remain rejected even after discovery', async () => {
    const executed = [];
    const agent = new Agent(scripted([call('find_trades'), call('read_trade', { tradeId: id(9) }), messages => {
        assert.equal(last(messages).error, 'selection_not_requested'); return final('No verified trade selected.');
    }]), { execute: async name => { executed.push(name); return page(name, [{ id: id(1) }]); } });
    await ask(agent, 'Find trades and show a matching trade');
    assert.deepEqual(executed, ['find_trades']);
});

test('cross-category chains enable tools without executing mutations', async () => {
    const executed = [];
    const agent = new Agent(scripted([
        call('find_trades'),
        (_, tools) => {
            assert.equal(tools.some(t => t.function.name === 'read_fx_spot'), true);
            assert.equal(tools.some(t => t.function.name === 'build_fx_forward_curve'), false);
            return call('enable_tools', { groups: ['analytics'] });
        },
        (_, tools) => {
            assert.equal(tools.some(t => t.function.name === 'build_fx_forward_curve'), true);
            return call('read_fx_spot', { baseCurrency: 'EUR', quoteCurrency: 'USD' });
        }, final('Trades and stored spot retrieved.'),
    ]), { execute: async name => { executed.push(name); return page(name, []); } });
    await ask(agent, 'Find trades and look up EUR/USD spot');
    assert.deepEqual(executed, ['find_trades', 'read_fx_spot']);
});

test('clarification resumes the original task and its verified references', async () => {
    const executed = [];
    const session = fixtureSession();
    const agent = new Agent(scripted([
        call('find_trades', { book: 'Group' }),
        messages => {
            assert.ok(messages.some(m => m.role === 'user' && m.content.includes('show the first trade')));
            assert.equal(last(messages).rows[0].id, id(1));
            return call('read_trade', { tradeId: id(1) });
        }, final('The selected book’s first trade details.'),
    ]), { execute: async (name, args) => {
        executed.push({ name, args });
        if (name === 'read_trade') return { tool: name, status: 'ok', trade: { id: id(1) } };
        if (args.book === id(2)) return page(name, [{ id: id(1) }]);
        return { tool: name, status: 'ambiguous', request: args, field: 'book', reason: 'Choose book.',
            candidates: [{ id: id(2), name: 'Group FX' }, { id: id(3), name: 'Group Loans' }] };
    } });
    assert.equal((await ask(agent, 'Find trades in book Group and show the first trade', session)).selectionRequired, 'book');
    const result = await ask(agent, 'Group FX', session);
    assert.deepEqual(executed.map(x => x.name), ['find_trades', 'find_trades', 'read_trade']);
    assert.equal(executed[1].args.book, id(2));
    assert.equal(result.answer, 'The selected book’s first trade details.');
    assert.equal(result.evidence.length, 3);
});

test('suspended workflows expire, reset and cannot cross party or session identities', () => {
    const conversations = new Conversations();
    const session = fixtureSession();
    const seed = () => {
        conversations.set(session, { kind: 'tool', name: 'find_trades', field: 'book', args: {}, candidates: [] });
        conversations.suspend(session, { plan: { message: 'original' }, messages: [], evidence: [{ secret: 'party data' }] });
    };
    for (const field of ['partyId', 'tenantId', 'sessionId']) {
        seed(); const old = session[field]; session[field] = 'changed';
        assert.equal(conversations.workflow(session), undefined); session[field] = old;
    }
    seed(); conversations.reset(session); assert.equal(conversations.workflow(session), undefined);
    seed(); const now = Date.now;
    try { Date.now = () => now() + 300001; assert.equal(conversations.workflow(session), undefined); }
    finally { Date.now = now; }
});

test('four successive tool calls still get a final model response', async () => {
    let reads = 0;
    const agent = new Agent(scripted([
        ...Array.from({ length: 4 }, (_, offset) => call('find_trades', { offset, limit: 1 })),
        messages => { assert.equal(last(messages).rows[0].id, id(4)); return final('All four pages checked.'); },
    ]), { execute: async name => page(name, [{ id: id(++reads) }]) });
    assert.equal((await ask(agent, 'Find trades and inspect four pages')).answer, 'All four pages checked.');
    assert.equal(reads, 4);
});

test('budget exhaustion reserves a tools-disabled summary and retains evidence', async () => {
    let reads = 0;
    const agent = new Agent({ complete: async (_, tools) => {
        if (reads === 16) { assert.deepEqual(tools, []); return final('Read sixteen pages; more pages remain.'); }
        return call('find_trades', { offset: reads, limit: 1 });
    } }, { execute: async name => page(name, [{ id: id(++reads) }]) });
    const result = await ask(agent, 'Find all trades');
    assert.match(result.answer, /incomplete.*tool-call limit/);
    assert.match(result.answer, /more pages remain/);
    assert.equal(result.evidence.length, 16);
});

test('successful repeats reuse evidence; failed calls can be repaired', async () => {
    let reads = 0;
    const agent = new Agent(scripted([call('find_trades'), call('find_trades'), final('Read once.')]), {
        execute: async name => { reads++; return page(name, []); },
    });
    await ask(agent, 'Find trades'); assert.equal(reads, 1);
    const repaired = new Agent(scripted([
        call('calculate', { operation: 'divide', left: '4', right: '0', places: 2 }),
        call('calculate', { operation: 'divide', left: '4', right: '2', places: 2 }), final('2'),
    ]), { execute: async (_, args) => {
        if (args.right === '0') throw new Error('private backend detail');
        return { tool: 'calculate', value: '2' };
    } });
    assert.equal((await ask(repaired, 'Calculate four divided by two')).answer, '2');
});

test('a pending call accounts for the rest of a batch without executing it', async () => {
    const first = call('find_trades', { book: 'Group' });
    first.tool_calls.push({ ...call('find_books').tool_calls[0], id: 'second' });
    const agent = new Agent(scripted([first, messages => {
        const batch = messages.find(m => m.tool_calls?.length === 2);
        for (const tool of batch.tool_calls) assert.ok(messages.some(m => m.role === 'tool' && m.tool_call_id === tool.id));
        return final('Selection resolved.');
    }]), { execute: async (name, args) => {
        assert.equal(name, 'find_trades');
        if (args.book === id(2)) return page(name, []);
        return { tool: name, status: 'ambiguous', field: 'book', request: args, candidates: [{ id: id(2), name: 'Group FX' }] };
    } });
    const session = fixtureSession();
    await ask(agent, 'Find trades in book Group', session);
    await ask(agent, 'Group FX', session);
});

test('the session stays busy throughout model continuation', async () => {
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    let ready;
    const entered = new Promise(resolve => { ready = resolve; });
    const session = fixtureSession();
    const agent = new Agent({ complete: async () => { ready(); await gate; return final('done'); } }, { execute: async () => assert.fail() });
    const pending = ask(agent, 'Find trades', session);
    await entered;
    try { await assert.rejects(ask(agent, 'Find books', session), { code: 'agent_busy' }); }
    finally { release(); await pending; }
    assert.equal(session.chatBusy, false);
});

test('a truncated completion retries the same step once without re-executing tools', async () => {
    let turns = 0, reads = 0;
    const agent = new Agent({ complete: async (messages, tools, signal, options) => {
        if (++turns === 1) return call('find_trades');
        if (turns === 2) throw new AppError('generation_limit', 'Truncated model output', 422);
        assert.deepEqual(options, { disableThinking: true });
        assert.equal(last(messages).rows[0].id, id(1));
        return final('The first trade is verified.');
    } }, { execute: async name => { reads++; return page(name, [{ id: id(1) }]); } });
    assert.equal((await ask(agent, 'Find trades')).answer, 'The first trade is verified.');
    assert.equal(reads, 1);
    assert.equal(turns, 3);
});

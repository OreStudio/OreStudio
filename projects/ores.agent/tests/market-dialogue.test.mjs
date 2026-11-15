// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { Agent } from '../dist/agent.js';
import { interpretMarketPoint } from '../dist/market-dialogue.js';

const signal = new AbortController().signal;
const asOf = '2026-11-06';
const candidates = ['ore', 'bloomberg'].map((provider, index) => ({
    provider, seriesId: '11111111-1111-4111-8111-00000000000' + (index + 1), instrument: 'fixture',
}));
const choice = {
    kind: 'market_point', name: 'read_fx_spot', field: 'series',
    args: { baseCurrency: 'EUR', quoteCurrency: 'USD', asOf }, candidates,
};
const proposes = proposal => ({
    complete: async () => ({ tool_calls: [{
        function: { name: 'interpret_market_reply', arguments: JSON.stringify(proposal) },
    }] }),
});

test('a proposed UUID must agree with the option mentioned in the reply', async () => {
    const model = proposes({ action: 'select', selection: candidates[0].seriesId });
    const result = await interpretMarketPoint(model, choice, [], 'Could you pick the second one?', signal);
    assert.equal(result.action, 'clarify');
    const two = await interpretMarketPoint(model, choice, [], 'Could you pick the first or second one?', signal);
    assert.equal(two.action, 'clarify');
});

test('a UUID proposed for a named provider preserves the provider selection', async () => {
    const anotherBloombergSeries = { ...candidates[1], seriesId: '11111111-1111-4111-8111-000000000003' };
    const pending = { ...choice, candidates: [...candidates, anotherBloombergSeries] };
    const model = proposes({ action: 'select', updates: [{ field: 'seriesId', text: candidates[1].seriesId }] });
    const result = await interpretMarketPoint(model, pending, [], 'Please go with Bloomberg for that one.', signal);
    assert.deepEqual(result, { action: 'update', args: { ...choice.args, provider: 'bloomberg' } });

    const wrongProvider = proposes({ action: 'select', selection: candidates[0].seriesId });
    assert.equal((await interpretMarketPoint(wrongProvider, pending, [], 'Please go with Bloomberg for that one.', signal)).action, 'clarify');
});

test('an option and an explicit new date can be selected together', async () => {
    const model = proposes({ action: 'select', selection: candidates[1].seriesId });
    const result = await interpretMarketPoint(model, choice, [], 'The second one on 2026-11-05, please.', signal);
    assert.deepEqual(result, { action: 'update', args: {
        ...choice.args, asOf: '2026-11-05', seriesId: candidates[1].seriesId,
    } });
});

test('clarifying two unlabeled periods preserves the date and IR identity', async () => {
    const calls = [];
    const agent = new Agent({ complete: async () => assert.fail('Explicit selectors need no model') }, {
        execute: async (name, args) => {
            calls.push(args);
            return { tool: name, status: 'no_observation', request: args, asOf, historicalRequest: true };
        },
    });
    const session = { active: true, expires: Date.now() + 60000, sessionId: 'session', tenantId: 'tenant', partyId: 'party' };
    const send = message => agent.chat({ message }, session, signal);
    const pending = await send('What was the GBP SONIA 3M 5Y swap rate from provider bloomberg on 2026-11-06?');
    assert.equal(pending.selectionRequired, 'term');
    assert.equal(calls.length, 0);
    const floating = await send('Use a floating tenor of three months.');
    assert.equal(floating.selectionRequired, 'term');
    assert.equal(calls.length, 0);
    await send('Five years.');
    assert.deepEqual(calls[0], {
        currency: 'GBP', instrument: 'ir_swap', term: '5Y', asOf, maxAgeHours: 72,
        provider: 'bloomberg', floatingTenor: '3M', indexName: 'SONIA',
    });
});

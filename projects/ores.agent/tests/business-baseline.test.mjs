import test from 'node:test';
import assert from 'node:assert/strict';
import { inspectBusiness } from '../dist/business-baseline.js';
const tenant = '11111111-1111-4111-8111-111111111111', party = '22222222-2222-4222-8222-222222222222';
const id = n => `33333333-3333-4333-8333-${String(n).padStart(12, '0')}`;
const entity = n => ({ id: id(n), tenant_id: tenant, party_id: party });
function fixture() {
    const calls = [], session = { tenantId: tenant, partyId: party, token: 'private', active: true, expires: Date.now() + 120000 };
    const data = { series: [], trades: [{ ...entity(1), trade_type: 'FxForward' }], books: [{ ...entity(2), name: 'fixture book', parent_portfolio_id: id(3) }],
        portfolios: [{ ...entity(3), name: 'fixture portfolio' }], reportDefinitions: [{ ...entity(4), name: 'fixture report', report_type: 'npv' }],
        reportInstances: [{ ...entity(5), name: 'fixture run', definition_id: id(4), priced_at: '2026-11-06', completed_at: '2026-11-06T12:00:00Z', price_note: '' }],
        runResults: [{ ...entity(6), report_instance_id: id(5), subject_type: 'trade', subject_id: id(1), measure: 'npv', currency: 'USD', as_of: '2026-11-06', figure: 123.45, source: 'labelled.fixture' }] };
    const keys = { series: 'market_series', trades: 'trades', books: 'books', portfolios: 'portfolios', reportDefinitions: 'definitions', reportInstances: 'instances', runResults: 'results' };
    const backend = { request: async (op, args, s) => {
        assert.equal(s, session); calls.push(op);
        if (op === 'runHealth') return { result: { outcome: 'ok' }, health: 'completed clean', state_name: 'Completed' };
        return { result: { outcome: 'ok' }, total: data[op].length, [keys[op]]: data[op].slice(args.offset, args.offset + args.limit) };
    } };
    return { backend, session, data, calls };
}
test('baseline establishes a real relationship between trade, dated result and clean completed run without exposing tokens', async () => {
    const f = fixture(), r = await inspectBusiness(f.backend, f.session);
    assert.equal(r.reportingReady, true); assert.equal(r.npvExample.figure, 123.45);
    assert.ok(!JSON.stringify(r).includes('private')); assert.ok(f.calls.every(c => !/put|trigger|delete|schedule/.test(c)));
});
test('empty data and wrong-party results are not interpreted as available reporting', async () => {
    const f = fixture(); f.data.runResults[0].party_id = id(8);
    const r = await inspectBusiness(f.backend, f.session); assert.equal(r.reportingReady, false); assert.equal(r.npvExample, null);
    assert.equal(r.findings.runResults.selectedPartyCount, 0);
});
test('an NPV row without clean run health or matching pricing date is not a verified example', async () => {
    const f = fixture(), original = f.backend.request;
    f.backend.request = async (op, ...args) => op === 'runHealth' ? { result: { outcome: 'ok' }, health: 'completed with errors', state_name: 'completed' } : original(op, ...args);
    assert.equal((await inspectBusiness(f.backend, f.session)).reportingReady, false);
    f.backend.request = original; f.data.runResults[0].as_of = '2026-11-05';
    assert.equal((await inspectBusiness(f.backend, f.session)).reportingReady, false);
});
test('denied, malformed and incomplete replies are distinct from an empty successful read', async () => {
    for (const response of [{ result: { outcome: 'denied', code: 'forbidden' }, total: 0 }, { broken: true }, { result: { outcome: 'ok' }, total: 10, trades: [] }]) {
        const f = fixture(), request = f.backend.request;
        f.backend.request = async (op, ...args) => op === 'trades' ? response : request(op, ...args);
        const r = await inspectBusiness(f.backend, f.session);
        assert.notEqual(r.findings.trades.status, 'ok'); assert.equal(r.reportingReady, false);
    }
});

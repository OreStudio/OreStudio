import test from 'node:test';
import assert from 'node:assert/strict';
import { Reporting, formatReporting } from '../dist/reporting.js';
const id = n => `11111111-1111-4111-8111-${String(n).padStart(12, '0')}`;
function fixture() {
    const session = { tenantId: id(1), partyId: id(2), active: true, expires: Date.now() + 120000 }, calls = [];
    const scoped = n => ({ id: id(n), tenant_id: id(1), party_id: id(2) });
    const trade = scoped(3);
    const run = { ...scoped(4), name: 'labelled fixture run', definition_id: id(5), priced_at: '2026-11-06 00:00:00.000000Z', price_note: '', completed_at: '2026-11-06 12:00:00.000000Z', cut_id: null, cut_label: null, cut_timepoint: null, engine_settings_id: null, engine_settings_version: null };
    const rows = [{ ...scoped(6), report_instance_id: id(4), report_definition_id: id(5), as_of: '2026-11-06 00:00:00.000000Z', subject_type: 'trade', subject_id: id(3), measure: 'npv', currency: 'GBP', figure: 0, source: 'labelled.fixture' }];
    const runs = [run], health = new Map([[id(4), 'completed clean']]);
    const backend = { request: async (op, args, s) => {
        assert.equal(s, session); calls.push(op); const ok = x => ({ result: { outcome: 'ok' }, ...x });
        if (op === 'trade') return ok({ trade });
        if (op === 'runHealth') return ok({ health: health.get(args.report_instance_id), state_name: 'fixture state' });
        let list = op === 'runResults' ? rows : runs;
        if (op === 'runResults') list = list.filter(r => Object.entries(args.filter).every(([k, v]) => v === null || r[k] === v));
        else if (args.filter?.id_one_of) list = list.filter(r => args.filter.id_one_of.includes(r.id));
        return ok({ total: list.length, [op === 'runResults' ? 'results' : 'instances']: list.slice(args.offset, args.offset + args.limit) });
    } };
    return { report: new Reporting(backend, () => Date.parse('2026-11-06T18:00:00Z')), backend, session, run, rows, runs, trade, health, calls };
}
const args = { tradeId: id(3) };
test('clean NPV zero is a real value with date, currency, source and run identity', async () => {
    const f = fixture(), r = await f.report.execute('read_trade_results', args, f.session);
    assert.equal(r.status, 'ok'); assert.equal(r.result.figure, 0); assert.equal(r.result.currency, 'GBP');
    assert.match(formatReporting(r), /labelled.fixture/); assert.match(formatReporting(r), /Actual pricing date/);
});
test('partial values require an explicit run and opt-in; running and failed outputs are always withheld', async () => {
    const f = fixture(); f.health.set(id(4), 'completed with errors');
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).status, 'unavailable');
    assert.equal((await f.report.execute('read_trade_results', { ...args, allowPartial: true }, f.session)).status, 'selection_required');
    const partial = await f.report.execute('read_trade_results', { ...args, runId: id(4), allowPartial: true }, f.session);
    assert.equal(partial.status, 'partial'); assert.match(formatReporting(partial), /not a clean valuation/);
    for (const health of ['failed', 'running', '']) { f.health.set(id(4), health); assert.equal((await f.report.execute('read_trade_results', { ...args, runId: id(4), allowPartial: true }, f.session)).result, undefined); }
});
test('missing rows do not become zero and mixed currencies require clarification', async () => {
    const f = fixture(); f.rows.push({ ...f.rows[0], id: id(7), currency: 'USD', figure: 123 });
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).status, 'ambiguous');
    assert.equal((await f.report.execute('read_trade_results', { ...args, currency: 'USD' }, f.session)).result.figure, 123);
    f.rows.length = 0; assert.equal((await f.report.execute('read_trade_results', args, f.session)).result, undefined);
});
test('trade and result scope, pricing date and report definition cannot be substituted', async () => {
    const f = fixture(); f.trade.party_id = id(9);
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).status, 'not_found'); assert.ok(!f.calls.includes('runResults'));
    f.trade.party_id = id(2); const original = f.backend.request;
    f.backend.request = async (op, ...rest) => { const r = await original(op, ...rest); if (op === 'runResults') return { ...r, results: r.results.map(x => ({ ...x, party_id: id(9) })) }; return r; };
    await assert.rejects(() => f.report.execute('read_trade_results', args, f.session), /requested identity/);
    f.backend.request = original; f.rows[0].report_definition_id = id(8);
    await assert.rejects(() => f.report.execute('read_trade_results', args, f.session), /provenance disagree/);
});
test('historical stored prices stay dated; explicit valuation date is not replaced by completion date', async () => {
    const f = fixture(); f.run.priced_at = '2016-12-30 00:00:00.000000Z'; f.rows[0].as_of = f.run.priced_at;
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).status, 'stale');
    const r = await f.report.execute('read_trade_results', { ...args, valuationDate: '2016-12-30' }, f.session);
    assert.equal(r.status, 'ok'); assert.equal(r.historicalRequest, true);
    assert.equal((await f.report.execute('read_trade_results', { ...args, valuationDate: '2016-12-29' }, f.session)).result, undefined);
});
test('different report configurations require a run choice; health changes invalidate a result', async () => {
    const f = fixture(); f.runs.push({ ...f.run, id: id(8), definition_id: id(9) }); f.rows.push({ ...f.rows[0], id: id(10), report_instance_id: id(8), report_definition_id: id(9) }); f.health.set(id(8), 'completed clean');
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).status, 'ambiguous');
    const g = fixture(), original = g.backend.request; let count = 0;
    g.backend.request = async (op, ...rest) => op === 'runHealth' && ++count === 2 ? { result: { outcome: 'ok' }, health: 'running', state_name: 'running' } : original(op, ...rest);
    assert.equal((await g.report.execute('read_trade_results', args, g.session)).status, 'incomplete');
});
test('expired sessions, denied responses and partial pages fail closed', async () => {
    const f = fixture(); await assert.rejects(() => f.report.execute('read_trade_results', args, { ...f.session, active: false }));
    f.backend.request = async () => ({ result: { outcome: 'denied' } }); await assert.rejects(() => f.report.execute('find_report_runs', {}, f.session), /authorize/);
    f.backend.request = async () => ({ result: { outcome: 'ok' }, total: 5, instances: [] }); await assert.rejects(() => f.report.execute('find_report_runs', {}, f.session), /incomplete/);
});
test('future pricing metadata and missing currency units cannot produce a usable figure', async () => {
    const f = fixture(); f.run.priced_at = '2099-01-01'; f.rows[0].as_of = '2099-01-01';
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).result, undefined);
    const g = fixture(); g.rows[0].currency = '';
    await assert.rejects(() => g.report.execute('read_trade_results', args, g.session));
});
test('run discovery permits non-monetary rows without currency while NPV answers still require units', async () => {
    const f = fixture(); f.rows.push({ ...f.rows[0], id: id(10), measure: 'notional', currency: '' });
    assert.equal((await f.report.execute('find_report_runs', { tradeId: id(3) }, f.session)).rows.length, 1);
    assert.equal((await f.report.execute('read_trade_results', args, f.session)).result.currency, 'GBP');
});

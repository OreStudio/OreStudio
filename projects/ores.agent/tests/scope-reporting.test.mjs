import test from 'node:test';
import assert from 'node:assert/strict';
import { ScopeReporting, decimalSum, formatScopeReporting } from '../dist/scope-reporting.js';
const id = n => `11111111-1111-4111-8111-${String(n).padStart(12, '0')}`;
const fixedNow = Date.parse('2026-11-07T08:00:00Z');
function fixture() {
    const session = { tenantId: id(1), partyId: id(2), active: true, expires: Date.now() + 120000 }, calls = [];
    const own = n => ({ id: id(n), tenant_id: id(1), party_id: id(2), recorded_at: '2026-11-05T00:00:00Z' });
    const run = { ...own(3), name: 'labelled clean fixture', definition_id: id(4), started_at: '2026-11-06T10:00:00Z', completed_at: '2026-11-06T10:10:00Z', priced_at: '2026-11-06T00:00:00Z', price_note: '', cut_id: null, cut_label: null, cut_timepoint: null, engine_settings_id: id(5), engine_settings_version: 1 };
    const books = [{ ...own(6), name: 'FX Book', parent_portfolio_id: id(8) }, { ...own(7), name: 'Loans Book', parent_portfolio_id: id(9) }];
    const portfolios = [{ ...own(8), name: 'Child Portfolio', parent_portfolio_id: id(9) }, { ...own(9), name: 'Root Portfolio', parent_portfolio_id: null }];
    const trades = [own(10), own(11), own(12)];
    const bookings = trades.map((t, i) => ({ tenant_id: id(1), trade_id: t.id, book_id: books[i === 2 ? 1 : 0].id, recorded_at: t.recorded_at }));
    const rows = trades.map((t, i) => ({ ...own(20 + i), report_instance_id: run.id, report_definition_id: run.definition_id, as_of: run.priced_at, subject_type: 'trade', subject_id: t.id, measure: 'npv_base', currency: 'GBP', figure: [0.1, 0.2, -10][i], source: 'npv.csv' }));
    const data = { books, portfolios, trades, tradeBookings: bookings, runResults: rows, reportInstances: [run] };
    const keys = { books: 'books', portfolios: 'portfolios', trades: 'trades', tradeBookings: 'trade_bookings', runResults: 'results', reportInstances: 'instances' };
    const f = { session, calls, run, books, portfolios, trades, bookings, rows, data, health: 'completed clean' };
    const backend = { request: async (op, args, s) => {
        assert.equal(s, session); calls.push({ op, args });
        const ok = value => ({ result: { outcome: 'ok' }, ...value });
        if (op === 'runHealth') return ok({ health: f.health, state_name: 'completed' });
        if (op === 'reportMetrics') {
            if (args.filter.family === 'sensitivity_report') return ok({ total: (f.greekMetrics ?? []).length, metrics: (f.greekMetrics ?? []).slice(args.offset, args.offset + args.limit) });
            const metrics = rows.filter(r => args.filter.subject_id_one_of.includes(r.subject_id)).map(r => ({ ...r, measure: 'NPV(Base)', family: 'npv_report', scenario: '', factor: '', qualifiers: JSON.stringify({ BaseCurrency: f.metricCurrency ?? r.currency }), value: r.figure }));
            return ok({ total: metrics.length, metrics: metrics.slice(args.offset, args.offset + args.limit) });
        }
        let list = data[op]; assert.ok(list, op);
        if (op === 'reportInstances') list = list.filter(r => args.filter.id_one_of.includes(r.id));
        if (op === 'tradeBookings') list = list.filter(r => args.filter.trade_id_one_of.includes(r.trade_id));
        if (op === 'runResults') list = list.filter(r => Object.entries(args.filter).every(([k, v]) => v === null || (k.endsWith('_one_of') ? v.includes(r[k.slice(0, -7)]) : r[k] === v)));
        return ok({ total: list.length, [keys[op]]: list.slice(args.offset, args.offset + args.limit) });
    } };
    f.backend = backend; f.report = new ScopeReporting(backend, () => fixedNow); return f;
}
const args = { scopeType: 'book', scope: 'FX Book', runId: id(3) };
test('scope totals use historical bookings, exact decimal arithmetic and bounded ranked drill-down', async () => {
    const f = fixture(), r = await f.report.execute({ ...args, limit: 1 }, f.session);
    assert.equal(r.status, 'ok'); assert.equal(r.total.figure, '0.3'); assert.equal(r.total.currency, 'GBP');
    assert.deepEqual(r.coverage, { expectedTradeCount: 2, valuedTradeCount: 2, missingTradeCount: 0, duplicateTradeCount: 0, failedTradeCount: null });
    assert.equal(r.contributors[0].tradeId, id(11)); assert.equal(r.nextOffset, 1); assert.equal(r.membershipAt, f.run.started_at);
    for (const c of f.calls.filter(c => ['books', 'trades', 'tradeBookings'].includes(c.op))) assert.equal(c.args.as_of, r.membershipAt);
    const next = await f.report.execute({ ...args, offset: 1, limit: 1 }, f.session);
    assert.equal(next.total.figure, '0.3'); assert.equal(next.contributors[0].tradeId, id(10)); assert.equal(next.nextOffset, null);
    assert.match(formatScopeReporting(r), /Coverage: 2\/2/); assert.match(formatScopeReporting(r), /Stored results only/); assert.doesNotThrow(() => JSON.stringify(r));
});

test('historical membership preserves microseconds and rejects newer submillisecond rows', async () => {
    const f = fixture(); f.run.started_at = '2026-11-06 10:00:00.000100Z';
    f.books[0].recorded_at = f.run.started_at;
    const r = await f.report.execute(args, f.session);
    assert.equal(r.membershipAt, f.run.started_at);
    for (const c of f.calls.filter(c => ['books', 'trades', 'tradeBookings'].includes(c.op))) assert.equal(c.args.as_of, f.run.started_at);
    f.books[0].recorded_at = '2026-11-06 10:00:00.000101Z';
    await assert.rejects(() => f.report.execute(args, f.session), /newer than/);
    f.run.completed_at = '2026-11-06 10:00:00.000099Z';
    await assert.rejects(() => f.report.execute(args, f.session), /dates are inconsistent/);
});
test('portfolio booking subtrees reconcile with books and do not use direct assignments or current export', async () => {
    const f = fixture();
    const parent = await f.report.execute({ ...args, scopeType: 'portfolio', scope: 'Root Portfolio' }, f.session);
    const child = await f.report.execute({ ...args, scopeType: 'portfolio', scope: 'Child Portfolio' }, f.session);
    assert.equal(parent.total.figure, '-9.7'); assert.equal(parent.scope.bookCount, 2); assert.equal(parent.coverage.expectedTradeCount, 3);
    assert.equal(child.total.figure, '0.3'); assert.match(formatScopeReporting(parent), /direct trade assignments are separate/);
    assert.ok(!f.calls.some(c => ['exportPortfolio', 'tradePortfolios'].includes(c.op)));
});
test('run selection and unavailable Greeks never silently substitute an NPV total', async () => {
    const f = fixture(); const { runId: _, ...noRun } = args;
    assert.equal((await f.report.execute(noRun, f.session)).status, 'selection_required');
    for (const measure of ['greeks', 'delta', 'gamma', 'vega', 'dv01']) {
        const r = await f.report.execute({ ...args, measure }, f.session); assert.equal(r.status, 'unavailable'); assert.equal(r.total, undefined); assert.match(formatScopeReporting(r), /Missing or suppressed sensitivities are not zero/);
    }
    assert.ok(f.calls.some(c => c.op === 'reportMetrics')); assert.ok(!f.calls.some(c => c.op === 'runResults'));
});

test('Greek scope reads preserve historical identity, full coverage, source evidence and deterministic formatting', async () => {
    const f = fixture();
    f.greekMetrics = f.trades.slice(0,2).map((t,i) => ({ ...f.rows[i], id:id(70+i), result_id:id(90), line:i+2, value:[0.1,0.2][i], measure:'Delta', family:'sensitivity_report', source:'sensitivity.csv', currency:'GBP', scenario:'', factor:'', qualifiers:JSON.stringify({ IsPar:'false', Factor_1:'DiscountCurve/GBP/0/1Y', ShiftSize_1:'0.000100', ShiftSize_2:'0.000000', Currency:'GBP' }) }));
    const r = await f.report.execute({ ...args, measure:'dv01', factor:'DiscountCurve/GBP/0/1Y', limit:1 }, f.session);
    assert.equal(r.status,'ok'); assert.equal(r.buckets[0].total.figure,'0.3'); assert.equal(r.buckets[0].coverage.expectedTradeCount,2);
    assert.equal(r.buckets[0].contributors[0].tradeId,id(11)); assert.equal(r.buckets[0].nextOffset,1);
    assert.match(formatScopeReporting(r),/cash finite difference: 0.3 GBP/); assert.match(formatScopeReporting(r),/target shift 0.0001/);
    assert.ok(!f.calls.some(c => c.op === 'runResults'));
    f.greekMetrics.pop(); const missing=await f.report.execute({ ...args, measure:'delta' },f.session);
    assert.equal(missing.status,'incomplete'); assert.equal(missing.buckets[0].total,null);
    f.greekMetrics[0].party_id=id(99);
    await assert.rejects(()=>f.report.execute({...args,measure:'delta'},f.session),/identity/);
});
test('failed, running and partial runs cannot produce scope totals', async () => {
    for (const state of ['failed', 'running', 'completed with errors']) {
        const f = fixture(); f.health = state;
        const r = await f.report.execute(args, f.session); assert.equal(r.status, 'unavailable'); assert.equal(r.total, undefined); assert.ok(!f.calls.some(c => c.op === 'books'));
    }
});
test('missing, duplicate and empty populations are not zero and withhold totals', async () => {
    const f = fixture(); f.rows.shift(); const missing = await f.report.execute(args, f.session);
    assert.equal(missing.status, 'incomplete'); assert.equal(missing.coverage.missingTradeCount, 1); assert.equal(missing.coverage.failedTradeCount, null); assert.equal(missing.total, undefined);
    const g = fixture(); g.rows.push({ ...g.rows[0], id: id(50) }); const duplicate = await g.report.execute(args, g.session);
    assert.equal(duplicate.coverage.duplicateTradeCount, 1); assert.equal(duplicate.total, undefined);
    const h = fixture(); h.bookings.length = 0; const empty = await h.report.execute(args, h.session);
    assert.equal(empty.status, 'empty_scope'); assert.equal(empty.total, undefined);
    const zero = fixture(); zero.rows[0].figure = 0; zero.rows[1].figure = 0;
    assert.equal((await zero.report.execute(args, zero.session)).total.figure, '0');
});
test('mixed currencies or a requested different currency cannot filter away uncovered trades', async () => {
    const f = fixture(); f.rows[0].currency = 'USD';
    for (const currency of [undefined, 'GBP']) { const r = await f.report.execute({ ...args, currency }, f.session); assert.equal(r.status, 'ambiguous'); assert.equal(r.total, undefined); assert.deepEqual(r.currencies, ['GBP', 'USD']); }
    const g = fixture(); const wrong = await g.report.execute({ ...args, currency: 'USD' }, g.session); assert.equal(wrong.total, undefined);
});
test('source and provenance mismatches, foreign identities and nonfinite figures fail closed', async () => {
    const source = fixture(); source.rows[0].source = 'sensitivity.csv'; assert.equal((await source.report.execute(args, source.session)).total, undefined);
    for (const [key, value] of [['report_definition_id', id(99)], ['as_of', '2026-11-05T00:00:00Z'], ['party_id', id(99)], ['tenant_id', id(99)], ['figure', Infinity]]) {
        const f = fixture(), original = f.backend.request;
        f.backend.request = async (op, ...rest) => { const r = await original(op, ...rest); if (op === 'runResults') r.results[0][key] = value; return r; };
        await assert.rejects(() => f.report.execute(args, f.session));
    }
    const foreign = fixture(); foreign.run.party_id = id(99); assert.equal((await foreign.report.execute(args, foreign.session)).status, 'not_found'); assert.ok(!foreign.calls.some(c => c.op === 'tradeBookings'));
});
test('ambiguous names, unknown historical parents and cyclic hierarchies never select a scope', async () => {
    const f = fixture(); f.books.push({ ...f.books[0], id: id(50) }); const r = await f.report.execute(args, f.session);
    assert.equal(r.status, 'ambiguous'); assert.equal(r.candidates.length, 2); assert.equal(r.total, undefined);
    for (const parent of [id(99), id(8)]) { const g = fixture(); g.portfolios[1].parent_portfolio_id = parent; await assert.rejects(() => g.report.execute({ ...args, scopeType: 'portfolio', scope: 'Root Portfolio' }, g.session), /hierarchy|parent/); }
    const upper = fixture(); upper.run.id = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'; upper.rows.forEach(r => { r.report_instance_id = upper.run.id; });
    upper.books[0].id = 'abcdefab-cdef-abcd-efab-cdefabcdefab'; upper.bookings.slice(0, 2).forEach(b => { b.book_id = upper.books[0].id; });
    assert.equal((await upper.report.execute({ ...args, runId: upper.run.id.toUpperCase(), scope: upper.books[0].id.toUpperCase() }, upper.session)).total.figure, '0.3');
});
test('ignored historical timestamps, denied reads, cancellation and logout cannot return a total', async () => {
    const f = fixture(); f.books[0].recorded_at = '2026-11-07T00:00:00Z'; await assert.rejects(() => f.report.execute(args, f.session), /newer/);
    const denied = fixture(); denied.backend.request = async () => ({ result: { outcome: 'denied' } }); await assert.rejects(() => denied.report.execute(args, denied.session), /refused/);
    const cancelled = fixture(); const controller = new AbortController(); controller.abort(); await assert.rejects(() => cancelled.report.execute(args, cancelled.session, controller.signal));
    const loggedOut = fixture(); loggedOut.session.active = false; await assert.rejects(() => loggedOut.report.execute(args, loggedOut.session), /Sign in/);
    const midRead = fixture(), original = midRead.backend.request;
    midRead.backend.request = async (op, ...rest) => { const r = await original(op, ...rest); if (op === 'reportMetrics') midRead.session.active = false; return r; };
    await assert.rejects(() => midRead.report.execute(args, midRead.session), /Sign in/);
});
test('run health/provenance changes and malformed dates invalidate a computed total', async () => {
    for (const mode of ['health', 'metadata']) {
        const f = fixture(), original = f.backend.request; let calls = 0;
        f.backend.request = async (op, ...rest) => { if (op === (mode === 'health' ? 'runHealth' : 'reportInstances') && ++calls === 2) { if (mode === 'health') f.health = 'running'; else f.run.price_note = 'changed'; } return original(op, ...rest); };
        const r = await f.report.execute(args, f.session); assert.equal(r.status, 'incomplete'); assert.equal(r.total, undefined);
    }
    const future = fixture(); future.run.completed_at = '2099-01-01T00:00:00Z'; await assert.rejects(() => future.report.execute(args, future.session), /future/);
    const missing = fixture(); missing.run.started_at = null; assert.equal((await missing.report.execute(args, missing.session)).status, 'unavailable');
});
test('historical values remain visibly dated and explicit valuation dates must match', async () => {
    const f = fixture(); const report = new ScopeReporting(f.backend, () => Date.parse('2026-12-01T00:00:00Z'));
    const stale = await report.execute(args, f.session); assert.equal(stale.status, 'stale'); assert.match(formatScopeReporting(stale), /historical/);
    assert.equal((await report.execute({ ...args, valuationDate: '2026-11-06' }, f.session)).status, 'ok');
    assert.equal((await report.execute({ ...args, valuationDate: '2026-11-05' }, f.session)).total, undefined);
});
test('pagination covers all expected members and inconsistent/duplicate pages cannot yield a total', async () => {
    const f = fixture();
    for (let i = 0; i < 55; i++) { const t = { ...f.trades[0], id: id(100 + i) }; f.trades.push(t); f.bookings.push({ ...f.bookings[0], trade_id: t.id }); f.rows.push({ ...f.rows[0], id: id(200 + i), subject_id: t.id, figure: 1 }); }
    const r = await f.report.execute(args, f.session); assert.equal(r.coverage.expectedTradeCount, 57); assert.equal(r.total.figure, '55.3'); assert.ok(f.calls.some(c => c.op === 'runResults' && c.args.offset === 50));
    for (const mode of ['short', 'duplicate']) { const g = fixture(), original = g.backend.request;
        g.backend.request = async (op, ...rest) => { const r = await original(op, ...rest); if (op === 'books') return mode === 'short' ? { ...r, total: 20 } : { ...r, books: [r.books[0], r.books[0]] }; return r; };
        await assert.rejects(() => g.report.execute(args, g.session), /incomplete|Duplicate/);
    }
});
test('decimal summation handles exponents, cancellation and negative zero without binary accumulation', () => {
    assert.equal(decimalSum([0.1, 0.2]), '0.3'); assert.equal(decimalSum([1e21, 0.3, -1e21]), '0.3');
    assert.equal(decimalSum([-0, 0]), '0'); assert.equal(decimalSum([-1.25, 0.25]), '-1'); assert.equal(decimalSum([1e-7, 2e-7]), '0.0000003');
    assert.throws(() => decimalSum([Infinity]));
});

test('base units join the explicit same-row CSV qualifier, not a requested or book currency', async () => {
    const f = fixture(); f.metricCurrency = 'GBP'; f.rows.forEach(r => { r.currency = ''; });
    const r = await f.report.execute(args, f.session); assert.equal(r.total.figure, '0.3'); assert.equal(r.total.currency, 'GBP'); assert.equal(r.total.currencyBasis, 'typed_metric.BaseCurrency');
    for (const mode of ['missing', 'duplicate', 'value', 'currency', 'qualifiers', 'scope', 'scenario']) {
        const g = fixture(), original = g.backend.request;
        g.backend.request = async (op, ...rest) => { const r = await original(op, ...rest); if (op === 'reportMetrics') {
            if (mode === 'missing') { r.metrics.pop(); r.total--; }
            if (mode === 'duplicate') { r.metrics.push({ ...r.metrics[0], id: id(99) }); r.total++; }
            if (mode === 'value') r.metrics[0].value++;
            if (mode === 'currency') r.metrics[0].currency = 'USD';
            if (mode === 'qualifiers') r.metrics[0].qualifiers = '{}';
            if (mode === 'scope') r.metrics[0].party_id = id(99);
            if (mode === 'scenario') r.metrics[0].scenario = 'stress';
        } return r; };
        if (['missing', 'duplicate'].includes(mode)) assert.equal((await g.report.execute(args, g.session)).total, undefined, mode);
        else await assert.rejects(() => g.report.execute(args, g.session), /currency|Currency|provenance|figure|metric/i);
    }
});

test('catalogue and result-population ceilings withhold totals and respect booking batch bounds', async () => {
    const f = fixture(), original = f.backend.request;
    f.backend.request = async (op, args, ...rest) => op === 'books' ? { result: { outcome: 'ok' }, total: 2001, books: Array.from({ length: 50 }, (_, i) => ({ ...f.books[0], id: id(100 + args.offset + i) })) } : original(op, args, ...rest);
    await assert.rejects(() => f.report.execute(args, f.session), /2,000-row/);
    const g = fixture();
    for (let i = 0; i < 1000; i++) { const t = { ...g.trades[0], id: id(100 + i) }; g.trades.push(t); g.bookings.push({ ...g.bookings[0], trade_id: t.id }); }
    const result = await g.report.execute(args, g.session); assert.equal(result.status, 'incomplete'); assert.equal(result.total, undefined); assert.match(result.reason, /1,000-trade/);
    assert.ok(g.calls.filter(c => c.op === 'tradeBookings').every(c => c.args.filter.trade_id_one_of.length <= 500));
});

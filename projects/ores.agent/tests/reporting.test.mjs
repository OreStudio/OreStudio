import test from 'node:test';
import assert from 'node:assert/strict';
import { Reporting, formatReporting } from '../dist/reporting.js';
import { readIntent, preserveReadSelectors } from '../dist/intent.js';
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
        if (op === 'runHealthBatch') return ok({health:args.instance_ids.map(id=>({report_instance_id:id,health:health.get(id),state_name:'fixture state'}))});
        let list = op === 'runResults' ? rows : runs;
        if (op === 'runResults') list = list.filter(r => Object.entries(args.filter).every(([k, v]) => v === null || r[k] === v));
        else if (args.filter?.id_one_of) list = list.filter(r => args.filter.id_one_of.includes(r.id));
        return ok({ total: list.length, [op === 'runResults' ? 'results' : 'instances']: list.slice(args.offset, args.offset + args.limit) });
    } };
    return { report: new Reporting(backend, () => Date.parse('2026-11-06T18:00:00Z')), backend, session, run, rows, runs, trade, health, calls };
}
const args = { tradeId: id(3) };

test('explicit completed-run selection cannot be broadened by model arguments',()=>{
    const plan=readIntent('List the latest completed report runs.');
    assert.equal(preserveReadSelectors('find_report_runs',{},plan).status,'completed');
    assert.throws(()=>preserveReadSelectors('find_report_runs',{status:'all'},plan),/explicit user request/);
});

test('batch health must cover exactly the selected-party runs without duplicates or extra identities',async()=>{
    for(const mode of ['missing','duplicate','foreign']){
        const f=fixture(),original=f.backend.request;
        f.backend.request=async(op,args,s)=>{
            if(op!=='runHealthBatch')return original(op,args,s);
            const row={report_instance_id:mode==='foreign'?id(99):id(4),health:'completed clean',state_name:'completed'};
            return {result:{outcome:'ok'},health:mode==='missing'?[]:mode==='duplicate'?[row,row]:[row]};
        };
        await assert.rejects(()=>f.report.execute('find_report_runs',{status:'completed'},f.session),error=>error.code==='incomplete_reporting'||error.name==='ZodError');
    }
});

test('run discovery scans a large tenant catalogue before filtering to the selected party',async()=>{
    const f=fixture();
    const foreign=Array.from({length:2200},(_,i)=>({...f.run,id:id(100+i),party_id:id(99)}));
    f.runs.splice(0,1,...foreign,f.run);
    const r=await f.report.execute('find_report_runs',{},f.session);
    assert.equal(r.totalMatches,1);assert.equal(r.rows[0].id,id(4));
    assert.ok(r.rows.every(x=>x.party_id===f.session.partyId));
    assert.equal(f.calls.filter(op=>op==='runHealth').length,1);
});

test('latest completed runs sort by completion time and exclude failed/running outputs without reading values',async()=>{
    const f=fixture();
    const newest={...f.run,id:id(8),priced_at:'2026-11-04',completed_at:'2026-11-06T14:00:00Z'};
    const failed={...f.run,id:id(9),completed_at:'2026-11-06T15:00:00Z'};
    const running={...f.run,id:id(10),completed_at:null};
    f.runs.push(newest,failed,running);f.health.set(id(8),'completed with errors');f.health.set(id(9),'failed');f.health.set(id(10),'running');
    const r=await f.report.execute('find_report_runs',{status:'completed',limit:1},f.session);
    assert.equal(r.rows[0].id,id(8));assert.equal(r.rows[0].health,'completed with errors');
    assert.equal(r.hasMore,true);assert.equal(r.nextOffset,1);
    const next=await f.report.execute('find_report_runs',{status:'completed',offset:1,limit:1},f.session);
    assert.equal(next.rows[0].id,id(4));assert.equal(next.hasMore,false);
    assert.ok(!f.calls.includes('runResults'));
    assert.match(formatReporting(r),/completed 2026-11-06T14:00:00Z/);
});
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

test('trade base NPV recovers an empty unit only from its exact stored metric', async () => {
    for (const mode of ['ok', 'missing', 'duplicate', 'different-id', 'different-date', 'different-figure', 'conflicting-unit', 'missing-qualifier']) {
        const f = fixture();
        Object.assign(f.rows[0], { measure: 'npv_base', source: 'npv.csv', currency: '', figure: 123.45 });
        const original = f.backend.request;
        f.backend.request = async (op, query, ...rest) => {
            if (op !== 'reportMetrics') return original(op, query, ...rest);
            assert.equal(query.filter.report_instance_id, id(4));
            assert.equal(query.filter.subject_id, id(3));
            assert.equal(query.filter.family, 'npv_report');
            const metric = { ...f.rows[0], measure: 'NPV(Base)', family: 'npv_report', value: f.rows[0].figure, scenario: '', factor: '', qualifiers: '{"BaseCurrency":"GBP"}' };
            if (mode === 'different-id') metric.id = id(90);
            if (mode === 'different-date') metric.as_of = '2026-11-06T12:00:00Z';
            if (mode === 'different-figure') metric.value++;
            if (mode === 'conflicting-unit') metric.currency = 'USD';
            if (mode === 'missing-qualifier') metric.qualifiers = '{}';
            const metrics = mode === 'missing' ? [] : mode === 'duplicate' ? [metric, { ...metric, id: id(91) }] : [metric];
            return { result: { outcome: 'ok' }, metrics, total: metrics.length };
        };
        const read = currency => f.report.execute('read_trade_results', { ...args, runId: id(4), measure: 'npv_base', currency }, f.session);
        if (mode === 'ok') {
            const result = await read('GBP');
            assert.equal(result.result.currency, 'GBP');
            assert.equal(result.result.figure, 123.45);
            assert.equal(result.currencyBasis, 'typed_metric.BaseCurrency');
            assert.equal((await read('USD')).result, undefined);
        } else await assert.rejects(read('GBP'), /currency|Currency|provenance|metric/, mode);
    }
});

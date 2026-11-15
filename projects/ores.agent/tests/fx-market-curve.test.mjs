// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { Agent } from '../dist/agent.js';
import { ToolExecutor } from '../dist/tools.js';
import { readIntent } from '../dist/intent.js';
import { fixtureSession } from '../evals/fixtures.mjs';
import { marketFixtureBackend, marketId } from '../evals/fx-market-fixtures.mjs';
import { testDatasetConventions } from '../dist/fx-market-request.js';
import { scriptedDialogue } from '../evals/fx-dialogue-fixtures.mjs';
const signal=()=>new AbortController().signal;
const initial='Use EUR/USD spot and forward quotes from provider ore, and USD curve USD-SOFR, as of 2026-01-01.';
const conventions='Spot date 2026-01-01; forward quotes are outrights; unadjusted dates; no end of month; USD tenors use valuation date.';
const complete=initial+' '+conventions;
const fixture=id=>{const backend=marketFixtureBackend(id);return{backend,agent:new Agent(scriptedDialogue(),new ToolExecutor(backend,()=>{})),session:fixtureSession()};};

test('orphaned test convention replies do not call unrelated tools', async () => {
    for(const message of ['use test data set conventions','use test dataset conventions','Please use the test data-set conventions.','Apply the test data set conventions please!']) {
        const f=fixture();
        const orphan=await f.agent.chat({message},f.session,signal());
        assert.match(orphan.answer,/no active curve request/);
        assert.equal(f.backend.calls.length,0);
    }
});

test('test dataset conventions require explicit consent and never override other data or choices', () => {
    const request = { provider: 'agent-e2e', usdCurve: 'USD-AGENT-E2E', asOf: '2016-12-30' };
    assert.equal(request.spotDate, undefined);
    const selected = testDatasetConventions(request);
    assert.equal(selected.spotDate, '2016-12-30');
    assert.equal(selected.forwardQuote, 'outright');
    assert.equal(selected.endOfMonth, false);
    for (const other of [{...request,provider:'ore'}, {...request,usdCurve:'USD-SOFR'}, {...request,asOf:'2026-01-01'}, {...request,forwardQuote:'points'}, {...request,spotDate:'2017-01-03'}]) {
        assert.equal(testDatasetConventions(other), undefined);
    }
});

test('clarification explains missing information and help retains the pending market selection', async () => {
    const f = fixture();
    const first = await f.agent.chat({message:initial},f.session,signal());
    assert.match(first.answer,/I found the stored/);
    assert.match(first.answer,/curve has not been built yet/);
    assert.match(first.answer,/date the spot currency exchange settles/);
    assert.doesNotMatch(first.answer,/use test dataset conventions/);
    const help = await f.agent.chat({message:'What does that mean?'},f.session,signal());
    assert.match(help.answer, /settlement date/);
    assert.equal(help.evidence.length,0);
    const built = await f.agent.chat({message:conventions},f.session,signal());
    assert.equal(built.evidence[0].status, 'ok');
});

test('natural selectors preserve provider, USD curve and labelled dates without JSON',()=>{
    const plan=readIntent(initial);
    assert.equal(plan.analyticsTool,'build_fx_curve_from_market');
    assert.deepEqual(plan.analyticsInput,{provider:'ore',usdCurve:'USD-SOFR',asOf:'2026-01-01'});
    assert.match(readIntent(initial.replace('2026-01-01','2026-13-01')).clarification,/valid/);
    assert.match(readIntent(initial+' forward quotes are points and forward quotes are outrights').clarification,/one forward quote unit/);
});

test('two-turn natural conversation selects existing records and calculates from NATS observations',async()=>{
    const f=fixture();
    const first=await f.agent.chat({message:initial},f.session,signal());
    assert.equal(first.evidence[0].field,'spotDate');assert.match(first.answer,/settlement date/);
    const next=await f.agent.chat({message:conventions},f.session,signal());
    assert.equal(next.evidence[0].status,'ok');assert.equal(next.evidence[0].result.validated,true);
    assert.equal(next.evidence[0].reproduction.observations.length,9);
    assert.ok(f.backend.calls.some(c=>c.op==='series'));assert.ok(f.backend.calls.some(c=>c.op==='observations'));
    assert.ok(f.backend.calls.every(c=>['series','observations'].includes(c.op)));
    assert.match(next.answer,/interpolated/);assert.doesNotMatch(next.answer,/JSON/);
});

test('provider, curve and convention questions accept simple human replies',async()=>{
    const f=fixture();
    let r=await f.agent.chat({message:'Build an EUR/USD forward curve.'},f.session,signal());assert.equal(r.evidence[0].field,'asOf');
    r=await f.agent.chat({message:'2026-13-01'},f.session,signal());assert.match(r.answer,/valid/);assert.equal(r.evidence.length,0);
    r=await f.agent.chat({message:'2026-01-01'},f.session,signal());assert.equal(r.evidence[0].field,'provider');
    r=await f.agent.chat({message:'ore'},f.session,signal());assert.equal(r.evidence[0].field,'usdCurve');
    r=await f.agent.chat({message:'USD curve USD-SOFR'},f.session,signal());assert.equal(r.evidence[0].field,'spotDate');
    r=await f.agent.chat({message:'2026-01-01'},f.session,signal());assert.equal(r.evidence[0].field,'forwardQuote');
    r=await f.agent.chat({message:'outrights'},f.session,signal());assert.equal(r.evidence[0].field,'calendar');
    r=await f.agent.chat({message:'unadjusted'},f.session,signal());assert.equal(r.evidence[0].field,'endOfMonth');
    r=await f.agent.chat({message:'no'},f.session,signal());assert.equal(r.evidence[0].field,'usdPillarAnchor');
    r=await f.agent.chat({message:'valuation date'},f.session,signal());assert.equal(r.evidence[0].status,'ok');
});

test('duplicate curve names require an exact series selection and preserve the original request',async()=>{
    const f=fixture('ambiguous-curve');const first=await f.agent.chat({message:complete},f.session,signal());
    assert.equal(first.evidence[0].field,'usdCurve');assert.equal(first.evidence[0].candidates.length,2);
    const selected=await f.agent.chat({message:marketId(33)},f.session,signal());
    assert.equal(selected.evidence[0].status,'ok');assert.equal(selected.evidence[0].selection.series[2].id,marketId(33));
});

test('missing, stale, conflicting and foreign market data never become a constructed curve',async()=>{
    for(const [id,status] of [['missing-series','not_found'],['foreign-series','not_found'],['stale','no_observation'],['empty-observation','no_observation'],['conflicting-observation','invalid_input']]){
        const f=fixture(id);const r=await f.agent.chat({message:complete},f.session,signal());assert.equal(r.evidence[0].status,status,id);assert.equal(r.evidence[0].result,undefined,id);
    }
    for(const [id,code] of [['foreign-observation','party_mismatch'],['incomplete-page','incomplete_analytics_inputs']]){
        const f=fixture(id);const executor=new ToolExecutor(f.backend,()=>{});
        await assert.rejects(()=>executor.execute('build_fx_curve_from_market',readIntent(complete).analyticsInput,f.session),e=>e.code===code);
    }
});

test('market-selection continuations cannot cross sessions or identities and respect cancellation',async()=>{
    const f=fixture();await f.agent.chat({message:initial},f.session,signal());
    const other=fixtureSession();other.sessionId='other';
    const calls=f.backend.calls.length;
    const orphan=await f.agent.chat({message:conventions},other,signal());
    assert.equal(orphan.evidence[0].field,'asOf');
    assert.equal(orphan.evidence[0].request.provider,undefined);
    assert.equal(f.backend.calls.length,calls);
    const control=new AbortController();control.abort();
    await assert.rejects(()=>f.agent.chat({message:complete},f.session,control.signal));
});

test('orphaned conventions ask for original selectors without inference or losing supplied values', async () => {
    const f=fixture();
    const orphan=await f.agent.chat({message:conventions},f.session,signal());
    assert.equal(orphan.evidence[0].field,'asOf');
    assert.equal(f.backend.calls.length,0);
    assert.match(orphan.answer,/kept the conventions/);
    const result=await f.agent.chat({message:initial},f.session,signal());
    assert.equal(result.evidence[0].status,'ok');
    const other=fixture();
    const preset=await other.agent.chat({message:'use test dataset conventions'},other.session,signal());
    assert.match(preset.answer,/no active curve request/);
    assert.equal(other.backend.calls.length,0);
});

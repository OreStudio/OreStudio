// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { buildCurve, maturity } from '../dist/fx-curve.js';
import { buildSurface, deltaStrike, fxDelta, blackPrice } from '../dist/fx-surface.js';
import { normalCdf, normalInverse } from '../dist/fx-numerics.js';
import { ToolExecutor } from '../dist/tools.js';
import { Agent } from '../dist/agent.js';
import { catalogue } from '../dist/catalogue.js';
import { flatCurve, flatSurface } from './fx-analytics-fixtures.mjs';
import { gradeFxAnalytics } from '../evals/fx-analytics-oracle.mjs';
const close = (actual, expected, tol = 1e-11) => assert.ok(Math.abs(actual - expected) < tol, `${actual} != ${expected}`);
const session = () => ({ active: true, expires: Date.now()+120000, tenantId:'11111111-1111-4111-8111-000000000001',partyId:'11111111-1111-4111-8111-000000000002',sessionId:'s',calls:0,busy:0,window:Date.now() });
const noBackend = { request: async()=>assert.fail('Supplied construction must not call the server') };
const noInference = { complete: async()=>assert.fail('Supplied inputs must not be rewritten by the model') };

test('FX curve recovers an independent exponential carry curve, including spot lag and points', () => {
    const input = flatCurve(); input.spotDate = '2026-01-05'; input.quoteConvention = 'points';
    input.usdCurve.points = [{date:'2026-01-01',discountFactor:1},{date:'2027-02-01',discountFactor:Math.exp(-.05*396/365)}];
    const days = d => (Date.parse(d)-Date.parse(input.spotDate))/86400000;
    input.forwards.forEach((q,i)=>{q.date=maturity(input.spotDate,[1,3,6,12][i],input.calendar);q.value=(input.spot*Math.exp(.03*days(q.date)/365)-input.spot)/input.pipSize;});
    const curve=buildCurve(input);
    assert.equal(curve.result.validated,true);
    for(const q of curve.result.repricing){close(q.eurDiscount,Math.exp(-.02*days(q.date)/365));close(q.residual,0,1e-9);}
    for(const q of curve.result.targets){close(q.outright,1.1*Math.exp(.03*days(q.date)/365));assert.equal(q.classification,'interpolated');}
    assert.equal(curve.result.referenceDate,input.spotDate);
});

test('curve rejects duplicate/invalid inputs, supports negative points, and never extrapolates', () => {
    const x=flatCurve();x.forwards[1].date=x.forwards[0].date;assert.throws(()=>buildCurve(x),/Duplicate/);
    const y=flatCurve();y.usdCurve.points.reverse();assert.throws(()=>buildCurve(y),/start/);
    const z=flatCurve();z.forwards.pop();const r=buildCurve(z).result;
    assert.equal(r.targets[2].classification,'unavailable');assert.equal(r.targets[2].outright,undefined);
    const negative=flatCurve();negative.quoteConvention='points';negative.forwards.forEach(q=>q.value=-10);
    close(buildCurve(negative).result.repricing[0].outright,1.099);
    const invalid=flatCurve();invalid.usdCurve.points[1].discountFactor=0;assert.throws(()=>buildCurve(invalid),/positive/);
});

test('calendar month ends, weekends and holidays are explicit and deterministic',()=>{
    close(normalCdf(1.5),.9331927987311419,2e-15);close(normalInverse(.1),-1.2815515655446008,1e-12);
    assert.equal(maturity('2026-01-31',1,{businessDayConvention:'unadjusted',endOfMonth:false,holidays:[]}),'2026-02-28');
    assert.equal(maturity('2026-01-31',1,{businessDayConvention:'modified_following',endOfMonth:false,holidays:['2026-02-27']}),'2026-02-26');
    assert.equal(maturity('2026-02-28',1,{businessDayConvention:'unadjusted',endOfMonth:true,holidays:[]}),'2026-03-31');
});

test('flat Black reference strikes and all four delta conventions round trip',()=>{
    const input=flatSurface(), result=buildSurface(input);
    assert.equal(result.validated,true);assert.equal(result.grid[0].classification,'interpolated');close(result.grid[0].volatility,.12);
    const s=result.expiries[0], expected={ '10P':1.0209370780047404,'25P':1.0585438959151103,'25C':1.1471455930581398,'10C':1.1894013758720343 };
    for(const n of s.nodes) if(expected[n.label]) close(n.strike,expected[n.label],1e-12);
    const market={forward:1.1,spot:1.1,foreignDiscount:.98,domesticDiscount:.96,time:.5};
    for(const delta of ['forward','spot','premium_adjusted_forward','premium_adjusted_spot']) {
        const c={...input.conventions,delta};
        for(const d of [.1,.25])for(const call of [false,true]){
            const k=deltaStrike(market,.13,d,c,call);close(Math.abs(fxDelta(market,k,.13,c,call)),d,1e-11);
        }
    }
    close(blackPrice(1,1,.2,1,1,true),.07965567455405798,1e-14);
});

test('FX evaluation checks independent numerical references instead of trusting validation flags',()=>{
    const result=buildSurface(flatSurface()), reply={evidence:[{result}]};
    assert.deepEqual(gradeFxAnalytics('fx-surface-flat-reference',reply),[]);
    result.expiries[0].nodes[0].volatility=.35;
    assert.equal(result.validated,true);
    assert.ok(gradeFxAnalytics('fx-surface-flat-reference',reply).some(f=>f.includes('volatility')));
});

test('smile and broker BF quotes recover both wings; interpretation changes the surface',()=>{
    const input=flatSurface();input.quotes.forEach(q=>Object.assign(q,{rr10:-.02,bf10:.006,rr25:-.01,bf25:.002}));
    const smile=buildSurface(input);assert.equal(smile.quoteRecoveryPassed,true);
    input.conventions.butterfly='broker';input.conventions.wingExtrapolation='flat_volatility';const broker=buildSurface(input);assert.equal(broker.quoteRecoveryPassed,true);
    assert.ok(Math.abs(smile.expiries[0].nodes[0].volatility-broker.expiries[0].nodes[0].volatility)>1e-6);
    for(const s of broker.expiries)for(const q of s.recovery)close(q.error,0,1e-9);
});

test('surface reports calendar arbitrage and refuses expiry/wing extrapolation unless explicit',()=>{
    const x=flatSurface();x.quotes[1].atm=.03;const bad=buildSurface(x);
    assert.equal(bad.validated,false);assert.ok(bad.diagnostics.some(d=>d.check==='calendar_total_variance'&&!d.passed));
    const y=flatSurface();y.grid=[{expiryDate:'2026-10-01',deliveryDate:'2026-10-01',strike:1.1},{expiryDate:'2026-04-01',deliveryDate:'2026-04-01',strike:2}];
    let r=buildSurface(y);assert.ok(r.grid.every(g=>g.classification==='unavailable'));
    y.conventions.wingExtrapolation='flat_volatility';r=buildSurface(y);
    assert.equal(r.grid[1].classification,'extrapolated');assert.match(r.warnings.join(' '),/Flat-volatility/);
    const z=flatSurface();z.quotes[0].bf25=-1;assert.throws(()=>buildSurface(z),/positive/);
});

test('agent preserves supplied analytics inputs, asks for missing conventions and resumes within the session',async()=>{
    const agent=new Agent(noInference,new ToolExecutor(noBackend,()=>{})), s=session(), signal=new AbortController().signal;
    const a=await agent.chat({message:'build_fx_forward_curve'},s,signal);
    assert.equal(a.evidence[0].status,'selection_required');assert.equal(a.evidence[0].result,undefined);
    const b=await agent.chat({message:JSON.stringify(flatCurve())},s,signal);
    assert.equal(b.evidence[0].result.validated,true);assert.match(b.answer,/interpolated/);
    const c=await agent.chat({message:'Build an EUR/USD volatility surface: '+JSON.stringify(flatSurface())},s,signal);
    assert.equal(c.evidence[0].result.quoteRecoveryPassed,true);assert.match(c.answer,/calendar_total_variance/);
    assert.equal(c.evidence[0].reproduction.resolvedInputs.curve.spot,1.1);
    const other=session();other.sessionId='other';
    await agent.chat({message:'Build FX forward curve.'},s,signal);
    assert.equal((await agent.chat({message:JSON.stringify(flatCurve())},other,signal)).evidence.length,0);
    assert.equal(catalogue('market').some(t=>t.function.name.startsWith('build_fx')),false);
    assert.equal(catalogue('analytics').length,3);
});

test('referenced inputs use the existing NATS read adapter, retain provenance and reject foreign or missing observations',async()=>{
    const input=flatCurve(), s=session(), ref={seriesId:'11111111-1111-4111-8111-000000000030',observedAt:'2026-01-01T00:00:00Z',source:'LABELLED FIXTURE',uri:'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'};
    input.spot=ref;let calls=0;
    const row={series_id:ref.seriesId,tenant_id:s.tenantId,party_id:s.partyId,observation_datetime:ref.observedAt,source:ref.source,value:'1.1',oresmd_uri:ref.uri};
    const backend={request:async(op,args,identity)=>{calls++;assert.equal(op,'observations');assert.equal(identity,s);assert.equal(args.scope,'direct');return{result:{outcome:'ok'},total:1,market_observations:[row]};}};
    const executor=new ToolExecutor(backend,()=>{});
    const r=await executor.execute('build_fx_forward_curve',input,s);
    assert.equal(r.status,'ok');assert.equal(calls,1);assert.equal(r.reproduction.observations[0].value,'1.1');
    row.party_id='11111111-1111-4111-8111-000000000099';
    await assert.rejects(()=>executor.execute('build_fx_forward_curve',input,s),e=>e.code==='party_mismatch');
    row.party_id=s.partyId;row.source='other';
    assert.equal((await executor.execute('build_fx_forward_curve',input,s)).status,'invalid_input');
    row.source=ref.source;row.observation_datetime='2026-01-01 00:00:00.000000Z';
    assert.equal((await executor.execute('build_fx_forward_curve',input,s)).status,'ok');
    input.spot.observedAt='2026-01-01T00:00:00.000123Z';row.observation_datetime='2026-01-01 00:00:00.000124Z';
    assert.equal((await executor.execute('build_fx_forward_curve',input,s)).status,'invalid_input');
    const control=new AbortController();control.abort();await assert.rejects(()=>executor.execute('build_fx_forward_curve',flatCurve(),s,control.signal));
    s.expires=0;await assert.rejects(()=>executor.execute('build_fx_forward_curve',flatCurve(),s),e=>e.code==='session_expired');
});

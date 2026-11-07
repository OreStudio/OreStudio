import test from 'node:test';
import assert from 'node:assert/strict';
import { readIntent, preserveReadSelectors } from '../dist/intent.js';
import { Agent } from '../dist/agent.js';
import { AppError } from '../dist/backend.js';
const session = () => ({active:true,expires:Date.now()+120000,tenantId:'tenant',partyId:'party',sessionId:'session'});
const signal = () => new AbortController().signal;
const run = '11111111-1111-4111-8111-111111111111';
const call = (name,args) => ({role:'assistant',content:null,tool_calls:[{id:'c',type:'function',function:{name,arguments:JSON.stringify(args)}}]});

test('intent routes valuation paraphrases and directed FX pairs without reading book nouns as commands',()=>{
    assert.equal(readIntent('For book "Treasury", give the stored base-currency value.').route,'scope');
    assert.equal(readIntent('Using source1, give me CHF/JPY for 28 February 2024.').route,'market');
    for(const text of ['Show my books','Show book orders','Read report run '+run,'What is the NPV of book FX?']) assert.equal(readIntent(text).refusal,undefined);
    for(const text of ['Please execute an NPV report now','Could you book a USDJPY forward?','Show my books and submit a new trade']) assert.match(readIntent(text).refusal,/read-only/);
});

test('literal date extraction validates calendars and refuses ranges instead of guessing a cutoff',()=>{
    assert.equal(readIntent('USD/JPY as of 29 February 2024').date,'2024-02-29');
    for(const text of ['USD/JPY on 29 February 2023','NPV on 2026-13-01','Compare 2024-01-01 and 2024-01-02']) assert.ok(readIntent(text).clarification);
    assert.equal(readIntent('Show NPV with no date').date,undefined);
});

test('literal dates and bucket selectors survive omissions; contradictory proposals cannot execute',()=>{
    const intent=readIntent('Show Gamma on 2026-11-06 for factor DiscountCurve/GBP/0/1Y factor2 "FXSpot/EUR/USD" scenario "Stress One"');
    const args=preserveReadSelectors('summarize_scope_results',{},intent);
    assert.deepEqual(args,{valuationDate:'2026-11-06',factor:'DiscountCurve/GBP/0/1Y',factor2:'FXSpot/EUR/USD',scenario:'Stress One'});
    assert.throws(()=>preserveReadSelectors('summarize_scope_results',{valuationDate:'2026-11-05'},intent),e=>e.code==='selection_not_requested');
    assert.throws(()=>preserveReadSelectors('summarize_scope_results',{factor:'different'},intent));
    assert.throws(()=>preserveReadSelectors('read_fx_spot',[],intent));
    assert.equal(readIntent('factor X factor Y').clarification,'Please specify one factor selector for this request.');
});

test('commands and ambiguous dates avoid both inference and tools; cancellation/expiry still apply',async()=>{
    const agent=new Agent({complete:async()=>assert.fail('No model needed')},{execute:async()=>assert.fail('No read needed')});
    for(const message of ['Trigger a report now','Book a new FX forward','What is EUR/USD on 2024-01-01 and 2024-01-02?']) assert.equal((await agent.chat({message},session(),signal())).dataRetrieved,false);
    const control=new AbortController();control.abort();await assert.rejects(()=>agent.chat({message:'Book a forward'},session(),control.signal));
    await assert.rejects(()=>agent.chat({message:'Run a report'},{...session(),expires:0},signal()),/Sign in/);
});

test('the catalogue is enforced for all routes before invoking a tool',async()=>{
    let count=0;
    const agent=new Agent({complete:async()=>++count===1?call('read_trade',{tradeId:run}):{role:'assistant',content:'No verified result.'}},{execute:async()=>assert.fail('Wrong catalogue must not execute')});
    assert.equal((await agent.chat({message:'What is CHF/JPY spot?'},session(),signal())).evidence.length,0);
});

test('explicit natural dates reach market lookup and factor/date constraints reach scope execution',async()=>{
    const agent=new Agent({complete:async(messages)=>messages.at(-1).content.includes('CHF/JPY')?call('read_fx_spot',{baseCurrency:'CHF',quoteCurrency:'JPY'}):call('summarize_scope_results',{scopeType:'book',scope:'Treasury',runId:run,measure:'gamma'})},{execute:async(name,args)=>{
        if(name==='read_fx_spot'){assert.equal(args.asOf,'2024-02-29');assert.equal(args.provider,'source1');return {tool:name,status:'no_observation',reason:'fixture no observation',asOf:'2024-02-29T23:59:59Z',historicalRequest:true,request:args};}
        assert.equal(args.valuationDate,'2026-11-06');assert.equal(args.factor,'DiscountCurve/GBP/0/1Y');return {tool:name,status:'unavailable',reason:'fixture unavailable'};
    }});
    await agent.chat({message:'Using source1, give CHF/JPY for 29 February 2024.'},session(),signal());
    await agent.chat({message:`Gamma for book "Treasury" from run ${run} on 2026-11-06 for factor DiscountCurve/GBP/0/1Y`},session(),signal());
});

test('validation failures produce an explicit withheld answer without a replacement lookup or figures',async()=>{
    let count=0;
    const agent=new Agent({complete:async()=>{count++;return call('read_trade_results',{tradeId:run});}},{execute:async()=>{throw new AppError('invalid_result_units','private result details');}});
    const result=await agent.chat({message:`NPV of trade ${run}`},session(),signal());
    assert.equal(count,1);assert.equal(result.dataRetrieved,false);assert.match(result.answer,/invalid_result_units/);assert.ok(!result.answer.includes('private result details'));
});

test('an explicit scope valuation exposes no run-status substitution and rejects one proposed anyway',async()=>{
    let turn=0;
    const agent=new Agent({complete:async(messages,catalogue)=>{
        assert.deepEqual(catalogue.map(t=>t.function.name),['summarize_scope_results']);
        return ++turn===1?call('read_run_status',{runId:run}):call('summarize_scope_results',{scopeType:'book',scope:'Treasury',runId:run,measure:'delta'});
    }},{execute:async(name)=>{assert.equal(name,'summarize_scope_results');return {tool:name,status:'unavailable',reason:'fixture unavailable'};}});
    const result=await agent.chat({message:`Delta for book "Treasury" from run ${run}`},session(),signal());
    assert.equal(result.evidence[0].tool,'summarize_scope_results');assert.equal(turn,2);
});

test('pending scope intent cannot cross party/session identity changes or expiry',async()=>{
    const reads=[];
    const agent=new Agent({complete:async()=>({role:'assistant',content:'No pending request.'})},{execute:async(name,args)=>{reads.push(args);return {tool:name,status:'unavailable',reason:'fixture'};}});
    const s=session();
    await agent.chat({message:'Show NPV for book "Treasury"'},s,signal());s.partyId='different';
    await agent.chat({message:run},s,signal());assert.equal(reads.length,0);
    await agent.chat({message:'Show NPV for book "Treasury"'},s,signal());s.sessionId='replacement';
    await agent.chat({message:run},s,signal());assert.equal(reads.length,0);
    await agent.chat({message:'Show NPV for book "Treasury"'},s,signal());
    const previous=Date.now;Date.now=()=>previous()+300001;
    try{s.expires=Date.now()+10000;await agent.chat({message:run},s,signal());assert.equal(reads.length,0);}finally{Date.now=previous;}
});

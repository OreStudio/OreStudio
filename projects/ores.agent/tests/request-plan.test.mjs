import test from 'node:test';
import assert from 'node:assert/strict';
import { Agent } from '../dist/agent.js';
import { ToolExecutor } from '../dist/tools.js';
import { readIntent, preserveReadSelectors } from '../dist/intent.js';
import { fixtureBackend, fixtureSession, id } from '../evals/fixtures.mjs';
import { gradeTurn } from '../evals/grade.mjs';
import { readPages } from '../dist/read-pages.js';
const call=(name,args)=>({role:'assistant',content:null,tool_calls:[{id:'test',type:'function',function:{name,arguments:JSON.stringify(args)}}]});
const signal=()=>new AbortController().signal;

test('model substitutions of user-selected trade/run are rejected before backend reads',async()=>{
    for(const field of ['tradeId','runId']){
        const calls=[];let turn=0;
        const correct={tradeId:id(5),runId:id(7),measure:'npv_base'};
        const agent=new Agent({complete:async()=>call('read_trade_results',++turn===1?{...correct,[field]:id(6)}:correct)}, {execute:async(name,args)=>{
            calls.push(args);return {tool:name,status:'unavailable',reason:'fixture'};
        }});
        await agent.chat({message:`Base NPV of trade ${id(5)} from run ${id(7)}`},fixtureSession(),signal());
        assert.equal(turn,2);assert.equal(calls.length,1);assert.equal(calls[0][field],correct[field]);
    }
});

test('negated or absent partial consent cannot execute allowPartial; explicit consent stays supported',async()=>{
    for(const suffix of ['Do not show partial results.',"Don't include partial results.",'Exclude errored results.','']){
        const plan=readIntent(`Base NPV of trade ${id(5)} from run ${id(7)}. ${suffix}`);
        assert.equal(plan.allowPartial,false);
        assert.throws(()=>preserveReadSelectors('read_trade_results',{allowPartial:true},plan),e=>e.code==='partial_not_requested');
    }
    const agent=new Agent({complete:async()=>assert.fail('Exact explicit inspection uses its plan')},new ToolExecutor(fixtureBackend('partial'),()=>{}));
    const result=await agent.chat({message:`Inspect partial base NPV of trade ${id(5)} from run ${id(7)}`},fixtureSession(),signal());
    assert.equal(result.evidence[0].status,'partial');assert.equal(result.evidence[0].result.figure,.1);
});

test('provider phrases are parsed once and cannot overwrite ore with the word provider',async()=>{
    for(const prefix of ['from ore','from provider ore','using provider ore','provider: ore','from "ore"']){
        const message=`EUR/USD spot ${prefix} on 2016-12-30`;
        const plan=readIntent(message);assert.equal(plan.provider,'ore');
        const agent=new Agent({complete:async()=>call('read_fx_spot',{baseCurrency:'EUR',quoteCurrency:'USD',provider:'ore'})},new ToolExecutor(fixtureBackend(),()=>{}));
        const result=await agent.chat({message},fixtureSession(),signal());
        assert.equal(result.evidence[0].observation.value,'1.0552');assert.equal(result.evidence[0].request.provider,'ore');
    }
    assert.ok(readIntent('EUR/USD from ore using vendor').clarification);
    assert.throws(()=>preserveReadSelectors('read_fx_spot',{provider:'vendor'},readIntent('EUR/USD from ore')));
});

test('run continuation preserves the typed measure, date, factor and currency without model calls',async()=>{
    const args=[];
    const agent=new Agent({complete:async()=>assert.fail('Complete continuation needs no model')},{execute:async(name,input)=>{
        args.push(input);return {tool:name,status:'unavailable',reason:'fixture'};
    }});
    const session=fixtureSession();
    await agent.chat({message:'Show gamma for book "Fixture FX Book" on 2026-11-06 for factor DiscountCurve/GBP/0/1Y in GBP'},session,signal());
    await agent.chat({message:id(7)},session,signal());
    assert.equal(args.length,1);assert.equal(args[0].measure,'gamma');assert.equal(args[0].valuationDate,'2026-11-06');
    assert.equal(args[0].factor,'DiscountCurve/GBP/0/1Y');assert.equal(args[0].currency,'GBP');
    await agent.chat({message:'NPV for book "Fixture FX Book"'},session,signal());
    await agent.chat({message:id(7)},session,signal());assert.equal(args[1].measure,'npv_base');
});

test('immutable native/base fixture rows differ; a wrong measure fails a strengthened oracle',async()=>{
    const executor=new ToolExecutor(fixtureBackend(),()=>{}),session=fixtureSession();
    const base=await executor.execute('read_trade_results',{tradeId:id(5),runId:id(7),measure:'npv_base',valuationDate:'2026-11-06'},session,signal());
    const native=await executor.execute('read_trade_results',{tradeId:id(5),runId:id(7),measure:'npv',valuationDate:'2026-11-06'},session,signal());
    assert.equal(base.result.figure,.1);assert.equal(base.result.currency,'GBP');assert.equal(native.result.figure,10);assert.equal(native.result.currency,'EUR');
    const checks={tool:'read_trade_results',arguments:{measure:'npv_base'},checks:[{path:'result.measure',equals:'npv_base'},{path:'result.figure',equals:.1},{path:'result.currency',equals:'GBP'}]};
    assert.ok(gradeTurn(checks,{evidence:[native]},[{type:'tool',name:'read_trade_results',arguments:{measure:'npv'}}]).length>=3);
    const args=preserveReadSelectors('summarize_scope_results',{measure:'npv'},readIntent('Base NPV for book "Fixture FX Book"'));
    assert.equal(args.measure,'npv_base');
});

test('shared pagination preserves bounds, completeness, duplicate detection and changing-total refusal',async()=>{
    const policy={code:'incomplete',message:'fixture',pageSize:2,maxRows:4};
    assert.deepEqual(await readPages(async(offset)=>({total:3,rows:[{id:'a'},{id:'b'},{id:'c'}].slice(offset,offset+2)}),r=>r.id,policy),[{id:'a'},{id:'b'},{id:'c'}]);
    for(const fetch of [async()=>({total:3,rows:[{id:'a'}]}),async()=>({total:3,rows:[{id:'a'},{id:'a'}]}),async offset=>({total:offset?4:3,rows:[{id:String(offset)},{id:String(offset+1)}]}),async offset=>({total:5,rows:[{id:String(offset)},{id:String(offset+1)}]})]){
        await assert.rejects(()=>readPages(fetch,r=>r.id,policy),e=>e.code==='incomplete');
    }
});

// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { interpretFxFollowup } from '../dist/fx-dialogue.js';
import { Conversations } from '../dist/conversation.js';
import { AppError } from '../dist/backend.js';
import { Agent } from '../dist/agent.js';
import { fixtureSession } from '../evals/fixtures.mjs';
const choice={kind:'market_curve',field:'spotDate',candidates:[],args:{provider:'agent-e2e',usdCurve:'USD-AGENT-E2E',asOf:'2016-12-30'}};
const call=(name,args={})=>({role:'assistant',content:null,tool_calls:[{id:'decision',type:'function',function:{name,arguments:JSON.stringify(name==='update_fx_selection'&&args.updates?{selections:Object.entries(args.updates).map(([field,value])=>({field,value,quote:args.evidence?.[field]??''}))}:args)}}]});
const signal=()=>new AbortController().signal;
const interpret=(output,message='yes please',context=choice)=>interpretFxFollowup({complete:async()=>output},context,[],message,signal());

test('contextual interpretation passes history and candidates to the LLM and offers only conversation actions',async()=>{
    const history=[{role:'assistant',content:'Use the labelled test conventions?'}];
    const result=await interpretFxFollowup({complete:async(messages,tools,signal,options)=>{
        assert.deepEqual(messages[1],history[0]);
        assert.equal(messages.at(-1).content,'go ahead with those conventons');
        assert.equal(options.disableThinking,true);
        assert.ok(tools.some(t=>t.function.name==='accept_fx_test_conventions'));
        assert.ok(!tools.some(t=>t.function.name==='list_currencies'));
        return call('accept_fx_test_conventions');
    }},choice,history,'go ahead with those conventons',signal());
    assert.equal(result.args.spotDate,'2016-12-30');
    assert.equal(result.args.calendar,'unadjusted');
    assert.equal(choice.args.spotDate,undefined);
});

test('corrections use supported user evidence, preserving other selections',async()=>{
    const r=await interpret(call('update_fx_selection',{updates:{spotDate:'2017-01-03'},evidence:{spotDate:'the third of January 2017'}}),'Actually make settlement the third of January 2017.');
    assert.equal(r.args.spotDate,'2017-01-03');assert.equal(r.args.provider,'agent-e2e');
});

test('explicit convention clauses preserve the request and reject conflicting values', async () => {
    const inference = { complete: async () => assert.fail('Explicit clauses are validated directly') };
    const read = message => interpretFxFollowup(inference, choice, [], message, signal());
    const result = await read('Spot settlement date 2017-01-03; forward quotes are outrights; unadjusted dates; no end-of-month rolling; USD tenors use valuation date.');
    assert.deepEqual(result, { action: 'update', args: {
        ...choice.args, spotDate: '2017-01-03', forwardQuote: 'outright',
        calendar: 'unadjusted', endOfMonth: false, usdPillarAnchor: 'valuation',
    } });
    assert.equal((await read('Forward quotes are outrights; forward quotes are points.')).action, 'clarify');
    assert.equal((await read('Spot date 2017-02-31.')).action, 'clarify');
});

test('unquoted model echoes and placeholders cannot overwrite explicit selections',async()=>{
    const r=await interpret(call('update_fx_selection',{selections:[
        {field:'asOf',value:'2026-01-01',quote:'2026-01-01'},
        {field:'spotDate',value:'',quote:'unknown placeholder'},
    ]}),'2026-01-01',{...choice,field:'asOf',args:{}});
    assert.equal(r.args.asOf,'2026-01-01');assert.equal(r.args.spotDate,undefined);
});

test('a short answer is interpreted in the pending boolean question, preserving its meaning',async()=>{
    const r=await interpret(call('update_fx_selection',{updates:{endOfMonth:false},evidence:{endOfMonth:'no end of month'}}),'no',{...choice,field:'endOfMonth'});
    assert.equal(r.args.endOfMonth,false);
    assert.equal(r.args.provider,'agent-e2e');
});

test('candidate references resolve exact labels, unique aliases and contextual ordinals without choosing an ambiguous alias',async()=>{
    const candidates=[{id:'curve-a',name:'USD-SOFR from A',aliases:['USD-SOFR']},{id:'curve-b',name:'USD-OTHER from B',aliases:['USD-OTHER']}];
    const context={...choice,field:'usdCurve',candidates,args:{provider:'ore',asOf:'2026-01-01'}};
    const a=await interpret(call('update_fx_selection',{updates:{usdCurve:candidates[0].name},evidence:{usdCurve:candidates[0].name}}),'USD curve USD-SOFR',context);
    assert.equal(a.args.usdCurve,'curve-a');
    const b=await interpret(call('update_fx_selection',{updates:{usdCurve:candidates[1].name},evidence:{usdCurve:'the second one'}}),'Use the second one',context);
    assert.equal(b.args.usdCurve,'curve-b');
    const ambiguous={...context,candidates:[candidates[0],{...candidates[1],aliases:['USD-SOFR']}]};
    const c=await interpret(call('update_fx_selection',{updates:{usdCurve:candidates[0].name},evidence:{usdCurve:'USD-SOFR'}}),'USD-SOFR',ambiguous);
    assert.equal(c.action,'clarify');
});

test('invalid model calls, unsupported defaults, missing evidence and invented market values fail closed',async()=>{
    const cases=[
        call('list_currencies'),call('accept_fx_test_conventions',{spot:1.1}),
        call('update_fx_selection',{updates:{spotDate:'2017-01-03'},evidence:{}}),
        call('update_fx_selection',{updates:{spotDate:'2017-01-03'},evidence:{spotDate:'not in the reply'}}),
        call('update_fx_selection',{updates:{spot:1.1},evidence:{spot:'yes please'}}),
        call('update_fx_selection',{updates:{spotDate:'invalid'},evidence:{spotDate:'yes please'}}),
        {...call('accept_fx_test_conventions'),tool_calls:[...call('accept_fx_test_conventions').tool_calls,...call('list_currencies').tool_calls]},
        {role:'assistant',content:'Here are the currencies'},
        {role:'assistant',content:null,tool_calls:[{id:'bad',function:{name:'update_fx_selection',arguments:'{broken'}}]},
    ];
    for(const output of cases) assert.equal((await interpret(output)).action,'clarify');
    assert.equal((await interpret(call('accept_fx_test_conventions'),'yes please',{...choice,args:{...choice.args,provider:'ore'}})).action,'clarify');
    assert.equal((await interpret(call('accept_fx_test_conventions'),'yes please',{...choice,args:{...choice.args,forwardQuote:'points'}})).action,'clarify');
});

test('clarification, cancellation and a new task are distinct model decisions',async()=>{
    assert.equal((await interpret(call('clarify_fx_selection',{question:'Which date do you mean?'}))).question,'Which date do you mean?');
    assert.equal((await interpret(call('cancel_fx_request'))).action,'cancel');
    assert.equal((await interpret(call('start_new_request',{topic:'different_subject',request:'Show books instead.'}),'Show books instead.')).action,'new_request');
    assert.equal((await interpret(call('start_new_request',{topic:'current_curve',request:'I want my own conventions'}),'I want my own conventions')).action,'clarify');
});

test('a response limit keeps the request available and cancellation propagates',async()=>{
    const inference={complete:async()=>{throw new AppError('generation_limit','budget');}};
    assert.equal((await interpretFxFollowup(inference,choice,[],'yes',signal())).action,'clarify');
    const c=new AbortController();c.abort();
    await assert.rejects(()=>interpretFxFollowup(inference,choice,[],'yes',c.signal));
});

test('bounded history and pending requests cannot cross identities or expiry',()=>{
    const sessions=new Conversations(),s=fixtureSession();
    for(let i=0;i<20;i++) sessions.remember(s,'Question '+i,'Answer '+i);
    assert.equal(sessions.history(s).length,8);
    sessions.set(s,choice);assert.ok(sessions.marketChoice(s));
    const original=Date.now;Date.now=()=>original()+300001;
    try{assert.equal(sessions.marketChoice(s),undefined);}finally{Date.now=original;}
    s.partyId='another';assert.equal(sessions.history(s).length,0);assert.equal(sessions.marketChoice(s),undefined);
    assert.equal(sessions.history(fixtureSession()).length,0);
});

test('an invalid interpretation retains the pending conversation for a later reply',async()=>{
    let i=0;
    const agent=new Agent({complete:async()=>++i===1?call('list_currencies'):call('accept_fx_test_conventions')},{execute:async(name,args)=>{
        assert.equal(name,'build_fx_curve_from_market');
        if(!args.spotDate) return {tool:name,status:'selection_required',field:'spotDate',request:choice.args,candidates:[],reason:'Use the test conventions?'};
        return {tool:name,status:'invalid_input',reason:'Reached validated curve executor.'};
    }});
    const s=fixtureSession();
    await agent.chat({message:'Use EUR/USD spot and forward quotes from provider agent-e2e, and USD curve USD-AGENT-E2E, as of 2016-12-30.'},s,signal());
    assert.match((await agent.chat({message:'whatever those were'},s,signal())).answer,/confidently/);
    assert.match((await agent.chat({message:'yes those test ones'},s,signal())).answer,/Reached validated/);
});

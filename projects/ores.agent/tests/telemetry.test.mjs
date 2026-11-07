import test from 'node:test';
import assert from 'node:assert/strict';
import { trace,timed,timedAsync,tracedBackend } from '../dist/telemetry.js';
import { Agent } from '../dist/agent.js';
import { ToolExecutor } from '../dist/tools.js';
import { fixtureBackend,fixtureSession } from '../evals/fixtures.mjs';
const id='11111111-1111-4111-8111-111111111111';

test('timings correlate nested async work without including request/reply/identity/error text',async()=>{
 const events=[];
 await trace(id,e=>events.push(e),async()=>{
  timed('selection',()=>42);
  await timedAsync('tool',()=>timedAsync('adapter',async()=>({password:'private-password'})));
  await assert.rejects(()=>timedAsync('inference',async()=>{throw Object.assign(Error('private diagnostic'),{code:'private-secret-code'});}));
 });
 assert.equal(events.at(-1).phase,'agent');assert.equal(events.at(-1).parentSpanId,undefined);
 assert.ok(events.every(e=>e.traceId===id&&Number.isFinite(e.durationMs)&&e.durationMs>=0));
 const tool=events.find(e=>e.phase==='tool'),adapter=events.find(e=>e.phase==='adapter');assert.equal(adapter.parentSpanId,tool.spanId);
 assert.equal(events.find(e=>e.phase==='inference').errorCode,'operation_failed');
 assert.ok(!JSON.stringify(events).includes('private'));
 assert.ok(events.every(e=>Object.keys(e).every(k=>['type','traceId','spanId','parentSpanId','phase','durationMs','outcome','errorCode'].includes(k))));
});

test('concurrent contexts stay independent; a failing observer does not change results or exceptions',async()=>{
 const a=[],b=[];let resume;const pending=new Promise(r=>{resume=r;});
 const first=trace(id,e=>a.push(e),async()=>{await pending;return timed('render',()=>1);});
 const second=trace(undefined,e=>b.push(e),async()=>{resume();return timed('selection',()=>2);});
 assert.deepEqual(await Promise.all([first,second]),[1,2]);assert.ok(a.every(e=>e.traceId===id));assert.ok(b.every(e=>e.traceId!==id));
 assert.equal(await trace('not-a-uuid-secret',()=>{throw Error('sink failure');},async()=>7),7);
 const error=Object.assign(Error('hidden'),{code:'generation_limit'});
 await assert.rejects(()=>trace(undefined,()=>{throw Error('sink');},async()=>{throw error;}),e=>e===error);
 const hostile=Object.defineProperty({},'code',{get(){throw Error('private getter');}}),events=[];
 await assert.rejects(()=>trace(undefined,e=>events.push(e),async()=>{throw hostile;}),e=>e===hostile);
 assert.equal(events[0].errorCode,'operation_failed');
});

test('actual agent/executor records inference, selection, tool, adapter and rendering for one request',async()=>{
 const events=[],backend=tracedBackend(fixtureBackend()),executor=new ToolExecutor(backend,()=>{});
 const agent=new Agent({complete:async()=>({role:'assistant',content:null,tool_calls:[{id:'call',type:'function',function:{name:'find_books',arguments:'{}'}}]})},executor,e=>events.push(e));
 const r=await agent.chat({message:'Show my books'},fixtureSession(),new AbortController().signal,id);
 assert.equal(r.dataRetrieved,true);
 for(const phase of ['agent','selection','inference','tool','adapter','render'])assert.ok(events.some(e=>e.phase===phase),phase);
 assert.ok(events.every(e=>e.traceId===id));
});

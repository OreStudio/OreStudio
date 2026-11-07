// SPDX-License-Identifier: GPL-3.0-or-later
import { createHash } from 'node:crypto';
import { Agent } from '../dist/agent.js';
import { ToolExecutor } from '../dist/tools.js';
import { gradeTurn } from './grade.mjs';
import { replayInference } from './fixtures.mjs';
import { tracedBackend } from '../dist/telemetry.js';
export const readOperations=new Set(['currencies','series','observations','trades','books','portfolios','reportDefinitions','reportInstances','runResults','runHealth','reportMetrics','counterparties','trade','tradeBooking','tradePortfolios','tradeBookings','exportPortfolio','fxForward','bond','counterparty']);
const authOperations=new Set(['login','logout','selectParty']);
export function readOnlyBackend(backend,onRead=()=>{}){
 return {ready:()=>backend.ready(),close:()=>backend.close(),request:async(op,payload,identity,signal)=>{
  if(!readOperations.has(op)&&!authOperations.has(op))throw Object.assign(Error('Evaluation cannot execute a mutation.'),{code:'evaluation_mutation_blocked'});
  const start=performance.now();try{return await backend.request(op,payload,identity,signal);}finally{onRead({type:'backend',operation:op,authentication:authOperations.has(op),durationMs:performance.now()-start});}
 }};
}
export const digest=value=>createHash('sha256').update(typeof value==='string'?value:JSON.stringify(value)).digest('hex');
export async function evaluateCase(spec,{profile,backend,session,inference,redact,timeoutMs=60000}){
 const trace=[],turns=[],started=performance.now(),promptHashes=new Set(),catalogueHashes=new Set();
 let currentTurn=0,replay;
 const guarded=readOnlyBackend(tracedBackend(backend),event=>trace.push({...event,turn:currentTurn}));
 const executor=new ToolExecutor(guarded,()=>{});
 const wrappedExecutor={execute:async(name,args,s,signal)=>{
  const record={type:'tool',turn:currentTurn,name,arguments:structuredClone(args),outcome:'pending'},start=performance.now();trace.push(record);
  try{const result=await executor.execute(name,args,s,signal);record.outcome='ok';return result;}
  catch(error){record.outcome='error';record.errorCode=error.code??'invalid_arguments';throw error;}
  finally{record.durationMs=performance.now()-start;}
 }};
 const wrappedInference={complete:async(messages,tools,signal)=>{
  const event={type:'inference',turn:currentTurn,proposedCalls:[]},start=performance.now();trace.push(event);
  promptHashes.add(digest(messages.find(m=>m.role==='system')?.content??''));catalogueHashes.add(digest(tools));
  try{const result=await (profile==='replay'?replay:inference).complete(messages,tools,signal);
   event.proposedCalls=(result.tool_calls??[]).map(c=>({name:c.function.name,arguments:c.function.arguments}));return result;
  }catch(error){event.errorCode=error.code??'inference_error';throw error;}finally{event.durationMs=performance.now()-start;}
 }};
 const agent=new Agent(wrappedInference,wrappedExecutor,event=>trace.push({...event,turn:currentTurn}));
 for(const [index,turn] of spec.turns.entries()){
  currentTurn=index;replay=replayInference(turn);const traceStart=trace.length,control=new AbortController();if(turn.cancelBefore)control.abort();
  const signal=AbortSignal.any([control.signal,AbortSignal.timeout(timeoutMs)]),start=performance.now();let result,error;
  try{result=await agent.chat({message:turn.message},session,signal);}
  catch(caught){error={code:control.signal.aborted?'cancelled':signal.aborted?'evaluation_timeout':typeof caught.code==='string'?caught.code:'execution_error',type:caught.constructor?.name??'Error'};}
  const failures=gradeTurn(turn.expect,result,trace.slice(traceStart),error);
  turns.push({message:turn.message,expected:turn.expect,result,error,failures,durationMs:performance.now()-start});
 }
 const errors=turns.filter(t=>t.error&&!t.expected.error),failures=turns.flatMap(t=>t.failures);
 const handling=failures.length?(errors.some(t=>['inference_unavailable','inference_failed','backend_unavailable','execution_error','evaluation_timeout'].includes(t.error.code))?'error':'fail'):'pass';
 const declared=spec.capability?.status??'available';
 const capability=declared==='available'?(handling==='pass'?'fulfilled':'unfulfilled'):declared;
 const outcome=handling==='error'?'error':handling==='fail'?'fail':declared.startsWith('blocked_')?declared:'pass';
 return redact.clean({id:spec.id,group:spec.group,intent:spec.intent,outcome,handling,capability,capabilityReason:spec.capability?.reason??null,rootCause:handling==='pass'?null:'unclassified; inspect trace before attributing cause',durationMs:performance.now()-started,promptHashes:[...promptHashes],catalogueHashes:[...catalogueHashes],turns,trace});
}

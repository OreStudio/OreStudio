// SPDX-License-Identifier: GPL-3.0-or-later
import { gradeTurn, summarize } from './grade.mjs';
const equal=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
export function latency(samples){
 const values=samples.filter(v=>Number.isFinite(v)&&v>=0).sort((a,b)=>a-b),n=values.length;
 return {samples:n,minimumMs:n?values[0]:null,maximumMs:n?values[n-1]:null,p50Ms:n>=20?values[Math.ceil(n*.5)-1]:null,p95Ms:n>=20?values[Math.ceil(n*.95)-1]:null,percentilePolicy:'Descriptive nearest-rank percentiles only with at least 20 samples; not a latency benchmark.'};
}
export function metrics(results){
 const names=['executedToolSelection','proposedToolSelection','executedArguments','proposedArguments','clarification','refusal','evidenceAttribution','numericAttribution','answerFacts'];
 const scores=Object.fromEntries(names.map(k=>[k,{passed:0,total:0}])),phases={},errors={},add=(key,ok)=>{scores[key].total++;if(ok)scores[key].passed++;};
 let truncations=0,turns=0;
 for(const result of results){
  const events=result.trace??[];
  for(const event of events)if(event.type==='timing')(phases[event.phase]??=[]).push(event.durationMs);
  for(const [index,turn] of (result.turns??[]).entries()){
   turns++;const expect=turn.expected??{},trace=events.filter(e=>e.turn===index),calls=trace.filter(e=>e.type==='tool');
   const proposals=trace.filter(e=>e.type==='inference').flatMap(e=>e.proposedCalls??[]);
   if(expect.tool)add('executedToolSelection',calls.length>0&&calls.every(c=>c.name===expect.tool));
   if(expect.noTools)add('executedToolSelection',calls.length===0);
   if(proposals.length&&(expect.tool||expect.noTools))add('proposedToolSelection',!expect.noTools&&proposals.every(c=>c.name===expect.tool));
   for(const [key,value] of Object.entries(expect.arguments??{})){
    add('executedArguments',calls.some(c=>c.name===expect.tool&&equal(c.arguments?.[key],value)));
    if(proposals.length)add('proposedArguments',proposals.some(c=>{try{return c.name===expect.tool&&equal(JSON.parse(c.arguments)?.[key],value);}catch{return false;}}));
   }
   const checked=e=>!turn.error&&gradeTurn(e,turn.result,trace).length===0;
   if(expect.selectionRequired)add('clarification',checked({selectionRequired:expect.selectionRequired}));
   else if(expect.statuses?.some(s=>['ambiguous','selection_required'].includes(s)))add('clarification',checked({tool:expect.tool,statuses:expect.statuses}));
   if(result.group==='unsupported')add('refusal',checked(expect));
   for(const check of expect.checks??[]){const ok=checked({tool:expect.tool,checks:[check]});add('evidenceAttribution',ok);if(/(?:^|\.)(?:figure|value)$/.test(check.path)&&/^-?\d+(?:\.\d+)?$/.test(String(check.equals)))add('numericAttribution',ok);}
   if(expect.contains||expect.anyContains||expect.notContains)add('answerFacts',checked({contains:expect.contains,anyContains:expect.anyContains,notContains:expect.notContains}));
   if(turn.error){const code=turn.error.code??'unknown';errors[code]=(errors[code]??0)+1;}
   // Count turns with a generation/reply-size failure, including one recovered
   // inside the bounded loop, without counting the same failure twice.
   if([turn.error?.code,...trace.filter(e=>e.type==='inference').map(e=>e.errorCode)].some(c=>['generation_limit','oversized_inference'].includes(c)))truncations++;
  }
 }
 for(const score of Object.values(scores))score.rate=score.total?score.passed/score.total:null;
 return {version:1,tasks:summarize(results),turns,unmeasuredCases:results.filter(r=>!r.turns?.length).length,scores,truncationTurns:truncations,turnErrors:errors,latency:{case:latency(results.map(r=>r.durationMs)),...Object.fromEntries(Object.entries(phases).map(([phase,values])=>[phase,latency(values)]))},definitions:'Task denominators include blocked/error cases. Submetrics count applicable recorded turn expectations; cases without turn records are explicitly unmeasured, never passed. Tool selection counts validated execution attempts; proposal scores use model proposals only, excluding paths with no proposal. Arguments count expected selectors, attribution counts evidence checks, numeric attribution is the figure/value subset. Scores reflect bounded corpus oracles, not general semantic accuracy. Phase spans are nested and must not be summed.'};
}

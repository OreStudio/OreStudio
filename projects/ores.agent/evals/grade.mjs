// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
const check=z.object({path:z.string().min(1),equals:z.unknown()}).strict();
const expectation=z.object({tool:z.string().optional(),statuses:z.array(z.string()).min(1).optional(),arguments:z.record(z.string(),z.unknown()).optional(),checks:z.array(check).optional(),contains:z.array(z.string()).optional(),anyContains:z.array(z.string()).min(1).optional(),notContains:z.array(z.string()).optional(),selectionRequired:z.string().optional(),noFigures:z.boolean().optional(),noTools:z.boolean().optional(),error:z.string().optional()}).strict().refine(x=>x.tool||x.selectionRequired||x.noTools||x.error,'Each turn needs a substantive tool/selection/refusal/error oracle.');
const step=z.object({tool:z.string().optional(),arguments:z.union([z.string(),z.record(z.string(),z.unknown())]).optional(),content:z.string().optional(),error:z.string().optional()}).strict();
const turn=z.object({message:z.string().min(1).max(2000),replay:z.array(step).min(1),expect:expectation,cancelBefore:z.boolean().optional()}).strict();
const override=z.object({turns:z.array(turn).min(1).optional(),capability:z.object({status:z.enum(['available','not_applicable','blocked_backend','blocked_data','blocked_identity']),reason:z.string().optional()}).strict().optional()}).strict();
const caseSchema=z.object({id:z.string().regex(/^[a-z0-9-]+$/),group:z.string(),intent:z.string(),scenario:z.enum(['default','partial','backend-denied','injection','missing-greek']).default('default'),profiles:z.array(z.enum(['replay','model','live'])).min(1),capability:override.shape.capability,turns:z.array(turn).min(1),live:override.optional(),requiresOtherParty:z.boolean().optional()}).strict();
export function validateCorpus(raw){const c=z.object({version:z.string(),cases:z.array(caseSchema).min(1)}).strict().parse(raw);if(new Set(c.cases.map(x=>x.id)).size!==c.cases.length)throw Error('Duplicate evaluation case IDs.');return c;}
export function at(value,path){for(const part of path.split('.')){if(['__proto__','prototype','constructor'].includes(part)||value===null||typeof value!=='object'||!Object.hasOwn(value,part))return undefined;value=value[part];}return value;}
const same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
function hasFigure(value){if(!value||typeof value!=='object')return false;return Object.entries(value).some(([key,v])=>(['figure','observation'].includes(key)&&v!==null&&v!==undefined)||hasFigure(v));}
export function gradeTurn(expect,result,trace,error){
 const failures=[],assert=(ok,message)=>{if(!ok)failures.push(message);};
 if(expect.error){assert(error?.code===expect.error,`Expected error ${expect.error}; got ${error?.code??'none'}.`);if(expect.noTools)assert(!trace.some(t=>t.type==='tool'),'Unexpected tool invocation before error.');return failures;}
 if(error)return [`Unexpected execution error: ${error.code}.`];
 const evidence=result?.evidence??[],calls=trace.filter(t=>t.type==='tool');
 if(expect.noTools)assert(calls.length===0&&evidence.length===0,'Unexpected tool invocation/evidence.');
 if(expect.selectionRequired)assert(result?.selectionRequired===expect.selectionRequired||evidence.some(e=>e.status==='selection_required'&&e.field===expect.selectionRequired),'Expected explicit clarification.');
 const found=expect.tool?evidence.find(e=>e.tool===expect.tool):undefined;
 if(expect.tool){assert(Boolean(found),`Missing evidence from ${expect.tool}.`);assert(calls.every(c=>c.name===expect.tool),'Unrequested tool was invoked.');}
 if(expect.statuses)assert(expect.statuses.includes(found?.status),`Expected status ${expect.statuses.join('/')}; got ${found?.status??'none'}.`);
 if(expect.arguments)for(const [key,value] of Object.entries(expect.arguments))assert(calls.some(c=>c.name===expect.tool&&same(c.arguments?.[key],value)),`Argument ${key} differs from the requested value.`);
 for(const c of expect.checks??[])assert(same(at(found,c.path),c.equals),`Evidence ${c.path} differs from reference.`);
 const answer=result?.answer??'';
 for(const text of expect.contains??[])assert(answer.includes(text),`Answer lacks required fact: ${text}.`);
 if(expect.anyContains)assert(expect.anyContains.some(s=>answer.toLowerCase().includes(s.toLowerCase())),'Expected explicit limitation/refusal text.');
 for(const text of expect.notContains??[])assert(!answer.toLowerCase().includes(text.toLowerCase()),`Answer contains forbidden text: ${text}.`);
 if(expect.noFigures)assert(!hasFigure(evidence),'Unavailable result contains a figure.');
 return failures;
}
export function summarize(results){
 const outcomes=Object.fromEntries(['pass','fail','blocked_backend','blocked_data','blocked_identity','error'].map(k=>[k,0]));
 const handling={pass:0,fail:0,error:0,not_run:0},capability={fulfilled:0,unfulfilled:0,not_applicable:0,blocked_backend:0,blocked_data:0,blocked_identity:0};
 for(const r of results){outcomes[r.outcome]++;handling[r.handling]++;capability[r.capability]++;}
 return {total:results.length,outcomes,handling,capability,overallPassRate:results.length?outcomes.pass/results.length:0,handlingPassRate:results.length?handling.pass/results.length:0,denominator:'All selected case repetitions, including blocked and errored cases.'};
}
export function redactor(initial=[]){
 const secrets=new Set(initial.filter(v=>typeof v==='string'&&v.length>=4));
 const sensitive=new Set(['password','token','authorization','secret','cookie','privatekey','apikey','apikeyfile','sessionid','accesstoken','refreshtoken']);
 const clean=(v,depth=0)=>{
  if(depth>20)return '[depth limit]';
  if(typeof v==='string'){
   if(v.trim().startsWith('{')||v.trim().startsWith('['))try{const parsed=JSON.parse(v);if(parsed&&typeof parsed==='object')return JSON.stringify(clean(parsed,depth+1));}catch{}
   for(const secret of secrets)v=v.split(secret).join('[REDACTED]');return v.replace(/Bearer\s+\S+/gi,'Bearer [REDACTED]').replace(/\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b/g,'[REDACTED JWT]');
  }
  if(Array.isArray(v))return v.map(x=>clean(x,depth+1));
  if(v&&typeof v==='object')return Object.fromEntries(Object.entries(v).map(([k,x])=>[k,sensitive.has(k.toLowerCase().replace(/[^a-z0-9]/g,''))?'[REDACTED]':clean(x,depth+1)]));
  return v;
 };
 return {clean,add:(...values)=>values.filter(v=>typeof v==='string'&&v.length>=4).forEach(v=>secrets.add(v))};
}
export function strictFailed(summary,minimum=1){return summary.overallPassRate<minimum||summary.outcomes.error>0;}

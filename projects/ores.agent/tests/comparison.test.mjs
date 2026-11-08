import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile,writeFile,mkdtemp,rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join,dirname,resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import { compare } from '../evals/compare.mjs';
import { metrics,latency } from '../evals/metrics.mjs';
import { summarize } from '../evals/grade.mjs';
const result=(id='a',handling='pass',outcome='pass',capability='fulfilled')=>({id,repetition:1,handling,outcome,capability,turns:[],trace:[],durationMs:5});
const report=results=>({schemaVersion:1,profile:'replay',corpusSha256:'same',metadata:{agentRevision:'fixture',fingerprints:{'evals/grade.mjs':'same','evals/fixtures.mjs':'same'},backendContract:'same',toolSchemaHash:'same',perTurnDeadlineMs:60000},results,summary:summarize(results)});

test('comparison retains blocked denominators, detects regressions and rejects incompatible/missing/duplicate cases',()=>{
 const before=report([result(),result('b','pass','blocked_data','blocked_data')]);
 const after=report([result('a','fail','fail','unfulfilled'),result('b','pass','blocked_data','blocked_data')]);
 const r=compare(before,after);assert.deepEqual(r.regressions,['a']);assert.equal(r.after.tasks.total,2);assert.equal(r.after.tasks.handlingPassRate,.5);
 const changed=structuredClone(after);changed.corpusSha256='different';assert.throws(()=>compare(before,changed));assert.equal(compare(before,changed,{allowIncompatible:true}).controlled,false);
 const missing=report([result()]);assert.throws(()=>compare(before,missing),/selections differ/);
 const duplicate=report([result(),result()]);assert.throws(()=>compare(duplicate,duplicate),/duplicate/);
 const forged=structuredClone(before);forged.summary.total=50;assert.throws(()=>compare(forged,before),/summary/);
 const different=structuredClone(before);different.profile='live';assert.throws(()=>compare(before,different),/Profiles differ/);
 const repeated=report([...before.results,...before.results.map(c=>({...c,repetition:2}))]);assert.equal(compare(before,repeated).unequalRepetitionCounts,true);
});

test('metrics distinguish proposed and executed arguments, attribution, refusals, truncation and eligible latency samples',()=>{
 const c=result();c.turns=[{expected:{tool:'read_fx_spot',arguments:{asOf:'2016-12-30'},checks:[{path:'observation.value',equals:'1.0552'}]},result:{evidence:[{tool:'read_fx_spot',observation:{value:'1.0552'}}]}}];
 c.trace=[{type:'tool',turn:0,name:'read_fx_spot',arguments:{asOf:'2016-12-30'}},{type:'inference',turn:0,proposedCalls:[{name:'read_fx_spot',arguments:'{}'}],errorCode:'generation_limit'}];
 const m=metrics([c]);assert.equal(m.scores.executedArguments.rate,1);assert.equal(m.scores.proposedArguments.rate,0);assert.equal(m.scores.evidenceAttribution.rate,1);assert.equal(m.truncationTurns,1);assert.equal(m.scores.clarification.rate,null);
 assert.equal(latency([1,2,3]).p95Ms,null);const enough=latency(Array.from({length:20},(_,i)=>i+1));assert.equal(enough.p50Ms,10);assert.equal(enough.p95Ms,19);
 c.group='unsupported';c.turns=[{expected:{noTools:true,anyContains:['read-only']},result:{answer:'read-only',evidence:[]}}];c.trace=[];assert.equal(metrics([c]).scores.refusal.rate,1);
});

test('offline verification and comparison CLI write non-overwriting reports without live services',async()=>{
 const root=await mkdtemp(join(tmpdir(),'ores-comparison-'));
 const run=(script,args)=>spawnSync(process.execPath,[script,...args],{encoding:'utf8',timeout:30000});
 try{
  const out=join(root,'verify'),check=run('evals/check.mjs',['--out',out]);assert.equal(check.status,0,check.stderr);
  const checks=JSON.parse(await readFile(join(out,'checks.json'),'utf8'));assert.equal(checks.reports.length,3);assert.ok(checks.reports.every(r=>r.summary.handling.fail===0));
  const file=join(out,'replay/results.json'),dest=join(root,'comparison');const same=run('evals/compare.mjs',[file,file,'--out',dest,'--strict']);assert.equal(same.status,0,same.stderr);
  const comparison=JSON.parse(await readFile(join(dest,'comparison.json'),'utf8'));assert.equal(comparison.controlled,true);assert.deepEqual(comparison.regressions,[]);
  const changed=JSON.parse(await readFile(file,'utf8'));changed.results[0].handling='fail';changed.results[0].outcome='fail';changed.results[0].capability='unfulfilled';changed.summary=summarize(changed.results);const changedFile=join(root,'changed.json');await writeFile(changedFile,JSON.stringify(changed));
  assert.equal(run('evals/compare.mjs',[file,changedFile,'--out',join(root,'regressed'),'--strict']).status,2);
  assert.equal(run('evals/compare.mjs',[file,file,'--out',dest]).status,1);assert.equal(run('evals/check.mjs',['--out',out]).status,1);
 }finally{assert.equal(dirname(resolve(root)),resolve(tmpdir()));await rm(root,{recursive:true,force:true});}
});

test('preflight reports offline readiness and missing real-model prerequisites without printing secrets',()=>{
 const run=args=>spawnSync(process.execPath,['evals/preflight.mjs',...args],{encoding:'utf8',timeout:10000});
 const offline=run(['--profile','replay']);assert.equal(offline.status,0,offline.stderr);assert.equal(JSON.parse(offline.stdout).ready,true);
 const unavailable=run(['--profile','model']);assert.equal(unavailable.status,1);assert.equal(JSON.parse(unavailable.stdout).ready,false);assert.match(unavailable.stdout,/unavailable/);
});

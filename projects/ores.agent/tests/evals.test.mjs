import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile,mkdtemp,rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join,resolve,dirname } from 'node:path';
import { spawnSync } from 'node:child_process';
import { gradeTurn,summarize,strictFailed,redactor,validateCorpus } from '../evals/grade.mjs';
import { evaluateCase,readOnlyBackend } from '../evals/engine.mjs';
import { fixtureBackend,fixtureSession } from '../evals/fixtures.mjs';
const corpus=validateCorpus(JSON.parse(await readFile(new URL('../evals/cases.json',import.meta.url),'utf8')));
test('evaluation corpus has unique IDs and substantive strict oracles',()=>{
 assert.ok(corpus.cases.length>=20);
 const bad=structuredClone(corpus);bad.cases[0].turns[0].expect={contains:['hello']};assert.throws(()=>validateCorpus(bad));
 const typo=structuredClone(corpus);typo.cases[0].turns[0].expect.allowAnything=true;assert.throws(()=>validateCorpus(typo));
 const duplicate=structuredClone(corpus);duplicate.cases.push(duplicate.cases[0]);assert.throws(()=>validateCorpus(duplicate),/Duplicate/);
});
test('grading rejects incorrect financial facts, unrequested calls and fabricated data in unavailable results',()=>{
 const expected={tool:'read_fx_spot',statuses:['ok'],arguments:{provider:'ore'},checks:[{path:'observation.value',equals:'1.0552'}]};
 const result={answer:'fixture',evidence:[{tool:'read_fx_spot',status:'ok',observation:{value:'2'}}]};
 const failures=gradeTurn(expected,result,[{type:'tool',name:'read_fx_spot',arguments:{provider:'invented'}}]);assert.equal(failures.length,2);
 assert.ok(gradeTurn({tool:'read_fx_spot',statuses:['no_observation'],noFigures:true},{evidence:[{tool:'read_fx_spot',status:'no_observation',observation:{value:'0'}}]},[]).length);
 assert.ok(gradeTurn({noTools:true},{answer:'read-only',evidence:[]},[{type:'tool',name:'book_trade'}]).length);
});
test('blocked fulfillment is distinct from correct handling and stays in all-case denominators',()=>{
 const summary=summarize([{outcome:'blocked_data',handling:'pass',capability:'blocked_data'},{outcome:'fail',handling:'fail',capability:'unfulfilled'},{outcome:'error',handling:'error',capability:'unfulfilled'},{outcome:'blocked_identity',handling:'not_run',capability:'blocked_identity'}]);
 assert.equal(summary.total,4);assert.equal(summary.overallPassRate,0);assert.equal(summary.handlingPassRate,0.25);assert.equal(summary.outcomes.blocked_identity,1);assert.equal(summary.handling.not_run,1);assert.equal(strictFailed(summary,0),true);
 assert.equal(strictFailed(summarize([{outcome:'pass',handling:'pass',capability:'fulfilled'}]),1),false);
});
test('redaction handles secret fields, proposed JSON arguments, JWTs and credential values without hiding token budgets',()=>{
 const redact=redactor(['actual-private-password']);redact.add('actual-session-token');
 const raw={password:'unknown-private',arguments:'{"password":"unknown-private","limit":10}',maxOutputTokens:2048,answer:'Bearer abcdef actual-private-password actual-session-token eyJhbGciOiJub25lIn0.eyJzdWIiOiIxIn0.signature'};
 const text=JSON.stringify(redact.clean(raw));for(const secret of ['unknown-private','actual-private-password','actual-session-token','eyJhbGciOiJub25lIn0'])assert.ok(!text.includes(secret));assert.equal(redact.clean(raw).maxOutputTokens,2048);
});
test('evaluation backend blocks mutations before forwarding and permits only named reads/session lifecycle',async()=>{
 const calls=[],gate=readOnlyBackend({ready:()=>true,close:async()=>{},request:async(name)=>{calls.push(name);return {};}});
 for(const mutation of ['trigger_report','put_trade','storagePut','exportTradesToStorage','copy_run_scope'])await assert.rejects(()=>gate.request(mutation,{}),/cannot execute a mutation/);
 await gate.request('books',{});await gate.request('login',{});assert.deepEqual(calls,['books','login']);
});
test('real Agent/executor replay preserves clarification state, exact numbers and expected failures',async()=>{
 for(const id of ['provider-clarification','scope-run-clarification','scope-npv','scope-gamma','cancelled','inference-failure']){
  const spec=corpus.cases.find(c=>c.id===id),result=await evaluateCase(spec,{profile:'replay',backend:fixtureBackend(spec.scenario),session:fixtureSession(),redact:redactor()});
  assert.equal(result.handling,'pass',JSON.stringify(result.turns));assert.ok(result.trace.every(t=>!Object.hasOwn(t,'token')));
 }
});
test('unexpected per-case inference errors are recorded without exposing messages or preventing later cases',async()=>{
 const spec=corpus.cases.find(c=>c.id==='books'),redact=redactor();
 const broken=await evaluateCase(spec,{profile:'model',backend:fixtureBackend(),session:fixtureSession(),inference:{complete:async()=>{throw Error('private diagnostic text');}},redact});
 assert.equal(broken.outcome,'error');assert.ok(!JSON.stringify(broken).includes('private diagnostic text'));
 const next=await evaluateCase(spec,{profile:'replay',backend:fixtureBackend(),session:fixtureSession(),redact});assert.equal(next.outcome,'pass');assert.equal(summarize([broken,next]).total,2);
});
test('runner writes complete non-overwriting reports; normal mode accepts quality gaps, strict mode exposes them',async()=>{
 const root=await mkdtemp(join(tmpdir(),'ores-eval-'));
 const run=(out,...extra)=>spawnSync(process.execPath,['evals/run.mjs','--profile','replay','--cases','missing-vega,books','--out',out,...extra],{cwd:resolve('.'),encoding:'utf8',timeout:20000});
 try{
  const normal=run(join(root,'normal'));assert.equal(normal.status,0,normal.stderr);
  const report=JSON.parse(await readFile(join(root,'normal/results.json'),'utf8'));assert.equal(report.results.length,2);assert.equal(report.summary.outcomes.blocked_data,1);assert.equal(report.summary.handling.pass,2);assert.match(report.metadata.inferenceMode,/SCRIPTED/);
  assert.match(await readFile(join(root,'normal/summary.md'),'utf8'),/does not imply model accuracy/);
  const strict=run(join(root,'strict'),'--strict');assert.equal(strict.status,2,strict.stderr);
  const previous=await readFile(join(root,'normal/results.json'),'utf8');assert.equal(run(join(root,'normal')).status,1);assert.equal(await readFile(join(root,'normal/results.json'),'utf8'),previous);
  assert.equal(run(join(root,'bad'),'--cases','not-a-case').status,1);
 }finally{assert.equal(dirname(resolve(root)),resolve(tmpdir()));await rm(root,{recursive:true,force:true});}
});

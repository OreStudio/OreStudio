// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve,dirname,join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { isDeepStrictEqual as equal } from 'node:util';
import { summarize } from './grade.mjs';
import { metrics } from './metrics.mjs';

function validate(report){
 if(report.schemaVersion!==1||!['replay','model','live'].includes(report.profile)||!Array.isArray(report.results)||!report.results.length||!report.metadata||typeof report.corpusSha256!=='string')throw Error('Invalid full evaluation report.');
 const seen=new Set();
 for(const r of report.results){
  const key=`${r.id}:${r.repetition}`;
  if(!/^[a-z0-9-]+$/.test(r.id)||!Number.isInteger(r.repetition)||r.repetition<1||seen.has(key)||!['pass','fail','blocked_backend','blocked_data','blocked_identity','error'].includes(r.outcome)||!['pass','fail','error','not_run'].includes(r.handling)||!['fulfilled','unfulfilled','not_applicable','blocked_backend','blocked_data','blocked_identity'].includes(r.capability))throw Error('Invalid or duplicate case result.');
  seen.add(key);
 }
 if(!equal(summarize(report.results),report.summary))throw Error('Report summary does not match its results.');
 return report;
}
export function compare(beforeRaw,afterRaw,{allowIncompatible=false}={}){
 const before=validate(beforeRaw),after=validate(afterRaw);
 if(before.profile!==after.profile)throw Error('Profiles differ; replay, model and live are separate comparisons.');
 const ids=r=>[...new Set(r.results.map(c=>c.id))].sort();
 if(!equal(ids(before),ids(after)))throw Error('Case selections differ; compare the same case IDs without dropping cases.');
 const a=before.metadata,b=after.metadata,mismatches=[];
 const check=(name,x,y)=>{if(!equal(x,y)||x===undefined||y===undefined)mismatches.push(name);};
 check('corpus',before.corpusSha256,after.corpusSha256);
 check('grader',a.fingerprints?.['evals/grade.mjs'],b.fingerprints?.['evals/grade.mjs']);
 check('backendContract',a.backendContract,b.backendContract);
 check('toolSchemas',a.toolSchemaHash,b.toolSchemaHash);
 check('perTurnDeadlineMs',a.perTurnDeadlineMs,b.perTurnDeadlineMs);
 if(before.profile!=='replay'){
  check('inferenceSettings',a.inference,b.inference);check('modelRuntime',a.modelRuntime,b.modelRuntime);
  if(!a.modelRuntime?.observedModelSha256||!b.modelRuntime?.observedModelSha256)mismatches.push('modelArtifactProvenanceMissing');
 }
 if(before.profile==='live')check('liveSelection',a.liveSelection,b.liveSelection);
 if(mismatches.length&&!allowIncompatible)throw Error('Incompatible evaluation provenance; use --allow-incompatible for an explicitly labelled descriptive comparison.');
 const cases=ids(before).map(id=>{
  const first=before.results.filter(c=>c.id===id),second=after.results.filter(c=>c.id===id),x=summarize(first),y=summarize(second);
  return {id,before:x,after:y,handlingDelta:y.handlingPassRate-x.handlingPassRate,overallDelta:y.overallPassRate-x.overallPassRate};
 });
 const regressions=cases.filter(c=>c.handlingDelta<0||c.overallDelta<0).map(c=>c.id);
 return {schemaVersion:1,profile:before.profile,controlled:mismatches.length===0,mismatches,beforeRevision:a.agentRevision,afterRevision:b.agentRevision,unequalRepetitionCounts:cases.some(c=>c.before.total!==c.after.total),regressions,cases,before:metrics(before.results),after:metrics(after.results),limitations:'Descriptive corpus comparison, not a significance test. Blocked/error cases remain in all-case denominators. Live records may change even with equal configuration. Artifact records do not attest running-process bytes. Different repeat counts and oracle versions must be reviewed.'};
}
export function comparisonMarkdown(r){
 return `# Agent evaluation comparison\n\nProfile: ${r.profile}. Controlled provenance: ${r.controlled}.\nMismatches: ${r.mismatches.join(', ')||'none'}. Unequal repetition counts: ${r.unequalRepetitionCounts}.\n\nHandling: ${r.before.tasks.handling.pass}/${r.before.tasks.total} → ${r.after.tasks.handling.pass}/${r.after.tasks.total}.\nOverall: ${r.before.tasks.outcomes.pass}/${r.before.tasks.total} → ${r.after.tasks.outcomes.pass}/${r.after.tasks.total}.\n\n| Case | Before handling | After handling | Before overall | After overall |\n| --- | --- | --- | --- | --- |\n${r.cases.map(c=>`| ${c.id} | ${c.before.handling.pass}/${c.before.total} | ${c.after.handling.pass}/${c.after.total} | ${c.before.outcomes.pass}/${c.before.total} | ${c.after.outcomes.pass}/${c.after.total} |`).join('\n')}\n\nRegression case IDs: ${r.regressions.join(', ')||'none'}.\n\n${r.limitations}\n`;
}
async function main(){
 const [first,second,...args]=process.argv.slice(2);
 const options={};
 if(!first||!second)throw Error('Two full reports are required.');
 for(let i=0;i<args.length;i++){
  const key=args[i];if(!['--out','--strict','--allow-incompatible'].includes(key)||Object.hasOwn(options,key))throw Error('Unknown or duplicate comparison option.');
  if(key==='--out'){const value=args[++i];if(!value||value.startsWith('--'))throw Error('A new output directory is required.');options[key]=value;}else options[key]=true;
 }
 if(!options['--out'])throw Error('A new output directory is required.');
 const report=compare(JSON.parse(await readFile(resolve(first),'utf8')),JSON.parse(await readFile(resolve(second),'utf8')),{allowIncompatible:options['--allow-incompatible']});
 const out=resolve(options['--out']);await mkdir(dirname(out),{recursive:true,mode:0o700});await mkdir(out,{mode:0o700});
 await writeFile(join(out,'comparison.json'),JSON.stringify(report,null,2)+'\n',{flag:'wx',mode:0o600});
 await writeFile(join(out,'comparison.md'),comparisonMarkdown(report),{flag:'wx',mode:0o600});
 console.log(JSON.stringify({comparisonDirectory:out,controlled:report.controlled,regressions:report.regressions}));
 return options['--strict']&&(!report.controlled||report.regressions.length)?2:0;
}
if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url))main().then(code=>{process.exitCode=code;}).catch(()=>{console.error('Comparison failed: check full reports, matching cases/provenance and a new output directory.');process.exitCode=1;});

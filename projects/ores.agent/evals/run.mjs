// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile,writeFile,mkdir,readdir } from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import { createHash,randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname,resolve,join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
import { tools } from '../dist/tools.js';
import { NatsBackend,backendRevision } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { HttpInference } from '../dist/inference.js';
import { loadConfig } from '../dist/config.js';
import { fixtureBackend,fixtureSession } from './fixtures.mjs';
import { validateCorpus,summarize,redactor,strictFailed } from './grade.mjs';
import { evaluateCase,digest,readOnlyBackend } from './engine.mjs';
import { metrics } from './metrics.mjs';
const component=resolve(dirname(fileURLToPath(import.meta.url)),'..');
const options={profile:'replay',repeat:1};
const switches=new Set(['strict','require-handling','live-read','help']);
const valued=new Set(['profile','repeat','cases','corpus','out','config','credentials','party','other-party','inference-root','min-pass','timeout-ms']);
for(let index=2;index<process.argv.length;index++){
 const name=process.argv[index].replace(/^--/,'');if(!process.argv[index].startsWith('--')||!switches.has(name)&&!valued.has(name))throw Error('Unknown evaluation option. Use --help.');
 if(switches.has(name))options[name]=true;else{const value=process.argv[++index];if(!value||value.startsWith('--'))throw Error('Missing evaluation option value.');options[name]=value;}
}
if(options.help){console.log('npm run eval -- --profile replay|model|live [--cases id,id] [--corpus PATH] [--repeat 1..5] [--timeout-ms 100..120000] [--out NEW_DIRECTORY] [--strict --min-pass 0..1 --require-handling]\nmodel: --config AGENT_CONFIG [--inference-root ROOT]\nlive: --live-read --config AGENT_CONFIG --credentials PRIVATE_JSON --party UUID [--other-party UUID] [--inference-root ROOT]');process.exit(0);}
async function hashFile(path){const hash=createHash('sha256');for await(const chunk of createReadStream(path))hash.update(chunk);return hash.digest('hex');}
function markdown(report){
 const s=report.summary,rows=report.results.map(r=>`| ${r.id} | ${r.repetition} | ${r.outcome} | ${r.handling} | ${r.capability} | ${Math.round(r.durationMs??0)} |`);
 const findings=report.results.filter(r=>r.outcome!=='pass').map(r=>`- ${r.id} (${r.repetition}): ${r.outcome}. ${r.capabilityReason??''} ${(r.turns??[]).flatMap(t=>t.failures??[]).join(' ')} ${r.error?.code??''}`);
 return `# Agent evaluation report\n\nProfile: ${report.profile}. Data: ${report.metadata.dataMode}.\nInference: ${report.metadata.inferenceMode}\nCorpus: ${report.corpusVersion}; revision: ${report.metadata.agentRevision}.\n\nRunner completion does not imply model accuracy or business capability fulfillment.\n\nTotal: ${s.total}. Outcomes: ${JSON.stringify(s.outcomes)}.\nHandling: ${JSON.stringify(s.handling)}. Capability: ${JSON.stringify(s.capability)}.\nOverall pass rate: ${(100*s.overallPassRate).toFixed(1)}%; handling pass rate: ${(100*s.handlingPassRate).toFixed(1)}%.\nDenominator: ${s.denominator}\n\n| Case | Repetition | Overall | Handling | Business capability | Milliseconds |\n| --- | ---: | --- | --- | --- | ---: |\n${rows.join('\n')}\n\n## Findings\n\n${findings.join('\n')||'No failed or blocked cases in this selection.'}\n\nMetrics: ${JSON.stringify(report.metrics.scores)}.\nTruncation turns: ${report.metrics.truncationTurns}. Timing sample counts and percentile eligibility: see results.json.\n\nSee results.json for expected/actual evidence, redacted traces, failures and provenance.\nLexical answer checks are bounded oracles, not comprehensive semantic or financial certification.\n`;
}
async function main(){
 if(!['replay','model','live'].includes(options.profile))throw Error('Invalid evaluation profile.');
 const repeat=Number(options.repeat),minimum=Number(options['min-pass']??1),timeoutMs=Number(options['timeout-ms']??60000);
 if(!Number.isInteger(repeat)||repeat<1||repeat>5||!Number.isFinite(minimum)||minimum<0||minimum>1)throw Error('Invalid repetition count or threshold.');
 if(!Number.isInteger(timeoutMs)||timeoutMs<100||timeoutMs>120000)throw Error('Invalid per-turn deadline.');
 if(options['min-pass']!==undefined&&!options.strict)throw Error('--min-pass requires --strict.');
 if(options['require-handling']&&!options.strict)throw Error('--require-handling requires --strict.');
 if(options.profile==='live'&&!options['live-read'])throw Error('Live reads require explicit --live-read.');
 if(options.profile!=='replay'&&!options.config)throw Error('Model/live profile requires --config.');
 if(options.profile==='live'&&(!options.credentials||!options.party))throw Error('Live profile requires credentials and a selected party UUID.');
 const corpusPath=options.corpus?resolve(options.corpus):join(component,'evals/cases.json');
 const corpusText=await readFile(corpusPath,'utf8'),corpus=validateCorpus(JSON.parse(corpusText));
 const requested=options.cases?.split(',');if(requested?.some(id=>!corpus.cases.some(c=>c.id===id&&c.profiles.includes(options.profile))))throw Error('Unknown case or case unavailable for selected profile.');
 const selected=corpus.cases.filter(c=>c.profiles.includes(options.profile)&&(!requested||requested.includes(c.id)));
 if(!selected.length)throw Error('No evaluation cases selected.');
 const output=resolve(options.out??join(component,'../../build/output/agent-evals',new Date().toISOString().replace(/[:.]/g,'-')+'-'+options.profile+'-'+randomUUID().slice(0,8)));
 await mkdir(dirname(output),{recursive:true,mode:0o700});
 await mkdir(output,{mode:0o700});
 // Refuse overwrites before executing any case or establishing backend sessions.
 const reservation=join(output,'run-reservation.json');await writeFile(reservation,JSON.stringify({profile:options.profile,startedAt:new Date().toISOString()})+'\n',{flag:'wx',mode:0o600});
 const redact=redactor(),results=[],startedAt=new Date().toISOString();let backend,sessions,mainSession,otherSession,loginError,otherLoginError;
 const config=options.config?await loadConfig(resolve(options.config)):null;
 if(config?.inference.apiKeyFile)redact.add((await readFile(config.inference.apiKeyFile,'utf8')).trim());
 const inference=config?new HttpInference(config.inference):null;
 let revision='unknown',dirty=true;try{revision=execFileSync('git',['rev-parse','HEAD'],{cwd:component,encoding:'utf8'}).trim();dirty=Boolean(execFileSync('git',['status','--porcelain','--','.'],{cwd:component,encoding:'utf8'}).trim());}catch{}
 const sourceFiles=['package.json','package-lock.json'];
 for(const folder of ['src','evals'])for(const entry of await readdir(join(component,folder),{withFileTypes:true}))if(entry.isFile()&&/\.(ts|mjs|json)$/.test(entry.name))sourceFiles.push(folder+'/'+entry.name);
 const fingerprints={};for(const name of sourceFiles.sort())fingerprints[name]=await hashFile(join(component,name));
 const metadata={agentRevision:revision,componentDirty:dirty,fingerprints,backendContract:backendRevision,node:process.version,platform:process.platform,
  dataMode:options.profile==='live'?'Existing backend records; no business mutations.':'LABELLED FIXTURE DATA; not live OreStudio.',
  inferenceMode:options.profile==='replay'?'SCRIPTED REPLAY; not a model-quality score.':'Actual configured local model.',perTurnDeadlineMs:timeoutMs,
  inference:config?{model:config.inference.model,timeoutMs:config.inference.timeoutMs,maxOutputTokens:config.inference.maxOutputTokens??2048,temperature:0.1,topK:50,repeatPenalty:1.1}:null,
  modelRuntime:{provenance:'Not supplied; use --inference-root to record configured artifacts. Actual served process is not attested.'},
  toolSchemaHash:digest(Object.entries(tools).map(([name,t])=>({name,description:t.description,schema:z.toJSONSchema(t.schema,{io:'input'})}))),
  liveSelection:options.profile==='live'?{partyId:options.party,otherPartyId:options['other-party']??null}:null};
 if(options['inference-root']){
  const root=resolve(options['inference-root']),lock=JSON.parse(await readFile(join(root,'inference-lock.json'),'utf8')),spec=JSON.parse(await readFile(join(root,'service.json'),'utf8'));
  const flags={};for(const key of ['--ctx-size','--parallel','--n-gpu-layers','--device','--threads','--threads-batch','--temp','--top-k','--repeat-penalty']){const at=spec.args.indexOf(key);if(at>=0)flags[key]=spec.args[at+1];}
  const observed=await hashFile(join(root,'models',lock.model.file));
  metadata.modelRuntime={provenance:'Configured files/artifact hashes; actual served process not attested by this evaluator.',release:lock.release,modelRepository:lock.model.repository,modelRevision:lock.model.revision,modelFile:lock.model.file,quantization:lock.model.file.replace(/\.gguf$/,'').split('-').slice(-2).join('-'),expectedModelSha256:lock.model.sha256,observedModelSha256:observed,modelHashMatches:observed===lock.model.sha256,runtimeBinarySha256:await hashFile(spec.command),flags};
  if(observed!==lock.model.sha256)throw Error('Configured model artifact hash mismatch.');
 }
 try{
  if(options.profile==='live'){
   const c=JSON.parse(await readFile(resolve(options.credentials),'utf8'));redact.add(c.tenantPassword);
   try{backend=readOnlyBackend(await NatsBackend.open(config.nats));sessions=new Sessions(backend);
    const auth=await sessions.login({principal:c.tenantUsername,password:c.tenantPassword,partyId:options.party});redact.add(auth.accessToken);mainSession=sessions.require('Bearer '+auth.accessToken);redact.add(mainSession.token,mainSession.sessionId);
   }catch(error){loginError={code:error.code??'connection_or_login_error',type:error.constructor.name};}
   if(mainSession&&options['other-party'])try{const auth=await sessions.login({principal:c.tenantUsername,password:c.tenantPassword,partyId:options['other-party']});redact.add(auth.accessToken);otherSession=sessions.require('Bearer '+auth.accessToken);redact.add(otherSession.token,otherSession.sessionId);}catch(error){otherLoginError={code:error.code??'party_login_error',type:error.constructor.name};}
  }
  for(let repetition=1;repetition<=repeat;repetition++)for(const original of selected){
   const spec=options.profile==='live'?{...original,...original.live}:original;
   const session=options.profile==='live'?(spec.requiresOtherParty?otherSession:mainSession):fixtureSession(spec.requiresOtherParty);
   let result;
   if(!session){const identityMissing=spec.requiresOtherParty&&!options['other-party'];result={id:spec.id,group:spec.group,intent:spec.intent,outcome:identityMissing?'blocked_identity':loginError?'error':'blocked_identity',handling:identityMissing?'not_run':loginError?'error':'not_run',capability:'blocked_identity',capabilityReason:identityMissing?'No existing alternate-party identity was supplied.':'Existing identity/backend could not be established.',error:loginError??otherLoginError,turns:[],trace:[],durationMs:0};}
   else try{result=await evaluateCase(spec,{profile:options.profile,backend:options.profile==='live'?backend:fixtureBackend(spec.scenario),session,inference,redact,timeoutMs});}
   catch(error){result={id:spec.id,group:spec.group,intent:spec.intent,outcome:'error',handling:'error',capability:'unfulfilled',error:{code:error.code??'case_execution_error',type:error.constructor.name},turns:[],trace:[],durationMs:0};}
   result.repetition=repetition;result.dataMode=metadata.dataMode;result.inferenceMode=metadata.inferenceMode;results.push(redact.clean(result));
   console.log(JSON.stringify({case:spec.id,repetition,outcome:result.outcome,handling:result.handling}));
   await writeFile(join(output,'progress.json'),JSON.stringify(redact.clean({completed:results.length,total:selected.length*repeat,last:result.id}))+'\n',{mode:0o600});
  }
 }finally{await sessions?.close();await backend?.close();}
 const report=redact.clean({schemaVersion:1,corpusVersion:corpus.version,corpusSha256:digest(corpusText),profile:options.profile,startedAt,completedAt:new Date().toISOString(),metadata,summary:summarize(results),metrics:metrics(results),results});
 await writeFile(join(output,'results.json'),JSON.stringify(report,null,2)+'\n',{flag:'wx',mode:0o600});
 await writeFile(join(output,'summary.md'),markdown(report),{flag:'wx',mode:0o600});
 console.log(JSON.stringify({reportDirectory:output,...report.summary}));
 return options.strict&&(strictFailed(report.summary,minimum)||options['require-handling']&&report.summary.handling.pass!==report.summary.total)?2:0;
}
main().then(code=>{process.exitCode=code;}).catch(()=>{console.error('Evaluation harness could not initialise or write a valid report. Check options, private paths and corpus validity; use --help.');process.exitCode=1;});

// SPDX-License-Identifier: GPL-3.0-or-later
import { mkdir,readFile,writeFile } from 'node:fs/promises';
import { resolve,join,dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
const component=resolve(dirname(fileURLToPath(import.meta.url)),'..');
async function main(){
 const args=process.argv.slice(2);
 if(args.length&&!(args.length===2&&args[0]==='--out'&&args[1]&&!args[1].startsWith('--')))throw Error('Usage: check.mjs [--out NEW_DIRECTORY]');
 const out=resolve(args[1]??join(component,'../../build/output/agent-checks',new Date().toISOString().replace(/[:.]/g,'-')+'-'+randomUUID().slice(0,8)));
 await mkdir(dirname(out),{recursive:true,mode:0o700});await mkdir(out,{mode:0o700});
 const reports=[];
 for(const [name,corpus] of [['replay','evals/cases.json'],['holdout','evals/holdout-stage5.json']]){
  const result=spawnSync(process.execPath,['evals/run.mjs','--profile','replay','--corpus',corpus,'--strict','--min-pass','0','--require-handling','--out',join(out,name)],{cwd:component,stdio:'inherit',timeout:120000});
  if(result.error||result.status!==0)throw Error('Offline replay integrity gate failed.');
  const report=JSON.parse(await readFile(join(out,name,'results.json'),'utf8'));reports.push({name,corpusSha256:report.corpusSha256,summary:report.summary});
 }
 await writeFile(join(out,'checks.json'),JSON.stringify({profile:'offline replay only',gate:'Every handling check must pass, no execution errors; blocked capabilities remain blocked.',reports},null,2)+'\n',{flag:'wx',mode:0o600});
 console.log(JSON.stringify({checksDirectory:out,handlingGate:'pass'}));
}
main().catch(()=>{console.error('Offline verification failed. Inspect the private reports and use a new output directory.');process.exitCode=1;});

// Deployed read-only Greek reconciliation against independent CSV/Decimal totals.
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {Client,StreamableHTTPClientTransport} from '@modelcontextprotocol/client';
const [base,credentialsPath,partyId,referencePath,otherPartyId,erroredRunId]=process.argv.slice(2);
if(!referencePath)throw Error('Pass URL, private credentials, party UUID, independent reference, optional other party and errored run UUID.');
const c=JSON.parse(await readFile(credentialsPath,'utf8')),reference=JSON.parse(await readFile(referencePath,'utf8'));
const post=async(path,body,token)=>{const response=await fetch(base+path,{method:'POST',headers:{'content-type':'application/json',...(token?{authorization:'Bearer '+token}:{})},body:JSON.stringify(body),signal:AbortSignal.timeout(125000)});const result=await response.json();assert.equal(response.status,200,JSON.stringify(result));return result;};
const login=p=>post('/auth/login',{principal:c.tenantUsername,password:c.tenantPassword,partyId:p});
const auth=await login(partyId),client=new Client({name:'greek-independent-csv-reference',version:'0.1.5'});
const call=async(args)=>{const r=await client.callTool({name:'summarize_scope_results',arguments:args});assert.ok(!r.isError,JSON.stringify(r));return JSON.parse(r.content[0].text);};
const key=b=>JSON.stringify([b.measure,b.factor,b.shift,b.factor2,b.shift2,b.currency,b.scenario]);
try{
 await client.connect(new StreamableHTTPClientTransport(new URL(base+'/mcp'),{requestInit:{headers:{authorization:'Bearer '+auth.accessToken}}}));
 for(const ref of reference){
  const selected={scopeType:ref.scopeType,scope:ref.scope,runId:ref.runId,measure:'greeks',bucketLimit:2,limit:25};
  const found=new Map();let bucketOffset=0;const start=Date.now();
  do{const page=await call({...selected,bucketOffset});assert.equal(page.membershipAt,ref.membershipAt);assert.equal(page.scope.id,ref.scopeId);
   if(!ref.buckets.length){assert.equal(page.status,'unavailable');break;}
   for(const b of page.buckets){assert.ok(!found.has(key(b)));found.set(key(b),b);}bucketOffset=page.nextBucketOffset;
  }while(bucketOffset!==null);
  assert.equal(found.size,ref.buckets.length);
  let contributors=0,complete=0;
  for(const expected of ref.buckets){
   const initial=found.get(key(expected));assert.ok(initial,key(expected));assert.equal(initial.coverage.expectedTradeCount,ref.expected);assert.equal(initial.coverage.valuedTradeCount,expected.valued);assert.equal(initial.coverage.missingTradeCount,expected.missing);assert.equal(initial.coverage.duplicateTradeCount,0);
   if(expected.complete){assert.equal(initial.total.figure,expected.total);complete++;}else assert.equal(initial.total,null);
   const seen=new Set();let offset=0;
   do{const result=await call({...selected,measure:expected.measure.toLowerCase(),factor:expected.factor,factor2:expected.factor2,scenario:expected.scenario,bucketLimit:10,offset});
    const bucket=result.buckets.find(b=>key(b)===key(expected));assert.ok(bucket);assert.equal(bucket.total?.figure??null,expected.total);
    for(const row of bucket.contributors){assert.ok(!seen.has(row.tradeId));seen.add(row.tradeId);assert.equal(row.figure,expected.contributors[row.tradeId]);assert.equal(row.currency,expected.currency);assert.ok(row.metricId&&row.resultId&&row.line>=2);}
    offset=bucket.nextOffset;
   }while(offset!==null);
   assert.equal(seen.size,Object.keys(expected.contributors).length);contributors+=seen.size;
  }
  console.log(JSON.stringify({independentMcp:true,csvDecimalReference:true,scope:ref.scope,buckets:found.size,completeBuckets:complete,reconciledContributors:contributors,durationMs:Date.now()-start}));
 }
 const book=reference.find(r=>r.scopeType==='book'&&r.buckets.some(b=>b.complete&&b.measure==='Delta'&&b.shift==='0.0001'));
 assert.ok(book,'A real complete rate sensitivity bucket is required.');
 const rate=book.buckets.find(b=>b.complete&&b.measure==='Delta'&&b.shift==='0.0001');
 const selected={scopeType:book.scopeType,scope:book.scope,runId:book.runId,measure:'delta',factor:rate.factor};
 const dv01=await call({...selected,measure:'dv01'});assert.equal(dv01.buckets[0].total.figure,rate.total);
 const wrong=await call({...selected,currency:rate.currency==='USD'?'GBP':'USD'});assert.ok(wrong.buckets.every(b=>!b.total));
 const vega=await call({...selected,measure:'vega'});assert.equal(vega.status,'unavailable');assert.equal(vega.buckets,undefined);
 if(erroredRunId){const bad=await call({...selected,runId:erroredRunId});assert.equal(bad.status,'unavailable');assert.equal(bad.buckets,undefined);}
 if(otherPartyId){const other=await login(otherPartyId);try{const denied=await post('/tools/summarize_scope_results',selected,other.accessToken);assert.equal(denied.status,'not_found');}finally{await post('/auth/logout',{},other.accessToken);}}
 for(const measure of ['delta','gamma','dv01']){
  const result=await post('/chat',{message:`Show ${measure} for book "${book.scope}" from run ${book.runId} for factor ${rate.factor}`},auth.accessToken);
  const evidence=result.evidence.find(e=>e.tool==='summarize_scope_results');assert.ok(evidence?.buckets?.length,JSON.stringify(result));
  const expected=book.buckets.find(b=>b.factor===rate.factor&&b.measure===(measure==='gamma'?'Gamma':'Delta'));
  assert.equal(evidence.buckets[0].total.figure,expected.total);assert.ok(result.answer.includes(expected.total+' '+expected.currency));assert.match(result.answer,/cash finite difference/);
  console.log(JSON.stringify({gpuChat:true,measure,total:expected.total,currency:expected.currency,durationMs:result.durationMs}));
 }
 const clarification=await post('/chat',{message:`Show Greeks for book "${book.scope}"`},auth.accessToken);assert.equal(clarification.selectionRequired,'runId');
 const resumed=await post('/chat',{message:book.runId},auth.accessToken);assert.ok(resumed.evidence.some(e=>e.buckets?.length));
 console.log(JSON.stringify({runClarification:true,missingNotZero:true,dv01ConventionExplicit:true,vegaNoData:true,otherPartyDenied:Boolean(otherPartyId),erroredRunWithheld:Boolean(erroredRunId)}));
}finally{try{await client.close();}finally{await post('/auth/logout',{},auth.accessToken);}}

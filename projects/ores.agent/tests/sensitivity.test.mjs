import test from 'node:test';
import assert from 'node:assert/strict';
import { summarizeSensitivities } from '../dist/sensitivity.js';
const id = n => `11111111-1111-4111-8111-${String(n).padStart(12,'0')}`;
const expected = new Set([id(1),id(2)]), identity={tenantId:id(3),partyId:id(4),runId:id(5),definitionId:id(6),pricedAt:'2026-11-06'};
const args={measure:'greeks',offset:0,limit:10,bucketOffset:0,bucketLimit:10};
function row(trade=1,measure='Delta',factor='DiscountCurve/GBP/0/1Y',value=trade/10,extra={}) {
 return {id:id(10+trade),tenant_id:id(3),party_id:id(4),report_instance_id:id(5),report_definition_id:id(6),result_id:id(7),line:trade+1,as_of:'2026-11-06T00:00:00Z',subject_type:'trade',subject_id:id(trade),measure,value,source:'sensitivity.csv',family:'sensitivity_report',currency:'GBP',scenario:'',factor:'',qualifiers:JSON.stringify({IsPar:'false',Factor_1:factor,ShiftSize_1:'0.0001',ShiftSize_2:'0',Currency:'GBP',...extra})};
}
const summarize=(rows,request={})=>summarizeSensitivities(rows,expected,{...args,...request},identity);
test('cash finite differences add exactly within each factor/shift/scenario/currency/measure bucket',()=>{
 const r=summarize([row(),row(2),row(1,'Gamma','DiscountCurve/GBP/0/1Y',-0.1),row(2,'Gamma','DiscountCurve/GBP/0/1Y',0.01)]);
 assert.equal(r.status,'ok');assert.equal(r.buckets.length,2);assert.deepEqual(r.buckets.map(b=>b.total.figure),['0.3','-0.09']);
 for(const b of r.buckets){assert.match(b.unit,/cash_finite_difference/);assert.match(b.normalization,/not divided/);assert.equal(b.coverage.failedTradeCount,null);}
});
test('sparse, suppressed, duplicate and currency-conflicting members never yield a full total',()=>{
 for(const rows of [[row()],[row(),row(2),{...row(),id:id(30)}],[row(),{...row(2),currency:'USD',qualifiers:row(2, 'Delta',undefined,0.2,{Currency:'USD'}).qualifiers}]]) {
  const r=summarize(rows);assert.equal(r.status,'incomplete');assert.ok(r.buckets.every(b=>b.total===null));
 }
 const mismatch=summarize([row(),row(2)],{currency:'USD'});assert.equal(mismatch.buckets[0].total,null);
 assert.equal(summarize([]).status,'unavailable');
});
test('factors, shifts, scenarios, raw/par dimensions and cross-gamma are never combined',()=>{
 const rows=[row(),row(2),row(1,'Delta','DiscountCurve/GBP/1/5Y'),row(2,'Delta','DiscountCurve/GBP/1/5Y'),{...row(),scenario:'stress'}, {...row(2),scenario:'stress'}];
 const r=summarize(rows);assert.equal(r.buckets.length,3);assert.ok(r.buckets.every(b=>b.coverage.expectedTradeCount===2));
 const shifted=summarize([row(),row(2,'Delta',undefined,0.2,{ShiftSize_1:'0.001'})]);assert.equal(shifted.buckets.length,2);assert.ok(shifted.buckets.every(b=>!b.total));
 const precise=summarize([row(),row(2,'Delta',undefined,0.2,{ShiftSize_1:'0.000100000000000000001'})]);assert.equal(precise.buckets.length,2);assert.ok(precise.buckets.every(b=>!b.total));
 const cross=[row(1,'Gamma',undefined,1,{Factor_2:'FXSpot/USDGBP/0/spot',ShiftSize_2:'0.01'}),row(2,'Gamma',undefined,2,{Factor_2:'FXSpot/USDGBP/0/spot',ShiftSize_2:'0.01'})];
 assert.equal(summarize(cross).buckets[0].total.figure,'3');assert.equal(summarize(cross,{measure:'delta'}).status,'unavailable');
 assert.equal(summarize([row(1,'Delta',undefined,1,{IsPar:'true'}),row(2)]).status,'unavailable');
});
test('DV01 selects only rate Delta at an explicit one-basis-point target shift; vega selects volatility Delta',()=>{
 const rates=[row(),row(2)], vols=[row(1,'Delta','FXVolatility/USDGBP/0/1Y',1,{ShiftSize_1:'0.01'}),row(2,'Delta','FXVolatility/USDGBP/0/1Y',2,{ShiftSize_1:'0.01'})];
 assert.equal(summarize([...rates,...vols],{measure:'dv01'}).buckets[0].total.figure,'0.3');
 const vega=summarize([...rates,...vols],{measure:'vega'});assert.equal(vega.buckets[0].total.figure,'3');assert.equal(vega.buckets[0].shift,'0.01');
 assert.equal(summarize(vols,{measure:'dv01'}).status,'unavailable');assert.equal(summarize(rates,{measure:'vega'}).status,'unavailable');
});
test('missing, rounded-to-zero or conflicting metadata withholds all figures; identities fail closed',()=>{
 for(const extra of [{ShiftSize_1:'0'}, {ShiftSize_1:'NaN'}, {ShiftSize_1:'-0.01'}, {Currency:'USD'}, {Factor_1:''}, {ShiftSize_2:'0.001'}, {Factor_2:'FXSpot/USDGBP/0/spot',ShiftSize_2:'0'}]) assert.equal(summarize([row(1,'Delta',undefined,1,extra),row(2)]).status,'unavailable');
 for(const extra of [{tenant_id:id(99)},{party_id:id(99)},{report_instance_id:id(99)},{report_definition_id:id(99)},{subject_id:id(99)},{source:'parsensitivity.csv'},{as_of:'2026-11-05T00:00:00Z'}]) assert.throws(()=>summarize([{...row(),...extra},row(2)]),/identity/);
 assert.throws(()=>summarize([{...row(),qualifiers:'invalid'},row(2)]),/Invalid sensitivity qualifiers/);
});
test('bucket and contributor pagination keep independent whole-scope totals and raw provenance',()=>{
 const rows=[row(),row(2),row(1,'Gamma',undefined,1),row(2,'Gamma',undefined,2)];
 const first=summarize(rows,{bucketLimit:1,limit:1}), next=summarize(rows,{bucketLimit:1,bucketOffset:1,limit:1});
 assert.equal(first.nextBucketOffset,1);assert.equal(next.nextBucketOffset,null);assert.equal(first.buckets[0].total.figure,'0.3');assert.equal(next.buckets[0].total.figure,'3');
 const contributor=first.buckets[0].contributors[0];assert.equal(contributor.resultId,id(7));assert.equal(contributor.line,3);assert.equal(first.buckets[0].nextOffset,1);
});

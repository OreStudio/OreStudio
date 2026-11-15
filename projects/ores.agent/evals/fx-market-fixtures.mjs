// SPDX-License-Identifier: GPL-3.0-or-later
// Labelled server-read fixtures; immutable historical corpora keep their own fixtures.
import { AppError } from '../dist/backend.js';
import { fxDataset } from './fx-e2e-dataset.mjs';
export const marketId = n => `11111111-1111-4111-8111-${String(n).padStart(12,'0')}`;
export function marketFixtureBackend(caseId = '') {
    const owner={tenant_id:marketId(1),party_id:marketId(2)};
    if(caseId.startsWith('market-dialogue-test')) {
        const data=fxDataset(owner.tenant_id,owner.party_id),calls=[];
        return {calls,ready:()=>true,close:async()=>{},request:async(op,args,session,signal)=>{
            signal?.throwIfAborted();calls.push({op,args:structuredClone(args),partyId:session?.partyId});
            const rows=op==='series'?data.series.map(s=>({...s,...owner})):op==='observations'?data.observations.filter(o=>o.series_id===args.series_id).map(o=>({...o,...owner})):undefined;
            if(!rows)throw Error('Read-only FX dialogue fixture only supports series and observations.');
            return {result:{outcome:'ok'},total:rows.length,[op==='series'?'market_series':'market_observations']:rows.slice(args.offset,args.offset+args.limit)};
        }};
    }
    const uri=(instrument,asset='fx',currency='EUR',extra='&ccy=USD')=>`oresmd://${asset}/${currency}?type=series&instrument=${instrument}&quote=rate${extra}`;
    const series=[
        {id:marketId(30),...owner,provider_code:'ore',oresmd_uri:uri('fx_spot'),series_subclass:'fx_spot'},
        {id:marketId(31),...owner,provider_code:'ore',oresmd_uri:uri('fx_fwd'),series_subclass:'fx_fwd'},
        {id:marketId(32),...owner,provider_code:'curve-provider',oresmd_uri:uri('discount','ir','USD','&curve_id=USD-SOFR'),series_subclass:'discount'},
    ];
    if(caseId.includes('ambiguous-curve'))series.push({...series[2],id:marketId(33),provider_code:'other-curve-provider'});
    if(caseId.includes('ambiguous-spot'))series.push({...series[0],id:marketId(34)});
    if(caseId.includes('ambiguous-forward'))series.push({...series[1],id:marketId(35)});
    if(caseId.includes('missing-series'))series.splice(1,1);
    if(caseId.includes('foreign-series'))series[1].party_id=marketId(99);
    const rows=s=>s.series_subclass==='fx_spot'?[{term:null,value:'1.1'}]:['1M','3M','6M','1Y'].map(term=>({term,value:s.series_subclass==='fx_fwd'?(caseId.includes('points')?'10':'1.1'):'1'}));
    const calls=[];
    return {calls,ready:()=>true,close:async()=>{},request:async(op,args,session,signal)=>{
        signal?.throwIfAborted();calls.push({op,args:structuredClone(args),partyId:session?.partyId});
        if(caseId.includes('denied'))throw new AppError('backend_denied','LABELLED FIXTURE denial',403);
        if(op==='series')return{result:{outcome:'ok'},total:series.length,market_series:series.slice(args.offset,args.offset+args.limit)};
        if(op==='observations'){
            const s=series.find(s=>s.id===args.series_id);if(!s)throw Error('Unknown fixture series');
            let observations=rows(s).map(r=>({series_id:s.id,...owner,observation_datetime:caseId.includes('stale')?'2025-12-31 00:00:00.000000Z':'2026-01-01 00:00:00.000000Z',source:'LABELLED EXISTING MARKET DATA FIXTURE',value:r.value,oresmd_uri:s.oresmd_uri.replace('type=series','type=quote')+(r.term?'&term='+r.term:'')}));
            if(caseId.includes('foreign-observation'))observations[0].party_id=marketId(99);
            if(caseId.includes('conflicting-observation'))observations.push({...observations[0],source:'OTHER LABELLED SOURCE',value:'9'});
            if(caseId.includes('empty-observation'))observations=[];
            if(caseId.includes('incomplete-page'))return{result:{outcome:'ok'},total:51,market_observations:observations.slice(0,1)};
            return{result:{outcome:'ok'},total:observations.length,market_observations:observations.slice(args.offset,args.offset+args.limit)};
        }
        throw Error('The market selection flow may only read series and observations.');
    }};
}

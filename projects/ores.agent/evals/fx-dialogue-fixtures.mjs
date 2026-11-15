// SPDX-License-Identifier: GPL-3.0-or-later
// Scripted model decisions for isolated executor/transport tests, NOT an NLU implementation.
export function dialogueDecision(updates = {}, evidence = {}, extra = {}) {
    const name=extra.acceptTestConventions?'accept_fx_test_conventions':extra.action==='clarify'?'clarify_fx_selection':'update_fx_selection';
    const args=extra.acceptTestConventions?{}:extra.action==='clarify'?{question:extra.question}:{selections:Object.entries(updates).map(([field,value])=>({field,value,quote:evidence[field]}))};
    return {role:'assistant',content:null,tool_calls:[{id:'fixture-dialogue',type:'function',function:{name,arguments:JSON.stringify(args)}}]};
}
export function scriptedDialogue() {
    return {complete:async(messages)=>{
        const message=messages.at(-1).content, {pending}=JSON.parse(messages[0].content.split('Pending curve context (data):\n').at(-1));
        const fields = {
            '2026-01-01': {[pending.field]:'2026-01-01'},
            '2026-13-01': {}, 'ore':{provider:'ore'},
            'USD curve USD-SOFR':{usdCurve:'USD-SOFR'},
            'outrights':{forwardQuote:'outright'}, 'unadjusted':{calendar:'unadjusted'},
            'no':{endOfMonth:false}, 'valuation date':{usdPillarAnchor:'valuation'},
            '11111111-1111-4111-8111-000000000033':{usdCurve:'11111111-1111-4111-8111-000000000033'},
            '11111111-1111-4111-8111-000000000034':{spotSeriesId:'11111111-1111-4111-8111-000000000034'},
            '11111111-1111-4111-8111-000000000035':{forwardSeriesId:'11111111-1111-4111-8111-000000000035'},
            'Use EUR/USD spot and forward quotes from provider ore, and USD curve USD-SOFR, as of 2026-01-01.':{provider:'ore',usdCurve:'USD-SOFR',asOf:'2026-01-01'},
            'Spot date 2026-01-01; forward quotes are outrights; unadjusted dates; no end of month; USD tenors use valuation date.':{spotDate:'2026-01-01',forwardQuote:'outright',calendar:'unadjusted',endOfMonth:false,usdPillarAnchor:'valuation'},
        };
        if(message==='use test data set conventions') return dialogueDecision({}, {}, {acceptTestConventions:true});
        if(message==='What does that mean?' || message==='2026-13-01') return dialogueDecision({}, {}, {action:'clarify',question:'What is the spot settlement date (the date the spot currency exchange settles)? Please use a valid YYYY-MM-DD date.'});
        const updates=fields[message]; if(!updates) throw Error('No scripted model decision for this test message');
        return dialogueDecision(updates,Object.fromEntries(Object.keys(updates).map(k=>[k,message])));
    }};
}

'use strict';
/* Shared by every prototype in the Trades group. The four copies are identical.
 *
 * Mock data only. The trade types are transcribed from
 * projects/ores.trading/modeling/ores.trading.trade_type_catalogue.org at
 * commit 07473ec3de. Nothing here calls a service.
 */

/* ---------- helpers ---------- */
const $ = (sel, root = document) => root.querySelector(sel);
function h(tag, attrs, ...kids) {
    const el = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs || {})) {
        if (v === false || v == null) continue;
        if (k === 'class') el.className = v;
        else if (k === 'text') el.textContent = v;
        else if (k.startsWith('on')) el.addEventListener(k.slice(2), v);
        else if (v === true) el.setAttribute(k, '');
        else el.setAttribute(k, v);
    }
    for (const kid of kids.flat(Infinity)) {
        if (kid == null || kid === false) continue;
        el.append(kid.nodeType ? kid : document.createTextNode(String(kid)));
    }
    return el;
}
const P = new URLSearchParams(location.search);
function qs(change) {
    const q = new URLSearchParams(location.search);
    for (const [k, v] of Object.entries(change)) {
        if (v == null || v === '') q.delete(k); else q.set(k, v);
    }
    const s = q.toString();
    return s ? '?' + s : location.pathname.split('/').pop() || './';
}
function store(key, value) {
    try {
        if (value === undefined) return JSON.parse(localStorage.getItem('ores-proto-trades:' + key) || 'null');
        if (value === null) localStorage.removeItem('ores-proto-trades:' + key);
        else localStorage.setItem('ores-proto-trades:' + key, JSON.stringify(value));
    } catch (e) { /* private window: the prototype still works without it */ }
    return null;
}
const NOW = new Date('2026-10-11T16:40:00+01:00');
function relTime(iso) {
    const s = Math.round((NOW - new Date(iso)) / 1000);
    if (s < 60) return 'just now';
    const units = [[86400 * 30, 'month'], [86400, 'day'], [3600, 'hour'], [60, 'minute']];
    for (const [n, name] of units) if (s >= n) { const k = Math.floor(s / n); return k + ' ' + name + (k > 1 ? 's' : '') + ' ago'; }
    return 'just now';
}
function when(iso) { return h('time', { datetime: iso, title: iso.replace('T', ' ') }, relTime(iso)); }
function money(n, ccy) {
    return Number(n).toLocaleString('en-GB', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + (ccy ? ' ' + ccy : '');
}
function chip(text, tone, title) { return h('span', { class: 'chip ' + (tone || ''), title }, text); }
function dash() { return h('span', { class: 'dash' }, '—'); }
function ivalue(v) { return v === '' || v == null ? dash() : v; }

/* ---------- people ---------- */
const PEOPLE = {
    'ada.lovelace': { name: 'Ada Lovelace', initials: 'AL', title: 'Head of Rates Desk' },
    'grace.hopper': { name: 'Grace Hopper', initials: 'GH', title: 'Operations lead' },
    'tomas': { name: 'Tomás Ribeiro', initials: 'TR', title: 'Operations analyst' },
    'priya.natarajan': { name: 'Priya Natarajan', initials: 'PN', title: 'Tenant administrator' },
    'ores.trading.service': { name: 'ores.trading.service', initials: 'OT', service: true },
    'ores.ore.service': { name: 'ores.ore.service', initials: 'OR', service: true }
};
function avatar(username, size) {
    const p = PEOPLE[username] || { initials: '?' };
    return h('span', { class: 'av ' + (size || 's'), title: p.name || username }, p.initials);
}
function person(username) {
    const p = PEOPLE[username];
    if (!p) return h('span', { class: 'muted' }, 'Someone outside your view');
    const label = p.service ? p.name : p.name + ' (' + username + ')';
    return h('span', { class: 'person', 'data-text': label }, avatar(username, 's'),
        p.service ? label : h('a', { href: '#', 'data-ref': 'person:' + username }, label));
}
/* The signed-in person decides what is drawn: a control that is not allowed is not drawn. */
const ROLE = ['trader', 'operations', 'reader'].includes(P.get('as')) ? P.get('as') : 'trader';
const CAN = {
    trader: { book: 1, amend: 1, structure: 1 },
    operations: { event: 1, import: 1, export: 1, amend: 1 },
    reader: {}
}[ROLE];
const ME = { trader: 'ada.lovelace', operations: 'grace.hopper', reader: 'tomas' }[ROLE];

/* ---------- references ----------
 * A value that names another record is a link to that record's page, with the
 * record's flag where it has one. The text a link carries is kept in data-text so
 * that an edit dialog can read the value back. */
const FLAGS = { EUR: '🇪🇺', USD: '🇺🇸', GBP: '🇬🇧', CHF: '🇨🇭', JPY: '🇯🇵', TARGET: '🇪🇺', GBLO: '🇬🇧', USNY: '🇺🇸' };
function ref(kind, key, text, flag) {
    return h('a', { class: 'ref', href: '#', 'data-ref': kind + ':' + key, 'data-text': text || key }, flag ? h('span', { class: 'flag' }, flag) : null, text || key);
}
const ccyRef = code => ref('currency', code, code, FLAGS[code]);
const calendarRef = code => ref('calendar', code, code, FLAGS[code]);
function moneyRef(n, ccy) { return h('span', { 'data-text': money(n, ccy) }, money(n), ' ', ccyRef(ccy)); }
const tradeLink = (code, text) => h('a', { href: '../trade-page/?trade=' + code, 'data-text': text || code }, text || code);
const plain = x => x && x.nodeType ? (x.dataset.text || x.textContent) : x;

/* ---------- reference data ---------- */
const TENANT = 'Acme Corporation';
const PARTY = 'Acme London';
/* The code sets of the book table are in the seed files refdata_book_purpose_types, book_statuses, regulatory_book_types and ledger_feed_types. A book in a sandbox is a virtual book: it has a sandbox_id, no ledger account and is not sweepable. */
const BOOK_ROWS = [
    { code: 'EMEA-RATES', name: 'EMEA Rates', description: 'Interest rate and inflation risk of the London desk', parent: 'Rates and credit', unit: 'EMEA Rates desk', ccy: 'EUR', gl: '1210-RATES', cost: 'CC-4100', status: 'Active', reg: 'Trading', purpose: 'Trading', feed: 'Automatic', sweepable: false, centre: 'GBLO' },
    { code: 'EMEA-FX', name: 'EMEA FX', description: 'Spot, forward and option flow', parent: 'FX flow', unit: 'EMEA FX desk', ccy: 'EUR', gl: '1210-FX', cost: 'CC-4200', status: 'Active', reg: 'Trading', purpose: 'Trading', feed: 'Automatic', sweepable: true, centre: 'GBLO' },
    { code: 'EMEA-CREDIT', name: 'EMEA Credit', description: 'Credit default swaps and bonds', parent: 'Rates and credit', unit: 'EMEA Credit desk', ccy: 'EUR', gl: '1210-CREDIT', cost: 'CC-4300', status: 'Active', reg: 'Banking', purpose: 'Trading', feed: 'Automatic', sweepable: false, centre: 'GBLO' },
    { code: 'EMEA-EQ', name: 'EMEA Equity', description: 'Equity forwards and options', parent: 'Equity', unit: 'EMEA Equity desk', ccy: 'EUR', gl: '1210-EQ', cost: 'CC-4400', status: 'Active', reg: 'Trading', purpose: 'Trading', feed: 'Manual', sweepable: false, centre: 'GBLO' },
    { code: 'EMEA-RATES-2025', name: 'EMEA Rates 2025', description: 'The 2025 rates book, closed at the year end', parent: 'Rates and credit', unit: 'EMEA Rates desk', ccy: 'EUR', gl: '1210-RATES25', cost: 'CC-4100', status: 'Closed', reg: 'Trading', purpose: 'Trading', feed: 'None', sweepable: false, centre: 'GBLO' },
    { code: 'EMEA-CMDTY', name: 'EMEA Commodity', description: 'Frozen while the desk is reviewed', parent: 'Rates and credit', unit: 'EMEA Rates desk', ccy: 'USD', gl: '1210-CMDTY', cost: 'CC-4500', status: 'Frozen', reg: 'Trading', purpose: 'Trading', feed: 'Automatic', sweepable: false, centre: 'USNY' },
    { code: 'VIRT-DRAFT', name: 'Virtual draft book', description: 'Drafts for the rates what-if', parent: 'What-if rates', unit: '', ccy: 'EUR', gl: '', cost: '', status: 'Active', reg: 'Trading', purpose: 'Test', feed: 'None', sweepable: false, centre: 'GBLO', sandbox: 'RATES-WHATIF' },
    { code: 'ORE-EX-SWAPS', name: 'ORE examples: swaps', description: 'The swap examples of the ORE distribution', parent: 'ORE examples', unit: '', ccy: 'USD', gl: '', cost: '', status: 'Active', reg: 'Trading', purpose: 'Test', feed: 'None', sweepable: false, centre: 'USNY', sandbox: 'ORE-SAMPLES' },
    { code: 'ORE-EX-FX', name: 'ORE examples: FX', description: 'The FX examples of the ORE distribution', parent: 'ORE examples', unit: '', ccy: 'USD', gl: '', cost: '', status: 'Active', reg: 'Trading', purpose: 'Test', feed: 'None', sweepable: false, centre: 'USNY', sandbox: 'ORE-SAMPLES' }
];
const BOOKS = BOOK_ROWS.map(b => Object.assign({ virtual: !!b.sandbox }, b));
/* The reasons two trades are joined: [code, description, role of the from end, role of the to end, economic]. */
const LINK_TYPES = [
    ['CloseOut', 'The parties end a trade early, in full or in part', 'original', 'closing', true],
    ['Novation', 'One party transfers its position to a third party, or assigns it', 'transferred', 'new_counterparty', true],
    ['Roll', 'A position is rolled to a new date by closing the trade and booking a replacement', 'rolled', 'replacement', true],
    ['Exercise', 'The holder exercises an option, in full or in part, into a new trade', 'exercised', 'resulting', true],
    ['MisbookingCorrection', 'A trade was booked in error and is cancelled and rebooked', 'correction', 'erroneous_original', true],
    ['Fixing', 'The rate of a non-deliverable forward fixes, so a fixing trade is booked', 'fixed', 'fixing', true],
    ['BookMove', 'A trade closes out and is rebooked into a book with a different legal entity, branch, regulatory book or accounting classification', 'backed_out', 'destination', true],
    ['Wash', 'Risk is routed from the client-facing trade to the book that owns it', 'client_facing', 'risk_bearing', false],
    ['WhatIfCopy', 'A user copies a trade into a sandbox to experiment on it', 'original', 'hypothetical', false],
    ['DealtFrom', 'A hypothetical trade is dealt, so a new actual trade is booked against it', 'hypothetical', 'actual', false],
    ['Hedge', 'A desk records the risk relationship between a hedged trade and its hedge', 'hedged', 'hedging', false],
    ['UserDefined', 'A user records an association between two trades that no other type names', 'either', 'either', false]
];
const linkType = code => LINK_TYPES.find(x => x[0] === code);
/* Links a person recorded by hand. A cancel and rebook writes its own link. */
const HAND_LINKS = [{ from: 'ACME-100201', to: 'ACME-100210', type: 'Hedge' }, { from: 'ACME-100215', to: 'ACME-100230', type: 'UserDefined' }];
const bookOpen = b => !!b && b.status === 'Active';
/* The facts a person needs to choose a book, in one line. */
const bookFacts = b => b ? [b.parent, b.ccy, b.reg + ' book', 'purpose ' + b.purpose, 'ledger feed ' + b.feed.toLowerCase(), 'rates centre ' + b.centre, b.sweepable ? 'sweepable' : null, b.status !== 'Active' ? b.status : null].filter(Boolean).join(' · ') : '';
/* A sandbox is a space where actions have no effect outside it. Its books are virtual books and its portfolios carry its id. */
const SANDBOXES = [
    { code: 'ORE-SAMPLES', name: 'ORE samples', purpose: 'The sample portfolios of the ORE distribution', visibility: 'shared', status: 'open', owner: 'ada.lovelace', review: '2027-01-31', portfolios: ['ORE examples'] },
    { code: 'RATES-WHATIF', name: 'Rates what-ifs', purpose: 'Try a restructure of the rates book without touching official data', visibility: 'private', status: 'open', owner: 'ada.lovelace', review: '2026-12-15', portfolios: ['What-if rates'] }
];
const sandbox = code => SANDBOXES.find(s => s.code === code);
const COUNTERPARTIES = [
    { code: 'NWCAP', name: 'Northwind Capital Ltd', open: true },
    { code: 'MRDN', name: 'Meridian Bank AG', open: true },
    { code: 'CSTL', name: 'Coastal Credit SA', open: true },
    { code: 'HLDN', name: 'Holding North America Inc', open: false }
];
const cpty = code => COUNTERPARTIES.find(c => c.code === code);
const book = code => BOOKS.find(b => b.code === code);
/* The closed sets of the trading tables, as [code, description]. */
const BOOKING_NATURES = [['actual', 'actual — the firm did a deal'], ['test', 'test — the booking exercises the system'], ['hypothetical', 'hypothetical — the booking answers a question']];
const ENTRY_CHANNELS = [['manual', 'manual — a user captured the trade'], ['stp', 'stp — straight-through processing from an upstream system'], ['ecn', 'ecn — an electronic communication network'], ['allocation', 'allocation — an allocation split from a block trade']];
const COUNTERPARTY_SCOPES = [['external', 'external — a party outside the group'], ['inter_entity', 'inter_entity — another legal entity of the same group'], ['intra_entity', 'intra_entity — another book in the same legal entity and branch']];

const TRADE_TYPES_RAW = [["CompositeTrade","Composite Trade","composite","composite_instrument"],["RateDigitalOption","Rate Digital Option","swap",""],["SwaptionStraddle","Swaption Straddle","swap",""],["ForwardVolatilityAgreement","Forward Volatility Agreement","swap",""],["Swap","Interest Rate Swap","swap","rate_instrument"],["CrossCurrencySwap","Cross Currency Swap","swap","rate_instrument"],["ForwardRateAgreement","Forward Rate Agreement","swap","rate_instrument"],["CapFloor","Cap or Floor","swap","rate_instrument"],["Swaption","Swaption","swap","rate_instrument"],["FlexiSwap","Flexi Swap","swap","rate_instrument"],["BalanceGuaranteedSwap","Balance Guaranteed Swap","swap","rate_instrument"],["CallableSwap","Callable Swap","swap","rate_instrument"],["KnockOutSwap","Knock-Out Swap","swap","rate_instrument"],["RiskParticipationAgreement","Risk Participation Agreement","credit","credit_instrument"],["InflationSwap","Inflation Swap","swap","rate_instrument"],["FxForwardVolatilityAgreement","FX Forward Volatility Agreement","fx",""],["FxForward","FX Forward","fx","fx_forward_instrument"],["FxSwap","FX Swap","fx","fx_forward_instrument"],["FxOption","FX Option","fx","fx_vanilla_option_instrument"],["FxDigitalOption","FX Digital Option","fx","fx_digital_option_instrument"],["FxAverageForward","FX Average Forward","fx","fx_asian_forward_instrument"],["FxAsianOption","FX Asian Option","fx",""],["FxBarrierOption","FX Barrier Option","fx","fx_barrier_option_instrument"],["FxDoubleBarrierOption","FX Double Barrier Option","fx","fx_barrier_option_instrument"],["FxEuropeanBarrierOption","FX European Barrier Option","fx","fx_barrier_option_instrument"],["FxWindowBarrierOption","FX Window Barrier Option","fx",""],["FxGenericBarrierOption","FX Generic Barrier Option","fx","fx_barrier_option_instrument"],["FxKIKOBarrierOption","FX Knock-In Knock-Out Barrier Option","fx","fx_barrier_option_instrument"],["FxTouchOption","FX Touch Option","fx","fx_digital_option_instrument"],["FxDoubleTouchOption","FX Double Touch Option","fx","fx_digital_option_instrument"],["FxDigitalBarrierOption","FX Digital Barrier Option","fx","fx_digital_option_instrument"],["FxVarianceSwap","FX Variance Swap","fx","fx_variance_swap_instrument"],["FxPairwiseVarianceSwap","FX Pairwise Variance Swap","fx",""],["FxBasketVarianceSwap","FX Basket Variance Swap","fx",""],["FxAccumulator","FX Accumulator","fx","fx_accumulator_instrument"],["FxTaRF","FX Target Redemption Forward","fx","fx_asian_forward_instrument"],["FxWorstOfBasketSwap","FX Worst-Of Basket Swap","fx",""],["FxBestEntryOption","FX Best Entry Option","fx",""],["FxBasketOption","FX Basket Option","fx",""],["FxRainbowOption","FX Rainbow Option","fx",""],["FxStrikeResettableOption","FX Strike Resettable Option","fx",""],["CreditDefaultSwap","Credit Default Swap","credit","credit_instrument"],["CreditDefaultSwapOption","Credit Default Swap Option","credit","credit_instrument"],["IndexCreditDefaultSwap","Index Credit Default Swap","credit","credit_instrument"],["IndexCreditDefaultSwapOption","Index Credit Default Swap Option","credit","credit_instrument"],["SyntheticCDO","Synthetic CDO","credit","credit_instrument"],["CreditLinkedSwap","Credit Linked Swap","credit","credit_instrument"],["CBO","Collateralised Bond Obligation","credit","credit_instrument"],["BondFutureOption","Bond Future Option","bond",""],["Bond","Fixed Income Bond","bond","bond_instrument"],["ForwardBond","Forward Bond","bond","bond_instrument"],["BondFuture","Bond Future","bond","bond_instrument"],["BondOption","Bond Option","bond","bond_instrument"],["BondRepo","Bond Repo","bond","bond_instrument"],["BondTRS","Bond Total Return Swap","bond","bond_instrument"],["BondPosition","Bond Position","bond","bond_instrument"],["CallableBond","Callable Bond","bond","bond_instrument"],["ConvertibleBond","Convertible Bond","bond","bond_instrument"],["Ascot","Asset Swapped Convertible Option Transaction","bond","bond_instrument"],["EquityAutoDeltaHedgedOption","Equity Auto Delta Hedged Option","equity",""],["EquityForwardVolatilityAgreement","Equity Forward Volatility Agreement","equity",""],["EquityOption","Equity Option","equity","equity_option_instrument"],["EquityFutureOption","Equity Future Option","equity",""],["EquityAsianOption","Equity Asian Option","equity","equity_asian_option_instrument"],["EquityBarrierOption","Equity Barrier Option","equity","equity_barrier_option_instrument"],["EquityDoubleBarrierOption","Equity Double Barrier Option","equity","equity_barrier_option_instrument"],["EquityEuropeanBarrierOption","Equity European Barrier Option","equity","equity_barrier_option_instrument"],["EquityWindowBarrierOption","Equity Window Barrier Option","equity",""],["EquityGenericBarrierOption","Equity Generic Barrier Option","equity",""],["EquityTouchOption","Equity Touch Option","equity","equity_digital_option_instrument"],["EquityDoubleTouchOption","Equity Double Touch Option","equity",""],["EquityDigitalOption","Equity Digital Option","equity","equity_digital_option_instrument"],["EquityForward","Equity Forward","equity","equity_forward_instrument"],["EquitySwap","Equity Swap","equity","equity_swap_instrument"],["EquityVarianceSwap","Equity Variance Swap","equity","equity_variance_swap_instrument"],["EquityPairwiseVarianceSwap","Equity Pairwise Variance Swap","equity",""],["EquityBasketVarianceSwap","Equity Basket Variance Swap","equity",""],["EquityCliquetOption","Equity Cliquet Option","equity","equity_option_instrument"],["EquityAccumulator","Equity Accumulator","equity","equity_accumulator_instrument"],["EquityTaRF","Equity Target Redemption Forward","equity","equity_accumulator_instrument"],["EquityWorstOfBasketSwap","Equity Worst-Of Basket Swap","equity","equity_swap_instrument"],["EquityBestEntryOption","Equity Best Entry Option","equity",""],["EquityBasketOption","Equity Basket Option","equity",""],["EquityRainbowOption","Equity Rainbow Option","equity",""],["EquityOutperformanceOption","Equity Outperformance Option","equity","equity_option_instrument"],["EquityStrikeResettableOption","Equity Strike Resettable Option","equity",""],["TotalReturnSwap","Total Return Swap","equity","composite_instrument"],["ContractForDifference","Contract for Difference","equity","composite_instrument"],["EquityPosition","Equity Position","equity","equity_position_instrument"],["EquityOptionPosition","Equity Option Position","equity","equity_position_instrument"],["CommodityForwardVolatilityAgreement","Commodity Forward Volatility Agreement","commodity","commodity_instrument"],["IntradayPowerForward","Intraday Power Forward","commodity","commodity_instrument"],["CommodityForward","Commodity Forward","commodity","commodity_instrument"],["CommodityOption","Commodity Option","commodity","commodity_instrument"],["CommodityDigitalOption","Commodity Digital Option","commodity","commodity_instrument"],["CommodityDigitalAveragePriceOption","Commodity Digital Average Price Option","commodity","commodity_instrument"],["CommodityAsianOption","Commodity Asian Option","commodity","commodity_instrument"],["CommodityAveragePriceOption","Commodity Average Price Option","commodity","commodity_instrument"],["CommoditySpreadOption","Commodity Spread Option","commodity","commodity_instrument"],["CommodityOptionStrip","Commodity Option Strip","commodity","commodity_instrument"],["CommoditySwap","Commodity Swap","commodity","commodity_instrument"],["CommoditySwaption","Commodity Swaption","commodity","commodity_instrument"],["CommodityVarianceSwap","Commodity Variance Swap","commodity","commodity_instrument"],["CommodityPairwiseVarianceSwap","Commodity Pairwise Variance Swap","commodity","commodity_instrument"],["CommodityBasketVarianceSwap","Commodity Basket Variance Swap","commodity","commodity_instrument"],["CommodityAccumulator","Commodity Accumulator","commodity","commodity_instrument"],["CommodityTaRF","Commodity Target Redemption Forward","commodity","commodity_instrument"],["CommodityWorstOfBasketSwap","Commodity Worst-Of Basket Swap","commodity","commodity_instrument"],["CommodityBestEntryOption","Commodity Best Entry Option","commodity","commodity_instrument"],["CommodityWindowBarrierOption","Commodity Window Barrier Option","commodity","commodity_instrument"],["CommodityGenericBarrierOption","Commodity Generic Barrier Option","commodity","commodity_instrument"],["CommodityBasketOption","Commodity Basket Option","commodity","commodity_instrument"],["CommodityRainbowOption","Commodity Rainbow Option","commodity","commodity_instrument"],["CommodityStrikeResettableOption","Commodity Strike Resettable Option","commodity","commodity_instrument"],["CommodityPosition","Commodity Position","commodity","commodity_instrument"],["CashPosition","Cash Position","equity",""],["ScriptedTrade","Scripted Trade (ORE generic payoff engine)","scripted","scripted_instrument"],["Autocallable_01","Autocallable (variant 01)","scripted","scripted_instrument"],["DoubleDigitalOption","Double Digital Option","scripted","scripted_instrument"],["EuropeanOptionBarrier","European Option with Barrier","scripted",""],["PerformanceOption_01","Performance Option (variant 01)","scripted","scripted_instrument"]];
const TRADE_TYPES = TRADE_TYPES_RAW.map(([code, description, product, instrument]) => ({ code, description, product, instrument }));
const tradeType = code => TRADE_TYPES.find(t => t.code === code);
const PRODUCTS = ['swap', 'fx', 'credit', 'bond', 'equity', 'composite'];

/* The four axes an activity type sits on. Codes marked * are in the model's
 * documentation; the rest are illustrative. */
/* The activity types the trading tables seed. The label is the code in words. */
const ACTIVITY_TYPES = [
    { code: 'new_booking', label: 'New booking', category: 'new_activity', economic: true, confirmable: true, real: true, priority: 9, description: 'Initial booking of a new trade.' },
    { code: 'new_internal_transfer', label: 'New internal transfer', category: 'new_activity', economic: true, confirmable: false, real: true, priority: 9, description: 'Internal transfer creating a new trade record.' },
    { code: 'draft_capture', label: 'Draft capture', category: 'new_activity', economic: false, confirmable: false, real: true, priority: 9, description: 'Capture of a trade the desk has not yet executed. The trade starts in draft and carries no market risk until it is executed.' },
    { code: 'execution', label: 'Execution', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Execution of a captured draft. The trade becomes live and starts to carry market risk.' },
    { code: 'amendment', label: 'Amendment', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Change to trade economics or contractual terms.' },
    { code: 'null_amend', label: 'Null amend', category: 'lifecycle_event', economic: false, confirmable: false, real: false, priority: 9, description: 'Amendment whose entered values equal the values held. Recorded for the audit trail; it versions nothing and raises no confirmation.' },
    { code: 'confirmation', label: 'Confirmation', category: 'lifecycle_event', economic: false, confirmable: false, real: true, priority: 9, description: 'Trade confirmed by both counterparties. Confirmation is its own machine: this moves the confirmation status, not the lifecycle.' },
    { code: 'novation', label: 'Novation', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Transfer of a trade to a new counterparty.' },
    { code: 'partial_termination', label: 'Partial termination', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 4, description: 'Reduction of notional on a live trade.' },
    { code: 'full_termination', label: 'Full termination', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 4, description: 'Full early termination of a live trade.' },
    { code: 'maturity', label: 'Maturity', category: 'lifecycle_event', economic: true, confirmable: false, real: true, priority: 6, description: 'Trade has reached its contractual maturity date.' },
    { code: 'exercise', label: 'Exercise', category: 'lifecycle_event', economic: true, confirmable: false, real: true, priority: 6, description: 'Option exercise event.' },
    { code: 'assignment', label: 'Assignment', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Assignment of trade rights to another party.' },
    { code: 'increase', label: 'Increase', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Increase of notional on a live trade.' },
    { code: 'decrease', label: 'Decrease', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 4, description: 'Decrease of notional on a live trade (partial unwind).' },
    { code: 'roll', label: 'Roll', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Extension of maturity date (roll or extension).' },
    { code: 'compression', label: 'Compression', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 4, description: 'Portfolio compression: partial termination of offsetting trades.' },
    { code: 'clearing', label: 'Clearing', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 9, description: 'Trade submitted for central clearing.' },
    { code: 'netting', label: 'Netting', category: 'lifecycle_event', economic: true, confirmable: true, real: true, priority: 4, description: 'Netting of offsetting positions.' },
    { code: 'rate_reset', label: 'Rate reset', category: 'valuation_change', economic: true, confirmable: false, real: true, priority: 7, description: 'Floating rate fixing or reset event.' },
    { code: 'fx_reset', label: 'Fx reset', category: 'valuation_change', economic: true, confirmable: false, real: true, priority: 7, description: 'FX rate reset for cross-currency trades.' },
    { code: 'accrual', label: 'Accrual', category: 'valuation_change', economic: false, confirmable: false, real: true, priority: 9, description: 'Accrual period end-of-day valuation update.' },
    { code: 'cashflow', label: 'Cashflow', category: 'valuation_change', economic: false, confirmable: false, real: true, priority: 9, description: 'Scheduled cashflow payment event.' },
    { code: 'correction', label: 'Correction', category: 'misbooking', economic: false, confirmable: false, real: true, priority: 8, description: 'Correction of a data entry error; no economic change.' },
    { code: 'rebook', label: 'Rebook', category: 'misbooking', economic: true, confirmable: true, real: true, priority: 8, description: 'Cancellation and re-booking of a misbooking.' },
    { code: 'maturity_reversal', label: 'Maturity reversal', category: 'misbooking', economic: true, confirmable: false, real: true, priority: 6, description: 'Reversal of a maturity applied in error. The trade returns to live.' },
    { code: 'cancellation_reversal', label: 'Cancellation reversal', category: 'misbooking', economic: true, confirmable: false, real: true, priority: 9, description: 'Reversal of a cancellation applied in error. The trade returns to live.' },
    { code: 'transfer', label: 'Transfer', category: 'misbooking', economic: false, confirmable: false, real: true, priority: 1, description: 'Transfer of a trade between books within the firm.' },
    { code: 'split', label: 'Split', category: 'misbooking', economic: true, confirmable: true, real: true, priority: 9, description: 'Split of a trade into two or more trades.' },
    { code: 'merge', label: 'Merge', category: 'misbooking', economic: true, confirmable: true, real: true, priority: 9, description: 'Merge of two or more trades into one.' },
    { code: 'cancel', label: 'Cancel', category: 'cancellation', economic: true, confirmable: false, real: true, priority: 9, description: 'Cancellation of a new trade before confirmation.' },
    { code: 'cancel_confirmed', label: 'Cancel confirmed', category: 'cancellation', economic: true, confirmable: false, real: true, priority: 9, description: 'Cancellation of a live (confirmed) trade.' },
    { code: 'void', label: 'Void', category: 'cancellation', economic: true, confirmable: false, real: true, priority: 9, description: 'Void of a trade that should never have been booked.' }
];
const activityType = code => ACTIVITY_TYPES.find(a => a.code === code);
/* The change reasons the data quality tables seed, as [code, description, for new, for amend, for delete]. */
const REASON_CODES = [
    ['system.new_record', 'New record created during normal operations', true, false, false],
    ['system.update', 'Existing record changed during normal operations', false, true, false],
    ['system.import', 'Data imported via CLI import command', true, true, false],
    ['common.non_material_update', 'Non-material update (Touch)', false, true, false],
    ['common.rectification', 'User/Booking Error', false, true, true],
    ['common.duplicate', 'Duplicate Record', false, false, true],
    ['common.stale_data', 'Data not updated within required liquidity horizon', false, true, true],
    ['common.outlier_correction', 'Manual override after plausibility check failure', false, true, true],
    ['common.feed_failure', 'Upstream vendor/API data issue', false, true, true],
    ['common.mapping_error', 'Incorrect ID translation (e.g., ISIN to FIGI)', false, true, true],
    ['common.judgmental_override', 'Expert judgment when market prices unavailable', false, true, true],
    ['common.regulatory', 'Mandatory compliance adjustment', false, true, true],
    ['common.other', 'Exceptional (requires audit note)', false, true, true],
    ['trade.fat_finger', 'Erroneous execution (wrong quantity/price)', false, true, true],
    ['trade.system_malfunction', 'Technical glitch or algorithm issue', false, true, true],
    ['trade.corporate_action', 'Stock split, dividend, or merger adjustment', false, true, true],
    ['trade.allocation_swap', 'House to client sub-account reallocation', false, true, true],
    ['trade.re_booking', 'Wrong legal entity correction', false, true, true],
    ['trade.other', 'Exceptional (requires audit note)', false, true, true]
];
/* The reasons that apply to a new record, an amendment or a deletion, as [code, description]. */
const reasonsFor = kind => REASON_CODES.filter(r => r[{ new: 2, amend: 3, delete: 4 }[kind]]).map(r => [r[0], r[1]]);

/* ---------- the trades ---------- */
/* Seven trades are written out by hand because the journeys walk through them.
 * The rest are generated so that the list has a size worth paging. */
const FEATURED = [
    { n: 1, type: 'Swap', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', date: '2026-09-14', notional: 50000000, ccy: 'EUR', ext: 3, ver: 5, structure: 'PKG-0004' },
    { n: 2, type: 'FxOption', cp: 'MRDN', book: 'EMEA-FX', state: 'live', date: '2026-10-08', notional: 10000000, ccy: 'EUR', ext: 1, ver: 2, structure: 'STR-0007' },
    { n: 3, type: 'Bond', cp: 'CSTL', book: 'EMEA-CREDIT', state: 'live', date: '2026-08-02', notional: 25000000, ccy: 'EUR', ext: 1, ver: 1 },
    { n: 4, type: 'BondRepo', cp: 'MRDN', book: 'EMEA-CREDIT', state: 'live', date: '2026-09-30', notional: 20000000, ccy: 'EUR', ext: 2, ver: 3, structure: 'PKG-0021' },
    { n: 5, type: 'CompositeTrade', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', date: '2026-10-05', notional: 30000000, ccy: 'EUR', ext: 1, ver: 1 },
    { n: 6, type: 'Swap', cp: 'CSTL', book: 'VIRT-DRAFT', state: 'draft', date: '2026-10-10', notional: 5000000, ccy: 'EUR', ext: 1, ver: 2, nature: 'hypothetical' },
    { n: 7, type: 'FxForward', cp: 'NWCAP', book: 'EMEA-FX', state: 'cancelled', date: '2026-07-21', notional: 8000000, ccy: 'GBP', ext: 2, ver: 4, rebookedTo: 'ACME-100208' },
    { n: 8, type: 'FxForward', cp: 'MRDN', book: 'EMEA-FX', state: 'live', date: '2026-10-09', notional: 8000000, ccy: 'GBP', ext: 1, ver: 1, rebookedFrom: 'ACME-100207', structure: 'STR-0010' },
    { n: 9, type: 'FxOption', cp: 'MRDN', book: 'EMEA-FX', state: 'live', date: '2026-10-08', notional: 10000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0007' },
    { n: 11, type: 'Swap', cp: 'CSTL', book: 'EMEA-RATES', state: 'live', date: '2026-09-02', notional: 40000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0009' },
    { n: 12, type: 'Swaption', cp: 'CSTL', book: 'EMEA-RATES', state: 'live', date: '2026-09-02', notional: 40000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0009' },
    { n: 13, type: 'Bond', cp: 'MRDN', book: 'EMEA-CREDIT', state: 'live', date: '2026-08-20', notional: 15000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'PKG-0021' },
    { n: 10, type: 'CapFloor', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', date: '2026-10-05', notional: 30000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'PKG-0004' }
];
const USED_TYPES = ['Swap', 'CrossCurrencySwap', 'Swaption', 'CapFloor', 'FxForward', 'FxOption', 'FxBarrierOption', 'Bond', 'BondRepo', 'BondOption',
    'CallableBond', 'EquityOption', 'EquityForward', 'CreditDefaultSwap', 'CompositeTrade'];
const BOOK_FOR = { swap: 'EMEA-RATES', fx: 'EMEA-FX', credit: 'EMEA-CREDIT', bond: 'EMEA-CREDIT', equity: 'EMEA-EQ', composite: 'EMEA-RATES' };
const CCYS = ['EUR', 'USD', 'GBP', 'CHF', 'JPY'];
const CALENDARS = ['TARGET', 'GBLO', 'USNY'];
const OPTION_TYPES = [['Call', 'The holder may buy the underlying'], ['Put', 'The holder may sell the underlying']];
const EXERCISE_STYLES = [['European', 'Exercisable only at maturity'], ['Bermudan', 'Exercisable on a set of dates'], ['American', 'Exercisable at any time up to maturity']];
const SETTLEMENT_TYPES = [['Physical', 'Settlement delivers the underlying'], ['Cash', 'Settlement pays the cash value']];
/* The columns of the instrument tables that have no card of their own: [name, type, required]. The type is t text, n number, d date, b boolean, i integer.
 * The code sets are the soft foreign keys of the trading and refdata seeds. */
const INSTRUMENT_COLUMNS = {
    commodity_instrument: [['commodity_code','t',1],['currency','t',1],['quantity','n',1],['unit','t',1],['start_date','d',0],['maturity_date','d',0],['fixed_price','n',0],['option_type','t',0],['strike_price','n',0],['exercise_type','t',0],['average_type','t',0],['averaging_start_date','d',0],['averaging_end_date','d',0],['spread_commodity_code','t',0],['spread_amount','n',0],['strip_frequency_code','t',0],['variance_strike','n',0],['accumulation_amount','n',0],['knock_out_barrier','n',0],['barrier_type','t',0],['lower_barrier','n',0],['upper_barrier','n',0],['day_count_fraction_code','t',0],['payment_frequency_code','t',0],['swaption_expiry_date','d',0],['description','t',0]],
    credit_instrument: [['reference_entity','t',1],['start_date','d',0],['maturity_date','d',0],['description','t',0]],
    equity_accumulator_instrument: [['underlying_name','t',1],['currency','t',1],['strike','n',1],['fixing_amount','n',1],['start_date','d',1],['expiry_date','d',1],['fixing_frequency','t',1],['long_short','t',1],['knock_out_level','n',0],['range_bounds_json','t',0],['target_amount','n',0],['target_type','t',0],['payoff_type','t',1],['description','t',0]],
    equity_asian_option_instrument: [['underlying_name','t',1],['currency','t',1],['notional','n',1],['option_type','t',1],['strike','n',1],['expiry_date','d',1],['exercise_type','t',1],['long_short','t',1],['average_type','t',1],['averaging_start_date','d',1],['averaging_end_date','d',1],['description','t',0]],
    equity_barrier_option_instrument: [['underlying_name','t',1],['currency','t',1],['notional','n',1],['option_type','t',1],['strike','n',1],['expiry_date','d',1],['exercise_type','t',1],['long_short','t',1],['lower_barrier','n',1],['lower_barrier_type','t',1],['upper_barrier','n',0],['upper_barrier_type','t',0],['rebate','n',0],['description','t',0]],
    equity_digital_option_instrument: [['underlying_name','t',1],['currency','t',1],['notional','n',1],['option_type','t',0],['strike','n',0],['barrier_level','n',0],['barrier_type','t',0],['expiry_date','d',1],['long_short','t',1],['payout_amount','n',0],['description','t',0]],
    equity_forward_instrument: [['underlying_name','t',1],['currency','t',1],['quantity','n',1],['forward_price','n',0],['expiry_date','d',1],['long_short','t',1],['settlement_type','t',0],['description','t',0]],
    equity_option_instrument: [['underlying_name','t',1],['currency','t',1],['notional','n',1],['option_type','t',1],['strike','n',1],['expiry_date','d',1],['exercise_type','t',1],['long_short','t',1],['settlement_type','t',0],['cliquet_frequency','t',0],['description','t',0]],
    equity_position_instrument: [['underlying_name','t',1],['currency','t',1],['quantity','n',1],['price','n',0],['description','t',0]],
    equity_swap_instrument: [['underlying_name','t',0],['basket_json','t',0],['initial_prices_json','t',0],['knock_out_levels_json','t',0],['fixed_trigger_levels_json','t',0],['currency','t',1],['notional','n',1],['return_type','t',1],['start_date','d',1],['maturity_date','d',1],['long_short','t',1],['payment_frequency_code','t',1],['description','t',0]],
    equity_variance_swap_instrument: [['underlying_name','t',1],['currency','t',1],['notional','n',1],['variance_strike','n',1],['start_date','d',1],['maturity_date','d',1],['long_short','t',1],['description','t',0]],
    fx_accumulator_instrument: [['currency','t',1],['fixing_amount','n',1],['strike','n',1],['underlying_code','t',1],['long_short','t',1],['start_date','d',1],['knock_out_barrier','n',0],['description','t',0]],
    fx_asian_forward_instrument: [['fx_index','t',1],['reference_currency','t',0],['reference_notional','n',0],['settlement_currency','t',0],['settlement_notional','n',0],['payment_date','d',0],['long_short','t',0],['currency','t',0],['fixing_amount','n',0],['target_amount','n',0],['strike','n',0],['description','t',0]],
    fx_barrier_option_instrument: [['bought_currency','t',1],['bought_amount','n',1],['sold_currency','t',1],['sold_amount','n',1],['option_type','t',0],['expiry_date','d',1],['settlement','t',0],['barrier_type','t',1],['lower_barrier','n',0],['upper_barrier','n',0],['underlying_code','t',0],['description','t',0]],
    fx_digital_option_instrument: [['foreign_currency','t',1],['domestic_currency','t',1],['payoff_currency','t',1],['payoff_amount','n',1],['option_type','t',0],['expiry_date','d',1],['long_short','t',1],['strike','n',0],['barrier_type','t',0],['lower_barrier','n',0],['upper_barrier','n',0],['description','t',0]],
    fx_forward_instrument: [['bought_currency','t',1],['bought_amount','n',1],['sold_currency','t',1],['sold_amount','n',1],['value_date','d',1],['settlement','t',0],['description','t',0]],
    fx_variance_swap_instrument: [['start_date','d',1],['end_date','d',1],['currency','t',1],['underlying_code','t',1],['long_short','t',1],['strike','n',1],['notional','n',1],['moment_type','t',1],['description','t',0]],
    scripted_instrument: [['script_name','t',1],['script_body','t',0],['events_json','t',0],['underlyings_json','t',0],['parameters_json','t',0],['description','t',0]],
};
const COLUMN_SETS = {
    option_type: OPTION_TYPES.map(x => x[0]), exercise_type: EXERCISE_STYLES.map(x => x[0]), settlement: SETTLEMENT_TYPES.map(x => x[0]), settlement_type: SETTLEMENT_TYPES.map(x => x[0]),
    long_short: ['Long', 'Short'], average_type: ['Arithmetic', 'Geometric'], return_type: ['Total', 'Price'], moment_type: ['Variance', 'Volatility'],
    payoff_type: ['Accumulator', 'Asian', 'AverageStrike', 'Decumulator', 'TargetExact', 'TargetFull', 'Vanilla'], target_type: ['TargetFull', 'TargetExact'],
    barrier_type: ['UpAndOut', 'UpAndIn', 'DownAndOut', 'DownAndIn', 'KnockIn', 'KnockOut', 'CumulatedProfitCap', 'CumulatedProfitCapPoints', 'FixingCap', 'FixingFloor'],
    payment_frequency_code: ['Once', 'Annual', 'Semiannual', 'Quarterly', 'Bimonthly', 'Monthly', 'Lunarmonth', 'Weekly', 'Daily'],
    day_count_fraction_code: ['A360', 'A365F', 'A365', 'ActAct(ISDA)', 'ActAct(ISMA)', 'ActAct(AFB)', '30/360', '30E/360', '30E/360(ISDA)', 'Business252', '1/1', 'Simple']
};
COLUMN_SETS.lower_barrier_type = COLUMN_SETS.upper_barrier_type = COLUMN_SETS.barrier_type;
COLUMN_SETS.strip_frequency_code = COLUMN_SETS.payment_frequency_code;
const COLUMN_VALUES = {
    commodity_code: 'WTI', unit: 'BBL', underlying_name: 'SX5E', underlying_code: 'EURUSD', reference_entity: 'Acme Holdings plc', fx_index: 'ECB/EURUSD', script_name: 'Autocallable',
    fixing_frequency: 'Monthly', cliquet_frequency: 'Annual', option_type: 'Call', exercise_type: 'European', long_short: 'Long', settlement_type: 'Cash', settlement: 'Cash',
    barrier_type: 'UpAndOut', lower_barrier_type: 'DownAndOut', average_type: 'Arithmetic', return_type: 'Total', moment_type: 'Variance', payoff_type: 'Accumulator',
    payment_frequency_code: 'Quarterly', quantity: 10000, strike: '1.0842', price: '102.5000'
};
function columnCard(t, table) {
    const label = k => k === 'long_short' ? 'Long or short' : k.replace(/_/g, ' ').replace(/ code$/, '').replace(/^./, c => c.toUpperCase());
    const fields = INSTRUMENT_COLUMNS[table].map(([k, ty, req]) => {
        const o = { economic: k !== 'description', optional: !req };
        let type = 'text', value = '';
        if (k === 'description') value = '';
        else if (/(^|_)currency$/.test(k)) { type = 'ccy'; value = req || /^(bought|sold)/.test(k) ? (/^sold/.test(k) ? 'USD' : t.ccy) : ''; }
        else if (ty === 'd') { type = 'date'; value = req ? (/start|averaging_start/.test(k) ? t.date : '2027-01-15') : ''; }
        else if (ty === 'b') { o.options = ['Yes', 'No']; value = req ? 'No' : ''; }
        else if (ty === 'n') {
            type = /amount|notional|quantity/.test(k) ? 'money' : 'decimal';
            value = req ? (k === 'notional' ? t.notional : k in COLUMN_VALUES ? (table.startsWith('equity') && /strike|price/.test(k) ? '4500.0000' : COLUMN_VALUES[k]) : /amount/.test(k) ? t.notional : table.startsWith('equity') ? '4500.0000' : '1.0842') : '';
        } else if (req || COLUMN_VALUES[k]) value = req ? (COLUMN_VALUES[k] || (/_json$/.test(k) ? '[]' : label(k))) : '';
        if (COLUMN_SETS[k]) { o.options = COLUMN_SETS[k]; }
        return { key: k, label: label(k), value, type, ...o };
    });
    return { id: 'instrument', title: tradeTypeName(table), table, version: t.ver, fields };
}
const BOND_ISSUES = ['Acme 3.25 % 2031', 'Acme 2.10 % 2029'];

function buildTrades() {
    const trades = [];
    const open = COUNTERPARTIES.filter(c => c.open);
    for (let i = 1; i <= 38; i++) {
        const f = FEATURED.find(x => x.n === i) || {};
        const type = f.type || USED_TYPES[(i * 7) % USED_TYPES.length];
        const tt = tradeType(type);
        let state = f.state || 'live';
        if (!f.state) { if (i % 9 === 0) state = 'draft'; else if (i % 13 === 0) state = 'cancelled'; else if (i % 11 === 0) state = 'expired'; }
        const day = new Date(NOW.getTime() - (i * 3.1) * 86400000).toISOString().slice(0, 10);
        trades.push({
            id: tradeId(i), code: 'ACME-' + (100200 + i), type, product: tt.product, table: tt.instrument,
            cp: f.cp || open[(i * 3) % open.length].code,
            book: f.book || BOOK_FOR[tt.product],
            state,
            date: f.date || day,
            notional: f.notional || (1 + (i * 37) % 20) * 1000000,
            ccy: f.ccy || CCYS[i % CCYS.length],
            ext: f.ext || 1 + (i % 3), ver: f.ver || 1 + (i % 3) + (i % 2),
            nature: f.nature || 'actual', channel: i % 5 === 0 ? 'stp' : 'manual',
            scope: 'external', structure: f.structure || null, rebookedTo: f.rebookedTo || null, rebookedFrom: f.rebookedFrom || null,
            updated: new Date(NOW.getTime() - (i * 5.3) * 3600000).toISOString().slice(0, 19) + '+01:00'
        });
    }
    return trades;
}
function tradeId(i) {
    const h8 = (i * 2654435761 >>> 0).toString(16).padStart(8, '0');
    return h8 + '-3b2d-4a91-8f6e-' + (i * 7919).toString(16).padStart(12, '0');
}
/* ---------- structures ----------
 * A structure holds trades under a kind. A template fixes the roles and how many legs each holds.
 * Members are trades of one counterparty. Booking and grouping write the same store. */
const STRUCTURE_KINDS = {
    Strategy: { whole: true, template: true, desc: 'One product family, one pricing model, fixed leg roles.' },
    Typed: { whole: true, template: true, desc: 'Legs from different families, with roles fixed by the type.' },
    Dynamic: { whole: true, template: false, desc: 'Roles read from data at run time, as a scripted product does.' },
    Package: { whole: false, template: false, desc: 'A container. Nothing economic binds the legs, so it has no template and any role.' }
};
const STRUCTURE_TEMPLATES = { Strategy: { Straddle: [['leg', 2, 2]], RiskReversal: [['long', 1, 1], ['short', 1, 1]], Butterfly: [['body', 1, 1], ['wing', 2, 2]], CalendarSpread: [['near', 1, 1], ['far', 1, 1]] }, Typed: { CallableSwap: [['swap', 1, 1], ['call', 1, 1]] } };
const BASE_DEALS = {
    'STR-0010': { kind: 'Dynamic', template: '', cp: 'MRDN', parent: '', members: [['ACME-100208', 'hedge']], created: '2026-08-20T14:00:00+01:00', by: 'ada.lovelace' },
    'STR-0007': { kind: 'Strategy', template: 'RiskReversal', cp: 'MRDN', parent: 'STR-0010', members: [['ACME-100202', 'long'], ['ACME-100209', 'short']], created: '2026-10-08T09:05:00+01:00', by: 'ada.lovelace' },
    'PKG-0021': { kind: 'Package', template: '', cp: 'MRDN', parent: 'STR-0010', members: [['ACME-100204', 'leg'], ['ACME-100213', 'leg']], created: '2026-08-20T14:05:00+01:00', by: 'ada.lovelace' },
    'STR-0009': { kind: 'Typed', template: 'CallableSwap', cp: 'CSTL', parent: '', members: [['ACME-100211', 'swap'], ['ACME-100212', 'call']], created: '2026-09-02T10:30:00+01:00', by: 'ada.lovelace' },
    'PKG-0004': { kind: 'Package', template: '', cp: 'NWCAP', parent: '', members: [['ACME-100201', 'leg'], ['ACME-100210', 'leg']], created: '2026-10-05T11:20:00+01:00', by: 'ada.lovelace' }
};
/* The legs of the structure STR-0007 differ in their option type and strike. Every other option is a call at the market strike. */
const OPTION_LEGS = { 'ACME-100202': { type: 'Call', strike: '1.0900' }, 'ACME-100209': { type: 'Put', strike: '1.0700' } };
const allDeals = () => Object.assign({}, BASE_DEALS, store('structures') || {});
const templateRoles = (kind, template) => template && STRUCTURE_TEMPLATES[kind] ? STRUCTURE_TEMPLATES[kind][template] || null : null;
/* The roles of a template that still need a leg, then the ones that may hold another. */
function openRoles(deal) {
    const rule = templateRoles(deal.kind, deal.template);
    if (!rule) return [];
    const n = r => deal.members.filter(m => m[1] === r).length;
    const need = rule.filter(([r, lo]) => n(r) < lo).map(r => r[0]);
    return need.length ? need : rule.filter(([r, , hi]) => !hi || n(r) < hi).map(r => r[0]);
}
function nextStructureId(kind) {
    const prefix = kind === 'Package' ? 'PKG-' : 'STR-';
    return prefix + String(100 + Object.keys(allDeals()).length + 1).padStart(4, '0');
}

let TRADES = buildTrades().concat(store('booked') || []);
const trade = code => TRADES.find(t => t.code === code || t.id === code);

/* A credit trade states its legs in the shared swap leg tables, and an option on it adds a premium and an exercise price. */
function creditChildren(t, d, rows) {
    const out = [{
        id: 'legs', title: 'Legs', table: 'swap_leg', version: t.ver, child: true,
        ...rows(['Leg', 'Payer', 'Leg type', 'Currency', 'Day count', 'Business day convention', 'Frequency'], [
            ['1', 'No', 'Fixed', ccyRef(t.ccy), 'A360', 'ModifiedFollowing', 'Quarterly']]), economic: true
    }, {
        id: 'leg_rates', title: 'Leg rates', table: 'swap_leg_rate', version: t.ver, child: true,
        ...rows(['Leg', 'Rate role', 'Sequence', 'Start date', 'Value'], [['1', 'fixed', '1', d, '0.010000']]), economic: true
    }];
    if (/Option$/.test(t.type)) out.push({
        id: 'premium', title: 'Premium', table: 'instrument_option_premium', version: 1, child: true,
        ...rows(['Sequence', 'Amount', 'Currency', 'Pay date'], [['1', money(75000), ccyRef(t.ccy), d]]), economic: true
    });
    return out;
}
/* ---------- the fields of an instrument ----------
 * economic:true is the model's declaration (:economic: true on the column). The
 * digest folds only those. digestGap marks the two tables the digest does not
 * read yet; ?digest=fixed shows them folded. */
const DIGEST_FIXED = P.get('digest') === 'fixed';
function instrumentCards(t) {
    const f = (key, label, value, o = {}) => Object.assign({ key, label, value, economic: false, type: 'text' }, o);
    const E = { economic: true };
    const rows = (columns, data, o = {}) => Object.assign({ columns, rows: data }, o);
    const d = t.date;
    switch (t.table) {
        case 'rate_instrument':
            return [{
                id: 'instrument', title: 'Interest rate instrument', table: 'rate_instrument', version: t.ver,
                fields: [
                    f('start_date', 'Start date', d, { ...E, type: 'date' }), f('maturity_date', 'Maturity date', '2031-09-14', { ...E, type: 'date' }),
                    f('description', 'Internal description', 'Five year receiver, hedges the Q3 bond issue', { optional: true })
                ]
            }, {
                id: 'legs', title: 'Legs', table: 'swap_leg', version: t.ver, child: true,
                ...rows(['Leg', 'Payer', 'Leg type', 'Currency', 'Day count', 'Business day convention', 'Frequency', 'Floating index'], [
                    ['1', 'Yes', 'Fixed', ccyRef(t.ccy), '30/360', 'ModifiedFollowing', 'Annual', ''],
                    ['2', 'No', 'Floating', ccyRef(t.ccy), 'A360', 'ModifiedFollowing', 'Semiannual', 'EUR-EURIBOR-6M']]), economic: true
            }, {
                id: 'leg_amounts', title: 'Leg amounts', table: 'swap_leg_amount', version: t.ver, child: true,
                ...rows(['Leg', 'Sequence', 'Start date', 'Amount'], [['1', '1', d, money(t.notional)], ['2', '1', d, money(t.notional)]]), economic: true
            }, {
                id: 'leg_rates', title: 'Leg rates', table: 'swap_leg_rate', version: t.ver, child: true,
                ...rows(['Leg', 'Rate role', 'Sequence', 'Start date', 'Value'], [['1', 'fixed', '1', d, '0.028500'], ['2', 'spread', '1', d, '0.000000']]), economic: true
            }];
        case 'fx_vanilla_option_instrument': {
            const ol = OPTION_LEGS[t.code] || { type: 'Call', strike: '1.0842' };
            return [{
                id: 'instrument', title: 'FX vanilla option', table: 'fx_vanilla_option_instrument', version: t.ver,
                fields: [
                    f('option_type', 'Option type', ol.type, { ...E, options: OPTION_TYPES.map(x => x[0]) }), f('bought_currency', 'Bought currency', 'EUR', { ...E, type: 'ccy' }),
                    f('bought_amount', 'Bought amount', t.notional, { ...E, type: 'money' }), f('sold_currency', 'Sold currency', 'USD', { ...E, type: 'ccy' }),
                    f('sold_amount', 'Sold amount', t.notional * 1.0842, { ...E, type: 'money' }),
                    f('expiry_date', 'Expiry date', '2027-01-15', { ...E, type: 'date' }),
                    f('exercise_style', 'Exercise style', 'European', { ...E, options: EXERCISE_STYLES.map(x => x[0]) }),
                    f('settlement', 'Settlement', 'Cash', { ...E, options: SETTLEMENT_TYPES.map(x => x[0]), optional: true }),
                    f('description', 'Internal description', 'Client hedge', { optional: true })
                ]
            }, {
                id: 'premium', title: 'Premium', table: 'instrument_option_premium', version: 1, child: true,
                ...rows(['Sequence', 'Amount', 'Currency', 'Pay date'], [['1', money(112500), ccyRef('USD'), '2026-10-10']]), economic: true
            }, {
                id: 'exercise_prices', title: 'Exercise prices', table: 'instrument_option_exercise_price', version: 1, child: true,
                ...rows(['Sequence', 'Exercise date', 'Price'], [['1', '2027-01-15', ol.strike]]), economic: true, digestGap: !DIGEST_FIXED
            }];
        }
        case 'bond_instrument': {
            const role = { BondRepo: 'repo', BondTRS: 'trs_funding', Ascot: 'ascot_swap' }[t.type];
            return [{
                id: 'instrument', title: 'Bond', table: 'bond_instrument', version: t.ver,
                fields: [
                    f('issue', 'Bond issue', 'Acme 3.25 % 2031', { ref: 'bond_issue', options: BOND_ISSUES }), f('notional', 'Notional', t.notional, { ...E, type: 'money', optional: true })
                ]
            }].concat(role ? [{
                id: 'legs', title: 'Legs', table: 'bond_leg', version: t.ver, child: true,
                ...rows(['Leg role', 'Leg', 'Payer', 'Leg type', 'Currency', 'Day counter'], [[role, '1', 'No', 'Fixed', ccyRef(t.ccy), 'A360']]), economic: true
            }] : []);
        }
        case 'composite_instrument':
            return [{
                id: 'instrument', title: 'Composite trade', table: 'composite_instrument', version: t.ver,
                fields: [f('description', 'Internal description', 'Receiver swap and cap as one package', { optional: true })]
            }, {
                id: 'composite_legs', title: 'Legs', table: 'composite_leg', version: t.ver, child: true,
                ...rows(['Leg sequence', 'Constituent trade'], [['1', tradeLink('ACME-100201', 'ACME-100201')], ['2', tradeLink('ACME-100210', 'ACME-100210')]]),
                economic: true, digestGap: !DIGEST_FIXED
            }];
        default:
            if (INSTRUMENT_COLUMNS[t.table]) return [columnCard(t, t.table)].concat(
                t.table === 'credit_instrument' ? creditChildren(t, d, rows) : []);
            return [{
                id: 'instrument', title: t.table ? tradeTypeName(t.table) : 'Instrument', table: t.table || '(none)', version: t.ver,
                fields: [
                    f('notional', 'Notional', t.notional, { ...E, type: 'money' }), f('currency', 'Currency', t.ccy, { ...E, type: 'ccy' }),
                    f('start_date', 'Start date', t.date, { ...E, type: 'date' }), f('end_date', 'End date', '2028-' + t.date.slice(5), { ...E, type: 'date' }),
                    f('description', 'Internal description', '', { optional: true })
                ]
            }];
    }
}
function tradeTypeName(table) { return table.replace(/_instrument$/, '').replace(/_/g, ' ').replace(/^./, c => c.toUpperCase()).replace(/^Fx\b/, 'FX'); }

/* The cards of the other aspects. */
function otherCards(t) {
    const f = (key, label, value, o = {}) => Object.assign({ key, label, value, economic: false, type: 'text' }, o);
    return {
        identifiers: {
            id: 'identifiers', title: 'Identifiers', table: 'trade_identifier', version: 1, child: true,
            columns: ['Id type', 'Value'], rows: [['Internal', t.code], ['ORE', 'Trade_' + t.code.slice(-4)], ['UTI', t.channel === 'manual' ? '' : 'UTI-' + t.code.slice(-5)]].filter(r => r[1])
        },
        roles: {
            id: 'roles', title: 'Party roles', table: 'trade_party_role', version: 1, child: true,
            columns: ['Role', 'Party'], rows: [['CalculationAgent', ref('counterparty', t.cp, cpty(t.cp).name)]]
        },
        portfolios: {
            id: 'portfolios', title: 'Portfolios', table: 'trade_portfolio', version: 1, child: true,
            columns: ['Portfolio'], rows: [[ref('portfolio', t.product === 'fx' ? 'FX flow' : 'Rates and credit')]]
        },
        additional: {
            id: 'additional', title: 'Additional fields', table: 'trade_additional_field', version: 1, child: true,
            columns: ['Name', 'Value'], rows: [['Confirmation channel', 'SWIFT'], ['Regulatory reporting', 'EMIR']]
        }
    };
}

/* ---------- the history of a trade ---------- */
function historyOf(t) {
    const day = (n, hh = 10, mm = 0) => new Date(NOW.getTime() - n * 86400000 + (hh - 16) * 3600000 + mm * 60000).toISOString().slice(0, 19) + '+01:00';
    const entries = [];
    const first = t.state === 'draft' ? 'draft_capture' : 'new_booking';
    entries.push({
        id: 'a1', at: day(t.ver * 6 + 4, 9, 41), kind: 'booked', activity: first, version: 1, ext: 1, actor: 'ada.lovelace',
        reason: 'system.new_record', note: first === 'draft_capture' ? 'Recorded from a phone call, awaiting counterparty details' : 'Booked from the confirmation',
        diffs: [['trade_type', '', t.type], ['counterparty', '', cpty(t.cp).name], ['book', '', book(t.book).name], ['notional', '', money(t.notional, t.ccy)]]
    });
    entries.push({
        id: 'c1', at: day(t.ver * 6 + 4, 9, 42), kind: 'booked', activity: first, record: 'Identifiers', what: 'Identifiers recorded', version: 1, ext: 1, actor: 'ada.lovelace',
        reason: 'system.new_record', note: '', diffs: [['internal_trade_id', '', t.code]]
    }, {
        id: 'c2', at: day(t.ver * 6 + 4, 9, 43), kind: 'booked', activity: first, record: 'Party roles', what: 'Party roles recorded', version: 1, ext: 1, actor: 'ada.lovelace',
        reason: 'system.new_record', note: '', diffs: [['calculation_agent', '', cpty(t.cp).name]]
    });
    if (t.ver >= 3) entries.push({
        id: 'c3', at: day(t.ver * 3 - 0.2, 12, 10), kind: 'amended', activity: 'correction', record: 'Additional fields', what: 'Additional field added', version: 2, ext: 1, actor: 'ada.lovelace',
        reason: 'system.update', note: 'EMIR reporting flag', diffs: [['regulatory_reporting', '', 'EMIR']]
    });
    if (t.ver >= 2) entries.push({
        id: 'a2', at: day(t.ver * 5, 14, 12), kind: 'amended', activity: 'correction', version: 2, ext: 1, actor: 'ada.lovelace',
        reason: 'common.rectification', note: 'Corrected the internal description', diffs: [['description', 'Hedge', 'Hedges the Q3 bond issue']]
    });
    if (t.ver >= 3) entries.push({
        id: 'a3', at: day(t.ver * 3, 11, 5), kind: 'amended', activity: 'amendment', version: 3, ext: Math.min(2, t.ext), actor: 'ada.lovelace',
        reason: 'system.update', note: 'Client asked for a larger notional', diffs: [['notional', money(t.notional * 0.8, t.ccy), money(t.notional, t.ccy)]], economic: true
    });
    if (t.ver >= 3) entries.push({
        id: 'a3c', at: day(t.ver * 3 - 0.5, 11, 40), kind: 'confirmed', activity: 'confirmation', version: 3, ext: Math.min(2, t.ext), actor: 'ores.trading.service',
        reason: 'common.non_material_update', note: 'Matched by ' + cpty(t.cp).name, diffs: [], noVersion: true
    });
    if (t.channel === 'stp') entries.unshift({
        id: 'a0', at: day(t.ver * 6 + 4.1, 9, 5), kind: 'imported', activity: 'new_booking', version: 1, ext: 1, actor: 'ores.ore.service',
        reason: 'system.import', note: 'Imported from an ORE portfolio file', diffs: [['ore_trade_id', '', 'Trade_' + t.code.slice(-4)]]
    });
    if (t.ver >= 5) entries.push({
        id: 'a5', at: day(2, 10, 20), kind: 'amended', activity: 'amendment', version: 5, ext: t.ext, actor: 'ada.lovelace',
        reason: 'system.update', note: 'New fixed rate agreed on the call', diffs: [['fixed_rate', '2.7500 %', '2.8500 %']], economic: true
    });
    if (t.state === 'cancelled') entries.push({
        id: 'a9', at: day(1, 9, 12), kind: 'cancelled', activity: 'novation', version: t.ver, ext: t.ext, actor: 'grace.hopper',
        reason: 'system.update', note: 'Novated to Meridian Bank AG. Rebooked as ACME-100208.', diffs: [['state', 'live', 'cancelled']]
    });
    return entries;
}

/* ---------- the shell ---------- */
const AREAS = [['Home', '#'], ['Reference data', '#'], ['Organisation', '#'], ['Market data', '#'], ['Trades', '../trade-page/'], ['Requests', '#'], ['Operations', '#']];
const SIBLINGS = [
    ['trade-page', 'Trades and the trade page'], ['book-a-trade', 'Book a trade'], ['ore-portfolio', 'ORE import and export'], ['structure-page', 'A structure or package']
];
/* opts: name, question, crumbs [[label, href?]], states [[label, change]], says (html-free string) */
function shell(opts) {
    document.title = opts.title + ' — Trades prototype';
    const note = h('div', { class: 'proto-note' }, 'Prototype · throwaway · mock data · ' + opts.name + ' · ' + opts.question);
    const menu = h('nav', { class: 'menu', 'aria-label': 'Areas' }, AREAS.map(([a, href]) => h('a', { href, class: a === 'Trades' ? 'on' : '', 'data-ref': href === '#' ? 'area:' + a : false }, a)));
    const top = h('header', { class: 'topbar' }, h('span', { class: 'brand' }, 'ORE Studio'), menu,
        h('span', { class: 'bell', title: 'Notifications' }, '🔔'),
        h('span', { class: 'me' }, avatar(ME, 's'), PEOPLE[ME].name));
    const crumbs = h('nav', { class: 'crumbs', 'aria-label': 'Breadcrumb' }, crumbList(opts.crumbs));
    const main = h('main', { class: 'page' }, crumbs);
    const pills = h('div', { class: 'pills' }, h('span', { class: 'pill' }, 'Tenant ' + TENANT), h('span', { class: 'pill' }, 'Party ' + PARTY),
        h('span', { class: 'pill' }, 'Client prototype'), h('span', { class: 'pill' }, 'Server mock'), h('span', { class: 'pill' }, 'Environment local'));
    const bar = h('div', { class: 'proto-bar' }, h('span', { class: 'label' }, opts.name),
        (opts.states || []).map(([label, change, on]) => h('a', { href: typeof change === 'string' ? change : qs(change), class: on ? 'on' : '' }, label)),
        opts.noRoles ? null : [h('span', { class: 'sep' }, '|'), h('span', { class: 'label' }, 'as'),
        ['trader', 'operations', 'reader'].map(r => h('a', { href: qs({ as: r }), class: r === ROLE ? 'on' : '' }, r))],
        h('span', { class: 'sep' }, '|'), h('span', { class: 'label' }, 'prototypes'),
        SIBLINGS.map(([dir, label]) => h('a', { href: '../' + dir + '/', title: label }, dir)));
    document.body.prepend(note, top, main);
    document.body.append(pills, bar);
    if (opts.says) document.body.append(h('div', { class: 'proto-says' }, h('b', null, 'What this state argues. '), opts.says));
    document.body.addEventListener('click', e => {
        const a = e.target.closest('a[data-ref]');
        if (!a) return;
        e.preventDefault();
        const [kind, key] = a.dataset.ref.split(':');
        toast('Opens the ' + kind + ' page for ' + key + '. Not drawn in this prototype.');
    });
    return main;
}
function crumbList(list) {
    return list.flatMap(([label, href], i) => [i ? h('span', { class: 'sep' }, '›') : null, href ? h('a', { href }, label) : h('span', null, label)]).filter(Boolean);
}
function setCrumbs(list, title) {
    $('.crumbs').replaceChildren(...crumbList(list));
    if (title) document.title = title + ' — Trades prototype';
}
function toast(msg, tone) {
    const old = $('.toast'); if (old) old.remove();
    const t = h('div', { class: 'toast', role: 'status' }, msg);
    document.body.append(t);
    setTimeout(() => t.remove(), 3800);
}
function pageHead({ title, lead, id, meta, actions }) {
    return h('div', { class: 'head' }, h('div', null, h('h1', null, title), id || meta ? h('div', { class: 'id' }, id, meta ? h('span', { class: 'meta' }, meta) : null) : null, lead ? h('p', { class: 'lead' }, lead) : null),
        h('div', { class: 'actions' }, actions));
}

/* ---------- refresh, workflows, pager ---------- */
let LOADED = new Date(NOW.getTime() - 4 * 60000);
function iso(d) { return new Date(d.getTime() + 3600000).toISOString().slice(0, 19) + '+01:00'; }
function refreshButton({ stale, onClick }) {
    const b = h('button', { class: 'btn' + (stale ? ' stale pulse' : ''), title: stale ? 'The data changed on the server at ' + iso(NOW) + '. Refresh to read it.' : 'Refresh', onclick: onClick }, '↻ Refresh');
    return h('div', null, b, h('span', { class: 'loaded', title: 'Loaded' }, iso(LOADED)));
}
function workflowMenu(items) {
    const allowed = items.filter(i => i.allowed !== false);
    if (!allowed.length) return null;
    const list = h('div', { class: 'wf-list', hidden: true }, allowed.map(i => {
        const inner = [h('b', null, i.title), h('span', null, i.line)];
        return i.href ? h('a', { href: i.href }, inner) : h('button', { onclick: () => { list.hidden = true; i.run(); } }, inner);
    }));
    const btn = h('button', { class: 'btn wfbtn', 'aria-haspopup': 'true', onclick: () => { list.hidden = !list.hidden; } }, 'Workflows ▾');
    document.addEventListener('click', e => { if (!e.target.closest('.wf')) list.hidden = true; });
    return h('div', { class: 'wf' }, btn, list);
}
const PAGE_SIZES = [15, 25, 50, 100];
function pager({ offset, limit, total, onChange }) {
    const last = Math.max(0, Math.floor((total - 1) / limit) * limit);
    const from = total ? offset + 1 : 0, to = Math.min(total, offset + limit);
    const mk = (label, off, dis) => h('button', { class: 'btn small', disabled: dis, onclick: () => onChange({ offset: off, limit }) }, label);
    return h('div', { class: 'pager' }, h('span', null, 'Showing ' + from + '–' + to + ' of ' + total),
        h('div', { class: 'btns' }, mk('« First', 0, offset === 0), mk('‹ Previous', Math.max(0, offset - limit), offset === 0),
            mk('Next ›', offset + limit, offset + limit >= total), mk('Last »', last, offset + limit >= total)),
        h('label', null, 'Page size ', h('select', { class: 'field-select', onchange: e => onChange({ offset: 0, limit: Number(e.target.value) }) },
            PAGE_SIZES.map(s => h('option', { value: s, selected: s === limit }, s)))));
}

/* Replace the children of an element, skipping the nulls a conditional leaves. */
function fill(el, ...kids) { el.replaceChildren(...kids.flat().filter(k => k != null && k !== false)); }

/* ---------- dialogs ---------- */
function openDialog({ title, loadedAt, body, buttons, onclose }) {
    const d = h('dialog', null,
        h('header', null, h('h2', null, title), loadedAt ? h('span', { class: 'faint small', title: 'Loaded' }, 'Loaded ' + iso(loadedAt)) : null),
        h('div', { class: 'dbody' }, body), h('footer', null, buttons));
    d.addEventListener('close', () => { d.remove(); if (onclose) onclose(); });
    document.body.append(d);
    d.showModal();
    return d;
}
function field(label, input, hint) { return h('div', { class: 'fld' }, h('label', null, label), input, hint ? h('span', { class: 'hint' }, hint) : null); }
function select(options, value, onchange) {
    return h('select', { class: 'field-select', onchange: e => onchange && onchange(e.target.value) },
        options.map(o => { const [v, l, off] = Array.isArray(o) ? o : [o, o]; return h('option', { value: v, selected: v === value, disabled: !!off }, l); }));
}

/* The input for one field of a card. A value that exists is chosen from a combo box, a date has a date input,
 * a fixed value is shown and never edited, and an optional value has Clear. */
/* An amount shows digit separators as it is typed. The value that is set holds digits and a point only. */
function groupDigits(raw) {
    const [whole, frac] = String(raw).split('.');
    return whole.replace(/\B(?=(\d{3})+(?!\d))/g, ',') + (frac !== undefined ? '.' + frac : '');
}
function moneyInput(value, set) {
    const plain = v => String(v == null ? '' : v).replace(/[^0-9.]/g, '').replace(/^(\d*\.\d*).*$/, (m, a) => a);
    return h('input', {
        class: 'field-input', type: 'text', inputmode: 'decimal', style: 'text-align:right', value: groupDigits(plain(value)),
        oninput: e => {
            const el = e.target, before = el.value.slice(0, el.selectionStart).replace(/[^0-9.]/g, '').length;
            const raw = plain(el.value);
            el.value = groupDigits(raw);
            let n = 0, pos = 0;
            while (pos < el.value.length && n < before) { if (/[0-9.]/.test(el.value[pos])) n++; pos++; }
            el.setSelectionRange(pos, pos);
            set(raw);
        }
    });
}
function valueInput(fd, value, set, off) {
    if (fd.fixed) return h('div', { class: 'static' }, ivalue(value));
    let el;
    const flagged = list => list.map(c => [c, (FLAGS[c] ? FLAGS[c] + ' ' : '') + c]);
    if (fd.type === 'ccy' || fd.type === 'calendar' || fd.options) {
        const list = fd.type === 'ccy' ? CCYS : fd.type === 'calendar' ? CALENDARS : fd.options;
        el = select(flagged(list.includes(value) ? list : [value].concat(list)), value, set);
    } else if (fd.type === 'money') el = moneyInput(value, set);
    else el = h('input', { class: 'field-input', type: fd.type === 'date' ? 'date' : 'text', value, oninput: e => set(e.target.value) });
    el.disabled = !!off;
    return fd.optional ? h('div', { class: 'withclear' }, el, h('button', { class: 'btn small', disabled: !!off || value === '', onclick: () => set('', true) }, 'Clear')) : el;
}
/* A failed request names its operation, status, server code and the first eight characters of the request id. */
function failure(op, status, code, id, text, actions) {
    return h('div', { class: 'notice bad' }, h('b', null, text + ' '), h('span', { class: 'mono small' }, op + ' answered ' + status + ' ' + code + ' (request ' + id.slice(0, 8) + '). '), actions);
}
/* A screen the reader cannot use names what it needs and links to a request for each role that allows it. */
function needsRole(screen, need, roles) {
    return h('div', { class: 'notice warn' }, h('b', null, 'You cannot use ' + screen + '. '), 'It needs the right to ' + need + '. You can ask for: ',
        roles.map((r, i) => [i ? ', ' : '', h('a', { href: '#', 'data-ref': 'access-request:' + r.toLowerCase().replace(/ /g, '-') }, r)]), '.');
}

/* A form with changes is not thrown away without asking. */
function leaveGuard(isDirty) {
    window.addEventListener('beforeunload', e => { if (isDirty()) e.preventDefault(); });
    document.addEventListener('click', e => {
        const a = e.target.closest('a[href]');
        if (a && isDirty() && a.getAttribute('href') !== '#' && !a.closest('.tabs') && !a.closest('.proto-bar') && !confirm('You have unsaved changes. Close anyway?')) e.preventDefault();
    }, true);
}

/* ---------- cards and fields ---------- */
function showValue(fd) {
    if (fd.node) return fd.node;
    if (fd.value === '' || fd.value == null) return dash();
    if (fd.type === 'money') return money(fd.value);
    if (fd.type === 'decimal' && fd.unit) return fd.value + ' ' + fd.unit;
    if (fd.type === 'ccy') return ccyRef(fd.value);
    if (fd.type === 'calendar') return calendarRef(fd.value);
    if (fd.ref) return ref(fd.ref, fd.value, fd.value);
    return ivalue(fd.value);
}
function tabs(list, current, hrefFor, onSelect) {
    return h('nav', { class: 'tabs', role: 'tablist' }, list.map(([id, label]) => h('a', { href: hrefFor(id), class: id === current ? 'on' : '', role: 'tab',
        onclick: onSelect ? e => { e.preventDefault(); onSelect(id); } : false }, label)));
}
function econMark(fd) { return fd.economic ? h('span', { class: 'econ', title: 'The model declares this field economic: a change moves the external version.' }, 'economic') : null; }
function gapMark(card) {
    return card.digestGap ? h('span', { class: 'gap', title: 'The economic digest does not read this table yet: a change leaves the external version alone.' }, 'not in the digest') : null;
}
/* A card of fields, or a card of rows. `edit` is drawn only when the person may change it. */
function card(c, { edit, readOnlyNote } = {}) {
    const head = h('header', null, h('h2', null, c.title), c.version != null ? chip('v' + c.version, 'v', 'Version of this record') : null,
        gapMark(c), h('span', { class: 'sp' }), readOnlyNote ? h('span', { class: 'access' }, 'Read only') : null,
        edit ? h('button', { class: 'btn small', onclick: () => edit(c) }, '✎ Edit') : null);
    let body;
    if (c.fields) {
        body = h('dl', { class: 'fields' }, c.fields.map(fd => h('div', null, h('dt', null, fd.label, econMark(fd)), h('dd', null, showValue(fd), fd.fixedWhy ? h('div', { class: 'why' }, fd.fixedWhy) : null))));
    } else {
        body = h('table', { class: 'grid' }, h('thead', null, h('tr', null, c.columns.map(x => h('th', null, x)))),
            h('tbody', null, c.rows.length ? c.rows.map(r => h('tr', null, r.map(v => h('td', null, ivalue(v))))) : h('tr', null, h('td', { colspan: c.columns.length }, h('span', { class: 'muted' }, 'Nothing recorded.')))));
    }
    return h('section', { class: 'card', 'data-card': c.id }, head, h('div', { class: 'body' }, body));
}

/* ---------- the timeline ---------- */
const KINDS = { booked: 'Booked', amended: 'Amended', event: 'Event', confirmed: 'Confirmed', cancelled: 'Cancelled', imported: 'Imported' };
function timeline(entries, { isService, flash } = {}) {
    const wrap = h('div');
    let hideSystem = false;
    function draw() {
        wrap.replaceChildren();
        const shown = entries.filter(e => !(hideSystem && PEOPLE[e.actor] && PEOPLE[e.actor].service)).slice().sort((a, b) => b.at.localeCompare(a.at));
        const HOUR = 3600000, DAY = 86400000, OFF = 3600000;
        const ms = e => new Date(e.at).getTime();
        const times = shown.map(ms);
        const lo = times.length ? Math.min(...times) : 0, hi = times.length ? Math.max(...times) : 0;
        const step = hi - lo > 2 * DAY ? DAY : HOUR;
        const keyOf = b => new Date(b).toISOString().slice(0, step === DAY ? 10 : 13);
        const buckets = [];
        if (shown.length) for (let b = Math.floor((lo + OFF) / step) * step; b <= hi + OFF; b += step) buckets.push({ start: b, key: keyOf(b), list: [] });
        for (const e of shown) { const b = buckets.find(x => x.key === keyOf(ms(e) + OFF)); if (b) b.list.push(e); }
        const jump = mid => {
            const near = shown.slice().sort((a, b) => Math.abs(ms(a) + OFF - mid) - Math.abs(ms(b) + OFF - mid))[0];
            const el = near && wrap.querySelector('[data-entry="' + near.id + '"]');
            if (el) { el.scrollIntoView({ block: 'center' }); el.classList.add('flash'); setTimeout(() => el.classList.remove('flash'), 1500); }
        };
        const chart = h('div', { class: 'chart', role: 'img', 'aria-label': 'Events over time' }, buckets.map(b => {
            const counts = {}; b.list.forEach(e => counts[e.kind] = (counts[e.kind] || 0) + 1);
            return h('div', { class: 'bar', title: b.list.length ? b.key.replace('T', ' ') + (step === HOUR ? ':00' : '') + ': ' + b.list.length : false, onclick: () => jump(b.start + step / 2) },
                Object.entries(counts).map(([k, n]) => h('i', { class: 'k-' + k, style: 'height:' + n * 18 + 'px', title: KINDS[k] + ': ' + n })));
        }));
        const axis = shown.length ? h('div', { class: 'axis' }, h('span', null, shown[shown.length - 1].at.slice(0, 16).replace('T', ' ')), h('span', null, shown[0].at.slice(0, 16).replace('T', ' '))) : null;
        const kinds = {}; shown.forEach(e => kinds[e.kind] = (kinds[e.kind] || 0) + 1);
        const legend = h('div', { class: 'legend' }, Object.entries(kinds).map(([k, n]) => h('span', null, h('i', { class: 'k-' + k }), KINDS[k] + ' ' + n)),
            h('span', { class: 'sp', style: 'flex:1' }),
            h('button', { class: 'btn small ghost', title: isService ? '' : 'Only a reader who may read accounts can tell a service from a person.', disabled: !isService, onclick: () => { hideSystem = !hideSystem; draw(); } },
                hideSystem ? 'Show system activity' : 'Hide system activity'));
        const rail = h('div', { class: 'rail' });
        let lastDay = '';
        for (const e of shown) {
            const day = e.at.slice(0, 10);
            if (day !== lastDay) { rail.append(h('div', { class: 'day', 'data-day': day }, new Date(day).toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' }))); lastDay = day; }
            const a = activityType(e.activity);
            const det = e.diffs && e.diffs.length ? h('details', { class: 'det' }, h('summary', null, 'Details'), h('div', { class: 'diff' }, e.diffs.map(([n, o, v]) => [
                o !== '' ? h('div', { class: 'dl old' }, h('span', { class: 'sign' }, '−'), h('span', { class: 'fname' }, n), h('span', null, o)) : null,
                h('div', { class: 'dl new' }, h('span', { class: 'sign' }, '+'), h('span', { class: 'fname' }, n), h('span', null, v))]))) : h('span', { class: 'faint small' }, 'No changes');
            rail.append(h('div', { class: 'entry' + (e.current ? ' cur' : '') + (flash === e.id ? ' flash' : ''), 'data-entry': e.id },
                h('div', { class: 'at', title: e.at.replace('T', ' ') }, e.at.slice(11, 19)),
                h('div', { class: 'dot' }, h('i', { title: e.current ? 'Current version' : '' })),
                h('div', { class: 'main' }, avatar(e.actor, 'm'), h('div', null,
                    h('div', { class: 'line1' }, chip(KINDS[e.kind], e.kind === 'cancelled' ? 'bad' : e.kind === 'event' ? 'flow' : e.kind === 'booked' ? 'acc' : 'warn'),
                        h('b', null, e.what || (a ? a.label : e.activity)), e.noVersion ? chip('no version', '', 'This entry moves neither version.') : chip((e.record || 'Trade') + ' v' + e.version, 'v'),
                        a && a.economic && a.real ? chip('economic', 'acc', 'Moves the external version') : null,
                        a && a.confirmable ? chip('confirmable', '', 'A new confirmation is issued') : null,
                        e.economic && e.ext ? chip('external v' + e.ext, 'v') : null,
                        PEOPLE[e.actor] && PEOPLE[e.actor].service ? h('span', null, PEOPLE[e.actor].name) : person(e.actor),
                        h('span', { class: 'faint small' }, REASON_CODES.find(r => r[0] === e.reason) ? REASON_CODES.find(r => r[0] === e.reason)[1] : e.reason),
                        null),
                    e.note ? h('div', { class: 'note' }, e.note) : null, det))));
        }
        wrap.append(chart, axis, legend, rail);
    }
    draw();
    return wrap;
}

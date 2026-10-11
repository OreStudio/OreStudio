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
    return h('span', { class: 'person' }, avatar(username, 's'),
        p.service ? label : h('a', { href: '#', 'data-ref': 'person:' + username }, label));
}
/* The signed-in person decides what is drawn: a control that is not allowed is not drawn. */
const ROLE = ['trader', 'operations', 'reader'].includes(P.get('as')) ? P.get('as') : 'trader';
const CAN = {
    trader: { book: 1, amend: 1, structure: 1 },
    operations: { event: 1, import: 1, export: 1, authorise: 1, amend: 1 },
    reader: {}
}[ROLE];
const ME = { trader: 'ada.lovelace', operations: 'grace.hopper', reader: 'tomas' }[ROLE];

/* ---------- reference data ---------- */
const TENANT = 'Acme Corporation';
const PARTY = 'Acme London';
const BOOKS = [
    { code: 'EMEA-RATES', name: 'EMEA Rates', purpose: 'Trading' },
    { code: 'EMEA-FX', name: 'EMEA FX', purpose: 'Trading' },
    { code: 'EMEA-CREDIT', name: 'EMEA Credit', purpose: 'Trading' },
    { code: 'EMEA-EQ', name: 'EMEA Equity', purpose: 'Trading' },
    { code: 'VIRT-DRAFT', name: 'Virtual draft book', purpose: 'Virtual', virtual: true }
];
const COUNTERPARTIES = [
    { code: 'NWCAP', name: 'Northwind Capital Ltd', open: true },
    { code: 'MRDN', name: 'Meridian Bank AG', open: true },
    { code: 'CSTL', name: 'Coastal Credit SA', open: true },
    { code: 'HLDN', name: 'Holding North America Inc', open: false }
];
const cpty = code => COUNTERPARTIES.find(c => c.code === code);
const book = code => BOOKS.find(b => b.code === code);
const BOOKING_NATURES = ['Real', 'Test', 'Hypothetical', 'Pre-agreement draft'];
const ENTRY_CHANNELS = ['Manual', 'Venue', 'ORE import', 'API'];
const COUNTERPARTY_SCOPES = ['Bilateral', 'Cleared', 'Internal'];

const TRADE_TYPES_RAW = [["CompositeTrade","Composite Trade","composite","composite_instrument"],["RateDigitalOption","Rate Digital Option","swap",""],["SwaptionStraddle","Swaption Straddle","swap",""],["ForwardVolatilityAgreement","Forward Volatility Agreement","swap",""],["Swap","Interest Rate Swap","swap","rate_instrument"],["CrossCurrencySwap","Cross Currency Swap","swap","rate_instrument"],["ForwardRateAgreement","Forward Rate Agreement","swap","rate_instrument"],["CapFloor","Cap or Floor","swap","rate_instrument"],["Swaption","Swaption","swap","rate_instrument"],["FlexiSwap","Flexi Swap","swap","rate_instrument"],["BalanceGuaranteedSwap","Balance Guaranteed Swap","swap","rate_instrument"],["CallableSwap","Callable Swap","swap","rate_instrument"],["KnockOutSwap","Knock-Out Swap","swap","rate_instrument"],["RiskParticipationAgreement","Risk Participation Agreement","credit","credit_instrument"],["InflationSwap","Inflation Swap","swap","rate_instrument"],["FxForwardVolatilityAgreement","FX Forward Volatility Agreement","fx",""],["FxForward","FX Forward","fx","fx_forward_instrument"],["FxSwap","FX Swap","fx","fx_forward_instrument"],["FxOption","FX Option","fx","fx_vanilla_option_instrument"],["FxDigitalOption","FX Digital Option","fx","fx_digital_option_instrument"],["FxAverageForward","FX Average Forward","fx","fx_asian_forward_instrument"],["FxAsianOption","FX Asian Option","fx",""],["FxBarrierOption","FX Barrier Option","fx","fx_barrier_option_instrument"],["FxDoubleBarrierOption","FX Double Barrier Option","fx","fx_barrier_option_instrument"],["FxEuropeanBarrierOption","FX European Barrier Option","fx","fx_barrier_option_instrument"],["FxWindowBarrierOption","FX Window Barrier Option","fx",""],["FxGenericBarrierOption","FX Generic Barrier Option","fx","fx_barrier_option_instrument"],["FxKIKOBarrierOption","FX Knock-In Knock-Out Barrier Option","fx","fx_barrier_option_instrument"],["FxTouchOption","FX Touch Option","fx","fx_digital_option_instrument"],["FxDoubleTouchOption","FX Double Touch Option","fx","fx_digital_option_instrument"],["FxDigitalBarrierOption","FX Digital Barrier Option","fx","fx_digital_option_instrument"],["FxVarianceSwap","FX Variance Swap","fx","fx_variance_swap_instrument"],["FxPairwiseVarianceSwap","FX Pairwise Variance Swap","fx",""],["FxBasketVarianceSwap","FX Basket Variance Swap","fx",""],["FxAccumulator","FX Accumulator","fx","fx_accumulator_instrument"],["FxTaRF","FX Target Redemption Forward","fx","fx_asian_forward_instrument"],["FxWorstOfBasketSwap","FX Worst-Of Basket Swap","fx",""],["FxBestEntryOption","FX Best Entry Option","fx",""],["FxBasketOption","FX Basket Option","fx",""],["FxRainbowOption","FX Rainbow Option","fx",""],["FxStrikeResettableOption","FX Strike Resettable Option","fx",""],["CreditDefaultSwap","Credit Default Swap","credit","credit_instrument"],["CreditDefaultSwapOption","Credit Default Swap Option","credit","credit_instrument"],["IndexCreditDefaultSwap","Index Credit Default Swap","credit","credit_instrument"],["IndexCreditDefaultSwapOption","Index Credit Default Swap Option","credit","credit_instrument"],["SyntheticCDO","Synthetic CDO","credit","credit_instrument"],["CreditLinkedSwap","Credit Linked Swap","credit","credit_instrument"],["CBO","Collateralised Bond Obligation","credit","credit_instrument"],["BondFutureOption","Bond Future Option","bond",""],["Bond","Fixed Income Bond","bond","bond_instrument"],["ForwardBond","Forward Bond","bond","bond_instrument"],["BondFuture","Bond Future","bond","bond_instrument"],["BondOption","Bond Option","bond","bond_instrument"],["BondRepo","Bond Repo","bond","bond_instrument"],["BondTRS","Bond Total Return Swap","bond","bond_instrument"],["BondPosition","Bond Position","bond","bond_instrument"],["CallableBond","Callable Bond","bond","bond_instrument"],["ConvertibleBond","Convertible Bond","bond","bond_instrument"],["Ascot","Asset Swapped Convertible Option Transaction","bond","bond_instrument"],["EquityAutoDeltaHedgedOption","Equity Auto Delta Hedged Option","equity",""],["EquityForwardVolatilityAgreement","Equity Forward Volatility Agreement","equity",""],["EquityOption","Equity Option","equity","equity_option_instrument"],["EquityFutureOption","Equity Future Option","equity",""],["EquityAsianOption","Equity Asian Option","equity","equity_asian_option_instrument"],["EquityBarrierOption","Equity Barrier Option","equity","equity_barrier_option_instrument"],["EquityDoubleBarrierOption","Equity Double Barrier Option","equity","equity_barrier_option_instrument"],["EquityEuropeanBarrierOption","Equity European Barrier Option","equity","equity_barrier_option_instrument"],["EquityWindowBarrierOption","Equity Window Barrier Option","equity",""],["EquityGenericBarrierOption","Equity Generic Barrier Option","equity",""],["EquityTouchOption","Equity Touch Option","equity","equity_digital_option_instrument"],["EquityDoubleTouchOption","Equity Double Touch Option","equity",""],["EquityDigitalOption","Equity Digital Option","equity","equity_digital_option_instrument"],["EquityForward","Equity Forward","equity","equity_forward_instrument"],["EquitySwap","Equity Swap","equity","equity_swap_instrument"],["EquityVarianceSwap","Equity Variance Swap","equity","equity_variance_swap_instrument"],["EquityPairwiseVarianceSwap","Equity Pairwise Variance Swap","equity",""],["EquityBasketVarianceSwap","Equity Basket Variance Swap","equity",""],["EquityCliquetOption","Equity Cliquet Option","equity","equity_option_instrument"],["EquityAccumulator","Equity Accumulator","equity","equity_accumulator_instrument"],["EquityTaRF","Equity Target Redemption Forward","equity","equity_accumulator_instrument"],["EquityWorstOfBasketSwap","Equity Worst-Of Basket Swap","equity","equity_swap_instrument"],["EquityBestEntryOption","Equity Best Entry Option","equity",""],["EquityBasketOption","Equity Basket Option","equity",""],["EquityRainbowOption","Equity Rainbow Option","equity",""],["EquityOutperformanceOption","Equity Outperformance Option","equity","equity_option_instrument"],["EquityStrikeResettableOption","Equity Strike Resettable Option","equity",""],["TotalReturnSwap","Total Return Swap","equity","composite_instrument"],["ContractForDifference","Contract for Difference","equity","composite_instrument"],["EquityPosition","Equity Position","equity","equity_position_instrument"],["EquityOptionPosition","Equity Option Position","equity","equity_position_instrument"],["CommodityForwardVolatilityAgreement","Commodity Forward Volatility Agreement","commodity","commodity_instrument"],["IntradayPowerForward","Intraday Power Forward","commodity","commodity_instrument"],["CommodityForward","Commodity Forward","commodity","commodity_instrument"],["CommodityOption","Commodity Option","commodity","commodity_instrument"],["CommodityDigitalOption","Commodity Digital Option","commodity","commodity_instrument"],["CommodityDigitalAveragePriceOption","Commodity Digital Average Price Option","commodity","commodity_instrument"],["CommodityAsianOption","Commodity Asian Option","commodity","commodity_instrument"],["CommodityAveragePriceOption","Commodity Average Price Option","commodity","commodity_instrument"],["CommoditySpreadOption","Commodity Spread Option","commodity","commodity_instrument"],["CommodityOptionStrip","Commodity Option Strip","commodity","commodity_instrument"],["CommoditySwap","Commodity Swap","commodity","commodity_instrument"],["CommoditySwaption","Commodity Swaption","commodity","commodity_instrument"],["CommodityVarianceSwap","Commodity Variance Swap","commodity","commodity_instrument"],["CommodityPairwiseVarianceSwap","Commodity Pairwise Variance Swap","commodity","commodity_instrument"],["CommodityBasketVarianceSwap","Commodity Basket Variance Swap","commodity","commodity_instrument"],["CommodityAccumulator","Commodity Accumulator","commodity","commodity_instrument"],["CommodityTaRF","Commodity Target Redemption Forward","commodity","commodity_instrument"],["CommodityWorstOfBasketSwap","Commodity Worst-Of Basket Swap","commodity","commodity_instrument"],["CommodityBestEntryOption","Commodity Best Entry Option","commodity","commodity_instrument"],["CommodityWindowBarrierOption","Commodity Window Barrier Option","commodity","commodity_instrument"],["CommodityGenericBarrierOption","Commodity Generic Barrier Option","commodity","commodity_instrument"],["CommodityBasketOption","Commodity Basket Option","commodity","commodity_instrument"],["CommodityRainbowOption","Commodity Rainbow Option","commodity","commodity_instrument"],["CommodityStrikeResettableOption","Commodity Strike Resettable Option","commodity","commodity_instrument"],["CommodityPosition","Commodity Position","commodity","commodity_instrument"],["CashPosition","Cash Position","equity",""],["ScriptedTrade","Scripted Trade (ORE generic payoff engine)","scripted","scripted_instrument"],["Autocallable_01","Autocallable (variant 01)","scripted","scripted_instrument"],["DoubleDigitalOption","Double Digital Option","scripted","scripted_instrument"],["EuropeanOptionBarrier","European Option with Barrier","scripted",""],["PerformanceOption_01","Performance Option (variant 01)","scripted","scripted_instrument"]];
const TRADE_TYPES = TRADE_TYPES_RAW.map(([code, description, product, instrument]) => ({ code, description, product, instrument }));
const tradeType = code => TRADE_TYPES.find(t => t.code === code);
const PRODUCTS = ['swap', 'fx', 'credit', 'bond', 'equity', 'composite'];

/* The four axes an activity type sits on. Codes marked * are in the model's
 * documentation; the rest are illustrative. */
const ACTIVITY_TYPES = [
    { code: 'new_booking', label: 'New booking', category: 'new_activity', economic: true, confirmable: true, real: true },
    { code: 'draft_capture', label: 'Draft capture', category: 'new_activity', economic: false, confirmable: false, real: true },
    { code: 'amendment_economic', label: 'Amendment: economic terms', category: 'lifecycle_event', economic: true, confirmable: true, real: true },
    { code: 'amendment_reference', label: 'Amendment: reference details', category: 'lifecycle_event', economic: false, confirmable: false, real: true },
    { code: 'null_amend', label: 'No change in substance', category: 'lifecycle_event', economic: false, confirmable: false, real: false },
    { code: 'misbooking_correction', label: 'Correction of a misbooking', category: 'misbooking', economic: true, confirmable: true, real: true },
    { code: 'exercise', label: 'Exercise', category: 'lifecycle_event', economic: true, confirmable: true, real: true },
    { code: 'partial_termination', label: 'Partial termination', category: 'lifecycle_event', economic: true, confirmable: true, real: true },
    { code: 'expiry', label: 'Expiry', category: 'lifecycle_event', economic: false, confirmable: false, real: true },
    { code: 'novation', label: 'Novation', category: 'lifecycle_event', economic: true, confirmable: true, real: true },
    { code: 'cancellation', label: 'Cancellation', category: 'cancellation', economic: true, confirmable: true, real: true },
    { code: 'reinstatement', label: 'Reinstatement', category: 'cancellation', economic: true, confirmable: true, real: true },
    { code: 'confirmation', label: 'Counterparty confirmation', category: 'lifecycle_event', economic: false, confirmable: false, real: false },
    { code: 'authorisation', label: 'Operational authorisation', category: 'lifecycle_event', economic: false, confirmable: false, real: false }
];
const activityType = code => ACTIVITY_TYPES.find(a => a.code === code);
const REASON_CODES = [
    ['trade.client_request', 'Client request'], ['trade.correction', 'Correction of an error'],
    ['trade.restructure', 'Restructure'], ['trade.upsize', 'Upsize'], ['trade.no_change', 'No change in substance']
];

/* ---------- the trades ---------- */
/* Seven trades are written out by hand because the journeys walk through them.
 * The rest are generated so that the list has a size worth paging. */
const FEATURED = [
    { n: 1, type: 'Swap', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', auth: true, date: '2026-09-14', notional: 50000000, ccy: 'EUR', ext: 3, ver: 5, structure: 'PKG-0004' },
    { n: 2, type: 'FxOption', cp: 'MRDN', book: 'EMEA-FX', state: 'live', auth: false, date: '2026-10-08', notional: 10000000, ccy: 'EUR', ext: 1, ver: 2, structure: 'STR-0007' },
    { n: 3, type: 'Bond', cp: 'CSTL', book: 'EMEA-CREDIT', state: 'live', auth: true, date: '2026-08-02', notional: 25000000, ccy: 'EUR', ext: 1, ver: 1 },
    { n: 4, type: 'BondRepo', cp: 'MRDN', book: 'EMEA-CREDIT', state: 'live', auth: true, date: '2026-09-30', notional: 20000000, ccy: 'EUR', ext: 2, ver: 3, structure: 'PKG-0021' },
    { n: 5, type: 'CompositeTrade', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', auth: false, date: '2026-10-05', notional: 30000000, ccy: 'EUR', ext: 1, ver: 1 },
    { n: 6, type: 'Swap', cp: 'CSTL', book: 'VIRT-DRAFT', state: 'draft', auth: null, date: '2026-10-10', notional: 5000000, ccy: 'EUR', ext: 1, ver: 2, nature: 'Pre-agreement draft' },
    { n: 7, type: 'FxForward', cp: 'NWCAP', book: 'EMEA-FX', state: 'cancelled', auth: null, date: '2026-07-21', notional: 8000000, ccy: 'GBP', ext: 2, ver: 4, rebookedTo: 'ACME-100208' },
    { n: 8, type: 'FxForward', cp: 'MRDN', book: 'EMEA-FX', state: 'live', auth: false, date: '2026-10-09', notional: 8000000, ccy: 'GBP', ext: 1, ver: 1, rebookedFrom: 'ACME-100207', structure: 'STR-0010' },
    { n: 9, type: 'FxOption', cp: 'MRDN', book: 'EMEA-FX', state: 'live', auth: true, date: '2026-10-08', notional: 10000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0007' },
    { n: 11, type: 'Swap', cp: 'CSTL', book: 'EMEA-RATES', state: 'live', auth: true, date: '2026-09-02', notional: 40000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0009' },
    { n: 12, type: 'Swaption', cp: 'CSTL', book: 'EMEA-RATES', state: 'live', auth: true, date: '2026-09-02', notional: 40000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'STR-0009' },
    { n: 13, type: 'Bond', cp: 'MRDN', book: 'EMEA-CREDIT', state: 'live', auth: true, date: '2026-08-20', notional: 15000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'PKG-0021' },
    { n: 10, type: 'CapFloor', cp: 'NWCAP', book: 'EMEA-RATES', state: 'live', auth: true, date: '2026-10-05', notional: 30000000, ccy: 'EUR', ext: 1, ver: 1, structure: 'PKG-0004' }
];
const USED_TYPES = ['Swap', 'CrossCurrencySwap', 'Swaption', 'CapFloor', 'FxForward', 'FxOption', 'FxBarrierOption', 'Bond', 'BondRepo', 'BondOption',
    'CallableBond', 'EquityOption', 'EquityForward', 'CreditDefaultSwap', 'CompositeTrade'];
const BOOK_FOR = { swap: 'EMEA-RATES', fx: 'EMEA-FX', credit: 'EMEA-CREDIT', bond: 'EMEA-CREDIT', equity: 'EMEA-EQ', composite: 'EMEA-RATES' };
const CCYS = ['EUR', 'USD', 'GBP', 'CHF', 'JPY'];

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
            state, auth: state === 'live' ? (f.auth !== undefined ? f.auth : i % 4 !== 0) : null,
            date: f.date || day,
            notional: f.notional || (1 + (i * 37) % 20) * 1000000,
            ccy: f.ccy || CCYS[i % CCYS.length],
            ext: f.ext || 1 + (i % 3), ver: f.ver || 1 + (i % 3) + (i % 2),
            nature: f.nature || 'Real', channel: i % 5 === 0 ? 'ORE import' : 'Manual',
            scope: 'Bilateral', structure: f.structure || null, rebookedTo: f.rebookedTo || null, rebookedFrom: f.rebookedFrom || null,
            updated: new Date(NOW.getTime() - (i * 5.3) * 3600000).toISOString().slice(0, 19) + '+01:00'
        });
    }
    return trades;
}
function tradeId(i) {
    const h8 = (i * 2654435761 >>> 0).toString(16).padStart(8, '0');
    return h8 + '-3b2d-4a91-8f6e-' + (i * 7919).toString(16).padStart(12, '0');
}
let TRADES = buildTrades().concat(store('booked') || []);
const trade = code => TRADES.find(t => t.code === code || t.id === code);

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
                    f('notional', 'Notional', t.notional, { ...E, type: 'money' }), f('currency', 'Currency', t.ccy, { ...E, type: 'ccy' }),
                    f('effective_date', 'Effective date', d, { ...E, type: 'date' }), f('maturity_date', 'Maturity date', '2031-09-14', { ...E, type: 'date' }),
                    f('fixed_rate', 'Fixed rate', '2.8500', { ...E, type: 'decimal', unit: '%' }), f('floating_index', 'Floating index', 'EUR-EURIBOR-6M', { ...E }),
                    f('spread', 'Spread', '0.0000', { ...E, type: 'decimal', unit: '%' }), f('day_count', 'Day count', '30/360', { ...E }),
                    f('payment_calendar', 'Payment calendar', 'TARGET', { ...E }), f('pay_receive', 'Fixed leg', 'Pay', { ...E }),
                    f('description', 'Internal description', 'Five year receiver, hedges the Q3 bond issue')
                ]
            }, {
                id: 'legs', title: 'Legs', table: 'swap_leg', version: t.ver, child: true,
                ...rows(['Leg', 'Type', 'Notional', 'Rate / index', 'Frequency'], [
                    ['1', 'Fixed', money(t.notional, t.ccy), '2.8500 %', 'Annual'],
                    ['2', 'Floating', money(t.notional, t.ccy), 'EUR-EURIBOR-6M + 0.00 %', 'Semi-annual']]), economic: true
            }];
        case 'fx_vanilla_option_instrument':
            return [{
                id: 'instrument', title: 'FX vanilla option', table: 'fx_vanilla_option_instrument', version: t.ver,
                fields: [
                    f('option_type', 'Option type', 'Call', { ...E }), f('bought_currency', 'Bought currency', 'EUR', { ...E, type: 'ccy' }),
                    f('bought_amount', 'Bought amount', t.notional, { ...E, type: 'money' }), f('sold_currency', 'Sold currency', 'USD', { ...E, type: 'ccy' }),
                    f('sold_amount', 'Sold amount', t.notional * 1.0842, { ...E, type: 'money' }), f('strike', 'Strike', '1.0842', { ...E, type: 'decimal' }),
                    f('expiry_date', 'Expiry date', '2027-01-15', { ...E, type: 'date' }), f('delivery_date', 'Delivery date', '2027-01-19', { ...E, type: 'date' }),
                    f('exercise_style', 'Exercise style', 'European', { ...E }), f('description', 'Internal description', 'Client hedge')
                ]
            }, {
                id: 'premium', title: 'Premium', table: 'fx_option_premium', version: 1, child: true,
                ...rows(['Currency', 'Amount', 'Payment date'], [['USD', money(112500), '2026-10-10']]), economic: true
            }, {
                id: 'exercise_prices', title: 'Exercise prices', table: 'option_exercise_price', version: 1, child: true,
                ...rows(['Exercise date', 'Strike'], [['2027-01-15', '1.0842']]), economic: true, digestGap: !DIGEST_FIXED
            }];
        case 'bond_instrument':
            return [{
                id: 'instrument', title: 'Bond', table: 'bond_instrument', version: t.ver,
                fields: [
                    f('issue', 'Bond issue', 'Acme 3.25 % 2031', { ref: 'bond_issue' }), f('face_amount', 'Face amount', t.notional, { ...E, type: 'money' }),
                    f('currency', 'Currency', t.ccy, { ...E, type: 'ccy' }), f('coupon', 'Coupon', '3.2500', { ...E, type: 'decimal', unit: '%' }),
                    f('maturity_date', 'Maturity date', '2031-08-02', { ...E, type: 'date' }), f('settlement_days', 'Settlement days', '2', { ...E }),
                    f('day_count', 'Day count', 'ACT/ACT', { ...E }), f('yield_convention', 'Yield convention', 'Street', { ...E }),
                    f('description', 'Internal description', t.type === 'BondRepo' ? 'Overnight repo against the Acme 2031' : 'Held to maturity')
                ]
            }, {
                id: 'legs', title: 'Legs', table: 'bond_leg', version: t.ver, child: true,
                ...rows(['Leg', 'Type', 'Notional', 'Rate', 'Frequency'], [['1', 'Fixed', money(t.notional, t.ccy), '3.2500 %', 'Annual']]), economic: true
            }];
        case 'composite_instrument':
            return [{
                id: 'instrument', title: 'Composite trade', table: 'composite_instrument', version: t.ver,
                fields: [f('aggregation', 'Notional aggregation', 'Sum'), f('description', 'Internal description', 'Receiver swap and cap as one package')]
            }, {
                id: 'composite_legs', title: 'Legs', table: 'composite_leg', version: t.ver, child: true,
                ...rows(['Leg', 'Trade', 'Weight'], [['1', 'ACME-100201 Interest rate swap', '1.00'], ['2', 'ACME-100210 Cap or floor', '1.00']]),
                economic: true, digestGap: !DIGEST_FIXED
            }];
        default:
            return [{
                id: 'instrument', title: t.table ? tradeTypeName(t.table) : 'Instrument', table: t.table || '(none)', version: t.ver,
                fields: [
                    f('notional', 'Notional', t.notional, { ...E, type: 'money' }), f('currency', 'Currency', t.ccy, { ...E, type: 'ccy' }),
                    f('start_date', 'Start date', t.date, { ...E, type: 'date' }), f('end_date', 'End date', '2028-' + t.date.slice(5), { ...E, type: 'date' }),
                    f('description', 'Internal description', '')
                ]
            }];
    }
}
function tradeTypeName(table) { return table.replace(/_instrument$/, '').replace(/_/g, ' ').replace(/^./, c => c.toUpperCase()); }

/* The cards of the other aspects. */
function otherCards(t) {
    const f = (key, label, value, o = {}) => Object.assign({ key, label, value, economic: false, type: 'text' }, o);
    return {
        identifiers: {
            id: 'identifiers', title: 'Identifiers', table: 'trade_identifier', version: 1, child: true,
            columns: ['Scheme', 'Value'], rows: [['Firm trade id', t.code], ['ORE trade id', 'Trade_' + t.code.slice(-4)], ['Venue reference', t.channel === 'Manual' ? '' : 'VEN-' + t.code.slice(-5)]].filter(r => r[1])
        },
        roles: {
            id: 'roles', title: 'Party roles', table: 'trade_party_role', version: 1, child: true,
            columns: ['Role', 'Party'], rows: [['Executing trader', 'ada.lovelace'], ['Counterparty contact', cpty(t.cp).name]]
        },
        portfolios: {
            id: 'portfolios', title: 'Portfolios', table: 'trade_portfolio', version: 1, child: true,
            columns: ['Portfolio'], rows: [[t.product === 'fx' ? 'FX flow' : 'Rates and credit']]
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
        id: 'a1', at: day(t.ver * 6 + 4, 9, 41), kind: first === 'draft_capture' ? 'booked' : 'booked', activity: first, version: 1, ext: 1, actor: 'ada.lovelace',
        reason: 'trade.client_request', note: first === 'draft_capture' ? 'Recorded from a phone call, awaiting counterparty details' : 'Booked from the confirmation',
        diffs: [['trade_type', '', t.type], ['counterparty', '', cpty(t.cp).name], ['book', '', book(t.book).name], ['notional', '', money(t.notional, t.ccy)]]
    });
    if (t.ver >= 2) entries.push({
        id: 'a2', at: day(t.ver * 5, 14, 12), kind: 'amended', activity: 'amendment_reference', version: 2, ext: 1, actor: 'ada.lovelace',
        reason: 'trade.correction', note: 'Corrected the internal description', diffs: [['description', 'Hedge', 'Hedges the Q3 bond issue']]
    });
    if (t.ver >= 3) entries.push({
        id: 'a3', at: day(t.ver * 3, 11, 5), kind: 'amended', activity: 'amendment_economic', version: 3, ext: Math.min(2, t.ext), actor: 'ada.lovelace',
        reason: 'trade.upsize', note: 'Client asked for a larger notional', diffs: [['notional', money(t.notional * 0.8, t.ccy), money(t.notional, t.ccy)]], economic: true
    });
    if (t.ver >= 3) entries.push({
        id: 'a3c', at: day(t.ver * 3 - 0.5, 11, 40), kind: 'confirmed', activity: 'confirmation', version: 3, ext: Math.min(2, t.ext), actor: 'ores.trading.service',
        reason: 'trade.no_change', note: 'Matched by ' + cpty(t.cp).name, diffs: [], noVersion: true
    });
    if (t.channel === 'ORE import') entries.unshift({
        id: 'a0', at: day(t.ver * 6 + 4.1, 9, 5), kind: 'imported', activity: 'new_booking', version: 1, ext: 1, actor: 'ores.ore.service',
        reason: 'trade.client_request', note: 'Imported from an ORE portfolio file', diffs: [['ore_trade_id', '', 'Trade_' + t.code.slice(-4)]]
    });
    if (t.ver >= 4) entries.push({
        id: 'a4', at: day(t.ver * 2, 15, 30), kind: 'event', activity: 'authorisation', version: 3, ext: Math.min(2, t.ext), actor: 'grace.hopper',
        reason: 'trade.no_change', note: 'Released by Operations', diffs: [], noVersion: true
    });
    if (t.ver >= 5) entries.push({
        id: 'a5', at: day(2, 10, 20), kind: 'amended', activity: 'amendment_economic', version: 5, ext: t.ext, actor: 'ada.lovelace',
        reason: 'trade.restructure', note: 'New fixed rate agreed on the call', diffs: [['fixed_rate', '2.7500 %', '2.8500 %']], economic: true
    });
    if (t.state === 'cancelled') entries.push({
        id: 'a9', at: day(1, 9, 12), kind: 'cancelled', activity: 'novation', version: t.ver, ext: t.ext, actor: 'grace.hopper',
        reason: 'trade.client_request', note: 'Novated to Meridian Bank AG. Rebooked as ACME-100208.', diffs: [['state', 'live', 'cancelled']]
    });
    return entries;
}

/* ---------- the shell ---------- */
const AREAS = [['Home', '#'], ['Reference data', '#'], ['Trades', '../trade-page/'], ['Organisation', '#'], ['Requests', '#'], ['Operations', '#']];
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
        h('span', { class: 'me' }, avatar(ME, 's'), PEOPLE[ME].name, h('span', { class: 'access' }, ROLE)));
    const crumbs = h('nav', { class: 'crumbs', 'aria-label': 'Breadcrumb' },
        opts.crumbs.flatMap(([label, href], i) => [i ? h('span', { class: 'sep' }, '›') : null, href ? h('a', { href }, label) : h('span', null, label)]));
    const main = h('main', { class: 'page' }, crumbs);
    const pills = h('div', { class: 'pills' }, h('span', { class: 'pill' }, 'Tenant ' + TENANT), h('span', { class: 'pill' }, 'Party ' + PARTY),
        h('span', { class: 'pill' }, 'Prototype build'));
    const bar = h('div', { class: 'proto-bar' }, h('span', { class: 'label' }, opts.name),
        (opts.states || []).map(([label, change, on]) => h('a', { href: typeof change === 'string' ? change : qs(change), class: on ? 'on' : '' }, label)),
        opts.noRoles ? null : [h('span', { class: 'sep' }, '|'), h('span', { class: 'label' }, 'as'),
        ['trader', 'operations', 'reader'].map(r => h('a', { href: qs({ as: r }), class: r === ROLE ? 'on' : '' }, r))],
        h('span', { class: 'sep' }, '|'), h('span', { class: 'label' }, 'prototypes'),
        SIBLINGS.map(([dir, label]) => h('a', { href: '../' + dir + '/', title: label }, dir)),
        h('a', { href: '../trade-entry/', title: 'The structure-based booking and pricing prototype' }, 'trade-entry'));
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
function toast(msg, tone) {
    const old = $('.toast'); if (old) old.remove();
    const t = h('div', { class: 'toast', role: 'status' }, msg);
    document.body.append(t);
    setTimeout(() => t.remove(), 3800);
}
function pageHead({ title, lead, id, actions }) {
    return h('div', { class: 'head' }, h('div', null, h('h1', null, title), id ? h('div', { class: 'id' }, id) : null, lead ? h('p', { class: 'lead' }, lead) : null),
        h('div', { class: 'actions' }, actions));
}

/* ---------- refresh, workflows, pager ---------- */
let LOADED = new Date(NOW.getTime() - 4 * 60000);
function iso(d) { return new Date(d.getTime() + 3600000).toISOString().slice(0, 19) + '+01:00'; }
function refreshButton({ stale, onClick }) {
    const b = h('button', { class: 'btn' + (stale ? ' stale pulse' : ''), title: stale ? 'Data changed on the server. Refresh to see it.' : 'Refresh', onclick: onClick }, '↻ Refresh');
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
const PAGE_SIZES = [15, 25, 50];
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
        options.map(o => { const [v, l] = Array.isArray(o) ? o : [o, o]; return h('option', { value: v, selected: v === value }, l); }));
}

/* ---------- cards and fields ---------- */
function showValue(fd) {
    if (fd.type === 'money') return money(fd.value);
    if (fd.type === 'decimal' && fd.unit) return fd.value + ' ' + fd.unit;
    return ivalue(fd.value);
}
function tabs(list, current, hrefFor) {
    return h('nav', { class: 'tabs', role: 'tablist' }, list.map(([id, label]) => h('a', { href: hrefFor(id), class: id === current ? 'on' : '', role: 'tab' }, label)));
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
        const days = {};
        for (const e of shown) { const k = e.at.slice(0, 10); (days[k] = days[k] || []).push(e); }
        const buckets = Object.entries(days).sort((a, b) => a[0].localeCompare(b[0]));
        const chart = h('div', { class: 'chart', role: 'img', 'aria-label': 'Events over time' }, buckets.map(([day, list]) => {
            const counts = {}; list.forEach(e => counts[e.kind] = (counts[e.kind] || 0) + 1);
            return h('div', { class: 'bar', title: day + ': ' + list.length, onclick: () => { const t = wrap.querySelector('[data-day="' + day + '"]'); if (t) t.scrollIntoView({ block: 'center' }); } },
                Object.entries(counts).map(([k, n]) => h('i', { class: 'k-' + k, style: 'height:' + n * 18 + 'px', title: KINDS[k] + ': ' + n })));
        }));
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
            rail.append(h('div', { class: 'entry' + (e.current ? ' cur' : '') + (flash === e.id ? ' flash' : '') },
                h('div', { class: 'at', title: e.at.replace('T', ' ') }, e.at.slice(11, 19)),
                h('div', { class: 'dot' }, h('i', { title: e.current ? 'Current version' : '' })),
                h('div', { class: 'main' }, avatar(e.actor, 'm'), h('div', null,
                    h('div', { class: 'line1' }, chip(KINDS[e.kind], e.kind === 'cancelled' ? 'bad' : e.kind === 'event' ? 'flow' : e.kind === 'booked' ? 'acc' : 'warn'),
                        h('b', null, a ? a.label : e.activity), e.noVersion ? chip('no version', '', 'Operational authorisation moves neither version.') : chip('v' + e.version, 'v'),
                        a && a.economic && a.real ? chip('economic', 'acc', 'Moves the external version') : null,
                        a && a.confirmable ? chip('confirmable', '', 'A new confirmation is issued') : null,
                        e.economic && e.ext ? chip('external v' + e.ext, 'v') : null,
                        PEOPLE[e.actor] && PEOPLE[e.actor].service ? h('span', null, PEOPLE[e.actor].name) : person(e.actor),
                        h('span', { class: 'faint small' }, REASON_CODES.find(r => r[0] === e.reason) ? REASON_CODES.find(r => r[0] === e.reason)[1] : e.reason),
                        e.current ? h('span', { class: 'cur-emblem', title: 'Current version' }, '●') : null),
                    e.note ? h('div', { class: 'note' }, e.note) : null, det))));
        }
        wrap.append(chart, legend, rail);
    }
    draw();
    return wrap;
}

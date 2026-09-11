/* Product types, strategies and fixtures for the trade entry prototype.
 *
 * The product types are the eight real values of
 * ores.trading::domain::product_type, grouped by asset class, plus the
 * trade_type_code values that already have a dedicated Qt form in
 * InstrumentFormRegistry. See the catalogue task for the full table.
 *
 * The fixtures are transcribed from external/ore/examples/Products/Example_Trades.
 */

const ASSET_CLASSES = [
    { code: 'rates', name: 'Rates' },
    { code: 'fx', name: 'FX' },
    { code: 'credit', name: 'Credit' },
    { code: 'equity', name: 'Equity' },
    { code: 'commodity', name: 'Commodity' },
    { code: 'container', name: 'Container' }
];

/* widget: which specialised widget opens. Anything not named here falls
 * through to the generic form, which is a real path: trade_type_code is
 * free text and most of the 132 example trades have no dedicated form.
 * defaults: what a newly added component of this family starts with, so
 * the specialised widget opens on something a reader can react to. */
const PRODUCT_TYPES = [
    { code: 'swap', name: 'Swap', assetClass: 'rates', qtForm: 'SwapInstrumentForm',
      widget: 'schedule',
      defaults: {
          legType: 'Fixed', payer: 'true', rate: '3.25', dayCounter: 'A360',
          startDate: '2025-02-10', endDate: '2030-02-10', tenor: '6M',
          calendar: 'TARGET', convention: 'ModifiedFollowing', rule: 'Forward',
          paymentConvention: 'ModifiedFollowing'
      } },
    { code: 'fx', name: 'FX', assetClass: 'fx', qtForm: 'FxInstrumentForm' },
    { code: 'bond', name: 'Bond', assetClass: 'credit', qtForm: 'BondInstrumentForm' },
    { code: 'credit', name: 'Credit', assetClass: 'credit', qtForm: 'CreditInstrumentForm' },
    { code: 'equity', name: 'Equity', assetClass: 'equity', qtForm: 'EquityInstrumentForm' },
    { code: 'commodity', name: 'Commodity', assetClass: 'commodity', qtForm: 'CommodityInstrumentForm' },
    { code: 'composite', name: 'Composite', assetClass: 'container', qtForm: 'CompositeInstrumentForm', widget: 'composite' },
    { code: 'scripted', name: 'Scripted', assetClass: 'container', qtForm: 'ScriptedInstrumentForm' }
];

/* Trade type codes with a dedicated Qt form. Only FX has them today. */
const TRADE_TYPE_CODES = [
    { code: 'FxOption', productType: 'fx', widget: 'vanilla' },
    { code: 'FxBarrierOption', productType: 'fx', widget: 'barrier' },
    { code: 'FxDoubleBarrierOption', productType: 'fx', widget: 'barrier' },
    { code: 'FxEuropeanBarrierOption', productType: 'fx', widget: 'barrier' },
    { code: 'FxKIKOBarrierOption', productType: 'fx', widget: 'barrier' },
    { code: 'FxGenericBarrierOption', productType: 'fx', widget: 'barrier' },
    { code: 'FxDigitalOption', productType: 'fx' },
    { code: 'FxDigitalBarrierOption', productType: 'fx' },
    { code: 'FxTouchOption', productType: 'fx' },
    { code: 'FxDoubleTouchOption', productType: 'fx' },
    { code: 'FxAverageForward', productType: 'fx' },
    { code: 'FxTaRF', productType: 'fx' },
    { code: 'FxAccumulator', productType: 'fx' },
    { code: 'FxVarianceSwap', productType: 'fx' }
];

/* Swap and composite carry no type code form: the picker opens the
 * family form, which the prototype renders as a specialised widget. */
function widgetFor(productType, tradeTypeCode) {
    const typeCode = TRADE_TYPE_CODES.find(t => t.code === tradeTypeCode);
    if (typeCode && typeCode.widget) return typeCode.widget;
    const family = PRODUCT_TYPES.find(p => p.code === productType);
    return (family && family.widget) || 'generic';
}

function productTypeName(code) {
    const pt = PRODUCT_TYPES.find(p => p.code === code);
    return pt ? pt.name : code;
}

/* Strategies. Strategy mode fixes the leg set and refuses a structure
 * that breaks the invariant. Each invariant is a testable condition over
 * the legs, evaluated by checkStrategy() below.
 *
 * The set is provisional. See the task notes: the decision ticket that
 * owns the final set is still open. */
const STRATEGIES = [
    {
        code: 'straddle',
        name: 'Straddle',
        legs: 2,
        describe: 'One call and one put, same strike, same expiry, both bought.',
        invariant: legs => {
            const [a, b] = legs;
            if (a.optionType === b.optionType) return 'one leg must be a call and one a put';
            if (a.longShort !== 'Long' || b.longShort !== 'Long')
                return 'both legs must be bought';
            if (Number(a.strike) !== Number(b.strike)) return 'strikes must match';
            if (a.expiry !== b.expiry) return 'expiries must match';
            if (Number(a.notional) !== Number(b.notional)) return 'notionals must match';
            return null;
        }
    },
    {
        code: 'risk-reversal',
        name: 'Risk Reversal',
        legs: 2,
        describe: 'A bought call against a sold put, same expiry, different strikes.',
        invariant: legs => {
            const call = legs.find(l => l.optionType === 'Call');
            const put = legs.find(l => l.optionType === 'Put');
            if (!call || !put) return 'one leg must be a call and one a put';
            if (call.longShort !== 'Long' || put.longShort !== 'Short')
                return 'the call must be bought and the put sold';
            if (call.expiry !== put.expiry) return 'expiries must match';
            if (Number(call.strike) <= Number(put.strike))
                return 'the call strike must sit above the put strike';
            return null;
        }
    },
    {
        code: 'butterfly',
        name: 'Butterfly',
        legs: 3,
        describe: 'One call low, two sold at the middle strike, one call high.',
        invariant: legs => {
            const strikes = legs.map(l => Number(l.strike)).sort((x, y) => x - y);
            if (strikes.length !== 3) return 'a butterfly has exactly three strikes';
            const [low, mid, high] = strikes;
            if (Math.abs((mid - low) - (high - mid)) > 1e-9)
                return 'the strikes must be evenly spaced';
            if (legs.some(l => l.optionType !== 'Call')) return 'every leg must be a call';
            const atMid = legs.filter(l => Number(l.strike) === mid);
            if (atMid.length !== 1 || atMid[0].longShort !== 'Short')
                return 'the middle strike must be sold';
            return null;
        }
    },
    {
        code: 'strangle',
        name: 'Strangle',
        legs: 2,
        describe: 'A bought call above a bought put, same expiry, separated strikes.',
        invariant: legs => {
            const call = legs.find(l => l.optionType === 'Call');
            const put = legs.find(l => l.optionType === 'Put');
            if (!call || !put) return 'one leg must be a call and one a put';
            if (call.longShort !== 'Long' || put.longShort !== 'Long')
                return 'both legs must be bought';
            if (call.expiry !== put.expiry) return 'expiries must match';
            if (Number(call.strike) <= Number(put.strike))
                return 'the call strike must sit above the put strike';
            return null;
        }
    }
];

/* The four archetype widgets, named for what each one manages. */
const WIDGET_NAMES = {
    vanilla: 'Schedule and expiry',
    barrier: 'Barrier management',
    schedule: 'Two-leg schedule and cashflows',
    composite: 'Leg list',
    generic: 'Generic form'
};

/* --- Fixtures ------------------------------------------------------ */

/* From FX_Option_European.xml. */
function fxOptionFixture() {
    return {
        productType: 'fx',
        tradeTypeCode: 'FxOption',
        label: 'EURUSD 1y call',
        fields: {
            longShort: 'Long',
            optionType: 'Call',
            style: 'European',
            settlement: 'Cash',
            payOffAtExpiry: 'false',
            expiry: '2033-02-20',
            boughtCurrency: 'EUR',
            boughtAmount: '1000000',
            soldCurrency: 'USD',
            soldAmount: '1100000',
            strike: '1.10',
            notional: '1000000'
        },
        premium: { amount: '10900', currency: 'EUR', payDate: '2025-02-20' }
    };
}

/* From FX_Barrier_Option.xml. */
function fxBarrierFixture() {
    return {
        productType: 'fx',
        tradeTypeCode: 'FxBarrierOption',
        label: 'EURUSD 10y up-and-in call',
        fields: {
            longShort: 'Long',
            optionType: 'Call',
            style: 'European',
            settlement: 'Cash',
            payOffAtExpiry: 'false',
            expiry: '2033-02-20',
            boughtCurrency: 'EUR',
            boughtAmount: '1000000',
            soldCurrency: 'USD',
            soldAmount: '1100000',
            strike: '1.10',
            notional: '1000000',
            barrierType: 'UpAndIn',
            barrierLevel: '1.2',
            rebate: '0.0',
            fxIndex: 'FX-TR20H-EUR-USD'
        },
        premium: null
    };
}

/* From a leg of Cash_BondRepo_and_Bond.xml. */
function swapFixture() {
    return {
        productType: 'swap',
        tradeTypeCode: '',
        label: 'USD 1y fixed leg',
        fields: {
            legType: 'Fixed',
            payer: 'true',
            currency: 'USD',
            notional: '28371509.99',
            startDate: '2024-02-12',
            endDate: '2026-05-14',
            tenor: '1Y',
            calendar: 'US',
            convention: 'MF',
            rule: 'Forward',
            dayCounter: 'A360',
            paymentConvention: 'F',
            rate: '0.0178'
        },
        premium: null
    };
}

/* From Hybrid_CompositeTrade.xml, which is already a straddle: long call
 * and short put, one strike, one expiry. */
function compositeFixture() {
    return {
        productType: 'composite',
        tradeTypeCode: 'CompositeTrade',
        label: 'S&P 500 synthetic forward',
        fields: {
            currency: 'USD',
            notionalCalculation: 'Mean',
            underlying: 'RIC:.SPX',
            strike: '2147.56',
            quantity: '775',
            expiry: '2025-10-10'
        },
        components: [
            {
                productType: 'equity',
                tradeTypeCode: 'EquityOption',
                label: 'S&P 500 call',
                fields: {
                    longShort: 'Long', optionType: 'Call', style: 'European',
                    settlement: 'Cash', expiry: '2025-10-10', underlying: 'RIC:.SPX',
                    strike: '2147.56', quantity: '775', notional: '1662128.00',
                    currency: 'USD'
                },
                premium: null
            },
            {
                productType: 'equity',
                tradeTypeCode: 'EquityOption',
                label: 'S&P 500 put',
                fields: {
                    longShort: 'Short', optionType: 'Put', style: 'European',
                    settlement: 'Cash', expiry: '2025-10-10', underlying: 'RIC:.SPX',
                    strike: '2147.56', quantity: '775', notional: '1662128.00',
                    currency: 'USD'
                },
                premium: null
            }
        ]
    };
}

/* Trades already booked, offered to "form a structure from existing
 * trades". Held separately from the structure so the screen can show a
 * candidate list. */
function existingTrades() {
    return [
        { id: 'T-8841', productType: 'fx', tradeTypeCode: 'FxOption', label: 'EURUSD 6m put', notional: '5000000', ccy: 'EUR' },
        { id: 'T-8842', productType: 'fx', tradeTypeCode: 'FxOption', label: 'EURUSD 6m call', notional: '5000000', ccy: 'EUR' },
        { id: 'T-8850', productType: 'bond', tradeTypeCode: '', label: 'US Treasury 2031', notional: '27807597.78', ccy: 'USD' },
        { id: 'T-8851', productType: 'bond', tradeTypeCode: 'BondRepo', label: 'Repo against UST 2031', notional: '28371509.99', ccy: 'USD' },
        { id: 'T-8863', productType: 'equity', tradeTypeCode: 'EquityOption', label: 'S&P 500 3m call', notional: '1662128.00', ccy: 'USD' },
        { id: 'T-8870', productType: 'swap', tradeTypeCode: '', label: 'USD 1y fixed leg', notional: '28371509.99', ccy: 'USD' }
    ];
}

/* A structure as it arrives from the book: one strategy-mode deal with a
 * named group and a hedge leg, in the shape the knowledge page sets out. */
function openingStructure() {
    return {
        id: 'ST-1042',
        name: 'EURUSD Risk Reversal 6m',
        counterparty: 'CPTY',
        nettingSet: 'NS',
        book: 'FX-OPTIONS',
        tradeDate: '2025-02-10',
        mode: 'strategy',
        strategy: 'risk-reversal',
        version: 7,
        authorisation: 'Draft',
        initialNpv: {
            amount: '184320.55',
            currency: 'USD',
            asOf: '2025-02-10T17:00:00Z',
            snapshot: 'MDS-2025-02-10-1700',
            model: 'FxVanillaOption / GarmanKohlhagen',
            runId: 'RUN-2291'
        },
        audit: [
            { at: '2025-02-10T09:12:00Z', actor: 'marco', text: 'Structure created' },
            { at: '2025-02-10T09:31:00Z', actor: 'marco', text: 'Component T-8841 added' },
            { at: '2025-02-10T10:02:00Z', actor: 'marco', text: 'Component T-8842 added' },
            { at: '2025-02-10T16:44:00Z', actor: 'marco', text: 'Authorised' }
        ],
        groups: [
            {
                id: 'G-1',
                name: 'Vanilla Group',
                components: [
                    {
                        id: 'T-8841',
                        productType: 'fx',
                        tradeTypeCode: 'FxOption',
                        label: 'EURUSD 6m put',
                        fields: {
                            longShort: 'Short', optionType: 'Put', style: 'European',
                            settlement: 'Cash', payOffAtExpiry: 'false', expiry: '2025-08-10',
                            boughtCurrency: 'EUR', boughtAmount: '5000000',
                            soldCurrency: 'USD', soldAmount: '5450000',
                            strike: '1.09', notional: '5000000'
                        },
                        premium: { amount: '48000', currency: 'EUR', payDate: '2025-02-14' }
                    },
                    {
                        id: 'T-8842',
                        productType: 'fx',
                        tradeTypeCode: 'FxOption',
                        label: 'EURUSD 6m call',
                        fields: {
                            longShort: 'Long', optionType: 'Call', style: 'European',
                            settlement: 'Cash', payOffAtExpiry: 'false', expiry: '2025-08-10',
                            boughtCurrency: 'EUR', boughtAmount: '5000000',
                            soldCurrency: 'USD', soldAmount: '5600000',
                            strike: '1.12', notional: '5000000'
                        },
                        premium: { amount: '61500', currency: 'EUR', payDate: '2025-02-14' }
                    }
                ]
            },
            {
                id: 'G-2',
                name: 'Hedge legs',
                hedge: true,
                components: [
                    {
                        id: 'T-8843',
                        productType: 'fx',
                        tradeTypeCode: 'FxForward',
                        label: 'EURUSD 6m hedge',
                        fields: {
                            longShort: 'Short', optionType: 'Forward', style: 'European',
                            settlement: 'Cash', expiry: '2025-08-10',
                            boughtCurrency: 'USD', boughtAmount: '11180000',
                            soldCurrency: 'EUR', soldAmount: '10000000',
                            strike: '1.118', notional: '10000000'
                        },
                        premium: null
                    }
                ]
            }
        ]
    };
}

/* A tenor to whole months. Months are approximated; the prototype is not
 * a date library, and the schedule only has to show that the rules drive
 * it. Used by the schedule generator and covered by the self-check. */
function tenorMonths(tenor) {
    const m = /^(\d+)([MYD])$/.exec(String(tenor).toUpperCase());
    if (!m) return 0;
    const n = Number(m[1]);
    return { M: n, Y: n * 12, D: 0 }[m[2]];
}

/* What a newly added swap starts with, taken from the family table. */
function swapDefaults() {
    const family = PRODUCT_TYPES.find(p => p.code === 'swap');
    return (family && family.defaults) || {};
}

/* The accrual periods a set of schedule rules generates. The rules drive
 * the schedule rather than the schedule being stored, which is the point
 * the widget exists to show. 40 periods is a guard, not a rule: a runaway
 * tenor must not lock the page up. */
function schedulePeriods(rules) {
    const months = tenorMonths(rules.tenor);
    if (!rules.startDate || !rules.endDate || !months) return [];

    const end = new Date(rules.endDate);
    const periods = [];
    let cursor = new Date(rules.startDate);
    while (cursor < end && periods.length < 40) {
        const next = new Date(cursor);
        next.setMonth(next.getMonth() + months);
        const stop = next > end ? end : next;
        periods.push({
            start: cursor.toISOString().slice(0, 10),
            end: stop.toISOString().slice(0, 10)
        });
        cursor = stop;
    }
    return periods;
}

/* --- Self-check ------------------------------------------------------
 *
 * The strategy invariants and the tenor parser carry the prototype's
 * non-trivial logic, so they get a check. Open either page with
 * ?selfcheck=1 to run it; it fails loudly in the console and returns the
 * count of failures.
 */

function runSelfCheck() {
    const failures = [];

    const check = (name, condition) => {
        if (!condition) failures.push(name);
    };

    const leg = (optionType, longShort, strike, expiry, notional) => ({
        optionType, longShort, strike, expiry, notional
    });

    const strategy = code => STRATEGIES.find(s => s.code === code);
    const straddle = strategy('straddle');
    const riskReversal = strategy('risk-reversal');
    const butterfly = strategy('butterfly');

    check('straddle accepts a call and a put at one strike',
        straddle.invariant([leg('Call', 'Long', 100, '2025-08-10', 1000),
                            leg('Put', 'Long', 100, '2025-08-10', 1000)]) === null);
    check('straddle refuses two calls',
        straddle.invariant([leg('Call', 'Long', 100, '2025-08-10', 1000),
                            leg('Call', 'Long', 100, '2025-08-10', 1000)]) !== null);
    check('straddle refuses mismatched strikes',
        straddle.invariant([leg('Call', 'Long', 100, '2025-08-10', 1000),
                            leg('Put', 'Long', 105, '2025-08-10', 1000)]) !== null);

    check('risk reversal accepts a bought call over a sold put',
        riskReversal.invariant([leg('Call', 'Long', 112, '2025-08-10', 5000),
                                leg('Put', 'Short', 109, '2025-08-10', 5000)]) === null);
    check('risk reversal refuses a call below the put',
        riskReversal.invariant([leg('Call', 'Long', 108, '2025-08-10', 5000),
                                leg('Put', 'Short', 109, '2025-08-10', 5000)]) !== null);

    check('butterfly accepts evenly spaced strikes',
        butterfly.invariant([leg('Call', 'Long', 100, '2025-08-10', 1000),
                             leg('Call', 'Short', 105, '2025-08-10', 1000),
                             leg('Call', 'Long', 110, '2025-08-10', 1000)]) === null);
    check('butterfly refuses uneven spacing',
        butterfly.invariant([leg('Call', 'Long', 100, '2025-08-10', 1000),
                             leg('Call', 'Short', 105, '2025-08-10', 1000),
                             leg('Call', 'Long', 111, '2025-08-10', 1000)]) !== null);

    check('the widget for FxBarrierOption is the barrier widget',
        widgetFor('fx', 'FxBarrierOption') === 'barrier');
    check('an unknown type code falls through to the generic form',
        widgetFor('fx', 'FxSomethingElse') === 'generic');
    check('a swap opens the two-leg schedule',
        widgetFor('swap', '') === 'schedule');
    check('a product family with no form falls through to the generic form',
        widgetFor('credit', '') === 'generic');
    check('a composite opens the leg list',
        widgetFor('composite', '') === 'composite');
    check('a new swap carries the rules its schedule is built from',
        typeof swapDefaults === 'function' && tenorMonths(swapDefaults().tenor) > 0);
    check('the swap schedule generates accrual periods',
        typeof schedulePeriods === 'function'
        && schedulePeriods(swapDefaults()).length > 0);

    check('a 1Y tenor is twelve months', typeof tenorMonths === 'function' && tenorMonths('1Y') === 12);
    check('a 3M tenor is three months', typeof tenorMonths === 'function' && tenorMonths('3M') === 3);
    check('a malformed tenor is zero months', typeof tenorMonths === 'function' && tenorMonths('x') === 0);

    if (failures.length) {
        console.error(`prototype self-check: ${failures.length} failed`, failures);
    } else {
        console.info('prototype self-check: all passed');
    }
    return failures.length;
}

if (typeof window !== 'undefined' && window.location.search.includes('selfcheck=1')) {
    window.addEventListener('DOMContentLoaded', runSelfCheck);
}

/* The running valuation the pricing screen fakes. Ticks move the spots;
 * the vols and rates are the trader's to tweak. */
function marketState() {
    return {
        spots: { EURUSD: 1.1180, GBPUSD: 1.2640, USDJPY: 149.20 },
        vols: { EURUSD: 7.80, GBPUSD: 8.40, USDJPY: 9.10 },
        rates: { EUR: 2.15, USD: 4.33, GBP: 4.05 },
        ticking: true
    };
}

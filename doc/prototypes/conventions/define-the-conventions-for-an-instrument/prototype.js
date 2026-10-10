/* Define-the-conventions-for-an-instrument journey prototype. Self-contained:
 * plain JavaScript, mock data, no framework, no build step, and nothing that
 * outlives the page.
 *
 * A convention is the set of market terms an instrument is built on. The
 * person starts from the instrument they want to trade, not from the entity
 * name. Every field, every picker and every operation is the one the model
 * and the generated protocol header actually carry:
 *
 *   projects/ores.refdata/modeling/ores.refdata.<entity>.org
 *   projects/ores.refdata/api/include/ores.refdata.api/messaging/<entity>_protocol.hpp
 *
 * The worked example is the swap convention
 * (ores.refdata.swap_convention.org), drawn in full; the deposit convention
 * (ores.refdata.deposit_convention.org) is the simpler contrast. The
 * instrument families are the convention entities the person's submenu
 * actually offers.
 *
 * The operations this journey uses are the ones refdata answers:
 *   refdata.v1.<plural>.list|.get|.put|.delete|.put_many|.delete_many
 *   refdata.v1.history.get          the generic history request, with
 *                                   entity_type = ores.refdata.<entity>
 *
 * States are chosen from the query string, as the navigation prototype does:
 *   ?state=instrument|convention|terms|review|outcome|history|refused
 *   ?family=swap                    the instrument family, already chosen
 *   ?id=EUR-6M-SWAP-CONVENTIONS     the convention, already chosen
 *   ?q=EUR-6M                       the convention list search, as typed
 *   ?field=fixed_calendar           focus this field on the terms step
 *   ?value=BUSINESS                 override that field's mock value
 *   ?fail=1                         refuse the write; 1 is the missing
 *                                   lookup, and ?refusal=1 is the alias
 *   ?version=3                      the version the history step shows
 *   ?diff=1                         show the history field diff (default on)
 * Four-eyes states, from the book controls note (four-eyes.js draws them):
 *   ?state=waiting|decide|declined  a raised request waiting, the checker's
 *                                   decision screen, and a declined request
 *   ?actor=m.risk|o.ops|...         who answers on the decision screen (decide only)
 *   ?maker=h.desk|m.risk|...        who raised the request; the maker cannot decide it
 *   ?check=lookup                   make the term-lookup check fail
 * The bar mirrors the same states as buttons. */

(function () {
    'use strict';

    /* ------------------------------------------------------- mock refdata */

    /* One row per instrument family the person's own submenu offers, in the
       order the submenu lists them. Each names the convention entity it
       resolves to, which is the same plural the protocol header uses. */
    var FAMILIES = [
        { id: 'deposit', name: 'Deposit', ent: 'deposit_conventions',
          brief: 'A money-market deposit, or an IBOR index fixing.',
          fields: 'index_based, index, calendar, convention, day_count_fraction, end_of_month, settlement_days' },
        { id: 'fra', name: 'FRA', ent: 'fra_conventions',
          brief: 'A forward rate agreement.',
          fields: 'index' },
        { id: 'future', name: 'Future', ent: 'future_conventions',
          brief: 'An interest rate or overnight index future.',
          fields: 'index, date_generation_rule, netting_type, calendar, overnight_index_tenor' },
        { id: 'ois', name: 'OIS', ent: 'ois_conventions',
          brief: 'An overnight index swap.',
          fields: 'spot_lag, index, fixed_*, payment_lag, rule, payment_calendar, rate_cutoff' },
        { id: 'average-ois', name: 'Average OIS', ent: 'average_ois_conventions',
          brief: 'An averaging overnight index swap.',
          fields: 'spot_lag, fixed_tenor, fixed_*, index, on_tenor, rate_cutoff' },
        { id: 'swap', name: 'Swap', ent: 'swap_conventions',
          brief: 'A vanilla interest rate swap. The worked example.',
          fields: 'fixed_calendar, fixed_frequency, fixed_convention, fixed_day_count_fraction, index, float_frequency, sub_periods_coupon_type' },
        { id: 'tenor-basis', name: 'Tenor basis swap', ent: 'tenor_basis_swap_conventions',
          brief: 'Each leg pays a different tenor off its own index.',
          fields: 'pay_index, pay_frequency, receive_index, receive_frequency, spread_on_rec, include_spread, sub_periods_coupon_type, long_index, long_pay_tenor, short_index, short_pay_tenor, spread_on_short' },
        { id: 'tenor-basis-2', name: 'Tenor basis two swap', ent: 'tenor_basis_two_swap_conventions',
          brief: 'A two-sided tenor basis swap.',
          fields: 'pay_index, pay_frequency, receive_index, receive_frequency, spread_on_rec, include_spread, sub_periods_coupon_type' },
        { id: 'xccy-basis', name: 'Cross-currency basis swap', ent: 'cross_currency_basis_conventions',
          brief: 'Two legs, two currencies, each with its own index and fixing rules.',
          fields: 'settlement_days, settlement_calendar, roll_convention, flat_index, spread_index, eom, is_resettable, flat_tenor, spread_tenor, spread_payment_lag, flat_payment_lag, spread_include_spread, spread_lookback, spread_fixing_days, spread_rate_cutoff, spread_is_averaged, spread_observation_shift, flat_include_spread, flat_lookback, flat_fixing_days, flat_rate_cutoff, flat_is_averaged, flat_observation_shift' },
        { id: 'xccy-fix-float', name: 'Cross-currency fix-float', ent: 'cross_currency_fix_float_conventions',
          brief: 'One fixed leg against one floating leg in another currency.',
          fields: 'the two legs and their calendars and conventions' },
        { id: 'bond-yield', name: 'Bond yield', ent: 'bond_yield_conventions',
          brief: 'The compounding and price type a bond yield is quoted on.',
          fields: 'compounding, frequency, price_type, accuracy, max_evaluations, guess' },
        { id: 'cds', name: 'CDS', ent: 'cds_conventions',
          brief: 'A credit default swap.',
          fields: 'settlement_days, calendar, frequency, payment_convention, rule, day_count_fraction, settles_accrual, pays_at_default_time, upfront_settlement_days, last_period_day_count_fraction' },
        { id: 'fx-option', name: 'FX option', ent: 'fx_option_conventions',
          brief: 'The at-the-money and delta conventions an FX option is quoted on.',
          fields: 'fx_convention_id, atm_type, delta_type, switch_tenor, long_term_atm_type, long_term_delta_type, risk_reversal_in_favor_of, butterfly_style' },
        { id: 'bma-basis', name: 'BMA basis swap', ent: 'bma_basis_swap_conventions',
          brief: 'A municipal-index basis swap.',
          fields: 'index, bma_index, bma_payment_*, index_payment_*, index_settlement_days, overnight_lockout_days' },
        { id: 'cms-spread', name: 'CMS spread option', ent: 'cms_spread_option_conventions',
          brief: 'A constant-maturity-swap spread option.',
          fields: 'forward_start, spot_days, swap_tenor, fixing_days, calendar, day_count_fraction, roll_convention' },
        { id: 'zero', name: 'Zero coupon', ent: 'zero_conventions',
          brief: 'A zero-coupon yield curve.',
          fields: 'tenor_based, day_count_fraction, compounding, compounding_frequency, tenor_calendar, spot_lag, spot_calendar, roll_convention, end_of_month' },
        { id: 'inflation', name: 'Inflation swap', ent: 'inflation_swap_conventions',
          brief: 'A fixed rate against a zero-coupon inflation index.',
          fields: 'fix_calendar, fix_convention, day_count_fraction, index, interpolated, observation_lag, inflation_calendar, inflation_convention, publication_*, start_delay*' },
        { id: 'commodity-forward', name: 'Commodity forward', ent: 'commodity_forward_conventions',
          brief: 'One leg pays a fixed price, the other the spot price.',
          fields: 'spot_days, points_factor, advance_calendar, spot_relative, delivery_location, business_day_convention, outright' },
        { id: 'commodity-future', name: 'Commodity future', ent: 'commodity_future_conventions',
          brief: 'A listed commodity future and its option.',
          fields: 'contract_frequency, calendar, expiry_calendar, expiry_month_lag, one_contract_month, anchor_*, option_*, peak_index, off_peak_index, delivery_location, future_continuation_mappings, option_continuation_mappings' },
        { id: 'power-load', name: 'Intraday power load', ent: 'intraday_power_load_conventions',
          brief: 'Load profiles for intraday power.',
          fields: 'dated load profiles, or business day rules' },
        { id: 'ibor-index', name: 'IBOR index', ent: 'ibor_index_conventions',
          brief: 'A term IBOR index such as EURIBOR or LIBOR.',
          fields: 'fixing_calendar, day_count_fraction, settlement_days, business_day_convention, end_of_month' },
        { id: 'overnight-index', name: 'Overnight index', ent: 'overnight_index_conventions',
          brief: 'An overnight index such as EONIA, SONIA or SOFR.',
          fields: 'fixing_calendar, day_count_fraction, settlement_days' },
        { id: 'swap-index', name: 'Named swap index', ent: 'swap_index_conventions',
          brief: 'The swap convention a named swap index refers to.',
          fields: 'conventions, fixing_calendar' },
        { id: 'tenor', name: 'Tenor resolution', ent: 'tenor_conventions',
          brief: 'How a tenor label resolves to a date for one curve type.',
          fields: 'code, description, measured_from, resolution_algorithm' },
        { id: 'currency-pair', name: 'Currency pair', ent: 'currency_pair_conventions',
          brief: 'The quoting and date conventions for one pair, 1:1 with it.',
          fields: 'pair_code, pip_factor, tick_size, decimal_places, business_day_convention, spot_relative, end_of_month, advance_calendar' }
    ];

    /* The lists the pickers read, by the entity the convention's field
       references. The value stored is the code, which is what the convention
       row holds. */
    var LISTS = {
        day_count_fraction_type: {
            title: 'Day count fractions', plural: 'day_count_fraction_types',
            code: 'code', name: 'name',
            options: [['ACT/360', 'Actual/360'], ['ACT/365', 'Actual/365 Fixed'],
                      ['ACT/365.25', 'Actual/365.25'], ['30/360', '30/360 Bond Basis'],
                      ['30E/360', '30E/360'], ['ACT/ACT', 'Actual/Actual ISDA'],
                      ['1/1', 'One over one']]
        },
        business_day_convention_type: {
            title: 'Business day conventions', plural: 'business_day_convention_types',
            code: 'code', name: 'name',
            options: [['Following', 'Following'], ['ModifiedFollowing', 'Modified following'],
                      ['Preceding', 'Preceding'], ['ModifiedPreceding', 'Modified preceding'],
                      ['Unadjusted', 'Unadjusted'], ['HalfMonthModifiedFollowing', 'Half-month modified following']]
        },
        calendar_name: {
            title: 'Calendars', plural: 'calendar_names',
            code: 'code', name: 'description',
            options: [['TARGET', 'Euro area TARGET2'], ['US-FED', 'United States, Federal Reserve'],
                      ['UK-LON', 'United Kingdom, London'], ['JP-TOK', 'Japan, Tokyo'],
                      ['CH-ZUR', 'Switzerland, Zurich'], ['AU-SYD', 'Australia, Sydney'],
                      ['WeekendsOnly', 'Weekends only']]
        },
        tenor: {
            title: 'Tenors', plural: 'tenors',
            code: 'code', name: 'display_name',
            options: [['O/N', 'Overnight'], ['S/N', 'Spot next'], ['1W', 'One week'],
                      ['1M', 'One month'], ['3M', 'Three months'], ['6M', 'Six months'],
                      ['1Y', 'One year'], ['2Y', 'Two years'], ['5Y', 'Five years'],
                      ['10Y', 'Ten years'], ['30Y', 'Thirty years'], ['SPOT', 'Spot']]
        },
        floating_index_type: {
            title: 'Floating indices', plural: 'floating_index_types',
            code: 'code', name: 'description',
            options: [['EUR-EURIBOR-6M', 'Euro interbank offered rate, six months'],
                      ['EUR-EURIBOR-3M', 'Euro interbank offered rate, three months'],
                      ['USD-LIBOR-3M', 'US dollar LIBOR, three months'],
                      ['USD-SOFR', 'Secured overnight financing rate'],
                      ['GBP-SONIA', 'Sterling overnight index average'],
                      ['GBP-LIBOR-6M', 'Sterling LIBOR, six months'],
                      ['JPY-TONAR', 'Tokyo overnight average rate'],
                      ['CHF-SARON', 'Swiss average rate overnight']]
        },
        payment_frequency: {
            title: 'Payment frequencies', plural: 'payment_frequencies',
            code: 'code', name: 'name',
            options: [['Annual', 'Annual'], ['Semiannual', 'Semiannual'],
                      ['Quarterly', 'Quarterly'], ['Monthly', 'Monthly'],
                      ['Weekly', 'Weekly'], ['Daily', 'Daily'], ['Once', 'Once']]
        },
        sub_periods_coupon_type: {
            title: 'Sub-period coupon types', plural: 'sub_periods_coupon_types',
            code: 'code', name: 'name',
            options: [['Compounding', 'Compounding'], ['Averaging', 'Averaging']]
        },
        currency: {
            title: 'Currencies', plural: 'currencies',
            code: 'iso_code', name: 'name',
            options: [['EUR', 'Euro'], ['USD', 'US dollar'], ['GBP', 'Pound sterling'],
                      ['JPY', 'Japanese yen'], ['CHF', 'Swiss franc']]
        }
    };

    /* A reference the person may not edit from this journey: it is another
       refdata list, and correcting it happens on that list. */
    function ref(entity, label) {
        return { lookup: true, list: entity, label: label };
    }

    /* A value the person writes here. */
    function text() {
        return { lookup: false };
    }

    /* Every field the swap convention model carries, in model order, told
       apart by what writes it. Nothing is invented: the names are the
       model's own column names. */
    var SWAP_FIELDS = [
        { name: 'id', label: 'Id', group: 'Identity', kind: text(),
          hint: 'The natural key. Examples: EUR-6M-SWAP-CONVENTIONS.', required: true },
        { name: 'fixed_calendar', label: 'Fixed Calendar', group: 'Fixed leg',
          kind: ref('calendar_name', 'calendars') },
        { name: 'fixed_frequency', label: 'Fixed Frequency', group: 'Fixed leg',
          kind: ref('payment_frequency', 'payment frequencies'), required: true },
        { name: 'fixed_convention', label: 'Fixed Convention', group: 'Fixed leg',
          kind: ref('business_day_convention_type', 'business day conventions') },
        { name: 'fixed_day_count_fraction', label: 'Fixed Day Count Fraction', group: 'Fixed leg',
          kind: ref('day_count_fraction_type', 'day count fractions'), required: true },
        { name: 'index', label: 'Index', group: 'Floating leg',
          kind: ref('floating_index_type', 'floating indices'), required: true,
          hint: 'The floating-leg index. The swap convention names it here; it is the same index a named swap index convention points at.' },
        { name: 'float_frequency', label: 'Float Frequency', group: 'Floating leg',
          kind: ref('payment_frequency', 'payment frequencies'),
          hint: 'When absent the frequency is derived from the index tenor.' },
        { name: 'sub_periods_coupon_type', label: 'Sub-Periods Coupon Type', group: 'Floating leg',
          kind: ref('sub_periods_coupon_type', 'sub-period coupon types') }
    ];

    /* The simpler contrast: eight columns, most of them optional, and every
       field inherited from the index when index_based is set. */
    var DEPOSIT_FIELDS = [
        { name: 'id', label: 'Id', group: 'Identity', kind: text(),
          hint: 'Examples: USD-LIBOR-CONVENTIONS, EUR-EURIBOR-CONVENTIONS.', required: true },
        { name: 'index_based', label: 'Index Based', group: 'Index',
          kind: { lookup: false, check: true },
          hint: 'When true, index must be set and the remaining fields are optional.' },
        { name: 'index', label: 'Index', group: 'Index',
          kind: ref('floating_index_type', 'floating indices') },
        { name: 'calendar', label: 'Calendar', group: 'Settlement',
          kind: ref('calendar_name', 'calendars') },
        { name: 'convention', label: 'Convention', group: 'Settlement',
          kind: ref('business_day_convention_type', 'business day conventions') },
        { name: 'day_count_fraction', label: 'Day Count Fraction', group: 'Settlement',
          kind: ref('day_count_fraction_type', 'day count fractions') },
        { name: 'end_of_month', label: 'End Of Month', group: 'Settlement',
          kind: { lookup: false, check: true } },
        { name: 'settlement_days', label: 'Settlement Days', group: 'Settlement',
          kind: { lookup: false, spin: true } }
    ];

    /* Which plan a family's terms step uses. Only the two the journey draws
       are authored in full; the others are listed so the picker is honest
       about the whole catalogue, and their terms step says so. */
    var PLANS = {
        swap: { entity: 'swap_convention', label: 'Swap Convention', fields: SWAP_FIELDS },
        deposit: { entity: 'deposit_convention', label: 'Deposit Convention', fields: DEPOSIT_FIELDS }
    };

    /* The mock convention rows, keyed by family. A real list answers
       refdata.v1.<plural>.list, paged and ordered by id. */
    var ROWS = {
        swap: [
            { id: 'EUR-6M-SWAP-CONVENTIONS', by: 'm.okafor', at: '2026-09-14 11:02', version: 3,
              terms: 'annual fixed 30/360, EUR-EURIBOR-6M' },
            { id: 'USD-3M-SWAP-CONVENTIONS', by: 'j.smith', at: '2026-08-30 16:41', version: 2,
              terms: 'semiannual fixed ACT/360, USD-SOFR' },
            { id: 'GBP-6M-SWAP-CONVENTIONS', by: 'a.tanaka', at: '2026-07-02 09:15', version: 1,
              terms: 'annual fixed ACT/365, GBP-LIBOR-6M' }
        ],
        deposit: [
            { id: 'USD-LIBOR-CONVENTIONS', by: 'j.smith', at: '2026-09-01 08:20', version: 4,
              terms: 'index based on USD-LIBOR, ACT/360' },
            { id: 'EUR-EURIBOR-CONVENTIONS', by: 'm.okafor', at: '2026-08-19 14:33', version: 2,
              terms: 'index based on EUR-EURIBOR, ACT/360' }
        ],
        ois: [
            { id: 'EUR-EONIA-OIS-CONVENTIONS', by: 'm.okafor', at: '2026-09-10 10:10', version: 2,
              terms: 'spot lag 2, EUR-EONIA, annual fixed' }
        ],
        future: [
            { id: 'EUR-EURIBOR-3M-FUTURES', by: 'a.tanaka', at: '2026-08-05 12:00', version: 1,
              terms: 'IMM dates, EUR-EURIBOR-3M, netting' }
        ],
        'ibor-index': [
            { id: 'EUR-EURIBOR', by: 'm.okafor', at: '2026-09-02 09:05', version: 3,
              terms: 'TARGET fixing, ACT/360, T+2' }
        ],
        'overnight-index': [
            { id: 'USD-SOFR', by: 'j.smith', at: '2026-09-02 09:06', version: 1,
              terms: 'US-FED fixing, ACT/360, T+0' }
        ],
        'currency-pair': [
            { id: 'EUR/USD', by: 'j.smith', at: '2026-08-22 17:12', version: 6,
              terms: 'pip factor 10000, 5 dp, spot relative' }
        ]
    };

    /* A row for a family the prototype does not fill by hand, so the list is
       never falsely empty. It is marked as such. */
    function placeholder(family) {
        var plural = family.ent.slice(0, -1).toUpperCase().replace(/_/g, '-');
        return [{ id: 'TENANT-' + plural + '-1', by: 'tenant_admin', at: '2026-06-01 00:00',
                  version: 1, terms: 'not drawn by this prototype', placeholder: true }];
    }

    /* ------------------------------------------------ the worked example */

    /* The swap convention's saved versions, as
       refdata.v1.history.get would return them: the full render of each
       version, and the field-level diff against the one before. */
    var SWAP_HISTORY = [
        { version: 1, by: 'j.smith', at: '2026-06-01 09:12',
          values: { id: 'EUR-6M-SWAP-CONVENTIONS', fixed_calendar: 'TARGET',
                    fixed_frequency: 'Semiannual', fixed_convention: 'ModifiedFollowing',
                    fixed_day_count_fraction: 'ACT/360', index: 'EUR-EURIBOR-6M',
                    float_frequency: 'Semiannual', sub_periods_coupon_type: '' } },
        { version: 2, by: 'a.tanaka', at: '2026-08-11 15:40',
          values: { id: 'EUR-6M-SWAP-CONVENTIONS', fixed_calendar: 'TARGET',
                    fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing',
                    fixed_day_count_fraction: '30/360', index: 'EUR-EURIBOR-6M',
                    float_frequency: 'Semiannual', sub_periods_coupon_type: '' } },
        { version: 3, by: 'm.okafor', at: '2026-09-14 11:02',
          values: { id: 'EUR-6M-SWAP-CONVENTIONS', fixed_calendar: 'TARGET',
                    fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing',
                    fixed_day_count_fraction: '30/360', index: 'EUR-EURIBOR-6M',
                    float_frequency: 'Semiannual', sub_periods_coupon_type: 'Compounding' } }
    ];

    /* ------------------------------------------------------------------- state */

    var STEPS = [
        { id: 'instrument', title: 'Choose the instrument', short: 'Instrument',
          lead: 'Start from what you want to trade. The instrument decides which convention entity serves it.' },
        { id: 'convention', title: 'Choose the convention', short: 'Convention',
          lead: 'Pick one, or create one. A convention is the set of market terms the instrument is built on.' },
        { id: 'terms', title: 'Author its terms', short: 'Terms',
          lead: 'Every field the model carries. A term that is a pick is not edited here.' },
        { id: 'review', title: 'Review', short: 'Review',
          lead: 'Nothing is written until you confirm.' },
        { id: 'outcome', title: 'Outcome', short: 'Outcome', final: true, lead: '' },
        /* The refusal is a state of its own rather than a notice inside
           another step: what was refused, that the record is unchanged, and
           the way back to the step that raised it. It holds no rail place,
           because it is not a step of the walk. */
        { id: 'refused', title: 'Refused', short: 'Refused', final: true, offrail: true, lead: '' }
    ];

    /* The one refusal this journey must handle: a term references a value
       the list no longer holds. */
    var FAILURE_KINDS = {
        '1': 'missing-lookup'
    };

    /* A refdata subject and its plural, for the refusal's own words. */
    function subjectFor(entity) {
        return 'refdata.v1.' + entity + 's.put';
    }

    var S = {
        at: 0,
        /* The history is not a rail step. It is the Terms step's own record
           view, so the walk keeps five steps and the overlay is one boolean. */
        history: false,
        family: 'swap',
        id: 'EUR-6M-SWAP-CONVENTIONS',
        q: '',
        focus: '',
        values: {},
        touched: {},
        /* The kind of refusal in force, or '' for none. */
        fail: '',
        version: 3,
        diff: true,
        saved: false,
        savedVersion: 3,
        original: {},
        /* The four-eyes state in view, or '' for none. */
        fe: ''
    };

    var REFUSED_AT = 5;

    /* The refusal in force, as the kind the URL names. '' is none. */
    function hasFail() {
        return S.fail !== '' && FAILURE_KINDS[S.fail] !== undefined;
    }

    /* Whether the refusal in force is the missing lookup. */
    function failIsMissingLookup() {
        return hasFail() && FAILURE_KINDS[S.fail] === 'missing-lookup';
    }

    /* Set the refusal kind and the field it belongs to, so the value a person
       sees is the value the server refused. */
    function setFail(kind) {
        S.fail = kind;
        if (failIsMissingLookup()) {
            S.values.fixed_calendar = 'US-NYSE-2026';
            S.touched.fixed_calendar = true;
        }
    }

    function plan() {
        return PLANS[S.family];
    }

    function family() {
        return FAMILIES.filter(function (f) { return f.id === S.family; })[0];
    }

    function fields() {
        var p = plan();
        return p === undefined ? [] : p.fields;
    }

    function rowsFor(familyId) {
        var f = FAMILIES.filter(function (x) { return x.id === familyId; })[0];
        if (f === undefined) return [];
        if (ROWS[familyId] !== undefined) return ROWS[familyId];
        return placeholder(f);
    }

    function rowFor(id) {
        var all = [];
        FAMILIES.forEach(function (f) { all = all.concat(rowsFor(f.id)); });
        return all.filter(function (r) { return r.id === id; })[0];
    }

    function definedValues() {
        var out = {};
        fields().forEach(function (f) {
            if (S.values[f.name] !== undefined) out[f.name] = S.values[f.name];
        });
        return out;
    }

    /* The convention's versions, in order. The last one is the current form:
       it is marked saved or uncommitted against the version read from the
       list, which is what the diff needs to say. */
    function historyFor() {
        var out = SWAP_HISTORY.slice(0, SWAP_HISTORY.length - 1);
        out.push({ version: SWAP_HISTORY[SWAP_HISTORY.length - 1].version,
                   by: S.saved ? 'tenant_admin' : 'you',
                   at: S.saved ? '2026-09-14 11:02' : 'not saved yet',
                   values: definedValues(),
                   uncommitted: !S.saved });
        return out;
    }

    /* The values a version shows, with any field the version does not carry
       filled from the model's own example, so the render is never partial. */
    function valuesOf(v) {
        var base = {};
        fields().forEach(function (f) { base[f.name] = S.values[f.name]; });
        Object.keys(v.values).forEach(function (k) { base[k] = v.values[k]; });
        return base;
    }

    /* The version the step shows, and the one before it, so the field diff is
       drawn between two real versions. */
    function versionAt(n) {
        var h = historyFor();
        var at = -1;
        h.forEach(function (v, i) { if (v.version === n) at = i; });
        if (at < 0) at = h.length - 1;
        var now = h[at];
        var prev = at > 0 ? h[at - 1] : undefined;
        return {
            now: now,
            prev: prev,
            nowValues: valuesOf(now),
            prevValues: prev === undefined ? {} : valuesOf(prev)
        };
    }

    /* ---------------------------------------------------------------- utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function slug(name) {
        return name.replace(/_/g, '-');
    }

    /* A picker's options, as the entity list holds them. */
    function optionsFor(entity) {
        return LISTS[entity] === undefined ? [] : LISTS[entity].options;
    }

    function listTitle(entity) {
        return LISTS[entity] === undefined ? entity : LISTS[entity].title;
    }

    function listPlural(entity) {
        return LISTS[entity] === undefined ? entity : LISTS[entity].plural;
    }

    /* Whether the list a field references still holds the value. The refusal
       this journey must handle is a reference the list no longer holds. */
    function missing(field) {
        if (!field.kind.lookup) return false;
        var v = S.values[field.name];
        if (v === undefined || v === '') return false;
        return !optionsFor(field.kind.list).some(function (o) { return o[0] === v; });
    }

    function labelFor(field) {
        if (!field.kind.lookup) return S.values[field.name];
        var hit = optionsFor(field.kind.list).filter(function (o) { return o[0] === S.values[field.name]; })[0];
        return hit === undefined ? S.values[field.name] : hit[1];
    }

    /* A field the screen calls "set from the list" is one a picker fills and a
       person writes nowhere on this screen. */
    function source(field) {
        if (field.kind.set) return 'set';
        return field.kind.lookup ? 'lookup' : 'text';
    }

    function toggleCheck(name) {
        S.values[name] = S.values[name] === true ? false : true;
        S.touched[name] = true;
    }

    function setValue(name, value) {
        S.values[name] = value;
        S.touched[name] = true;
    }

    function reportList(field) {
        return field.kind.lookup ? listPlural(field.kind.list) : 'this record';
    }

    /* ------------------------------------------------- the instrument picker */

    function instrumentStep() {
        return '<div>' +
            '<div class="field"><span class="lbl">Instrument family</span>' +
            '<input data-focus="q" data-q="1" placeholder="Filter the families, e.g. swap" value="' +
            esc(S.q) + '"></div>' +
            '<div class="families">' + filterFamilies() + '</div>' +
            footerHint('The list is the convention entities this tenant actually keeps. A family with no ' +
                'convention yet opens the terms step with an empty form.') +
            '</div>';
    }

    function filterFamilies() {
        var q = S.q.trim().toLowerCase();
        var list = FAMILIES.filter(function (f) {
            if (q === '') return true;
            return f.name.toLowerCase().indexOf(q) >= 0 ||
                f.ent.indexOf(q.replace(/\s+/g, '_')) >= 0 ||
                f.brief.toLowerCase().indexOf(q) >= 0;
        });
        if (list.length === 0) return '<div class="panel-soft" style="grid-column:1/-1">' +
            '<div class="none" style="color:var(--ink-faint)">No instrument family matches.</div></div>';
        return list.map(function (f) {
            var n = rowsFor(f.id).length;
            var on = S.family === f.id;
            return '<button type="button" class="family' + (on ? ' on' : '') + '"' +
                ' data-act="family" data-family="' + esc(f.id) + '"' +
                (on ? ' aria-current="true"' : '') + '>' +
                '<span class="nm">' + esc(f.name) +
                '<span class="n">' + n + (n === 1 ? ' convention' : ' conventions') + '</span></span>' +
                '<p>' + esc(f.brief) + '</p>' +
                '<div class="ent">conventions live in <span class="mono">refdata.' +
                esc(f.ent) + '</span></div></button>';
        }).join('');
    }

    function footerHint(say) {
        return '<div class="gap"><b>Note.</b> A convention is a lookup, so the family picks the entity and ' +
            'the entity picks the operation: <span class="code">refdata.v1.' +
            esc(family() === undefined ? '<plural>' : family().ent) + '.list</span>. ' + esc(say) + '</div>';
    }

    /* -------------------------------------------------- the convention list */

    function visibleRows() {
        var q = S.q.trim().toLowerCase();
        var f = family();
        var list = rowsFor(S.family);
        if (q !== '') {
            list = list.filter(function (r) {
                return r.id.toLowerCase().indexOf(q) >= 0 ||
                    (r.terms !== undefined && r.terms.toLowerCase().indexOf(q) >= 0);
            });
        }
        if (f === undefined || f.placeholder) return list;
        return list;
    }

    function conventionStep() {
        var f = family();
        if (f === undefined) return '<div class="notice info">Choose an instrument family first.</div>';
        var list = visibleRows();
        var body = list.length === 0 ?
            '<tr><td colspan="5" class="none">No convention matches.</td></tr>' :
            list.map(function (r) {
                var on = r.id === S.id;
                return '<tr class="pick' + (on ? ' on' : '') + '" data-act="row" data-id="' + esc(r.id) + '">' +
                    '<td><span class="key">' + esc(r.id) + '</span>' +
                    '<span class="sub">' + esc(r.terms) + '</span></td>' +
                    '<td class="v">' + esc(r.by) + '</td>' +
                    '<td class="v">' + esc(r.at) + '</td>' +
                    '<td class="v">v' + esc(r.version) + '</td>' +
                    '<td class="acts">' +
                    '<button type="button" class="btn ghost small" data-act="edit" data-id="' + esc(r.id) + '">Edit</button>' +
                    '<button type="button" class="btn ghost small" data-act="history" data-id="' + esc(r.id) + '">History</button>' +
                    '</td></tr>';
            }).join('');

        var drawn = plan() !== undefined;
        return '<div>' +
            '<div class="field"><span class="lbl">Search ' + esc(f.name) + ' conventions</span>' +
            '<input data-focus="q" data-q="1" placeholder="' + esc(f.ent) + ' id, e.g. EUR-6M" value="' + esc(S.q) + '">' +
            '<div class="hint">The filter takes <b>id_one_of</b> only: ' +
            '<span class="mono">refdata.v1.' + esc(f.ent) + '.list</span> filters by id, not by a term.</div></div>' +
            '<table class="rows"><thead><tr>' +
            '<th>Convention</th><th>Modified by</th><th>Recorded at</th><th>Version</th><th class="acts"></th>' +
            '</tr></thead><tbody>' + body + '</tbody></table>' +
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="back">Back</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="new" data-id="">' +
            'New ' + esc(f.name) + ' convention</button></div>' +
            (drawn ? '' : gap('This prototype draws the terms step of the <b>swap</b> and <b>deposit</b> ' +
                'conventions in full. ' + esc(f.name) + ' convention rows are listed here so the picker ' +
                'is honest about the catalogue, but its terms step is not drawn.')) +
            '</div>';
    }

    /* ------------------------------------------------------- the terms step */

    function groupsOf(list) {
        var out = [];
        list.forEach(function (f) {
            var g = out.filter(function (x) { return x.title === f.group; })[0];
            if (g === undefined) { g = { title: f.group, fields: [] }; out.push(g); }
            g.fields.push(f);
        });
        return out;
    }

    function fieldMarkup(field) {
        var name = field.name;
        var val = S.values[name] === undefined ? '' : S.values[name];
        var src = source(field);
        var srcTag = '<span class="src ' + src + '">' +
            (src === 'lookup' ? 'from ' + esc(listTitle(field.kind.list).toLowerCase()) : 'free text') + '</span>';
        var focus = S.focus === name ? ' data-focus="' + esc(name) + '"' : '';

        if (field.kind.lookup) {
            var bad = missing(field);
            if (bad) {
                return '<div class="field span2"><span class="lbl">' + esc(field.label) + srcTag + '</span>' +
                    '<div class="ref missing"><span class="val missing">' + esc(val) + '</span>' +
                    '<button type="button" class="to" data-act="openlist" data-list="' + esc(field.kind.list) + '">' +
                    'open ' + esc(reportList(field)) + '</button></div>' +
                    '<div class="hint" style="color:var(--bad)">Not in <b>' + esc(field.kind.list) +
                    '</b>. This journey cannot create it: the list owns it.</div></div>';
            }
            var opts = optionsFor(field.kind.list).map(function (o) {
                return '<option value="' + esc(o[0]) + '"' + (o[0] === val ? ' selected' : '') + '>' +
                    esc(o[0]) + ' \u2014 ' + esc(o[1]) + '</option>';
            }).join('');
            return '<div class="field"><span class="lbl">' + esc(field.label) + srcTag + '</span>' +
                '<select data-f="' + esc(name) + '"' + focus + '>' +
                (val === '' ? '<option value="" selected>\u2014 none \u2014</option>' : '') + opts + '</select>' +
                (field.hint === undefined ? '' : '<div class="hint">' + field.hint + '</div>') +
                '<div class="hint"><button type="button" class="btn ghost small" style="margin-left:-11px" ' +
                'data-act="openlist" data-list="' + esc(field.kind.list) + '">Edit ' +
                esc(listTitle(field.kind.list)) + '\u2026</button></div></div>';
        }

        if (field.kind.check) {
            return '<div class="field"><span class="lbl">' + esc(field.label) + srcTag + '</span>' +
                '<label class="checkline"><input type="checkbox" data-f="' + esc(name) + '"' +
                (val === true ? ' checked' : '') + '>' +
                '<span>' + esc(field.hint === undefined ? field.label : field.hint) + '</span></label></div>';
        }

        if (field.kind.spin) {
            return '<div class="field"><span class="lbl">' + esc(field.label) + srcTag + '</span>' +
                '<input type="number" min="0" data-f="' + esc(name) + '"' + focus + ' value="' + esc(val) + '">' +
                (field.hint === undefined ? '' : '<div class="hint">' + field.hint + '</div>') + '</div>';
        }

        var disabled = field.name === 'id' && S.id !== '' && rowFor(S.id) !== undefined;
        return '<div class="field"><span class="lbl">' + esc(field.label) + srcTag + '</span>' +
            '<input data-f="' + esc(name) + '"' + focus + (disabled ? ' disabled' : '') +
            ' placeholder="' + esc(field.hint === undefined ? '' : '') + '" value="' + esc(val) + '">' +
            (field.hint === undefined ? '' : '<div class="hint">' + field.hint + '</div>') +
            (disabled ? '<div class="hint">The id is the key. A different id is a different convention.</div>' : '') +
            '</div>';
    }

    function missingFields() {
        return fields().filter(missing);
    }

    function termsStep() {
        var p = plan();
        var f = family();
        if (p === undefined) {
            return '<div class="notice warn">This prototype does not draw the terms step for ' +
                esc(f === undefined ? 'this family' : f.name) + '. ' +
                'Its entity is <span class="mono">' + esc(f === undefined ? '' : f.ent) + '</span>. ' +
                'Return to the picker and choose Swap or Deposit.</div>';
        }
        var blocks = groupsOf(p.fields);
        var body = blocks.map(function (g) {
            var overlay = {};
            if (S.family === 'swap' && g.title === 'Fixed leg') {
                overlay = {
                    note: 'The model carries no leg objects. The <span class="mono">fixed_*</span> columns ' +
                        'are the fixed leg and <span class="mono">index</span> and ' +
                        '<span class="mono">float_*</span> are the floating leg, so this grouping is the ' +
                        'screen\'s, not the row\'s.',
                    a: 'Fixed Leg', b: 'Floating Leg',
                    axis: 'fixed leg terms',
                    first: p.fields.filter(function (x) { return x.group === 'Fixed leg'; }).length,
                    arrow: 'index, float_frequency, sub_periods_coupon_type',
                    who: 'Each is a column on the one convention row.'
                };
            }
            var inner = g.fields.map(fieldMarkup).join('');
            var pair = '';
            if (overlay.note !== undefined) {
                pair = '<div class="panel-soft" style="margin-bottom:18px">' +
                    '<div class="grid2">' +
                    '<div><div class="lbl" style="font-size:12px;color:var(--ink-dim);margin-bottom:6px">' +
                    '<b>' + esc(overlay.a) + '</b></div><div class="hint" style="margin:0">' +
                    overlay.first + ' columns</div></div>' +
                    '<div><div class="lbl" style="font-size:12px;color:var(--ink-dim);margin-bottom:6px">' +
                    '<b>' + esc(overlay.b) + '</b></div><div class="hint" style="margin:0">' +
                    esc(overlay.arrow) + '</div></div></div>' +
                    '<div class="hint" style="margin-top:10px">' + overlay.note + '</div></div>';
            }
            return '<fieldset><legend>' + esc(g.title) + '</legend>' + pair +
                '<div class="grid2">' + inner + '</div></fieldset>';
        }).join('');

        var refusal = failIsMissingLookup() ? refusalNotice() : '';
        var saved = S.saved ? '<div class="notice success">Version ' + esc(S.version) +
            ' is saved. The terms below are what a new trade reads.</div>' : '';

        return '<div>' + saved + refusal +
            '<div class="field span2"><span class="lbl">Convention</span>' +
            '<div class="panel-soft"><span class="mono">' + esc(p.entity) + '</span>' +
            '<span class="hint" style="margin-left:10px">one row in <span class="mono">refdata.' +
            esc(f.ent) + '</span>, keyed by <span class="mono">id</span></span></div></div>' +
            body +
            '<div class="gap"><b>Why the badges.</b> ' +
            'A <b>from the list</b> badge means the value is a pick from another refdata list, and a person ' +
            'must not edit that list from here: the screen links to it instead. ' +
            'A <b>free text</b> badge means this convention is the only place the value lives. ' +
            'The stored value is the list\'s code, never its label.</div>' +
            (S.family === 'swap' ? swapUnderlying() : '') +
            '</div>';
    }

    /* How a convention declares what it is built on. For the swap convention
       the model carries no leg table and no FK: the index is a name in the
       row, and the ORE document it feeds is where a person sees the legs. */
    function swapUnderlying() {
        return '<div class="gap"><b>Where the underlyings are.</b> ' +
            'The swap convention declares them as terms, not as child rows: ' +
            '<span class="code">index</span> names the floating-leg index and the ' +
            '<span class="code">fixed_*</span> columns describe the fixed leg. ' +
            'The model declares no foreign key on <span class="code">index</span>, so the server ' +
            'stores any text. The cross-currency basis convention reaches the same idea with ' +
            '<span class="code">flat_index</span> and <span class="code">spread_index</span>; the ' +
            'tenor basis convention with <span class="code">pay_index</span> and ' +
            '<span class="code">receive_index</span>.</div>';
    }

    function refusalNotice() {
        var miss = missingFields();
        var lines = miss.map(function (f) {
            return '<li><span class="mono">' + esc(f.name) + '</span> = ' +
                '<span class="mono">' + esc(S.values[f.name]) + '</span> is not in ' +
                '<span class="mono">refdata.' + esc(f.kind.list) + '</span>. ' +
                'The list holds ' + optionsFor(f.kind.list).length + ' values; none has this code.</li>';
        }).join('');
        if (lines === '') {
            lines = '<li>This convention references a lookup the list no longer holds.</li>';
        }
        return '<div class="notice error"><b>The server refused the write.</b> ' +
            '<span class="mono">refdata.v1.' + esc(family() === undefined ? '' : family().ent) + '.put</span> ' +
            'answered <span class="mono">result.success = false</span>:' +
            '<ul>' + lines + '</ul>' +
            'Pick a value the list holds, or correct the list. This journey does not create a ' +
            'list value: <span class="mono">refdata.' + esc(miss.length > 0 ? miss[0].kind.list : '') +
            '.put</span> belongs to the list\'s own journey.</div>';
    }

    /* ------------------------------------------------------ the review step */

    function reviewStep() {
        var p = plan();
        if (p === undefined) {
            return '<div class="notice warn">Nothing to review. Choose Swap or Deposit on the instrument step.</div>';
        }
        var groups = groupsOf(p.fields);
        var dl = groups.map(function (g) {
            return '<dt style="grid-column:1/-1;color:var(--ink);font-weight:600;margin-top:10px">' +
                esc(g.title) + '</dt>' +
                g.fields.map(function (f) {
                    return '<dt>' + esc(f.label) + '</dt><dd>' + esc(f.kind.check ?
                        (S.values[f.name] === true ? 'yes' : 'no') : labelFor(f)) + '</dd>';
                }).join('');
        }).join('');

        var changed = Object.keys(S.touched).filter(function (k) { return S.touched[k]; });
        var delta = changed.length === 0 ? 'A new convention. Every field is new.' :
            changed.length + ' field' + (changed.length === 1 ? '' : 's') + ' differ from the version read.';

        var miss = missingFields();
        var block = miss.length > 0 ?
            '<div class="notice error">' + miss.length + ' referenced value' +
            (miss.length === 1 ? '' : 's') + ' cannot be written. The write will be refused.</div>' : '';

        var f = family();
        return '<div>' + reviewGate() + block +
            '<div class="notice info">This is what will change. The write is one request: ' +
            '<span class="mono">refdata.v1.' + esc(f.ent) + '.put</span> with ' +
            '<span class="mono">change.write</span>, the precondition, and the intent.</div>' +
            '<dl class="reviewgrid">' + dl + '</dl>' +
            '<div class="panel-soft" style="margin-top:18px">' +
            '<div class="hint" style="margin:0">' + esc(delta) + '</div></div>' +
            '<label class="failtoggle"><input type="checkbox" data-fail="1"' + (hasFail() ? ' checked' : '') +
            '> Prototype: make the referenced lookup missing</label>' +
            '</div>';
    }

    /* ----------------------------------------------------- the outcome step */

    function outcomeStep() {
        if (hasFail() && missingFields().length > 0) {
            return '<div class="outcome">' +
                '<div class="mark bad">\u2715</div>' +
                '<div class="what">Nothing was written</div>' +
                '<div class="say">The write was refused. The record keeps its version ' + esc(S.version) +
                ' and the terms you typed are still on the Terms step.</div></div>' +
                '<div class="stepfoot"><button type="button" class="btn ghost" data-act="state" ' +
                'data-state="terms">Back to the terms</button>' +
                '<button type="button" class="btn" data-act="state" data-state="history">Open the history</button></div>';
        }
        var f = family();
        return '<div class="outcome">' +
            '<div class="mark ok">\u25cf</div>' +
            '<div class="what">' + esc(S.values.id) + (FE.S.applied ? ' is approved and saved as version ' : ' is saved as version ') + esc(S.savedVersion) + '</div>' +
            '<div class="say">A trade that picks ' + esc(f === undefined ? 'this family' : f.name) +
            ' reads these terms.</div></div>' +
            '<div class="nextcards" style="margin-top:20px">' +
            '<button type="button" class="nextcard" data-act="state" data-state="history">' +
            '<span class="nm">See its history</span><p>The versions and the field diff.</p></button>' +
            '<button type="button" class="nextcard" data-act="state" data-state="convention">' +
            '<span class="nm">Back to the list</span><p>Another convention for the same instrument.</p></button>' +
            '<button type="button" class="nextcard" data-act="state" data-state="instrument">' +
            '<span class="nm">Another instrument</span><p>Start from the family again.</p></button></div>';
    }

    /* ---------------------------------------------------- the refused step */

    /* The refusal as its own screen: what was refused, that the record keeps
       its version, and the way back to the step that raised it. */
    function refusedStep() {
        var p = plan();
        var f = family();
        var entity = p === undefined ? (f === undefined ? '' : f.ent) : p.entity;
        var miss = missingFields();
        var who = STEPS[2];

        var what = miss.length === 0 ?
            '<li>The write was refused. No field names a value the list no longer holds, ' +
            'so the server refused it for another reason.</li>' :
            miss.map(function (m) {
                return '<li><span class="mono">' + esc(m.name) + '</span> = ' +
                    '<span class="mono">' + esc(S.values[m.name]) + '</span> is not in ' +
                    '<span class="mono">refdata.' + esc(m.kind.list) + '</span>. ' +
                    'That list holds ' + optionsFor(m.kind.list).length + ' values; none has this code.</li>';
            }).join('');

        return '<div>' +
            '<div class="outcome">' +
            '<div class="mark bad">\u2715</div>' +
            '<div class="what">The write was refused</div>' +
            '<div class="say">' + esc(entity === '' ? 'The convention' : entity) +
            ' was not changed. It keeps version ' + esc(S.version) + '.</div></div>' +
            '<div class="notice error" style="margin-top:20px">' +
            '<b><span class="mono">' + esc(subjectFor(entity)) + '</span> answered ' +
            '<span class="mono">result.success = false</span>:</b>' +
            '<ul>' + what + '</ul></div>' +
            '<div class="panel-soft">' +
            '<dl class="reviewgrid">' +
            '<dt>Convention</dt><dd>' + esc(S.values.id === undefined || S.values.id === '' ?
                '(a new convention)' : S.values.id) + '</dd>' +
            '<dt>Version</dt><dd>' + esc(S.version) + ' \u2014 unchanged by this attempt</dd>' +
            '<dt>Raised on</dt><dd>' + esc(who.title) + '</dd>' +
            '<dt>Refusal kind</dt><dd>' +
            esc('?fail=' + (S.fail === '' ? '1' : S.fail) + ' \u2014 ' +
                (failIsMissingLookup() ? 'a term references a value the list does not hold' :
                    'the server refused the write')) + '</dd>' +
            '</dl></div>' +
            '<div class="stepfoot" style="border-top:none;padding-top:1rem">' +
            '<button type="button" class="btn ghost" data-act="clear-fail">Clear the refusal</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="state" data-state="terms">' +
            'Back to the terms</button></div>' +
            '<div class="gap"><b>Where the fix lives.</b> This journey does not create a list value. ' +
            (miss.length === 0 ? '' :
                '<span class="code">refdata.' + esc(miss[0].kind.list) + '.put</span> belongs to that ' +
                'list\'s own journey, and the person reaches it from the link beside the field. ') +
            'Correcting the list here would make one convention the owner of a shared term.</div>' +
            '</div>';
    }

    /* ----------------------------------------------------- the history step */

    function diffRows(nowValues, prevValues, list) {
        return list.map(function (f) {
            var a = prevValues[f.name];
            var b = nowValues[f.name];
            var s = function (v) { return v === undefined || v === '' ? '\u2014' : String(v); };
            var cls, cell;
            if (a === undefined) {
                cls = 'added';
                cell = '<span class="now">' + esc(s(b)) + '</span>';
            } else if (String(a) !== String(b)) {
                cls = 'changed';
                cell = '<span class="was">' + esc(s(a)) + '</span> \u2192 ' +
                    '<span class="now">' + esc(s(b)) + '</span>';
            } else {
                cls = 'same';
                cell = '<span class="same">' + esc(s(b)) + '</span>';
            }
            return '<div class="diffrow ' + cls + '"><dt>' + esc(f.label) + '</dt><dd>' + cell + '</dd></div>';
        }).join('');
    }

    function historyStep() {
        var p = plan();
        if (p === undefined) {
            return '<div class="notice warn">The history step is drawn for the swap convention. ' +
                'Choose Swap on the instrument step.</div>';
        }
        var v = versionAt(S.version);
        var now = v.now;
        var prev = v.prev;

        /* Which fields the diff covers: the union of both renders, in the
           model's own order. */
        var list = p.fields.slice();
        Object.keys(v.prevValues).forEach(function (k) {
            if (!list.some(function (f) { return f.name === k; })) {
                list.push({ name: k, label: k });
            }
        });

        var tag = '<span class="tag' + (now.uncommitted ? '' : ' now') + '">' +
            (now.uncommitted ? 'not saved yet' : 'version ' + now.version) + '</span>' +
            '<span>by <b>' + esc(now.by) + '</b> at <span class="mono">' + esc(now.at) + '</span></span>' +
            '<span class="hint" style="margin:0">' +
            (prev === undefined ? 'the oldest version; nothing before it' :
                S.diff ? 'with the field diff against version ' + prev.version : 'field diff hidden') +
            '</span>';

        var body;
        if (S.diff) {
            body = '<div class="panel-soft" style="padding:10px 16px">' +
                diffRows(v.nowValues, v.prevValues, list) + '</div>';
        } else {
            body = '<dl class="reviewgrid">' + p.fields.map(function (f) {
                return '<dt>' + esc(f.label) + '</dt><dd>' + esc(
                    f.kind.check ? (v.nowValues[f.name] === true ? 'yes' : 'no') : v.nowValues[f.name]) + '</dd>';
            }).join('') + '</dl>';
        }

        var pick = historyFor().map(function (h) {
            return '<button type="button" class="btn ghost small' +
                (h.version === S.version ? ' on' : '') + '" data-act="version" data-version="' + h.version + '">' +
                (h.uncommitted ? 'now' : 'v' + h.version) + '</button>';
        }).join('');

        return '<div>' +
            '<div class="notice info">Every refdata record keeps its versions. Both the versions your ' +
            'convention writes and this term\'s own ' +
            '<span class="mono">refdata.v1.' + esc(family().ent) + '_versions.list</span> are read through one ' +
            'request: <span class="mono">refdata.v1.history.get</span> with ' +
            '<span class="mono">entity_type = ores.refdata.' + esc(p.entity) + '</span> and the id as ' +
            '<span class="mono">entity_id</span>.</div>' +
            '<div class="vers">' + tag + '</div>' +
            '<div class="stepfoot" style="margin-top:0;border-top:none;padding-top:0;margin-bottom:14px">' +
            '<span class="label" style="color:var(--ink-faint);font-size:12px">version</span>' + pick +
            '<button type="button" class="btn ghost small ml-auto" data-act="diff">' +
            (S.diff ? 'hide the diff' : 'show the diff') + '</button></div>' +
            body +
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="state" data-state="terms">Back to the terms</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="state" data-state="outcome">' +
            'Close</button></div>' +
            '<div class="gap"><b>Revert.</b> A revert is not a history operation. It reads the old version and ' +
            'writes its values as a new version, through ' +
            '<span class="code">refdata.v1.' + esc(family().ent) + '.put</span> with a new change reason.</div>' +
            (now.uncommitted ?
                '<div class="gap"><b>Drawn, but not provided.</b> The <b>now</b> version above is this ' +
                'prototype\'s own draft. The server has no draft version: ' +
                '<span class="code">refdata.v1.history.get</span> returns saved versions only, so a real ' +
                'screen shows <b>now</b> only after the write.</div>' : '') +
            '</div>';
    }

    function gap(say) {
        return '<div class="gap">' + say + '</div>';
    }

    /* ------------------------------------------------------------ the header */

    function stepHeader() {
        var f = family();
        if (f === undefined) return '';
        var bits = [f.name];
        if (S.values.id !== undefined && S.values.id !== '') bits.push(S.values.id);
        return '<div class="stepheader"><div>' +
            '<div class="nm">' + esc(bits.join(' \u00b7 ')) + '</div>' +
            '<div class="sub mono">' + esc(f.ent) + ' \u00b7 ' + esc(plan() === undefined ?
                'not drawn' : plan().entity) + '</div></div></div>';
    }

    /* ------------------------------------------------------------ the journey */

    /* The rail is the walk only. A step marked `offrail` draws its own screen
       and holds no place in it, so the refusal does not become step six. */
    function rail() {
        var at = S.at;
        return '<nav class="railnav" aria-label="Journey steps"><ol>' +
            STEPS.map(function (s, i) {
                if (s.offrail === true) return '';
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.short) + '</li>';
            }).join('') + '</ol></nav>';
    }

    /* The changes the save would write. Every convention write is decided by
       Market Risk, which owns how a trade, a curve or a report reads these
       terms, from the book controls note. */
    function changesOf() {
        var p = plan();
        if (p === undefined) return [];
        var touched = p.fields.filter(function (f) { return S.touched[f.name]; });
        if (touched.length === 0) {
            return [{ what: 'New convention ' + S.values.id, who: family().name, from: null, to: null, decider: 'Market Risk' }];
        }
        return touched.map(function (f) {
            return { what: 'Set ' + f.label, who: S.values.id,
                from: String(S.original[f.name] === '' || S.original[f.name] === undefined ? '\u2014' : S.original[f.name]),
                to: String(f.kind.check ? (S.values[f.name] === true ? 'yes' : 'no') : labelFor(f)),
                decider: 'Market Risk' };
        });
    }

    var FE = FourEyes.create({
        getChanges: changesOf,
        subject: function () { return S.values.id === undefined || S.values.id === '' ? 'The convention' : S.values.id; },
        checks: function (actor, changes, check) {
            var missing = missingFields().length > 0 || check === 'lookup';
            return [{ ok: !missing, label: 'Every term names a value its list still holds',
                detail: missing ? 'A term names a value the list no longer holds.' : 'All terms resolve.' }];
        }
    });

    var FE_TITLES = { waiting: 'Waiting', decide: 'Decide', declined: 'Declined' };
    var FE_LEADS = {
        waiting: 'The request is raised and the convention has not changed.',
        decide: 'The checker reads the change and answers it.',
        declined: 'The request was declined and the convention has not changed.'
    };

    function reviewGate() {
        var changes = changesOf();
        return FE.reviewNotice(changes) +
            '<ul class="fe-changes" style="margin-bottom:16px">' + changes.map(function (c) {
                var fromto = c.from === null ? '' :
                    '<div class="fe-fromto"><span class="fe-was">' + esc(c.from) + '</span><span>\u2192</span><span class="fe-now">' + esc(c.to) + '</span></div>';
                return '<li><div class="fe-what">' + esc(c.what) + FE.badge(c) + '</div><div class="fe-who">' + esc(c.who) + '</div>' + fromto + '</li>';
            }).join('') + '</ul>';
    }

    function stepBody() {
        if (S.fe !== '') return FE.html(S.fe);
        var id = STEPS[S.at].id;
        if (id === 'instrument') return instrumentStep();
        if (id === 'convention') return conventionStep();
        if (id === 'terms') return S.history ? historyStep() : termsStep();
        if (id === 'review') return reviewStep();
        if (id === 'refused') return refusedStep();
        return outcomeStep();
    }

    function stepLead() {
        if (S.fe !== '') return FE_LEADS[S.fe];
        var id = STEPS[S.at].id;
        if (id === 'terms' && S.history) {
            return 'Every version of this convention, with the field-level difference from the one before.';
        }
        if (id === 'refused') {
            return 'The write was refused. This convention was not changed.';
        }
        if (id === 'outcome' && hasFail() && missingFields().length > 0) {
            return 'The write was refused. The convention is unchanged.';
        }
        if (id === 'outcome') return S.values.id + ' is written.';
        return STEPS[S.at].lead;
    }

    /* The history body carries its own foot, so the step foot is withheld
       while the overlay is open. */
    function nextOf() {
        if (S.fe !== '') return null;
        var id = STEPS[S.at].id;
        if (S.history) return null;
        if (id === 'refused') return null;
        if (id === 'instrument') return { label: 'Continue', enabled: family() !== undefined };
        if (id === 'convention') return { label: 'Author its terms', enabled: plan() !== undefined };
        if (id === 'terms') {
            var miss = missingFields();
            return { label: 'Review', enabled: plan() !== undefined && miss.length === 0 };
        }
        if (id === 'review') return { label: FE.primaryLabel(changesOf(), 'Save the convention'), enabled: plan() !== undefined };
        return null;
    }

    function render() {
        var at = S.at;
        var step = STEPS[at];
        var backAllowed = at > 0 && STEPS[at - 1].final !== true;
        var next = nextOf();
        var foot;
        if (S.fe !== '') {
            foot = '';
        } else if (next === null) {
            foot = '<div class="stepfoot">' +
                '<button type="button" class="btn ghost" data-act="back"' +
                (backAllowed ? '' : ' disabled') + '>Back</button></div>';
        } else {
            foot = '<div class="stepfoot">' +
                '<button type="button" class="btn ghost" data-act="back"' +
                (backAllowed ? '' : ' disabled') + '>Back</button>' +
                '<button type="button" class="btn primary ml-auto" data-act="next"' +
                (next.enabled ? '' : ' disabled') + '>' + esc(next.label) + '</button></div>';
        }

        document.getElementById('app').innerHTML =
            '<div class="page"><h1>Define the conventions for an instrument</h1>' +
            '<div class="journey">' + rail() +
            '<section class="card">' + stepHeader() +
            '<h2>' + esc(S.fe !== '' ? FE_TITLES[S.fe] : step.title) + (S.history ? ' \u00b7 history' : '') + '</h2>' +
            '<p class="lead">' + stepLead() + '</p>' +
            stepBody() + foot + '</section></div></div>' +
            workspaceNote();

        renderNote();
        renderBar();
    }

    /* The note the journey must carry: the catalogue expected the workspace
       removal to change this journey. It has already changed the model, so the
       note states what is true now. Stated once, on every screen. */
    function workspaceNote() {
        return '<div class="page" style="padding-top:0"><div class="gap" style="max-width:60rem">' +
            '<b>Desk scope.</b> The agreed catalogue expected the instrument conventions to bind the ' +
            '<span class="code">workspace-scoped-lookup</span> profile, so that a convention set would be ' +
            'curated per trading desk and the workspace removal would change this journey. That is no ' +
            'longer the state of the model: commit <span class="code">d0de221445</span> dropped the ' +
            '<span class="code">workspace</span> column and the conventions now bind ' +
            '<span class="code">simple-lookup</span>. A convention is tenant-wide, there is no desk step ' +
            'to draw, and no workspace removal is outstanding for this journey.</div></div>';
    }

    function renderNote() {
        var miss = missingFields().length;
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey Define the conventions for an instrument \u00b7 ' +
            (family() === undefined ? 'no family' : family().name) + ' \u00b7 ' +
            (S.values.id === undefined ? 'no convention' : S.values.id) + ' \u00b7 state ' +
            STEPS[S.at].id + (miss > 0 ? ' \u00b7 ' + miss + ' missing lookup' + (miss === 1 ? '' : 's') : '');
    }

    function renderBar() {
        var steps = STEPS.map(function (s, i) {
            return '<button data-act="state" data-state="' + s.id + '"' +
                (S.at === i ? ' class="on"' : '') + '>' + esc(s.short.toLowerCase()) + '</button>';
        }).join('');
        var fams = FAMILIES.map(function (f) {
            return '<button data-act="family" data-family="' + esc(f.id) + '"' +
                (S.family === f.id ? ' class="on"' : '') + '>' + esc(f.id) + '</button>';
        }).join('');
        var extras = '<span class="sep">|</span>' +
            '<button data-act="fail" data-kind="1"' + (hasFail() ? ' class="on"' : '') + '>fail</button>' +
            '<button data-act="diff"' + (S.diff ? ' class="on"' : '') + '>diff</button>' +
            '<button data-act="state" data-state="history"' + (S.history ? ' class="on"' : '') + '>history</button>' +
            '<button data-act="state" data-state="refused"' +
            (STEPS[S.at].id === 'refused' ? ' class="on"' : '') + '>refused</button>' +
            FE.states.map(function (st) {
                return '<button data-act="state" data-state="' + st + '"' + (S.fe === st ? ' class="on"' : '') + '>' + st + '</button>';
            }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + steps +
            '<span class="sep">|</span><span class="label">instrument</span>' + fams + extras;
    }

    /* --------------------------------------------------------------- behaviour */

    function rerender() {
        var ae = document.activeElement;
        var key = ae && ae.getAttribute ? ae.getAttribute('data-focus') : null;
        var start = ae && typeof ae.selectionStart === 'number' ? ae.selectionStart : null;
        var end = ae && typeof ae.selectionEnd === 'number' ? ae.selectionEnd : null;
        render();
        if (key) {
            var el = document.querySelector('[data-focus="' + key + '"]');
            if (el !== null && el.focus !== undefined) {
                el.focus();
                if (start !== null && el.setSelectionRange) {
                    try { el.setSelectionRange(start, end); } catch (e) { /* not a text input */ }
                }
            }
        }
    }

    function pickFamily(id) {
        var f = FAMILIES.filter(function (x) { return x.id === id; })[0];
        if (f === undefined) return;
        S.family = id;
        S.q = '';
        var first = rowsFor(id)[0];
        if (first !== undefined && !first.placeholder) S.id = first.id;
        loadForm();
    }

    function pickRow(id) {
        S.id = id;
        loadForm();
    }

    /* The values the terms step starts from. A row that exists starts from
       its own values; a new one starts from the model's own example. */
    function loadForm() {
        var p = plan();
        var seed = {
            swap: { id: S.id, fixed_calendar: 'TARGET', fixed_frequency: 'Semiannual',
                    fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/360',
                    index: 'EUR-EURIBOR-6M', float_frequency: 'Semiannual',
                    sub_periods_coupon_type: 'Compounding' },
            deposit: { id: S.id !== '' ? S.id : 'USD-LIBOR-CONVENTIONS', index_based: true,
                       index: 'USD-LIBOR-3M', calendar: 'TARGET', convention: 'ModifiedFollowing',
                       day_count_fraction: 'ACT/360', end_of_month: false, settlement_days: 2 }
        };
        S.values = {};
        if (p === undefined) return;
        var base = seed[S.family] === undefined ? { id: S.id } : seed[S.family];
        p.fields.forEach(function (f) {
            S.values[f.name] = base[f.name] === undefined ? '' : base[f.name];
        });
        S.values.id = S.id !== '' ? S.id : (base.id === undefined ? '' : base.id);
        S.touched = {};
        S.original = JSON.parse(JSON.stringify(S.values));
    }

    /* One place resolves a named state, so the bar, the links and the query
       string all agree about what `refused` and `history` mean. */
    function goToState(id) {
        if (FE.states.indexOf(id) >= 0) {
            S.fe = id;
            S.history = false;
            S.at = 3;
            return;
        }
        S.fe = '';
        if (id === 'history') {
            S.history = true;
            S.at = 2;
            return;
        }
        S.history = false;
        STEPS.forEach(function (s, i) { if (s.id === id) S.at = i; });
    }

    function applySave() {
        S.savedVersion = S.version + 1;
        S.version = S.savedVersion;
        S.saved = true;
        S.fe = '';
        S.at = 4;
    }

    function goNext() {
        var id = STEPS[S.at].id;
        if (id === 'review') {
            if (hasFail() && missingFields().length > 0) {
                S.at = REFUSED_AT;
                return;
            }
            var changes = changesOf();
            if (FE.gated(changes).length > 0) {
                FE.raise(changes);
                S.fe = 'waiting';
                return;
            }
            FE.reset();
            applySave();
            return;
        }
        S.at += 1;
    }

    function onField(el) {
        var name = el.getAttribute('data-f');
        if (name === null) return false;
        var p = plan();
        var field = p === undefined ? undefined :
            p.fields.filter(function (f) { return f.name === name; })[0];
        if (field !== undefined && field.kind.check) S.values[name] = el.checked;
        else S.values[name] = el.value;
        S.touched[name] = true;
        return true;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (el === null) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');

        if (act === 'state') {
            goToState(el.getAttribute('data-state'));
        } else if (act === 'clear-fail') {
            setFail('');
            S.history = false;
            S.at = 2;
        } else if (act === 'family') {
            pickFamily(el.getAttribute('data-family'));
            S.history = false;
            S.at = 1;
        } else if (act === 'row' || act === 'edit') {
            pickRow(el.getAttribute('data-id'));
            S.history = false;
            if (act === 'edit') S.at = 2;
        } else if (act === 'new') {
            S.id = '';
            loadForm();
            S.values.id = '';
            S.history = false;
            S.at = 2;
        } else if (act === 'history') {
            var id = el.getAttribute('data-id');
            if (id !== null && id !== '') pickRow(id);
            S.diff = true;
            S.history = true;
            S.at = 2;
        } else if (act === 'back') {
            if (!el.disabled && S.at > 0 && STEPS[S.at - 1].final !== true) {
                S.history = false;
                S.at -= 1;
            }
        } else if (act === 'next') {
            if (!el.disabled) goNext();
        } else if (act === 'fail' || act === 'refusal') {
            /* The refusal this journey must handle: a term references a
               value the list no longer holds. It is set on the field it
               belongs to, and the state that draws it is `refused`. */
            setFail(hasFail() ? '' : '1');
            S.history = false;
            S.at = hasFail() ? REFUSED_AT : 2;
        } else if (act === 'diff') {
            S.diff = !S.diff;
        } else if (act === 'version') {
            S.version = parseInt(el.getAttribute('data-version'), 10);
        } else if (act === 'openlist') {
            /* Another refdata list owns the value, so this journey links to
               that list rather than editing it here. */
        } else {
            return;
        }
        rerender();
    });

    document.addEventListener('input', function (ev) {
        var el = ev.target;
        if (el === null || el.getAttribute === undefined) return;
        if (el.getAttribute('data-q') !== null) { S.q = el.value; rerender(); return; }
        if (el.getAttribute('data-fail') !== null) { setFail(el.checked ? '1' : ''); rerender(); return; }
        if (el.getAttribute('data-f') !== null) { onField(el); rerender(); }
    });

    document.addEventListener('change', function (ev) {
        var el = ev.target;
        if (el === null || el.getAttribute === undefined) return;
        if (el.getAttribute('data-fail') !== null) { setFail(el.checked ? '1' : ''); rerender(); return; }
        if (el.getAttribute('data-f') !== null) { onField(el); rerender(); }
    });

    /* ------------------------------------------------------------------ boot */

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (p.get('family') !== null) {
            var f = FAMILIES.filter(function (x) { return x.id === p.get('family'); })[0];
            if (f !== undefined) S.family = f.id;
        }
        var first = rowsFor(S.family)[0];
        if (first !== undefined && !first.placeholder) S.id = first.id;
        var id = p.get('id');
        if (id !== null && id !== '') S.id = id;
        loadForm();
        if (p.get('q') !== null) S.q = p.get('q');
        var fail = p.get('fail');
        if (fail === null) fail = p.get('refusal');
        if (fail !== null && fail !== '' && fail !== '0') setFail(fail === '1' ? '1' : fail);
        if (p.get('diff') !== null) S.diff = p.get('diff') !== '0';
        var ver = parseInt(p.get('version'), 10);
        if (!isNaN(ver)) S.version = ver;
        var field = p.get('field');
        if (field !== null) {
            S.focus = field;
            if (p.get('value') !== null) S.values[field] = p.get('value');
        }
        FE.params(p);
        var state = p.get('state');
        if (state !== null && state !== '') goToState(state);
        /* The refused screen always names a kind, so the first kind is the
           default when the URL asks for the state alone. */
        if (STEPS[S.at].id === 'refused' && !hasFail()) setFail('1');
        if (S.saved) S.version = S.savedVersion;
    }

    readParams();
    render();

    /* The four-eyes screens: their own controls, not the journey's. */
    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-fe]');
        if (!el) return;
        ev.preventDefault();
        var to = FE.click(el);
        if (to === null || to === 'stay') return;
        if (to === 'outcome') applySave();
        else goToState(to);
        rerender();
    });

    function onFeInput(ev) {
        var el = ev.target;
        if (el && el.getAttribute && FE.input(el)) rerender();
    }

    document.addEventListener('input', onFeInput);
    document.addEventListener('change', onFeInput);
})();

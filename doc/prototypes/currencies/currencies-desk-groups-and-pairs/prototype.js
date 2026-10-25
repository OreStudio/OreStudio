/* Currencies, desk groups and currency pairs: the prototype's screens, drawn from the query string. */
'use strict';

// ---------------------------------------------------------------------------
// Mock data. The fourteen currencies, six groups and sixteen pairs are the Acme
// seed as the server holds them on 2026-10-25; the history and the two user
// changes are invented.
// ---------------------------------------------------------------------------
const CURRENCIES = [
    { iso_code: 'EUR', name: 'Euro', numeric_code: '978', symbol: '€', fraction_symbol: 'c', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '€#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 1, country: 'EU', version: 3, modified_by: 'priya.natarajan', recorded_at: '2026-10-22T10:04:00Z', change_reason_code: 'user.booking_error', change_commentary: 'The format lost its fraction digits.' },
    { iso_code: 'USD', name: 'US Dollar', numeric_code: '840', symbol: '$', fraction_symbol: '¢', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '$#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 1, day_basis: 'ACT/360', base_precedence: 5, country: 'US', version: 1 },
    { iso_code: 'GBP', name: 'British Pound Sterling', numeric_code: '826', symbol: '£', fraction_symbol: 'p', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '£#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/365', base_precedence: 2, country: 'GB', version: 1 },
    { iso_code: 'JPY', name: 'Japanese Yen', numeric_code: '392', symbol: '¥', fraction_symbol: '', fractions_per_unit: 0, rounding_type: 'Closest', rounding_precision: 0, format: '¥#,##0', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 8, country: 'JP', version: 1 },
    { iso_code: 'CHF', name: 'Swiss Franc', numeric_code: '756', symbol: 'CHF', fraction_symbol: 'c', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'CHF #,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 7, country: 'CH', version: 1 },
    { iso_code: 'AUD', name: 'Australian Dollar', numeric_code: '036', symbol: 'A$', fraction_symbol: '¢', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'A$#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/365', base_precedence: 3, country: 'AU', version: 1 },
    { iso_code: 'CAD', name: 'Canadian Dollar', numeric_code: '124', symbol: '$', fraction_symbol: '¢', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '$#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/365', base_precedence: 6, country: 'CA', version: 1 },
    { iso_code: 'NZD', name: 'New Zealand Dollar', numeric_code: '554', symbol: 'NZ$', fraction_symbol: 'c', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'NZ$#,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/365', base_precedence: 4, country: 'NZ', version: 1 },
    { iso_code: 'NOK', name: 'Norwegian Krone', numeric_code: '578', symbol: 'kr', fraction_symbol: 'ø', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'kr #,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 9, country: 'NO', version: 1 },
    { iso_code: 'SEK', name: 'Swedish Krona', numeric_code: '752', symbol: 'kr', fraction_symbol: 'ö', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '#,##0.00 kr', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 10, country: 'SE', version: 1 },
    { iso_code: 'DKK', name: 'Danish Krone', numeric_code: '208', symbol: 'kr', fraction_symbol: 'ø', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'kr #,##0.00', monetary_nature: 'fiat', market_tier: 'g10', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 11, country: 'DK', version: 1 },
    { iso_code: 'BRL', name: 'Brazilian Real', numeric_code: '986', symbol: 'R$', fraction_symbol: '¢', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: 'R$#,##0.00', monetary_nature: 'fiat', market_tier: 'emerging', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 100, country: 'BR', version: 1 },
    { iso_code: 'CNY', name: 'Chinese Yuan', numeric_code: '156', symbol: '¥', fraction_symbol: '分', fractions_per_unit: 100, rounding_type: 'Closest', rounding_precision: 2, format: '¥#,##0.00', monetary_nature: 'fiat', market_tier: 'emerging', ore_currency_type: 'Major', spot_days: 2, day_basis: 'ACT/360', base_precedence: 100, country: 'CN', version: 1 },
    { iso_code: 'XAU', name: 'Gold', numeric_code: '959', symbol: 'XAU', fraction_symbol: '', fractions_per_unit: 0, rounding_type: 'Closest', rounding_precision: 2, format: 'XAU #,##0.00', monetary_nature: 'commodity', market_tier: 'g10', ore_currency_type: 'Metal', spot_days: 2, day_basis: 'ACT/360', base_precedence: 0, country: 'ZZ', version: 2, modified_by: 'adrian.vance', recorded_at: '2026-10-20T08:15:00Z', change_reason_code: 'system.new_record', change_commentary: '' },
];
const COUNTRIES = { EU: 'Euro area', US: 'United States', GB: 'United Kingdom', JP: 'Japan', CH: 'Switzerland', AU: 'Australia', CA: 'Canada', NZ: 'New Zealand', NO: 'Norway', SE: 'Sweden', DK: 'Denmark', BR: 'Brazil', CN: 'China', ZZ: 'No country', DE: 'Germany', FR: 'France', IT: 'Italy', ES: 'Spain', NL: 'Netherlands', IE: 'Ireland' };
const CURRENCY_COUNTRIES = { EUR: ['DE', 'FR', 'IT', 'ES', 'NL', 'IE'], USD: ['US'], GBP: ['GB'], JPY: ['JP'], CHF: ['CH'], AUD: ['AU'], CAD: ['CA'], NZD: ['NZ'], NOK: ['NO'], SEK: ['SE'], DKK: ['DK'], BRL: ['BR'], CNY: ['CN'], XAU: [] };
const CALENDARS = { TARGET: 'TARGET (Euro area)', 'UnitedStates.Settlement': 'United States (Settlement)', 'UnitedKingdom.Settlement': 'United Kingdom (Settlement)', Japan: 'Japan', Switzerland: 'Switzerland', Australia: 'Australia', 'Canada.Settlement': 'Canada (Settlement)', NewZealand: 'New Zealand', Norway: 'Norway', Sweden: 'Sweden', Denmark: 'Denmark', Brazil: 'Brazil', 'China.SSE': 'China (Shanghai Stock Exchange)', 'UnitedKingdom.Metals': 'United Kingdom (Metals)' };
const CURRENCY_CALENDARS = { EUR: ['TARGET'], USD: ['UnitedStates.Settlement'], GBP: ['UnitedKingdom.Settlement'], JPY: ['Japan'], CHF: ['Switzerland'], AUD: ['Australia'], CAD: ['Canada.Settlement'], NZD: ['NewZealand'], NOK: ['Norway'], SEK: ['Sweden'], DKK: ['Denmark'], BRL: ['Brazil'], CNY: ['China.SSE'], XAU: ['UnitedKingdom.Metals'] };
const CALENDAR_COUNTRY = { TARGET: 'EU', 'UnitedStates.Settlement': 'US', 'UnitedKingdom.Settlement': 'GB', Japan: 'JP', Switzerland: 'CH', Australia: 'AU', 'Canada.Settlement': 'CA', NewZealand: 'NZ', Norway: 'NO', Sweden: 'SE', Denmark: 'DK', Brazil: 'BR', 'China.SSE': 'CN', 'UnitedKingdom.Metals': 'GB' };

const GROUPS = [
    { code: 'G11', name: 'G11', description: 'The eleven liquid majors that desks allocate by: EUR, USD, GBP, JPY, AUD, CAD, CHF, DKK, NOK, NZD and SEK', display_order: 1, version: 2, modified_by: 'priya.natarajan', recorded_at: '2026-10-19T14:30:00Z', change_reason_code: 'common.non_material_update', change_commentary: 'Named the eleven in the description.' },
    { code: 'SCANDIES', name: 'Scandies', description: 'The Scandinavian currencies that desks often trade together: DKK, NOK and SEK', display_order: 2, version: 1 },
    { code: 'ANTIPODEANS', name: 'Antipodeans', description: 'The correlated Pacific currencies: AUD and NZD', display_order: 3, version: 1 },
    { code: 'COMMODITY', name: 'Commodity currencies', description: 'Currencies whose value follows a commodity export such as oil, metals or dairy', display_order: 4, version: 1 },
    { code: 'ASIANS', name: 'Asians', description: 'The Asian currencies that desks group by region, not by liquidity', display_order: 5, version: 1 },
    { code: 'LATAMS', name: 'Latams', description: 'The Latin American currencies that often share a desk and a price verification process', display_order: 6, version: 1 },
];
const MEMBERS = { G11: ['EUR', 'USD', 'GBP', 'JPY', 'AUD', 'CAD', 'CHF', 'DKK', 'NOK', 'NZD', 'SEK'], SCANDIES: ['DKK', 'NOK', 'SEK'], ANTIPODEANS: ['AUD', 'NZD'], COMMODITY: ['AUD', 'CAD', 'NOK', 'NZD', 'BRL'], ASIANS: ['JPY', 'CNY'], LATAMS: ['BRL'] };

const PAIRS = [
    { pair_code: 'EUR/USD', base_currency: 'EUR', quote_currency: 'USD', classification: 'major', version: 2, modified_by: 'priya.natarajan', recorded_at: '2026-10-23T16:45:00Z', change_reason_code: 'common.non_material_update', change_commentary: 'Reclassified after the desk review.' },
    { pair_code: 'GBP/USD', base_currency: 'GBP', quote_currency: 'USD', classification: 'major', version: 1 },
    { pair_code: 'USD/JPY', base_currency: 'USD', quote_currency: 'JPY', classification: 'major', version: 1 },
    { pair_code: 'USD/CHF', base_currency: 'USD', quote_currency: 'CHF', classification: 'major', version: 1 },
    { pair_code: 'USD/CAD', base_currency: 'USD', quote_currency: 'CAD', classification: 'major', version: 1 },
    { pair_code: 'AUD/USD', base_currency: 'AUD', quote_currency: 'USD', classification: 'major', version: 1 },
    { pair_code: 'NZD/USD', base_currency: 'NZD', quote_currency: 'USD', classification: 'major', version: 1 },
    { pair_code: 'EUR/GBP', base_currency: 'EUR', quote_currency: 'GBP', classification: 'minor', version: 1 },
    { pair_code: 'EUR/JPY', base_currency: 'EUR', quote_currency: 'JPY', classification: 'minor', version: 1 },
    { pair_code: 'EUR/CHF', base_currency: 'EUR', quote_currency: 'CHF', classification: 'minor', version: 1 },
    { pair_code: 'EUR/NOK', base_currency: 'EUR', quote_currency: 'NOK', classification: 'minor', version: 1 },
    { pair_code: 'EUR/SEK', base_currency: 'EUR', quote_currency: 'SEK', classification: 'minor', version: 1 },
    { pair_code: 'GBP/JPY', base_currency: 'GBP', quote_currency: 'JPY', classification: 'minor', version: 1 },
    { pair_code: 'USD/BRL', base_currency: 'USD', quote_currency: 'BRL', classification: 'exotic', version: 1 },
    { pair_code: 'USD/CNY', base_currency: 'USD', quote_currency: 'CNY', classification: 'exotic', version: 1 },
    { pair_code: 'XAU/USD', base_currency: 'XAU', quote_currency: 'USD', classification: 'commodity', version: 1 },
];
function conventionOf(p) {
    const jpy = p.quote_currency === 'JPY';
    const own = { 'EUR/USD': { pip_factor: 0.0001, tick_size: 0.5, decimal_places: 5, business_day_convention: 'ModifiedFollowing', spot_relative: true, end_of_month: false, version: 3 } }[p.pair_code];
    return own ?? { pip_factor: jpy ? 0.01 : 0.0001, tick_size: 1, decimal_places: jpy ? 2 : 4, business_day_convention: 'ModifiedFollowing', spot_relative: true, end_of_month: false, version: 1 };
}
const PAIR_CALENDARS = (p) => [...new Set([...(CURRENCY_CALENDARS[p.base_currency] ?? []), ...(CURRENCY_CALENDARS[p.quote_currency] ?? [])])];

const ROUNDING = { Up: 'Round up', Down: 'Round down', Closest: 'Round to nearest', Floor: 'Floor', Ceiling: 'Ceiling' };
const NATURES = { fiat: 'Fiat currency', commodity: 'Commodity currency', synthetic: 'Synthetic currency', supranational: 'Supranational currency' };
const TIERS = { g10: 'G10 / Majors', emerging: 'Emerging', exotic: 'Exotic', frontier: 'Frontier', historical: 'Historical' };
const ORE_TYPES = { Major: 'Major', Metal: 'Metal', Crypto: 'Crypto' };
const DAY_BASES = { 'ACT/360': 'ACT/360', 'ACT/365': 'ACT/365', '30/360': '30/360', 'ACT/ACT': 'ACT/ACT' };
const CLASSIFICATIONS = { major: 'Major', minor: 'Minor', exotic: 'Exotic', commodity: 'Commodity' };
const BDC = { Following: 'Following', ModifiedFollowing: 'Modified following', Preceding: 'Preceding', ModifiedPreceding: 'Modified preceding', Unadjusted: 'Unadjusted' };
const REASONS = ['Non-material update (Touch)', 'User/Booking Error', 'Duplicate Record', 'Data not updated within required liquidity horizon', 'Manual override after plausibility check failure', 'Upstream vendor/API data issue', 'Mandatory compliance adjustment', 'Exceptional (requires audit note)'];
const LOADED = '2026-10-25T14:20:11+00:00';

/* The history of a record is one stream of events, newest first, as the Entity
   screen specification's History and the timeline section draws it. */
const SEED_EVENT = (v) => ({ version: v, kind: 'Created', at: '2026-10-05T11:04:42Z', by: 'ores_brave_hopper_refdata_service', service: true, reason: 'system.external_data_import', note: 'Imported from DQ dataset: ISO 4217 currencies', changes: [] });
const HISTORY = {
    'currency:EUR': [
        { version: 3, kind: 'Changed', at: '2026-10-22T10:04:12Z', by: 'priya.natarajan', service: false, reason: 'user.booking_error', note: 'The format lost its fraction digits.', changes: [['Format', '€#,##0', '€#,##0.00']] },
        { version: 2, kind: 'Changed', at: '2026-10-19T09:40:55Z', by: 'adrian.vance', service: false, reason: 'common.non_material_update', note: 'Base precedence set so EUR quotes first.', changes: [['Base precedence', '2', '1']] },
        SEED_EVENT(1),
    ],
    'currency:XAU': [
        { version: 2, kind: 'Changed', at: '2026-10-20T08:15:30Z', by: 'adrian.vance', service: false, reason: 'common.non_material_update', note: 'Metal, not a major.', changes: [['ORE currency type', 'Major', 'Metal']] },
        { version: 1, kind: 'Created', at: '2026-10-20T08:11:02Z', by: 'adrian.vance', service: false, reason: 'system.new_record', note: '', changes: [] },
    ],
    'group:G11': [
        { version: 2, kind: 'Changed', at: '2026-10-19T14:30:08Z', by: 'priya.natarajan', service: false, reason: 'common.non_material_update', note: 'Named the eleven in the description.', changes: [['Description', 'The eleven liquid majors that desks allocate by', 'The eleven liquid majors that desks allocate by: EUR, USD, GBP, JPY, AUD, CAD, CHF, DKK, NOK, NZD and SEK']] },
        { ...SEED_EVENT(1), note: 'Imported from DQ dataset: FX Market Conventions' },
    ],
    'pair:EUR/USD': [
        { version: 2, kind: 'Changed', at: '2026-10-23T16:45:40Z', by: 'priya.natarajan', service: false, reason: 'common.non_material_update', note: 'Reclassified after the desk review.', changes: [['Classification', 'minor', 'major']] },
        { ...SEED_EVENT(1), note: 'Imported from DQ dataset: FX Market Conventions' },
    ],
    'convention:EUR/USD': [
        { version: 3, kind: 'Changed', at: '2026-10-24T09:02:17Z', by: 'adrian.vance', service: false, reason: 'business.data_not_updated', note: 'Half-pip ticks, five decimals, as the venue quotes.', changes: [['Tick size (pips)', '1', '0.5'], ['Decimal places', '4', '5']] },
        { version: 2, kind: 'Changed', at: '2026-10-21T11:12:03Z', by: 'adrian.vance', service: false, reason: 'user.booking_error', note: '', changes: [['Spot relative', 'No', 'Yes']] },
        { ...SEED_EVENT(1), note: 'Imported from DQ dataset: FX Market Conventions' },
    ],
};
const KIND_COLOUR = { Created: '#8957e5', Changed: '#58a6ff' };

// ---------------------------------------------------------------------------
// The query string is the state.
// ---------------------------------------------------------------------------
const q = new URLSearchParams(location.search);
const state = {
    screen: q.get('screen') ?? 'currencies',
    code: q.get('code') ?? '',
    tab: q.get('tab') ?? 'details',
    dialog: q.get('dialog') ?? '',
    link: q.get('link') ?? '',
    who: q.get('who') ?? 'admin',
    search: q.get('q') ?? '',
    page: Number(q.get('page') ?? '1'),
    size: Number(q.get('size') ?? '15'),
    sort: q.get('sort') ?? '',
    dir: q.get('dir') ?? 'asc',
    guard: q.get('guard') === '1',
    done: q.get('done') ?? '',
    step: Number(q.get('step') ?? '1'),
    file: q.get('file') ?? '',
    clean: q.get('clean') === '1',
};
const admin = state.who === 'admin';

function href(changes) {
    const next = new URLSearchParams(location.search);
    for (const [k, v] of Object.entries(changes)) {
        if (v === '' || v === null || v === undefined) next.delete(k); else next.set(k, String(v));
    }
    const s = next.toString();
    return s === '' ? location.pathname : `?${s}`;
}
function go(changes) { location.href = href(changes); }
function esc(s) { return String(s ?? '').replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])); }
function icon(name) {
    const paths = {
        refresh: 'M4 4v5h5M20 20v-5h-5M5.6 15A7 7 0 0 0 18.4 9M18.4 15A7 7 0 0 1 5.6 9',
        add: 'M12 5v14M5 12h14',
        edit: 'M4 20h4l10-10-4-4L4 16v4zM13 7l4 4',
        delete: 'M4 7h16M9 7V4h6v3M6 7l1 13h10l1-13',
        save: 'M5 3h11l3 3v15H5zM8 3v6h7V3M8 21v-7h8v7',
        cancel: 'M6 6l12 12M18 6L6 18',
        history: 'M3 12a9 9 0 1 0 3-6.7M3 4v5h5M12 7v5l3 3',
        remove: 'M7 12h10M4 7a3 3 0 0 1 3-3h10a3 3 0 0 1 3 3v10a3 3 0 0 1-3 3H7a3 3 0 0 1-3-3z',
        revert: 'M3 10h10a5 5 0 1 1 0 10H9M3 10l4-4M3 10l4 4',
        search: 'M11 4a7 7 0 1 1 0 14 7 7 0 0 1 0-14zM20 20l-4-4',
        import: 'M4 12h11M11 8l4 4-4 4M19 4v16',
        export: 'M9 12h11M16 8l4 4-4 4M5 4v16',
        left: 'M15 6l-6 6 6 6', right: 'M9 6l6 6-6 6',
        file: 'M6 3h8l4 4v14H6zM14 3v4h4',
    };
    return `<svg class="icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="${paths[name]}"/></svg>`;
}
function flag(code, small) { return `<span class="flag${small ? ' sm' : ''}" aria-hidden="true">${esc(String(code).slice(0, 3).toUpperCase())}</span>`; }
function pairFlag(p) { return `<span class="flag pair" aria-hidden="true">${esc(p.base_currency)}/${esc(p.quote_currency)}</span>`; }
const currency = (code) => CURRENCIES.find((c) => c.iso_code === code);
const group = (code) => GROUPS.find((g) => g.code === code);
const pair = (code) => PAIRS.find((p) => p.pair_code === code);
/* ES-REF-01: a code that names another record is a link to its page, with its flag. */
const currencyHref = (code) => href({ screen: 'currency', code, tab: '', dialog: '', link: '', q: '', page: '', sort: '', dir: '' });
const groupHref = (code) => href({ screen: 'group', code, tab: '', dialog: '', link: '', q: '', page: '', sort: '', dir: '' });
const pairHref = (code) => href({ screen: 'pair', code, tab: '', dialog: '', link: '', q: '', page: '', sort: '', dir: '' });
const countryHref = (code) => `/refdata/countries/${encodeURIComponent(code)}`;
const calendarHref = (code) => `/refdata/calendars/${encodeURIComponent(code)}`;
const classificationHref = (list) => `/refdata/classifications/${list}`;
const currencyLink = (code, small) => `<span class="cell-flag">${flag(code, small)}<a href="${currencyHref(code)}" onclick="event.stopPropagation()">${esc(code)}</a></span>`;
const classified = (list, map, code) => `<a href="${classificationHref(list)}" onclick="event.stopPropagation()">${esc(map[code] ?? code)}</a>`;

// ---------------------------------------------------------------------------
// Screens
// ---------------------------------------------------------------------------
const main = document.getElementById('main');
const layer = document.getElementById('layer');
const ROOT = [{ label: 'Home', href: '#' }, { label: 'Reference data', href: '#' }];

function crumbs(items) {
    return `<nav class="crumbs" aria-label="Breadcrumb">${items.map((it, i) => (it.href ? `<a href="${it.href}">${esc(it.label)}</a>` : `<span>${esc(it.label)}</span>`) + (i < items.length - 1 ? ' <span>›</span>' : '')).join(' ')}</nav>`;
}
function refreshButton() {
    return `<div class="actions-col"><button class="btn" type="button">${icon('refresh')} Refresh</button><span class="loaded">${esc(LOADED)}</span></div>`;
}
/* RS-ORE-02: the pair stands between Refresh and Add on a list whose kind has an ORE
   document. Export needs read, so a reader sees it; Import needs write. */
function exchangeButtons() {
    return `<a class="btn" href="${href({ dialog: 'export' })}">${icon('export')} Export</a>${admin ? `<a class="btn" href="${href({ screen: 'import', dialog: '', step: '' })}">${icon('import')} Import</a>` : ''}`;
}

const LISTS = {
    currencies: {
        title: 'Currencies', lead: 'The currencies the tenant works in, with the countries, calendars and desk groups each belongs to.',
        add: 'Add currency', rows: CURRENCIES, key: 'iso_code', open: currencyHref, exchange: true, defaultSort: 'iso_code',
        columns: [
            ['iso_code', 'ISO code', (c) => `<span class="cell-flag">${flag(c.iso_code, true)}<span class="mono">${esc(c.iso_code)}</span></span>`],
            ['name', 'Name', (c) => esc(c.name)],
            ['symbol', 'Symbol', (c) => esc(c.symbol)],
            ['monetary_nature', 'Monetary nature', (c) => classified('monetary-nature', NATURES, c.monetary_nature)],
            ['market_tier', 'Market tier', (c) => classified('currency-market-tier', TIERS, c.market_tier)],
        ],
        searchIn: (c) => `${c.iso_code} ${c.name}`,
    },
    groups: {
        title: 'Desk groups', lead: 'Groups of currencies the desk names, such as G11 or Scandies, in display order.',
        add: 'Add group', rows: GROUPS, key: 'code', open: groupHref, defaultSort: 'display_order',
        columns: [
            ['display_order', 'Display order', (g) => `<span class="num">${g.display_order}</span>`],
            ['code', 'Code', (g) => `<span class="mono">${esc(g.code)}</span>`],
            ['name', 'Name', (g) => esc(g.name)],
            ['description', 'Description', (g) => esc(g.description)],
            [null, 'Members', (g) => `<span class="num">${(MEMBERS[g.code] ?? []).length}</span>`],
        ],
        searchIn: (g) => `${g.code} ${g.name} ${g.description}`,
    },
    pairs: {
        title: 'Currency pairs', lead: 'The pairs the desk trades, with how each quotes, displays and settles.',
        add: 'Add pair', rows: PAIRS, key: 'pair_code', open: pairHref, defaultSort: 'pair_code',
        columns: [
            ['pair_code', 'Pair code', (p) => `<span class="cell-flag">${pairFlag(p)}<span class="mono">${esc(p.pair_code)}</span></span>`],
            ['base_currency', 'Base currency', (p) => currencyLink(p.base_currency, true)],
            ['quote_currency', 'Quote currency', (p) => currencyLink(p.quote_currency, true)],
            ['classification', 'Classification', (p) => classified('currency-pair-classification', CLASSIFICATIONS, p.classification)],
        ],
        searchIn: (p) => `${p.pair_code} ${p.base_currency} ${p.quote_currency} ${CLASSIFICATIONS[p.classification]}`,
    },
};

function renderList(kind) {
    const L = LISTS[kind];
    document.title = `ORE Studio ${L.title}`;
    const search = state.search.trim().toLowerCase();
    const rows = L.rows.filter((r) => !search || L.searchIn(r).toLowerCase().includes(search));
    const key = state.sort || L.defaultSort;
    rows.sort((a, b) => (typeof a[key] === 'number' ? a[key] - b[key] : String(a[key] ?? '').localeCompare(String(b[key] ?? ''))) * (state.dir === 'desc' ? -1 : 1));
    const total = rows.length;
    const from = (state.page - 1) * state.size;
    const pageRows = rows.slice(from, from + state.size);
    const pages = Math.max(1, Math.ceil(total / state.size));
    const th = (id, label) => id === null ? `<th>${label}</th>` : `<th><button type="button" ${key === id ? `aria-sort="${state.dir === 'asc' ? 'ascending' : 'descending'}"` : ''} onclick="go({sort:'${id}',dir:'${key === id && state.dir === 'asc' ? 'desc' : 'asc'}',page:''})">${label}${key === id ? (state.dir === 'asc' ? ' ↑' : ' ↓') : ''}</button></th>`;
    const plural = L.title.toLowerCase();
    main.innerHTML = `
      ${crumbs([...ROOT, { label: L.title }])}
      <div class="head">
        <div class="titles"><h1>${esc(L.title)}</h1><p class="lead">${esc(L.lead)}</p></div>
        <div class="actions">${refreshButton()}${L.exchange ? exchangeButtons() : ''}${admin ? `<a class="btn primary" href="${href({ dialog: 'add' })}">${icon('add')} ${esc(L.add)}</a>` : ''}</div>
      </div>
      <section class="card">
        <div class="toolbar">
          <form class="search" role="search" onsubmit="event.preventDefault(); go({q: this.q.value, page: ''})">${icon('search')}<input type="search" name="q" placeholder="Search…" value="${esc(state.search)}" aria-label="Search">${state.search ? `<button type="button" aria-label="Clear the search" onclick="go({q:'',page:''})">✕</button>` : ''}</form>
          <button class="btn ghost sm" type="button">▸ Columns</button>
        </div>
        <div class="table-wrap"><table>
          <thead><tr>${L.columns.map(([id, label]) => th(id, label)).join('')}</tr></thead>
          <tbody>${pageRows.length === 0 ? `<tr><td colspan="${L.columns.length}"><div class="empty">No ${esc(plural)} match the search. <a href="${href({ q: '', page: '' })}">Clear the search</a></div></td></tr>` : pageRows.map((r) => `
            <tr class="open" onclick="location.href='${L.open(r[L.key])}'">${L.columns.map(([, , cell]) => `<td>${cell(r)}</td>`).join('')}</tr>`).join('')}</tbody>
        </table></div>
        <div class="foot">
          <span class="num">${total === 0 ? '0–0 of 0' : `${from + 1}–${Math.min(from + state.size, total)} of ${total}`}</span>
          <span class="pager">
            <a class="btn sm" href="${href({ page: '' })}" ${state.page === 1 ? 'aria-disabled="true"' : ''}>First</a>
            <a class="btn sm" href="${href({ page: state.page > 2 ? state.page - 1 : '' })}" ${state.page === 1 ? 'aria-disabled="true"' : ''}>Previous</a>
            <a class="btn sm" href="${href({ page: Math.min(pages, state.page + 1) })}" ${state.page >= pages ? 'aria-disabled="true"' : ''}>Next</a>
            <a class="btn sm" href="${href({ page: pages })}" ${state.page >= pages ? 'aria-disabled="true"' : ''}>Last</a>
            <label>Page size <select onchange="go({size:this.value,page:''})">${[15, 30, 50].map((n) => `<option ${n === state.size ? 'selected' : ''}>${n}</option>`).join('')}</select></label>
          </span>
        </div>
      </section>`;
    if (state.done) doneToast();
    else if (state.guard) guardBox();
    else if (admin && state.dialog === 'add') ({ currencies: dialogCurrency, groups: dialogGroup, pairs: dialogPair })[kind](null);
    else if (state.dialog === 'export') dialogExport();
}

/* The one notice a write leaves behind: the dialog has closed and Refresh is the way to the new version. */
function doneToast() {
    const words = {
        saved: 'Saved. Refresh shows the new version.',
        added: 'Added. The page opened on the new record.',
        removed: 'Removed. Its history stays.',
        reverted: 'Reverted: a new version carries the old values.',
        linked: 'Added. Refresh shows the link.',
        unlinked: 'Removed. The record itself stays.',
        exported: 'Saved currencyconfig.xml, 14 currencies.',
        imported: 'Imported: 1 added, 2 changed, 1 kept. Refresh shows the new versions.',
    }[state.done];
    layer.innerHTML = `<div class="notice ok toast">${esc(words)} <a href="${href({ done: '' })}">Dismiss</a></div>`;
}
function guardBox() {
    layer.innerHTML = `<div class="guard"><div class="box"><h3>You have unsaved changes. Close anyway?</h3><div class="actions"><a class="btn" href="${href({ guard: '' })}">Keep editing</a><a class="btn danger" href="${href({ guard: '', dialog: '', link: '' })}">Close anyway</a></div></div></div>`;
}

function recordHead({ crumbsItems, title, titleFlag, key, version, own, editable, editTitle }) {
    return `
      ${crumbs(crumbsItems)}
      <div class="head">
        <div class="titles">
          <h1>${titleFlag ?? ''}${esc(title)}</h1>
          <p class="lead"><span class="mono">${esc(key)}</span> · version ${version}</p>
        </div>
        <div class="actions">
          ${refreshButton()}
          <span class="mark${admin && editable ? '' : ' ro'}">${admin && editable ? `${icon('edit')} Editable` : 'Read only'}</span>
          ${own ?? ''}
          ${admin && editable ? `<a class="btn" href="${href({ dialog: 'edit' })}" title="Edit ${esc(editTitle)}">${icon('edit')} Edit</a><a class="btn danger" href="${href({ dialog: 'delete' })}" title="Delete ${esc(editTitle)}">${icon('delete')} Delete</a>` : ''}
        </div>
      </div>`;
}
function tabsNav(tabs, names, tab) {
    return `<nav class="tabs" role="tablist">${tabs.map((t) => `<a role="tab" aria-selected="${t === tab}" href="${href({ tab: t, dialog: '', link: '' })}">${names[t]}</a>`).join('')}</nav>`;
}
function lastChanged(r, fallbackReason) {
    const by = r.modified_by ?? 'ores_brave_hopper_refdata_service';
    return `<div class="last-changed">Last changed: <span class="mono">version ${r.version}</span> · ${esc(by)} · ${by.includes('service') ? '3 hours ago' : '3 days ago'} · <span class="mono">${esc(r.change_reason_code ?? fallbackReason)}</span>${r.change_commentary ? ` · “${esc(r.change_commentary)}”` : ''}</div>`;
}
/* RS-PAGE-05 to 08: label above value, four columns, the field table's order, a dash for nothing. */
function detailsCard(fields, r, reason) {
    return `<section class="card"><div class="pad"><dl>${fields.map(([label, value, help]) => `<div><dt>${esc(label)}</dt><dd>${value === '' || value === null || value === undefined ? '<span class="muted">—</span>' : value}${help ? `<span class="help">${esc(help)}</span>` : ''}</dd></div>`).join('')}</dl></div>${lastChanged(r, reason)}</section>`;
}
/* RS-RELATED-01 to 03: the linked records by key and name, a name opens the record, Add picks one not linked yet, Remove unlinks. */
function linkPanel({ title, lead, rows, columns, addLabel, may, nameOf }) {
    return `
      <p class="muted small">${esc(lead)}</p>
      <section class="card">
        <div class="table-wrap"><table>
          <thead><tr>${columns.map((h) => `<th>${h}</th>`).join('')}${may ? '<th></th>' : ''}</tr></thead>
          <tbody>${rows.length === 0 ? `<tr><td colspan="${columns.length + 1}"><div class="empty">None yet.</div></td></tr>` : rows.map((r) => `<tr>${r.cells.map((v) => `<td>${v}</td>`).join('')}${may ? `<td class="actions-cell"><a class="btn sm ghost danger" href="${href({ dialog: 'remove-link', link: r.key })}" title="Remove ${esc(nameOf(r))}">${icon('remove')} Remove</a></td>` : ''}</tr>`).join('')}</tbody>
        </table></div>
        ${may ? `<div class="under"><span class="muted small">${rows.length} ${esc(title.toLowerCase())}</span><a class="btn primary" href="${href({ dialog: 'add-link' })}">${icon('add')} ${esc(addLabel)}</a></div>` : ''}
      </section>`;
}

const CURRENCY_TABS = { details: 'Details', countries: 'Countries', calendars: 'Calendars', groups: 'Desk groups', history: 'History' };
function renderCurrency() {
    const c = currency(state.code);
    if (!c) { main.innerHTML = `<div class="notice error">There is no currency ${esc(state.code)}. <a href="${href({ screen: 'currencies', code: '' })}">Back to the currencies</a></div>`; return; }
    const tabs = Object.keys(CURRENCY_TABS);
    const tab = tabs.includes(state.tab) ? state.tab : 'details';
    document.title = `ORE Studio ${c.name}`;
    main.innerHTML = recordHead({ crumbsItems: [...ROOT, { label: 'Currencies', href: href({ screen: 'currencies', code: '', tab: '', dialog: '' }) }, { label: c.iso_code }], title: c.name, titleFlag: flag(c.iso_code), key: c.iso_code, version: c.version, editable: true, editTitle: c.name })
        + tabsNav(tabs, CURRENCY_TABS, tab) + '<div id="tab"></div>';
    const el = document.getElementById('tab');
    if (tab === 'details') {
        el.innerHTML = detailsCard([
            ['ISO code', `<span class="cell-flag">${flag(c.iso_code, true)}<span class="mono">${esc(c.iso_code)}</span></span>`, 'Set when the record was made, and never changed here.'],
            ['Name', esc(c.name)],
            ['Numeric code', `<span class="mono">${esc(c.numeric_code)}</span>`],
            ['Symbol', esc(c.symbol)],
            ['Fraction symbol', esc(c.fraction_symbol)],
            ['Fractions per unit', `<span class="num">${c.fractions_per_unit}</span>`],
            ['Rounding type', classified('rounding-type', ROUNDING, c.rounding_type)],
            ['Rounding precision', `<span class="num">${c.rounding_precision}</span>`],
            ['Format', `<span class="mono">${esc(c.format)}</span>`, `Reads ${esc(sampleAmount(c))}.`],
            ['Monetary nature', classified('monetary-nature', NATURES, c.monetary_nature)],
            ['Market tier', classified('currency-market-tier', TIERS, c.market_tier)],
            ['ORE currency type', classified('ore-currency-type', ORE_TYPES, c.ore_currency_type), 'What ORE writes in the currency configuration.'],
            ['Spot days', `<span class="num">${c.spot_days}</span>`],
            ['Day basis', classified('day-basis', DAY_BASES, c.day_basis)],
            ['Base precedence', `<span class="num">${c.base_precedence}</span>`, 'Lower quotes first in a pair.'],
            ['Flag', flag(c.iso_code)],
        ], c, 'system.external_data_import');
    } else if (tab === 'countries') {
        const codes = CURRENCY_COUNTRIES[c.iso_code] ?? [];
        el.innerHTML = linkPanel({ title: 'Countries', lead: 'The countries that issue the currency. A country opens its page.', addLabel: 'Add country', may: admin, nameOf: (r) => COUNTRIES[r.key] ?? r.key,
            columns: ['Code', 'Country'], rows: codes.map((k) => ({ key: k, cells: [`<span class="cell-flag">${flag(k, true)}<span class="mono">${esc(k)}</span></span>`, `<a href="${countryHref(k)}">${esc(COUNTRIES[k] ?? k)}</a>`] })) });
    } else if (tab === 'calendars') {
        const codes = CURRENCY_CALENDARS[c.iso_code] ?? [];
        el.innerHTML = linkPanel({ title: 'Calendars', lead: 'The holiday calendars used for the currency\'s spot and settlement dates. A calendar opens its page.', addLabel: 'Add calendar', may: admin, nameOf: (r) => CALENDARS[r.key] ?? r.key,
            columns: ['Code', 'Calendar'], rows: codes.map((k) => ({ key: k, cells: [`<span class="cell-flag">${flag(CALENDAR_COUNTRY[k] ?? 'ZZ', true)}<span class="mono">${esc(k)}</span></span>`, `<a href="${calendarHref(k)}">${esc(CALENDARS[k] ?? k)}</a>`] })) });
    } else if (tab === 'groups') {
        const codes = GROUPS.filter((g) => (MEMBERS[g.code] ?? []).includes(c.iso_code)).map((g) => g.code);
        el.innerHTML = linkPanel({ title: 'Desk groups', lead: 'The desk groups the currency belongs to. A group opens its page.', addLabel: 'Add desk group', may: admin, nameOf: (r) => group(r.key).name,
            columns: ['Code', 'Desk group'], rows: codes.map((k) => ({ key: k, cells: [`<span class="mono">${esc(k)}</span>`, `<a href="${groupHref(k)}">${esc(group(k).name)}</a>`] })) });
    } else {
        el.innerHTML = timeline({ record: `currency ${c.iso_code}`, events: HISTORY[`currency:${c.iso_code}`] ?? [SEED_EVENT(c.version)], fields: currencyFields(c), canRevert: admin, gaps: ['The links to countries, calendars and desk groups are not versioned, so a link added or removed does not appear.'] });
    }
    dialogsCurrency(c, tab);
}
function sampleAmount(c) {
    const n = c.rounding_precision === 0 ? '1,234' : `1,234.${'5'.padEnd(c.rounding_precision, '6')}`;
    return c.format.replace(/#,##0(\.0+)?/, n);
}
function currencyFields(c) {
    return [['ISO code', c.iso_code], ['Name', c.name], ['Numeric code', c.numeric_code], ['Symbol', c.symbol], ['Fraction symbol', c.fraction_symbol || '—'], ['Fractions per unit', c.fractions_per_unit], ['Rounding type', ROUNDING[c.rounding_type]], ['Rounding precision', c.rounding_precision], ['Format', c.format], ['Monetary nature', NATURES[c.monetary_nature]], ['Market tier', TIERS[c.market_tier]], ['ORE currency type', c.ore_currency_type], ['Spot days', c.spot_days], ['Day basis', c.day_basis], ['Base precedence', c.base_precedence]];
}

const GROUP_TABS = { details: 'Details', members: 'Members', history: 'History' };
function renderGroup() {
    const g = group(state.code);
    if (!g) { main.innerHTML = `<div class="notice error">There is no desk group ${esc(state.code)}. <a href="${href({ screen: 'groups', code: '' })}">Back to the desk groups</a></div>`; return; }
    const tabs = Object.keys(GROUP_TABS);
    const tab = tabs.includes(state.tab) ? state.tab : 'details';
    document.title = `ORE Studio ${g.name}`;
    main.innerHTML = recordHead({ crumbsItems: [...ROOT, { label: 'Desk groups', href: href({ screen: 'groups', code: '', tab: '', dialog: '' }) }, { label: g.code }], title: g.name, key: g.code, version: g.version, editable: true, editTitle: g.name })
        + tabsNav(tabs, GROUP_TABS, tab) + '<div id="tab"></div>';
    const el = document.getElementById('tab');
    const members = MEMBERS[g.code] ?? [];
    if (tab === 'details') {
        el.innerHTML = detailsCard([
            ['Code', `<span class="mono">${esc(g.code)}</span>`, 'Set when the record was made, and never changed here.'],
            ['Name', esc(g.name)],
            ['Description', esc(g.description)],
            ['Display order', `<span class="num">${g.display_order}</span>`, 'Where the group stands in a list of groups.'],
            ['Members', `<span class="num">${members.length}</span>`, 'From the Members tab.'],
        ], g, 'system.external_data_import');
    } else if (tab === 'members') {
        el.innerHTML = linkPanel({ title: 'Members', lead: 'The currencies in the group. A currency opens its page.', addLabel: 'Add currency', may: admin, nameOf: (r) => currency(r.key)?.name ?? r.key,
            columns: ['ISO code', 'Currency'], rows: members.map((k) => ({ key: k, cells: [`<span class="cell-flag">${flag(k, true)}<span class="mono">${esc(k)}</span></span>`, `<a href="${currencyHref(k)}">${esc(currency(k)?.name ?? k)}</a>`] })) });
    } else {
        el.innerHTML = timeline({ record: `desk group ${g.code}`, events: HISTORY[`group:${g.code}`] ?? [{ ...SEED_EVENT(g.version), note: 'Imported from DQ dataset: FX Market Conventions' }], fields: [['Code', g.code], ['Name', g.name], ['Description', g.description], ['Display order', g.display_order]], canRevert: admin, gaps: ['The members are links and not versioned, so a currency added or removed does not appear.'] });
    }
    dialogsGroup(g, tab);
}

const PAIR_TABS = { details: 'Details', calendars: 'Calendars', history: 'History' };
function renderPair() {
    const p = pair(state.code);
    if (!p) { main.innerHTML = `<div class="notice error">There is no currency pair ${esc(state.code)}. <a href="${href({ screen: 'pairs', code: '' })}">Back to the currency pairs</a></div>`; return; }
    const v = conventionOf(p);
    const tabs = Object.keys(PAIR_TABS);
    const tab = tabs.includes(state.tab) ? state.tab : 'details';
    document.title = `ORE Studio ${p.pair_code}`;
    main.innerHTML = recordHead({ crumbsItems: [...ROOT, { label: 'Currency pairs', href: href({ screen: 'pairs', code: '', tab: '', dialog: '' }) }, { label: p.pair_code }], title: p.pair_code, titleFlag: pairFlag(p), key: p.pair_code, version: p.version, editable: true, editTitle: p.pair_code })
        + tabsNav(tabs, PAIR_TABS, tab) + '<div id="tab"></div>';
    const el = document.getElementById('tab');
    if (tab === 'details') {
        el.innerHTML = detailsCard([
            ['Pair code', `<span class="cell-flag">${pairFlag(p)}<span class="mono">${esc(p.pair_code)}</span></span>`, 'Base over quote. Set when the pair was made, and never changed here.'],
            ['Base currency', currencyLink(p.base_currency, true)],
            ['Quote currency', currencyLink(p.quote_currency, true)],
            ['Classification', classified('currency-pair-classification', CLASSIFICATIONS, p.classification)],
        ], p, 'system.external_data_import')
        + `<h3 style="margin-top:0.5rem">Quoting convention</h3>`
        + detailsCard([
            ['Pip factor', `<span class="num">${v.pip_factor}</span>`, 'The size of one pip.'],
            ['Tick size (pips)', `<span class="num">${v.tick_size}</span>`],
            ['Decimal places', `<span class="num">${v.decimal_places}</span>`, `A rate at this precision reads ${esc(sampleRate(v.decimal_places))}.`],
            ['Business day convention', classified('business-day-convention-type', BDC, v.business_day_convention)],
            ['Spot relative', v.spot_relative ? 'Yes' : 'No', 'Settlement counts from spot, not from trade date.'],
            ['End of month', v.end_of_month ? 'Yes' : 'No'],
        ], { ...v, modified_by: v.version > 1 ? 'adrian.vance' : undefined, change_reason_code: v.version > 1 ? 'business.data_not_updated' : undefined, change_commentary: v.version > 1 ? 'Half-pip ticks, five decimals, as the venue quotes.' : '' }, 'system.external_data_import');
    } else if (tab === 'calendars') {
        const codes = PAIR_CALENDARS(p);
        el.innerHTML = linkPanel({ title: 'Settlement calendars', lead: 'The holiday calendars the pair settles on: the union of the two currencies\' calendars by default, and any the desk adds.', addLabel: 'Add calendar', may: admin, nameOf: (r) => CALENDARS[r.key] ?? r.key,
            columns: ['Code', 'Calendar'], rows: codes.map((k) => ({ key: k, cells: [`<span class="cell-flag">${flag(CALENDAR_COUNTRY[k] ?? 'ZZ', true)}<span class="mono">${esc(k)}</span></span>`, `<a href="${calendarHref(k)}">${esc(CALENDARS[k] ?? k)}</a>`] })) });
    } else {
        /* RS-HIST-01: the pair's story carries its convention, each entry named by its part. */
        const own = HISTORY[`pair:${p.pair_code}`] ?? [{ ...SEED_EVENT(p.version), note: 'Imported from DQ dataset: FX Market Conventions' }];
        const part = (HISTORY[`convention:${p.pair_code}`] ?? [{ ...SEED_EVENT(1), at: '2026-10-05T11:04:43Z', note: 'Imported from DQ dataset: FX Market Conventions' }]).map((e) => ({ ...e, part: 'Convention' }));
        const events = [...own, ...part].sort((a, b) => b.at.localeCompare(a.at));
        el.innerHTML = timeline({ record: `pair ${p.pair_code}`, events, fields: [['Pair code', p.pair_code], ['Base currency', p.base_currency], ['Quote currency', p.quote_currency], ['Classification', CLASSIFICATIONS[p.classification]]], canRevert: admin, gaps: ['A convention deleted from the pair is not read, so its versions do not appear.', 'The settlement calendars are links and not versioned.'] });
    }
    dialogsPair(p, v, tab);
}
function sampleRate(places) { return (1.23456789).toFixed(places); }

/* The shared timeline: the event chart, then the rail of entries, newest first.
   An entry of a part carries the part's name (RS-HIST-01). */
function timeline({ record, events, fields, canRevert, gaps }) {
    const hide = q.get('sys') === 'hide';
    const shown = hide ? events.filter((e) => !e.service) : events;
    const newest = events.find((e) => !e.part) ?? events[0];
    const dayOf = (at) => at.slice(0, 10);
    const longDay = (d) => new Date(d + 'T00:00:00Z').toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' }).toUpperCase();
    const clock = (at) => at.slice(11, 19);
    const stamp = (at) => new Date(at).toLocaleString('en-GB', { day: 'numeric', month: 'short', year: 'numeric', hour: '2-digit', minute: '2-digit', timeZone: 'UTC' });
    const addDays = (d, n) => { const x = new Date(d); x.setUTCDate(x.getUTCDate() + n); return x; };
    const days = [];
    const stamps = shown.map((e) => dayOf(e.at)).sort();
    if (stamps.length) {
        const first = new Date(stamps[0] + 'T00:00:00Z');
        const last = new Date(stamps[stamps.length - 1] + 'T00:00:00Z');
        for (let d = first; d <= last; d = addDays(d, 1)) days.push(d.toISOString().slice(0, 10));
    }
    const counts = Object.fromEntries(days.map((d) => [d, {}]));
    for (const e of shown) counts[dayOf(e.at)][e.kind] = (counts[dayOf(e.at)][e.kind] ?? 0) + 1;
    const max = Math.max(1, ...Object.values(counts).map((k) => Object.values(k).reduce((a, b) => a + b, 0)));
    const kinds = {};
    for (const e of shown) kinds[e.kind] = (kinds[e.kind] ?? 0) + 1;
    const chart = `<div class="chart card">
        <div class="bars" style="--max:${max}">${days.map((d) => { const k = counts[d]; const total = Object.values(k).reduce((a, b) => a + b, 0); return `<div class="bar" title="${d}: ${total} ${total === 1 ? 'entry' : 'entries'}" style="--n:${total}">${Object.entries(k).map(([kind, n]) => `<span style="flex:${n};background:${KIND_COLOUR[kind]}" title="${kind} ${n}"></span>`).join('')}</div>`; }).join('')}</div>
        <div class="chart-foot"><span>${days.length ? stamp(shown[shown.length - 1].at) : ''}</span><span>Times in UTC</span><span>${days.length ? stamp(shown[0].at) : ''}</span></div>
        <div class="legend">${Object.entries(kinds).map(([k, n]) => `<span><i style="background:${KIND_COLOUR[k]}"></i>${k} ${n}</span>`).join('')}</div>
      </div>`;
    let day = '';
    const rows = shown.map((e) => {
        const current = e === newest;
        const divider = dayOf(e.at) !== day ? `<li class="day">${longDay(dayOf(e.at))}</li>` : '';
        day = dayOf(e.at);
        const changedNames = e.changes.map(([f]) => f).join(', ');
        const detail = e.changes.length > 0
            ? `<details><summary>Details</summary><table class="changes"><thead><tr><th>Field</th><th>Was</th><th>Now</th></tr></thead><tbody>${e.changes.map(([f, a, b]) => `<tr><td>${esc(f)}</td><td class="old">${esc(a)}</td><td class="new">${esc(b)}</td></tr>`).join('')}</tbody></table></details>`
            : e.kind === 'Changed' ? `<p class="small faint">No changes</p>`
            : e.part ? '' : `<details><summary>Details</summary><table class="changes"><tbody>${fields.map(([f, v]) => `<tr><td>${esc(f)}</td><td class="new">${esc(v)}</td></tr>`).join('')}</tbody></table></details>`;
        const revert = canRevert && !current && !e.part ? `<div class="entry-actions"><a class="btn sm" href="${href({ dialog: 'revert', v: e.version })}">${icon('revert')} Revert</a></div>` : '';
        return `${divider}<li class="entry">
          <div class="gutter"><span class="mono">${clock(e.at)}</span>${current ? `<span class="current" role="img" aria-label="Current version" title="Current version">✓</span>` : ''}</div>
          <div class="rail"><span class="dot" style="border-color:${KIND_COLOUR[e.kind]}"></span></div>
          <div class="body">
            <div class="entry-head">
              <span class="avatar${e.service ? ' service' : ''}" title="${esc(e.by)}">${e.service ? '⚙' : esc(e.by.split('.').map((p) => p[0]).join('').toUpperCase())}</span>
              <span class="kind" style="border-color:${KIND_COLOUR[e.kind]};color:${KIND_COLOUR[e.kind]}">${e.kind}</span>
              <span class="mono muted">${esc(e.part ? `${e.part} v${e.version}` : `${record} v${e.version}`)}</span>
              <span class="reason">${esc(e.reason)}</span>
              <a href="#" class="actor">${esc(e.by)}</a>
              ${changedNames ? `<span class="muted">${esc(changedNames)}</span>` : ''}
              <span class="when faint small">${stamp(e.at)}</span>
            </div>
            ${e.note ? `<p class="note">${esc(e.note)}</p>` : ''}
            ${detail}
            ${revert}
          </div>
        </li>`;
    }).join('');
    const gapBox = gaps.length ? `<div class="notice"><strong class="small">What this history cannot show</strong><ul class="small">${gaps.map((g) => `<li>${esc(g)}</li>`).join('')}</ul></div>` : '';
    return `<div class="timeline">
      <div class="tl-bar"><a class="btn ${hide ? '' : 'primary'}" href="${href({ sys: hide ? '' : 'hide' })}">${hide ? 'Show system activity' : 'Hide system activity'}</a></div>
      ${chart}
      <ol class="entries">${rows || '<li class="small muted">Every entry is system activity, and it is hidden.</li>'}</ol>
      ${gapBox}</div>`;
}

// ---------------------------------------------------------------------------
// Dialogs. Every write goes through one of the shared shapes: the record
// dialog, the remove dialog, the revert dialog, the operation dialog; and the
// Import steps page.
// ---------------------------------------------------------------------------
function field(label, control, help, span) {
    return `<div class="field${span ? ' span' : ''}"><label>${esc(label)}</label>${control}${help ? `<span class="help">${esc(help)}</span>` : ''}</div>`;
}
function input(value, attrs = '') { return `<input type="text" value="${esc(value ?? '')}" ${attrs}>`; }
function number(value, attrs = '') { return `<input type="number" value="${esc(value ?? '')}" ${attrs}>`; }
function select(options, value, attrs = '') { return `<select ${attrs}>${options.map(([v, l]) => `<option value="${esc(v)}" ${String(v) === String(value) ? 'selected' : ''}>${esc(l)}</option>`).join('')}</select>`; }
const choices = (map) => [['', '—'], ...Object.entries(map)];
const yesNo = (v) => select([['true', 'Yes'], ['false', 'No']], String(v));
function reasonFields(firstIsTouch) {
    const options = REASONS.map((r) => [r, r]);
    return field('Why', select(options, firstIsTouch ? REASONS[0] : REASONS[1]), null, true) + field('Commentary', '<textarea rows="2"></textarea>', null, true);
}
function newNote(still) { return `<p class="small muted span">The row is recorded as a new record; no reason is needed.</p>${still ? `<p class="still span">Still to fill in: ${esc(still)}</p>` : ''}`; }
function dialogShell({ title, loaded, body, foot, narrow }) {
    layer.innerHTML = `<div class="scrim" role="presentation"><div class="dialog${narrow ? ' narrow' : ''}" role="dialog" aria-modal="true" aria-labelledby="dlg-title">
      <div class="dialog-head"><div><h2 id="dlg-title">${title}</h2>${loaded ? `<span class="loaded">Loaded ${esc(loaded)}</span>` : ''}</div><a class="x closes" aria-label="Close" href="${href({ dialog: '', link: '', guard: '', v: '' })}">✕</a></div>
      <div class="dialog-body">${body}</div>
      <div class="dialog-foot">${foot}</div></div></div>`;
    guardChanges();
}
/* RS-WRITE-24: closing with changes asks first. */
function guardChanges() {
    let dirty = false;
    layer.querySelectorAll('.dialog input, .dialog select, .dialog textarea').forEach((el) => el.addEventListener('input', () => { dirty = true; }));
    layer.querySelectorAll('.dialog a.closes').forEach((a) => a.addEventListener('click', (event) => {
        if (!dirty) return;
        event.preventDefault();
        go({ guard: '1' });
    }));
}
function cancelLink() { return `<a class="btn closes" href="${href({ dialog: '', link: '', guard: '', v: '' })}">${icon('cancel')} Cancel</a>`; }
function saveLink(label, iconName, done, extra = {}) { return `<a class="btn primary" href="${href({ dialog: '', link: '', done, ...extra })}">${icon(iconName ?? 'save')} ${label}</a>`; }

/* The currency dialog: sixteen fields, so three groups (RS-WRITE-09). Day basis
   and ORE currency type are selects over their lists, not free text
   (RS-WRITE-01); the numbers are number controls (RS-WRITE-32). */
function dialogCurrency(c) {
    dialogShell({
        title: c ? `Edit ${esc(c.name)}` : 'Add a currency',
        loaded: c ? LOADED : null,
        body: `<div class="form-grid">
          <h3 class="group-title">Identity</h3>
          ${field('ISO code', input(c ? c.iso_code : '', c ? 'readonly' : 'placeholder="EUR" maxlength="3"'), c ? 'Set when the record was made, and never changed here.' : 'Three letters, ISO 4217.')}
          ${field('Name', input(c ? c.name : ''))}
          ${field('Numeric code', input(c ? c.numeric_code : '', 'maxlength="3" inputmode="numeric"'), 'Three digits, ISO 4217.')}
          ${field('Flag', `<span class="flag">${esc(c ? c.iso_code : 'ZZ')}</span>`, 'Optional. A picture for the list and the page.')}
          <h3 class="group-title">Display</h3>
          ${field('Symbol', input(c ? c.symbol : ''))}
          ${field('Fraction symbol', input(c ? c.fraction_symbol : ''), 'Optional. Empty for a currency with no fraction.')}
          ${field('Fractions per unit', number(c ? c.fractions_per_unit : '', 'min="0"'), '100 for cents; 0 for a currency with no fraction.')}
          ${field('Rounding type', select(choices(ROUNDING), c ? c.rounding_type : ''))}
          ${field('Rounding precision', number(c ? c.rounding_precision : '', 'min="0"'), 'Decimal places an amount is rounded to.')}
          ${field('Format', input(c ? c.format : '', 'placeholder="€#,##0.00"'), c ? `Reads ${esc(sampleAmount(c))}.` : 'A pattern with # for a digit and 0 for a digit always shown.')}
          <h3 class="group-title">Market</h3>
          ${field('Monetary nature', select(choices(NATURES), c ? c.monetary_nature : ''))}
          ${field('Market tier', select(choices(TIERS), c ? c.market_tier : ''))}
          ${field('ORE currency type', select(choices(ORE_TYPES), c ? c.ore_currency_type : ''), 'Optional. What ORE writes in the currency configuration.')}
          ${field('Spot days', number(c ? c.spot_days : '', 'min="0"'), 'Business days from trade to spot.')}
          ${field('Day basis', select(choices(DAY_BASES), c ? c.day_basis : ''))}
          ${field('Base precedence', number(c ? c.base_precedence : '', 'min="0"'), 'Lower quotes first in a pair.')}
          ${c ? reasonFields(true) : newNote('ISO code, Name, Numeric code, Symbol, Fractions per unit, Rounding type, Rounding precision, Format, Monetary nature, Market tier, Spot days, Day basis, Base precedence')}
        </div>`,
        foot: `${cancelLink()}${saveLink(c ? 'Save' : 'Add', c ? 'save' : 'add', c ? 'saved' : 'added', c ? {} : { screen: 'currency', code: 'XAU', tab: '' })}`,
    });
}
function dialogGroup(g) {
    dialogShell({
        title: g ? `Edit ${esc(g.name)}` : 'Add a desk group',
        loaded: g ? LOADED : null,
        body: `<div class="form-grid">
          ${field('Code', input(g ? g.code : '', g ? 'readonly' : 'placeholder="MAJORS"'), g ? 'Set when the record was made, and never changed here.' : 'Letters and digits, upper case.')}
          ${field('Name', input(g ? g.name : ''))}
          ${field('Description', input(g ? g.description : ''), 'Optional.', true)}
          ${field('Display order', number(g ? g.display_order : '', 'min="1"'), 'Where the group stands in a list of groups.')}
          ${g ? reasonFields(true) : newNote('Code, Name, Display order')}
        </div>`,
        foot: `${cancelLink()}${saveLink(g ? 'Save' : 'Add', g ? 'save' : 'add', g ? 'saved' : 'added', g ? {} : { screen: 'group', code: 'G11', tab: '' })}`,
    });
}
/* The pair dialog: one dialog of two groups, the pair and its convention
   (RS-WRITE-09); the code is derived and not asked for (RS-WRITE-07); the two
   legs must differ, said before the save (RS-WRITE-08); the sample rate is a
   line of help (RS-WRITE-12). */
function dialogPair(p, v) {
    const currencyChoices = [['', '—'], ...CURRENCIES.map((c) => [c.iso_code, `${c.iso_code} · ${c.name}`])];
    const same = q.get('legs') === 'same';
    const base = p ? p.base_currency : (same ? 'EUR' : '');
    const quote = p ? p.quote_currency : (same ? 'EUR' : '');
    dialogShell({
        title: p ? `Edit ${esc(p.pair_code)}` : 'Add a currency pair',
        loaded: p ? LOADED : null,
        body: `<div class="form-grid three">
          <h3 class="group-title">The pair</h3>
          ${field('Base currency', select(currencyChoices, base, p ? 'disabled' : ''), p ? 'Set when the pair was made, and never changed here.' : 'The currency one unit of which is priced.')}
          ${field('Quote currency', select(currencyChoices, quote, p ? 'disabled' : ''), p ? '' : 'The currency the price is in.')}
          ${field('Classification', select(choices(CLASSIFICATIONS), p ? p.classification : ''))}
          <p class="sample">Pair code: <span class="mono">${esc(base && quote ? `${base}/${quote}` : '—')}</span></p>
          ${same ? '<div class="notice warn span">The base and quote currencies must differ.</div>' : ''}
          <h3 class="group-title">Quoting convention</h3>
          ${field('Pip factor', number(v ? v.pip_factor : '0.0001', 'step="any" min="0"'), 'The size of one pip.')}
          ${field('Tick size (pips)', number(v ? v.tick_size : '1', 'step="any" min="0"'))}
          ${field('Decimal places', number(v ? v.decimal_places : '4', 'min="0" max="10"'))}
          ${field('Business day convention', select(choices(BDC), v ? v.business_day_convention : 'ModifiedFollowing'))}
          ${field('Spot relative', yesNo(v ? v.spot_relative : true), 'Settlement counts from spot, not from trade date.')}
          ${field('End of month', yesNo(v ? v.end_of_month : false))}
          <p class="sample">A rate at this precision reads <span class="mono">${esc(sampleRate(v ? v.decimal_places : 4))}</span>.</p>
          ${p ? reasonFields(true) : `${newNote(base && quote && !same ? 'Classification' : 'Base currency, Quote currency, Classification')}<p class="small faint span">The pair and its convention are two writes. If the second fails, the pair is saved without a convention and the page says so.</p>`}
        </div>`,
        foot: `${cancelLink()}${same ? `<a class="btn primary" aria-disabled="true">${icon('add')} Add</a>` : saveLink(p ? 'Save' : 'Add', p ? 'save' : 'add', p ? 'saved' : 'added', p ? {} : { screen: 'pair', code: 'EUR/USD', tab: '', legs: '' })}`,
    });
}
function dialogDelete({ name, what, after }) {
    dialogShell({ narrow: true, title: `Delete ${esc(name)}?`, body: `<div class="notice warn">${esc(what)}</div><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn danger" href="${href({ dialog: '', screen: after, code: '', tab: '', done: 'removed' })}">${icon('delete')} Delete</a>` });
}
function dialogRevert() {
    dialogShell({ narrow: true, title: `Revert to v${esc(q.get('v'))}?`, body: `<p class="muted small">A new version is written with the values of v${esc(q.get('v'))}. Nothing is deleted.</p><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn primary" href="${href({ dialog: '', done: 'reverted', v: '' })}">${icon('revert')} Revert</a>` });
}
function dialogAddLink({ title, label, options, help }) {
    dialogShell({ narrow: true, title: esc(title), body: `<div class="form-grid">${field(label, select([['', '—'], ...options]), help, true)}<p class="small muted span">The link is recorded as a new record; no reason is needed.</p></div>`, foot: `${cancelLink()}${saveLink('Add', 'add', 'linked')}` });
}
function dialogRemoveLink({ name, what }) {
    dialogShell({ narrow: true, title: `Remove ${esc(name)}?`, body: `<p class="muted small">${esc(what)}</p><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn danger" href="${href({ dialog: '', link: '', done: 'unlinked' })}">${icon('remove')} Remove</a>` });
}
/* RS-ORE-04: Export is one reply. The dialog names the document and its scope
   before the press, and the browser saves what the server returns. */
function dialogExport() {
    dialogShell({
        narrow: true,
        title: 'Export',
        body: `<p class="muted small">Writes <span class="mono">currencyconfig.xml</span>, the ORE currency configuration, with the tenant's <strong>${CURRENCIES.length} currencies</strong> as they stand now.</p>
          <p class="small faint">The server writes the document; your browser saves it. Nothing changes here.</p>`,
        foot: `${cancelLink()}<a class="btn primary" href="${href({ dialog: '', done: 'exported' })}">${icon('export')} Export</a>`,
    });
}

function dialogsCurrency(c, tab) {
    if (state.done) return doneToast();
    if (state.guard) return guardBox();
    if (!admin) { layer.innerHTML = ''; return; }
    switch (state.dialog) {
        case 'edit': return dialogCurrency(c);
        case 'delete': return dialogDelete({ name: c.name, what: 'This removes the currency and its links to countries, calendars and desk groups. Nothing checks whether trades, books or pairs still use it; a record that still names it fails its next save.', after: 'currencies' });
        case 'revert': return dialogRevert();
        case 'add-link': {
            if (tab === 'countries') return dialogAddLink({ title: `Add a country to ${c.name}`, label: 'Country', options: Object.entries(COUNTRIES).filter(([k]) => !(CURRENCY_COUNTRIES[c.iso_code] ?? []).includes(k) && k !== 'ZZ').map(([k, n]) => [k, `${k} · ${n}`]), help: 'A country not linked yet.' });
            if (tab === 'calendars') return dialogAddLink({ title: `Add a calendar to ${c.name}`, label: 'Calendar', options: Object.entries(CALENDARS).filter(([k]) => !(CURRENCY_CALENDARS[c.iso_code] ?? []).includes(k)).map(([k, n]) => [k, `${k} · ${n}`]), help: 'A holiday calendar not linked yet.' });
            return dialogAddLink({ title: `Add ${c.name} to a desk group`, label: 'Desk group', options: GROUPS.filter((g) => !(MEMBERS[g.code] ?? []).includes(c.iso_code)).map((g) => [g.code, `${g.code} · ${g.name}`]), help: 'A group the currency is not in yet.' });
        }
        case 'remove-link': {
            const name = tab === 'countries' ? COUNTRIES[state.link] : tab === 'calendars' ? CALENDARS[state.link] : group(state.link)?.name;
            return dialogRemoveLink({ name: name ?? state.link, what: `${c.name} loses its link to ${name ?? state.link}. The ${tab === 'groups' ? 'desk group' : tab === 'countries' ? 'country' : 'calendar'} itself stays.` });
        }
        default: layer.innerHTML = '';
    }
}
function dialogsGroup(g, tab) {
    if (state.done) return doneToast();
    if (state.guard) return guardBox();
    if (!admin) { layer.innerHTML = ''; return; }
    switch (state.dialog) {
        case 'edit': return dialogGroup(g);
        case 'delete': return dialogDelete({ name: g.name, what: `This removes the group. Its ${(MEMBERS[g.code] ?? []).length} members are taken out of it first; the currencies themselves stay.`, after: 'groups' });
        case 'revert': return dialogRevert();
        case 'add-link': return dialogAddLink({ title: `Add a currency to ${g.name}`, label: 'Currency', options: CURRENCIES.filter((c) => !(MEMBERS[g.code] ?? []).includes(c.iso_code)).map((c) => [c.iso_code, `${c.iso_code} · ${c.name}`]), help: 'A currency not in the group yet.' });
        case 'remove-link': return dialogRemoveLink({ name: currency(state.link)?.name ?? state.link, what: `${currency(state.link)?.name ?? state.link} leaves ${g.name}. The currency itself stays.` });
        default: layer.innerHTML = '';
    }
}
function dialogsPair(p, v, tab) {
    if (state.done) return doneToast();
    if (state.guard) return guardBox();
    if (!admin) { layer.innerHTML = ''; return; }
    switch (state.dialog) {
        case 'edit': return dialogPair(p, v);
        case 'delete': return dialogDelete({ name: p.pair_code, what: 'This removes the pair and its convention. Nothing checks whether trades still use the pair.', after: 'pairs' });
        case 'revert': return dialogRevert();
        case 'add-link': return dialogAddLink({ title: `Add a calendar to ${p.pair_code}`, label: 'Calendar', options: Object.entries(CALENDARS).filter(([k]) => !PAIR_CALENDARS(p).includes(k)).map(([k, n]) => [k, `${k} · ${n}`]), help: 'A holiday calendar the pair does not settle on yet.' });
        case 'remove-link': return dialogRemoveLink({ name: CALENDARS[state.link] ?? state.link, what: `${p.pair_code} no longer settles on ${CALENDARS[state.link] ?? state.link}. The calendar itself stays.` });
        default: layer.innerHTML = '';
    }
}

// ---------------------------------------------------------------------------
// Import: the steps of the configuration standard on the steps UI, at its own
// address (RS-ORE-05). A table kind has no target to choose, so four steps.
// ---------------------------------------------------------------------------
const STEPS = ['Choose the file', 'What the file holds', 'What the system does not know', 'The difference, and the review'];
const IMPORTED = { 'In the file': 14, 'Already held': 11, 'Changed': 2, 'New': 1, 'Not in the file': 1 };
const UNKNOWN = state.clean ? { values: [], dropped: [], duplicates: [] } : {
    values: [['RoundingType', 'Nearest', 'Rounding type', 'Round to nearest is the closest row; the import writes nothing until you choose']],
    dropped: [['<Currency><Decimals>', 'No column holds it; ORE derives it from the rounding precision']],
    duplicates: [['USD', 'Appears twice; the second has Spot days 2 and the first 1']],
};
const DIFF = {
    removed: [['XAU', 'Gold', 'Not in the file']],
    added: [['MXN', 'Mexican Peso', 'New to the tenant']],
    changed: [['EUR', 'Euro', 'Spot days 2 → 1'], ['JPY', 'Japanese Yen', 'Format ¥#,##0 → ¥#,##0.00']],
};
function renderImport() {
    document.title = 'ORE Studio Import currencies';
    if (!admin) { main.innerHTML = `<div class="notice error">Only a person who may write currencies can import them. <a href="${href({ screen: 'currencies' })}">Back to the currencies</a></div>`; return; }
    const step = Math.min(STEPS.length, Math.max(1, state.step));
    const wrong = state.file === 'wrong';
    const rail = `<nav class="railnav" aria-label="Steps"><ol>${STEPS.map((s, i) => { const n = i + 1; const cls = n < step ? 'done' : n === step ? 'current' : 'ahead'; return `<li class="railentry ${cls}"><span class="railmark ${cls}">${n < step ? '✓' : n}</span><span>${esc(s)}</span></li>`; }).join('')}</ol></nav>`;
    const bodies = {
        1: `<p class="muted">An ORE <span class="mono">currencyconfig.xml</span>: the root element is <span class="mono">CurrencyConfig</span>. A file of another kind is refused.</p>
            <div class="drop">${icon('file')}<span>Drop the file here, or <a href="${href({ file: 'ok' })}">choose one</a>.</span>${state.file ? `<span class="mono">${wrong ? 'calendaradjustment.xml' : 'currencyconfig.xml'} · 18 KB</span>` : ''}</div>
            ${wrong ? `<div class="notice error">This file is not a currency configuration: its root element is <span class="mono">CalendarAdjustments</span>. Choose a <span class="mono">CurrencyConfig</span> file, or import it from the calendars.</div>` : ''}
            <p class="small faint">Try <a href="${href({ file: 'wrong' })}">the wrong kind of file</a>.</p>`,
        2: `<p class="muted">The server read the file. Nothing is written yet.</p>
            <div class="counts">${Object.entries(IMPORTED).map(([l, n]) => `<div class="count"><span class="n">${n}</span><span class="l">${esc(l)}</span></div>`).join('')}</div>
            <p class="small faint">Already held is a currency the tenant has with the same values. Changed is one it has with other values. Not in the file is one the tenant has and the file omits.</p>`,
        3: UNKNOWN.values.length + UNKNOWN.dropped.length + UNKNOWN.duplicates.length === 0
            ? `<div class="notice ok">Everything in the file is known: every value has its row, every element has its column, and no currency appears twice.</div><p class="small faint">Try <a href="${href({ clean: '' })}">a file with unknowns</a>.</p>`
            : `${UNKNOWN.values.length ? `<h3>Values with no row</h3><p class="small muted">A value the lists do not hold. Choose the row it means, or the import leaves the field as the tenant has it.</p><section class="card"><table><thead><tr><th>Element</th><th>Value</th><th>List</th><th>What happens</th></tr></thead><tbody>${UNKNOWN.values.map(([e, v, l, w]) => `<tr><td class="mono">${esc(e)}</td><td class="mono">${esc(v)}</td><td><a href="${classificationHref('rounding-type')}">${esc(l)}</a></td><td>${esc(w)}: <span class="choice"><label><input type="radio" name="v1" checked> Round to nearest</label><label><input type="radio" name="v1"> Leave as held</label></span></td></tr>`).join('')}</tbody></table></section>` : ''}
               ${UNKNOWN.dropped.length ? `<h3>Elements with no column</h3><p class="small muted">Dropped, and named here so nothing is dropped in silence.</p><section class="card"><table><thead><tr><th>Element</th><th>Why</th></tr></thead><tbody>${UNKNOWN.dropped.map(([e, w]) => `<tr><td class="mono">${esc(e)}</td><td>${esc(w)}</td></tr>`).join('')}</tbody></table></section>` : ''}
               ${UNKNOWN.duplicates.length ? `<h3>Duplicates</h3><p class="small muted">A currency the file names twice. Choose which one to keep.</p><section class="card"><table><thead><tr><th>Currency</th><th>Where they differ</th><th>Keep</th></tr></thead><tbody>${UNKNOWN.duplicates.map(([c, w]) => `<tr><td class="mono">${esc(c)}</td><td>${esc(w)}</td><td><span class="choice"><label><input type="radio" name="d1" checked> The first</label><label><input type="radio" name="d1"> The second</label></span></td></tr>`).join('')}</tbody></table></section>` : ''}
               <p class="small faint">Try <a href="${href({ clean: '1' })}">a clean file</a>.</p>`,
        4: `<p class="muted">What the import writes, removed rows first. A file that omits a currency usually leaves it out by accident, so <strong>Keep it</strong> is the default.</p>
            <section class="card"><table class="diff"><thead><tr><th>Change</th><th>Currency</th><th>Name</th><th>What</th><th>Choice</th></tr></thead><tbody>
              ${DIFF.removed.map(([c, n, w]) => `<tr><td class="removed">Removed</td><td class="mono">${esc(c)}</td><td>${esc(n)}</td><td>${esc(w)}</td><td><span class="choice"><label><input type="radio" name="r-${c}" checked> Keep it</label><label><input type="radio" name="r-${c}"> Delete it</label></span></td></tr>`).join('')}
              ${DIFF.changed.map(([c, n, w]) => `<tr><td class="changed">Changed</td><td class="mono">${esc(c)}</td><td>${esc(n)}</td><td>${esc(w)}</td><td><span class="choice"><label><input type="radio" name="c-${c}" checked> Take the file's</label><label><input type="radio" name="c-${c}"> Keep ours</label></span></td></tr>`).join('')}
              ${DIFF.added.map(([c, n, w]) => `<tr><td class="added">Added</td><td class="mono">${esc(c)}</td><td>${esc(n)}</td><td>${esc(w)}</td><td><span class="choice"><label><input type="radio" name="a-${c}" checked> Add it</label><label><input type="radio" name="a-${c}"> Skip it</label></span></td></tr>`).join('')}
            </tbody></table></section>
            <p class="small muted">Reach: the 3 currencies written are read by every pair, book and trade that names them. The write claims the versions read at ${esc(LOADED)}.</p>
            <div class="form-grid">${reasonFields(false)}</div>`,
    };
    const next = step < STEPS.length
        ? (step === 1 && (!state.file || wrong) ? `<a class="btn primary" aria-disabled="true">Next ${icon('right')}</a>` : `<a class="btn primary" href="${href({ step: step + 1 })}">Next ${icon('right')}</a>`)
        : `<a class="btn primary" href="${href({ screen: 'currencies', step: '', file: '', clean: '', done: 'imported' })}">${icon('import')} Import</a>`;
    main.innerHTML = `
      ${crumbs([...ROOT, { label: 'Currencies', href: href({ screen: 'currencies', step: '', file: '', clean: '' }) }, { label: 'Import' }])}
      <div class="head"><div class="titles"><h1>Import currencies</h1><p class="lead">Bring an ORE currency configuration in. The server reads it, you review the difference, and nothing is written before the last step.</p></div></div>
      <div class="journey">${rail}<section class="card"><div class="pad step"><h2>${step}. ${esc(STEPS[step - 1])}</h2>${bodies[step]}
        <div class="stepfoot"><a class="btn" href="${href({ screen: 'currencies', step: '', file: '', clean: '' })}">${icon('cancel')} Cancel</a><div class="right">${step > 1 ? `<a class="btn" href="${href({ step: step - 1 })}">${icon('left')} Back</a>` : ''}${next}</div></div>
      </div></section></div>`;
    if (state.guard) guardBox();
}

// ---------------------------------------------------------------------------
// The prototype bar
// ---------------------------------------------------------------------------
function bar() {
    document.getElementById('me').textContent = admin ? 'TA' : 'AV';
    document.getElementById('who-seg').innerHTML = [['admin', 'tenant_admin, who may write'], ['reader', 'a reader']].map(([v, l]) => `<a href="${href({ who: v, dialog: '' })}" aria-pressed="${state.who === v}">${l}</a>`).join('');
    const link = (changes, label) => `<a href="${href({ code: '', tab: '', dialog: '', link: '', done: '', guard: '', q: '', page: '', sort: '', dir: '', step: '', file: '', clean: '', v: '', sys: '', legs: '', ...changes })}">${label}</a>`;
    document.getElementById('proto-where').innerHTML = [
        link({ screen: 'currencies' }, 'Currencies'), link({ screen: 'currency', code: 'EUR' }, 'EUR'),
        link({ screen: 'groups' }, 'Desk groups'), link({ screen: 'group', code: 'G11' }, 'G11'),
        link({ screen: 'pairs' }, 'Pairs'), link({ screen: 'pair', code: 'EUR/USD' }, 'EUR/USD'),
        link({ screen: 'currencies', dialog: 'export' }, 'Export'), link({ screen: 'import' }, 'Import'),
    ].join(' · ');
}

({ currencies: () => renderList('currencies'), groups: () => renderList('groups'), pairs: () => renderList('pairs'), currency: renderCurrency, group: renderGroup, pair: renderPair, import: renderImport }[state.screen] ?? (() => renderList('currencies')))();
bar();

'use strict';
/* Booking a trade. Serves: Book a trade. Two variants of the same form: the
 * trade page filled in (default) and a guided sequence. The sequence is the
 * exception the entity specification allows only if it is quicker. */

const VARIANT = P.get('variant') === 'steps' ? 'steps' : 'page';
const FAIL = P.get('fail') || '';
const LIKE = P.get('like') ? trade(P.get('like')) : null;
const STEPS = ['Type', 'Parties and book', 'Terms', 'Identify', 'Review'];
let step = Number(P.get('step') || 0);

const main = shell({
    title: 'Book a trade', name: 'book-a-trade',
    question: 'Is booking quicker as one page of cards or as a guided sequence?',
    crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', '../trade-page/'], ['Book a trade']],
    states: [['page', { variant: null, step: null }, VARIANT === 'page'], ['steps', { variant: 'steps' }, VARIANT === 'steps'],
        ['half-booked', { fail: FAIL === 'instrument' ? null : 'instrument' }, FAIL === 'instrument'], ['duplicate', { fail: FAIL === 'duplicate' ? null : 'duplicate' }, FAIL === 'duplicate']],
    says: VARIANT === 'page'
        ? 'The trade page, filled in. The trade type comes first because it decides which instrument card appears. Nothing is written until Book. Compare with the steps variant: for one deal, is the sequence quicker, or only longer?'
        : 'The same form as a guided sequence. The entity specification allows a sequence only when it makes the task quicker or easier. Count the clicks to book one swap in each variant, then decide.'
});

/* ---------- the form ---------- */
const bookable = TRADE_TYPES.filter(t => t.instrument);
const F = {
    type: LIKE ? LIKE.type : '', productFilter: '', typeSearch: '',
    cp: LIKE ? LIKE.cp : '', book: LIKE ? LIKE.book : 'EMEA-RATES', date: '2026-10-11', time: '16:40', nature: 'Real', channel: 'Manual', scope: 'Bilateral',
    mode: 'live', terms: {}, ids: { firm: 'ACME-100309', ore: '' }, portfolio: 'Rates and credit', trader: ME
};
function termFields() {
    if (!F.type) return [];
    const tt = tradeType(F.type);
    const seed = LIKE || { code: 'new', type: F.type, table: tt.instrument, notional: 10000000, ccy: 'EUR', date: F.date, ver: 1, ext: 1, state: 'draft', product: tt.product, cp: F.cp || 'NWCAP', channel: 'Manual' };
    const t2 = Object.assign({}, seed, { table: tt.instrument, type: F.type });
    const cards = tt.instrument ? instrumentCards(t2) : [];
    return cards[0] ? cards[0].fields.filter(fd => fd.key !== 'issue') : [];
}
function problems() {
    const out = [];
    if (!F.type) out.push('Choose a trade type.');
    else if (!tradeType(F.type).instrument) out.push(F.type + ' has no instrument table yet, so its terms cannot be held.');
    if (!F.cp) out.push('Choose a counterparty.');
    const b = book(F.book);
    if (b && b.virtual && !(F.mode === 'draft' || ['Test', 'Hypothetical', 'Pre-agreement draft'].includes(F.nature))) out.push('A virtual book holds only a draft, a test or a hypothetical. This trade is live and real: choose a real book.');
    return out;
}

/* ---------- sections ---------- */
function typeSection() {
    const all = TRADE_TYPES.filter(t => (!F.productFilter || t.product === F.productFilter) && (!F.typeSearch || (t.code + t.description).toLowerCase().includes(F.typeSearch.toLowerCase())));
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trade type'), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, bookable.length + ' of ' + TRADE_TYPES.length + ' can be booked: the other ' + (TRADE_TYPES.length - bookable.length) + ' have no instrument table yet')),
        h('div', { class: 'body', style: 'display:grid;gap:.6rem' },
            h('div', { class: 'filters', style: 'margin:0' }, h('input', { placeholder: 'Search trade types', value: F.typeSearch, oninput: debounce(e => { F.typeSearch = e.target.value; redraw(); }) }),
                select([['', 'All products']].concat(PRODUCTS.map(p => [p, p])), F.productFilter, v => { F.productFilter = v; redraw(); })),
            h('div', { class: 'picker' }, all.slice(0, 24).map(t => h('button', { class: 'opt' + (t.code === F.type ? ' on' : ''), disabled: !t.instrument, title: t.instrument ? '' : 'No instrument table yet', onclick: () => { F.type = t.code; F.terms = {}; redraw(); } },
                t.description, h('small', null, t.instrument ? t.product + ' · ' + t.instrument : t.product + ' · no instrument table')))),
            all.length > 24 ? h('div', { class: 'faint small' }, 'Showing 24 of ' + all.length + '. Search to narrow.') : null,
            F.type === 'CompositeTrade' ? h('div', { class: 'notice acc' }, 'A composite is built from its legs. ', h('a', { href: '../trade-entry/' }, 'Compose it in the structure editor'), ' (the trade-entry prototype), then book it here.') : null));
}
function partiesSection() {
    const b = book(F.book);
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Parties, book and how it was struck')),
        h('div', { class: 'body', style: 'display:grid;gap:.8rem' }, h('div', { class: 'grid2' },
            field('Party', h('input', { class: 'field-input', value: PARTY, disabled: true }), 'Fixed for the life of the trade.'),
            field('Counterparty', select([['', 'Choose…']].concat(COUNTERPARTIES.filter(c => c.open).map(c => [c.code, c.name])), F.cp, v => { F.cp = v; redraw(); }), 'Only counterparties this party may trade with. Holding North America Inc is closed, so it is not offered.'),
            field('Book', select(BOOKS.map(x => [x.code, x.name + (x.virtual ? ' (virtual)' : '')]), F.book, v => { F.book = v; redraw(); }), b.virtual ? 'A virtual book holds only a draft, a test or a hypothetical.' : ''),
            field('Booking nature', select(BOOKING_NATURES, F.nature, v => { F.nature = v; redraw(); })),
            field('Trade date', h('input', { class: 'field-input', type: 'date', value: F.date, oninput: e => { F.date = e.target.value; } })),
            field('Execution time', h('input', { class: 'field-input', type: 'time', value: F.time, oninput: e => { F.time = e.target.value; } })),
            field('Entry channel', select(ENTRY_CHANNELS, F.channel, v => { F.channel = v; })), field('Counterparty scope', select(COUNTERPARTY_SCOPES, F.scope, v => { F.scope = v; }))),
            h('div', { class: 'grid2' }, h('label', null, h('input', { type: 'radio', name: 'mode', checked: F.mode === 'live', onchange: () => { F.mode = 'live'; redraw(); } }), ' Book against an agreement already struck ', h('span', { class: 'faint small' }, '(activity new_booking, state live)')),
                h('label', null, h('input', { type: 'radio', name: 'mode', checked: F.mode === 'draft', onchange: () => { F.mode = 'draft'; if (F.nature === 'Real') F.nature = 'Pre-agreement draft'; redraw(); } }), ' Capture as a draft ', h('span', { class: 'faint small' }, '(activity draft_capture, state draft)')))));
}
function termsSection() {
    const fields = termFields();
    if (!fields.length) return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Instrument terms')), h('div', { class: 'body' }, h('span', { class: 'muted' }, 'Choose a trade type. Its terms appear here, and no others.')));
    return h('section', { class: 'card' }, h('header', null, h('h2', null, tradeTypeName(tradeType(F.type).instrument)), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, tradeType(F.type).instrument)),
        h('div', { class: 'body' }, h('div', { class: 'grid2' }, fields.map(fd => field(fd.label + (fd.economic ? ' · economic' : ''),
            h('input', { class: 'field-input', value: F.terms[fd.key] != null ? F.terms[fd.key] : fd.value, oninput: e => { F.terms[fd.key] = e.target.value; } }))))));
}
function identifySection() {
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Identifiers, roles and portfolio'), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, 'All optional')),
        h('div', { class: 'body' }, h('div', { class: 'grid2' }, field('Firm trade id', h('input', { class: 'field-input', value: F.ids.firm, oninput: e => { F.ids.firm = e.target.value; } })),
            field('ORE trade id', h('input', { class: 'field-input', value: F.ids.ore, placeholder: 'Trade_100309', oninput: e => { F.ids.ore = e.target.value; } })),
            field('Executing trader', h('div', null, person(F.trader))), field('Portfolio', select(['Rates and credit', 'FX flow', 'Equity'], F.portfolio, v => { F.portfolio = v; })))));
}
function reviewSection() {
    const ps = problems();
    const calls = ['trading.v1.ops.book_trade  (anchor, booking, first state, activity ' + (F.mode === 'live' ? 'new_booking' : 'draft_capture') + ')'];
    if (F.type && tradeType(F.type).instrument) calls.push('trading.v1.' + tradeType(F.type).instrument + 's.put  (the instrument)');
    calls.push('trading.v1.trade_identifiers.put_many', 'trading.v1.trade_party_roles.put_many', 'trading.v1.trade_portfolios.put_many');
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Review')), h('div', { class: 'body', style: 'display:grid;gap:.7rem' },
        h('dl', { class: 'fields' }, [['Trade', F.type ? tradeType(F.type).description : '—'], ['Counterparty', F.cp ? cpty(F.cp).name : '—'], ['Book', book(F.book).name], ['State', F.mode === 'live' ? 'live' : 'draft'], ['Trade date', F.date + ' ' + F.time], ['Nature', F.nature]]
            .map(([k, v]) => h('div', null, h('dt', null, k), h('dd', null, v)))),
        ps.length ? h('div', { class: 'notice bad' }, ps.map(p => h('div', null, p))) : null,
        h('div', { class: 'notice' }, h('b', null, 'Sent on Book'), calls.map(c => h('div', { class: 'mono small' }, c)),
            h('div', { class: 'faint small' }, 'book_trade does not write the instrument today, so the instrument is a second write. See the Book a trade journey, What is missing.'))));
}

/* ---------- booking ---------- */
let result = null;
function book_() {
    if (problems().length) return;
    if (FAIL === 'duplicate') { result = { error: 'already_exists', text: 'A trade with this id is already booked. book_trade answered conflict / already_exists. Nothing was written.' }; return redraw(); }
    const n = TRADES.length + 1;
    const tt = tradeType(F.type);
    const t = { id: tradeId(n), code: F.ids.firm, type: F.type, product: tt.product, table: tt.instrument, cp: F.cp, book: F.book, state: F.mode === 'live' ? 'live' : 'draft', auth: F.mode === 'live' ? false : null,
        date: F.date, notional: Number((F.terms.notional || F.terms.face_amount || F.terms.bought_amount || 10000000)) || 10000000, ccy: 'EUR', ext: 1, ver: 1, nature: F.nature, channel: F.channel, scope: F.scope, structure: null, updated: '2026-10-11T16:41:00+01:00' };
    const booked = (store('booked') || []).filter(x => x.code !== t.code).concat([t]);
    store('booked', booked);
    result = FAIL === 'instrument' ? { half: t } : { ok: t };
    redraw();
}

function resultView() {
    if (result.error) return h('div', { class: 'cards' }, h('div', { class: 'notice bad' }, h('b', null, 'Refused. '), result.text, ' ', h('button', { class: 'btn small', onclick: () => { result = null; redraw(); } }, 'Change the trade id')));
    const t = result.ok || result.half;
    return h('div', { class: 'cards' },
        result.half ? h('div', { class: 'notice warn' }, h('b', null, 'Booked, but the instrument was not written. '), t.code + ' exists with its booking and its first state and has no instrument terms. This is the half-booked trade the journey names. ',
            h('button', { class: 'btn small', onclick: () => { result = { ok: t }; redraw(); } }, 'Retry the instrument write')) : null,
        result.ok ? h('div', { class: 'notice ok' }, h('b', null, t.code + ' is booked. '), 'State ' + t.state + ', internal version 1, external version 1. Activity ' + (t.state === 'live' ? 'new_booking' : 'draft_capture') + '.') : null,
        h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/?trade=' + t.code }, 'Open the trade'), h('a', { class: 'btn', href: qs({ like: t.code }) }, 'Book a similar trade'),
            h('a', { class: 'btn', href: '../structure-page/?mode=group&trades=' + t.code }, 'Group with other trades'), h('a', { class: 'btn', href: '../trade-entry/pricing.html' }, 'Price it (trade-entry prototype)')));
}

/* ---------- layout ---------- */
function redraw() {
    main.querySelectorAll('.dyn').forEach(n => n.remove());
    const head = pageHead({ title: 'Book a trade', lead: 'Record a new deal, live or as a draft. Nothing is written until Book.' });
    let body;
    if (!CAN.book) body = h('div', { class: 'notice warn' }, h('b', null, 'You cannot use this screen. '), 'It needs the right to book trades. Ask a tenant administrator for the Trader role.');
    else if (result) body = resultView();
    else if (VARIANT === 'page') body = h('div', { class: 'cards' }, typeSection(), partiesSection(), termsSection(), identifySection(), reviewSection(),
        h('div', { class: 'filters' }, h('button', { class: 'btn primary', disabled: problems().length > 0, onclick: book_ }, 'Book'), h('a', { class: 'btn', href: '../trade-page/' }, 'Cancel')));
    else {
        const sections = [typeSection, partiesSection, termsSection, identifySection, reviewSection];
        const rail = h('nav', { class: 'rail-v' }, STEPS.map((s, i) => h('a', { href: '#', class: i === step ? 'on' : i < step ? 'done' : '', onclick: e => { e.preventDefault(); step = i; redraw(); } }, (i + 1) + '. ' + s)));
        const last = step === STEPS.length - 1;
        body = h('div', { class: 'layout' }, rail, h('div', { style: 'display:grid;gap:1rem' }, sections[step](),
            h('div', { class: 'filters' }, step ? h('button', { class: 'btn', onclick: () => { step--; redraw(); } }, 'Back') : null,
                last ? h('button', { class: 'btn primary', disabled: problems().length > 0, onclick: book_ }, 'Book') : h('button', { class: 'btn primary', onclick: () => { step++; redraw(); } }, 'Next'),
                h('a', { class: 'btn', href: '../trade-page/' }, 'Cancel'))));
    }
    const w = h('div', { class: 'dyn' }, head, body); main.append(w);
}
function debounce(fn) { let t; return e => { clearTimeout(t); t = setTimeout(() => fn(e), 300); }; }
redraw();

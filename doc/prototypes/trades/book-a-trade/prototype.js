'use strict';
/* Booking a trade. Serves: Book a trade.
 * One page, in tabs: the trade, its instrument, its identifiers, and the review.
 * The trade type is a combo box. It decides which instrument the Instrument tab shows,
 * and each part of the instrument (its terms, its legs, its premium) is a tab of its own. */

const FAIL = P.get('fail') || '';
const LIKE = P.get('like') ? trade(P.get('like')) : null;
const TOP = [['trade', 'Trade'], ['instrument', 'Instrument'], ['identifiers', 'Identifiers'], ['review', 'Review']];
let tab = TOP.some(t => t[0] === P.get('tab')) ? P.get('tab') : 'trade';
let subtab = '';

const main = shell({
    title: 'Book a trade', name: 'book-a-trade',
    question: 'Is booking quick on one page, with the instrument in tabs?',
    crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', '../trade-page/'], ['Book a trade']],
    states: [['book', { fail: null, like: null }, !FAIL && !LIKE], ['like an existing trade', { like: 'ACME-100201' }, !!LIKE], ['half-booked', { fail: FAIL === 'instrument' ? null : 'instrument' }, FAIL === 'instrument'], ['duplicate', { fail: FAIL === 'duplicate' ? null : 'duplicate' }, FAIL === 'duplicate']],
    says: 'One page, in tabs. The trade type is a combo box: it is the one choice that decides what the Instrument tab holds, and the tab then has a sub-tab for each part of that instrument. Nothing is written until Book. Choose a type, open the Instrument tab, and look at the parts a swap has and an FX option has.'
});

/* ---------- the form ---------- */
const bookable = TRADE_TYPES.filter(t => t.instrument);
const F = {
    type: LIKE ? LIKE.type : '',
    cp: LIKE ? LIKE.cp : '', book: LIKE ? LIKE.book : 'EMEA-RATES', date: '2026-10-11', time: '16:40', nature: 'Real', channel: 'Manual', scope: 'Bilateral',
    mode: 'live', terms: {}, rows: {}, ids: { firm: 'ACME-100309', ore: '' }, portfolio: 'Rates and credit', trader: ME
};
function seed() {
    const tt = tradeType(F.type);
    const base = LIKE || { code: 'new', type: F.type, notional: 10000000, ccy: 'EUR', date: F.date, ver: 1, ext: 1, state: 'draft', product: tt.product, cp: F.cp || 'NWCAP', channel: 'Manual' };
    return Object.assign({}, base, { table: tt.instrument, type: F.type });
}
/* The parts of the instrument: its terms, then each child table. */
function parts() {
    if (!F.type || !tradeType(F.type).instrument) return [];
    return instrumentCards(seed()).map(c => (c.id === 'instrument' ? Object.assign({}, c, { title: 'Terms' }) : c));
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
function show(next) {
    tab = next;
    history.replaceState(null, '', qs({ tab: next === 'trade' ? null : next }));
    redraw();
}

/* ---------- the tabs ---------- */
function typeCard() {
    const tt = F.type ? tradeType(F.type) : null;
    const byProduct = PRODUCTS.map(p => [p, TRADE_TYPES.filter(t => t.product === p)]).filter(([, l]) => l.length);
    const combo = h('select', { class: 'field-select', id: 'trade-type', 'aria-label': 'Trade type', style: 'width:100%', onchange: e => { F.type = e.target.value; F.terms = {}; F.rows = {}; subtab = ''; redraw(); } },
        h('option', { value: '' }, 'Choose a trade type…'),
        byProduct.map(([p, list]) => h('optgroup', { label: p.charAt(0).toUpperCase() + p.slice(1) }, list.map(t => h('option', { value: t.code, disabled: !t.instrument, selected: t.code === F.type },
            t.description + (t.instrument ? '' : ' — no instrument table yet'))))));
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trade type'), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, bookable.length + ' of ' + TRADE_TYPES.length + ' can be booked: the other ' + (TRADE_TYPES.length - bookable.length) + ' have no instrument table yet')),
        h('div', { class: 'body', style: 'display:grid;gap:.6rem' }, field('Trade type', combo, 'The trade type decides the instrument. Types with no instrument table are listed and cannot be chosen.'),
            tt ? h('div', { class: 'filters', style: 'margin:0' }, chip(tt.product, 'acc'), chip(tt.instrument || 'no instrument table', tt.instrument ? '' : 'bad'), chip(tt.code, 'v')) : null,
            F.type === 'CompositeTrade' ? h('div', { class: 'notice acc' }, 'A composite is built from its legs. ', h('a', { href: '../trade-entry/' }, 'Compose it in the structure editor'), ' (the trade-entry prototype), then book it here.') : null));
}
function partiesCard() {
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
function instrumentTab() {
    const list = parts();
    if (!list.length) return h('div', { class: 'notice' }, F.type ? F.type + ' has no instrument table yet, so there are no terms to enter.' : 'Choose a trade type on the Trade tab. Its instrument appears here, one tab for each part.');
    if (!list.some(c => c.id === subtab)) subtab = list[0].id;
    const c = list.find(x => x.id === subtab);
    const subs = h('nav', { class: 'tabs', role: 'tablist', 'aria-label': 'Parts of the instrument' }, list.map(p => h('a', { href: '#', class: p.id === subtab ? 'on' : '', role: 'tab', onclick: e => { e.preventDefault(); subtab = p.id; redraw(); } }, p.title)));
    let body;
    if (c.fields) {
        body = h('div', { class: 'grid2' }, c.fields.filter(fd => fd.key !== 'issue').map(fd => field(fd.label + (fd.economic ? ' · economic' : ''),
            h('input', { class: 'field-input', value: F.terms[fd.key] != null ? F.terms[fd.key] : fd.value, oninput: e => { F.terms[fd.key] = e.target.value; } }))));
    } else {
        const rows = F.rows[c.id] || (F.rows[c.id] = c.rows.map(r => r.slice()));
        body = h('div', { style: 'display:grid;gap:.5rem' }, h('table', { class: 'grid' }, h('thead', null, h('tr', null, c.columns.map(x => h('th', null, x)).concat(h('th', null)))),
            h('tbody', null, rows.map((r, ri) => h('tr', null, r.map((v, ci) => h('td', null, h('input', { class: 'field-input', value: v, oninput: e => { rows[ri][ci] = e.target.value; } }))),
                h('td', null, h('button', { class: 'btn small ghost', onclick: () => { rows.splice(ri, 1); redraw(); } }, 'Remove')))))),
            h('div', null, h('button', { class: 'btn small', onclick: () => { rows.push(c.columns.map(() => '')); redraw(); } }, '+ Add a row')),
            c.digestGap ? h('div', { class: 'notice warn' }, 'The economic digest does not read this table yet: a change to it leaves the external version alone.') : null);
    }
    return h('section', { class: 'card' }, h('header', null, h('h2', null, tradeTypeName(tradeType(F.type).instrument)), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, tradeType(F.type).instrument)), h('div', { class: 'body' }, subs, body));
}
function identifiersTab() {
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Identifiers, roles and portfolio'), h('span', { class: 'sp' }), h('span', { class: 'faint small' }, 'All optional')),
        h('div', { class: 'body' }, h('div', { class: 'grid2' }, field('Firm trade id', h('input', { class: 'field-input', value: F.ids.firm, oninput: e => { F.ids.firm = e.target.value; } })),
            field('ORE trade id', h('input', { class: 'field-input', value: F.ids.ore, placeholder: 'Trade_100309', oninput: e => { F.ids.ore = e.target.value; } })),
            field('Executing trader', h('div', null, person(F.trader))), field('Portfolio', select(['Rates and credit', 'FX flow', 'Equity'], F.portfolio, v => { F.portfolio = v; })))));
}
function reviewTab() {
    const ps = problems();
    const calls = ['trading.v1.ops.book_trade  (anchor, booking, first state, activity ' + (F.mode === 'live' ? 'new_booking' : 'draft_capture') + ')'];
    if (F.type && tradeType(F.type).instrument) calls.push('trading.v1.' + tradeType(F.type).instrument + 's.put  (the instrument, then each part: ' + parts().slice(1).map(p => p.table).join(', ') + ')');
    calls.push('trading.v1.trade_identifiers.put_many', 'trading.v1.trade_party_roles.put_many', 'trading.v1.trade_portfolios.put_many');
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Review')), h('div', { class: 'body', style: 'display:grid;gap:.7rem' },
        h('dl', { class: 'fields' }, [['Trade', F.type ? tradeType(F.type).description : '—'], ['Counterparty', F.cp ? cpty(F.cp).name : '—'], ['Book', book(F.book).name], ['State', F.mode === 'live' ? 'live' : 'draft'], ['Trade date', F.date + ' ' + F.time], ['Nature', F.nature]]
            .map(([k, v]) => h('div', null, h('dt', null, k), h('dd', null, v)))),
        ps.length ? h('div', { class: 'notice bad' }, ps.map(p => h('div', null, p))) : null,
        h('div', { class: 'notice' }, h('b', null, 'Sent on Book'), calls.map(c => h('div', { class: 'mono small' }, c)),
            h('div', { class: 'faint small' }, 'book_trade is a domain operation: it writes several entities as one transaction. It does not write the instrument today, so the instrument is a second, standard put. See the Book a trade journey, What is missing.'))));
}

/* ---------- booking ---------- */
let result = null;
function bookIt() {
    if (problems().length) return;
    if (FAIL === 'duplicate') { result = { error: 'already_exists', text: 'A trade with this id is already booked. book_trade answered conflict with the code already_exists. Nothing was written.' }; return redraw(); }
    const n = TRADES.length + 1;
    const tt = tradeType(F.type);
    const t = { id: tradeId(n), code: F.ids.firm, type: F.type, product: tt.product, table: tt.instrument, cp: F.cp, book: F.book, state: F.mode === 'live' ? 'live' : 'draft',
        date: F.date, notional: Number((F.terms.notional || F.terms.face_amount || F.terms.bought_amount || 10000000)) || 10000000, ccy: 'EUR', ext: 1, ver: 1, nature: F.nature, channel: F.channel, scope: F.scope, structure: null, updated: '2026-10-11T16:41:00+01:00' };
    store('booked', (store('booked') || []).filter(x => x.code !== t.code).concat([t]));
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
        h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/?trade=' + t.code }, 'Open the trade'), h('a', { class: 'btn', href: qs({ like: t.code, fail: null }) }, 'Book a similar trade'),
            h('a', { class: 'btn', href: '../structure-page/?mode=group&trades=' + t.code }, 'Group with other trades'), h('a', { class: 'btn', href: '../trade-entry/pricing.html' }, 'Price it (trade-entry prototype)')));
}

/* ---------- layout ---------- */
function redraw() {
    main.querySelectorAll('.dyn').forEach(n => n.remove());
    const head = pageHead({ title: 'Book a trade', lead: 'Record a new deal, live or as a draft. Nothing is written until Book.',
        actions: CAN.book && !result ? h('button', { class: 'btn primary', disabled: problems().length > 0, onclick: bookIt }, 'Book') : null });
    let body;
    if (!CAN.book) body = h('div', { class: 'notice warn' }, h('b', null, 'You cannot use this screen. '), 'It needs the right to book trades. Ask a tenant administrator for the Trader role.');
    else if (result) body = resultView();
    else {
        const nav = h('nav', { class: 'tabs', role: 'tablist' }, TOP.map(([id, label]) => h('a', { href: '#', class: id === tab ? 'on' : '', role: 'tab', onclick: e => { e.preventDefault(); show(id); } },
            label + (id === 'instrument' && parts().length ? ' · ' + parts().length : ''))));
        const content = tab === 'instrument' ? instrumentTab() : tab === 'identifiers' ? identifiersTab() : tab === 'review' ? reviewTab() : h('div', { class: 'cards' }, typeCard(), partiesCard());
        body = h('div', null, nav, content);
    }
    main.append(h('div', { class: 'dyn' }, head, body));
}
redraw();

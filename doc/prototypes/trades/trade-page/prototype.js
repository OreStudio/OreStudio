'use strict';
/* The trade list and the trade page. Serves: review, amend, lifecycle event,
 * and the doors to booking, ORE import and export, and structures. */

const STATE_P = P.get('state') || '';
const SERVER = P.get('server') === 'today' ? 'today' : 'target';
let TAB = P.get('tab') || 'details';
let WORKFLOW = P.get('workflow') || '';
const CODE = P.get('trade');
const STRUCT_KIND = { 'PKG-0004': { kind: 'Package', whole: false, legs: 2 }, 'STR-0007': { kind: 'Strategy', template: 'RiskReversal', whole: true, legs: 2, parent: 'STR-0010' }, 'STR-0009': { kind: 'Typed', template: 'CallableSwap', whole: true, legs: 2 }, 'STR-0010': { kind: 'Dynamic', template: '', whole: true, legs: 1 }, 'PKG-0021': { kind: 'Package', whole: false, legs: 2, parent: 'STR-0010' } };
const dealHref = id => '../structure-page/?deal=' + id;
const TABS = [['details', 'Details'], ['instrument', 'Instrument'], ['identifiers', 'Identifiers and roles'], ['links', 'Links'], ['history', 'History']];

/* What the server holds, and what the screen has been told about. A save does
 * not reload the page: it makes Refresh gold, and the person presses it. */
// Per trade: card id, then field key, then the value written. A card of rows holds its rows under "rows".
const OV = {};
// Per trade: the history entries written this session.
const EXTRA = {};
// The writes the screen has not read back yet.
const PENDING = [];
let stale = STATE_P === 'stale';
let flashId = null;
/* The list is its address: the page, the page size, the search, the filters and the order. */
const num = (key, dflt) => { const n = Number(P.get(key)); return P.get(key) && Number.isFinite(n) && n >= 0 ? n : dflt; };
let listQuery = {
    offset: num('offset', 0), limit: PAGE_SIZES.includes(num('limit', 15)) ? num('limit', 15) : 15, search: P.get('search') || '',
    status: P.get('status') || '', book: P.get('book') || '', cp: P.get('cp') || '', type: P.get('type') || '', from: P.get('from') || '', to: P.get('to') || '',
    sort: P.get('sort') || '', desc: P.get('dir') !== 'asc'
};
let selected = new Set();
const pageCrumbs = () => [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', qs({ trade: null, tab: null, workflow: null })], [CODE]];
const main = shell({
    title: CODE ? 'Trade ' + CODE : 'Trades', name: 'trade-page',
    question: 'Does one trade page serve review, amendment and lifecycle events?',
    crumbs: CODE ? pageCrumbs() : [['Home', '#'], ['Trades', '../trade-page/'], ['Trades']],
    states: CODE
        ? [['page', { state: null, workflow: null }, !STATE_P], ['stale', { state: 'stale', workflow: null }, STATE_P === 'stale'], ['conflict', { state: 'conflict', workflow: null }, STATE_P === 'conflict'], ['digest fixed', { digest: DIGEST_FIXED ? null : 'fixed' }, DIGEST_FIXED],
            ['loading', { state: 'loading' }, STATE_P === 'loading'], ['error', { state: 'error' }, STATE_P === 'error'], ['refused', { state: 'refused', tab: 'history' }, STATE_P === 'refused'], ['list', { trade: null, tab: null, workflow: null, state: null }]]
        : [['list', { state: null }, !STATE_P], ['stale', { state: 'stale' }, STATE_P === 'stale'], ['loading', { state: 'loading' }, STATE_P === 'loading'], ['empty', { state: 'empty' }, STATE_P === 'empty'], ['error', { state: 'error' }, STATE_P === 'error'],
            ['server today', { server: SERVER === 'today' ? null : 'today' }, SERVER === 'today']],
    says: CODE
        ? 'One page per trade, one tab per aspect, an Edit button on each card, and history as a timeline. A tab switches in place, so a save is not lost. Edit asks for the kind of change first and offers only the fields that kind may touch. A save does not reload the page: Refresh turns gold and the person presses it. Recording a lifecycle event is a workflow with its own address. Try Edit on the instrument card as a trader, then Workflows as operations.'
        : 'The list is the request: the pager, the filters, the order and the request line under the table are one thing, and all of them are in the address. The Workflows menu carries the doors to ORE import and export and to structures. Book a trade is the Add of the list.'
});

function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); el.classList.add('dyn'); main.append(el); }
const wrap = (...kids) => h('div', { class: 'dyn' }, kids);

/* ---------- the server, as the list sees it ---------- */
const SORTS = { code: t => t.code, type: t => tradeType(t.type).description, cp: t => cpty(t.cp).name, book: t => book(t.book).name, status: t => t.state, date: t => t.date, notional: t => t.notional, ver: t => t.ver };
const SORT_FIELDS = { code: 'code', type: 'trade_type_code', cp: 'counterparty_id', book: 'book_id', status: 'state', date: 'trade_date', notional: 'notional', ver: 'version' };
function listTrades(q) {
    let rows = TRADES.filter(t => (!q.status || t.state === q.status) && (!q.book || t.book === q.book) && (!q.cp || t.cp === q.cp) && (!q.type || t.type === q.type) && (!q.from || t.date >= q.from) && (!q.to || t.date <= q.to));
    if (q.search) {
        const s = q.search.toLowerCase();
        rows = SERVER === 'today'
            ? rows.filter(t => t.id.startsWith(s) || t.code.toLowerCase() === s)
            : rows.filter(t => (t.code + ' ' + t.type + ' ' + tradeType(t.type).description + ' ' + cpty(t.cp).name).toLowerCase().includes(s));
    }
    const key = SORTS[q.sort] || (t => t.updated), dir = q.sort && !q.desc ? 1 : -1;
    rows = rows.slice().sort((a, b) => { const x = key(a), y = key(b); return dir * (typeof x === 'number' ? x - y : String(x).localeCompare(String(y))); });
    return { rows: rows.slice(q.offset, q.offset + q.limit), total: rows.length };
}
/* The request as the standard entity protocol states it: a page, a stated order, and a
 * filter record with equals members, one-of members, range members and one search. The
 * state, the book and the identifiers are columns of other entities, so the list reads
 * the trade summary read model. Today the server filters by trade id only. */
function requestLine(q, total) {
    const f = {};
    if (q.search) f[SERVER === 'today' ? 'id_one_of' : 'search'] = q.search;
    if (SERVER !== 'today') {
        if (q.status) f.state = q.status;
        if (q.cp) f.counterparty_id = q.cp;
        if (q.type) f.trade_type_code = q.type;
        if (q.book) f.book_id_one_of = [q.book];
        if (q.from) f.trade_date_from = q.from;
        if (q.to) f.trade_date_to = q.to;
    }
    const subject = SERVER === 'today' ? 'trading.v1.trades.list' : 'trading.v1.trade_summaries.list';
    const order = q.sort ? { field: SORT_FIELDS[q.sort], descending: q.desc } : { field: 'updated_at', descending: true };
    return subject + '  offset=' + q.offset + ' limit=' + q.limit + ' order=' + JSON.stringify(order) + ' filter=' + JSON.stringify(f) + '  →  total=' + total;
}
const stateChip = s => chip(s, { live: 'ok', draft: 'warn', expired: '', cancelled: 'bad' }[s]);
const tradeHref = (code, tab) => qs({ trade: code, tab: tab || null, state: null, workflow: null });
function syncUrl() {
    const q = listQuery;
    history.replaceState(null, '', qs({ offset: q.offset || null, limit: q.limit === 15 ? null : q.limit, search: q.search, status: q.status, book: q.book, cp: q.cp, type: q.type, from: q.from, to: q.to, sort: q.sort, dir: q.sort ? (q.desc ? 'desc' : 'asc') : null }));
}
function sortBy(key) {
    const q = listQuery;
    if (q.sort !== key) { q.sort = key; q.desc = false; } else if (!q.desc) q.desc = true; else { q.sort = ''; q.desc = true; }
    q.offset = 0;
    renderList();
}

/* ---------- the list ---------- */
function renderList() {
    const q = listQuery;
    syncUrl();
    const head = pageHead({
        title: 'Trades', lead: 'The trades of ' + PARTY + ' and the books it holds.',
        actions: [refreshButton({ stale, onClick: refresh }), CAN.book ? h('a', { class: 'btn primary', href: '../book-a-trade/' }, '+ Book a trade') : null, workflowMenu([
            { title: 'Import a portfolio from ORE', line: 'Bring an ORE portfolio file into a book.', href: '../ore-portfolio/?mode=import', allowed: !!CAN.import },
            { title: 'Export a portfolio to ORE', line: 'Write trades out as the portfolio the engine reads.', href: '../ore-portfolio/?mode=export' + (selected.size ? '&trades=' + [...selected].join(',') : ''), allowed: !!CAN.export },
            { title: 'Group trades', line: selected.size >= 2 ? 'Hold the ' + selected.size + ' chosen trades as one deal, or as a package that confirms trade by trade.' : 'Choose two or more trades first.', run: () => selected.size >= 2 ? location.href = '../structure-page/?mode=group&trades=' + [...selected].join(',') : toast('Choose two or more trades in the list first.'), allowed: !!CAN.structure }
        ])]
    });
    /* A combo box offers only the values that exist in the data. */
    const have = key => [...new Set(TRADES.map(x => x[key]))];
    const dateBox = (key, label) => h('input', { type: 'date', class: 'field-input', 'aria-label': label, title: label, value: q[key], onchange: e => { q[key] = e.target.value; q.offset = 0; renderList(); } });
    const filters = SERVER === 'today'
        ? h('div', { class: 'filters' }, searchInput('Trade id', 'Trade id'),
            h('span', { class: 'faint small' }, 'Today the server filters by trade id only.'))
        : h('div', { class: 'filters' },
            searchInput('Search by trade id, type or counterparty', 'Search'),
            select([['', 'All counterparties']].concat(COUNTERPARTIES.filter(c => have('cp').includes(c.code)).map(c => [c.code, c.name])), q.cp, v => { q.cp = v; q.offset = 0; renderList(); }),
            select([['', 'All trade types']].concat(have('type').map(c => [c, tradeType(c).description]).sort((x, y) => x[1].localeCompare(y[1]))), q.type, v => { q.type = v; q.offset = 0; renderList(); }),
            select([['', 'All books']].concat(BOOKS.filter(b => have('book').includes(b.code)).map(b => [b.code, b.name])), q.book, v => { q.book = v; q.offset = 0; renderList(); }),
            select([['', 'All states']].concat(['live', 'draft', 'expired', 'cancelled'].filter(s => have('state').includes(s)).map(s => [s, s.charAt(0).toUpperCase() + s.slice(1)])), q.status, v => { q.status = v; q.offset = 0; renderList(); }),
            h('span', { class: 'faint small' }, 'Trade date'), dateBox('from', 'Trade date from'), h('span', { class: 'faint small' }, 'to'), dateBox('to', 'Trade date to'));
    if (STATE_P === 'loading') return put(wrap(head, filters, h('div', { class: 'notice' }, 'Loading…')));
    if (STATE_P === 'error') return put(wrap(head, h('div', { class: 'notice bad' }, h('b', null, 'The trades could not be read. '), 'trading.v1.trade_summaries.list answered 503 (request 4f1c9a02). ', h('button', { class: 'btn small', onclick: () => location.href = qs({ state: null }) }, 'Retry'))));
    const { rows, total } = STATE_P === 'empty' ? { rows: [], total: 0 } : listTrades(q);
    const sel = CAN.structure || CAN.export;
    const filtered = !!(q.search || q.status || q.book || q.cp || q.type || q.from || q.to);
    /* The change the stale list was told about names one trade. The row pulses when the page holds it, and a strip says so when it does not. */
    const newsCode = 'ACME-100202';
    const newsOnPage = rows.some(t => t.code === newsCode);
    const sortTh = (key, label, cls) => h('th', { class: cls || '', 'aria-sort': q.sort === key ? (q.desc ? 'descending' : 'ascending') : 'none' },
        h('button', { class: 'sort', onclick: () => sortBy(key) }, label + (q.sort === key ? (q.desc ? ' ▾' : ' ▴') : '')));
    const table = h('div', { class: 'tablewrap' }, h('table', { class: 'grid' },
        h('thead', null, h('tr', null, sel ? h('th', { class: 'sel' }) : null, sortTh('code', 'Trade'), sortTh('type', 'Type'), sortTh('cp', 'Counterparty'), sortTh('book', 'Book'), sortTh('status', 'State'), sortTh('date', 'Trade date'), sortTh('notional', 'Notional', 'num'), sortTh('ver', 'Version', 'num'))),
        h('tbody', null, rows.length ? rows.map(t => h('tr', { class: stale && t.code === newsCode ? 'row-news' : '' },
            sel ? h('td', { class: 'sel' }, h('input', { type: 'checkbox', checked: selected.has(t.code), 'aria-label': 'Choose ' + t.code, onchange: e => { e.target.checked ? selected.add(t.code) : selected.delete(t.code); renderList(); } })) : null,
            h('td', { class: 'nw' }, h('a', { href: tradeHref(t.code) }, t.code)), h('td', null, tradeType(t.type).description),
            h('td', null, ref('counterparty', t.cp, cpty(t.cp).name)), h('td', null, ref('book', t.book, book(t.book).name)),
            h('td', { class: 'nw' }, stateChip(t.state)), h('td', { class: 'nw' }, t.date), h('td', { class: 'num nw' }, moneyRef(t.notional, t.ccy)),
            h('td', { class: 'num mono nw' }, 'v' + t.ver + ' · ext ' + t.ext))) :
            h('tr', null, h('td', { colspan: sel ? 9 : 8 }, filtered
                ? ['No trade matches. ', h('button', { class: 'btn small', onclick: () => { Object.assign(listQuery, { offset: 0, search: '', status: '', book: '', cp: '', type: '', from: '', to: '' }); renderList(); } }, 'Clear filters')]
                : ['There are no trades yet. ', CAN.book ? h('a', { href: '../book-a-trade/' }, 'Book a trade') : null])))));
    put(wrap(head, selected.size ? h('div', { class: 'notice acc' }, selected.size + ' chosen. ', h('button', { class: 'btn small', onclick: () => { selected.clear(); renderList(); } }, 'Clear')) : null,
        stale && !newsOnPage ? h('div', { class: 'strip' }, 'Something changed that this page cannot show. Press Refresh to read it again.') : null,
        filters, table, pager({ offset: q.offset, limit: q.limit, total, onChange: v => { Object.assign(q, v); renderList(); } }),
        h('div', { class: 'reqline' }, requestLine(q, total))));
}
function debounce(fn) { let t; return e => { clearTimeout(t); t = setTimeout(() => fn(e), 300); }; }

/* A search re-draws the list, and the box is part of it. Put the caret back where it was. */
function searchInput(placeholder, label) {
    const el = h('input', {
        placeholder, value: listQuery.search, 'aria-label': label, id: 'trade-search',
        oninput: debounce(e => {
            const at = e.target.selectionStart;
            listQuery.search = e.target.value;
            listQuery.offset = 0;
            renderList();
            const again = document.getElementById('trade-search');
            if (again) { again.focus(); again.setSelectionRange(at, at); }
        })
    });
    return el;
}

/* ---------- applying writes after Refresh ---------- */
function refresh() {
    LOADED = NOW; stale = false;
    for (const w of PENDING.splice(0)) w();
    if (CODE) renderPage(); else renderList();
}

/* ---------- the page ---------- */
function applyOverrides(t, cards) {
    const ov = OV[t.code] || {};
    cards.forEach(c => {
        if (c.fields) c.fields.forEach(fd => { if (ov[c.id] && fd.key in ov[c.id]) fd.value = ov[c.id][fd.key]; });
        else if (ov[c.id] && ov[c.id].rows) c.rows = ov[c.id].rows;
    });
    return cards;
}
/* A tab, or a workflow, opens in place and puts its address in the history, so what the person did on the page stays. */
function go(change, render) {
    history.pushState(null, '', qs(change));
    render();
}
function renderPage() {
    const t = trade(CODE);
    if (!t) return put(wrap(h('div', { class: 'notice bad' }, 'No trade ' + CODE + '. It may have been removed, or you may not see its party.')));
    if (WORKFLOW === 'lifecycle') return renderLifecycle(t);
    setCrumbs(pageCrumbs(), 'Trade ' + CODE);
    if (STATE_P === 'loading') return put(wrap(h('div', { class: 'notice' }, 'Loading…')));
    if (STATE_P === 'error') return put(wrap(h('div', { class: 'notice bad' }, h('b', null, 'The trade could not be read. '), 'trading.v1.trades.get answered 503 (request 7b20e5d1). ', h('button', { class: 'btn small', onclick: () => location.href = qs({ state: null }) }, 'Retry'))));
    const head = pageHead({
        title: t.code, id: t.id,
        meta: [stateChip(t.state), chip('v' + t.ver, 'v', 'Internal version: moves on every recorded change'), chip('external v' + t.ext, 'v', 'External version: moves only when the agreed terms change')],
        actions: [refreshButton({ stale, onClick: refresh }),
            CAN.book ? h('a', { class: 'btn', href: '../book-a-trade/?like=' + t.code }, 'Book a similar trade') : null,
            workflowMenu([
                { title: 'Record a lifecycle event', line: 'Exercise, terminate, cancel, novate or move book.', run: () => go({ workflow: 'lifecycle' }, () => { WORKFLOW = 'lifecycle'; renderPage(); }), allowed: !!CAN.event },
                { title: 'Group with other trades', line: 'Hold this trade with others as one deal, or as a package.', href: '../structure-page/?mode=group&trades=' + t.code, allowed: !!CAN.structure && !t.structure },
                { title: 'Export to ORE', line: 'Write this trade out as an ORE portfolio.', href: '../ore-portfolio/?mode=export&trades=' + t.code, allowed: !!CAN.export }
            ])]
    });
    const cur = TABS.some(x => x[0] === TAB) ? TAB : 'details';
    const content = cur === 'instrument' ? instrumentTab(t) : cur === 'identifiers' ? identifiersTab(t) : cur === 'links' ? linksTab(t) : cur === 'history' ? historyTab(t) : detailsTab(t);
    const hrefFor = id => tradeHref(t.code, id === 'details' ? null : id);
    put(wrap(head, tabs(TABS, cur, hrefFor, id => go({ trade: t.code, tab: id === 'details' ? null : id, workflow: null }, () => { TAB = id; renderPage(); })), content));
}
window.addEventListener('popstate', () => {
    if (!CODE) return;
    const p = new URLSearchParams(location.search);
    TAB = p.get('tab') || 'details';
    WORKFLOW = p.get('workflow') || '';
    renderPage();
});
const canEdit = t => CAN.amend && (t.state === 'live' || t.state === 'draft');
function detailsTab(t) {
    /* A fixed field says why, and a reference is drawn as a link. Book and State move by a workflow, never by Edit. */
    const f = (label, value, why, node) => ({ label, value, fixedWhy: why, fixed: true, node, type: 'text' });
    const fixed = 'Fixed for the life of the trade. A change is a cancel and rebook.';
    const identity = { id: 'identity', title: 'Identity and classification', table: 'trade', version: 1, fields: [
        f('Trade type', tradeType(t.type).description + ' (' + t.type + ')'), f('Party', PARTY, fixed, ref('party', PARTY)), f('Counterparty', cpty(t.cp).name, fixed, ref('counterparty', t.cp, cpty(t.cp).name)),
        f('Booking nature', t.nature), f('Entry channel', t.channel), f('Counterparty scope', t.scope)] };
    const booking = { id: 'booking', title: 'Booking', table: 'trade_booking', version: t.ver, fields: [
        { key: 'trade_date', label: 'Trade date', value: t.date, type: 'date' }, { key: 'execution_time', label: 'Execution time', value: t.date + ' 10:42', type: 'text' },
        { key: 'netting_set', label: 'Netting set', value: 'NWCAP-CSA-1', type: 'text', ref: 'netting_set' },
        f('Book', book(t.book).name, 'Changed by a book move, which is a cancel and rebook.', ref('book', t.book, book(t.book).name)),
        f('State', t.state, 'Changed by recording a lifecycle event.', stateChip(t.state))]
        .concat(t.structure ? [f('Structure or package', t.structure, null, h('a', { href: dealHref(t.structure) }, t.structure))] : []) };
    return h('div', { class: 'cards' }, card(identity, { readOnlyNote: true }), card(applyOverrides(t, [booking])[0], { edit: canEdit(t) ? c => openAmend(t, c) : null, readOnlyNote: !canEdit(t) }));
}
function instrumentTab(t) {
    if (!t.table) return h('div', { class: 'notice warn' }, h('b', null, tradeType(t.type).code + ' has no instrument table yet. '), 'The trade type is in the catalogue, but nothing routes it to a table, so its terms cannot be held.');
    return h('div', { class: 'cards' }, applyOverrides(t, instrumentCards(t)).map(c => card(c, { edit: canEdit(t) ? c2 => openAmend(t, c2) : null, readOnlyNote: !canEdit(t) })));
}
function identifiersTab(t) {
    const o = otherCards(t);
    return h('div', { class: 'cards' }, applyOverrides(t, ['identifiers', 'roles', 'portfolios', 'additional'].map(k => o[k])).map(c => card(c, { edit: canEdit(t) ? c2 => openAmend(t, c2) : null, readOnlyNote: !canEdit(t) })));
}
function linksTab(t) {
    const cards = [];
    cards.push(t.structure
        ? h('section', { class: 'card' }, h('header', null, h('h2', null, STRUCT_KIND[t.structure].kind === 'Package' ? 'Package' : 'Structure'), h('span', { class: 'sp' }), h('span', { class: 'access' }, 'Read only')), h('div', { class: 'body' }, h('a', { href: dealHref(t.structure) }, t.structure), ' — ' + STRUCT_KIND[t.structure].kind + (STRUCT_KIND[t.structure].template ? ' (' + STRUCT_KIND[t.structure].template + ')' : '') + (STRUCT_KIND[t.structure].parent ? ', inside ' + STRUCT_KIND[t.structure].parent : '') + ', ' + STRUCT_KIND[t.structure].legs + ' legs. ' + (STRUCT_KIND[t.structure].whole ? 'Confirmed and priced as one deal.' : 'Each leg confirms on its own. A package has no price of its own.')))
        : h('section', { class: 'card' }, h('header', null, h('h2', null, 'Structure or package'), h('span', { class: 'sp' }), h('span', { class: 'access' }, 'Read only')), h('div', { class: 'body' }, 'Not part of a structure or a package. ', CAN.structure ? h('a', { href: '../structure-page/?mode=group&trades=' + t.code }, 'Group with other trades') : null)));
    const links = [];
    if (t.rebookedTo) links.push([t.rebookKind || 'Novation', 'transferred → new trade', t.rebookedTo]);
    if (t.rebookedFrom) links.push([(trade(t.rebookedFrom) || {}).rebookKind || 'Novation', 'new trade ← transferred', t.rebookedFrom]);
    cards.push({ id: 'links', title: 'Linked trades', table: 'trade_link', version: 1, child: true, columns: ['Link type', 'Direction', 'Trade'], rows: links.map(l => [l[0], l[1], h('a', { href: tradeHref(l[2]) }, l[2])]) });
    return h('div', { class: 'cards' }, cards.map(c => c.nodeType ? c : card(c, { readOnlyNote: true })));
}
function historyTab(t) {
    /* A refused read is stated on its own tab, in words, and the other tabs carry on. */
    if (STATE_P === 'refused') return h('div', { class: 'notice' }, h('b', null, 'You may not read the history of this trade. '), 'Ask the tenant administrator for a role that reads trade history.');
    const entries = historyOf(t).concat(EXTRA[t.code] || []).sort((a, b) => a.at.localeCompare(b.at));
    /* The newest version of each record carries the Current version emblem. */
    const newest = {};
    entries.forEach(e => { e.current = false; if (!e.noVersion) newest[e.record || 'Trade'] = e; });
    Object.values(newest).forEach(e => { e.current = true; });
    return timeline(entries, { isService: ROLE !== 'reader', flash: flashId });
}

/* ---------- Edit: the kind of change comes first ---------- */
function allowedActivities(c) {
    if (c.id === 'instrument' || c.digestGap || c.economic) return ['amendment_economic', 'misbooking_correction', 'amendment_reference', 'null_amend'];
    return ['amendment_reference', 'misbooking_correction', 'null_amend'];
}
function openAmend(t, c) {
    const loadedAt = LOADED;
    const rowsMode = !c.fields;
    const cur = rowsMode ? c.rows.map(r => r.map(plain)) : c.fields.map(fd => fd.value);
    let draft = JSON.parse(JSON.stringify(cur));
    let act = allowedActivities(c)[0], reason = 'trade.client_request', note = '';
    const body = h('div', { style: 'display:grid;gap:.8rem' });
    const saveBtn = h('button', { class: 'btn primary' }, 'Save');
    const changedFields = () => rowsMode ? (JSON.stringify(draft) !== JSON.stringify(cur) ? [['rows', '', 'changed']] : [])
        : c.fields.map((fd, i) => String(draft[i]) !== String(cur[i]) ? [fd, cur[i], draft[i]] : null).filter(Boolean);
    function draw() {
        const a = activityType(act), diff = changedFields();
        const econChange = diff.some(([fd]) => fd.economic || (rowsMode && c.economic)) && a.real;
        const folded = econChange && !c.digestGap;
        const allowed = fd => !fd.fixed && (act === 'null_amend' ? false : act === 'misbooking_correction' ? true : a.economic ? !!fd.economic : !fd.economic);
        const rowsOff = act === 'null_amend' || (!a.economic && c.economic && act !== 'misbooking_correction');
        const reasons = REASON_CODES.filter(r => diff.length ? r[0] !== 'trade.no_change' : r[0] === 'trade.no_change');
        if (!reasons.find(r => r[0] === reason)) reason = reasons[0][0];
        /* A value that exists is chosen from a combo box. A fixed value is shown, never edited. An optional value has a Clear control. */
        const inputFor = (fd, i) => {
            if (fd.fixed) return h('div', { class: 'static' }, ivalue(fd.value));
            const off = !allowed(fd), set = v => { draft[i] = v; draw2(); };
            let el;
            if (fd.type === 'ccy' || fd.options) {
                const list = fd.type === 'ccy' ? CCYS : fd.options;
                el = select(list.includes(draft[i]) ? list : [draft[i]].concat(list), draft[i], set);
            } else el = h('input', { class: 'field-input', type: fd.type === 'date' ? 'date' : 'text', value: draft[i], oninput: e => set(e.target.value) });
            el.disabled = off;
            return fd.optional ? h('div', { class: 'withclear' }, el, h('button', { class: 'btn small', disabled: off || draft[i] === '', onclick: () => { draft[i] = ''; draw(); } }, 'Clear')) : el;
        };
        fill(body,
            STATE_P === 'conflict' ? h('div', { class: 'notice warn' }, h('b', null, 'Priya Natarajan saved v' + (t.ver + 1) + ' while you were editing. '), 'fixed_rate moved from 2.8500 % to 2.9000 %. ',
                h('button', { class: 'btn small', onclick: () => toast('Your edit now sits on top of v' + (t.ver + 1) + '.') }, 'Keep my edit'), ' ', h('button', { class: 'btn small', onclick: () => toast('The form now holds the new values.') }, 'Take the new values')) : null,
            field('What kind of change is this?', select(allowedActivities(c).map(code => [code, activityType(code).label]), act, v => { act = v; if (v === 'null_amend') draft = JSON.parse(JSON.stringify(cur)); draw(); }),
                'The kind of change decides which fields you may edit.'),
            h('div', { class: 'filters', style: 'margin:0' }, a.economic ? chip('economic', 'acc') : chip('not economic'), a.confirmable ? chip('confirmable', '', 'A new confirmation is issued') : chip('not confirmable'), a.real ? chip('real') : chip('null', 'warn', 'Recorded, but nothing is versioned')),
            rowsMode
                ? h('div', null,
                    h('table', { class: 'grid' }, h('thead', null, h('tr', null, c.columns.map(x => h('th', null, x)), h('th'))),
                        h('tbody', null, draft.map((r, ri) => h('tr', null, r.map((v, ci) =>
                            h('td', null, h('input', { class: 'field-input', value: v, disabled: rowsOff, oninput: e => { draft[ri][ci] = e.target.value; draw2(); } }))),
                            h('td', null, h('button', { class: 'btn small', disabled: rowsOff, onclick: () => { draft.splice(ri, 1); draw(); } }, 'Remove')))))),
                    h('div', { class: 'rowtools' }, h('button', { class: 'btn small', disabled: rowsOff, onclick: () => { draft.push(c.columns.map(() => '')); draw(); } }, 'Add a row')))
                : h('div', { class: 'grid2' }, c.fields.map((fd, i) => field(fd.label + (fd.economic ? ' · economic' : ''), inputFor(fd, i),
                    fd.fixed ? fd.fixedWhy : !allowed(fd) && act !== 'null_amend' ? 'Not part of this kind of change.' : ''))),
            h('div', { class: 'grid2' }, field('Reason', select(reasons, reason, v => { reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { note = e.target.value; } }, note))),
            effect(t, a, diff, econChange, folded, c));
        saveBtn.disabled = a.real ? diff.length === 0 : diff.length !== 0;
    }
    function draw2() { const a = activityType(act), diff = changedFields(); const econChange = diff.some(([fd]) => fd.economic || (rowsMode && c.economic)) && a.real; const old = body.querySelector('[data-effect]'); old.replaceWith(effect(t, a, diff, econChange, econChange && !c.digestGap, c)); saveBtn.disabled = a.real ? diff.length === 0 : diff.length !== 0; }
    const d = openDialog({
        title: 'Edit ' + c.title.toLowerCase(), loadedAt, body,
        buttons: [h('button', { class: 'btn', onclick: () => { if (changedFields().length && !confirm('You have unsaved changes. Close anyway?')) return; d.close(); } }, 'Cancel'), saveBtn]
    });
    saveBtn.addEventListener('click', () => {
        const a = activityType(act), diff = changedFields(), econChange = diff.some(([fd]) => fd.economic || (rowsMode && c.economic)) && a.real, folded = econChange && !c.digestGap;
        d.close();
        PENDING.push(() => {
            const ov = OV[t.code] = OV[t.code] || {};
            ov[c.id] = ov[c.id] || {};
            if (rowsMode) ov[c.id].rows = draft;
            else c.fields.forEach((fd, i) => { if (fd.key) ov[c.id][fd.key] = draft[i]; });
            if (a.real) { t.ver += 1; if (folded) t.ext += 1; }
            const e = { id: 'x' + Date.now(), at: iso(NOW), kind: 'amended', activity: act, record: c.title, version: t.ver, ext: t.ext, actor: ME, reason, note,
                economic: folded, diffs: diff.map(([fd, o, v]) => [fd.label || 'rows', String(o), String(v)]), noVersion: !a.real };
            (EXTRA[t.code] = EXTRA[t.code] || []).push(e); flashId = e.id;
        });
        stale = true; renderPage();
        toast(a.real ? 'Saved. Refresh shows the new version.' : 'Recorded for the audit trail. Nothing was versioned.');
    });
    draw();
}
function effect(t, a, diff, econChange, folded, c) {
    const lines = [];
    if (!a.real) lines.push(diff.length ? 'A null amend must change nothing. Put the values back to save it.' : 'Recorded for the audit trail. Neither version moves and no confirmation is raised.');
    else if (!diff.length) lines.push('Change a field to continue.');
    else {
        lines.push('Internal version ' + t.ver + ' → ' + (t.ver + 1) + '.');
        lines.push(folded ? ['External version ', h('b', null, t.ext + ' → ' + (t.ext + 1)), ': an economic term changed, so the customer agreed a change.']
            : econChange && c.digestGap ? ['External version stays ' + t.ext + '. ', h('b', null, 'The digest does not read this table yet, so the customer agreed a change that no version records.')]
                : 'External version stays ' + t.ext + ': no economic term changed.');
        if (a.confirmable) lines.push('A new confirmation is issued to ' + cpty(t.cp).name + '.');
        if (t.structure && econChange && STRUCT_KIND[t.structure]) lines.push(STRUCT_KIND[t.structure].whole ? 'This trade is a leg of ' + t.structure + ', which confirms as one document. The deal\'s external version moves, and the customer receives one confirmation for the deal.' : 'This trade is a leg of the package ' + t.structure + '. The package confirms trade by trade, so only this trade\'s confirmation changes.');
        if (t.structure && econChange && STRUCT_KIND[t.structure] && STRUCT_KIND[t.structure].parent && !STRUCT_KIND[STRUCT_KIND[t.structure].parent].parent) lines.push('The ' + STRUCT_KIND[t.structure].kind.toLowerCase() + ' is inside ' + STRUCT_KIND[t.structure].parent + ', which confirms as one document. Whether this trade still confirms on its own is not decided: see the capture.');
    }
    return h('div', { class: 'notice ' + (econChange && c.digestGap ? 'warn' : 'acc'), 'data-effect': true }, h('b', null, 'What this will do'), lines.map(l => h('div', { class: 'econ-line' + (folded ? ' moves' : '') }, l)),
        h('div', { class: 'mono small faint' }, 'trading.v1.' + c.table + 's.put  precondition: version ' + t.ver + '  change intent: reason, commentary'));
}

/* ---------- the lifecycle workflow: a page of its own, with its own address and trail ---------- */
function lifecycleEvents(t) {
    const hasOptions = /Option|Swaption|CapFloor/.test(t.type);
    if (t.state === 'draft') return [['execute', 'Execute', 'The agreement is struck. The draft goes live.', 'new_booking'], ['discard', 'Discard', 'The draft comes to nothing.', 'cancellation']];
    if (t.state === 'expired' || t.state === 'cancelled') return [['reinstate', 'Reinstate', 'The ' + (t.state === 'expired' ? 'expiry' : 'cancellation') + ' was recorded in error.', 'reinstatement']];
    return [hasOptions ? ['exercise', 'Exercise', 'The holder exercises the option.', 'exercise'] : null, ['partial', 'Partial termination', 'Part of the notional is closed out.', 'partial_termination'],
        ['expire', 'Expiry', 'The contract ceases naturally.', 'expiry'], ['cancel', 'Cancel', 'Remove the trade from the book. The record stays.', 'cancellation'],
        ['novate', 'Novation', 'The counterparty changes. Cancel and rebook, joined by a link.', 'novation'], ['bookmove', 'Book move', 'Move to a book of another legal entity. Cancel and rebook.', 'novation']].filter(Boolean);
}
function closeWorkflow() {
    go({ workflow: null }, () => { WORKFLOW = ''; renderPage(); });
}
function renderLifecycle(t) {
    setCrumbs([['Home', '#'], ['Trades', '../trade-page/'], ['Record a lifecycle event']], 'Record a lifecycle event');
    if (!CAN.event) return put(wrap(h('div', { class: 'notice' }, h('b', null, 'You may not record lifecycle events. '), 'Ask the tenant administrator for a role that does.')));
    const evs = lifecycleEvents(t);
    let step = 0, ev = evs[0], when_ = '2026-10-11', amount = '', newCp = COUNTERPARTIES.find(c => c.open && c.code !== t.cp).code, newBook = 'EMEA-RATES', reason = 'trade.client_request', note = '', done = null;
    const body = h('div');
    const STEPS = ['Event', 'Facts', 'Effect', 'Record'];
    const next = h('button', { class: 'btn primary' }, 'Next');
    const cancel = h('button', { class: 'btn', onclick: closeWorkflow }, 'Cancel');
    function draw() {
        const a = activityType(ev[3]);
        const rail = h('div', { class: 'steps' }, STEPS.map((s, i) => h('span', { class: 'step' + (i === step ? ' on' : i < step ? ' done' : '') }, (i + 1) + ' ' + s)));
        let pane;
        if (done) pane = h('div', { class: 'notice ok' }, h('b', null, ev[1] + ' recorded. '), 'Refresh the page to read it. ', done.map(x => x));
        else if (step === 0) pane = h('div', { class: 'picker' }, evs.map(e => h('button', { class: 'opt' + (e === ev ? ' on' : ''), onclick: () => { ev = e; draw(); } }, e[1], h('small', null, e[2]))));
        else if (step === 1) pane = h('div', { style: 'display:grid;gap:.8rem' }, h('div', { class: 'grid2' }, field('Event date', h('input', { class: 'field-input', type: 'date', value: when_, oninput: e => { when_ = e.target.value; } })),
            ev[0] === 'exercise' || ev[0] === 'partial' ? field(ev[0] === 'exercise' ? 'Exercised amount' : 'Terminated amount', h('input', { class: 'field-input', value: amount, placeholder: money(t.notional, t.ccy), oninput: e => { amount = e.target.value; } })) : null,
            ev[0] === 'novate' ? field('New counterparty', select(COUNTERPARTIES.filter(c => c.open && c.code !== t.cp).map(c => [c.code, c.name]), newCp, v => { newCp = v; })) : null,
            ev[0] === 'bookmove' ? field('Destination book', select(BOOKS.filter(b => !b.virtual && b.code !== t.book).map(b => [b.code, b.name]), newBook, v => { newBook = v; })) : null),
            h('div', { class: 'grid2' }, field('Reason', select(REASON_CODES.filter(r => r[0] !== 'trade.no_change'), reason, v => { reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { note = e.target.value; } }, note))));
        else if (step === 2) pane = h('div', { class: 'notice acc' }, h('b', null, 'What this will do'),
            h('div', null, 'Activity ' + a.label + ' (' + a.category.replace('_', ' ') + ').'),
            ['novate', 'bookmove'].includes(ev[0]) ? [h('div', null, 'Cancels ' + t.code + '.'), h('div', null, 'Books a new trade with a fresh id ' + (ev[0] === 'novate' ? 'against ' + cpty(newCp).name : 'in ' + book(newBook).name) + '.'), h('div', null, 'Joins the two with a ' + (ev[0] === 'novate' ? 'Novation' : 'Book move') + ' link. One transaction.')]
                : h('div', null, t.code + ' moves to ' + ({ execute: 'live', discard: 'cancelled', reinstate: 'live', cancel: 'cancelled', expire: 'expired' }[ev[0]] || 'live (terms reduced)') + '.'),
            a.confirmable ? h('div', null, 'A new confirmation is issued to ' + cpty(t.cp).name + '.') : null, h('div', { class: 'faint small' }, 'A version is never deleted. Undo it with the mirror event.'));
        else pane = h('div', { class: 'notice' }, 'Press Record to write the activity and the versions it causes.');
        fill(body, rail, pane);
        next.textContent = done ? 'Close' : step === 3 ? 'Record' : 'Next';
        cancel.hidden = !!done;
    }
    next.addEventListener('click', () => {
        if (done) return closeWorkflow();
        if (step < 3) { step++; return draw(); }
        const slot = TRADES.length + PENDING.length;
        const newCode = 'ACME-100' + (300 + slot);
        PENDING.push(() => {
            const a = activityType(ev[3]);
            t.state = { execute: 'live', discard: 'cancelled', reinstate: 'live', cancel: 'cancelled', expire: 'expired', novate: 'cancelled', bookmove: 'cancelled' }[ev[0]] || t.state;
            if (a.real) t.ver += 1;
            if (a.economic) t.ext += 1;
            const e = { id: 'x' + Date.now(), at: iso(NOW), kind: t.state === 'cancelled' ? 'cancelled' : 'event', activity: ev[3], version: t.ver, ext: t.ext, actor: ME, reason, note: note || ev[2], economic: a.economic, diffs: [['state', 'live', t.state]] };
            (EXTRA[t.code] = EXTRA[t.code] || []).push(e); flashId = e.id;
            if (['novate', 'bookmove'].includes(ev[0])) {
                const n = Object.assign({}, t, { id: tradeId(300 + slot), code: newCode, cp: ev[0] === 'novate' ? newCp : t.cp, book: ev[0] === 'bookmove' ? newBook : t.book,
                    state: 'live', ext: 1, ver: 1, structure: null, rebookedTo: null, rebookedFrom: t.code, updated: iso(NOW) });
                t.rebookedTo = n.code; t.rebookKind = ev[0] === 'novate' ? 'Novation' : 'Book move'; n.rebookKind = t.rebookKind;
                TRADES.push(n); store('booked', (store('booked') || []).concat([n]));
            }
        });
        stale = true; done = ['novate', 'bookmove'].includes(ev[0]) ? ['The new trade is ' + newCode + '.'] : [];
        draw();
    });
    put(wrap(pageHead({ title: 'Record a lifecycle event', lead: 'For trade ' + t.code + ', ' + tradeType(t.type).description + '.' }), body, h('div', { class: 'filters' }, cancel, next)));
    draw();
}

if (CODE) renderPage(); else renderList();

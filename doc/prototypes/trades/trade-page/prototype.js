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
    sort: P.get('sort') || '', desc: P.get('dir') !== 'asc', show: ['trades', 'structures'].includes(P.get('show')) ? P.get('show') : '',
    portfolio: P.get('portfolio') || ''
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
/* A row of the list is a trade or a structure. A structure has no state or version of its own: its legs do. */
const dealRows = () => Object.entries(allDeals()).map(([id, d]) => ({ kind: 'structure', id, d, legs: d.members.map(m => trade(m[0])).filter(Boolean) }));
const rowCode = r => r.kind === 'trade' ? r.t.code : r.id;
const dealTypeText = d => d.kind === 'Package' ? 'Package' : d.kind + (d.template ? ' · ' + d.template.replace(/([A-Z])/g, ' $1').trim().toLowerCase().replace(/^./, c => c.toUpperCase()) : ' structure');
const dealBooks = r => [...new Set(r.legs.map(l => l.book))];
/* The role a trade plays inside the deal that holds it, or null when no deal does. A trade is a leg of one deal at a
 * time: the trade states it, and the deal's membership states the role. */
const roleOfTrade = code => {
    for (const d of Object.values(allDeals())) {
        const member = d.members.find(m => m[0] === code);
        if (member) return member[1];
    }
    return null;
};
/* A composite instrument holds constituent trades of its own, which is not the same thing as a structure: a structure
 * is a deal assembled from several trades, and an instrument belongs to one trade. The mock states the constituents the
 * trade page draws for the one composite trade of the list. */
const COMPOSITE_LEGS = { 'ACME-100205': ['ACME-100201', 'ACME-100210'] };
const compositeLegs = code => COMPOSITE_LEGS[code] || [];
const SORTS = {
    code: r => rowCode(r), type: r => r.kind === 'trade' ? tradeType(r.t.type).description : dealTypeText(r.d), cp: r => cpty(r.kind === 'trade' ? r.t.cp : r.d.cp).name,
    book: r => r.kind === 'trade' ? book(r.t.book).name : (dealBooks(r).length ? book(dealBooks(r)[0]).name : ''), status: r => r.kind === 'trade' ? r.t.state : '',
    date: r => r.kind === 'trade' ? r.t.date : r.d.created.slice(0, 10), ver: r => r.kind === 'trade' ? r.t.ver : 0
};
const SORT_FIELDS = { code: 'code', type: 'trade_type_code', cp: 'counterparty_id', book: 'book_id', status: 'state', date: 'trade_date', ver: 'version' };

/* ---------- the portfolio tree ---------- */
/* The tree holds portfolios and books. A portfolio nests under a portfolio, and a book hangs from exactly one
 * portfolio, so the tree draws both: a portfolio with the briefcase and a book with the book. The icons are the two
 * the icon guidelines name for the concept, ic_fluent_briefcase_20_regular and ic_fluent_book_20_regular, written
 * here because a prototype stands alone. A trade is filed in one book, so choosing a portfolio keeps the trades of
 * every book under it. A sandbox's portfolios are virtual. */
const ICONS = {
    portfolio: '<svg viewBox="0 0 20 20" width="16" height="16" fill="none" aria-hidden="true"><path fill="currentColor" d="M11.25 2C12.2165 2 13 2.7835 13 3.75V5H15C16.6569 5 18 6.34315 18 8V14C18 15.6569 16.6569 17 15 17H5C3.39489 17 2.08421 15.7394 2.00391 14.1543L2 14V8C2 6.34315 3.34315 5 5 5H7V3.75C7 2.7835 7.7835 2 8.75 2H11.25ZM17 11.2314C16.469 11.7077 15.7695 12 15 12H11V12.5C11 12.7761 10.7761 13 10.5 13H9.5C9.22386 13 9 12.7761 9 12.5V12H5C4.23053 12 3.53103 11.7077 3 11.2314V14L3.01074 14.2041C3.113 15.2128 3.96435 16 5 16H15C16.1046 16 17 15.1046 17 14V11.2314ZM5 6C3.89543 6 3 6.89543 3 8V9C3 10.1046 3.89543 11 5 11H9V10.5C9 10.2239 9.22386 10 9.5 10H10.5C10.7761 10 11 10.2239 11 10.5V11H15C16.1046 11 17 10.1046 17 9V8C17 6.89543 16.1046 6 15 6H5ZM8.75 3C8.33579 3 8 3.33579 8 3.75V5H12V3.75C12 3.33579 11.6642 3 11.25 3H8.75Z"/></svg>',
    book: '<svg viewBox="0 0 20 20" width="16" height="16" fill="none" aria-hidden="true"><path fill="currentColor" d="M6 5C6 4.44772 6.44772 4 7 4H13C13.5523 4 14 4.44772 14 5V6C14 6.55228 13.5523 7 13 7H7C6.44772 7 6 6.55228 6 6V5ZM7 5V6H13V5H7ZM4 4V16C4 17.1046 4.89543 18 6 18H15.5C15.7761 18 16 17.7761 16 17.5C16 17.2239 15.7761 17 15.5 17H6C5.44772 17 5 16.5523 5 16H15C15.5523 16 16 15.5523 16 15V4C16 2.89543 15.1046 2 14 2H6C4.89543 2 4 2.89543 4 4ZM14 3C14.5523 3 15 3.44772 15 4V15H5V4C5 3.44772 5.44772 3 6 3H14Z"/></svg>'
};
const treeIcon = kind => {
    const el = h('span', { class: 'tree-icon' });
    el.innerHTML = ICONS[kind];
    return el;
};
const PORTFOLIOS = [
    { id: 'ACME-LONDON', name: 'Acme London', parent: null },
    { id: 'TRADING', name: 'Trading', parent: 'ACME-LONDON' },
    { id: 'RATES', name: 'Rates', parent: 'TRADING' },
    { id: 'FX', name: 'FX flow', parent: 'TRADING' },
    { id: 'CREDIT', name: 'Credit', parent: 'TRADING' },
    { id: 'EQUITY', name: 'Equity', parent: 'TRADING' },
    { id: 'COMMODITY', name: 'Commodity', parent: 'TRADING' },
    { id: 'CLOSED-2025', name: 'Closed 2025', parent: 'ACME-LONDON' },
    { id: 'SANDBOX', name: 'Sandboxes', parent: 'ACME-LONDON', virtual: true },
    { id: 'SANDBOX-ORE', name: 'ORE samples', parent: 'SANDBOX', virtual: true },
    { id: 'SANDBOX-WHATIF', name: 'Rates what-ifs', parent: 'SANDBOX', virtual: true }
];
/* The portfolio each mock book belongs to. A book states its portfolio with parent_portfolio_id. */
const PORTFOLIO_OF_BOOK = {
    'EMEA-RATES': 'RATES', 'EMEA-RATES-2025': 'CLOSED-2025', 'EMEA-FX': 'FX', 'EMEA-CREDIT': 'CREDIT',
    'EMEA-EQ': 'EQUITY', 'EMEA-CMDTY': 'COMMODITY', 'VIRT-DRAFT': 'SANDBOX-WHATIF',
    'ORE-EX-SWAPS': 'SANDBOX-ORE', 'ORE-EX-FX': 'SANDBOX-ORE'
};
const childPortfolios = id => PORTFOLIOS.filter(p => p.parent === id);
const booksIn = id => BOOKS.filter(b => PORTFOLIO_OF_BOOK[b.code] === id);
/* A portfolio holds its own books and the books of every portfolio under it. */
function subtree(id) {
    const out = [id];
    for (const child of childPortfolios(id)) out.push(...subtree(child.id));
    return out;
}
const booksUnder = id => subtree(id).flatMap(booksIn);
const tradesInBook = code => TRADES.filter(t => t.book === code).length;
const tradesInPortfolio = id => {
    const codes = new Set(booksUnder(id).map(b => b.code));
    return TRADES.filter(t => codes.has(t.book)).length;
};
function portfolioRail(q) {
    const node = (label, kind, chosen, count, depth, opts) => h('button', {
        class: 'tree-node' + (chosen ? ' tree-chosen' : '') + (opts.virtual ? ' tree-virtual' : ''),
        style: 'padding-left:' + (0.5 + depth * 0.9).toFixed(2) + 'rem',
        'aria-current': chosen ? 'true' : null,
        title: opts.title,
        onclick: opts.pick
    }, treeIcon(kind), h('span', { class: 'tree-name' }, label), h('span', { class: 'tree-count' }, String(count)));
    const portfolioRow = (p, depth) => [
        node(p.name, 'portfolio', q.portfolio === p.id, tradesInPortfolio(p.id), depth, {
            virtual: p.virtual,
            title: p.virtual ? p.name + ' — a virtual portfolio, in a sandbox' : p.name + ' — a portfolio',
            pick: () => { q.portfolio = q.portfolio === p.id ? '' : p.id; q.book = ''; q.offset = 0; renderList(); }
        }),
        childPortfolios(p.id).map(k => portfolioRow(k, depth + 1)),
        booksIn(p.id).map(b => node(b.name, 'book', q.book === b.code, tradesInBook(b.code), depth + 1, {
            title: b.name + ' — a book in ' + p.name,
            pick: () => { q.book = q.book === b.code ? '' : b.code; q.portfolio = ''; q.offset = 0; renderList(); }
        }))
    ];
    const chosen = q.portfolio || q.book;
    return h('aside', { class: 'rail', 'aria-label': 'Portfolio tree' },
        h('div', { class: 'rail-head' }, 'Portfolio tree'),
        PORTFOLIOS.filter(p => !p.parent).map(p => portfolioRow(p, 0)),
        h('div', { class: 'rail-note faint small' }, chosen ? 'A portfolio holds the books under it. Choose it again to clear.' : 'Choose a portfolio or a book to narrow the list.'));
}
/* A structure matches a filter when one of its legs does. */
function listTrades(q) {
    /* Choosing a portfolio keeps the trades of every book under it, and choosing a book keeps its own. */
    const chosenBooks = q.book ? new Set([q.book]) : q.portfolio ? new Set(booksUnder(q.portfolio).map(b => b.code)) : null;
    const inTree = t => !chosenBooks || chosenBooks.has(t.book);
    const tradeOk = t => inTree(t) && (!q.status || t.state === q.status) && (!q.type || t.type === q.type) && (!q.from || t.date >= q.from) && (!q.to || t.date <= q.to);
    const showT = q.show !== 'structures', showS = q.show !== 'trades' && SERVER !== 'today';
    let rows = showT ? TRADES.filter(t => tradeOk(t) && (!q.cp || t.cp === q.cp)).map(t => ({ kind: 'trade', t })) : [];
    if (showS) rows = rows.concat(dealRows().filter(r => (!q.cp || r.d.cp === q.cp) && r.legs.some(tradeOk)));
    if (q.search) {
        const s = q.search.toLowerCase();
        rows = SERVER === 'today'
            ? rows.filter(r => r.t.id.startsWith(s) || r.t.code.toLowerCase() === s)
            : rows.filter(r => (r.kind === 'trade'
                ? r.t.code + ' ' + r.t.type + ' ' + tradeType(r.t.type).description + ' ' + cpty(r.t.cp).name
                : r.id + ' ' + dealTypeText(r.d) + ' ' + cpty(r.d.cp).name + ' ' + r.d.members.map(m => m[0]).join(' ')).toLowerCase().includes(s));
    }
    const key = SORTS[q.sort] || (r => r.kind === 'trade' ? r.t.updated : r.d.created), dir = q.sort && !q.desc ? 1 : -1;
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
        /* A portfolio names no column of a trade: a trade is filed in one book, so the portfolio is the books under it. */
        if (q.portfolio) f.book_id_one_of = booksUnder(q.portfolio).map(b => b.code);
        if (q.status) f.state = q.status;
        if (q.cp) f.counterparty_id = q.cp;
        if (q.type) f.trade_type_code = q.type;
        if (q.book) f.book_id_one_of = [q.book];
        if (q.from) f.trade_date_from = q.from;
        if (q.to) f.trade_date_to = q.to;
    }
    const subject = SERVER === 'today' ? 'trading.v1.trades.list' : 'trading.v1.trade_summaries.list';
    const order = q.sort ? { field: SORT_FIELDS[q.sort], descending: q.desc } : { field: 'updated_at', descending: true };
    const structures = q.show !== 'trades' && SERVER !== 'today' ? '  +  trading.v1.structures.list  (a structure matches when one of its legs does)' : '';
    return subject + '  offset=' + q.offset + ' limit=' + q.limit + ' order=' + JSON.stringify(order) + ' filter=' + JSON.stringify(f) + structures + '  →  total=' + total;
}
const stateChip = s => chip(s, { live: 'ok', draft: 'warn', expired: '', cancelled: 'bad' }[s]);
const tradeHref = (code, tab) => qs({ trade: code, tab: tab || null, state: null, workflow: null });
function syncUrl() {
    const q = listQuery;
    history.replaceState(null, '', qs({ offset: q.offset || null, limit: q.limit === 15 ? null : q.limit, search: q.search, portfolio: q.portfolio, status: q.status, book: q.book, cp: q.cp, type: q.type, from: q.from, to: q.to, sort: q.sort, dir: q.sort ? (q.desc ? 'desc' : 'asc') : null, show: q.show || null }));
}
function sortBy(key) {
    const q = listQuery;
    if (q.sort !== key) { q.sort = key; q.desc = false; } else if (!q.desc) q.desc = true; else { q.sort = ''; q.desc = true; }
    q.offset = 0;
    renderList();
}

/* A structure row sits beside the trade rows. Its state and version are the legs', so the cells say how many legs it holds. */
function structureRow(r, sel) {
    const books = dealBooks(r), d = r.d;
    return h('tr', null,
        sel ? h('td', { class: 'sel' }) : null,
        h('td', { class: 'nw' }, h('a', { href: dealHref(r.id) }, r.id)), h('td', null, dealTypeText(d), d.parent ? h('span', { class: 'faint small' }, ' · in ' + d.parent) : null,
            h('div', null, chip(d.kind === 'Package' ? 'package' : 'structure', 'acc', 'A ' + d.kind.toLowerCase() + ', holding ' + r.legs.length + (r.legs.length === 1 ? ' trade' : ' trades')))),
        h('td', null, ref('counterparty', d.cp, cpty(d.cp).name)),
        h('td', null, books.length === 1 ? ref('book', books[0], book(books[0]).name) : books.length + ' books'),
        h('td', { class: 'nw faint' }, r.legs.length + (r.legs.length === 1 ? ' trade' : ' trades')), h('td', { class: 'nw' }, d.created.slice(0, 10)),
        h('td', { class: 'num mono nw faint' }, '—'));
}

/* ---------- the list ---------- */
function renderList() {
    const q = listQuery;
    syncUrl();
    const head = pageHead({
        title: 'Trades', lead: 'The trades and structures of ' + PARTY + ' and the books it holds.',
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
        : h('div', null,
            h('div', { class: 'filters' }, searchInput('Search by id, type or counterparty', 'Search')),
            h('div', { class: 'filters' },
                select([['', 'Trades and structures'], ['trades', 'Trades only'], ['structures', 'Structures only']], q.show, v => { q.show = v; q.offset = 0; renderList(); }),
                select([['', 'All counterparties']].concat(COUNTERPARTIES.filter(c => have('cp').includes(c.code)).map(c => [c.code, c.name])), q.cp, v => { q.cp = v; q.offset = 0; renderList(); }),
                select([['', 'All trade types']].concat(have('type').map(c => [c, tradeType(c).description]).sort((x, y) => x[1].localeCompare(y[1]))), q.type, v => { q.type = v; q.offset = 0; renderList(); }),
                select([['', 'All books']].concat(BOOKS.filter(b => have('book').includes(b.code)).map(b => [b.code, b.name])), q.book, v => { q.book = v; q.portfolio = ''; q.offset = 0; renderList(); }),
                select([['', 'All states']].concat(['live', 'draft', 'expired', 'cancelled'].filter(s => have('state').includes(s)).map(s => [s, s.charAt(0).toUpperCase() + s.slice(1)])), q.status, v => { q.status = v; q.offset = 0; renderList(); }),
                h('span', { class: 'filter-group' }, h('span', { class: 'faint small' }, 'Trade date'), dateBox('from', 'Trade date from'), h('span', { class: 'faint small' }, 'to'), dateBox('to', 'Trade date to'))));
    /* The tree sits beside the list. It keeps its shape while the list shows its state, so a person can leave a
     * portfolio that holds nothing. */
    const rail = portfolioRail(q);
    const body = (...kids) => h('div', { class: 'listrail' }, rail, h('div', { class: 'listmain' }, kids));
    if (STATE_P === 'loading') return put(wrap(head, body(filters, h('div', { class: 'notice' }, 'Loading…'))));
    if (STATE_P === 'error') return put(wrap(head, body(h('div', { class: 'notice bad' }, h('b', null, 'The trades could not be read. '), 'trading.v1.trade_summaries.list answered 503 (request 4f1c9a02). ', h('button', { class: 'btn small', onclick: () => location.href = qs({ state: null }) }, 'Retry')))));
    const { rows, total } = STATE_P === 'empty' ? { rows: [], total: 0 } : listTrades(q);
    const sel = CAN.structure || CAN.export;
    const filtered = !!(q.show || q.search || q.status || q.book || q.cp || q.type || q.from || q.to || q.portfolio);
    /* The change the stale list was told about names one trade. The row pulses when the page holds it, and a strip says so when it does not. */
    const newsCode = 'ACME-100202';
    const newsOnPage = rows.some(r => r.kind === 'trade' && r.t.code === newsCode);
    const sortTh = (key, label, cls) => h('th', { class: cls || '', 'aria-sort': q.sort === key ? (q.desc ? 'descending' : 'ascending') : 'none' },
        h('button', { class: 'sort', onclick: () => sortBy(key) }, label + (q.sort === key ? (q.desc ? ' ▾' : ' ▴') : '')));
    const table = h('div', { class: 'tablewrap' }, h('table', { class: 'grid' },
        h('thead', null, h('tr', null, sel ? h('th', { class: 'sel' }) : null, sortTh('code', 'Id'), sortTh('type', 'Type'), sortTh('cp', 'Counterparty'), sortTh('book', 'Book'), sortTh('status', 'State'), sortTh('date', 'Trade date'), sortTh('ver', 'Version', 'num'))),
        h('tbody', null, rows.length ? rows.map(r => r.kind === 'structure' ? structureRow(r, sel) : (t => h('tr', { class: stale && t.code === newsCode ? 'row-news' : '' },
            sel ? h('td', { class: 'sel' }, h('input', { type: 'checkbox', checked: selected.has(t.code), 'aria-label': 'Choose ' + t.code, onchange: e => { e.target.checked ? selected.add(t.code) : selected.delete(t.code); renderList(); } })) : null,
            h('td', { class: 'nw' }, h('a', { href: tradeHref(t.code) }, t.code)), h('td', null, tradeType(t.type).description,
                compositeLegs(t.code).length || t.structure ? h('div', { class: 'chips' },
                    compositeLegs(t.code).length ? h('a', { class: 'chip acc', href: tradeHref(t.code), title: 'Its instrument holds ' + compositeLegs(t.code).join(' and ') }, 'composite · ' + compositeLegs(t.code).length + ' constituent trades') : null,
                    t.structure ? h('a', { class: 'chip acc', href: dealHref(t.structure), title: 'A ' + (roleOfTrade(t.code) || 'leg') + ' of ' + t.structure }, 'leg of ' + t.structure) : null) : null),
            h('td', null, ref('counterparty', t.cp, cpty(t.cp).name)), h('td', null, ref('book', t.book, book(t.book).name)),
            h('td', { class: 'nw' }, stateChip(t.state)), h('td', { class: 'nw' }, t.date),
            h('td', { class: 'num mono nw' }, 'v' + t.ver + ' · ext ' + t.ext)))(r.t)) :
            h('tr', null, h('td', { colspan: sel ? 8 : 7 }, filtered
                ? ['Nothing matches. ', h('button', { class: 'btn small', onclick: () => { Object.assign(listQuery, { offset: 0, show: '', search: '', status: '', book: '', cp: '', type: '', from: '', to: '', portfolio: '' }); renderList(); } }, 'Clear filters')]
                : ['There are no trades yet. ', CAN.book ? h('a', { href: '../book-a-trade/' }, 'Book a trade') : null])))));
    put(wrap(head, selected.size ? h('div', { class: 'notice acc' }, selected.size + ' chosen. ', h('button', { class: 'btn small', onclick: () => { selected.clear(); renderList(); } }, 'Clear')) : null,
        stale && !newsOnPage ? h('div', { class: 'strip' }, 'Something changed that this page cannot show. Press Refresh to read it again.') : null,
        body(filters, table, pager({ offset: q.offset, limit: q.limit, total, onChange: v => { Object.assign(q, v); renderList(); } }),
            h('div', { class: 'reqline' }, requestLine(q, total)))));
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
    const all = HAND_LINKS.slice();
    if (t.rebookedTo) all.push({ from: t.code, to: t.rebookedTo, type: t.rebookKind || 'Novation' });
    if (t.rebookedFrom) all.push({ from: t.rebookedFrom, to: t.code, type: (trade(t.rebookedFrom) || {}).rebookKind || 'Novation' });
    const mine = all.filter(l => l.from === t.code || l.to === t.code);
    const rows = mine.map(l => {
        const lt = linkType(l.type), out = l.from === t.code, other = out ? l.to : l.from;
        return [ref('trade_link_type', l.type, l.type), out ? lt[2] : lt[3], out ? lt[3] : lt[2], h('a', { href: tradeHref(other) }, other), lt[4] ? 'Yes' : 'No'];
    });
    cards.push({ id: 'links', title: 'Linked trades', table: 'trade_link', version: 1, child: true, columns: ['Link type', 'This trade is', 'The other trade is', 'Trade', 'Economic'], rows });
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
    if (c.id === 'instrument' || c.digestGap || c.economic) return ['amendment', 'rebook', 'correction', 'null_amend'];
    return ['correction', 'rebook', 'null_amend'];
}
function openAmend(t, c) {
    const loadedAt = LOADED;
    const rowsMode = !c.fields;
    const cur = rowsMode ? c.rows.map(r => r.map(plain)) : c.fields.map(fd => fd.value);
    let draft = JSON.parse(JSON.stringify(cur));
    let act = allowedActivities(c)[0], reason = 'system.update', note = '';
    const body = h('div', { style: 'display:grid;gap:.8rem' });
    const saveBtn = h('button', { class: 'btn primary' }, 'Save');
    const changedFields = () => rowsMode ? (JSON.stringify(draft) !== JSON.stringify(cur) ? [['rows', '', 'changed']] : [])
        : c.fields.map((fd, i) => String(draft[i]) !== String(cur[i]) ? [fd, cur[i], draft[i]] : null).filter(Boolean);
    function draw() {
        const a = activityType(act), diff = changedFields();
        const econChange = diff.some(([fd]) => fd.economic || (rowsMode && c.economic)) && a.real;
        const folded = econChange && !c.digestGap;
        const allowed = fd => !fd.fixed && (act === 'null_amend' ? false : act === 'rebook' ? true : a.economic ? !!fd.economic : !fd.economic);
        const rowsOff = act === 'null_amend' || (!a.economic && c.economic && act !== 'rebook');
        const reasons = (diff.length ? reasonsFor('amend').filter(r => r[0] !== 'common.non_material_update') : reasonsFor('amend').filter(r => r[0] === 'common.non_material_update'));
        if (!reasons.find(r => r[0] === reason)) reason = reasons[0][0];
        const inputFor = (fd, i) => valueInput(fd, draft[i], (v, redraw) => { draft[i] = v; redraw ? draw() : draw2(); }, !allowed(fd));
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
    if (t.state === 'draft') return [['execute', 'Execute', 'The agreement is struck. The draft goes live.', 'execution'], ['discard', 'Discard', 'The draft comes to nothing.', 'cancel']];
    if (t.state === 'expired' || t.state === 'cancelled') return [['reinstate', 'Reinstate', 'The ' + (t.state === 'expired' ? 'expiry' : 'cancellation') + ' was recorded in error.', t.state === 'expired' ? 'maturity_reversal' : 'cancellation_reversal']];
    return [hasOptions ? ['exercise', 'Exercise', 'The holder exercises the option.', 'exercise'] : null, ['partial', 'Partial termination', 'Part of the notional is closed out.', 'partial_termination'],
        ['expire', 'Expiry', 'The contract ceases naturally.', 'maturity'], ['cancel', 'Cancel', 'Remove the trade from the book. The record stays.', 'cancel_confirmed'],
        ['novate', 'Novation', 'The counterparty changes. Cancel and rebook, joined by a link.', 'novation'], ['bookmove', 'Book move', 'Move to a book of another legal entity. Cancel and rebook.', 'transfer']].filter(Boolean);
}
function closeWorkflow() {
    go({ workflow: null }, () => { WORKFLOW = ''; renderPage(); });
}
function renderLifecycle(t) {
    setCrumbs([['Home', '#'], ['Trades', '../trade-page/'], ['Record a lifecycle event']], 'Record a lifecycle event');
    if (!CAN.event) return put(wrap(h('div', { class: 'notice' }, h('b', null, 'You may not record lifecycle events. '), 'Ask the tenant administrator for a role that does.')));
    const evs = lifecycleEvents(t);
    let step = 0, ev = evs[0], when_ = '2026-10-11', amount = '', newCp = COUNTERPARTIES.find(c => c.open && c.code !== t.cp).code, newBook = 'EMEA-RATES', reason = 'system.update', note = '', done = null;
    const body = h('div');
    const STEPS = ['Event', 'Facts', 'Effect', 'Record'];
    const next = h('button', { class: 'btn primary' }, 'Next');
    const cancel = h('button', { class: 'btn', onclick: closeWorkflow }, 'Cancel');
    function draw() {
        const a = activityType(ev[3]);
        const rs = reasonsFor(a.category === 'cancellation' ? 'delete' : 'amend'); if (!rs.find(r => r[0] === reason)) reason = rs[0][0];
        const rail = h('div', { class: 'steps' }, STEPS.map((s, i) => h('span', { class: 'step' + (i === step ? ' on' : i < step ? ' done' : '') }, (i + 1) + ' ' + s)));
        let pane;
        if (done) pane = h('div', { class: 'notice ok' }, h('b', null, ev[1] + ' recorded. '), 'Refresh the page to read it. ', done.map(x => x));
        else if (step === 0) pane = h('div', { class: 'picker' }, evs.map(e => h('button', { class: 'opt' + (e === ev ? ' on' : ''), onclick: () => { ev = e; draw(); } }, e[1], h('small', null, e[2]))));
        else if (step === 1) pane = h('div', { style: 'display:grid;gap:.8rem' }, h('div', { class: 'grid2' }, field('Event date', h('input', { class: 'field-input', type: 'date', value: when_, oninput: e => { when_ = e.target.value; } })),
            ev[0] === 'exercise' || ev[0] === 'partial' ? field(ev[0] === 'exercise' ? 'Exercised amount' : 'Terminated amount', h('input', { class: 'field-input', value: amount, placeholder: money(t.notional, t.ccy), oninput: e => { amount = e.target.value; } })) : null,
            ev[0] === 'novate' ? field('New counterparty', select(COUNTERPARTIES.filter(c => c.open && c.code !== t.cp).map(c => [c.code, c.name]), newCp, v => { newCp = v; })) : null,
            ev[0] === 'bookmove' ? field('Destination book', select(BOOKS.filter(b => !b.virtual && bookOpen(b) && b.code !== t.book).map(b => [b.code, b.name]), newBook, v => { newBook = v; })) : null),
            h('div', { class: 'grid2' }, field('Reason', select(reasonsFor(a.category === 'cancellation' ? 'delete' : 'amend').filter(r => r[0] !== 'common.non_material_update'), reason, v => { reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { note = e.target.value; } }, note))));
        else if (step === 2) pane = h('div', { class: 'notice acc' }, h('b', null, 'What this will do'),
            h('div', null, 'Activity ' + a.label + ' (' + a.category.replace('_', ' ') + ').'),
            ['novate', 'bookmove'].includes(ev[0]) ? [h('div', null, 'Cancels ' + t.code + '.'), h('div', null, 'Books a new trade with a fresh id ' + (ev[0] === 'novate' ? 'against ' + cpty(newCp).name : 'in ' + book(newBook).name) + '.'), h('div', null, 'Joins the two with a ' + (ev[0] === 'novate' ? 'Novation' : 'BookMove') + ' link: ' + linkType(ev[0] === 'novate' ? 'Novation' : 'BookMove').slice(2, 4).join(' → ') + '. One transaction.')]
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
                t.rebookedTo = n.code; t.rebookKind = ev[0] === 'novate' ? 'Novation' : 'BookMove'; n.rebookKind = t.rebookKind;
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

'use strict';
/* The trade list and the trade page. Serves: review, amend, lifecycle event,
 * and the doors to booking, ORE import and export, and structures. */

const STATE_P = P.get('state') || '';
const SERVER = P.get('server') === 'today' ? 'today' : 'target';
let TAB = P.get('tab') || 'details';
let WORKFLOW = P.get('workflow') || '';
const CODE = P.get('trade') || 'ACME-100201';
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
    party: P.get('party') || '', portfolio: P.get('portfolio') || ''
};
let selected = new Set();
const pageCrumbs = () => [['Home', '#'], ['Trades', '../blotter/'], ['Trade ' + CODE]];
const main = shell({
    title: 'Trade ' + CODE, name: 'trade-page',
    question: 'Does one trade page serve review, amendment and lifecycle events?',
    crumbs: pageCrumbs(),
    states: [['page', { state: null, workflow: null }, !STATE_P], ['stale', { state: 'stale', workflow: null }, STATE_P === 'stale'], ['conflict', { state: 'conflict', workflow: null }, STATE_P === 'conflict'], ['digest fixed', { digest: DIGEST_FIXED ? null : 'fixed' }, DIGEST_FIXED],
        ['loading', { state: 'loading' }, STATE_P === 'loading'], ['error', { state: 'error' }, STATE_P === 'error'], ['refused', { state: 'refused', tab: 'history' }, STATE_P === 'refused']],
    says: 'One page per trade, one tab per aspect, an Edit button on each card, and history as a timeline. A tab switches in place, so a save is not lost. Edit asks for the kind of change first and offers only the fields that kind may touch. A save does not reload the page: Refresh turns gold and the person presses it. Recording a lifecycle event is a workflow with its own address. Try Edit on the instrument card as a trader, then Workflows as operations.'
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
    portfolio: '<svg viewBox="0 0 20 20" width="20" height="20" fill="none" aria-hidden="true"><path fill="currentColor" d="M11.25 2C12.2165 2 13 2.7835 13 3.75V5H15C16.6569 5 18 6.34315 18 8V14C18 15.6569 16.6569 17 15 17H5C3.39489 17 2.08421 15.7394 2.00391 14.1543L2 14V8C2 6.34315 3.34315 5 5 5H7V3.75C7 2.7835 7.7835 2 8.75 2H11.25ZM17 11.2314C16.469 11.7077 15.7695 12 15 12H11V12.5C11 12.7761 10.7761 13 10.5 13H9.5C9.22386 13 9 12.7761 9 12.5V12H5C4.23053 12 3.53103 11.7077 3 11.2314V14L3.01074 14.2041C3.113 15.2128 3.96435 16 5 16H15C16.1046 16 17 15.1046 17 14V11.2314ZM5 6C3.89543 6 3 6.89543 3 8V9C3 10.1046 3.89543 11 5 11H9V10.5C9 10.2239 9.22386 10 9.5 10H10.5C10.7761 10 11 10.2239 11 10.5V11H15C16.1046 11 17 10.1046 17 9V8C17 6.89543 16.1046 6 15 6H5ZM8.75 3C8.33579 3 8 3.33579 8 3.75V5H12V3.75C12 3.33579 11.6642 3 11.25 3H8.75Z"/></svg>',
    party: '<svg viewBox="0 0 20 20" width="20" height="20" fill="none" aria-hidden="true"><path fill="currentColor" d="M9.99992 2C8.34271 2 6.99927 3.34344 6.99927 5.00066C6.99927 6.48754 8.08073 7.72183 9.49999 7.95985V9.5H6.5C5.67157 9.5 5 10.1716 5 11V12.0416C3.5811 12.2799 2.5 13.514 2.5 15.0007C2.5 16.6579 3.84344 18.0013 5.50066 18.0013C7.15787 18.0013 8.50131 16.6579 8.50131 15.0007C8.50131 13.5136 7.41954 12.2791 6 12.0414V11C6 10.7239 6.22386 10.5 6.5 10.5H13.5C13.7761 10.5 14 10.7239 14 11V12.0416C12.5811 12.2799 11.5 13.514 11.5 15.0007C11.5 16.6579 12.8434 18.0013 14.5007 18.0013C16.1579 18.0013 17.5013 16.6579 17.5013 15.0007C17.5013 13.5136 16.4195 12.2791 15 12.0414V11C15 10.1716 14.3284 9.5 13.5 9.5H10.5V7.95983C11.9192 7.72176 13.0006 6.48749 13.0006 5.00066C13.0006 3.34344 11.6571 2 9.99992 2ZM7.99927 5.00066C7.99927 3.89572 8.89499 3 9.99992 3C11.1049 3 12.0006 3.89572 12.0006 5.00066C12.0006 6.10559 11.1049 7.00131 9.99992 7.00131C8.89499 7.00131 7.99927 6.10559 7.99927 5.00066ZM3.5 15.0007C3.5 13.8957 4.39572 13 5.50066 13C6.60559 13 7.50131 13.8957 7.50131 15.0007C7.50131 16.1056 6.60559 17.0013 5.50066 17.0013C4.39572 17.0013 3.5 16.1056 3.5 15.0007ZM14.5007 13C15.6056 13 16.5013 13.8957 16.5013 15.0007C16.5013 16.1056 15.6056 17.0013 14.5007 17.0013C13.3957 17.0013 12.5 16.1056 12.5 15.0007C12.5 13.8957 13.3957 13 14.5007 13Z"/></svg>',
    book: '<svg viewBox="0 0 20 20" width="20" height="20" fill="none" aria-hidden="true"><path fill="currentColor" d="M6 5C6 4.44772 6.44772 4 7 4H13C13.5523 4 14 4.44772 14 5V6C14 6.55228 13.5523 7 13 7H7C6.44772 7 6 6.55228 6 6V5ZM7 5V6H13V5H7ZM4 4V16C4 17.1046 4.89543 18 6 18H15.5C15.7761 18 16 17.7761 16 17.5C16 17.2239 15.7761 17 15.5 17H6C5.44772 17 5 16.5523 5 16H15C15.5523 16 16 15.5523 16 15V4C16 2.89543 15.1046 2 14 2H6C4.89543 2 4 2.89543 4 4ZM14 3C14.5523 3 15 3.44772 15 4V15H5V4C5 3.44772 5.44772 3 6 3H14Z"/></svg>'
};
const treeIcon = kind => {
    const el = h('span', { class: 'tree-icon' });
    el.innerHTML = ICONS[kind];
    return el;
};
/* A party holds its own portfolios and nests under its parent party, so a holding company shows its operating companies
 * beneath it. The mock states the group the trades belong to, and one operating company with no books yet, because the
 * legacy tree drew a party that holds nothing. */
const PARTIES = [
    { id: 'GRP-ACME', name: 'Acme Corporation', parent: null, holding: true },
    { id: 'PTY-LONDON', name: 'Acme London', parent: 'GRP-ACME' },
    { id: 'PTY-NEW-YORK', name: 'Acme New York', parent: 'GRP-ACME' }
];
/* Only a root portfolio names its party; a child portfolio hangs off its parent, as the model states it. */
const PORTFOLIOS = [
    { id: 'ACME-LONDON', name: 'Acme London', parent: null, party: 'PTY-LONDON' },
    { id: 'TRADING', name: 'Trading', parent: 'ACME-LONDON' },
    { id: 'RATES', name: 'Rates', parent: 'TRADING' },
    { id: 'FX', name: 'FX flow', parent: 'TRADING' },
    { id: 'CREDIT', name: 'Credit', parent: 'TRADING' },
    { id: 'EQUITY', name: 'Equity', parent: 'TRADING' },
    { id: 'COMMODITY', name: 'Commodity', parent: 'TRADING' },
    { id: 'CLOSED-2025', name: 'Closed 2025', parent: 'ACME-LONDON', party: 'PTY-LONDON' },
    { id: 'SANDBOX', name: 'Sandboxes', parent: 'ACME-LONDON', virtual: true, party: 'PTY-LONDON' },
    { id: 'SANDBOX-ORE', name: 'ORE samples', parent: 'SANDBOX', virtual: true },
    { id: 'SANDBOX-WHATIF', name: 'Rates what-ifs', parent: 'SANDBOX', virtual: true }
];
/* The mock books carry a group name, not a portfolio id, so the prototype maps each book to its portfolio by hand. The
 * model states the same link with book.parent_portfolio_id, which is what the junction capture asks the server for. */
const PORTFOLIO_OF_BOOK = {
    'EMEA-RATES': 'RATES', 'EMEA-RATES-2025': 'CLOSED-2025', 'EMEA-FX': 'FX', 'EMEA-CREDIT': 'CREDIT',
    'EMEA-EQ': 'EQUITY', 'EMEA-CMDTY': 'COMMODITY', 'VIRT-DRAFT': 'SANDBOX-WHATIF',
    'ORE-EX-SWAPS': 'SANDBOX-ORE', 'ORE-EX-FX': 'SANDBOX-ORE'
};
const childPortfolios = id => PORTFOLIOS.filter(p => p.parent === id);
const rootPortfoliosOf = party => PORTFOLIOS.filter(p => !p.parent && p.party === party);
const childParties = id => PARTIES.filter(p => p.parent === id);
const booksIn = id => BOOKS.filter(b => PORTFOLIO_OF_BOOK[b.code] === id);
/* A folder that counted only its own books would look empty while its children held the population. */
function subtree(id) {
    const out = [id];
    for (const child of childPortfolios(id)) out.push(...subtree(child.id));
    return out;
}
const partiesUnder = id => [id, ...childParties(id).flatMap(p => partiesUnder(p.id))];
const booksUnderPortfolio = id => subtree(id).flatMap(booksIn);
/* A party's books are the books of its root portfolios and of every party under it. */
const booksUnderParty = id => partiesUnder(id).flatMap(party => rootPortfoliosOf(party).flatMap(p => booksUnderPortfolio(p.id)));
const tradesIn = codes => {
    const set = new Set(codes);
    return TRADES.filter(t => set.has(t.book)).length;
};
const tradesInBook = code => tradesIn([code]);
const tradesInPortfolio = id => tradesIn(booksUnderPortfolio(id).map(b => b.code));
const tradesInParty = id => tradesIn(booksUnderParty(id).map(b => b.code));
/* The node the tree has chosen, and the books that choice selects: a book selects itself, a portfolio the books under
 * it, and a party every book of the parties under it. The list, the request line and the note ask these two, so the
 * rule is written once. */
const TREE_KINDS = ['party', 'portfolio', 'book'];
const chosenNode = q => q.book || q.portfolio || q.party || '';
const chosenBookCodes = q => {
    if (q.book) return [q.book];
    if (q.portfolio) return booksUnderPortfolio(q.portfolio).map(b => b.code);
    if (q.party) return booksUnderParty(q.party).map(b => b.code);
    return null;
};
function portfolioRail(q) {
    const chosen = chosenNode(q);
    const treeRow = ({ kind, id, label, count, depth, virtual, title }) => h('button', {
        class: 'tree-node' + (chosen === id ? ' tree-chosen' : '') + (virtual ? ' tree-virtual' : ''),
        style: 'padding-left:' + (0.5 + depth * 0.9).toFixed(2) + 'rem',
        'aria-current': chosen === id ? 'true' : null,
        title,
        onclick: () => {
            q[kind] = q[kind] === id ? '' : id;
            TREE_KINDS.filter(other => other !== kind).forEach(other => { q[other] = ''; });
            q.offset = 0;
            renderList();
        }
    }, treeIcon(kind), h('span', { class: 'tree-name' }, label), h('span', { class: 'tree-count' }, String(count)));
    /* The order the legacy tree drew: a party's portfolios, then the parties it holds. */
    const portfolioRows = (p, depth) => [
        treeRow({ kind: 'portfolio', id: p.id, label: p.name, count: tradesInPortfolio(p.id), depth, virtual: p.virtual,
            title: p.virtual ? p.name + ' — a virtual portfolio, in a sandbox' : p.name + ' — a portfolio' }),
        childPortfolios(p.id).map(k => portfolioRows(k, depth + 1)),
        booksIn(p.id).map(b => treeRow({ kind: 'book', id: b.code, label: b.name, count: tradesInBook(b.code), depth: depth + 1,
            title: b.name + ' — a book in ' + p.name }))
    ];
    const partyRows = (party, depth) => [
        treeRow({ kind: 'party', id: party.id, label: party.name, count: tradesInParty(party.id), depth,
            title: party.holding ? party.name + ' — a holding company' : party.name + ' — a party' }),
        rootPortfoliosOf(party.id).map(p => portfolioRows(p, depth + 1)),
        childParties(party.id).map(child => partyRows(child, depth + 1))
    ];
    return h('aside', { class: 'ptree', 'aria-label': 'Portfolio tree' },
        h('div', { class: 'ptree-head' }, 'Portfolio tree'),
        PARTIES.filter(p => !p.parent).map(p => partyRows(p, 0)),
        h('div', { class: 'ptree-note faint small' }, chosen ? 'A folder holds the books under it. Choose it again to clear.' : 'Choose a party, a portfolio or a book to narrow the list.'));
}
/* A structure matches a filter when one of its legs does. */
function listTrades(q) {
    const codes = chosenBookCodes(q);
    const chosen = codes ? new Set(codes) : null;
    const tradeOk = t => (!chosen || chosen.has(t.book)) && (!q.status || t.state === q.status) && (!q.type || t.type === q.type) && (!q.from || t.date >= q.from) && (!q.to || t.date <= q.to);
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
        /* The narrowest member that states the choice. A party has a column of its own, and a portfolio names none, so
         * it asks for the books under it while a book asks for itself: one value is an equals member and many are a
         * one-of member, which is the shape the filter record states. */
        if (q.party) {
            f.party_id = q.party;
        } else {
            const codes = chosenBookCodes(q);
            if (codes && codes.length === 1) f.book_id = codes[0];
            else if (codes && codes.length > 1) f.book_id_one_of = codes;
        }
        if (q.status) f.status = q.status;
        if (q.cp) f.counterparty_id = q.cp;
        if (q.type) f.instrument_type = q.type;
        if (q.from) f.trade_date_from = q.from;
        if (q.to) f.trade_date_to = q.to;
    }
    const subject = SERVER === 'today' ? 'trading.v1.trades.list' : 'trading.v1.trade_summaries.list';
    const order = q.sort ? { field: SORT_FIELDS[q.sort], descending: q.desc } : { field: 'updated_at', descending: true };
    const structures = q.show !== 'trades' && SERVER !== 'today' ? '  +  trading.v1.structures.list  (a structure matches when one of its legs does)' : '';
    return subject + '  offset=' + q.offset + ' limit=' + q.limit + ' order=' + JSON.stringify(order) + ' filter=' + JSON.stringify(f) + structures + '  →  total=' + total;
}
/* A row says what it is: a trade whose instrument is composite, or a trade a deal holds. Both are links to the record
 * that holds the trade, and a plain trade says nothing. */
const rowChips = t => {
    const legs = compositeLegs(t.code);
    if (!legs.length && !t.structure) return null;
    return h('div', { class: 'chips' },
        legs.length ? h('a', { class: 'chip acc', href: tradeHref(t.code), title: 'Its instrument holds ' + legs.join(' and ') }, 'composite · ' + legs.length + ' constituent trades') : null,
        t.structure ? h('a', { class: 'chip acc', href: dealHref(t.structure), title: 'A ' + (roleOfTrade(t.code) || 'leg') + ' of ' + t.structure }, 'leg of ' + t.structure) : null);
};
const stateChip = s => chip(s, { live: 'ok', draft: 'warn', expired: '', cancelled: 'bad' }[s]);
const tradeHref = (code, tab) => qs({ trade: code, tab: tab || null, state: null, workflow: null });
function syncUrl() {
    const q = listQuery;
    history.replaceState(null, '', qs({ offset: q.offset || null, limit: q.limit === 15 ? null : q.limit, search: q.search, party: q.party, portfolio: q.portfolio, status: q.status, book: q.book, cp: q.cp, type: q.type, from: q.from, to: q.to, sort: q.sort, dir: q.sort ? (q.desc ? 'desc' : 'asc') : null, show: q.show || null }));
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

/* ---------- applying writes after Refresh ---------- */
function refresh() {
    LOADED = NOW; stale = false;
    for (const w of PENDING.splice(0)) w();
    renderPage();
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

renderPage();

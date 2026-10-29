/* Trade blotter prototype.
 *
 * One screen for trades and structures: a portfolio tree on the left, the trade
 * list beside it. The list holds what a person books and sees at the top level —
 * leaf trades and whole structures. A structure's legs are not rows of it; the
 * flat option is what turns them into rows. A row opens the trade view or the
 * structure view, which are the other two prototypes.
 *
 * A structure has no book. It takes the book its legs share, and when its legs
 * span books it is placed at the node that holds them all and says that it spans.
 * The investigation "The book of a structure" records why the rule is not
 * enforced instead.
 */

const KIND = STRUCTURE_KINDS;
const noun = k => (k === 'Package' ? 'package' : 'structure');
const Noun = k => noun(k).replace(/^./, c => c.toUpperCase());
const DEALS = allDeals();
const ROOT_ID = 'party';

/* A top-level structure is the one a person sees: an inner structure is inside
 * its parent, so it is never a row of the list. */
const isStructure = id => !!DEALS[id];
const topStructures = () => Object.entries(DEALS).filter(([, d]) => !d.parent).map(([id]) => id);

/* Every trade under a structure, at any depth, with the structure that holds it
 * directly. A leg of an inner structure belongs to that inner structure. */
function leavesOf(id, seen = new Set()) {
    if (seen.has(id)) return [];
    seen.add(id);
    const own = (DEALS[id].members || [])
        .filter(([code]) => trade(code))
        .map(([code, role]) => ({ code, role, holder: id }));
    const inner = Object.entries(DEALS)
        .filter(([, d]) => d.parent === id)
        .flatMap(([cid]) => leavesOf(cid, seen));
    return own.concat(inner);
}

const booksOfLeaves = leaves =>
    [...new Set(leaves.map(l => trade(l.code).book).filter(Boolean))];

/* The node a structure sits under. One book: the book. Books on one desk: the
 * desk. Anything wider: the party, which holds every desk. */
function placeOf(books) {
    if (books.length === 0) return { node: ROOT_ID, spans: false };
    if (books.length === 1) return { node: books[0], spans: false };
    const desks = [...new Set(books.map(code => book(code).parent))];
    return { node: desks.length === 1 ? desks[0] : ROOT_ID, spans: true };
}

/* The rows the blotter draws, before a node or a perspective narrows them. */
const ROWS = (() => {
    const structures = topStructures().map(id => {
        const deal = DEALS[id];
        const leaves = leavesOf(id);
        const books = booksOfLeaves(leaves);
        const { node, spans } = placeOf(books);
        const products = [...new Set(leaves.map(l => trade(l.code).product))];
        const dates = leaves.map(l => trade(l.code).date).filter(Boolean).sort();
        return {
            kind: 'structure',
            id,
            what: deal.kind + (deal.template ? ' · ' + deal.template : ''),
            description: KIND[deal.kind].whole ? 'Confirmed as one document' : 'Each leg confirms on its own',
            book: books.length === 1 ? books[0] : books.join(' + '),
            node,
            spans,
            cp: deal.cp,
            products,
            date: dates[0] || '',
            state: '',
            ext: '',
            leaves,
        };
    });
    const held = new Set(structures.flatMap(r => r.leaves.map(l => l.code)));
    const loose = TRADES.filter(t => !held.has(t.code)).map(t => ({
        kind: 'trade',
        id: t.code,
        what: tradeType(t.type).description,
        description: t.type,
        book: t.book,
        node: t.book,
        spans: false,
        cp: t.cp,
        products: [t.product],
        date: t.date,
        state: t.state,
        ext: t.ext,
        leaves: [],
    }));
    return structures.concat(loose);
})();

/* The tree's nodes, built from the books the mock data holds. */
const DESKS = [...new Set(BOOKS.map(b => b.parent))].sort();
const nodes = new Map([[ROOT_ID, { id: ROOT_ID, label: PARTY, kind: 'party' }]]);
DESKS.forEach(d => nodes.set(d, { id: d, label: d, kind: 'desk' }));
BOOKS.forEach(b => nodes.set(b.code, { id: b.code, label: b.name, kind: 'book', desk: b.parent }));

/* The rows under a tree node: its own, and every node below it. */
function rowsUnder(id) {
    if (id === ROOT_ID) return ROWS;
    const node = nodes.get(id);
    if (!node) return [];
    const own = ROWS.filter(r => r.node === id);
    if (node.kind === 'desk') {
        const below = BOOKS.filter(b => b.parent === id).map(b => b.code);
        return own.concat(ROWS.filter(r => below.includes(r.node)));
    }
    return own;
}

/* The perspectives. Each is a topology the same rows are read through. */
const VIEWS = [['portfolio', 'Portfolio'], ['counterparty', 'Counterparty'], ['instrument', 'Asset class']];
const view = VIEWS.some(([id]) => id === P.get('view')) ? P.get('view') : 'portfolio';
const flat = P.get('flat') === '1';
let sel = P.get('sel') || '';
/* The rows a perspective rail offers: the counterparties, or the asset classes. */
function railRows() {
    if (view === 'counterparty') {
        return COUNTERPARTIES.filter(c => ROWS.some(r => r.cp === c.code)).map(c => ({ id: c.code, label: c.name, kind: 'cpty' }));
    }
    if (view === 'instrument') {
        return PRODUCTS.map(p => ({ id: p, label: p.replace(/^./, c => c.toUpperCase()), kind: 'product' }));
    }
    return [];
}
const rowsOf = row => {
    if (view === 'counterparty') return sel === '' || row.cp === sel;
    if (view === 'instrument') return sel === '' || row.products.includes(sel);
    return true;
};

/* One list row, flattened to what the table prints. */
function listRows() {
    const rows = view === 'portfolio' ? rowsUnder(sel === '' ? ROOT_ID : sel) : ROWS;
    const kept = rows.filter(rowsOf);
    if (!flat) return kept;
    /* Flat: a structure is replaced by its legs, so every row is a trade. */
    return kept.flatMap(r => r.kind === 'trade'
        ? [r]
        : r.leaves.map(l => {
              const t = trade(l.code);
              return {
                  kind: 'trade', id: t.code, structure: l.holder, what: tradeType(t.type).description,
                  description: t.type, book: t.book, node: r.node, spans: false, cp: t.cp,
                  products: [t.product], date: t.date, state: t.state, ext: t.ext, leaves: [],
              };
          }));
}

const hrefOf = row => row.kind === 'structure'
    ? '../structure-page/?deal=' + encodeURIComponent(row.id)
    : '../trade-page/?trade=' + encodeURIComponent(row.id);

/* ---------- the screen ---------- */

const railPanel = h('section', { class: 'panel' });
const listPanel = h('section', { class: 'panel' });
const main = shell({
    title: 'Trade blotter', name: 'blotter',
    question: 'What does a book hold, and where does one structure sit in it?',
    crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Trade blotter']],
    states: [
        ['portfolio', { view: null, flat: null, sel: null, state: null }, view === 'portfolio'],
        ['flat', { flat: '1' }, flat],
        ['counterparty', { view: 'counterparty', sel: null, flat: null, state: null }, view === 'counterparty'],
        ['asset class', { view: 'instrument', sel: null, flat: null, state: null }, view === 'instrument'],
        ['spanning structure', { view: null, flat: null, sel: null, state: null }, false],
    ],
    says: 'One screen reads trades and structures together. The tree on the left is the portfolio; the list beside it holds the top-level trades and the whole structures. ' +
        'A structure\'s legs are not rows: press Flat to read them as trades. A structure whose legs span books sits at the node that holds them all and says so.',
});
main.append(h('div', { class: 'toolbar', style: 'display:flex;gap:.5rem;align-items:center;flex-wrap:wrap;margin-bottom:.75rem' },
    VIEWS.map(([id, label]) => h('a', {
        class: 'btn small' + (view === id ? ' primary' : ''),
        href: qs({ view: id === 'portfolio' ? null : id, sel: null }),
        onclick: e => { e.preventDefault(); go({ view: id === 'portfolio' ? null : id, sel: null }, render); },
    }, label)),
    h('span', { class: 'sep faint small', style: 'margin-left:.25rem' }, '|'),
    h('a', {
        class: 'btn small' + (flat ? ' primary' : ''),
        href: qs({ flat: flat ? null : '1' }),
        'aria-pressed': flat ? 'true' : 'false',
        title: 'Read the legs of a structure as trades',
        onclick: e => { e.preventDefault(); go({ flat: flat ? null : '1' }, render); },
    }, 'Flat'),
    h('span', { class: 'small faint', style: 'margin-left:auto' }, 'Mock data · one party · ' +
        ROWS.length + ' rows at the top level'),
), h('div', { class: 'workspace', style: 'grid-template-columns:320px minmax(0,1fr)' }, railPanel, listPanel));

function go(change, render) {
    history.pushState(null, '', qs(change));
    render();
}

function renderRail() {
    const header = h('header', null,
        h('h2', null, view === 'portfolio' ? 'Portfolio' : view === 'counterparty' ? 'Counterparties' : 'Asset classes'),
        h('span', { class: 'legend' }, view === 'portfolio'
            ? 'The party, its desks and their books'
            : view === 'counterparty' ? 'One counterparty at a time' : 'One asset class at a time'));
    const body = h('div', { class: 'tree' });
    /* A node is chosen by the node, and the count is the node's own: the rail
     * never counts what the current filter left. */
    const link = (id, label, meta, depth, count) => {
        const chosen = view === 'portfolio' ? (sel === '' ? ROOT_ID : sel) === id : sel === id;
        return h('a', {
            href: qs({ sel: id === '' || (view === 'portfolio' && id === ROOT_ID) ? null : id }),
            class: 'node-row' + (chosen ? ' selected' : ''),
            style: 'text-decoration:none;color:inherit;margin-left:' + depth * 14 + 'px',
            onclick: e => {
                e.preventDefault();
                go({ sel: id === '' || (view === 'portfolio' && id === ROOT_ID) ? null : id }, render);
            },
        }, h('span', { class: 'label' }, h('span', { class: 'name' }, label),
            meta ? h('span', { class: 'meta' }, meta) : null,
            h('span', { class: 'num' }, '  ' + count)));
    };

    if (view === 'portfolio') {
        body.append(link(ROOT_ID, PARTY, 'party · every book', 0, ROWS.length));
        DESKS.forEach(desk => {
            if (rowsUnder(desk).length === 0) return;
            body.append(link(desk, desk, 'desk', 1, rowsUnder(desk).length));
            BOOKS.filter(b => b.parent === desk && rowsUnder(b.code).length > 0).forEach(b => {
                body.append(link(b.code, b.name, b.status === 'Active' ? b.code : b.status.toLowerCase(), 2, rowsUnder(b.code).length));
            });
        });
    } else {
        if (sel !== '' && !railRows().some(r => r.id === sel)) sel = '';
        const countOf = id => ROWS.filter(r => (view === 'counterparty' ? r.cp === id : r.products.includes(id))).length;
        body.append(link('', 'All ' + (view === 'counterparty' ? 'counterparties' : 'asset classes'), '', 0, ROWS.length));
        railRows().forEach(r => body.append(link(r.id, r.label, r.kind === 'cpty' ? r.id : 'asset class', 1, countOf(r.id))));
    }
    railPanel.replaceChildren(header, body, h('div', { class: 'tree-note' },
        view === 'portfolio'
            ? 'A structure sits where its legs are. When they span books it sits at the node holding them all and says so.'
            : view === 'counterparty'
                ? 'The counterparty is the topology sales and credit risk read. Every row states it.'
                : 'The asset class is the topology risk reads. A structure that mixes families is listed under each.'));
}

function renderList() {
    const rows = listRows();
    const total = rows.length;
    const header = h('header', null,
        h('h2', null, flat ? 'Trades' : 'Trades and structures'),
        h('span', { class: 'legend' },
            flat ? 'Every row is a trade: a structure is drawn as its legs'
                : 'Trades that belong to no structure, and structures as one row each'));

    const columns = ['Identifier', 'What', 'Book', 'Counterparty', 'Date', 'State'];
    if (flat) columns.splice(1, 0, 'Structure');
    const head = h('tr', null, columns.map(c => h('th', null, c)));
    const body = h('tbody', null);
    if (total === 0) {
        body.append(h('tr', null, h('td', { colspan: columns.length }, h('span', { class: 'muted' }, 'Nothing is booked here.'))));
    }
    rows.forEach(r => {
        const other = cpty(r.cp);
        const tds = [
            h('td', { class: 'mono' }, h('a', { href: hrefOf(r), 'data-text': r.id }, r.id),
                r.spans ? h('span', null, ' ', chip('spans books', 'warn', 'Its legs are booked in more than one book. It sits at the node that holds them all.')) : null),
        ];
        if (flat) tds.push(h('td', { class: 'mono faint' }, r.structure || '—'));
        tds.push(
            h('td', null, r.what,
                r.kind === 'structure' ? h('span', { class: 'faint small' }, ' · ' + r.description) : null),
            h('td', { class: 'mono', style: 'white-space:nowrap' }, r.book),
            h('td', { style: 'white-space:nowrap' }, other ? other.name : r.cp),
            h('td', { class: 'mono', style: 'white-space:nowrap' }, r.date || '—'),
            h('td', null, r.state ? chip(r.state, r.state === 'live' ? 'ok' : r.state === 'draft' ? 'warn' : '') : h('span', { class: 'faint' }, '—')),
        );
        body.append(h('tr', null, ...tds));
    });

    const table = h('div', { class: 'tablewrap', style: 'overflow:auto' },
        h('table', { class: 'grid', style: 'font-size:.82rem;white-space:nowrap' }, h('thead', null, head), body));
    const note = h('div', { class: 'tree-note', style: 'border:1px solid var(--line);border-radius:var(--radius-card);margin-top:.6rem' },
        flat
            ? 'Flat: the list holds the legs of every structure and every trade that is not in one. The structure a leg belongs to is its own column.'
            : 'A structure is one row and its legs are inside it. Open the row for the structure view; a trade row opens the trade view.');
    const pagerLine = h('div', { class: 'pager' }, h('span', null, 'Showing 1–' + total + ' of ' + total), h('span', { class: 'btns small faint' }, 'No paging in the prototype'));
    listPanel.replaceChildren(header, h('div', { style: 'padding:.5rem .9rem 0' }, table), h('div', { style: 'padding:0 .9rem' }, note, pagerLine));
}

function render() {
    renderRail();
    renderList();
}

render();

'use strict';
/* The trade list and the trade page. Serves: review, amend, lifecycle event,
 * and the doors to booking, ORE import and export, and structures. */

const STATE_P = P.get('state') || '';
const SERVER = P.get('server') === 'today' ? 'today' : 'target';
const TAB = P.get('tab') || 'details';
const CODE = P.get('trade');
const TABS = [['details', 'Details'], ['instrument', 'Instrument'], ['identifiers', 'Identifiers and roles'], ['links', 'Links'], ['history', 'History']];

/* What the server holds, and what the screen has been told about. A save does
 * not reload the page: it makes Refresh gold, and the person presses it. */
const OV = {};            // per trade: card id -> field key -> value
const EXTRA = {};         // per trade: history entries written this session
const PENDING = [];       // writes the screen has not read back yet
let stale = STATE_P === 'stale';
let flashId = null;
let listQuery = { offset: 0, limit: 15, search: '', state: '', book: '', cp: '' };
let selected = new Set();
const main = shell({
    title: CODE ? 'Trade ' + CODE : 'Trades', name: 'trade-page',
    question: 'Does one trade page serve review, amendment and lifecycle events?',
    crumbs: CODE ? [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', qs({ trade: null, tab: null })], [CODE]] : [['Home', '#'], ['Trades', '../trade-page/'], ['Trades']],
    states: CODE
        ? [['page', { state: null }, !STATE_P], ['stale', { state: 'stale' }, STATE_P === 'stale'], ['conflict', { state: 'conflict' }, STATE_P === 'conflict'], ['digest fixed', { digest: DIGEST_FIXED ? null : 'fixed' }, DIGEST_FIXED], ['list', { trade: null, tab: null }]]
        : [['list', { state: null }, !STATE_P], ['stale', { state: 'stale' }, STATE_P === 'stale'], ['loading', { state: 'loading' }, STATE_P === 'loading'], ['empty', { state: 'empty' }, STATE_P === 'empty'], ['error', { state: 'error' }, STATE_P === 'error'],
            ['server today', { server: SERVER === 'today' ? null : 'today' }, SERVER === 'today']],
    says: CODE
        ? 'One page per trade, one tab per aspect, an Edit button on each card, and history as a timeline. Edit asks for the kind of change first and offers only the fields that kind may touch. A save does not reload the page: Refresh turns gold and the person presses it. Try Edit on the instrument card as a trader, then Workflows as operations.'
        : 'The list is the request: the pager, the filters and the request line under the table are one thing. The Workflows menu carries the doors to booking, ORE import and export, and structures, so a person looking at trades finds them here.'
});

function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); el.classList.add('dyn'); main.append(el); }
const wrap = (...kids) => h('div', { class: 'dyn' }, kids);

/* ---------- the server, as the list sees it ---------- */
function listTrades(q) {
    let rows = TRADES.filter(t => (!q.state || t.state === q.state) && (!q.book || t.book === q.book) && (!q.cp || t.cp === q.cp));
    if (q.search) {
        const s = q.search.toLowerCase();
        rows = SERVER === 'today'
            ? rows.filter(t => t.id.startsWith(s) || t.code.toLowerCase() === s)
            : rows.filter(t => (t.code + ' ' + t.type + ' ' + tradeType(t.type).description + ' ' + cpty(t.cp).name).toLowerCase().includes(s));
    }
    rows = rows.slice().sort((a, b) => b.updated.localeCompare(a.updated));
    return { rows: rows.slice(q.offset, q.offset + q.limit), total: rows.length };
}
function requestLine(q, total) {
    const f = {};
    if (q.search) f[SERVER === 'today' ? 'id_one_of' : 'search'] = q.search;
    if (q.state) f.state = q.state; if (q.book) f.book = q.book; if (q.cp) f.counterparty = q.cp;
    return 'trading.v1.trades.list  offset=' + q.offset + ' limit=' + q.limit + ' filter=' + JSON.stringify(f) + '  →  total=' + total;
}
const stateChip = s => chip(s, { live: 'ok', draft: 'warn', expired: '', cancelled: 'bad' }[s]);
function authChip(t) { return t.state !== 'live' ? null : t.auth ? chip('authorised', 'ok', 'Operations released it. This is metadata, not a term of the trade.') : chip('unauthorised', 'warn', 'Operations has not released it yet.'); }
const tradeHref = (code, tab) => qs({ trade: code, tab: tab || null, state: null });

/* ---------- the list ---------- */
function renderList() {
    const q = listQuery;
    const head = pageHead({
        title: 'Trades', lead: 'The trades of ' + PARTY + ' and the books it holds.',
        actions: [refreshButton({ stale, onClick: refresh }), workflowMenu([
            { title: 'Book a trade', line: 'Record a new deal, live or as a draft.', href: '../book-a-trade/', allowed: !!CAN.book },
            { title: 'Import a portfolio from ORE', line: 'Bring an ORE portfolio file into a book.', href: '../ore-portfolio/?mode=import', allowed: !!CAN.import },
            { title: 'Export a portfolio to ORE', line: 'Write trades out as the portfolio the engine reads.', href: '../ore-portfolio/?mode=export' + (selected.size ? '&trades=' + [...selected].join(',') : ''), allowed: !!CAN.export },
            { title: 'Group trades into a structure', line: selected.size >= 2 ? 'Hold the ' + selected.size + ' chosen trades as one deal.' : 'Choose two or more trades first.', run: () => selected.size >= 2 ? location.href = '../structure-page/?mode=group&trades=' + [...selected].join(',') : toast('Choose two or more trades in the list first.'), allowed: !!CAN.structure }
        ])]
    });
    const filters = SERVER === 'today'
        ? h('div', { class: 'filters' }, h('input', { placeholder: 'Trade id', value: q.search, oninput: debounce(e => { q.search = e.target.value; q.offset = 0; renderList(); }) }),
            h('span', { class: 'faint small' }, 'Today the server filters by trade id only.'))
        : h('div', { class: 'filters' },
            h('input', { placeholder: 'Search by trade id, type or counterparty', value: q.search, 'aria-label': 'Search', oninput: debounce(e => { q.search = e.target.value; q.offset = 0; renderList(); }) }),
            select([['', 'All states'], ['live', 'Live'], ['draft', 'Draft'], ['expired', 'Expired'], ['cancelled', 'Cancelled']], q.state, v => { q.state = v; q.offset = 0; renderList(); }),
            select([['', 'All books']].concat(BOOKS.map(b => [b.code, b.name])), q.book, v => { q.book = v; q.offset = 0; renderList(); }),
            select([['', 'All counterparties']].concat(COUNTERPARTIES.map(c => [c.code, c.name])), q.cp, v => { q.cp = v; q.offset = 0; renderList(); }));
    if (STATE_P === 'loading') return put(wrap(head, filters, h('div', { class: 'notice' }, 'Loading…')));
    if (STATE_P === 'error') return put(wrap(head, h('div', { class: 'notice bad' }, h('b', null, 'The trades could not be read. '), 'trading.v1.trades.list answered 503 (request 4f1c9a02). ', h('button', { class: 'btn small', onclick: () => location.href = qs({ state: null }) }, 'Retry'))));
    const { rows, total } = STATE_P === 'empty' ? { rows: [], total: 0 } : listTrades(q);
    const sel = CAN.structure || CAN.export;
    const table = h('div', { class: 'tablewrap' }, h('table', { class: 'grid' },
        h('thead', null, h('tr', null, sel ? h('th', { class: 'sel' }) : null, ['Trade', 'Type', 'Counterparty', 'Book', 'State', 'Trade date'].map(c => h('th', null, c)), h('th', { class: 'num' }, 'Notional'), h('th', { class: 'num' }, 'Version'))),
        h('tbody', null, rows.length ? rows.map(t => h('tr', { class: stale && t.code === 'ACME-100202' ? 'row-news' : '' },
            sel ? h('td', { class: 'sel' }, h('input', { type: 'checkbox', checked: selected.has(t.code), 'aria-label': 'Choose ' + t.code, onchange: e => { e.target.checked ? selected.add(t.code) : selected.delete(t.code); renderList(); } })) : null,
            h('td', { class: 'nw' }, h('a', { href: tradeHref(t.code) }, t.code)), h('td', null, tradeType(t.type).description),
            h('td', null, h('a', { href: '#', 'data-ref': 'counterparty:' + t.cp }, cpty(t.cp).name)), h('td', null, h('a', { href: '#', 'data-ref': 'book:' + t.book }, book(t.book).name)),
            h('td', { class: 'nw' }, stateChip(t.state), ' ', authChip(t)), h('td', { class: 'nw' }, t.date), h('td', { class: 'num nw' }, money(t.notional, t.ccy)),
            h('td', { class: 'num mono nw' }, 'v' + t.ver + ' · ext ' + t.ext))) :
            h('tr', null, h('td', { colspan: 9 }, STATE_P === 'empty' || q.search || q.state || q.book || q.cp ? ['No trade matches. ', h('button', { class: 'btn small', onclick: () => { listQuery = { offset: 0, limit: q.limit, search: '', state: '', book: '', cp: '' }; location.href = qs({ state: null }); } }, 'Clear filters')] : 'There are no trades yet.')))));
    put(wrap(head, selected.size ? h('div', { class: 'notice acc' }, selected.size + ' chosen. ', h('button', { class: 'btn small', onclick: () => { selected.clear(); renderList(); } }, 'Clear')) : null,
        stale ? h('div', { class: 'strip' }, 'Something changed that this page cannot show. Press Refresh to read it again.') : null,
        filters, table, pager({ offset: q.offset, limit: q.limit, total, onChange: v => { Object.assign(q, v); renderList(); } }),
        h('div', { class: 'reqline' }, requestLine(q, total))));
}
function debounce(fn) { let t; return e => { clearTimeout(t); t = setTimeout(() => fn(e), 300); }; }

/* ---------- applying writes after Refresh ---------- */
function refresh() {
    LOADED = NOW; stale = false;
    for (const w of PENDING.splice(0)) w();
    if (CODE) renderPage(); else renderList();
}

/* ---------- the page ---------- */
function applyOverrides(t, cards) {
    const ov = OV[t.code] || {};
    cards.forEach(c => { if (c.fields) c.fields.forEach(fd => { if (ov[c.id] && fd.key in ov[c.id]) fd.value = ov[c.id][fd.key]; }); });
    return cards;
}
function renderPage() {
    const t = trade(CODE);
    if (!t) return put(wrap(h('div', { class: 'notice bad' }, 'No trade ' + CODE + '. It may have been removed, or you may not see its party.')));
    const tt = tradeType(t.type);
    const head = pageHead({
        title: tt.description, id: t.id,
        lead: null,
        actions: [CAN.authorise && t.state === 'live' ? h('button', { class: 'btn', onclick: () => authorise(t) }, t.auth ? 'Withdraw authorisation' : 'Authorise') : null,
            refreshButton({ stale, onClick: refresh }),
            workflowMenu([
                { title: 'Record a lifecycle event', line: 'Exercise, terminate, cancel, novate or move book.', run: () => lifecycle(t), allowed: !!CAN.event },
                { title: 'Group into a structure', line: 'Hold this trade with others as one deal.', href: '../structure-page/?mode=group&trades=' + t.code, allowed: !!CAN.structure && !t.structure },
                { title: 'Book a similar trade', line: 'Start a booking from this trade\'s terms.', href: '../book-a-trade/?like=' + t.code, allowed: !!CAN.book },
                { title: 'Export to ORE', line: 'Write this trade out as an ORE portfolio.', href: '../ore-portfolio/?mode=export&trades=' + t.code, allowed: !!CAN.export }
            ])]
    });
    const sub = h('div', { class: 'filters', style: 'margin-top:-.25rem' }, h('span', { class: 'chip' }, t.code), stateChip(t.state), authChip(t),
        chip('v' + t.ver, 'v', 'Internal version: moves on every recorded change'), chip('external v' + t.ext, 'v', 'External version: moves only when the agreed terms change'),
        t.structure ? h('a', { class: 'chip acc', href: '../structure-page/?structure=' + t.structure }, 'in ' + t.structure) : null);
    const content = TAB === 'instrument' ? instrumentTab(t) : TAB === 'identifiers' ? identifiersTab(t) : TAB === 'links' ? linksTab(t) : TAB === 'history' ? historyTab(t) : detailsTab(t);
    put(wrap(head, sub, stale ? h('div', { class: 'strip' }, 'This trade changed since the page was read. Press Refresh to read it again.') : null,
        tabs(TABS, TABS.some(x => x[0] === TAB) ? TAB : 'details', id => tradeHref(t.code, id === 'details' ? null : id)), content));
}
const canEdit = t => CAN.amend && (t.state === 'live' || t.state === 'draft');
function detailsTab(t) {
    const f = (label, value, why) => ({ label, value, fixedWhy: why, type: 'text' });
    const fixed = 'Fixed for the life of the trade. A change is a cancel and rebook.';
    const identity = { id: 'identity', title: 'Identity and classification', table: 'trade', version: 1, fields: [
        f('Trade type', tradeType(t.type).description + ' (' + t.type + ')'), f('Party', PARTY, fixed), f('Counterparty', cpty(t.cp).name, fixed),
        f('Booking nature', t.nature), f('Entry channel', t.channel), f('Counterparty scope', t.scope)] };
    const booking = { id: 'booking', title: 'Booking', table: 'trade_booking', version: t.ver, fields: [
        { key: 'trade_date', label: 'Trade date', value: t.date, type: 'date' }, { key: 'execution_time', label: 'Execution time', value: t.date + ' 10:42', type: 'text' },
        { key: 'netting_set', label: 'Netting set', value: 'NWCAP-CSA-1', type: 'text' }, f('Book', book(t.book).name), f('State', t.state), f('Operational authorisation', t.state !== 'live' ? '' : t.auth ? 'Authorised' : 'Unauthorised')] };
    const idCard = card(identity, { readOnlyNote: true });
    const cp = idCard.querySelector('dl div:nth-child(3) dd');
    if (cp) { cp.replaceChildren(h('a', { href: '#', 'data-ref': 'counterparty:' + t.cp }, cpty(t.cp).name), h('div', { class: 'why' }, fixed)); }
    return h('div', { class: 'cards' }, idCard, card(applyOverrides(t, [booking])[0], { edit: canEdit(t) ? c => openAmend(t, c) : null }));
}
function instrumentTab(t) {
    if (!t.table) return h('div', { class: 'notice warn' }, h('b', null, tradeType(t.type).code + ' has no instrument table yet. '), 'The trade type is in the catalogue, but nothing routes it to a table, so its terms cannot be held.');
    return h('div', { class: 'cards' }, applyOverrides(t, instrumentCards(t)).map(c => card(c, { edit: canEdit(t) ? c2 => openAmend(t, c2) : null })));
}
function identifiersTab(t) {
    const o = otherCards(t);
    return h('div', { class: 'cards' }, ['identifiers', 'roles', 'portfolios', 'additional'].map(k => card(o[k], { edit: canEdit(t) ? c => openAmend(t, c) : null })));
}
function linksTab(t) {
    const cards = [];
    cards.push(t.structure
        ? h('section', { class: 'card' }, h('header', null, h('h2', null, 'Structure'), h('span', { class: 'sp' })), h('div', { class: 'body' }, h('a', { href: '../structure-page/?structure=' + t.structure }, t.structure), ' — Strategy, 2 members. This trade is the receiver leg.'))
        : h('section', { class: 'card' }, h('header', null, h('h2', null, 'Structure')), h('div', { class: 'body' }, 'Not part of a structure. ', CAN.structure ? h('a', { href: '../structure-page/?mode=group&trades=' + t.code }, 'Group into a structure') : null)));
    const links = [];
    if (t.rebookedTo) links.push([t.rebookKind || 'Novation', 'transferred → new trade', t.rebookedTo]);
    if (t.rebookedFrom) links.push([(trade(t.rebookedFrom) || {}).rebookKind || 'Novation', 'new trade ← transferred', t.rebookedFrom]);
    cards.push({ id: 'links', title: 'Linked trades', table: 'trade_link', version: 1, child: true, columns: ['Link type', 'Direction', 'Trade'], rows: links.map(l => [l[0], l[1], h('a', { href: tradeHref(l[2]) }, l[2])]) });
    return h('div', { class: 'cards' }, cards.map(c => c.nodeType ? c : card(c)));
}
function historyTab(t) {
    const entries = historyOf(t).concat(EXTRA[t.code] || []).sort((a, b) => a.at.localeCompare(b.at));
    entries.forEach(e => e.current = false);
    for (let i = entries.length - 1; i >= 0; i--) if (!entries[i].noVersion) { entries[i].current = true; break; }
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
    const cur = rowsMode ? c.rows.map(r => r.map(x => x.nodeType ? x.textContent : x)) : c.fields.map(fd => fd.value);
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
        const allowed = fd => act === 'null_amend' ? false : act === 'misbooking_correction' ? true : a.economic ? !!fd.economic : !fd.economic;
        const reasons = REASON_CODES.filter(r => diff.length ? r[0] !== 'trade.no_change' : r[0] === 'trade.no_change');
        if (!reasons.find(r => r[0] === reason)) reason = reasons[0][0];
        fill(body,
            STATE_P === 'conflict' ? h('div', { class: 'notice warn' }, h('b', null, 'Priya Natarajan saved v' + (t.ver + 1) + ' while you were editing. '), 'fixed_rate moved from 2.8500 % to 2.9000 %. ',
                h('button', { class: 'btn small', onclick: () => toast('Your edit now sits on top of v' + (t.ver + 1) + '.') }, 'Keep my edit'), ' ', h('button', { class: 'btn small', onclick: () => toast('The form now holds the new values.') }, 'Take the new values')) : null,
            field('What kind of change is this?', select(allowedActivities(c).map(code => [code, activityType(code).label]), act, v => { act = v; if (v === 'null_amend') draft = JSON.parse(JSON.stringify(cur)); draw(); }),
                'The kind of change decides which fields you may edit.'),
            h('div', { class: 'filters', style: 'margin:0' }, a.economic ? chip('economic', 'acc') : chip('not economic'), a.confirmable ? chip('confirmable', '', 'A new confirmation is issued') : chip('not confirmable'), a.real ? chip('real') : chip('null', 'warn', 'Recorded, but nothing is versioned')),
            rowsMode
                ? h('table', { class: 'grid' }, h('thead', null, h('tr', null, c.columns.map(x => h('th', null, x)))), h('tbody', null, draft.map((r, ri) => h('tr', null, r.map((v, ci) =>
                    h('td', null, h('input', { class: 'field-input', value: v, disabled: act === 'null_amend' || (!a.economic && c.economic && act !== 'misbooking_correction'), oninput: e => { draft[ri][ci] = e.target.value; draw2(); } })))))))
                : h('div', { class: 'grid2' }, c.fields.map((fd, i) => field(fd.label + (fd.economic ? ' · economic' : ''),
                    h('input', { class: 'field-input', value: draft[i], disabled: !allowed(fd), oninput: e => { draft[i] = e.target.value; draw2(); } }),
                    !allowed(fd) && act !== 'null_amend' ? 'Not part of this kind of change.' : ''))),
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
            if (!rowsMode) { ov[c.id] = ov[c.id] || {}; c.fields.forEach((fd, i) => { ov[c.id][fd.key] = draft[i]; }); }
            if (a.real) { t.ver += 1; if (folded) t.ext += 1; }
            const e = { id: 'x' + Date.now(), at: iso(NOW), kind: 'amended', activity: act, version: t.ver, ext: t.ext, actor: ME, reason, note,
                economic: folded, diffs: diff.map(([fd, o, v]) => [fd.label || 'rows', String(o), String(v)]), noVersion: !a.real };
            if (a.real && t.auth) { t.auth = false; }
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
        if (t.auth) lines.push('Operational authorisation is withdrawn. Operations must release the trade again. This moves neither version.');
    }
    return h('div', { class: 'notice ' + (econChange && c.digestGap ? 'warn' : 'acc'), 'data-effect': true }, h('b', null, 'What this will do'), lines.map(l => h('div', { class: 'econ-line' + (folded ? ' moves' : '') }, l)));
}
function authorise(t) {
    PENDING.push(() => {
        t.auth = !t.auth;
        const e = { id: 'x' + Date.now(), at: iso(NOW), kind: 'event', activity: 'authorisation', version: t.ver, ext: t.ext, actor: ME, reason: 'trade.no_change', note: t.auth ? 'Released by Operations' : 'Release withdrawn', diffs: [], noVersion: true };
        (EXTRA[t.code] = EXTRA[t.code] || []).push(e); flashId = e.id;
    });
    stale = true; renderPage();
    toast('Done. Neither version moves: authorisation is our workflow state, not a term of the trade.');
}

/* ---------- the lifecycle workflow ---------- */
function lifecycleEvents(t) {
    const hasOptions = /Option|Swaption|CapFloor/.test(t.type);
    if (t.state === 'draft') return [['execute', 'Execute', 'The agreement is struck. The draft goes live.', 'new_booking'], ['discard', 'Discard', 'The draft comes to nothing.', 'cancellation']];
    if (t.state === 'expired' || t.state === 'cancelled') return [['reinstate', 'Reinstate', 'The ' + (t.state === 'expired' ? 'expiry' : 'cancellation') + ' was recorded in error.', 'reinstatement']];
    return [hasOptions ? ['exercise', 'Exercise', 'The holder exercises the option.', 'exercise'] : null, ['partial', 'Partial termination', 'Part of the notional is closed out.', 'partial_termination'],
        ['expire', 'Expiry', 'The contract ceases naturally.', 'expiry'], ['cancel', 'Cancel', 'Remove the trade from the book. The record stays.', 'cancellation'],
        ['novate', 'Novation', 'The counterparty changes. Cancel and rebook, joined by a link.', 'novation'], ['bookmove', 'Book move', 'Move to a book of another legal entity. Cancel and rebook.', 'novation']].filter(Boolean);
}
function lifecycle(t) {
    const evs = lifecycleEvents(t);
    let step = 0, ev = evs[0], when_ = '2026-10-11', amount = '', newCp = COUNTERPARTIES.find(c => c.open && c.code !== t.cp).code, newBook = 'EMEA-RATES', reason = 'trade.client_request', note = '', done = null;
    const body = h('div');
    const STEPS = ['Event', 'Facts', 'Effect', 'Record'];
    const next = h('button', { class: 'btn primary' }, 'Next');
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
    }
    const d = openDialog({ title: 'Record a lifecycle event', loadedAt: LOADED, body, buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), next] });
    next.addEventListener('click', () => {
        if (done) return d.close();
        if (step < 3) { step++; return draw(); }
        const links = [];
        PENDING.push(() => {
            const a = activityType(ev[3]);
            t.state = { execute: 'live', discard: 'cancelled', reinstate: 'live', cancel: 'cancelled', expire: 'expired', novate: 'cancelled', bookmove: 'cancelled' }[ev[0]] || t.state;
            if (a.real) t.ver += 1; if (a.economic) t.ext += 1;
            if (t.state !== 'live') t.auth = null; else if (t.auth === null) t.auth = false;
            const e = { id: 'x' + Date.now(), at: iso(NOW), kind: t.state === 'cancelled' ? 'cancelled' : 'event', activity: ev[3], version: t.ver, ext: t.ext, actor: ME, reason, note: note || ev[2], economic: a.economic, diffs: [['state', 'live', t.state]] };
            (EXTRA[t.code] = EXTRA[t.code] || []).push(e); flashId = e.id;
            if (['novate', 'bookmove'].includes(ev[0])) {
                const n = Object.assign({}, t, { id: tradeId(300 + TRADES.length), code: 'ACME-100' + (300 + TRADES.length), cp: ev[0] === 'novate' ? newCp : t.cp, book: ev[0] === 'bookmove' ? newBook : t.book,
                    state: 'live', auth: false, ext: 1, ver: 1, structure: null, rebookedTo: null, rebookedFrom: t.code, updated: iso(NOW) });
                t.rebookedTo = n.code; t.rebookKind = ev[0] === 'novate' ? 'Novation' : 'Book move'; n.rebookKind = t.rebookKind;
                TRADES.push(n); store('booked', (store('booked') || []).concat([n]));
            }
        });
        stale = true; done = ['novate', 'bookmove'].includes(ev[0]) ? ['The new trade is ACME-100' + (300 + TRADES.length) + '.'] : [];
        draw(); renderPage();
    });
    draw();
}

if (CODE) renderPage(); else renderList();

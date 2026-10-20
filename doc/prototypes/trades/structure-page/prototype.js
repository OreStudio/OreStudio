'use strict';
/* The page of a structure, and the workflow that makes one. Serves: Group trades
 * into a structure.
 *
 * A package is not a different entity. It is the bottom rung of the composition
 * ladder: a structure of kind Package. Same table, same anchor, same page. What the
 * kind changes is what the customer sees. A strategy, a typed structure and a dynamic
 * structure are one deal: confirmed as one document and priced as a whole. A package is
 * a set of trades the customer holds under their own terms: each leg confirms on its
 * own and the price is the sum of the parts. So the screen is one, and its words follow
 * the kind: a package says package, a strategy says structure. */

const MODE = P.get('mode') === 'group' ? 'group' : '';
let TAB = P.get('tab') || 'details';
const STATE_P = P.get('state') || '';
const REASONS = reasonsFor('amend');
// The writes the screen has not read back yet.
const PENDING = [];

const KIND = STRUCTURE_KINDS;
const TEMPLATES = STRUCTURE_TEMPLATES;
const noun = k => k === 'Package' ? 'package' : 'structure';
const Noun = k => noun(k).replace(/^./, c => c.toUpperCase());
const confirmText = k => KIND[k].whole ? 'Confirmed as one document' : 'Each leg confirms on its own';
const pricingText = k => KIND[k].whole ? 'Priced as a whole' : 'Priced leg by leg';

/* A structure is a tree. The root is the deal the customer sees. Inner nodes are
 * structures, of any kind, and the leaves are trades. The model nests one level at most:
 * parent is the parent_structure_id of the structure table. */
const DEALS = allDeals();
const childrenOf = id => Object.entries(DEALS).filter(([, v]) => v.parent === id);
const rootOf = id => DEALS[id] && DEALS[id].parent ? rootOf(DEALS[id].parent) : id;
let SID = P.get('deal') || '';
let SEL = P.get('trade') || '';
const LIST = !SID && !MODE;
let stale = STATE_P === 'stale';
/* A tab, a structure or a trade opens in place and puts its address in the history, so what the person did on the page stays. */
function go(change, render) {
    history.pushState(null, '', qs(change));
    render();
}
const openDeal = (id, trade) => go({ deal: id, trade: trade || null, tab: null }, () => { SID = id; SEL = trade || ''; TAB = 'details'; renderDeal(); });
const dealLink = id => h('a', { href: qs({ deal: id, trade: null, tab: null }), 'data-text': id, onclick: e => { e.preventDefault(); openDeal(id); } }, id);
function dealCrumbs() {
    const c = DEALS[SID];
    return [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages', qs({ deal: null, tab: null, trade: null })]].concat(c && c.parent ? [[c.parent, qs({ deal: c.parent, tab: null, trade: null })]] : []).concat([[SID]]);
}

const main = shell({
    title: MODE ? 'Group trades' : LIST ? 'Structures and packages' : (DEALS[SID] ? Noun(DEALS[SID].kind) : 'Structure') + ' ' + SID, name: 'structure-page',
    question: 'How are several trades held together, as one deal or as a package?',
    crumbs: MODE ? [['Home', '#'], ['Trades', '../trade-page/'], ['Group trades']] : LIST ? [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages']] : dealCrumbs(),
    states: LIST
        ? [['list', { state: null }, !STATE_P], ['stale', { state: 'stale' }, STATE_P === 'stale'], ['loading', { state: 'loading' }, STATE_P === 'loading'], ['empty', { state: 'empty' }, STATE_P === 'empty'], ['error', { state: 'error' }, STATE_P === 'error']]
        : [['list', { mode: null, trades: null, deal: null, tab: null, trade: null, state: null }, false], ['structure tree', { mode: null, trades: null, deal: 'STR-0010', tab: null, trade: null, state: null }, SID === 'STR-0010'], ['strategy', { mode: null, trades: null, deal: 'STR-0007', tab: null, trade: null, state: null }, SID === 'STR-0007'],
            ['package', { mode: null, trades: null, deal: 'PKG-0021', tab: null, trade: null, state: null }, SID === 'PKG-0021'], ['typed structure', { mode: null, trades: null, deal: 'STR-0009', tab: null, trade: null, state: null }, SID === 'STR-0009'], ['package alone', { mode: null, trades: null, deal: 'PKG-0004', tab: null, trade: null, state: null }, SID === 'PKG-0004'],
            ['group', { mode: 'group', deal: null, trades: 'ACME-100202,ACME-100209', state: null }, MODE === 'group'], ['mismatch', { mode: 'group', deal: null, trades: 'ACME-100201,ACME-100202' }, false],
            ['loading', { state: 'loading' }, STATE_P === 'loading'], ['error', { state: 'error' }, STATE_P === 'error'], ['refused', { state: 'refused', tab: 'history' }, STATE_P === 'refused']],
    says: LIST ? 'A structure is a tree. The root is the deal the customer sees. Inside it are strategies, packages and plain trades. The list shows the trees; open the root to see the whole tree.' : MODE
        ? 'One workflow, and the kind decides what the customer receives. A strategy is one deal. A package is trades the customer holds one by one. The screen states each kind\'s consequences and the rules the server will enforce, before it writes.'
        : 'A package is a kind of structure, so there is one screen and its words follow the kind. Compare the strategy and the package: how it confirms, how it prices, where the external version lives, what unlinking a leg means.'
});
const wrap = (...kids) => h('div', { class: 'dyn' }, kids);
function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); main.append(el); }
const kindRule = s => s.template ? TEMPLATES[s.kind][s.template] : null;
const tmem = s => s.members.map(([c, r]) => ({ t: trade(c), role: r })).filter(x => x.t);

/* ---------- the page ---------- */
function refresh() {
    LOADED = NOW; stale = false;
    if (marketNewer) { Object.assign(MARKET, { spot: 1.0862, vol: 8.4, rate: 3.08, readAt: NOW }); marketNewer = false; }
    for (const w of PENDING.splice(0)) w();
    renderDeal();
}
function renderDeal() {
    const s = DEALS[SID];
    if (!s) return put(wrap(h('div', { class: 'notice bad' }, 'No structure or package ' + SID + '. It may have been removed, or you may not see its party.')));
    setCrumbs(dealCrumbs(), title(s) + ' ' + SID);
    if (STATE_P === 'loading') return put(wrap(h('div', { class: 'notice' }, 'Loading…')));
    if (STATE_P === 'error') return put(wrap(failure('trading.v1.structures.get', 503, 'unavailable', 'b52e07d4a911', 'The structure could not be read.', h('button', { class: 'btn small', onclick: () => { location.href = qs({ state: null }); } }, 'Retry'))));
    const N = noun(s.kind), pk = s.kind === 'Package';
    const ms = tmem(s);
    const ttl = title(s);
    const roleCheck = kindRule(s) ? kindRule(s).map(([role, lo, hi]) => ({ role, lo, hi, n: s.members.filter(m => m[1] === role).length })) : [];
    const ok = roleCheck.every(r => r.n >= r.lo && (!r.hi || r.n <= r.hi));
    const head = pageHead({
        title: ttl, id: SID, meta: [chip(s.kind, 'acc'), s.template ? chip(s.template) : null, s.template ? (ok ? chip('complete', 'ok') : chip('legs missing', 'warn')) : null],
        actions: [refreshButton({ stale, onClick: refresh }),
            workflowMenu([{ title: 'Book a leg', line: 'Book a trade of ' + cpty(s.cp).name + ' and add it to this ' + N + '.', href: '../book-a-trade/?structure=' + SID + (openRoles(s).length ? '&role=' + openRoles(s)[0] : ''), allowed: !!CAN.book && !!CAN.structure && (!kindRule(s) || openRoles(s).length > 0) }])]
    });
    let content;
    if (TAB === 'members') content = membersCard(s, ms, roleCheck);
    else if (TAB === 'price') content = priceCard(s, ms);
    else if (TAB === 'history') content = STATE_P === 'refused' ? h('div', { class: 'notice' }, h('b', null, 'You may not read the history of this ' + N + '. '), 'Ask the tenant administrator for a role that reads trade history.') : timeline(historyEntries(s, ms), { isService: false });
    else content = h('div', { class: 'cards' }, card({ id: 'identity', title: 'Identity', table: 'structure', version: 1, fields: [
        { label: 'Party', value: PARTY, node: ref('party', PARTY), fixedWhy: 'Shared by every leg, for the life of the ' + (pk ? 'package' : 'deal') + '.', type: 'text' },
        { label: 'Counterparty', value: cpty(s.cp).name, node: ref('counterparty', s.cp, cpty(s.cp).name), fixedWhy: 'Shared by every leg, for the life of the ' + (pk ? 'package' : 'deal') + '.', type: 'text' },
        { label: 'Kind', value: s.kind, fixedWhy: KIND[s.kind].desc, type: 'text' },
        pk ? null : { label: 'Template', value: s.template, node: s.template ? ref('structure_template', s.template) : null, type: 'text' },
        { label: 'Inside', value: s.parent, node: s.parent ? dealLink(s.parent) : null, fixedWhy: s.parent ? 'The ' + N + ' is a node of the tree of ' + s.parent + '.' : null, type: 'text' }].filter(Boolean) }, { readOnlyNote: true }),
        card({ id: 'how', title: 'How it confirms and prices', table: 'structure_kind', version: 1, fields: [
            { label: 'Confirmation', value: confirmText(s.kind), type: 'text', fixedWhy: pk ? 'The customer holds each trade under its own terms, even though they were executed together.' : 'The customer holds one deal. A partial close-out gives one confirmation for the remainder.' },
            { label: 'Pricing', value: pricingText(s.kind), type: 'text', fixedWhy: pk ? 'No leg depends on another, so the price is the sum of the parts.' : 'The legs interact, so the deal has one price.' },
            { label: 'External version', value: pk ? 'Of each trade' : 'Of the deal', type: 'text', fixedWhy: pk ? 'Each leg moves on its own: see the Members tab.' : 'Moves when the economics of a leg change. The structure table has no column for it yet.' }] }, { readOnlyNote: true }),
        membersCard(s, ms, roleCheck));
    const list = [['details', 'Details'], ['members', 'Members'], ['price', 'Price'], ['history', 'History']];
    const right = SEL ? tradePanel(SEL) : h('div', null, tabs(list, TAB, id => qs({ tab: id === 'details' ? null : id }), id => go({ tab: id === 'details' ? null : id }, () => { TAB = id; renderDeal(); })), content);
    put(wrap(head, h('div', { class: 'workspace' }, treePanel(), right)));
}
window.addEventListener('popstate', () => {
    if (!SID) return;
    const p = new URLSearchParams(location.search);
    SID = p.get('deal') || SID; SEL = p.get('trade') || ''; TAB = p.get('tab') || 'details';
    renderDeal();
});
/* The tree is its own panel, beside the page: the root, its inner structures, and the trades, one row each. */
const title = (v) => v.kind === 'Package' ? 'Package' : (v.template ? v.template.replace(/([A-Z])/g, ' $1').trim() : v.kind + ' structure');
const productTag = t => ({ swap: 'Rates', fx: 'FX', credit: 'Credit', bond: 'Bond', equity: 'Equity', composite: 'Composite' }[t.product] || t.product);
function treeRow(depth, selected, onclick, kids) {
    return h('div', { class: 'node-row' + (selected ? ' selected' : ''), role: 'treeitem', tabindex: 0, onclick, onkeydown: e => { if (e.key === 'Enter') onclick(); } }, kids);
}
function treeNode(id, depth) {
    const v = DEALS[id], legs = tmem(v), kids = childrenOf(id);
    const here = id === SID && !SEL;
    const row = treeRow(depth, here, () => openDeal(id), [
        h('span', { class: 'twisty' }, '▾'),
        depth ? h('span', { class: 'type-tag' }, v.kind) : null,
        h('span', { class: 'label' }, h('span', { class: 'name' }, title(v)), h('span', { class: 'meta' }, id + ' · ' + (kids.length ? kids.length + ' inner · ' : '') + legs.length + ' trade' + (legs.length === 1 ? '' : 's'))),
        depth ? h('span', { class: 'num' }, KIND[v.kind].whole ? 'one document' : 'by trade') : chip(v.kind, 'acc')]);
    const children = h('div', { class: 'group-children' }, kids.map(([cid]) => treeNode(cid, depth + 1)),
        legs.map(m => h('div', { class: 'node depth-2' }, treeRow(2, SEL === m.t.code, () => openDeal(id, m.t.code), [
            h('span', { class: 'twisty' }),
            h('span', { class: 'type-tag' }, productTag(m.t)),
            h('span', { class: 'label' }, h('span', { class: 'name' }, tradeType(m.t.type).description), h('span', { class: 'meta' }, m.t.code + ' · ' + m.role)),
            h('span', { class: 'num' }, moneyRef(m.t.notional, m.t.ccy))]))));
    return h('div', { class: 'node depth-' + depth }, row, children);
}
function treePanel() {
    const root = rootOf(SID), rv = DEALS[root];
    const inner = childrenOf(root).length;
    return h('section', { class: 'panel' }, h('header', null, h('h2', null, 'Structure'), h('span', { class: 'legend' }, inner + ' inner structure' + (inner === 1 ? '' : 's') + ' · ' + 'root ' + root)),
        h('div', { class: 'tree', role: 'tree', 'aria-label': 'The structure tree' }, treeNode(root, 0)),
        inner && rv.kind !== 'Package' ? h('div', { class: 'tree-note' }, h('div', { class: 'notice warn' }, 'The root confirms as one document. How a package inside it confirms is not decided. It is filed as a capture.')) : null);
}
function tradePanel(code) {
    const t = trade(code), role = (tmem(DEALS[SID]).find(m => m.t.code === code) || {}).role;
    if (!t) return h('div', { class: 'notice bad' }, 'No trade ' + code + '.');
    const pk = DEALS[SID].kind === 'Package';
    return h('div', { class: 'cards' }, card({ id: 'leg', title: 'Trade in ' + SID, table: 'structure_member', version: 1, fields: [
        { label: 'Trade', value: t.code, node: tradeLink(t.code), type: 'text' }, { label: 'Type', value: tradeType(t.type).description, node: ref('trade_type', t.type, tradeType(t.type).description), type: 'text' }, { label: 'Role', value: role, type: 'text' },
        { label: 'State', value: t.state, node: chip(t.state, { live: 'ok', draft: 'warn', cancelled: 'bad' }[t.state]), type: 'text' }, { label: 'Counterparty', value: cpty(t.cp).name, node: ref('counterparty', t.cp, cpty(t.cp).name), type: 'text' }, { label: 'Notional', value: t.notional, node: moneyRef(t.notional, t.ccy), type: 'money' },
        { label: 'External version', value: 'v' + t.ext, type: 'text', fixedWhy: pk ? 'This trade confirms on its own, so this is the version the customer quotes.' : 'The deal carries the confirmation. This trade has its own version too.' }] }, { readOnlyNote: true }),
        h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/?trade=' + t.code }, 'Open the trade'), h('a', { class: 'btn', href: qs({ trade: null }), onclick: e => { e.preventDefault(); go({ trade: null }, () => { SEL = ''; renderDeal(); }); } }, 'Back to the ' + noun(DEALS[SID].kind))));
}
function membersCard(s, ms, roleCheck) {
    const pk = s.kind === 'Package';
    const cols = [pk ? 'Leg' : 'Role', 'Trade', 'Type', 'State', 'Notional'].concat(pk ? ['External version'] : []).concat(['']);
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trades'), chip(ms.length + ' legs', 'v'), h('span', { class: 'sp' }), CAN.structure ? [CAN.book && (!kindRule(s) || openRoles(s).length) ? h('a', { class: 'btn small', href: '../book-a-trade/?structure=' + SID + (openRoles(s).length ? '&role=' + openRoles(s)[0] : '') }, 'Book a leg') : null, ' ', h('button', { class: 'btn small', onclick: () => linkDialog(s) }, 'Link a trade')] : h('span', { class: 'access' }, 'Read only')),
        h('div', { class: 'body' }, roleCheck.length ? h('div', { class: 'filters', style: 'margin-top:0' }, roleCheck.map(r => chip(r.role + ' ' + r.n + (r.hi ? ' of ' + r.lo + (r.hi !== r.lo ? '–' + r.hi : '') : ''), r.n >= r.lo && (!r.hi || r.n <= r.hi) ? 'ok' : 'warn'))) : null,
            legChecks(s, ms).map(([okc, text]) => h('div', { class: 'notice ' + (okc ? 'ok' : 'bad'), style: 'margin-bottom:.4rem' }, (okc ? '✓ ' : '✗ ') + text)),
            pk ? h('div', { class: 'notice', style: 'margin-bottom:.6rem' }, 'A package confirms trade by trade. Each leg has its own external version, and the customer quotes the one for the trade they hold.') : null,
            h('table', { class: 'grid' }, h('thead', null, h('tr', null, cols.map(c => h('th', null, c)))),
                h('tbody', null, ms.map((m, i) => h('tr', null, h('td', null, pk ? String(i + 1) : m.role), h('td', { class: 'nw' }, tradeLink(m.t.code)), h('td', null, ref('trade_type', m.t.type, tradeType(m.t.type).description)), h('td', null, chip(m.t.state, { live: 'ok', draft: 'warn', cancelled: 'bad' }[m.t.state])),
                    h('td', { class: 'num nw' }, moneyRef(m.t.notional, m.t.ccy)), pk ? h('td', { class: 'mono' }, 'ext ' + m.t.ext) : null, h('td', null, CAN.structure ? h('button', { class: 'btn small ghost', onclick: () => unlink(s, m) }, 'Unlink') : null)))))));
}
/* What the template of a structure asks of its legs, read from the legs' instruments. */
function legFacts(t) {
    const cards = t.table ? instrumentCards(t) : [];
    const card0 = cards.find(c => c.id === 'instrument');
    const get = key => { const fd = card0 && card0.fields && card0.fields.find(x => x.key === key); return fd ? String(fd.value) : ''; };
    const px = cards.find(c => c.id === 'exercise_prices');
    const strike = px && px.rows && px.rows[0] ? String(px.rows[0][2]) : '';
    return { optionType: get('option_type'), strike, expiry: get('expiry_date'), type: t.type, product: t.product };
}
function legChecks(s, ms) {
    const out = [];
    if (s.kind === 'Strategy' && ms.length > 1) {
        const fam = [...new Set(ms.map(m => m.t.product))];
        out.push([fam.length === 1, fam.length === 1 ? 'Every leg is in one product family (' + fam[0] + ').' : 'A strategy has one product family. These legs are ' + fam.join(' and ') + '.']);
    }
    const facts = ms.map(m => Object.assign({ role: m.role, code: m.t.code }, legFacts(m.t)));
    const two = facts.length === 2;
    const same = key => new Set(facts.map(f => f[key])).size === 1;
    const opposite = two && facts[0].optionType && facts[1].optionType && facts[0].optionType !== facts[1].optionType;
    if (!s.template || !facts.length) return out;
    if (s.template === 'Straddle' && two) out.push([opposite && same('strike') && same('expiry'), 'A straddle is a call and a put at one strike and one expiry.' + (opposite && same('strike') && same('expiry') ? '' : ' The legs differ: ' + facts.map(f => f.code + ' ' + f.optionType + ' ' + f.strike + ' ' + f.expiry).join(', ') + '.')]);
    if (s.template === 'RiskReversal' && two) out.push([opposite && same('expiry'), 'A risk reversal is a call and a put at one expiry.' + (opposite && same('expiry') ? '' : ' The legs differ: ' + facts.map(f => f.code + ' ' + f.optionType + ' ' + f.expiry).join(', ') + '.')]);
    if (s.template === 'CalendarSpread' && two) out.push([same('strike') && !same('expiry'), 'A calendar spread is one option at two expiries.']);
    if (s.template === 'Butterfly' && facts.length === 3) {
        const k = facts.map(f => Number(f.strike)).sort((a, b) => a - b);
        out.push([Math.abs((k[1] - k[0]) - (k[2] - k[1])) < 1e-9, 'The strikes of a butterfly are evenly spaced.']);
    }
    if (s.template === 'CallableSwap' && two) {
        const sw = facts.find(f => f.role === 'swap'), cl = facts.find(f => f.role === 'call');
        out.push([!!sw && !!cl && /Swap$/.test(sw.type) && /Swaption|Callable/.test(cl.type), 'The swap role holds a swap and the call role holds the right to terminate.']);
    }
    return out;
}
/* The market is a snapshot, read at a time. A newer snapshot turns Refresh gold. Nothing ticks. */
const MARKET = { spot: 1.0850, vol: 8.2, rate: 3.10, readAt: LOADED };
const SHIFT = { spot: '0', vol: '0', rate: '0' };
let APPLIED = Object.assign({}, SHIFT), PRICED_AT = LOADED, marketNewer = false;
function indicative(m, shifts) {
    const n = Number(m.t.code.slice(-2)) || 0;
    const sign = ['short', 'swap'].includes(m.role) ? -1 : 1;
    const base = 0.0042 + (((n * 7) % 11) - 5) * 0.0009;
    const move = (Number(shifts.spot) || 0) * 0.009 + (Number(shifts.vol) || 0) * 0.0006 - (Number(shifts.rate) || 0) * 0.00002;
    return sign * m.t.notional * (base + move) * (MARKET.spot / 1.085);
}
function priceCard(s, ms) {
    const pk = s.kind === 'Package';
    const rows = ms.map(m => ({ m, npv: indicative(m, APPLIED) }));
    const total = rows.reduce((a, r) => a + r.npv, 0);
    const shiftField = (key, label, unit) => field(label + ' (' + unit + ')', valueInput({ type: 'decimal' }, SHIFT[key], v => { SHIFT[key] = v; }));
    return h('div', { class: 'cards' },
        h('section', { class: 'card' }, h('header', null, h('h2', null, pk ? 'Price of the legs' : 'Price of the structure'), h('span', { class: 'sp' }), h('span', { class: 'faint small', title: 'The market snapshot' }, 'Market read ' + iso(MARKET.readAt))),
            h('div', { class: 'body', style: 'display:grid;gap:.8rem' },
                h('div', { class: 'notice' }, (pk ? 'A package has no price of its own. The price is the sum of the legs.' : 'The legs interact, so the structure has one price.') + ' The price is indicative and is not the result of a model. The market is a snapshot: it does not tick. Refresh reads a newer one.'),
                h('dl', { class: 'fields' }, [['Spot', MARKET.spot.toFixed(4)], ['Volatility', MARKET.vol.toFixed(1) + ' %'], ['Rate', MARKET.rate.toFixed(2) + ' %']].map(([k, v]) => h('div', null, h('dt', null, k), h('dd', null, v)))),
                h('div', { class: 'grid2' }, shiftField('spot', 'Spot shift', '%'), shiftField('vol', 'Volatility shift', 'points'), shiftField('rate', 'Rate shift', 'basis points')),
                h('div', { class: 'filters', style: 'margin:0' }, h('button', { class: 'btn primary', onclick: () => { APPLIED = Object.assign({}, SHIFT); PRICED_AT = NOW; renderDeal(); } }, 'Reprice'),
                    h('span', { class: 'faint small' }, 'Priced ' + iso(PRICED_AT) + (APPLIED.spot !== '0' || APPLIED.vol !== '0' || APPLIED.rate !== '0' ? ' with the shifts above.' : '.'))),
                h('table', { class: 'grid' }, h('thead', null, h('tr', null, [pk ? 'Leg' : 'Role', 'Trade', 'Type', 'Notional', 'Indicative price'].map((c, i) => h('th', { class: i > 2 ? 'num' : '' }, c)))),
                    h('tbody', null, rows.map(({ m, npv }, i) => h('tr', null, h('td', null, pk ? String(i + 1) : m.role), h('td', { class: 'nw' }, tradeLink(m.t.code)), h('td', null, ref('trade_type', m.t.type, tradeType(m.t.type).description)),
                        h('td', { class: 'num nw' }, moneyRef(m.t.notional, m.t.ccy)), h('td', { class: 'num nw' }, money(npv, m.t.ccy)))),
                        h('tr', null, h('td', { colspan: 4 }, h('b', null, pk ? 'Sum of the legs' : 'Price of the structure')), h('td', { class: 'num nw' }, h('b', null, money(total, ms[0] ? ms[0].t.ccy : '')))))))));
}
function historyEntries(s, ms) {
    const N = noun(s.kind);
    const e = [{ id: 's1', at: s.created, kind: 'booked', activity: 'new_booking', record: 'Structure', version: 1, actor: s.by, reason: 'system.new_record', note: Noun(s.kind) + ' created with ' + s.members.length + ' legs', diffs: [['kind', '', s.kind], ['counterparty', '', cpty(s.cp).name]] }];
    const linked = x => (s.events || []).some(z => z.what && z.what.startsWith(x.t.code + ' linked'));
    ms.filter(m => !linked(m)).forEach((m, i) => e.push({ id: 's' + (i + 2), at: new Date(new Date(s.created).getTime() + (i + 1) * 60000).toISOString().slice(0, 19) + '+01:00', kind: 'event', activity: 'new_booking', record: 'Member', what: m.t.code + ' linked as ' + m.role, version: 1, actor: s.by, reason: 'system.update', note: '', diffs: [], noVersion: true }));
    ms.forEach((m, i) => e.push({ id: 'l' + i, at: '2026-10-10T1' + i + ':15:00+01:00', kind: 'amended', activity: 'amendment', record: 'Leg ' + m.t.code, version: m.t.ver, ext: m.t.ext, actor: 'ada.lovelace', reason: 'system.update', note: 'Read from the leg ' + m.t.code + '. Not a version of the ' + N + '.', diffs: [['notional', '', money(m.t.notional, m.t.ccy)]], economic: true }));
    const all = e.concat((s.events || []).map((z, i) => Object.assign({ id: 'x' + i, kind: 'amended', activity: 'amendment', record: 'Member', version: 1, noVersion: true }, z))).sort((a, b) => a.at.localeCompare(b.at));
    all.forEach(x => { x.current = x.id === 's1'; });
    return all;
}
/* The reason and the commentary of a change, and the request it sends. */
function changeFields(s, call, get) {
    return [h('div', { class: 'grid2' }, field('Reason', select(REASONS, get.reason, v => { get.reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { get.note = e.target.value; } }, get.note))),
        h('div', { class: 'mono small faint' }, 'trading.v1.' + call + '  precondition: members as loaded at ' + iso(LOADED) + '  change intent: reason, commentary')];
}
/* A change to the members is not saved into the page: Refresh gold says so, and Refresh reads it. */
function queueChange(s, apply, entry, message) {
    PENDING.push(() => {
        apply();
        (s.events = s.events || []).push(Object.assign({ at: iso(NOW), activity: 'amendment', actor: ME }, entry));
        store('structures', Object.assign(store('structures') || {}, { [SID]: s }));
    });
    stale = true; renderDeal(); toast(message);
}
function unlink(s, m) {
    const N = noun(s.kind), pk = s.kind === 'Package';
    const st = { reason: 'common.rectification', note: '' };
    const d = openDialog({ title: 'Unlink ' + m.t.code, loadedAt: LOADED,
        body: h('div', { style: 'display:grid;gap:.8rem' }, h('div', { class: 'notice' }, 'The trade stays as it is. It is no longer a leg of ' + SID + ', and can be linked to another ' + N + ' without being rewritten.'),
            pk ? h('div', { class: 'notice acc' }, 'A package binds nothing economic, so the other legs are unaffected and keep their own confirmations.')
                : h('div', { class: 'notice warn' }, 'This deal confirms as one document' + (s.template ? ' and ' + s.template + ' needs this leg' : '') + '. Without it the customer holds a different deal. Unlinking is not an amendment: use a lifecycle event for a partial close-out.'),
            changeFields(s, 'structure_members.put_many', st)),
        buttons: [h('button', { class: 'btn', onclick: () => { if (st.note && !confirm('You have unsaved changes. Close anyway?')) return; d.close(); } }, 'Cancel'),
            h('button', { class: 'btn primary', onclick: () => { d.close(); queueChange(s, () => { s.members = s.members.filter(x => x[0] !== m.t.code); }, { what: m.t.code + ' unlinked', reason: st.reason, note: st.note, diffs: [['member', m.t.code, '']] }, 'Unlinked. Refresh shows the change.'); } }, 'Unlink')] });
}
function linkDialog(s) {
    const pk = s.kind === 'Package';
    const free = TRADES.filter(t => t.cp === s.cp && !s.members.some(m => m[0] === t.code) && !t.structure && t.state !== 'cancelled').slice(0, 8);
    let pick = free[0] && free[0].code, role = s.template ? kindRule(s)[0][0] : 'leg';
    const st = { reason: 'system.update', note: '' };
    const d = openDialog({ title: 'Link a trade to ' + SID, loadedAt: LOADED,
        body: h('div', { style: 'display:grid;gap:.8rem' }, free.length ? field('Trade', select(free.map(t => [t.code, t.code + ' · ' + tradeType(t.type).description]), pick, v => { pick = v; }), 'Only trades of ' + PARTY + ' and ' + cpty(s.cp).name + ' that are not a leg of a structure or a package.') : h('div', { class: 'notice' }, 'No trade of this counterparty is free to link.'),
            pk ? null : field('Role', select(kindRule(s) ? kindRule(s).map(r => r[0]) : ['leg'], role, v => { role = v; }), kindRule(s) ? 'The template allows these roles.' : 'Roles are read from data.'),
            changeFields(s, 'structure_members.put_many', st)),
        buttons: [h('button', { class: 'btn', onclick: () => { if (st.note && !confirm('You have unsaved changes. Close anyway?')) return; d.close(); } }, 'Cancel'),
            h('button', { class: 'btn primary', disabled: !free.length, onclick: () => { const code = pick, r = role; d.close(); queueChange(s, () => { s.members.push([code, r]); }, { what: code + ' linked as ' + r, reason: st.reason, note: st.note, diffs: [['member', '', code + ' as ' + r]] }, 'Linked. Refresh shows the change.'); } }, 'Link')] });
}

/* ---------- grouping ---------- */
const CHOSEN = (P.get('trades') || '').split(',').filter(Boolean).map(trade).filter(Boolean);
const GROUP_LEAD = 'Hold several trades together, as one deal or as a package. The kind decides what the customer receives.';
let G = { step: 0, kind: '', template: '', roles: {}, done: null, reason: 'system.update', note: '' };
function renderGroup() {
    if (!CAN.structure) return put(wrap(pageHead({ title: 'Group trades', lead: GROUP_LEAD }), needsRole('Group trades', 'group trades', ['Trader'])));
    const cps = new Set(CHOSEN.map(t => t.cp)), shared = cps.size === 1;
    const alreadyIn = CHOSEN.filter(t => t.structure);
    const checks = [[CHOSEN.length >= 2, 'Two or more trades are chosen.'],
        [shared, 'Every trade has the same counterparty' + (shared ? ' (' + cpty([...cps][0]).name + ').' : '. These have ' + [...cps].map(c => cpty(c).name).join(' and ') + '.')],
        [!alreadyIn.length, alreadyIn.length ? alreadyIn.map(t => t.code).join(', ') + ' already belong to a structure or a package.' : 'No trade is already a leg of a structure or a package.'],
        [CHOSEN.every(t => t.state !== 'cancelled'), 'No chosen trade is cancelled.']];
    const good = checks.every(c => c[0]);
    const fam = [...new Set(CHOSEN.map(t => t.product))];
    const K = G.kind ? KIND[G.kind] : null;
    const GN = G.kind ? noun(G.kind) : 'structure or package';
    const tpls = TEMPLATES[G.kind] ? Object.keys(TEMPLATES[G.kind]) : [];
    if (K && K.template && !G.template) G.template = tpls[0];
    if (K && !K.template) G.template = '';
    const steps = ['Trades', 'Kind', G.kind === 'Package' ? 'Legs' : 'Roles', 'Confirm'];
    const rail = h('div', { class: 'steps' }, steps.map((x, i) => h('span', { class: 'step' + (i === G.step ? ' on' : i < G.step ? ' done' : '') }, (i + 1) + ' ' + x)));
    const roles = G.template ? TEMPLATES[G.kind][G.template].map(r => r[0]) : ['leg'];
    CHOSEN.forEach((t, i) => { if (!G.roles[t.code]) G.roles[t.code] = roles[i % roles.length]; });
    const familyBad = G.kind === 'Strategy' && fam.length > 1;
    let pane, nav;
    const back = h('button', { class: 'btn', onclick: () => { G.step--; renderGroup(); } }, 'Back');
    const fwd = dis => h('button', { class: 'btn primary', disabled: dis, onclick: () => { G.step++; renderGroup(); } }, 'Continue');
    if (G.done) { pane = h('div', { class: 'notice ok' }, h('b', null, G.done + ' created. '), 'The ' + CHOSEN.length + ' trades are its legs. ' + confirmText(G.kind) + '. ' + pricingText(G.kind) + '.'); nav = [h('a', { class: 'btn primary', href: '?deal=' + G.done }, 'Open the ' + GN)]; }
    else if (G.step === 0) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trades')), h('div', { class: 'body', style: 'display:grid;gap:.6rem' },
            h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Trade', 'Type', 'Family', 'Counterparty', 'Notional'].map(c => h('th', null, c)))), h('tbody', null, CHOSEN.map(t => h('tr', null, h('td', null, tradeLink(t.code)), h('td', null, ref('trade_type', t.type, tradeType(t.type).description)), h('td', null, t.product), h('td', null, ref('counterparty', t.cp, cpty(t.cp).name)), h('td', { class: 'num' }, moneyRef(t.notional, t.ccy)))))),
            checks.map(([okc, text]) => h('div', { class: 'notice ' + (okc ? 'ok' : 'bad') }, (okc ? '✓ ' : '✗ ') + text))));
        nav = [fwd(!good)];
    } else if (G.step === 1) {
        pane = h('div', { class: 'cards' }, h('section', { class: 'card' }, h('header', null, h('h2', null, 'Kind')), h('div', { class: 'body picker' }, Object.keys(KIND).map(k => h('button', { class: 'opt' + (k === G.kind ? ' on' : ''), onclick: () => { G.kind = k; G.template = ''; G.roles = {}; renderGroup(); } },
            k, h('small', null, KIND[k].desc), h('small', null, confirmText(k) + ' · ' + pricingText(k)))))),
            tpls.length ? h('section', { class: 'card' }, h('header', null, h('h2', null, 'Template')), h('div', { class: 'body' }, field('Template', select(tpls, G.template, v => { G.template = v; G.roles = {}; renderGroup(); }), 'The template fixes the roles and how many legs each holds.'))) : null,
            !K ? h('div', { class: 'notice' }, 'Choose how the customer holds these trades. A structure is one deal. A package is each trade under its own terms.')
                : K.whole ? h('div', { class: 'notice acc' }, 'The customer holds one deal, so the structure carries the confirmation and is priced as a whole. If the customer holds each trade under its own terms, choose Package.')
                    : h('div', { class: 'notice acc' }, 'A package carries no confirmation of its own. Each trade keeps its own external version and confirms on its own. It has no template and no price of its own.'),
            familyBad ? h('div', { class: 'notice bad' }, 'A strategy has one product family. These trades are ' + fam.join(' and ') + '. Choose Typed for legs from different families.') : null);
        nav = [back, fwd(familyBad || !G.kind)];
    } else if (G.step === 2) {
        const need = G.template ? TEMPLATES[G.kind][G.template] : null;
        const okr = !need || need.every(([r, lo, hi]) => { const n = CHOSEN.filter(t => G.roles[t.code] === r).length; return n >= lo && (!hi || n <= hi); });
        const pk = G.kind === 'Package';
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, steps[2]), h('span', { class: 'sp' }), need ? need.map(([r, lo, hi]) => chip(r + ' ' + lo + (hi && hi !== lo ? '–' + hi : ''))) : null),
            h('div', { class: 'body' }, pk ? h('div', { class: 'notice', style: 'margin-bottom:.6rem' }, 'A package rests on no template, so no role is required. Each trade stays a leg with its own confirmation.') : (!need ? h('div', { class: 'notice', style: 'margin-bottom:.6rem' }, 'Roles are read from data at run time. Name each leg\'s role.') : null),
                h('table', { class: 'grid' }, h('tbody', null, CHOSEN.map(t => h('tr', null, h('td', null, t.code + ' · ' + tradeType(t.type).description), pk ? h('td', { class: 'muted' }, 'external version ' + t.ext) : h('td', null, need ? select(roles, G.roles[t.code], v => { G.roles[t.code] = v; renderGroup(); }) : h('input', { class: 'field-input', value: G.roles[t.code], oninput: e => { G.roles[t.code] = e.target.value; } })))))),
                okr ? null : h('div', { class: 'notice warn' }, 'The template needs ' + need.map(r => r[0] + ' ' + r[1] + (r[2] && r[2] !== r[1] ? '–' + r[2] : '')).join(', ') + '.')));
        nav = [back, fwd(!okr)];
    } else {
        const pk = G.kind === 'Package';
        pane = h('div', { class: 'notice acc' }, h('b', null, 'What this will do'), h('div', null, 'Creates ' + (pk ? 'a package' : 'a ' + G.kind + (G.template ? ' (' + G.template + ')' : '') + ' structure') + ' for ' + PARTY + ' and ' + cpty([...cps][0]).name + '.'), h('div', null, 'Links ' + CHOSEN.length + ' trades as legs. No trade is rewritten.'),
            h('div', null, confirmText(G.kind) + '. ' + pricingText(G.kind) + '.'), pk ? h('div', null, 'The legs keep their own confirmations.') : h('div', null, 'The legs\' own confirmations are replaced by one for the deal.'),
            h('div', { class: 'grid2' }, field('Reason', select(REASONS, G.reason, v => { G.reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { G.note = e.target.value; } }, G.note))),
            h('div', { class: 'mono small' }, 'trading.v1.structures.put, structure_members.put_many, trade_links.put_many  change intent: reason, commentary'));
        nav = [back, h('button', { class: 'btn primary', onclick: () => {
            const id = (pk ? 'PKG-0' : 'STR-0') + (100 + Object.keys(DEALS).length); const rec = { kind: G.kind, template: G.template, cp: [...cps][0], members: CHOSEN.map(t => [t.code, G.roles[t.code]]), created: '2026-10-11T16:41:00+01:00', by: ME };
            store('structures', Object.assign(store('structures') || {}, { [id]: rec })); G.done = id; renderGroup(); } }, 'Create the ' + GN)];
    }
    put(wrap(pageHead({ title: 'Group trades', lead: GROUP_LEAD }), rail, pane, h('div', { class: 'filters' }, nav)));
}
/* ---------- the list ---------- */
/* The list is its address: the page, the page size, the search, the filter and the order. */
const num = (key, dflt) => { const n = Number(P.get(key)); return P.get(key) && Number.isFinite(n) && n >= 0 ? n : dflt; };
const LQ = { offset: num('offset', 0), limit: PAGE_SIZES.includes(num('limit', 15)) ? num('limit', 15) : 15, kind: P.get('kind') || '', search: P.get('search') || '', sort: P.get('sort') || '', desc: P.get('dir') !== 'asc' };
const LSORT = { id: ([id]) => id, kind: ([, v]) => v.kind, template: ([, v]) => v.template, cp: ([, v]) => cpty(v.cp).name, trades: ([, v]) => v.members.length };
const LSORT_FIELDS = { id: 'id', kind: 'kind', template: 'template', cp: 'counterparty_id', trades: 'member_count' };
function sortLBy(key) {
    if (LQ.sort !== key) { LQ.sort = key; LQ.desc = false; } else if (!LQ.desc) LQ.desc = true; else { LQ.sort = ''; LQ.desc = true; }
    LQ.offset = 0;
    renderList();
}
function listQuery() {
    const roots = Object.entries(DEALS).filter(([, v]) => !v.parent);
    const key = LSORT[LQ.sort] || (([, v]) => v.created), dir = LQ.sort && !LQ.desc ? 1 : -1;
    roots.sort((a, b) => { const x = key(a), y = key(b); return dir * (typeof x === 'number' ? x - y : String(x || '').localeCompare(String(y || ''))); });
    const rows = [];
    roots.forEach(([id, v]) => { rows.push([id, v, 0]); childrenOf(id).forEach(([cid, cv]) => rows.push([cid, cv, 1])); });
    const q = LQ.search.toLowerCase();
    return rows.filter(([id, v]) => (!LQ.kind || v.kind === LQ.kind) && (!q || (id + ' ' + v.template + ' ' + cpty(v.cp).name).toLowerCase().includes(q)));
}
function searchBox() {
    let t;
    return h('input', { placeholder: 'Search by identifier, template or counterparty', value: LQ.search, 'aria-label': 'Search', id: 'deal-search', oninput: e => {
        clearTimeout(t);
        t = setTimeout(() => { const at = e.target.selectionStart; LQ.search = e.target.value; LQ.offset = 0; renderList(); const again = document.getElementById('deal-search'); if (again) { again.focus(); again.setSelectionRange(at, at); } }, 300);
    } });
}
function renderList() {
    history.replaceState(null, '', qs({ offset: LQ.offset || null, limit: LQ.limit === 15 ? null : LQ.limit, kind: LQ.kind, search: LQ.search, sort: LQ.sort, dir: LQ.sort ? (LQ.desc ? 'desc' : 'asc') : null }));
    const head = pageHead({ title: 'Structures and packages', lead: 'What the tenant holds together. A structure is a tree: the deal the customer sees at the root, with strategies, packages and trades inside.',
        actions: [refreshButton({ stale, onClick: () => { stale = false; LOADED = NOW; renderList(); } }), workflowMenu([{ title: 'Group trades', line: 'Choose trades in the trade list, then hold them together.', href: '../trade-page/', allowed: !!CAN.structure }])] });
    const filters = h('div', { class: 'filters' }, searchBox(), select([['', 'All kinds']].concat(Object.keys(KIND).map(k => [k, k])), LQ.kind, v => { LQ.kind = v; LQ.offset = 0; renderList(); }));
    if (STATE_P === 'loading') return put(wrap(head, filters, h('div', { class: 'notice' }, 'Loading…')));
    if (STATE_P === 'error') return put(wrap(head, failure('trading.v1.structures.list', 503, 'unavailable', '1e8c4a07d3f5', 'The structures could not be read.', h('button', { class: 'btn small', onclick: () => { location.href = qs({ state: null }); } }, 'Retry'))));
    const shown = STATE_P === 'empty' ? [] : listQuery();
    const page = shown.slice(LQ.offset, LQ.offset + LQ.limit);
    const filtered = !!(LQ.kind || LQ.search);
    const sortTh = (key, label) => h('th', { 'aria-sort': LQ.sort === key ? (LQ.desc ? 'descending' : 'ascending') : 'none' }, h('button', { class: 'sort', onclick: () => sortLBy(key) }, label + (LQ.sort === key ? (LQ.desc ? ' ▾' : ' ▴') : '')));
    const table = h('div', { class: 'tablewrap' }, h('table', { class: 'grid' }, h('thead', null, h('tr', null, sortTh('id', 'Identifier'), sortTh('kind', 'Kind'), sortTh('template', 'Template'), sortTh('cp', 'Counterparty'), sortTh('trades', 'Inside it'), h('th', null, 'Confirms'), h('th', null, 'Priced'))),
        h('tbody', null, page.length ? page.map(([id, v, depth]) => { const kids = childrenOf(id).length; return h('tr', null, h('td', { class: 'nw', style: 'padding-left:' + (0.7 + depth * 1.4) + 'rem' }, depth ? h('span', { class: 'faint' }, '└ ') : null, h('a', { href: qs({ deal: id, offset: null, limit: null, kind: null, search: null, sort: null, dir: null }) }, id)), h('td', null, chip(v.kind, 'acc')), h('td', null, v.template ? ref('structure_template', v.template) : h('span', { class: 'dash' }, '—')), h('td', null, ref('counterparty', v.cp, cpty(v.cp).name)),
            h('td', null, (kids ? kids + ' inner · ' : '') + v.members.length + ' trade' + (v.members.length === 1 ? '' : 's')), h('td', null, confirmText(v.kind)), h('td', null, pricingText(v.kind))); })
            : h('tr', null, h('td', { colspan: 7 }, filtered
                ? ['No structure matches. ', h('button', { class: 'btn small', onclick: () => { Object.assign(LQ, { offset: 0, kind: '', search: '' }); renderList(); } }, 'Clear filters')]
                : ['There are no structures yet. ', CAN.structure ? h('a', { href: '../trade-page/' }, 'Group trades') : null])))));
    const order = LQ.sort ? { field: LSORT_FIELDS[LQ.sort], descending: LQ.desc } : { field: 'updated_at', descending: true };
    const filter = {}; if (LQ.kind) filter.kind = LQ.kind; if (LQ.search) filter.search = LQ.search;
    put(wrap(head, filters, table, pager({ offset: LQ.offset, limit: LQ.limit, total: shown.length, onChange: v => { Object.assign(LQ, v); renderList(); } }),
        h('div', { class: 'reqline' }, 'trading.v1.structures.list  offset=' + LQ.offset + ' limit=' + LQ.limit + ' order=' + JSON.stringify(order) + ' filter=' + JSON.stringify(filter) + '  →  total=' + shown.length)));
}
setTimeout(() => { if (TAB === 'price' && SID && !stale) { marketNewer = true; stale = true; renderDeal(); } }, 8000);
leaveGuard(() => MODE === 'group' && !G.done && (G.step > 0 || !!G.kind));
if (MODE) renderGroup(); else if (LIST) renderList(); else renderDeal();

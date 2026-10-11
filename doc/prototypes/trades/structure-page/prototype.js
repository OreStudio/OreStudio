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
const TAB = P.get('tab') || 'details';

const KIND = {
    Strategy: { whole: true, template: true, desc: 'One product family, one pricing model, fixed leg roles.' },
    Typed: { whole: true, template: true, desc: 'Legs from different families, with roles fixed by the type.' },
    Dynamic: { whole: true, template: false, desc: 'Roles read from data at run time, as a scripted product does.' },
    Package: { whole: false, template: false, desc: 'A container. Nothing economic binds the legs, so it has no template and any role.' }
};
const TEMPLATES = { Strategy: { Straddle: [['leg', 2, 2]], RiskReversal: [['call', 1, 1], ['put', 1, 1]], Butterfly: [['wing', 2, 2], ['body', 1, 1]] }, Typed: { CallableSwap: [['swap', 1, 1], ['call_right', 1, 1]] } };
const noun = k => k === 'Package' ? 'package' : 'structure';
const Noun = k => noun(k).replace(/^./, c => c.toUpperCase());
const confirmText = k => KIND[k].whole ? 'Confirmed as one document' : 'Each leg confirms on its own';
const pricingText = k => KIND[k].whole ? 'Priced as a whole' : 'Priced leg by leg';

/* A structure is a tree. The root is the deal the customer sees. Inner nodes are
 * structures, of any kind, and the leaves are trades. The model nests one level at most:
 * parent is the parent_structure_id of the structure table. */
const DEALS = Object.assign({
    'STR-0010': { kind: 'Dynamic', template: '', cp: 'MRDN', parent: '', members: [['ACME-100208', 'hedge']], created: '2026-08-20T14:00:00+01:00', by: 'ada.lovelace' },
    'STR-0007': { kind: 'Strategy', template: 'RiskReversal', cp: 'MRDN', parent: 'STR-0010', members: [['ACME-100202', 'call'], ['ACME-100209', 'put']], created: '2026-10-08T09:05:00+01:00', by: 'ada.lovelace' },
    'PKG-0021': { kind: 'Package', template: '', cp: 'MRDN', parent: 'STR-0010', members: [['ACME-100204', 'leg'], ['ACME-100213', 'leg']], created: '2026-08-20T14:05:00+01:00', by: 'ada.lovelace' },
    'STR-0009': { kind: 'Typed', template: 'CallableSwap', cp: 'CSTL', parent: '', members: [['ACME-100211', 'swap'], ['ACME-100212', 'call_right']], created: '2026-09-02T10:30:00+01:00', by: 'ada.lovelace' },
    'PKG-0004': { kind: 'Package', template: '', cp: 'NWCAP', parent: '', members: [['ACME-100201', 'leg'], ['ACME-100210', 'leg']], created: '2026-10-05T11:20:00+01:00', by: 'ada.lovelace' }
}, store('structures') || {});
const childrenOf = id => Object.entries(DEALS).filter(([, v]) => v.parent === id);
const rootOf = id => DEALS[id] && DEALS[id].parent ? rootOf(DEALS[id].parent) : id;
const SID = P.get('deal') || '';
const LIST = !SID && !MODE;
const CUR = DEALS[SID];
let stale = false;

const main = shell({
    title: MODE ? 'Group trades' : LIST ? 'Structures and packages' : (CUR ? Noun(CUR.kind) : 'Structure') + ' ' + SID, name: 'structure-page',
    question: 'How are several trades held together, as one deal or as a package?',
    crumbs: MODE ? [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', '../trade-page/'], ['Group trades']] : LIST ? [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages']] : [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages', qs({ deal: null, tab: null })]].concat(CUR && CUR.parent ? [[CUR.parent, qs({ deal: CUR.parent, tab: null })]] : []).concat([[SID]]),
    states: [['list', { mode: null, trades: null, deal: null, tab: null }, LIST], ['structure tree', { mode: null, trades: null, deal: 'STR-0010' }, SID === 'STR-0010'], ['strategy', { mode: null, trades: null, deal: 'STR-0007' }, SID === 'STR-0007'],
        ['package', { mode: null, trades: null, deal: 'PKG-0021' }, SID === 'PKG-0021'], ['typed structure', { mode: null, trades: null, deal: 'STR-0009' }, SID === 'STR-0009'], ['package alone', { mode: null, trades: null, deal: 'PKG-0004' }, SID === 'PKG-0004'],
        ['group', { mode: 'group', deal: null, trades: 'ACME-100202,ACME-100209' }, MODE === 'group'], ['mismatch', { mode: 'group', deal: null, trades: 'ACME-100201,ACME-100202' }, false]],
    says: LIST ? 'A structure is a tree. The root is the deal the customer sees. Inside it are strategies, packages and plain trades. The list shows the trees; open the root to see the whole tree.' : MODE
        ? 'One workflow, and the kind decides what the customer receives. A strategy is one deal. A package is trades the customer holds one by one. The screen states each kind\'s consequences and the rules the server will enforce, before it writes.'
        : 'A package is a kind of structure, so there is one screen and its words follow the kind. Compare the strategy and the package: how it confirms, how it prices, where the external version lives, what unlinking a leg means.'
});
const wrap = (...kids) => h('div', { class: 'dyn' }, kids);
function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); main.append(el); }
const kindRule = s => s.template ? TEMPLATES[s.kind][s.template] : null;
const tmem = s => s.members.map(([c, r]) => ({ t: trade(c), role: r })).filter(x => x.t);
const EDITOR = '../trade-entry/';

/* ---------- the page ---------- */
function renderDeal() {
    const s = DEALS[SID];
    if (!s) return put(wrap(h('div', { class: 'notice bad' }, 'No structure or package ' + SID + '. It may have been removed, or you may not see its party.')));
    const N = noun(s.kind), pk = s.kind === 'Package';
    const ms = tmem(s);
    const ttl = title(s);
    const roleCheck = kindRule(s) ? kindRule(s).map(([role, lo, hi]) => ({ role, lo, hi, n: s.members.filter(m => m[1] === role).length })) : [];
    const ok = roleCheck.every(r => r.n >= r.lo && (!r.hi || r.n <= r.hi));
    const head = pageHead({
        title: ttl, id: SID, actions: [refreshButton({ stale, onClick: () => { stale = false; LOADED = NOW; renderDeal(); } }),
            workflowMenu([{ title: 'Link a trade', line: 'Add a trade of this party and counterparty as a leg.', run: () => linkDialog(s), allowed: !!CAN.structure },
                pk ? { title: 'Price the legs', line: 'A package has no price of its own. Open the pricing screen of the trade-entry prototype.', href: EDITOR + 'pricing.html' }
                    : { title: 'Price this structure', line: 'Open the pricing screen of the trade-entry prototype.', href: EDITOR + 'pricing.html' },
                { title: 'Edit in the ' + N + ' editor', line: 'Manage the legs in the trade-entry prototype.', href: EDITOR }])]
    });
    const sub = h('div', { class: 'filters', style: 'margin-top:-.25rem' }, chip(s.kind, 'acc'), s.template ? chip(s.template) : null, chip(confirmText(s.kind), pk ? 'warn' : 'flow'), chip(pricingText(s.kind), 'flow'),
        s.template ? (ok ? chip('complete', 'ok') : chip('legs missing', 'warn')) : null, chip('version read from the legs', 'v', 'A ' + N + ' stores no version of its own. It is read from its legs.'));
    let content;
    if (TAB === 'members') content = membersCard(s, ms, roleCheck);
    else if (TAB === 'history') content = timeline(history(s, ms), { isService: false });
    else content = h('div', { class: 'cards' }, card({ id: 'identity', title: 'Identity', table: 'structure', version: 1, fields: [
        { label: 'Party', value: PARTY, fixedWhy: 'Shared by every leg, for the life of the ' + (pk ? 'package' : 'deal') + '.', type: 'text' },
        { label: 'Counterparty', value: cpty(s.cp).name, fixedWhy: 'Shared by every leg, for the life of the ' + (pk ? 'package' : 'deal') + '.', type: 'text' },
        { label: 'Kind', value: s.kind, fixedWhy: KIND[s.kind].desc, type: 'text' },
        pk ? null : { label: 'Template', value: s.template, type: 'text' },
        { label: 'Inside', value: s.parent, fixedWhy: s.parent ? 'The ' + N + ' is a node of the tree of ' + s.parent + '.' : 'A ' + N + ' nests one level at most: a root holds inner structures, and an inner structure holds only trades.', type: 'text' }].filter(Boolean) }, { readOnlyNote: true }),
        card({ id: 'how', title: 'How it confirms and prices', table: 'structure_kind', version: 1, fields: [
            { label: 'Confirmation', value: confirmText(s.kind), type: 'text', fixedWhy: pk ? 'The customer holds each trade under its own terms, even though they were executed together.' : 'The customer holds one deal. A partial close-out gives one confirmation for the remainder.' },
            { label: 'Pricing', value: pricingText(s.kind), type: 'text', fixedWhy: pk ? 'No leg depends on another, so the price is the sum of the parts.' : 'The legs interact, so the deal has one price.' },
            { label: 'External version', value: pk ? 'Of each trade' : 'Of the deal', type: 'text', fixedWhy: pk ? 'Each leg moves on its own: see the Members tab.' : 'Moves when the economics of a leg change. The structure table has no column for it yet.' },
            { label: 'Internal version', value: 'Read from the legs', type: 'text', fixedWhy: 'A ' + N + ' stores none.' }] }, { readOnlyNote: true }),
        membersCard(s, ms, roleCheck));
    const right = SEL ? tradePanel(SEL) : h('div', null, tabs([['details', 'Details'], ['members', 'Members'], ['history', 'History']], TAB, id => qs({ tab: id === 'details' ? null : id })), content);
    put(wrap(head, sub, h('div', { class: 'workspace' }, treePanel(), right)));
}
/* The tree is its own panel, beside the page, as in the structure editor of the
 * trade-entry prototype: the root, its inner structures, and the trades, one row each. */
const SEL = P.get('trade') || '';
const title = (v) => v.kind === 'Package' ? 'Package' : (v.template ? v.template.replace(/([A-Z])/g, ' $1').trim() : v.kind + ' structure');
const productTag = t => ({ swap: 'Rates', fx: 'FX', credit: 'Credit', bond: 'Bond', equity: 'Equity', composite: 'Composite' }[t.product] || t.product);
function treeRow(depth, selected, onclick, kids) {
    return h('div', { class: 'node-row' + (selected ? ' selected' : ''), role: 'treeitem', tabindex: 0, onclick, onkeydown: e => { if (e.key === 'Enter') onclick(); } }, kids);
}
function treeNode(id, depth) {
    const v = DEALS[id], legs = tmem(v), kids = childrenOf(id);
    const here = id === SID && !SEL;
    const row = treeRow(depth, here, () => { location.href = qs({ deal: id, trade: null, tab: null }); }, [
        h('span', { class: 'twisty' }, '▾'),
        depth ? h('span', { class: 'type-tag' }, v.kind) : null,
        h('span', { class: 'label' }, h('span', { class: 'name' }, title(v)), h('span', { class: 'meta' }, id + ' · ' + (kids.length ? kids.length + ' inner · ' : '') + legs.length + ' trade' + (legs.length === 1 ? '' : 's'))),
        depth ? h('span', { class: 'num' }, confirmText(v.kind) === 'Confirmed as one document' ? 'one document' : 'by trade') : chip(v.kind, 'acc')]);
    const children = h('div', { class: 'group-children' }, kids.map(([cid]) => treeNode(cid, depth + 1)),
        legs.map(m => h('div', { class: 'node depth-2' }, treeRow(2, SEL === m.t.code, () => { location.href = qs({ deal: id, trade: m.t.code, tab: null }); }, [
            h('span', { class: 'twisty' }),
            h('span', { class: 'type-tag' }, productTag(m.t)),
            h('span', { class: 'label' }, h('span', { class: 'name' }, tradeType(m.t.type).description), h('span', { class: 'meta' }, m.t.code + ' · ' + m.role)),
            h('span', { class: 'num' }, money(m.t.notional, m.t.ccy))]))));
    return h('div', { class: 'node depth-' + depth }, row, children);
}
function treePanel() {
    const root = rootOf(SID), rv = DEALS[root];
    const inner = childrenOf(root).length;
    return h('section', { class: 'panel' }, h('header', null, h('h2', null, 'Structure'), h('span', { class: 'legend' }, inner + ' inner structure' + (inner === 1 ? '' : 's') + ' · ' + 'root ' + root)),
        h('div', { class: 'tree', role: 'tree', 'aria-label': 'The structure tree' }, treeNode(root, 0)),
        h('div', { class: 'tree-note' }, 'A structure nests one level at most: the root holds inner structures, and an inner structure holds only trades. The insert trigger refuses a deeper tree.',
            inner && rv.kind !== 'Package' ? h('div', { class: 'notice warn', style: 'margin-top:.5rem' }, 'The root confirms as one document. How a package inside it confirms is not decided. It is filed as a capture.') : null));
}
function tradePanel(code) {
    const t = trade(code), role = (tmem(DEALS[SID]).find(m => m.t.code === code) || {}).role;
    if (!t) return h('div', { class: 'notice bad' }, 'No trade ' + code + '.');
    const pk = DEALS[SID].kind === 'Package';
    return h('div', { class: 'cards' }, card({ id: 'leg', title: 'Trade in ' + SID, table: 'structure_member', version: 1, fields: [
        { label: 'Trade', value: t.code, type: 'text' }, { label: 'Type', value: tradeType(t.type).description, type: 'text' }, { label: 'Role', value: role, type: 'text' },
        { label: 'State', value: t.state, type: 'text' }, { label: 'Counterparty', value: cpty(t.cp).name, type: 'text' }, { label: 'Notional', value: t.notional, type: 'money' },
        { label: 'External version', value: 'v' + t.ext, type: 'text', fixedWhy: pk ? 'This trade confirms on its own, so this is the version the customer quotes.' : 'The deal carries the confirmation. This trade has its own version too.' }] }, { readOnlyNote: true }),
        h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/?trade=' + t.code }, 'Open the trade'), h('a', { class: 'btn', href: qs({ trade: null }) }, 'Back to the ' + noun(DEALS[SID].kind))));
}
function membersCard(s, ms, roleCheck) {
    const pk = s.kind === 'Package';
    const cols = [pk ? 'Leg' : 'Role', 'Trade', 'Type', 'State', 'Notional'].concat(pk ? ['External version'] : []).concat(['']);
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trades'), chip(ms.length + ' legs', 'v'), h('span', { class: 'sp' }), CAN.structure ? h('button', { class: 'btn small', onclick: () => linkDialog(s) }, '+ Link a trade') : null),
        h('div', { class: 'body' }, roleCheck.length ? h('div', { class: 'filters', style: 'margin-top:0' }, roleCheck.map(r => chip(r.role + ' ' + r.n + (r.hi ? ' of ' + r.lo + (r.hi !== r.lo ? '–' + r.hi : '') : ''), r.n >= r.lo && (!r.hi || r.n <= r.hi) ? 'ok' : 'warn'))) : null,
            pk ? h('div', { class: 'notice', style: 'margin-bottom:.6rem' }, 'A package confirms trade by trade. Each leg has its own external version, and the customer quotes the one for the trade they hold.') : null,
            h('table', { class: 'grid' }, h('thead', null, h('tr', null, cols.map(c => h('th', null, c)))),
                h('tbody', null, ms.map((m, i) => h('tr', null, h('td', null, pk ? String(i + 1) : m.role), h('td', { class: 'nw' }, h('a', { href: '../trade-page/?trade=' + m.t.code }, m.t.code)), h('td', null, tradeType(m.t.type).description), h('td', null, chip(m.t.state, { live: 'ok', draft: 'warn', cancelled: 'bad' }[m.t.state])),
                    h('td', { class: 'num nw' }, money(m.t.notional, m.t.ccy)), pk ? h('td', { class: 'mono' }, 'ext ' + m.t.ext) : null, h('td', null, CAN.structure ? h('button', { class: 'btn small ghost', onclick: () => unlink(s, m) }, 'Unlink') : null)))))));
}
function history(s, ms) {
    const N = noun(s.kind);
    const e = [{ id: 's1', at: s.created, kind: 'booked', activity: 'new_booking', version: 1, actor: s.by, reason: 'trade.client_request', note: Noun(s.kind) + ' created with ' + s.members.length + ' legs', diffs: [['kind', '', s.kind], ['counterparty', '', cpty(s.cp).name]] }];
    ms.forEach((m, i) => e.push({ id: 's' + (i + 2), at: new Date(new Date(s.created).getTime() + (i + 1) * 60000).toISOString().slice(0, 19) + '+01:00', kind: 'event', activity: 'new_booking', version: 1, actor: s.by, reason: 'trade.client_request', note: m.t.code + ' linked as ' + m.role, diffs: [], noVersion: true }));
    ms.forEach((m, i) => e.push({ id: 'l' + i, at: '2026-10-10T1' + i + ':15:00+01:00', kind: 'amended', activity: 'amendment_economic', version: m.t.ver, ext: m.t.ext, actor: 'ada.lovelace', reason: 'trade.restructure', note: 'Read from the leg ' + m.t.code + '. Not a version of the ' + N + '.', diffs: [['notional', '', money(m.t.notional, m.t.ccy)]], economic: true }));
    e.forEach(x => x.current = false); e[e.length - 1].current = true;
    return e;
}
function unlink(s, m) {
    const N = noun(s.kind), pk = s.kind === 'Package';
    const d = openDialog({ title: 'Unlink ' + m.t.code, loadedAt: LOADED,
        body: [h('div', { class: 'notice' }, 'The trade stays as it is. It is no longer a leg of ' + SID + ', and can be linked to another ' + N + ' without being rewritten.'),
            pk ? h('div', { class: 'notice acc' }, 'A package binds nothing economic, so the other legs are unaffected and keep their own confirmations.')
                : h('div', { class: 'notice warn' }, 'This deal confirms as one document' + (s.template ? ' and ' + s.template + ' needs this leg' : '') + '. Without it the customer holds a different deal. Unlinking is not an amendment: use a lifecycle event for a partial close-out.'),
            field('Reason', select(REASON_CODES.filter(r => r[0] !== 'trade.no_change'), 'trade.correction'))],
        buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), h('button', { class: 'btn primary', onclick: () => { s.members = s.members.filter(x => x[0] !== m.t.code); d.close(); stale = true; renderDeal(); toast('Unlinked. Refresh shows the change.'); } }, 'Unlink')] });
}
function linkDialog(s) {
    const pk = s.kind === 'Package';
    const free = TRADES.filter(t => t.cp === s.cp && !s.members.some(m => m[0] === t.code) && !t.structure && t.state !== 'cancelled').slice(0, 8);
    let pick = free[0] && free[0].code, role = s.template ? kindRule(s)[0][0] : 'leg';
    const d = openDialog({ title: 'Link a trade to ' + SID, loadedAt: LOADED,
        body: [free.length ? field('Trade', select(free.map(t => [t.code, t.code + ' · ' + tradeType(t.type).description]), pick, v => { pick = v; }), 'Only trades of ' + PARTY + ' and ' + cpty(s.cp).name + ' that are not a leg of a structure or a package.') : h('div', { class: 'notice' }, 'No trade of this counterparty is free to link.'),
            pk ? null : field('Role', select(kindRule(s) ? kindRule(s).map(r => r[0]) : ['leg'], role, v => { role = v; }), kindRule(s) ? 'The template allows these roles.' : 'Roles are read from data.')],
        buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), h('button', { class: 'btn primary', disabled: !free.length, onclick: () => { s.members.push([pick, role]); d.close(); stale = true; renderDeal(); toast('Linked. Refresh shows the change.'); } }, 'Link')] });
}

/* ---------- grouping ---------- */
const CHOSEN = (P.get('trades') || '').split(',').filter(Boolean).map(trade).filter(Boolean);
let G = { step: 0, kind: '', template: '', roles: {}, done: null };
function renderGroup() {
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
            h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Trade', 'Type', 'Family', 'Counterparty', 'Notional'].map(c => h('th', null, c)))), h('tbody', null, CHOSEN.map(t => h('tr', null, h('td', null, h('a', { href: '../trade-page/?trade=' + t.code }, t.code)), h('td', null, tradeType(t.type).description), h('td', null, t.product), h('td', null, cpty(t.cp).name), h('td', { class: 'num' }, money(t.notional, t.ccy)))))),
            checks.map(([okc, text]) => h('div', { class: 'notice ' + (okc ? 'ok' : 'bad') }, (okc ? '✓ ' : '✗ ') + text)), good ? null : h('div', { class: 'faint small' }, 'The server enforces this too, and answers with an error. The screen states it first.')));
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
            h('div', { class: 'mono small' }, 'trading.v1.structures.put, structure_members.put_many, trade_links.put_many'));
        nav = [back, h('button', { class: 'btn primary', onclick: () => {
            const id = (pk ? 'PKG-0' : 'STR-0') + (100 + Object.keys(DEALS).length); const rec = { kind: G.kind, template: G.template, cp: [...cps][0], members: CHOSEN.map(t => [t.code, G.roles[t.code]]), created: '2026-10-11T16:41:00+01:00', by: ME };
            store('structures', Object.assign(store('structures') || {}, { [id]: rec })); G.done = id; renderGroup(); } }, 'Create the ' + GN)];
    }
    put(wrap(pageHead({ title: 'Group trades', lead: 'Hold several trades together, as one deal or as a package. The kind decides what the customer receives.' }),
        CAN.structure ? null : h('div', { class: 'notice warn' }, h('b', null, 'You cannot use this screen. '), 'It needs the right to group trades. Ask a tenant administrator for the Trader role.'), rail, pane, h('div', { class: 'filters' }, nav)));
}
/* ---------- the list ---------- */
let LQ = { kind: '', offset: 0, limit: 15 };
function renderList() {
    const roots = Object.entries(DEALS).filter(([, v]) => !v.parent);
    const rows = [];
    roots.forEach(([id, v]) => { rows.push([id, v, 0]); childrenOf(id).forEach(([cid, cv]) => rows.push([cid, cv, 1])); });
    const shown = rows.filter(([, v]) => !LQ.kind || v.kind === LQ.kind);
    const page = shown.slice(LQ.offset, LQ.offset + LQ.limit);
    const head = pageHead({ title: 'Structures and packages', lead: 'What the tenant holds together. A structure is a tree: the deal the customer sees at the root, with strategies, packages and trades inside.',
        actions: [refreshButton({ stale: false, onClick: () => { LOADED = NOW; renderList(); } }), workflowMenu([{ title: 'Group trades', line: 'Choose trades in the trade list, then hold them together.', href: '../trade-page/', allowed: !!CAN.structure }])] });
    const filters = h('div', { class: 'filters' }, select([['', 'All kinds']].concat(Object.keys(KIND).map(k => [k, k])), LQ.kind, v => { LQ.kind = v; LQ.offset = 0; renderList(); }));
    const table = h('div', { class: 'tablewrap' }, h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['', 'Kind', 'Template', 'Counterparty', 'Inside it', 'Confirms', 'Priced'].map(c => h('th', null, c)))),
        h('tbody', null, page.map(([id, v, depth]) => { const kids = childrenOf(id).length; return h('tr', null, h('td', { class: 'nw', style: 'padding-left:' + (0.7 + depth * 1.4) + 'rem' }, depth ? h('span', { class: 'faint' }, '└ ') : null, h('a', { href: qs({ deal: id }) }, id)), h('td', null, chip(v.kind, 'acc')), h('td', null, v.template || h('span', { class: 'dash' }, '—')), h('td', null, cpty(v.cp).name),
            h('td', null, (kids ? kids + ' inner · ' : '') + v.members.length + ' trade' + (v.members.length === 1 ? '' : 's')), h('td', null, confirmText(v.kind)), h('td', null, pricingText(v.kind))); }))));
    put(wrap(head, filters, table, pager({ offset: LQ.offset, limit: LQ.limit, total: shown.length, onChange: v => { Object.assign(LQ, v); renderList(); } }),
        h('div', { class: 'reqline' }, 'trading.v1.structures.list  offset=' + LQ.offset + ' limit=' + LQ.limit + ' filter=' + JSON.stringify(LQ.kind ? { kind: LQ.kind } : {}) + '  →  total=' + shown.length)));
}
if (MODE) renderGroup(); else if (LIST) renderList(); else renderDeal();

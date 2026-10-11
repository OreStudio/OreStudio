'use strict';
/* A structure, and grouping trades into one. Serves: Group trades into a structure.
 * A structure is an entity: an immutable anchor, and legs that are trades. */

const MODE = P.get('mode') === 'group' ? 'group' : '';
const SID = P.get('structure') || (MODE ? '' : 'STR-0004');
const TAB = P.get('tab') || 'details';
const KINDS_ = { Strategy: 'One product family, one pricing model, fixed leg roles. Confirms as one document.', Typed: 'Typed legs, priced together.', Dynamic: 'Legs added over time.', Package: 'A container. Nothing economic binds the legs.' };
const TEMPLATES = { Strategy: { Straddle: [['leg', 2, 2]], RiskReversal: [['call', 1, 1], ['put', 1, 1]], Butterfly: [['wing', 2, 2], ['body', 1, 1]] } };

/* The structures the screen knows. Two are written out; the rest are made by the group workflow. */
const STRUCTS = Object.assign({
    'STR-0004': { kind: 'Package', template: '', cp: 'NWCAP', members: [['ACME-100205', 'member'], ['ACME-100210', 'member']], created: '2026-10-05T11:20:00+01:00', by: 'ada.lovelace' },
    'STR-0007': { kind: 'Strategy', template: 'RiskReversal', cp: 'MRDN', members: [['ACME-100202', 'call'], ['ACME-100209', 'put']], created: '2026-10-08T09:05:00+01:00', by: 'ada.lovelace' }
}, store('structures') || {});
let stale = false;
const main = shell({
    title: MODE ? 'Group trades into a structure' : 'Structure ' + SID, name: 'structure-page',
    question: 'How is a deal made of several trades read and put together?',
    crumbs: MODE ? [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', '../trade-page/'], ['Group trades into a structure']] : [['Home', '#'], ['Trades', '../trade-page/'], ['Structures', '#'], [SID]],
    states: [['structure', { mode: null, trades: null, structure: 'STR-0004' }, !MODE && SID === 'STR-0004'], ['strategy', { mode: null, trades: null, structure: 'STR-0007' }, SID === 'STR-0007'],
        ['group', { mode: 'group', structure: null, trades: 'ACME-100202,ACME-100206' }, MODE === 'group'], ['mismatch', { mode: 'group', structure: null, trades: 'ACME-100201,ACME-100202' }, false]],
    says: MODE
        ? 'Grouping is a workflow with a check at the start: every leg shares the party and the counterparty, and a structure nests one level at most. The client states the rule before the server refuses it. Try the mismatch state.'
        : 'A structure is an entity with its own page. Its identity is fixed. Its version is read from its legs, never stored beside them. The editor and the pricing screen are the trade-entry prototype, one click away.'
});
const wrap = (...kids) => h('div', { class: 'dyn' }, kids);
function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); main.append(el); }
const kindRule = (s) => s.template ? TEMPLATES[s.kind][s.template] : null;
const tmem = (s) => s.members.map(([c, r]) => ({ t: trade(c), role: r })).filter(x => x.t);

function renderStructure() {
    const s = STRUCTS[SID];
    if (!s) return put(wrap(h('div', { class: 'notice bad' }, 'No structure ' + SID + '.')));
    const ms = tmem(s);
    const title = s.template ? s.template.replace(/([A-Z])/g, ' $1').trim() : s.kind;
    const roleCheck = kindRule(s) ? kindRule(s).map(([role, lo, hi]) => ({ role, lo, hi, n: s.members.filter(m => m[1] === role).length })) : [];
    const ok = roleCheck.every(r => r.n >= r.lo && (!r.hi || r.n <= r.hi));
    const head = pageHead({
        title, id: SID, actions: [refreshButton({ stale, onClick: () => { stale = false; LOADED = NOW; renderStructure(); } }),
            workflowMenu([{ title: 'Link a trade', line: 'Add a trade of this party and counterparty as a leg.', run: () => linkDialog(s), allowed: !!CAN.structure },
                { title: 'Price this structure', line: 'Open the pricing screen of the trade-entry prototype.', href: '../../books/trade-entry/pricing.html' },
                { title: 'Edit in the structure editor', line: 'Manage the legs in the trade-entry prototype.', href: '../../books/trade-entry/' }])]
    });
    const sub = h('div', { class: 'filters', style: 'margin-top:-.25rem' }, chip(s.kind, 'acc'), s.template ? chip(s.template) : null, ok ? chip('complete', 'ok') : chip('legs missing', 'warn'),
        chip('version read from the legs', 'v', 'The structure stores no version of its own. It is read from its legs.'));
    let content;
    if (TAB === 'members') content = membersCard(s, ms, roleCheck);
    else if (TAB === 'history') content = timeline(structHistory(s, ms), { isService: false });
    else content = h('div', { class: 'cards' }, card({ id: 'identity', title: 'Identity', table: 'structure', version: 1, fields: [
        { label: 'Party', value: PARTY, fixedWhy: 'Shared by every leg, for the life of the deal.', type: 'text' }, { label: 'Counterparty', value: cpty(s.cp).name, fixedWhy: 'Shared by every leg, for the life of the deal.', type: 'text' },
        { label: 'Kind', value: s.kind, fixedWhy: KINDS_[s.kind], type: 'text' }, { label: 'Template', value: s.template, type: 'text' }, { label: 'Parent structure', value: '', fixedWhy: 'A structure nests one level at most.', type: 'text' }] }, { readOnlyNote: true }),
        membersCard(s, ms, roleCheck), h('div', { class: 'notice acc' }, h('b', null, 'The version is read from the legs. '), ms.map(m => m.t.code + ' external v' + m.t.ext).join(' · ') + '. A change to the economics of any leg moves the deal. An authorisation does not.'));
    put(wrap(head, sub, tabs([['details', 'Details'], ['members', 'Members'], ['history', 'History']], TAB, id => qs({ tab: id === 'details' ? null : id })), content));
}
function membersCard(s, ms, roleCheck) {
    return h('section', { class: 'card' }, h('header', null, h('h2', null, 'Members'), chip(ms.length + ' legs', 'v'), h('span', { class: 'sp' }), CAN.structure ? h('button', { class: 'btn small', onclick: () => linkDialog(s) }, '+ Link a trade') : null),
        h('div', { class: 'body' }, roleCheck.length ? h('div', { class: 'filters', style: 'margin-top:0' }, roleCheck.map(r => chip(r.role + ' ' + r.n + (r.hi ? ' of ' + r.lo + (r.hi !== r.lo ? '–' + r.hi : '') : ''), r.n >= r.lo && (!r.hi || r.n <= r.hi) ? 'ok' : 'warn'))) : null,
            h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Role', 'Trade', 'Type', 'State', 'Notional', ''].map(c => h('th', null, c)))),
                h('tbody', null, ms.map(m => h('tr', null, h('td', null, m.role), h('td', { class: 'nw' }, h('a', { href: '../trade-page/?trade=' + m.t.code }, m.t.code)), h('td', null, tradeType(m.t.type).description), h('td', null, chip(m.t.state, { live: 'ok', draft: 'warn', cancelled: 'bad' }[m.t.state])),
                    h('td', { class: 'num nw' }, money(m.t.notional, m.t.ccy)), h('td', null, CAN.structure ? h('button', { class: 'btn small ghost', onclick: () => unlink(s, m) }, 'Unlink') : null)))))));
}
function structHistory(s, ms) {
    const e = [{ id: 's1', at: s.created, kind: 'booked', activity: 'new_booking', version: 1, actor: s.by, reason: 'trade.client_request', note: 'Structure created with ' + s.members.length + ' legs', diffs: [['kind', '', s.kind], ['counterparty', '', cpty(s.cp).name]] }];
    ms.forEach((m, i) => e.push({ id: 's' + (i + 2), at: new Date(new Date(s.created).getTime() + (i + 1) * 60000).toISOString().slice(0, 19) + '+01:00', kind: 'event', activity: 'new_booking', version: 1, actor: s.by, reason: 'trade.client_request', note: m.t.code + ' linked as ' + m.role, diffs: [], noVersion: true }));
    ms.forEach((m, i) => e.push({ id: 'l' + i, at: '2026-10-10T1' + i + ':15:00+01:00', kind: 'amended', activity: 'amendment_economic', version: m.t.ver, ext: m.t.ext, actor: 'ada.lovelace', reason: 'trade.restructure', note: 'Read from the leg ' + m.t.code + '. Not a version of the structure.', diffs: [['notional', '', money(m.t.notional, m.t.ccy)]], economic: true }));
    e.forEach(x => x.current = false); e[e.length - 1].current = true;
    return e;
}
function unlink(s, m) {
    const d = openDialog({ title: 'Unlink ' + m.t.code, loadedAt: LOADED, body: [h('div', { class: 'notice' }, 'The trade stays as it is. It is no longer a leg of ' + SID + ', and can be linked to another deal without being rewritten.'), field('Reason', select(REASON_CODES.filter(r => r[0] !== 'trade.no_change'), 'trade.correction'))],
        buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), h('button', { class: 'btn primary', onclick: () => { s.members = s.members.filter(x => x[0] !== m.t.code); d.close(); stale = true; renderStructure(); toast('Unlinked. Refresh shows the change.'); } }, 'Unlink')] });
}
function linkDialog(s) {
    const free = TRADES.filter(t => t.cp === s.cp && !s.members.some(m => m[0] === t.code) && t.state !== 'cancelled').slice(0, 8);
    let pick = free[0] && free[0].code, role = s.template ? kindRule(s)[0][0] : 'member';
    const d = openDialog({ title: 'Link a trade to ' + SID, loadedAt: LOADED,
        body: [free.length ? field('Trade', select(free.map(t => [t.code, t.code + ' · ' + tradeType(t.type).description]), pick, v => { pick = v; }), 'Only trades of ' + PARTY + ' and ' + cpty(s.cp).name + ' that are not in another structure.') : h('div', { class: 'notice' }, 'No trade of this counterparty is free to link.'),
            field('Role', select(s.template ? kindRule(s).map(r => r[0]) : ['member'], role, v => { role = v; }))],
        buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), h('button', { class: 'btn primary', disabled: !free.length, onclick: () => { s.members.push([pick, role]); d.close(); stale = true; renderStructure(); toast('Linked. Refresh shows the change.'); } }, 'Link')] });
}

/* ---------- grouping ---------- */
const CHOSEN = (P.get('trades') || '').split(',').filter(Boolean).map(trade).filter(Boolean);
let G = { step: 0, kind: 'Package', template: '', roles: {}, done: null };
function renderGroup() {
    const cps = new Set(CHOSEN.map(t => t.cp)), shared = cps.size === 1;
    const alreadyIn = CHOSEN.filter(t => t.structure);
    const checks = [[CHOSEN.length >= 2, 'Two or more trades are chosen.'], [shared, 'Every trade has the same counterparty' + (shared ? ' (' + cpty([...cps][0]).name + ').' : '. These have ' + [...cps].map(c => cpty(c).name).join(' and ') + '.')],
        [!alreadyIn.length, alreadyIn.length ? alreadyIn.map(t => t.code).join(', ') + ' already belong to a structure.' : 'No trade is already a leg of another structure.'], [CHOSEN.every(t => t.state !== 'cancelled'), 'No chosen trade is cancelled.']];
    const good = checks.every(c => c[0]);
    const steps = ['Trades', 'Kind', 'Roles', 'Confirm'];
    const rail = h('div', { class: 'steps' }, steps.map((s, i) => h('span', { class: 'step' + (i === G.step ? ' on' : i < G.step ? ' done' : '') }, (i + 1) + ' ' + s)));
    const roles = G.template ? TEMPLATES.Strategy[G.template].map(r => r[0]) : ['member'];
    CHOSEN.forEach((t, i) => { if (!G.roles[t.code]) G.roles[t.code] = roles[i % roles.length]; });
    let pane, nav;
    if (G.done) { pane = h('div', { class: 'notice ok' }, h('b', null, G.done + ' created. '), 'The ' + CHOSEN.length + ' trades are its legs.'); nav = [h('a', { class: 'btn primary', href: '?structure=' + G.done }, 'Open the structure')]; }
    else if (G.step === 0) { pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Trades')), h('div', { class: 'body', style: 'display:grid;gap:.6rem' },
        h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Trade', 'Type', 'Counterparty', 'Notional'].map(c => h('th', null, c)))), h('tbody', null, CHOSEN.map(t => h('tr', null, h('td', null, h('a', { href: '../trade-page/?trade=' + t.code }, t.code)), h('td', null, tradeType(t.type).description), h('td', null, cpty(t.cp).name), h('td', { class: 'num' }, money(t.notional, t.ccy)))))),
        checks.map(([okc, text]) => h('div', { class: 'notice ' + (okc ? 'ok' : 'bad') }, (okc ? '✓ ' : '✗ ') + text)), good ? null : h('div', { class: 'faint small' }, 'The server enforces this too, and answers with an error. The screen states it first.'))); nav = [h('button', { class: 'btn primary', disabled: !good, onclick: () => { G.step = 1; renderGroup(); } }, 'Continue')]; }
    else if (G.step === 1) { pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Kind and template')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' }, h('div', { class: 'grid2' },
        field('Kind', select(Object.keys(KINDS_), G.kind, v => { G.kind = v; G.template = ''; G.roles = {}; renderGroup(); }), KINDS_[G.kind]),
        G.kind === 'Strategy' ? field('Template', select(Object.keys(TEMPLATES.Strategy), G.template || 'RiskReversal', v => { G.template = v; G.roles = {}; renderGroup(); })) : null))); if (G.kind === 'Strategy' && !G.template) G.template = 'RiskReversal'; nav = [h('button', { class: 'btn', onclick: () => { G.step = 0; renderGroup(); } }, 'Back'), h('button', { class: 'btn primary', onclick: () => { G.step = 2; renderGroup(); } }, 'Continue')]; }
    else if (G.step === 2) { const need = G.template ? TEMPLATES.Strategy[G.template] : null; const okr = !need || need.every(([r, lo, hi]) => { const n = CHOSEN.filter(t => G.roles[t.code] === r).length; return n >= lo && (!hi || n <= hi); });
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Roles'), h('span', { class: 'sp' }), need ? need.map(([r, lo, hi]) => chip(r + ' ' + lo + (hi && hi !== lo ? '–' + hi : ''))) : null), h('div', { class: 'body' }, h('table', { class: 'grid' }, h('tbody', null, CHOSEN.map(t => h('tr', null, h('td', null, t.code + ' · ' + tradeType(t.type).description), h('td', null, select(roles, G.roles[t.code], v => { G.roles[t.code] = v; renderGroup(); }))))),
            okr ? null : h('div', { class: 'notice warn' }, 'The template needs ' + need.map(r => r[0] + ' ' + r[1] + (r[2] && r[2] !== r[1] ? '–' + r[2] : '')).join(', ') + '.'))));
        nav = [h('button', { class: 'btn', onclick: () => { G.step = 1; renderGroup(); } }, 'Back'), h('button', { class: 'btn primary', disabled: !okr, onclick: () => { G.step = 3; renderGroup(); } }, 'Continue')]; }
    else { pane = h('div', { class: 'notice acc' }, h('b', null, 'What this will do'), h('div', null, 'Creates a ' + G.kind + (G.template ? ' (' + G.template + ')' : '') + ' structure for ' + PARTY + ' and ' + cpty([...cps][0]).name + '.'), h('div', null, 'Links ' + CHOSEN.length + ' trades as legs. No trade is rewritten.'),
        h('div', { class: 'mono small' }, 'trading.v1.structures.put, structure_members.put_many, trade_links.put_many'));
        nav = [h('button', { class: 'btn', onclick: () => { G.step = 2; renderGroup(); } }, 'Back'), h('button', { class: 'btn primary', onclick: () => {
            const id = 'STR-0' + (100 + Object.keys(STRUCTS).length); const rec = { kind: G.kind, template: G.template, cp: [...cps][0], members: CHOSEN.map(t => [t.code, G.roles[t.code]]), created: '2026-10-11T16:41:00+01:00', by: ME };
            store('structures', Object.assign(store('structures') || {}, { [id]: rec })); G.done = id; renderGroup(); } }, 'Create the structure')]; }
    put(wrap(pageHead({ title: 'Group trades into a structure', lead: 'Hold several trades as one deal, so that they are read and managed together.' }), CAN.structure ? null : h('div', { class: 'notice warn' }, h('b', null, 'You cannot use this screen. '), 'It needs the right to group trades. Ask a tenant administrator for the Trader role.'), rail, pane, h('div', { class: 'filters' }, nav)));
}
if (MODE) renderGroup(); else renderStructure();

/* Keep a counterparty's details current prototype. Self-contained: plain
 * JavaScript, mock data, no framework, no build step, no fetch, and nothing
 * that outlives the page.
 *
 * The counterparty as a record screen, drawn like the parties screen and not as
 * a workflow: a list, then one record made of several kinds of row, each on a
 * tab, each card or table changed in a dialog. A counterparty carries more than
 * a party does -- the parties that may trade with it -- so the record has a
 * link tab the party record has no twin for.
 *
 * Which changes need a second person is the approval policy's, as the book
 * controls note states it:
 *   counterparty (put)                 needs Operations
 *   counterparty identifier (put)      needs Operations
 *   counterparty party link (put/delete) needs Operations
 *   counterparty contact information   needs nobody: written directly
 *   counterparty business centre       needs nobody: written directly
 * A dialog never writes. It collects the change, the review panel shows each
 * change with the function that decides it, and the primary button either saves
 * or raises one request. A raised request is drawn by the shared request
 * screens: waiting, the checker's decision, and a decline.
 *
 * Every field below is a column the models carry:
 *   ores.refdata.counterparty, counterparty_identifier,
 *   counterparty_contact_information, counterparty_business_centre_junction,
 *   party_counterparty_junction.
 *
 * States are chosen from the query string, as the sibling prototypes do:
 *   ?state=list                        the counterparty list
 *   ?state=details|identifiers|contacts|centres|parties|history
 *                                      one record, on the named tab
 *   ?state=edit                        the details dialog, form step
 *   ?state=review-gated                the review of changes that need Operations
 *   ?state=review-direct               the review of a contact change, written directly
 *   ?state=new                         the new counterparty dialog
 *   ?state=waiting|decide|declined     the raised request, the checker's decision, a decline
 *   ?state=outcome                     the request applied
 *   ?cp=ATLAS                          the open counterparty, by short code
 *   ?query=Atlas                       the list search, as typed
 *   ?actor=f.ledger|o.ops|...          who answers on the decision screen
 *   ?maker=h.desk|f.ledger|...         who raised the request; the maker cannot decide it
 *   ?check=stale                       make the version check fail
 * The bar at the foot mirrors the states. */

(function () {
    'use strict';

    /* ------------------------------------------------------------ mock data */

    var TYPES = ['Bank', 'Clearing house', 'Broker', 'Insurer', 'Fund'];
    var STATUSES = ['Active', 'Inactive', 'Pending', 'Closed'];
    var SCHEMES = ['LEI', 'BIC', 'MIC', 'NATIONAL_ID', 'INTERNAL'];
    var CONTACT_TYPES = ['Legal', 'Operations', 'Settlement', 'Billing'];
    var CENTRES = { GBLO: 'London', USNY: 'New York', SGSL: 'Singapore', DEFR: 'Frankfurt', IEDU: 'Dublin' };
    var PARTIES = { NWCAP: 'Northwind Capital Ltd', NWMKT: 'Northwind Markets LLC', NWAPAC: 'Northwind Asia Pacific Pte Ltd' };
    var REASONS = [
        { code: 'common.rectification', text: 'Correct an error' },
        { code: 'common.non_material_update', text: 'Routine update' },
        { code: 'common.regulatory', text: 'Regulatory change' }
    ];
    var DECIDER = 'Operations';

    var CPS = [
        { code: 'ATLAS', name: 'Atlas Clearing House Ltd', translit: '', type: 'Clearing house', status: 'Active',
            parent: '', centres: ['GBLO', 'DEFR'], parties: ['NWCAP', 'NWMKT'], version: 4, by: 'tenant_admin', at: '2026-09-30 11:04',
            identifiers: [
                { scheme: 'LEI', value: '5493001ATLASCLEARING1', desc: 'GLEIF LEI', version: 2 },
                { scheme: 'BIC', value: 'ATLSGB2L', desc: 'Head office BIC', version: 1 }
            ],
            contacts: [
                { type: 'Operations', line1: '10 Exchange Square', city: 'London', country: 'GB', phone: '+44 20 7100 0001', email: 'ops@atlas.example', version: 3 },
                { type: 'Legal', line1: '10 Exchange Square', city: 'London', country: 'GB', phone: '+44 20 7100 0002', email: 'legal@atlas.example', version: 1 }
            ],
            history: [
                { version: 4, by: 'tenant_admin', at: '2026-09-30 11:04', reason: 'common.rectification', change: 'Status: Pending → Active' },
                { version: 3, by: 'j.silva', at: '2026-08-21 15:12', reason: 'common.non_material_update', change: 'Transliterated name set' },
                { version: 1, by: 'tenant_admin', at: '2026-06-11 13:20', reason: 'common.new', change: 'Created' }
            ] },
        { code: 'MERID', name: 'Meridian Bank plc', translit: '', type: 'Bank', status: 'Active',
            parent: '', centres: ['GBLO', 'USNY'], parties: ['NWCAP'], version: 2, by: 'a.reyes', at: '2026-07-04 09:47',
            identifiers: [{ scheme: 'LEI', value: '549300MERIDIANBANK02', desc: 'GLEIF LEI', version: 1 }],
            contacts: [{ type: 'Settlement', line1: '1 Canada Square', city: 'London', country: 'GB', phone: '+44 20 7200 0003', email: 'settle@meridian.example', version: 1 }],
            history: [{ version: 2, by: 'a.reyes', at: '2026-07-04 09:47', reason: 'common.rectification', change: 'Full name corrected' },
                { version: 1, by: 'tenant_admin', at: '2026-06-11 13:22', reason: 'common.new', change: 'Created' }] },
        { code: 'KESTR', name: 'Kestrel Futures S.A.', translit: '', type: 'Broker', status: 'Pending',
            parent: '', centres: ['DEFR'], parties: [], version: 1, by: 'm.okafor', at: '2026-05-02 17:35',
            identifiers: [], contacts: [],
            history: [{ version: 1, by: 'm.okafor', at: '2026-05-02 17:35', reason: 'common.new', change: 'Created' }] },
        { code: 'HARBR', name: 'Harbor Reinsurance AG', translit: '', type: 'Insurer', status: 'Inactive',
            parent: 'MERID', centres: ['IEDU'], parties: ['NWAPAC'], version: 3, by: 'j.silva', at: '2026-04-14 10:30',
            identifiers: [{ scheme: 'NATIONAL_ID', value: 'CHE-123.456.789', desc: 'Swiss UID', version: 1 }],
            contacts: [], history: [{ version: 3, by: 'j.silva', at: '2026-04-14 10:30', reason: 'common.regulatory', change: 'Status: Active → Inactive' }] }
    ];

    var fe;
    var S = {
        view: 'list', cp: 'ATLAS', tab: 'details', query: '',
        dialog: null, request: null, toast: '', reqState: 'waiting', nextRequest: 513
    };

    /* -------------------------------------------------------------- helpers */

    function esc(v) {
        return String(v === null || v === undefined ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function byCode(code) {
        return CPS.filter(function (c) { return c.code === code; })[0];
    }

    function cur() { return byCode(S.cp) || CPS[0]; }

    function options(list, selected, label) {
        return list.map(function (v) {
            var value = typeof v === 'string' ? v : v.value;
            var text = typeof v === 'string' ? (label ? label(v) : v) : v.text;
            return '<option value="' + esc(value) + '"' + (value === selected ? ' selected' : '') + '>' + esc(text) + '</option>';
        }).join('');
    }

    function pendingFor(code) {
        return S.request && S.request.cp === code && S.request.live ? S.request : null;
    }

    function nowStamp() { return '2026-10-12 15:00'; }

    /* ------------------------------------------------------- the four eyes */

    function createFourEyes() {
        fe = FourEyes.create({
            requestId: 'REQ-0512',
            maker: 'h.desk',
            getChanges: function () { return S.request ? S.request.changes : []; },
            subject: function () { return S.request ? 'The counterparty ' + S.request.name : 'The counterparty'; },
            checks: function (actor, changes, forced) {
                var req = S.request;
                var live = req ? byCode(req.cp) : null;
                if (req && !live) {
                    return [{ ok: true, label: 'The short code is free', detail: req.cp + ' is not used by another counterparty.' }];
                }
                var stale = forced === 'stale' || (req && live.version !== req.baseVersion);
                return [{
                    ok: !stale,
                    label: 'The counterparty is still at the version the maker read',
                    detail: stale ? 'It moved to version ' + (live ? live.version : '?') + ' since the request was raised.' :
                        'Version ' + (req ? req.baseVersion : '') + ' is current.'
                }];
            }
        });
    }

    /* ----------------------------------------------------------- the pieces */

    function badge(text, kind) {
        return '<span class="badge ' + (kind || '') + '">' + esc(text) + '</span>';
    }

    function statusBadge(status) {
        return badge(status, status === 'Active' ? 'active' : status === 'Pending' ? 'pending' : 'inactive');
    }

    function crumb(parts) {
        return '<div class="faint" style="margin-bottom:6px">' + parts.join(' › ') + '</div>';
    }

    function link(text, go) {
        return '<a href="#" class="dim" data-go="' + esc(go) + '">' + esc(text) + '</a>';
    }

    function toastHtml() {
        if (S.toast === '') return '';
        var t = S.toast;
        return '<div class="notice success">' + esc(t) + '</div>';
    }

    /* ---------------------------------------------------------------- list */

    function listView() {
        var q = S.query.toLowerCase();
        var rows = CPS.filter(function (c) {
            return q === '' || c.name.toLowerCase().indexOf(q) >= 0 || c.code.toLowerCase().indexOf(q) >= 0;
        }).map(function (c) {
            var p = pendingFor(c.code);
            return '<tr class="clickable" data-open="' + esc(c.code) + '"><td class="mono">' + esc(c.code) + '</td><td>' + esc(c.name) +
                (p ? ' ' + badge('request waiting', 'pending') : '') + '</td><td>' + esc(c.type) + '</td><td>' + statusBadge(c.status) +
                '</td><td class="num">' + c.centres.length + '</td><td class="num">' + c.parties.length + '</td><td class="num">v' + c.version + '</td></tr>';
        }).join('');
        return '<div class="page">' + crumb(['Reference data', 'Counterparties']) +
            '<h1>Counterparties</h1><p class="lede">The entities the tenant trades with: their identifiers, contacts and the parties and business centres they connect to.</p>' +
            toastHtml() +
            '<div class="searchrow"><label class="field"><span class="lbl">Search</span><input data-focus="query" data-query="1" placeholder="Short code or name" value="' + esc(S.query) + '"></label>' +
            '<button type="button" class="btn primary" data-act="new">New counterparty</button></div>' +
            '<div class="countline faint">' + (rows === '' ? 'No counterparty matches.' : '') + '</div>' +
            '<table class="table"><thead><tr><th>Short code</th><th>Full name</th><th>Type</th><th>Status</th><th class="num">Business centres</th><th class="num">Trading parties</th><th class="num">Version</th></tr></thead><tbody>' + rows + '</tbody></table></div>';
    }

    /* -------------------------------------------------------------- record */

    var TABS = [['details', 'Details'], ['identifiers', 'Identifiers'], ['contacts', 'Contacts'],
        ['centres', 'Business centres'], ['parties', 'Trading parties'], ['history', 'History']];

    function tabsHtml() {
        return '<div class="tabs">' + TABS.map(function (t) {
            return '<button type="button" class="tab' + (S.tab === t[0] ? ' on' : '') + '" data-tab="' + t[0] + '">' + t[1] + '</button>';
        }).join('') + '</div>';
    }

    function banner(c) {
        var p = pendingFor(c.code);
        if (!p) return '';
        return '<div class="notice warn"><b>A request is waiting.</b> ' + esc(p.id) + ' changes this counterparty and needs ' + esc(DECIDER) +
            '. The record stays at version ' + c.version + ' until it is approved. <a href="#" data-go="request">Open the request</a></div>';
    }

    function kv(label, value) {
        return '<dt>' + esc(label) + '</dt><dd>' + (value === '' ? '<span class="faint">—</span>' : esc(value)) + '</dd>';
    }

    function detailsTab(c) {
        return '<div class="card"><div class="stepheader"><span class="nm">Counterparty</span><span class="ml-auto"><button type="button" class="btn small" data-act="edit">Edit</button></span></div>' +
            '<dl class="reviewgrid">' + kv('Short code', c.code) + kv('Full name', c.name) + kv('Transliterated name', c.translit) + kv('Type', c.type) +
            kv('Status', c.status) + kv('Parent counterparty', c.parent === '' ? '' : byCode(c.parent).name) + '</dl>' +
            '<p class="hint">Changing any of these needs ' + esc(DECIDER) + '. The short code cannot be changed once the counterparty exists.</p></div>';
    }

    function identifiersTab(c) {
        var rows = c.identifiers.map(function (i, n) {
            return '<tr><td>' + esc(i.scheme) + '</td><td class="mono">' + esc(i.value) + '</td><td>' + esc(i.desc) + '</td><td class="num">v' + i.version +
                '</td><td class="actions"><button type="button" class="btn small" data-act="edit-identifier" data-n="' + n + '">Edit</button></td></tr>';
        }).join('');
        return '<div class="card"><div class="stepheader"><span class="nm">Identifiers</span><span class="ml-auto"><button type="button" class="btn small primary" data-act="add-identifier">Add identifier</button></span></div>' +
            '<table class="table"><thead><tr><th>Scheme</th><th>Value</th><th>Description</th><th class="num">Version</th><th></th></tr></thead><tbody>' +
            (rows || '<tr><td colspan="5" class="faint">No identifier is recorded.</td></tr>') + '</tbody></table>' +
            '<p class="hint">An identifier change needs ' + esc(DECIDER) + '.</p></div>';
    }

    function contactsTab(c) {
        var rows = c.contacts.map(function (k, n) {
            return '<tr><td>' + esc(k.type) + '</td><td>' + esc(k.line1) + ', ' + esc(k.city) + ' ' + esc(k.country) + '</td><td>' + esc(k.phone) + '</td><td>' + esc(k.email) +
                '</td><td class="actions"><button type="button" class="btn small" data-act="edit-contact" data-n="' + n + '">Edit</button></td></tr>';
        }).join('');
        return '<div class="card"><div class="stepheader"><span class="nm">Contacts</span><span class="ml-auto"><button type="button" class="btn small primary" data-act="add-contact">Add contact</button></span></div>' +
            '<table class="table"><thead><tr><th>Type</th><th>Address</th><th>Phone</th><th>Email</th><th></th></tr></thead><tbody>' +
            (rows || '<tr><td colspan="5" class="faint">No contact is recorded.</td></tr>') + '</tbody></table>' +
            '<p class="hint">A contact change needs nobody: it is written directly, as a new version.</p></div>';
    }

    function linkPanel(title, items, kind, addList, hint) {
        var rows = items.map(function (it) {
            return '<li><span class="grow">' + esc(it.text) + '</span><button type="button" class="btn small danger" data-act="remove-' + kind + '" data-v="' + esc(it.value) + '">Remove</button></li>';
        }).join('');
        var add = addList.length === 0 ? '<span class="faint">Everything is already linked.</span>' :
            '<select data-pick="' + kind + '">' + options(addList, addList[0].value) + '</select> <button type="button" class="btn small primary" data-act="add-' + kind + '">Add</button>';
        return '<div class="card"><div class="stepheader"><span class="nm">' + esc(title) + '</span></div><ul class="rowlist">' +
            (rows || '<li class="faint">None.</li>') + '</ul><div style="margin-top:10px">' + add + '</div><p class="hint">' + hint + '</p></div>';
    }

    function centresTab(c) {
        var items = c.centres.map(function (k) { return { value: k, text: k + ' · ' + CENTRES[k] }; });
        var rest = Object.keys(CENTRES).filter(function (k) { return c.centres.indexOf(k) < 0; })
            .map(function (k) { return { value: k, text: k + ' · ' + CENTRES[k] }; });
        return linkPanel('Business centres', items, 'centre', rest, 'A business centre link needs nobody: it is written directly.');
    }

    function partiesTab(c) {
        var items = c.parties.map(function (k) { return { value: k, text: k + ' · ' + PARTIES[k] }; });
        var rest = Object.keys(PARTIES).filter(function (k) { return c.parties.indexOf(k) < 0; })
            .map(function (k) { return { value: k, text: k + ' · ' + PARTIES[k] }; });
        return linkPanel('Parties that may trade with it', items, 'party', rest,
            'Linking or unlinking a party changes who may trade with this counterparty, so it needs ' + esc(DECIDER) + '.');
    }

    function historyTab(c) {
        var rows = c.history.map(function (h) {
            return '<tr><td class="num">v' + h.version + '</td><td>' + esc(h.at) + '</td><td>' + esc(h.by) + '</td><td class="mono">' + esc(h.reason) + '</td><td>' + esc(h.change) + '</td></tr>';
        }).join('');
        return '<div class="card"><div class="stepheader"><span class="nm">History</span></div><table class="table"><thead><tr><th class="num">Version</th><th>When</th><th>Who</th><th>Reason</th><th>What changed</th></tr></thead><tbody>' + rows +
            '</tbody></table><p class="hint">Every version is kept. A request that was declined writes no version.</p></div>';
    }

    function recordView() {
        var c = cur();
        var body = { details: detailsTab, identifiers: identifiersTab, contacts: contactsTab, centres: centresTab, parties: partiesTab, history: historyTab }[S.tab](c);
        return '<div class="page">' + crumb([link('Counterparties', 'list'), esc(c.name)]) +
            '<h1>' + esc(c.name) + ' ' + statusBadge(c.status) + ' <span class="faint" style="font-size:13px">v' + c.version + '</span></h1>' +
            toastHtml() + banner(c) + tabsHtml() + body + '</div>';
    }

    /* -------------------------------------------------------------- dialogs */

    function field(label, name, value, extra) {
        return '<label class="field"><span class="lbl">' + esc(label) + '</span><input data-d="' + name + '" data-focus="d-' + name + '" value="' + esc(value) + '"' + (extra || '') + '></label>';
    }

    function select(label, name, list, value) {
        return '<label class="field"><span class="lbl">' + esc(label) + '</span><select data-d="' + name + '">' + options(list, value) + '</select></label>';
    }

    function dialogForm(d) {
        var f = d.values;
        var body = '';
        if (d.kind === 'details') {
            body = '<div class="grid2">' +
                (d.isNew ? field('Short code', 'code', f.code) : '<label class="field"><span class="lbl">Short code</span><input value="' + esc(f.code) + '" disabled></label>') +
                field('Full name', 'name', f.name) + field('Transliterated name', 'translit', f.translit) +
                select('Type', 'type', TYPES, f.type) + select('Status', 'status', STATUSES, f.status) +
                select('Parent counterparty', 'parent', [{ value: '', text: '— none —' }].concat(CPS.filter(function (c) { return c.code !== f.code; }).map(function (c) { return { value: c.code, text: c.name }; })), f.parent) +
                '</div>';
        } else if (d.kind === 'identifier') {
            body = '<div class="grid2">' + select('Scheme', 'scheme', SCHEMES, f.scheme) + field('Value', 'value', f.value) + '<div class="span2">' + field('Description', 'desc', f.desc) + '</div></div>';
        } else if (d.kind === 'contact') {
            body = '<div class="grid2">' + select('Contact type', 'type', CONTACT_TYPES, f.type) + field('Street', 'line1', f.line1) + field('City', 'city', f.city) +
                field('Country', 'country', f.country) + field('Phone', 'phone', f.phone) + field('Email', 'email', f.email) + '</div>';
        }
        return body;
    }

    function dialogTitle(d) {
        return { details: d.isNew ? 'New counterparty' : 'Edit the counterparty', identifier: d.index === null ? 'Add an identifier' : 'Edit the identifier',
            contact: d.index === null ? 'Add a contact' : 'Edit the contact' }[d.kind];
    }

    function reviewPanel(d) {
        var changes = d.changes;
        var gated = fe.gated(changes);
        var list = changes.length === 0 ? '<div class="notice info">Nothing changed. There is nothing to save.</div>' :
            '<ul class="rowlist">' + changes.map(function (ch) {
                return '<li><span class="grow"><b>' + esc(ch.what) + '</b><span class="sub">' + esc(ch.who) + (ch.from !== null && ch.from !== undefined ? ' · ' + esc(ch.from) + ' → ' + esc(ch.to) : ' · ' + esc(ch.to)) + '</span></span>' + fe.badge(ch) + '</li>';
            }).join('') + '</ul>';
        var reason = changes.length === 0 ? '' : '<div class="grid2">' + select('Change reason', 'reason', REASONS.map(function (r) { return { value: r.code, text: r.text }; }), d.reason) +
            (gated.length > 0 ? field('Why you ask', 'why', d.why) : field('Commentary', 'why', d.why)) + '</div>';
        var label = fe.primaryLabel(changes, 'Save');
        var blocked = changes.length === 0 || (gated.length > 0 && d.why.trim() === '');
        return '<h2 style="margin-top:0">Review the changes</h2><p class="lede">Each change is shown with the function that decides it.</p>' + list +
            (changes.length === 0 ? '' : fe.reviewNotice(changes)) + reason +
            '<div class="stepfoot">' + (d.kind === 'link' ? '' : '<button type="button" class="btn ghost" data-act="back">Back</button>') + '<span class="ml-auto"></span>' +
            '<button type="button" class="btn ghost" data-act="close">Cancel</button> <button type="button" class="btn primary" data-act="confirm"' + (blocked ? ' disabled' : '') + '>' + esc(label) + '</button></div>';
    }

    function dialogView() {
        var d = S.dialog;
        var inner = d.step === 'form' ?
            '<h2 style="margin-top:0">' + esc(dialogTitle(d)) + '</h2>' + dialogForm(d) +
            '<div class="stepfoot"><span class="ml-auto"></span><button type="button" class="btn ghost" data-act="close">Cancel</button> <button type="button" class="btn primary" data-act="review">Review changes</button></div>' :
            reviewPanel(d);
        return '<div class="modal"><div class="modalcard card">' + inner + '</div></div>';
    }

    /* ---------------------------------------------------- building changes */

    function openDetails(isNew) {
        var c = isNew ? { code: '', name: '', translit: '', type: 'Bank', status: 'Pending', parent: '' } : cur();
        S.dialog = { kind: 'details', isNew: isNew, step: 'form', index: null, reason: 'common.rectification', why: '', changes: [],
            values: { code: c.code, name: c.name, translit: c.translit, type: c.type, status: c.status, parent: c.parent } };
    }

    function openIdentifier(n) {
        var i = n === null ? { scheme: 'LEI', value: '', desc: '' } : cur().identifiers[n];
        S.dialog = { kind: 'identifier', step: 'form', index: n, reason: 'common.rectification', why: '', changes: [],
            values: { scheme: i.scheme, value: i.value, desc: i.desc } };
    }

    function openContact(n) {
        var k = n === null ? { type: 'Operations', line1: '', city: '', country: '', phone: '', email: '' } : cur().contacts[n];
        S.dialog = { kind: 'contact', step: 'form', index: n, reason: 'common.non_material_update', why: '', changes: [],
            values: { type: k.type, line1: k.line1, city: k.city, country: k.country, phone: k.phone, email: k.email } };
    }

    /* A change: what it is, the account that writes it, from and to, and the
     * function that must approve it (empty when nobody must). */
    function change(what, from, to, decider, apply) {
        return { what: what, who: 'Counterparty ' + (S.dialog && S.dialog.values && S.dialog.values.code ? S.dialog.values.code : cur().code), from: from, to: to, decider: decider, apply: apply };
    }

    function buildChanges(d) {
        var f = d.values, c = cur(), out = [];
        if (d.kind === 'details') {
            if (d.isNew) {
                out.push(change('Create the counterparty ' + f.name, null, f.code + ' · ' + f.type, DECIDER, function () {
                    CPS.push({ code: f.code, name: f.name, translit: f.translit, type: f.type, status: f.status, parent: f.parent, centres: [], parties: [],
                        version: 0, by: 'h.desk', at: nowStamp(), identifiers: [], contacts: [], history: [] });
                    S.cp = f.code;
                }));
            } else {
                [['name', 'Full name'], ['translit', 'Transliterated name'], ['type', 'Type'], ['status', 'Status'], ['parent', 'Parent counterparty']].forEach(function (p) {
                    if (c[p[0]] !== f[p[0]]) {
                        out.push(change(p[1], c[p[0]] || '—', f[p[0]] || '—', DECIDER, function () { c[p[0]] = f[p[0]]; }));
                    }
                });
            }
        } else if (d.kind === 'identifier') {
            var i = d.index === null ? null : c.identifiers[d.index];
            if (i === null) {
                out.push(change('Add the identifier ' + f.scheme, null, f.value, DECIDER, function () { c.identifiers.push({ scheme: f.scheme, value: f.value, desc: f.desc, version: 1 }); }));
            } else {
                [['scheme', 'Scheme'], ['value', 'Value'], ['desc', 'Description']].forEach(function (p) {
                    if (i[p[0]] !== f[p[0]]) {
                        out.push(change('Identifier ' + i.scheme + ': ' + p[1], i[p[0]], f[p[0]], DECIDER, function () { i[p[0]] = f[p[0]]; i.version += 1; }));
                    }
                });
            }
        } else if (d.kind === 'contact') {
            var k = d.index === null ? null : c.contacts[d.index];
            if (k === null) {
                out.push(change('Add a ' + f.type + ' contact', null, f.email || f.phone, '', function () { c.contacts.push({ type: f.type, line1: f.line1, city: f.city, country: f.country, phone: f.phone, email: f.email, version: 1 }); }));
            } else {
                [['type', 'Type'], ['line1', 'Street'], ['city', 'City'], ['country', 'Country'], ['phone', 'Phone'], ['email', 'Email']].forEach(function (p) {
                    if (k[p[0]] !== f[p[0]]) {
                        out.push(change('Contact ' + k.type + ': ' + p[1], k[p[0]], f[p[0]], '', function () { k[p[0]] = f[p[0]]; k.version += 1; }));
                    }
                });
            }
        }
        return out;
    }

    function openLinkChange(kind, adding, value) {
        var c = cur();
        var gated = kind === 'party';
        var text = kind === 'party' ? value + ' · ' + PARTIES[value] : value + ' · ' + CENTRES[value];
        var list = kind === 'party' ? c.parties : c.centres;
        var ch = change((adding ? 'Link ' : 'Unlink ') + (kind === 'party' ? 'the party ' : 'the business centre ') + text, adding ? null : 'linked', adding ? 'linked' : 'unlinked',
            gated ? DECIDER : '', function () {
                if (adding) list.push(value); else list.splice(list.indexOf(value), 1);
            });
        S.dialog = { kind: 'link', step: 'review', index: null, reason: 'common.rectification', why: '', changes: [ch], values: {} };
    }

    /* ----------------------------------------------------- confirm and apply */

    function applyChanges(changes, reason, who) {
        var c = cur();
        changes.forEach(function (ch) { ch.apply({ reason: reason }); });
        c = cur();
        c.version += 1;
        c.by = who;
        c.at = nowStamp();
        c.history.unshift({ version: c.version, by: who, at: nowStamp(), reason: reason, change: changes.map(function (ch) { return ch.what; }).join('; ') });
    }

    function confirm() {
        var d = S.dialog;
        var changes = d.changes;
        if (changes.length === 0) return;
        if (fe.gated(changes).length === 0) {
            applyChanges(changes, d.reason, 'h.desk');
            S.toast = 'Saved as version ' + cur().version + '. No approval was needed.';
            S.dialog = null;
            return;
        }
        var id = 'REQ-0' + S.nextRequest;
        var isNew = d.kind === 'details' && d.isNew;
        S.request = { id: id, name: isNew ? d.values.name : cur().name, cp: isNew ? d.values.code : cur().code, baseVersion: isNew ? 0 : cur().version, changes: changes, reason: d.reason, why: d.why, live: true };
        fe.raise(changes);
        S.dialog = null;
        S.view = 'request';
        S.reqState = 'waiting';
    }

    /* ---------------------------------------------------------- the request */

    function requestView() {
        var c = (S.request ? byCode(S.request.cp) : null) || cur();
        var inner;
        if (S.reqState === 'outcome') {
            inner = '<div class="fe-center"><div class="fe-mark">✓</div><h2>The request was approved</h2><p>Every decider answered. ' + esc(c.name) + ' is now at version ' + c.version +
                ' and the history records who asked and who approved.</p></div>' +
                '<div class="fe-cards"><button type="button" class="fe-card" data-go="record"><b>Open the counterparty</b>See the new version.</button></div>';
        } else {
            inner = fe.html(S.reqState);
        }
        return '<div class="page narrow">' + crumb([link('Requests', 'list'), S.request ? esc(S.request.id) : 'Request']) + '<h1>Request ' + esc(S.request ? S.request.id : '') + '</h1>' + inner + '</div>';
    }

    /* ---------------------------------------------------------------- render */

    function render() {
        var app = document.getElementById('app');
        var active = document.activeElement;
        var focusId = active && active.getAttribute ? active.getAttribute('data-focus') : null;
        var caret = active && active.selectionStart !== undefined ? active.selectionStart : null;
        var html = S.view === 'list' ? listView() : S.view === 'request' ? requestView() : recordView();
        if (S.dialog) html += dialogView();
        app.innerHTML = html;
        renderBar();
        if (focusId) {
            var el = app.querySelector('[data-focus="' + focusId + '"]');
            if (el) { el.focus(); if (caret !== null && el.setSelectionRange) el.setSelectionRange(caret, caret); }
        }
    }

    function renderBar() {
        var states = [['list', 'List'], ['details', 'Details'], ['identifiers', 'Identifiers'], ['contacts', 'Contacts'], ['centres', 'Centres'], ['parties', 'Parties'], ['history', 'History'],
            ['edit', 'Edit'], ['review-gated', 'Review (needs approval)'], ['review-direct', 'Review (direct)'], ['new', 'New'], ['waiting', 'Waiting'], ['decide', 'Decide'], ['declined', 'Declined'], ['outcome', 'Outcome']];
        document.getElementById('proto-bar').innerHTML = states.map(function (s) {
            return '<button type="button" data-state="' + s[0] + '">' + s[1] + '</button>';
        }).join('');
    }

    /* ----------------------------------------------------------- the states */

    function goState(name, p) {
        S.toast = '';
        S.dialog = null;
        if (name === 'list') { S.view = 'list'; return; }
        if (TABS.some(function (t) { return t[0] === name; })) { S.view = 'record'; S.tab = name; return; }
        S.view = 'record';
        if (name === 'edit') { S.tab = 'details'; openDetails(false); return; }
        if (name === 'new') { S.view = 'list'; openDetails(true); S.dialog.values = { code: 'NEWCO', name: 'Newco Clearing Ltd', translit: '', type: 'Clearing house', status: 'Pending', parent: '' }; return; }
        if (name === 'review-gated') {
            S.tab = 'details'; openDetails(false);
            S.dialog.values.name = 'Atlas Clearing House Limited'; S.dialog.values.status = 'Inactive';
            S.dialog.changes = buildChanges(S.dialog); S.dialog.step = 'review'; S.dialog.why = 'Atlas has been renamed and is paused';
            return;
        }
        if (name === 'review-direct') {
            S.tab = 'contacts'; openContact(0);
            S.dialog.values.phone = '+44 20 7100 0099';
            S.dialog.changes = buildChanges(S.dialog); S.dialog.step = 'review';
            return;
        }
        if (name === 'waiting' || name === 'decide' || name === 'declined' || name === 'outcome') {
            S.tab = 'details'; openDetails(false);
            S.dialog.values.name = 'Atlas Clearing House Limited'; S.dialog.values.status = 'Inactive';
            S.dialog.changes = buildChanges(S.dialog); S.dialog.why = 'Atlas has been renamed and is paused';
            confirm();
            S.view = 'request';
            if (name === 'outcome') {
                fe.S.answers[DECIDER] = 'approved';
                applyChanges(S.request.changes, S.request.reason, 'o.ops');
                S.request.live = false;
            }
            S.reqState = name;
        }
    }

    /* ----------------------------------------------------------------- events */

    function onClick(ev) {
        var el = ev.target;
        while (el && el !== document && !(el.getAttribute && (el.getAttribute('data-act') || el.getAttribute('data-tab') || el.getAttribute('data-open') ||
            el.getAttribute('data-go') || el.getAttribute('data-state') || el.getAttribute('data-fe')))) el = el.parentNode;
        if (!el || el === document) return;
        var act = el.getAttribute('data-act');
        var n = el.getAttribute('data-n');
        var index = n === null ? null : parseInt(n, 10);
        if (el.getAttribute('data-fe') !== null) {
            var next = fe.click(el);
            if (next === 'review') { S.request.live = false; S.request = null; S.view = 'record'; S.tab = 'details'; }
            else if (next === 'outcome') { applyChanges(S.request.changes, S.request.reason, fe.actors.filter(function (a) { return a.id === fe.S.actor; })[0].id); S.request.live = false; S.reqState = 'outcome'; }
            else if (next === 'waiting' || next === 'decide' || next === 'declined') { S.reqState = next; if (next === 'declined') S.request.live = false; }
            render();
            ev.preventDefault();
            return;
        }
        ev.preventDefault();
        if (el.getAttribute('data-state')) { goState(el.getAttribute('data-state')); }
        else if (el.getAttribute('data-open')) { S.cp = el.getAttribute('data-open'); S.view = 'record'; S.tab = 'details'; S.toast = ''; }
        else if (el.getAttribute('data-tab')) { S.tab = el.getAttribute('data-tab'); S.toast = ''; }
        else if (el.getAttribute('data-go')) {
            var go = el.getAttribute('data-go');
            S.toast = '';
            S.view = go === 'list' ? 'list' : go === 'request' ? 'request' : 'record';
            if (go === 'request') S.reqState = 'waiting';
        }
        else if (act === 'new') { goState('new'); S.dialog.values = { code: '', name: '', translit: '', type: 'Bank', status: 'Pending', parent: '' }; }
        else if (act === 'edit') openDetails(false);
        else if (act === 'add-identifier') openIdentifier(null);
        else if (act === 'edit-identifier') openIdentifier(index);
        else if (act === 'add-contact') openContact(null);
        else if (act === 'edit-contact') openContact(index);
        else if (act === 'add-party' || act === 'add-centre') {
            var kind = act === 'add-party' ? 'party' : 'centre';
            openLinkChange(kind, true, document.getElementById('app').querySelector('[data-pick="' + kind + '"]').value);
        }
        else if (act === 'remove-party' || act === 'remove-centre') {
            openLinkChange(act === 'remove-party' ? 'party' : 'centre', false, el.getAttribute('data-v'));
        }
        else if (act === 'review') { S.dialog.changes = buildChanges(S.dialog); S.dialog.step = 'review'; }
        else if (act === 'back') S.dialog.step = 'form';
        else if (act === 'close') S.dialog = null;
        else if (act === 'confirm') confirm();
        render();
    }

    function onInput(ev) {
        var el = ev.target;
        if (fe && fe.input(el)) { render(); return; }
        if (el.getAttribute('data-query') !== null) { S.query = el.value; render(); return; }
        var name = el.getAttribute('data-d');
        if (name === null || !S.dialog) return;
        if (name === 'why') S.dialog.why = el.value;
        else if (name === 'reason') S.dialog.reason = el.value;
        else S.dialog.values[name] = el.value;
        if (el.tagName === 'SELECT' || name === 'why') render();
    }

    function start() {
        createFourEyes();
        var p = new URLSearchParams(window.location.search);
        fe.params(p);
        if (p.get('cp') && byCode(p.get('cp'))) S.cp = p.get('cp');
        if (p.get('query')) S.query = p.get('query');
        if (p.get('state')) goState(p.get('state'));
        // Raising a request chooses the first decider, so the query's own choice is applied last.
        fe.params(p);
        document.getElementById('proto-note').textContent = 'Prototype · mock data · nothing is saved';
        var app = document.getElementById('app');
        app.addEventListener('click', onClick);
        document.addEventListener('click', function (ev) { if (ev.target.closest && ev.target.closest('#proto-bar')) onClick(ev); });
        app.addEventListener('input', onInput);
        app.addEventListener('change', onInput);
        render();
    }

    start();
})();

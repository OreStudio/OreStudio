/* See the conventions a party uses.
 *
 * The plain entity screens of the conventions, drawn as the record screen
 * standard draws every record kind: the list of kinds, the list of one kind's
 * documents, and the record page of one document. The workflows that manage
 * conventions sit in the Workflows menu and open as their own pages, and the
 * requests that wait for a decision are a tab of the record page. A write that
 * needs a second person says Propose. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    var S = {
        view: Q.get('view') || 'kinds',
        kind: Q.get('kind') || 'swap',
        key: Q.get('key') || '',
        tab: Q.get('tab') || 'details',
        q: Q.get('q') || '',
        source: Q.get('source') || 'all',
        scope: Q.get('scope') === 'shared' ? 'shared' : 'party',
        cls: Q.get('class') || 'all',
        sort: { id: 'key', dir: 1 },
        pgK: { page: 0, size: 15 },
        pgD: { page: 0, size: 15 },
        state: Q.get('state') || '',
        empty: Q.get('empty') === '1',
        flash: '',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }
    if (S.key && Q.get('view') === null) { S.view = 'record'; S.scope = M.kind(S.kind) && !M.kind(S.kind).party ? 'shared' : S.scope; }
    if (!M.kind(S.kind)) { S.kind = 'swap'; }

    /* The asset classes of the product model, and the class each kind serves. */
    var CLASSES = ['Interest rates', 'FX', 'Inflation', 'Credit', 'Commodity', 'Bond'];
    var CLASS_OF = {
        average_ois: 'Interest rates', bma_basis_swap: 'Interest rates', cms_spread_option: 'Interest rates', deposit: 'Interest rates',
        fra: 'Interest rates', future: 'Interest rates', ibor_index: 'Interest rates', ois: 'Interest rates', overnight_index: 'Interest rates',
        swap: 'Interest rates', swap_index: 'Interest rates', tenor: 'Interest rates', tenor_basis_swap: 'Interest rates',
        tenor_basis_two_swap: 'Interest rates', zero: 'Interest rates',
        cross_currency_basis: 'FX', cross_currency_fix_float: 'FX', currency_pair: 'FX', fx_option: 'FX',
        inflation_swap: 'Inflation', zero_inflation_index: 'Inflation',
        cds: 'Credit',
        commodity_forward: 'Commodity', commodity_future: 'Commodity', intraday_power_load: 'Commodity',
        bond_yield: 'Bond'
    };
    function classOf(k) { return CLASS_OF[k.key]; }
    function scopeKinds(scope) { return M.KINDS.filter(function (k) { return k.party === (scope === 'party'); }); }

    function canWrite() { return S.person.part === 'Operations' || S.person.part === 'Market Risk'; }
    function docs() { return S.empty ? [] : M.docs(S.kind); }
    function doc() { return M.doc(S.kind, S.key) || M.docs(S.kind)[0]; }

    /* Fixtures: the requests that wait on documents. */
    function requestsFor(d) {
        if (d.key === 'AED-EIBOR-6M-SWAP') { return [{ id: 'r1', t: 'Change AED swap day count', by: 'M. Okafor', at: '2026-10-17T12:00:00Z' }]; }
        if (d.key === 'AED-EIBOR-6M-SWAP-201') { return [{ id: 'r3', t: 'Delete AED-EIBOR-6M-SWAP-201', by: 'M. Okafor', at: '2026-10-19T08:00:00Z' }]; }
        return [];
    }
    function waiting(kind) { return kind === 'swap' ? 2 : 0; }

    function workflows(forDoc) {
        if (!canWrite()) { return []; }
        if (forDoc) {
            return [{ t: 'Retire a convention', d: 'Read what depends on a document, then ask for it to be deleted.',
                href: '../retire-a-convention/?kind=' + S.kind + '&key=' + encodeURIComponent(forDoc.key) + '&actor=' + S.person.id }];
        }
        return [
            { t: 'Find a convention by its terms', d: 'Search a kind for the document that states a set of terms.', href: '../find-a-convention-by-its-terms/?kind=' + (M.modelled(S.kind) ? S.kind : 'swap') + '&actor=' + S.person.id },
            { t: 'Name a convention set', d: 'Group one document of each kind under a name a report can choose.', href: '../name-a-convention-set/?actor=' + S.person.id },
            { t: 'Define instrument conventions', d: 'Start from an instrument and walk to the convention that serves it.', href: '../define-the-conventions-for-an-instrument/' }
        ];
    }

    /* ------------------------------------------------------------ */
    /* The list of kinds                                              */
    /* ------------------------------------------------------------ */

    function kindsView() {
        var cols = [
            { id: 'label', h: 'Kind', cell: function (k) { return M.esc(k.label); }, val: function (k) { return k.label; } },
            { id: 'docs', h: 'Documents', cls: 'right', cell: function (k) { return '<span class="num">' + M.docs(k.key).length + '</span>'; }, val: function (k) { return M.docs(k.key).length; } },
            { id: 'sets', h: 'Term sets', cls: 'right', cell: function (k) { return M.modelled(k.key) ? '<span class="num">' + M.termSets(k.key) + '</span>' : M.dash(''); }, val: function (k) { return M.modelled(k.key) ? M.termSets(k.key) : 0; } },
            { id: 'wait', h: 'Waiting', cell: function (k) { return waiting(k.key) ? M.badge('pending', waiting(k.key) + ' waiting') : M.dash(''); }, val: function (k) { return waiting(k.key); } }
        ];
        var sort = S.sort.id === 'key' ? { id: 'label', dir: 1 } : S.sort;
        var inScope = scopeKinds(S.scope);
        var rows = M.sortRows(inScope.filter(function (k) {
            return (!S.q || (k.label + k.key).toLowerCase().indexOf(S.q.toLowerCase()) >= 0) && (S.cls === 'all' || classOf(k) === S.cls);
        }), cols, sort);
        S.shownKinds = [];
        var body = '';
        CLASSES.forEach(function (c) {
            var inClass = rows.filter(function (k) { return classOf(k) === c; });
            if (!inClass.length) { return; }
            var n = inClass.reduce(function (t, k) { return t + M.docs(k.key).length; }, 0);
            body += '<tr class="grp"><th colspan="' + cols.length + '" scope="rowgroup">' + M.esc(c) + '<span class="dim"> · ' + inClass.length + (inClass.length === 1 ? ' kind' : ' kinds') + ' · ' + n + ' documents</span></th></tr>';
            inClass.forEach(function (k) {
                body += '<tr class="pick" tabindex="0" data-row="' + S.shownKinds.length + '">' +
                    cols.map(function (c2) { return '<td class="' + (c2.cls || '') + '">' + c2.cell(k) + '</td>'; }).join('') + '</tr>';
                S.shownKinds.push(k);
            });
        });
        var head = '<thead><tr>' + cols.map(function (c) {
            var on = sort.id === c.id;
            return '<th class="' + (c.cls || '') + '" aria-sort="' + (on ? (sort.dir > 0 ? 'ascending' : 'descending') : 'none') + '"><button type="button" class="btn ghost small" data-act="sort" data-arg="' + c.id + '">' +
                M.esc(c.h) + (on ? (sort.dir > 0 ? ' ↑' : ' ↓') : '') + '</button></th>';
        }).join('') + '</tr></thead>';
        var total = inScope.reduce(function (t, k) { return t + M.docs(k.key).length; }, 0);
        var acts = M.btn('refresh', 'Refresh', { icon: 'arrow_clockwise' }) + M.workflowMenu(workflows(null));
        var present = CLASSES.filter(function (c) { return inScope.some(function (k) { return classOf(k) === c; }); });
        var tabs = [['party', 'Party conventions', scopeKinds('party').length], ['shared', 'Shared conventions', scopeKinds('shared').length]];
        return M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Conventions' }]) +
            M.pagehead('Conventions', 'The market terms the instruments are built on, grouped by asset class.', acts) +
            '<div class="tabs">' + tabs.map(function (t) {
                return '<button class="' + (S.scope === t[0] ? 'on' : '') + '" data-act="scope" data-arg="' + t[0] + '">' + t[1] + ' (' + t[2] + ')</button>';
            }).join('') + '</div>' +
            (S.scope === 'shared' ? M.notice('warn', '<b>Every party reads these conventions.</b> They are held once for the tenant, and a change applies to all parties.') : '') +
            toolbar('Search kinds', [{ id: 'class', all: 'All asset classes', opts: present.map(function (c) { return { v: c, t: c }; }), v: S.cls }]) +
            '<p class="listnote">' + total + ' documents across ' + inScope.length + ' kinds.</p>' +
            (rows.length === 0 ? '<div class="tblstate">No kinds match the search. ' + M.btn('clear', 'Clear', { cls: 'small' }) + '</div>'
                : '<table class="rows grouped">' + head + '<tbody>' + body + '</tbody></table>') +
            M.gap('The server holds no count of documents or of distinct term sets for a kind, so the screen reads each list to count. Document counts for the kinds without mock terms are fixtures. A kind carries no asset class in the model today, so the screen holds the grouping.');
    }

    function toolbar(placeholder, filters) {
        return '<div class="listtools"><div class="searchbox">' + M.icon('search') + '<input type="text" placeholder="' + M.esc(placeholder) + '" data-input="q" value="' + M.esc(S.q) + '"></div>' +
            filters.map(function (f) {
                return M.select('f.' + f.id, [{ v: 'all', t: f.all }].concat(f.opts), f.v, { inline: true });
            }).join('') + '</div>';
    }

    /* ------------------------------------------------------------ */
    /* The list of one kind's documents                               */
    /* ------------------------------------------------------------ */

    function listCols() {
        var tc = (M.COLUMNS[S.kind] || []).slice(0, 4);
        return [{ id: 'key', h: 'Key', cell: function (d) { return '<span class="key">' + M.esc(d.key) + '</span>'; }, val: function (d) { return d.key; } }]
            .concat(tc.map(function (c) { return { id: c.id, h: c.h, cell: function (d) { return M.dash(d.terms[c.id]); }, val: function (d) { return d.terms[c.id] || ''; } }; }))
            .concat([{ id: 'source', h: 'Source', cell: function (d) { return M.esc(d.source); }, val: function (d) { return d.source; } }]);
    }

    function listView() {
        var k = M.kind(S.kind);
        var cols = listCols();
        var all = docs();
        var rows = all.filter(function (d) {
            return (!S.q || d.key.toLowerCase().indexOf(S.q.toLowerCase()) >= 0) &&
                (S.source === 'all' || (S.source === 'curated') === (d.source === 'Curated dataset'));
        });
        rows = M.sortRows(rows, cols, S.sort);
        S.shown = rows.slice(S.pgD.page * S.pgD.size, (S.pgD.page + 1) * S.pgD.size);
        var acts = M.accessMark(canWrite()) + M.btn('refresh', 'Refresh', { icon: 'arrow_clockwise' }) + M.workflowMenu(workflows(null)) +
            (canWrite() ? M.btn('dialog', 'Add', { cls: 'primary', icon: 'add', arg: 'add' }) : '');
        var body;
        if (S.state === 'loading') { body = '<div class="tblstate">Loading…</div>'; }
        else if (S.state === 'error') { body = M.notice('error', '<b>The ' + M.esc(k.label.toLowerCase()) + ' documents could not be read.</b> The server did not answer. ' + M.btn('retry', 'Retry', { cls: 'small' })); }
        else if (all.length === 0) { body = '<div class="tblstate">There are no ' + M.esc(k.label.toLowerCase()) + ' documents yet. ' + (canWrite() ? M.btn('dialog', 'Add', { cls: 'small primary', arg: 'add' }) : '') + '</div>'; }
        else if (rows.length === 0) { body = '<div class="tblstate">No ' + M.esc(k.label.toLowerCase()) + ' documents match the search. ' + M.btn('clear', 'Clear', { cls: 'small' }) + '</div>'; }
        else { body = M.table(cols, S.shown, { sort: S.sort }); }
        return M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Conventions', act: 'toKinds' }, { t: k.label }]) +
            (S.flash ? M.notice('success', S.flash) : '') +
            M.pagehead(k.label, classOf(k) + ' · the ' + k.label.toLowerCase() + ' documents.', acts) +
            (k.party ? '' : M.notice('warn', '<b>Every party reads these documents.</b> They are held once for the tenant, and a change applies to all parties.')) +
            toolbar('Search by key', [{ id: 'source', all: 'All sources', opts: [{ v: 'curated', t: 'Curated dataset' }, { v: 'authored', t: 'Authored' }], v: S.source }]) +
            (M.modelled(S.kind) && S.state === '' ? '<p class="listnote">' + all.length + ' documents state ' + M.termSets(S.kind) + ' distinct term sets.</p>' : '') +
            body + (S.state === '' && rows.length ? M.pager(S.pgD, rows.length) : '') +
            M.gap('The list filter takes <span class="mono">id_one_of</span> and nothing else, so the search above matches the key through a read of every page. A filter on the term columns is the gap that the Find workflow draws.');
    }

    /* ------------------------------------------------------------ */
    /* The record page of one document                                */
    /* ------------------------------------------------------------ */

    function versions(d) {
        var v = [];
        if (d.version >= 2) {
            v.push({ n: 2, by: 'm.okafor', at: '2026-10-12T09:20:00Z', code: 'common.rectification', note: 'Corrected the float frequency', diff: [['Float frequency', 'Quarterly', 'Semi-Annual']] });
        }
        v.push({ n: 1, by: d.modifiedBy, at: d.recordedAt, code: d.reason, note: d.commentary, diff: null });
        return v;
    }

    function recordView() {
        var d = doc();
        var k = M.kind(S.kind);
        var reqs = requestsFor(d);
        var w = canWrite();
        var acts = M.accessMark(w) + (w ? M.btn('dialog', 'Edit', { icon: 'edit', arg: 'edit' }) : '') + M.workflowMenu(workflows(d));
        var tabs = [['details', 'Details'], ['requests', 'Requests' + (reqs.length ? ' (' + reqs.length + ')' : '')], ['history', 'History']];
        var body;
        if (S.tab === 'requests') {
            body = reqs.length === 0 ? '<p class="dim">None yet.</p>' :
                '<table class="rows"><thead><tr><th>Request</th><th>Asked by</th><th>State</th><th>Asked</th></tr></thead><tbody>' + reqs.map(function (r) {
                    return '<tr><td><a href="../decide-a-convention-change/?actor=t.brandt&request=' + r.id + '">' + M.esc(r.t) + '</a></td><td>' + M.esc(r.by) + '</td><td>' + M.badge('pending', 'Pending') + '</td><td>' + M.relTime(r.at) + '</td></tr>';
                }).join('') + '</tbody></table>';
        } else if (S.tab === 'history') {
            body = '<ol class="timeline">' + versions(d).map(function (v, i) {
                return '<li><div class="when">' + M.relTime(v.at) + '</div><div><div class="head"><b>' + (v.n === 1 ? 'Created' : 'Changed') + '</b><span class="mono">' + M.esc(d.key) + ' v' + v.n + '</span><span class="mono dim">' + M.esc(v.code) + '</span><span class="dim">' + M.esc(v.by) + '</span>' +
                    (i === 0 ? '<span class="cur" title="Current version">Current version</span>' : '') +
                    (i > 0 && w ? M.btn('revert', 'Revert to this version', { cls: 'small ghost', icon: 'arrow_rotate_counterclockwise', arg: v.n }) : '') + '</div><p class="note">' + M.esc(v.note) + '</p>' +
                    (v.diff ? '<details><summary>Details</summary><table class="diff"><thead><tr><th>Field</th><th>Was</th><th>Now</th></tr></thead><tbody>' +
                        v.diff.map(function (r) { return '<tr class="changed"><td>' + M.esc(r[0]) + '</td><td class="was">' + M.esc(r[1]) + '</td><td class="now">' + M.esc(r[2]) + '</td></tr>'; }).join('') + '</tbody></table></details>' : '') + '</div></li>';
            }).join('') + '</ol>';
        } else {
            var cols = M.COLUMNS[S.kind];
            body = (cols ? '<dl class="fields">' + cols.map(function (c) { return '<div><dt>' + M.esc(c.h) + '</dt><dd>' + M.dash(d.terms[c.id]) + '</dd></div>'; }).join('') + '</dl>'
                : '<p class="dim">The terms of this kind are not drawn in the prototype.</p>') +
                '<div class="lastchanged">Last changed: version ' + d.version + ' · ' + M.esc(d.modifiedBy) + ' · ' + M.relTime(d.recordedAt) + ' · ' + M.esc(d.reason) + ' · “' + M.esc(d.commentary) + '”</div>' +
                (k.party ? '' : M.notice('warn', '<b>Every party reads this document.</b> It is held once for the tenant.')).replace(/^/, '<div style="margin-top:16px"></div>');
        }
        return M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Conventions', act: 'toKinds' }, { t: k.label, act: 'toList' }, { t: d.key }]) +
            (S.flash ? M.notice('success', S.flash) : '') +
            M.pagehead(d.key, d.key + ' · version ' + d.version, acts).replace('<h1>', '<h1 class="mono">') +
            '<div class="tabs">' + tabs.map(function (t) { return '<button class="' + (S.tab === t[0] ? 'on' : '') + '" data-act="tab" data-arg="' + t[0] + '">' + t[1] + '</button>'; }).join('') + '</div>' + body;
    }

    /* ------------------------------------------------------------ */
    /* Add and Edit                                                   */
    /* ------------------------------------------------------------ */

    var D = null;

    function openDialog(mode) {
        var cols = M.COLUMNS[S.kind] || [];
        var orig = {};
        if (mode === 'edit') { cols.forEach(function (c) { orig[c.id] = doc().terms[c.id] || ''; }); }
        D = { mode: mode, key: mode === 'edit' ? doc().key : '', vals: JSON.parse(JSON.stringify(orig)), orig: orig, reason: '', commentary: '', tried: false };
        drawDialog();
    }

    function changed() {
        return Object.keys(D.orig).filter(function (id) { return (D.vals[id] || '') !== D.orig[id]; });
    }

    function syncText() {
        var k = document.querySelector('[data-input=dkey]');
        if (k) { D.key = k.value; }
        var c = document.querySelector('[data-input=dcomment]');
        if (c) { D.commentary = c.value; }
    }

    function drawDialog() {
        var k = M.kind(S.kind);
        var cols = M.COLUMNS[S.kind] || [];
        var add = D.mode === 'add';
        var keyBad = D.tried && add && (!D.key.trim() || M.doc(S.kind, D.key.trim()));
        var reasons = M.CHANGE_REASONS.amend.filter(function (r) { return changed().length ? r.v !== 'common.non_material_update' : r.v === 'common.non_material_update'; });
        var reasonBad = D.tried && !add && !D.reason;
        var html = '<h2>' + (add ? 'Add ' + M.esc(k.label.toLowerCase()) + ' document' : 'Edit ' + M.esc(D.key)) + '</h2>' +
            '<p class="lede">' + (add ? 'A new document is written with the reason New record.' : 'Change the terms, then say why.') + '</p>' +
            '<div class="field' + (keyBad ? ' bad' : '') + '"><span class="lbl">Key <span class="req">(' + (add ? 'required' : 'fixed') + ')</span></span><input type="text" data-input="dkey" value="' + M.esc(D.key) + '"' + (add ? '' : ' disabled') + '>' +
            (keyBad ? '<p class="msg">' + (D.key.trim() ? 'A document with the key “' + M.esc(D.key.trim()) + '” exists.' : 'Give the document a key.') + '</p>' : '') + '</div>' +
            (cols.length ? '<div class="grid2">' + cols.map(function (c) {
                var opts = (c.optional ? [{ v: '', t: 'Not stated' }] : []).concat(c.options);
                return '<div class="field"><span class="lbl">' + M.esc(c.h) + '</span>' + M.select('d.' + c.id, opts, D.vals[c.id] === undefined ? '' : D.vals[c.id], { placeholder: 'Choose…' }) + '</div>';
            }).join('') + '</div>' : '<p class="dim">The terms of this kind are not drawn in the prototype.</p>') +
            (add ? '' : '<div class="field' + (reasonBad ? ' bad' : '') + '"><span class="lbl">Reason <span class="req">(required)</span></span>' + M.select('dreason', reasons, D.reason, { placeholder: 'Choose a reason' }) +
                (reasonBad ? '<p class="msg">Choose a reason for the change.</p>' : '') + '</div><div class="field"><span class="lbl">Commentary</span><textarea data-input="dcomment">' + M.esc(D.commentary) + '</textarea></div>') +
            (k.party ? '' : M.notice('warn', 'This document is shared. The change applies to every party.')) +
            M.notice('info', 'Market Risk decides this change. Nothing changes until it approves, and you cannot approve your own request.') +
            '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('propose', 'Propose', { cls: 'primary', disabled: !add && changed().length === 0 && D.reason !== 'common.non_material_update' && false }) + '</div>';
        M.dialog(html, true);
        M.wireSelects(document.getElementById('dialog-scrim'), function (id, v) {
            syncText();
            if (id === 'dreason') { D.reason = v; } else { D.vals[id.slice(2)] = v; D.reason = changed().length && D.reason === 'common.non_material_update' ? '' : D.reason; }
            drawDialog();
        });
    }

    /* ------------------------------------------------------------ */

    function render() {
        var body = S.view === 'kinds' ? kindsView() : S.view === 'list' ? listView() : recordView();
        document.getElementById('app').innerHTML = M.shell('Reference data', S.person) + '<div class="page">' + body + '</div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: 'kinds', t: 'Party kinds' }, { v: 'shared', t: 'Shared kinds' }, { v: 'list', t: 'Swap list' }, { v: 'record', t: 'A document' },
            { v: 'err', t: 'Read error' }, { v: 'readonly', t: 'Read-only person' }], S.view === 'kinds' && S.scope === 'shared' ? 'shared' : S.view);
        var app = document.getElementById('app');
        M.wireSelects(app, function (id, v) {
            if (id === 'pagesize') { (S.view === 'kinds' ? S.pgK : S.pgD).size = Number(v); (S.view === 'kinds' ? S.pgK : S.pgD).page = 0; }
            else if (id === 'f.class') { S.cls = v; }
            else if (id === 'f.source') { S.source = v; S.pgD.page = 0; }
            render();
        });
        M.wireWorkflowMenus(app);
        var qi = document.querySelector('[data-input=q]');
        if (qi) {
            qi.addEventListener('input', function () {
                S.q = qi.value; S.pgK.page = 0; S.pgD.page = 0;
                var pos = qi.selectionStart; render();
                var n = document.querySelector('[data-input=q]'); n.focus(); n.setSelectionRange(pos, pos);
            });
        }
        if (Q.get('menu') === '1' && !render.menuShown) { render.menuShown = true; var m = document.querySelector('.wf-menu'); if (m) { m.hidden = false; } }
    }

    document.addEventListener('click', function (e) {
        var tr = e.target.closest('tr[data-row]');
        if (tr && S.view === 'kinds') { S.kind = S.shownKinds[Number(tr.getAttribute('data-row'))].key; S.q = ''; S.cls = 'all'; S.pgD.page = 0; S.source = 'all'; S.view = 'list'; render(); return; }
        if (tr && S.view === 'list') { S.key = S.shown[Number(tr.getAttribute('data-row'))].key; S.tab = 'details'; S.flash = ''; S.view = 'record'; render(); return; }
        var b = e.target.closest('[data-act]');
        if (!b || b.closest('.sel')) { return; }
        var name = b.getAttribute('data-act');
        var arg = b.getAttribute('data-arg');
        if (name === 'bar') { return bar(arg); }
        if (name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (name === 'toKinds') { S.view = 'kinds'; S.q = ''; S.flash = ''; S.scope = M.kind(S.kind).party ? 'party' : 'shared'; render(); }
        else if (name === 'scope') { S.scope = arg; S.q = ''; S.cls = 'all'; render(); }
        else if (name === 'toList') { S.view = 'list'; S.q = ''; render(); }
        else if (name === 'tab') { S.tab = arg; render(); }
        else if (name === 'sort') { S.sort = { id: arg, dir: S.sort.id === arg ? -S.sort.dir : 1 }; render(); }
        else if (name === 'pg') { var st = S.view === 'kinds' ? S.pgK : S.pgD; M.pageStep(st, arg, S.view === 'kinds' ? M.KINDS.length : docs().length); render(); }
        else if (name === 'clear') { S.q = ''; S.source = 'all'; S.cls = 'all'; render(); }
        else if (name === 'refresh') { S.flash = ''; render(); }
        else if (name === 'retry') { S.state = ''; render(); }
        else if (name === 'dialog') { openDialog(arg); }
        else if (name === 'cancel') { M.closeDialog(); }
        else if (name === 'propose') {
            syncText();
            D.tried = true;
            if (D.mode === 'add' && (!D.key.trim() || M.doc(S.kind, D.key.trim()))) { drawDialog(); return; }
            if (D.mode === 'edit' && !D.reason) { drawDialog(); return; }
            M.closeDialog();
            S.flash = '<b>Request raised.</b> Market Risk decides it. Nothing changes until it approves. <a href="../decide-a-convention-change/?actor=t.brandt">Open Requests</a>';
            render();
        }
        else if (name === 'revert') {
            M.dialog('<h2>Revert to version ' + M.esc(arg) + '?</h2><p class="lede">The values of version ' + M.esc(arg) + ' are written back as a new version. No version is lost.</p>' +
                M.notice('info', 'Market Risk decides this change. Nothing changes until it approves.') +
                '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('cancel', 'Propose', { cls: 'primary' }) + '</div>');
        }
    });
    document.addEventListener('keydown', function (e) {
        var tr = e.target.closest && e.target.closest('tr[data-row]');
        if (tr && e.key === 'Enter') { tr.click(); }
    });

    function bar(arg) {
        S.flash = '';
        if (arg === 'readonly') { S.person = M.PEOPLE['k.mori']; render(); return; }
        if (arg === 'err') { S.view = 'list'; S.state = 'error'; render(); return; }
        S.state = '';
        if (arg === 'shared') { S.scope = 'shared'; arg = 'kinds'; } else if (arg === 'kinds') { S.scope = 'party'; }
        S.view = arg;
        if (arg === 'record' && !S.key) { S.key = 'AED-EIBOR-6M-SWAP'; S.kind = 'swap'; }
        if (arg === 'list') { S.kind = 'swap'; }
        render();
    }

    render();
    if (Q.get('dialog') === 'add' || Q.get('dialog') === 'edit') { openDialog(Q.get('dialog')); }
})();

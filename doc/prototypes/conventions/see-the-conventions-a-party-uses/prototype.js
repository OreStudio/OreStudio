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
        sort: { id: Q.get('sort') || 'key', dir: Q.get('dir') === 'desc' ? -1 : 1 },
        pgK: { page: 0, size: 15 },
        pgD: { page: Math.max(0, Number(Q.get('page') || 1) - 1), size: Number(Q.get('size')) || 15 },
        state: Q.get('state') || '',
        stale: Q.get('stale') === '1',
        loaded: M.LOADED,
        news: Q.get('news') === '1' ? ['AED-EIBOR-6M-SWAP'] : [],
        empty: Q.get('empty') === '1',
        flash: '',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }
    if (S.key && Q.get('view') === null) { S.view = 'record'; S.scope = M.kind(S.kind) && !M.kind(S.kind).party ? 'shared' : S.scope; }
    if (!M.kind(S.kind)) { S.kind = 'swap'; }

    /* Page, size, search, filter, order and tab live in the address. */
    function sync() {
        var p = new URLSearchParams();
        p.set('view', S.view);
        if (S.view === 'kinds') {
            if (S.scope === 'shared') { p.set('scope', 'shared'); }
            if (S.cls !== 'all') { p.set('class', S.cls); }
        } else {
            p.set('kind', S.kind);
            if (S.view === 'record') { p.set('key', S.key); if (S.tab !== 'details') { p.set('tab', S.tab); } }
            if (S.view === 'list') {
                if (S.source !== 'all') { p.set('source', S.source); }
                if (S.pgD.page > 0) { p.set('page', String(S.pgD.page + 1)); }
                if (S.pgD.size !== 15) { p.set('size', String(S.pgD.size)); }
            }
        }
        if (S.q) { p.set('q', S.q); }
        if (S.view !== 'record' && S.sort.id !== 'key') { p.set('sort', S.sort.id); }
        if (S.view !== 'record' && S.sort.dir < 0) { p.set('dir', 'desc'); }
        if (S.stale) { p.set('stale', '1'); }
        if (S.state) { p.set('state', S.state); }
        if (S.person.id !== 'm.okafor') { p.set('actor', S.person.id); }
        try { history.replaceState(null, '', location.pathname + '?' + p.toString()); } catch (e) { /* a file page may refuse */ }
    }

    function refreshControl() { return M.refresh(S.stale, S.loaded); }

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
        var inScope = M.scopeKinds(S.scope);
        var rows = M.sortRows(inScope.filter(function (k) {
            return (!S.q || (k.label + k.key).toLowerCase().indexOf(S.q.toLowerCase()) >= 0) && (S.cls === 'all' || M.classOf(k) === S.cls);
        }), cols, sort);
        var grouped = M.kindTable(cols, rows, { sort: sort, pick: true });
        S.shownKinds = grouped.shown;
        var total = inScope.reduce(function (t, k) { return t + M.docs(k.key).length; }, 0);
        var acts = refreshControl() + M.workflowMenu(workflows(null));
        var present = M.CLASSES.filter(function (c) { return inScope.some(function (k) { return M.classOf(k) === c; }); });
        return M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Conventions' }]) +
            M.pagehead('Conventions', 'The market terms the instruments are built on, grouped by asset class.', acts) +
            M.scopeTabs(S.scope) +
            (S.scope === 'shared' ? M.sharedWarn('these conventions') : '') +
            toolbar('Search kinds', [{ id: 'class', all: 'All asset classes', opts: present.map(function (c) { return { v: c, t: c }; }), v: S.cls }]) +
            '<p class="listnote">' + total + ' documents across ' + inScope.length + ' kinds.</p>' +
            (rows.length === 0 ? '<div class="tblstate">No kinds match the search. ' + M.btn('clear', 'Clear', { cls: 'small' }) + '</div>'
                : grouped.html) +
            M.gap('The families route returns a count of documents for each kind, but no count of distinct term sets, so the screen reads each list to count the term sets. Document counts for the kinds without mock terms are fixtures. A kind carries no asset class in the model today, so the screen holds the grouping.');
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
        var acts = M.accessMark(canWrite()) + refreshControl() + M.workflowMenu(workflows(null)) +
            (canWrite() ? M.btn('dialog', 'Add', { cls: 'primary', icon: 'add', arg: 'add' }) : '');
        var body;
        if (S.state === 'loading') { body = '<div class="tblstate">Loading…</div>'; }
        else if (S.state === 'refused') { body = M.notice('info', '<b>You cannot read these documents.</b> Ask an administrator for the permission to read ' + M.esc(k.label.toLowerCase()) + ' conventions.'); }
        else if (S.state === 'reloading') { body = M.table(cols, S.shown, { sort: S.sort, refreshing: true }); }
        else if (S.state === 'error') { body = M.notice('error', '<b>The ' + M.esc(k.label.toLowerCase()) + ' documents could not be read.</b> The server did not answer. ' + M.btn('retry', 'Retry', { cls: 'small' })); }
        else if (all.length === 0) { body = '<div class="tblstate">There are no ' + M.esc(k.label.toLowerCase()) + ' documents yet. ' + (canWrite() ? M.btn('dialog', 'Add', { cls: 'small primary', arg: 'add' }) : '') + '</div>'; }
        else if (rows.length === 0) { body = '<div class="tblstate">No ' + M.esc(k.label.toLowerCase()) + ' documents match the search. ' + M.btn('clear', 'Clear', { cls: 'small' }) + '</div>'; }
        else { body = M.table(cols, S.shown, { sort: S.sort, rowClass: function (d) { return S.news.indexOf(d.key) >= 0 ? 'row-news' : ''; } }); }
        return M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Conventions', act: 'toKinds' }, { t: k.label }]) +
            (S.flash ? M.notice('success', S.flash) : '') +
            M.pagehead(k.label, M.classOf(k) + ' · the ' + k.label.toLowerCase() + ' documents.', acts) +
            (k.party ? '' : M.sharedWarn('these documents')) +
            toolbar('Search by key', [{ id: 'source', all: 'All sources', opts: [{ v: 'curated', t: 'Curated dataset' }, { v: 'authored', t: 'Authored' }], v: S.source }]) +
            (M.modelled(S.kind) && (S.state === '' || S.state === 'reloading') ? '<p class="listnote">' + all.length + ' documents state ' + M.termSets(S.kind) + ' distinct term sets.</p>' : '') +
            body + (S.state !== 'loading' && S.state !== 'error' && S.state !== 'refused' && rows.length ? M.pager(S.pgD, rows.length) : '') +
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
        var acts = M.accessMark(w) + refreshControl() + M.workflowMenu(workflows(d));
        var tabs = [['details', 'Details']];
        if (reqs.length) { tabs.push(['requests', 'Requests (' + reqs.length + ')']); }
        tabs.push(['history', 'History']);
        if (!tabs.some(function (t) { return t[0] === S.tab; })) { S.tab = 'details'; }
        var body;
        if (S.tab === 'requests') {
            body = '<table class="rows"><thead><tr><th>Request</th><th>Asked by</th><th>State</th><th>Asked</th></tr></thead><tbody>' + reqs.map(function (r) {
                return '<tr><td><a href="../decide-a-convention-change/?actor=t.brandt&request=' + r.id + '">' + M.esc(r.t) + '</a></td><td>' + M.person(r.by) + '</td><td>' + M.badge('pending', 'Pending') + '</td><td>' + M.relTime(r.at) + '</td></tr>';
            }).join('') + '</tbody></table>';
        } else if (S.tab === 'history') {
            body = '<ol class="timeline">' + versions(d).map(function (v, i) {
                return '<li class="ph">' + M.avatar(v.by) + '<div class="when">' + M.relTime(v.at) + '</div><div><div class="head"><b>' + (v.n === 1 ? 'Created' : 'Changed') + '</b><span class="mono">' + M.esc(d.key) + ' v' + v.n + '</span><span class="dim">' + M.esc(M.reasonName(v.code)) + '</span><span class="dim">' + M.esc(M.PEOPLE[v.by] ? M.PEOPLE[v.by].name : v.by) + '</span>' +
                    (i === 0 ? '<span class="cur" title="Current version">Current version</span>' : '') +
                    (i > 0 && w ? M.btn('revert', 'Revert to this version', { cls: 'small ghost', icon: 'arrow_rotate_counterclockwise', arg: v.n }) : '') + '</div><p class="note">' + M.esc(v.note) + '</p>' +
                    (v.diff ? '<details><summary>Details</summary><table class="diff"><thead><tr><th>Field</th><th>Was</th><th>Now</th></tr></thead><tbody>' +
                        v.diff.map(function (r) { return '<tr class="changed"><td>' + M.esc(r[0]) + '</td><td class="was">' + M.esc(r[1]) + '</td><td class="now">' + M.esc(r[2]) + '</td></tr>'; }).join('') + '</tbody></table></details>' : '') + '</div></li>';
            }).join('') + '</ol>';
        } else {
            var cols = M.COLUMNS[S.kind] || [{ id: 'source', h: 'Source' }];
            var shown = M.COLUMNS[S.kind] ? cols : [{ id: 'source', h: 'Source', value: d.source }];
            body = (k.party ? '' : '<div class="cardgap">' + M.sharedWarn('this document') + '</div>') +
                '<section class="card"><div class="cardhead"><h2>Terms</h2>' + (w ? M.btn('dialog', 'Edit', { icon: 'edit', arg: 'edit' }) : '') + '</div>' +
                '<dl class="fields">' + shown.map(function (c) { return '<div><dt>' + M.esc(c.h) + '</dt><dd>' + M.dash(c.value !== undefined ? c.value : d.terms[c.id]) + '</dd></div>'; }).join('') + '</dl>' +
                '<div class="lastchanged">Last changed: version ' + d.version + ' · ' + M.person(d.modifiedBy) + ' · ' + M.relTime(d.recordedAt) + ' · ' + M.esc(M.reasonName(d.reason)) + ' · “' + M.esc(d.commentary) + '”</div></section>';
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

    /* A reason that needs a commentary. */
    var NEEDS_COMMENT = ['common.market_practice'];
    /* Fixture: a second person saved the next version while the dialog was open. */
    var NEWER = { by: 't.brandt', at: '2026-10-19T13:01:10+01:00', col: 'fixed_day_count', h: 'Fixed day count fraction', was: 'ACT/360', now: 'ACT/365' };

    function openDialog(mode) {
        var cols = M.COLUMNS[S.kind] || [];
        var orig = {};
        if (mode === 'edit') { cols.forEach(function (c) { orig[c.id] = doc().terms[c.id] || ''; }); }
        D = { mode: mode, key: mode === 'edit' ? doc().key : '', vals: JSON.parse(JSON.stringify(orig)), orig: orig, reason: '', commentary: '', tried: false,
            loaded: S.loaded, newer: mode === 'edit' && Q.get('newer') === '1' ? NEWER : null, refused: false, confirmClose: false, keyTouched: false };
        drawDialog();
    }

    function changed() {
        return Object.keys(D.orig).filter(function (id) { return (D.vals[id] || '') !== D.orig[id]; });
    }

    function dirty() {
        return D.mode === 'add' ? !!(D.key.trim() || Object.keys(D.vals).some(function (id) { return D.vals[id]; })) : changed().length > 0 || !!D.reason || !!D.commentary;
    }

    function syncText() {
        var k = document.querySelector('[data-input=dkey]');
        if (k) { D.key = k.value; }
        var c = document.querySelector('[data-input=dcomment]');
        if (c) { D.commentary = c.value; }
    }

    function keyProblem() {
        if (D.mode !== 'add') { return ''; }
        if (!D.key.trim()) { return 'Give the document a key.'; }
        if (M.doc(S.kind, D.key.trim())) { return 'A document with the key “' + D.key.trim() + '” exists.'; }
        return '';
    }

    function drawDialog() {
        var k = M.kind(S.kind);
        var cols = M.COLUMNS[S.kind] || [];
        var add = D.mode === 'add';
        var keyBad = (D.tried || D.keyTouched) && !!keyProblem();
        var reasons = M.CHANGE_REASONS.amend.filter(function (r) { return r.v !== 'common.non_material_update'; });
        var reasonBad = D.tried && !add && !D.reason;
        var needsComment = NEEDS_COMMENT.indexOf(D.reason) >= 0;
        var commentBad = D.tried && !add && needsComment && !D.commentary.trim();
        var canSave = add ? !keyProblem() && cols.every(function (c) { return c.optional || D.vals[c.id]; }) : changed().length > 0;
        var html = '<h2>' + (add ? 'Add ' + M.esc(k.label.toLowerCase()) + ' document' : 'Edit ' + M.esc(D.key)) + '</h2>' +
            (add ? '<p class="lede">A new document is written with the reason New record.</p>'
                : '<p class="lede">Change the terms, then say why. <span class="loadedline">Loaded <span class="mono">' + D.loaded + '</span></span></p>') +
            (k.party ? '' : M.notice('warn', 'This document is shared. The change applies to every party.')) +
            (D.newer ? M.notice('warn', '<b>' + M.esc(M.PEOPLE[D.newer.by].name) + ' saved a newer version at <span class="mono">' + D.newer.at + '</span>.</b> ' + M.esc(D.newer.h) + ' moved from ' + M.esc(D.newer.was) + ' to ' + M.esc(D.newer.now) + '. ' +
                M.btn('keep', 'Keep my edit', { cls: 'small' }) + ' ' + M.btn('take', 'Take the new values', { cls: 'small' })) : '') +
            (D.refused ? M.notice('error', '<b>The save was refused.</b> Version ' + (doc().version + 1) + ' of ' + M.esc(D.key) + ' exists and your edit started from version ' + doc().version + '. Your edit stays in the form. Choose Keep my edit to replace version ' + (doc().version + 1) + ', or Take the new values.') : '') +
            '<div class="field' + (keyBad ? ' bad' : '') + '" data-field="dkey"><span class="lbl">Key <span class="req">(' + (add ? 'required' : 'fixed') + ')</span></span><input type="text" data-input="dkey" value="' + M.esc(D.key) + '"' + (add ? '' : ' disabled') + '>' +
            (add ? '' : '<p class="hint">The key is fixed once the document exists.</p>') +
            '<p class="msg" data-msg="dkey"' + (keyBad ? '' : ' hidden') + '>' + M.esc(keyProblem()) + '</p></div>' +
            (cols.length ? '<div class="grid2">' + cols.map(function (c) {
                var opts = (c.optional ? [{ v: '', t: 'Not stated' }] : []).concat(c.options);
                var val = D.vals[c.id] === undefined ? '' : D.vals[c.id];
                return '<div class="field"><span class="lbl">' + M.esc(c.h) + (c.optional && val ? ' ' + M.btn('dclear', 'Clear', { cls: 'small ghost', arg: c.id }) : '') + '</span>' + M.select('d.' + c.id, opts, val, { placeholder: 'Choose…' }) + '</div>';
            }).join('') + '</div>' : '') +
            (add ? '' : '<div class="field' + (reasonBad ? ' bad' : '') + '"><span class="lbl">Reason <span class="req">(required)</span></span>' + M.select('dreason', reasons, D.reason, { placeholder: 'Choose a reason' }) +
                (reasonBad ? '<p class="msg">Choose a reason for the change.</p>' : '') + '</div><div class="field' + (commentBad ? ' bad' : '') + '"><span class="lbl">Commentary' + (needsComment ? ' <span class="req">(required)</span>' : '') + '</span><textarea data-input="dcomment">' + M.esc(D.commentary) + '</textarea>' +
                (commentBad ? '<p class="msg">This reason needs a commentary. Say what changed in the market.</p>' : '') + '</div>') +
            M.notice('info', 'Market Risk decides this change. Nothing changes until it approves, and you cannot approve your own request.') +
            (D.confirmClose ? M.notice('warn', 'You have unsaved changes. Close anyway?') : '') +
            '<div class="foot">' + (D.confirmClose ? M.btn('closeanyway', 'Close anyway', { cls: 'ghost', icon: 'dismiss' }) + M.btn('keepediting', 'Keep editing', { cls: 'primary', icon: 'edit' })
                : M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('propose', 'Propose', { cls: 'primary', disabled: !canSave })) + '</div>';
        M.dialog(html, true);
        M.beforeClose = function () {
            syncText();
            if (!dirty() || D.confirmClose) { return true; }
            D.confirmClose = true; drawDialog(); return false;
        };
        var sc = document.getElementById('dialog-scrim');
        M.wireSelects(sc, function (id, v) {
            syncText();
            if (id === 'dreason') { D.reason = v; } else { D.vals[id.slice(2)] = v; }
            drawDialog();
        });
        var ki = sc.querySelector('[data-input=dkey]');
        if (ki && add) {
            ki.addEventListener('blur', function () {
                D.key = ki.value; D.keyTouched = true;
                var bad = !!keyProblem();
                var f = sc.querySelector('[data-field=dkey]');
                var m = sc.querySelector('[data-msg=dkey]');
                f.classList.toggle('bad', bad); m.textContent = keyProblem(); m.hidden = !bad;
            });
        }
    }

    /* ------------------------------------------------------------ */

    function render() {
        var body = S.view === 'kinds' ? kindsView() : S.view === 'list' ? listView() : recordView();
        document.getElementById('app').innerHTML = M.shell('Reference data', S.person) + '<div class="page">' + body + '</div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: 'kinds', t: 'Party kinds' }, { v: 'shared', t: 'Shared kinds' }, { v: 'list', t: 'Swap list' }, { v: 'record', t: 'A document' },
            { v: 'err', t: 'Read error' }, { v: 'stale', t: 'Changed on the server' }, { v: 'readonly', t: 'Read-only person' }], S.view === 'kinds' && S.scope === 'shared' ? 'shared' : S.view);
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
        sync();
    }

    function closeDialogAsked() {
        if (!M.beforeClose || M.beforeClose()) { M.closeDialog(); }
    }

    /* Refresh reads again. A stale screen learns which rows changed. */
    function refresh() {
        S.flash = '';
        S.state = 'reloading';
        render();
        setTimeout(function () {
            S.state = '';
            S.loaded = '2026-10-19T13:05:12+01:00';
            if (S.stale) { S.news = ['AED-EIBOR-6M-SWAP']; setTimeout(function () { S.news = []; render(); }, 8000); }
            S.stale = false;
            render();
        }, 600);
    }

    document.addEventListener('click', function (e) {
        var tr = e.target.closest('tr[data-row]');
        if (tr && S.view === 'kinds') { S.kind = S.shownKinds[Number(tr.getAttribute('data-row'))].key; S.q = ''; S.cls = 'all'; S.pgD.page = 0; S.source = 'all'; S.sort = { id: 'key', dir: 1 }; S.view = 'list'; render(); return; }
        if (tr && S.view === 'list') { S.key = S.shown[Number(tr.getAttribute('data-row'))].key; S.tab = 'details'; S.flash = ''; S.state = ''; S.view = 'record'; render(); return; }
        var b = e.target.closest('[data-act]');
        if (!b || b.closest('.sel')) { return; }
        var name = b.getAttribute('data-act');
        var arg = b.getAttribute('data-arg');
        if (name === 'bar') { return bar(arg); }
        if (name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (name === 'toKinds') { S.view = 'kinds'; S.q = ''; S.flash = ''; S.state = ''; S.sort = { id: 'key', dir: 1 }; S.scope = M.kind(S.kind).party ? 'party' : 'shared'; render(); }
        else if (name === 'scope') { S.scope = arg; S.q = ''; S.cls = 'all'; render(); }
        else if (name === 'toList') { S.view = 'list'; S.q = ''; S.state = ''; S.sort = { id: 'key', dir: 1 }; render(); }
        else if (name === 'tab') { S.tab = arg; render(); }
        else if (name === 'sort') { S.sort = { id: arg, dir: S.sort.id === arg ? -S.sort.dir : 1 }; render(); }
        else if (name === 'pg') { var st = S.view === 'kinds' ? S.pgK : S.pgD; M.pageStep(st, arg, S.view === 'kinds' ? M.KINDS.length : docs().length); render(); }
        else if (name === 'clear') { S.q = ''; S.source = 'all'; S.cls = 'all'; render(); }
        else if (name === 'refresh') { refresh(); }
        else if (name === 'retry') { S.state = ''; render(); }
        else if (name === 'dialog') { openDialog(arg); }
        else if (name === 'cancel') { closeDialogAsked(); }
        else if (name === 'closeanyway') { M.closeDialog(); }
        else if (name === 'keepediting') { D.confirmClose = false; drawDialog(); }
        else if (name === 'keep') { syncText(); D.newer = null; D.refused = false; D.loaded = '2026-10-19T13:06:00+01:00'; drawDialog(); }
        else if (name === 'take') { syncText(); D.vals[NEWER.col] = NEWER.now; D.orig[NEWER.col] = NEWER.now; D.newer = null; D.refused = false; D.loaded = '2026-10-19T13:06:00+01:00'; drawDialog(); }
        else if (name === 'dclear') { syncText(); D.vals[arg] = ''; drawDialog(); }
        else if (name === 'propose') {
            syncText();
            D.tried = true;
            if (D.mode === 'add' && (!D.key.trim() || M.doc(S.kind, D.key.trim()))) { drawDialog(); return; }
            if (D.mode === 'edit' && (!D.reason || (NEEDS_COMMENT.indexOf(D.reason) >= 0 && !D.commentary.trim()))) { drawDialog(); return; }
            if (D.mode === 'edit' && D.newer) { D.refused = true; drawDialog(); return; }
            M.closeDialog();
            S.flash = '<b>Request raised.</b> Market Risk decides it. Nothing changes until it approves. <a href="../decide-a-convention-change/?actor=t.brandt">Open Requests</a>';
            render();
        }
        else if (name === 'revert') {
            M.dialog('<h2>Revert to version ' + M.esc(arg) + '?</h2><p class="lede">The values of version ' + M.esc(arg) + ' are written back as a new version. No version is lost.</p>' +
                M.notice('info', 'Market Risk decides this change. Nothing changes until it approves.') +
                '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('cancel', 'Propose', { cls: 'primary' }) + '</div>');
            M.beforeClose = null;
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
        if (arg === 'stale') { S.stale = true; render(); return; }
        S.state = '';
        if (arg === 'shared') { S.scope = 'shared'; arg = 'kinds'; } else if (arg === 'kinds') { S.scope = 'party'; }
        S.view = arg;
        if (arg === 'record' && !S.key) { S.key = 'AED-EIBOR-6M-SWAP'; S.kind = 'swap'; }
        if (arg === 'list') { S.kind = 'swap'; }
        render();
    }

    render();
    if (Q.get('dialog') === 'add' || Q.get('dialog') === 'edit') {
        openDialog(Q.get('dialog'));
        if (Q.get('refused') === '1' && D.newer) { D.refused = true; drawDialog(); }
    }
})();

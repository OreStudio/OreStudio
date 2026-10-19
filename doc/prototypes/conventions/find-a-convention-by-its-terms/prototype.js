/* Find a convention by its terms.
 *
 * Three steps on one rail: the terms the person holds, the documents that state
 * them, and one document read as a record page. The generated list filter takes
 * the key only, so the screen reads every row and compares the terms; the notice
 * above the matches says how many rows it read. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();
    var STEPS = ['The terms', 'The matches', 'A match'];
    var KINDS = ['swap', 'deposit', 'ibor_index'];

    var S = {
        step: Number(Q.get('step') || 0),
        kind: KINDS.indexOf(Q.get('kind')) >= 0 ? Q.get('kind') : 'swap',
        terms: {},
        sort: { id: 'key', dir: 1 },
        pg: { page: 0, size: 15 },
        key: Q.get('key'),
        tab: 'details',
        searching: false,
        failed: Q.get('fail') === '1',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }
    readTerms();
    if (S.key && S.step < 2) { S.step = 2; }

    function readTerms() {
        S.terms = {};
        M.COLUMNS[S.kind].forEach(function (c) {
            var v = Q.get(c.id);
            S.terms[c.id] = v === null ? '*' : (v === 'none' ? '' : v);
        });
    }

    function setKind(k) {
        S.kind = k;
        S.terms = {};
        M.COLUMNS[k].forEach(function (c) { S.terms[c.id] = '*'; });
        S.pg.page = 0;
    }

    function matches() {
        return M.docs(S.kind).filter(function (d) {
            return M.COLUMNS[S.kind].every(function (c) {
                var want = S.terms[c.id];
                return want === '*' || (d.terms[c.id] || '') === want;
            });
        });
    }

    function anySet() {
        return M.COLUMNS[S.kind].some(function (c) { return S.terms[c.id] !== '*'; });
    }

    /* A fixture: the curated swap is the one document a set names. */
    function usedBy(d) { return d.key === 'AED-EIBOR-6M-SWAP' ? '1 set' : ''; }

    /* ------------------------------------------------------------ */

    function stepTerms() {
        var k = M.kind(S.kind);
        var total = M.docs(S.kind).length;
        var cols = M.COLUMNS[S.kind];
        return '<h2>The terms I hold</h2><p class="lede">Set the terms you know. A term left as Any matches every value.</p>' +
            '<div class="field"><span class="lbl">Kind</span>' +
            M.select('kind', KINDS.map(function (x) { return { v: x, t: M.kind(x).label }; }), S.kind, { inline: true }) +
            '<p class="hint"><b>' + total + ' documents</b> state <b>' + M.termSets(S.kind) + ' distinct term sets</b>. ' +
            (k.party ? 'They belong to ACME’s party.' : 'They are held once for the tenant, so every party reads them.') + '</p></div>' +
            '<div class="grid2">' + cols.map(function (c) {
                var opts = [{ v: '*', t: 'Any' }];
                if (c.optional) { opts.push({ v: '', t: 'Not stated' }); }
                opts = opts.concat(c.options);
                return '<div class="field"><span class="lbl">' + M.esc(c.h) + '</span>' +
                    M.select('t.' + c.id, opts, S.terms[c.id]) + '</div>';
            }).join('') + '</div>' +
            M.gap('The list filter takes <span class="mono">id_one_of</span> and nothing else, so the screen reads every row and compares the terms in the browser. The candidate is a filter on the term columns of each kind.') +
            '<div class="stepfoot">' + M.btn('clear', 'Clear', { cls: 'ghost' }) +
            '<span class="ml-auto"></span>' + M.btn('find', 'Find', { cls: 'primary', icon: 'search', disabled: !anySet() }) + '</div>';
    }

    function stepMatches() {
        var total = M.docs(S.kind).length;
        if (S.failed) {
            return '<h2>The matches</h2>' + M.notice('error', '<b>The convention list could not be read.</b> The server did not answer. ' +
                M.btn('retry', 'Retry', { cls: 'small' })) + '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 0 }) + '</div>';
        }
        if (S.searching) {
            return '<h2>The matches</h2><div class="tblstate">Reading ' + M.kind(S.kind).label.toLowerCase() + ' documents…</div>';
        }
        var rows = matches();
        var differing = M.COLUMNS[S.kind].filter(function (c) { return S.terms[c.id] === '*'; });
        var cols = [{ id: 'key', h: 'Key', cell: function (d) { return '<span class="key">' + M.esc(d.key) + '</span>'; }, val: function (d) { return d.key; } }]
            .concat(differing.map(function (c) {
                return { id: c.id, h: c.h, cell: function (d) { return M.dash(d.terms[c.id]); }, val: function (d) { return d.terms[c.id] || ''; } };
            }))
            .concat([{ id: 'used', h: 'Used by', cell: function (d) { return M.dash(usedBy(d)); }, val: usedBy },
                { id: 'version', h: 'Version', cls: 'right', cell: function (d) { return '<span class="num">' + d.version + '</span>'; }, val: function (d) { return d.version; } }]);
        var sorted = M.sortRows(rows, cols, S.sort);
        var page = sorted.slice(S.pg.page * S.pg.size, (S.pg.page + 1) * S.pg.size);
        S.shown = page;
        var h = '<h2>The matches</h2><p class="lede">' + M.esc(termsLine()) + '</p>' +
            M.notice('info', 'Read <b>' + total + ' of ' + total + '</b> documents. <b>' + rows.length + '</b> state these terms.');
        if (rows.length === 0) {
            return h + '<div class="tblstate">No ' + M.kind(S.kind).label.toLowerCase() + ' documents state these terms.</div>' +
                '<div class="stepfoot">' + M.btn('goto', 'Change the terms', { cls: 'ghost', arg: 0 }) + '<span class="ml-auto"></span>' +
                (S.person.part === 'Operations' || S.person.part === 'Market Risk'
                    ? '<a class="btn primary" href="../define-the-conventions-for-an-instrument/?state=terms&family=' + S.kind + '">' + M.icon('add') + 'Add a ' + M.kind(S.kind).label.toLowerCase() + ' document</a>' : '') + '</div>';
        }
        var twins = new Set(rows.map(function (d) { return JSON.stringify(d.terms); }));
        return h + (twins.size < rows.length
            ? '<p class="listnote">Several documents state the same terms under different keys.</p>' : '') +
            M.table(cols, page, { sort: S.sort }) + M.pager(S.pg, rows.length) +
            M.gap('A document does not say which run reads it. No default marker exists, and a configuration holds no rows, so the Used by column shows only the sets that name a document, and no set exists yet.') +
            '<div class="stepfoot">' + M.btn('goto', 'Change the terms', { cls: 'ghost', arg: 0 }) + '</div>';
    }

    function termsLine() {
        var set = M.COLUMNS[S.kind].filter(function (c) { return S.terms[c.id] !== '*'; }).map(function (c) {
            return c.h + ' ' + (S.terms[c.id] === '' ? 'not stated' : S.terms[c.id]);
        });
        return M.kind(S.kind).label + ': ' + set.join(', ');
    }

    function stepMatch() {
        var d = M.doc(S.kind, S.key) || M.docs(S.kind)[0];
        var write = S.person.part === 'Operations' || S.person.part === 'Market Risk';
        var twins = M.docs(S.kind).filter(function (x) { return x.key !== d.key && M.sameTerms(x, d); });
        var acts = write ? '<a class="btn" href="../define-the-conventions-for-an-instrument/?state=terms&family=' + S.kind + '&id=' + encodeURIComponent(d.key) + '">' + M.icon('edit') + 'Edit</a>' +
            '<a class="btn danger" href="../retire-a-convention/?kind=' + S.kind + '&key=' + encodeURIComponent(d.key) + '">' + M.icon('delete') + 'Delete</a>' : '';
        var details = '<dl class="fields">' + M.COLUMNS[S.kind].map(function (c) {
            return '<div><dt>' + M.esc(c.h) + '</dt><dd>' + M.dash(d.terms[c.id]) + '</dd></div>';
        }).join('') + '</dl><div class="lastchanged">Last changed: version ' + d.version + ' · ' + M.esc(d.modifiedBy) + ' · ' +
            M.relTime(d.recordedAt) + ' · ' + M.esc(d.reason) + ' · “' + M.esc(d.commentary) + '”</div>';
        var twinTab = twins.length === 0 ? '<p class="dim">None yet.</p>' :
            '<p class="dim">' + twins.length + ' other documents state the same terms.</p><ul>' + twins.slice(0, 8).map(function (t) {
                return '<li class="key mono">' + M.esc(t.key) + '</li>';
            }).join('') + '</ul>';
        return M.pagehead(d.key, d.key + ' · version ' + d.version, M.accessMark(write) + acts).replace('<h1>', '<h1 class="mono">') +
            '<div class="tabs"><button class="' + (S.tab === 'details' ? 'on' : '') + '" data-act="tab" data-arg="details">Details</button>' +
            '<button class="' + (S.tab === 'twins' ? 'on' : '') + '" data-act="tab" data-arg="twins">Same terms</button></div>' +
            (S.tab === 'details' ? details : twinTab) +
            '<div class="stepfoot">' + M.btn('goto', 'Back to the matches', { cls: 'ghost', arg: 1 }) + '</div>';
    }

    /* ------------------------------------------------------------ */

    function render() {
        var body = S.step === 0 ? stepTerms() : S.step === 1 ? stepMatches() : stepMatch();
        document.getElementById('app').innerHTML = M.shell('Reference data', S.person) + '<div class="page">' +
            M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Find a convention by its terms' }], '../see-the-conventions-a-party-uses/?view=list&kind=' + S.kind) +
            (S.step === 2 ? '' : M.pagehead('Find a convention by its terms', 'Which document of a kind states these terms, or none does.', '')) +
            '<div class="journey">' + M.rail(STEPS, S.step) + '<div class="card">' + body + '</div></div></div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: '0', t: 'Terms' }, { v: '1', t: 'Matches' }, { v: '2', t: 'A match' }, { v: 'fail', t: 'Server error' }], String(S.step));
        M.wireSelects(document.getElementById('app'), function (id, v) {
            if (id === 'kind') { setKind(v); }
            else if (id === 'pagesize') { S.pg.size = Number(v); S.pg.page = 0; }
            else if (id.indexOf('t.') === 0) { S.terms[id.slice(2)] = v; }
            render();
        });
    }

    function go(n) { S.step = n; render(); }

    function find() {
        S.failed = Q.get('fail') === '1';
        S.pg.page = 0;
        S.searching = true;
        go(1);
        setTimeout(function () { S.searching = false; render(); }, 450);
    }

    document.addEventListener('click', function (e) {
        var b = e.target.closest('[data-act]');
        if (b && !b.closest('.proto-bar') && !b.closest('.sel')) { act(b.getAttribute('data-act'), b.getAttribute('data-arg')); return; }
        var bar = e.target.closest('.proto-bar [data-act]');
        if (bar) { act('bar', bar.getAttribute('data-arg'), bar.getAttribute('data-act')); return; }
        var tr = e.target.closest('tr[data-row]');
        if (tr && S.step === 1) { S.key = S.shown[Number(tr.getAttribute('data-row'))].key; S.tab = 'details'; go(2); }
    });
    document.addEventListener('keydown', function (e) {
        var tr = e.target.closest && e.target.closest('tr[data-row]');
        if (tr && e.key === 'Enter') { tr.click(); }
    });

    function act(name, arg, barAct) {
        if (barAct === 'gaps' || name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (name === 'bar') {
            if (arg === 'fail') { S.failed = true; S.step = 1; S.searching = false; render(); return; }
            S.failed = false;
            if (arg === '1' && !anySet()) { S.terms.fixed_calendar = 'AED'; if (S.kind !== 'swap') { setKind('swap'); S.terms.fixed_calendar = 'AED'; } }
            go(Number(arg)); return;
        }
        if (name === 'goto') { go(Number(arg)); }
        else if (name === 'goto0') { go(0); }
        else if (name === 'goto1') { go(1); }
        else if (name === 'find' || name === 'retry') { Q.set('fail', '0'); find(); }
        else if (name === 'clear') { setKind(S.kind); render(); }
        else if (name === 'tab') { S.tab = arg; render(); }
        else if (name === 'sort') { S.sort = { id: arg, dir: S.sort.id === arg ? -S.sort.dir : 1 }; S.pg.page = 0; render(); }
        else if (name === 'pg') { M.pageStep(S.pg, arg, matches().length); render(); }
    }

    if (S.step === 1) { S.searching = false; }
    render();
})();

/* Name a convention set.
 *
 * Five steps on one rail: the sets that exist, the three stored fields, one
 * document for each kind, the review of the reach, and the request that waits
 * for Market Risk. The set holds no term. It names documents that already
 * exist, one for each kind it covers. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();
    var STEPS = ['The sets', 'Describe it', 'The documents', 'Review', 'The request'];

    /* Fixtures: the sets a system holds today. A conventions configuration is
       the nearest entity, and it carries a name only. */
    var SETS = [
        { name: 'Standard market', purpose: 'The terms the desks book on', kinds: 24, reports: 6 },
        { name: 'Regulatory reporting', purpose: 'Terms a regulator prescribes', kinds: 9, reports: 2 },
        { name: 'Credit desk', purpose: 'Credit default swap terms for the credit desk', kinds: 4, reports: 1 }
    ];

    var S = {
        step: Number(Q.get('step') || 0),
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['j.ruiz'],
        f: { name: Q.get('name') || '', purpose: Q.get('purpose') || '', description: Q.get('description') || '' },
        picks: {},
        tried: Q.get('tried') === '1',
        withdrawn: false,
        sort: { id: 'name', dir: 1 },
        pg: { page: 0, size: 15 },
        kpg: { page: 0, size: 15 }
    };
    (Q.get('pick') || '').split(',').filter(Boolean).forEach(function (p) { var a = p.split(':'); S.picks[a[0]] = a[1]; });
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function canWrite() { return S.person.part === 'Operations' || S.person.part === 'Market Risk'; }
    function covered() { return Object.keys(S.picks).length; }
    function nameTaken() {
        return SETS.some(function (x) { return x.name.toLowerCase() === S.f.name.trim().toLowerCase(); });
    }
    function problems() {
        var p = {};
        if (!S.f.name.trim()) { p.name = 'Give the set a name.'; }
        else if (nameTaken()) { p.name = 'A set named “' + S.f.name.trim() + '” exists. Choose another name.'; }
        if (!S.f.purpose.trim()) { p.purpose = 'Say what the set is for.'; }
        return p;
    }

    /* ------------------------------------------------------------ */

    function setsStep() {
        var cols = [
            { id: 'name', h: 'Name', cell: function (r) { return M.esc(r.name); }, val: function (r) { return r.name; } },
            { id: 'purpose', h: 'Purpose', cell: function (r) { return '<span class="dim">' + M.esc(r.purpose) + '</span>'; }, val: function (r) { return r.purpose; } },
            { id: 'kinds', h: 'Kinds covered', cls: 'right', cell: function (r) { return '<span class="num">' + r.kinds + ' of 26</span>'; }, val: function (r) { return r.kinds; } },
            { id: 'reports', h: 'Used by', cls: 'right', cell: function (r) { return '<span class="num">' + r.reports + ' reports</span>'; }, val: function (r) { return r.reports; } }
        ];
        var rows = M.sortRows(SETS, cols, S.sort);
        return '<h2>Convention sets</h2><p class="lede">A report chooses one set. Few sets exist, and each says what it is for.</p>' +
            M.table(cols, rows, { sort: S.sort }) + M.pager(S.pg, rows.length) +
            M.gap('No set exists in the server. A conventions configuration holds a name, a type and an owning component, and no purpose, no description and no kinds.') +
            '<div class="stepfoot"><span class="ml-auto"></span>' + (canWrite() ? M.btn('goto', 'Add', { cls: 'primary', icon: 'add', arg: 1 }) : '') + '</div>';
    }

    function field(id, label, req, ctrl, hint) {
        var p = S.tried ? problems()[id] : '';
        return '<div class="field' + (p ? ' bad' : '') + '"><span class="lbl">' + label + (req ? ' <span class="req">(required)</span>' : ' <span class="req">(optional)</span>') + '</span>' + ctrl +
            (p ? '<p class="msg">' + M.esc(p) + '</p>' : (hint ? '<p class="hint">' + hint + '</p>' : '')) + '</div>';
    }

    function describeStep() {
        return '<h2>Describe the set</h2><p class="lede">The three fields are stored. None is worked out from the terms.</p>' +
            '<div class="grid2"><div>' + field('name', 'Name', true, '<input type="text" maxlength="60" data-input="name" value="' + M.esc(S.f.name) + '">', 'The name is fixed once the set exists.') + '</div>' +
            '<div>' + field('purpose', 'Purpose', true, '<input type="text" maxlength="80" data-input="purpose" value="' + M.esc(S.f.purpose) + '">', 'One line. A reader picks a set by this.') + '</div>' +
            '<div class="span2">' + field('description', 'Description', false, '<textarea data-input="description">' + M.esc(S.f.description) + '</textarea>', '') + '</div></div>' +
            '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 0 }) + '<span class="ml-auto"></span>' + M.btn('toDocs', 'Next', { cls: 'primary' }) + '</div>';
    }

    function docsStep() {
        var rows = M.KINDS;
        var cols = [
            { id: 'kind', h: 'Kind', cell: function (k) { return M.esc(k.label); } },
            { id: 'doc', h: 'Document', cell: function (k) {
                return S.picks[k.key] ? '<span class="key">' + M.esc(S.picks[k.key]) + '</span>' : '<span class="muted-dash">Not covered</span>'; } },
            { id: 'terms', h: 'Terms in words', cell: function (k) {
                var d = S.picks[k.key] && M.doc(k.key, S.picks[k.key]);
                return d ? '<span class="dim">' + M.esc(M.words(d)) + '</span>' : M.dash(''); } },
            { id: 'scope', h: 'Held for', cell: function (k) { return k.party ? 'ACME' : M.badge('class', 'Whole tenant'); } },
            { id: 'act', h: '', cls: 'right', cell: function (k) {
                return M.btn('pick', S.picks[k.key] ? 'Change' : 'Choose', { cls: 'small', arg: k.key }) +
                    (S.picks[k.key] ? M.btn('unpick', 'Clear', { cls: 'small ghost', arg: k.key }) : ''); } }
        ];
        var page = rows.slice(S.kpg.page * S.kpg.size, (S.kpg.page + 1) * S.kpg.size);
        var t = M.table(cols, page, { plain: true });
        return '<h2>Choose a document for each kind</h2><p class="lede">The set covers a kind when it names one document of that kind. <b>' + covered() + ' of 26</b> kinds covered.</p>' +
            t + M.pager(S.kpg, rows.length) +
            M.gap('A set holds no rows today, so it cannot name a document. The set needs one row for each covered kind carrying the key of one document.') +
            '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 1 }) + '<span class="ml-auto"></span>' + M.btn('toReview', 'Review', { cls: 'primary', disabled: covered() === 0 }) + '</div>';
    }

    function reviewStep() {
        var tenant = Object.keys(S.picks).filter(function (k) { return !M.kind(k).party; });
        var h = '<h2>Review the set</h2><p class="lede">Read what you are about to propose.</p>' +
            '<dl class="plain"><dt>Name</dt><dd>' + M.esc(S.f.name) + '</dd><dt>Purpose</dt><dd>' + M.esc(S.f.purpose) + '</dd><dt>Description</dt><dd>' + M.dash(S.f.description) + '</dd>' +
            '<dt>Kinds covered</dt><dd><span class="num">' + covered() + ' of 26</span></dd></dl>' +
            '<h3>Documents the set names</h3><table class="rows"><thead><tr><th>Kind</th><th>Document</th><th>Terms in words</th></tr></thead><tbody>' +
            Object.keys(S.picks).map(function (k) {
                return '<tr><td>' + M.esc(M.kind(k).label) + '</td><td class="key">' + M.esc(S.picks[k]) + '</td><td class="dim">' + M.esc(M.words(M.doc(k, S.picks[k]))) + '</td></tr>';
            }).join('') + '</tbody></table>';
        h += '<h3>What it reaches</h3>';
        if (tenant.length) { h += M.notice('warn', '<b>' + tenant.length + ' of these documents are held for the whole tenant.</b> Every party of the tenant reads them through this set.'); }
        h += '<dl class="plain"><dt>Reports using it</dt><dd><span class="num">0</span> until a report author chooses it</dd><dt>Reports that could move to it</dt><dd><span class="num">4</span> that bind a conventions configuration</dd></dl>';
        h += M.notice('info', 'Market Risk decides a new set. The set does not exist until it approves, and you cannot approve your own request.');
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 2 }) + '<span class="ml-auto"></span>' + M.btn('propose', 'Propose', { cls: 'primary' }) + '</div>';
    }

    function requestStep() {
        var st = S.withdrawn ? ['inactive', 'Withdrawn'] : ['pending', 'Pending'];
        var h = '<h2>Your request</h2><p class="lede">A new set needs a second person.</p>' +
            '<dl class="plain"><dt>Request</dt><dd>New set: ' + M.esc(S.f.name || 'Dubai swaps desk') + '</dd><dt>State</dt><dd>' + M.badge(st[0], st[1]) + '</dd>' +
            '<dt>Asked by</dt><dd>' + M.esc(S.person.name) + '</dd><dt>Decides</dt><dd>Market Risk</dd><dt>Asked</dt><dd>just now</dd></dl><div style="height:16px"></div>';
        h += S.withdrawn ? M.notice('info', '<b>You withdrew this request.</b> No set was created.')
            : M.notice('info', '<b>Waiting for Market Risk.</b> The set does not exist yet, so no report can choose it.');
        h += M.gap('No approval kind exists for a set, a request holds no pending set, and nothing creates the set when the last decider approves.');
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back to the sets', { cls: 'ghost', arg: 0 }) + '<span class="ml-auto"></span>' +
            (!S.withdrawn ? M.btn('withdraw', 'Withdraw', { cls: 'ghost' }) : '') +
            '<a class="btn primary" href="../decide-a-convention-change/?actor=t.brandt&request=r2">Open the request as Market Risk</a></div>';
    }

    /* ------------------------------------------------------------ */

    var P = { kind: null, q: '', pg: { page: 0, size: 15 } };

    function pickerDialog() {
        var k = M.kind(P.kind);
        var all = M.docs(P.kind).filter(function (d) { return !P.q || d.key.toLowerCase().indexOf(P.q.toLowerCase()) >= 0; });
        var page = all.slice(P.pg.page * P.pg.size, (P.pg.page + 1) * P.pg.size);
        var html = '<h2>Choose a ' + M.esc(k.label.toLowerCase()) + ' document</h2><p class="lede">The set names one document of this kind.</p>' +
            '<div class="listtools"><div class="searchbox">' + M.icon('search') + '<input type="text" placeholder="Search by key" data-input="pq" value="' + M.esc(P.q) + '"></div></div>' +
            (M.modelled(P.kind) ? '<p class="listnote">The search matches the key. To find a document by its terms, <a href="../find-a-convention-by-its-terms/?kind=' + P.kind + '" target="_blank" rel="noopener">find a convention by its terms</a>.</p>' : '') +
            (all.length === 0 ? '<div class="tblstate">No ' + M.esc(k.label.toLowerCase()) + ' documents match the search. ' + M.btn('pclear', 'Clear', { cls: 'small' }) + '</div>' :
                '<table class="rows"><thead><tr><th>Key</th><th>Terms in words</th><th class="right">Version</th></tr></thead><tbody>' + page.map(function (d, i) {
                    return '<tr class="pick' + (S.picks[P.kind] === d.key ? ' on' : '') + '" tabindex="0" data-prow="' + i + '"><td class="key">' + M.esc(d.key) + '</td><td class="dim">' + M.esc(M.words(d)) + '</td><td class="num">' + d.version + '</td></tr>';
                }).join('') + '</tbody></table>') + M.pager(P.pg, all.length).replace(/data-act="pg"/g, 'data-act="ppg"') +
            '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + '</div>';
        P.page = page;
        M.dialog(html, true);
        var sc = document.getElementById('dialog-scrim');
        M.wireSelects(sc, function (id, v) { if (id === 'pagesize') { P.pg.size = Number(v); P.pg.page = 0; } pickerDialog(); });
        var qi = sc.querySelector('[data-input=pq]');
        qi.addEventListener('input', function () { P.q = qi.value; P.pg.page = 0; var pos = qi.selectionStart; pickerDialog(); var n = document.querySelector('[data-input=pq]'); n.focus(); n.setSelectionRange(pos, pos); });
        sc.querySelectorAll('tr[data-prow]').forEach(function (tr) {
            tr.addEventListener('click', function () { S.picks[P.kind] = P.page[Number(tr.getAttribute('data-prow'))].key; M.closeDialog(); render(); });
        });
    }

    /* ------------------------------------------------------------ */

    function render() {
        var body = [setsStep, describeStep, docsStep, reviewStep, requestStep][S.step]();
        document.getElementById('app').innerHTML = M.shell('Reference data', S.person) + '<div class="page">' +
            M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Name a convention set' }], '../see-the-conventions-a-party-uses/') +
            M.pagehead('Name a convention set', 'The named sets of convention documents a report can choose.', M.accessMark(canWrite())) +
            '<div class="journey">' + M.rail(STEPS, S.step) + '<div class="card">' + body + '</div></div></div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: '0', t: 'Sets' }, { v: '1', t: 'Describe' }, { v: '2', t: 'Documents' },
            { v: '3', t: 'Review' }, { v: '4', t: 'Request' }, { v: 'k.mori', t: 'Read-only person' }], String(S.step));
        M.wireSelects(document.getElementById('app'), function (id, v) {
            if (id === 'pagesize') { (S.step === 2 ? S.kpg : S.pg).size = Number(v); (S.step === 2 ? S.kpg : S.pg).page = 0; }
            render();
        });
        ['name', 'purpose', 'description'].forEach(function (id) {
            var el = document.querySelector('[data-input=' + id + ']');
            if (el) { el.addEventListener('input', function () { S.f[id] = el.value; }); }
        });
    }

    function go(n) { S.step = n; render(); }

    document.addEventListener('click', function (e) {
        var b = e.target.closest('[data-act]');
        if (!b || b.closest('.sel')) { return; }
        var name = b.getAttribute('data-act');
        var arg = b.getAttribute('data-arg');
        if (name === 'bar') { return bar(arg); }
        if (name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (name === 'goto') { go(Number(arg)); }
        else if (name === 'toDocs') { S.tried = true; if (Object.keys(problems()).length) { render(); } else { go(2); } }
        else if (name === 'toReview') { go(3); }
        else if (name === 'sort') { S.sort = { id: arg, dir: S.sort.id === arg ? -S.sort.dir : 1 }; render(); }
        else if (name === 'pg') { M.pageStep(S.step === 2 ? S.kpg : S.pg, arg, S.step === 2 ? M.KINDS.length : SETS.length); render(); }
        else if (name === 'pick') { P = { kind: arg, q: '', pg: { page: 0, size: 15 } }; pickerDialog(); }
        else if (name === 'ppg') { var tot = M.docs(P.kind).filter(function (d) { return !P.q || d.key.toLowerCase().indexOf(P.q.toLowerCase()) >= 0; }).length; M.pageStep(P.pg, arg, tot); pickerDialog(); }
        else if (name === 'pclear') { P.q = ''; pickerDialog(); }
        else if (name === 'unpick') { delete S.picks[arg]; render(); }
        else if (name === 'cancel') { M.closeDialog(); }
        else if (name === 'propose') { S.withdrawn = false; go(4); }
        else if (name === 'withdraw') { S.withdrawn = true; render(); }
    });

    function bar(arg) {
        if (M.PEOPLE[arg]) { S.person = M.PEOPLE[arg]; S.step = 0; render(); return; }
        var n = Number(arg);
        if (n >= 1 && !S.f.name) { S.f = { name: 'Dubai swaps desk', purpose: 'Terms for the Dubai swaps book', description: 'The AED swap and deposit terms the desk books on.' }; }
        if (n >= 3 && covered() === 0) { S.picks = { swap: 'AED-EIBOR-6M-SWAP', deposit: 'AED-DEPO-1M' }; }
        S.withdrawn = false;
        go(n);
    }

    if (S.step >= 3 && covered() === 0) { S.picks = { swap: 'AED-EIBOR-6M-SWAP', deposit: 'AED-DEPO-1M' }; }
    if (S.step >= 3 && !S.f.name) { S.f = { name: 'Dubai swaps desk', purpose: 'Terms for the Dubai swaps book', description: 'The AED swap and deposit terms the desk books on.' }; }
    render();
    if (Q.get('picker')) { P = { kind: Q.get('picker'), q: Q.get('q') || '', pg: { page: 0, size: 15 } }; pickerDialog(); }
})();

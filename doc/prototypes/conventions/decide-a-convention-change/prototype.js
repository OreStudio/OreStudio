/* Decide a convention change.
 *
 * Four steps on one rail, in the Requests area: the queue of requests the
 * person may decide, one change read as old term beside new term, the
 * decision, and its outcome. The person who asked never sees the request in
 * the queue, and a person who cannot decide sees only their own requests. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();
    var STEPS = ['The queue', 'The change', 'The decision', 'The outcome'];

    var curated = M.doc('swap', 'AED-EIBOR-6M-SWAP');

    /* The change each request asks for. kind: change, new, delete, set. */
    var REQUESTS = [
        { id: 'r1', title: 'Change AED swap day count', kind: 'change', by: 'm.okafor', at: '2026-10-17T12:00:00Z',
          target: 'swap', key: 'AED-EIBOR-6M-SWAP', version: 2, reason: 'The AED swaps are booked on the wrong basis.',
          diff: { fixed_day_count: ['ACT/365', 'ACT/360'] } },
        { id: 'r2', title: 'New set: Dubai swaps desk', kind: 'set', by: 'k.mori', at: '2026-10-18T12:00:00Z',
          reason: 'No set on the list fits the Dubai swaps desk.',
          set: { name: 'Dubai swaps desk', purpose: 'Terms for the Dubai swaps book',
              description: 'The AED swap and deposit terms the desk books on.',
              docs: [['swap', 'AED-EIBOR-6M-SWAP'], ['deposit', 'AED-DEPO-1M']] } },
        { id: 'r3', title: 'Delete AED-EIBOR-6M-SWAP-201', kind: 'delete', by: 'm.okafor', at: '2026-10-19T08:00:00Z',
          target: 'swap', key: 'AED-EIBOR-6M-SWAP-201', version: 1, reason: 'Duplicate of AED-EIBOR-6M-SWAP.' },
        { id: 'r4', title: 'New swap document AED-OIS-SWAP-NEW', kind: 'new', by: 'j.ruiz', at: '2026-10-19T09:30:00Z',
          target: 'swap', key: 'AED-OIS-SWAP-NEW', version: 0, reason: 'A counterparty agreement states terms no document carries.',
          terms: { fixed_calendar: 'AED', fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing', fixed_day_count: 'ACT/360',
              index: 'AED-EIBOR-6M', float_frequency: 'Semi-Annual', sub_periods: '' } }
    ];
    var KIND_TEXT = { change: 'Change', new: 'New document', delete: 'Delete', set: 'New set' };

    var S = {
        step: Number(Q.get('step') || 0),
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['j.ruiz'],
        reqId: Q.get('request'),
        decision: Q.get('decision') || '',
        comment: Q.get('comment') || '',
        tried: false,
        done: null,
        search: '',
        filter: 'all',
        sort: { id: 'at', dir: -1 },
        pg: { page: 0, size: 15 }
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }
    if (S.reqId && S.step === 0) { S.step = 1; }

    function decider() { return S.person.decides.indexOf('market_risk') >= 0; }
    function req() { return REQUESTS.filter(function (r) { return r.id === S.reqId; })[0] || queue()[0]; }
    function who(id) { return (M.PEOPLE[id] || { name: id }).name; }

    function queue() {
        return REQUESTS.filter(function (r) { return r.by !== S.person.id; });
    }
    function mine() { return REQUESTS.filter(function (r) { return r.by === S.person.id; }); }

    /* ------------------------------------------------------------ */

    function queueStep() {
        if (!decider()) {
            var own = mine();
            return '<h2>My requests</h2><p class="lede">The changes you asked for, and where each one waits.</p>' +
                (own.length === 0 ? '<div class="tblstate">You have no requests.</div>' :
                    '<table class="rows"><thead><tr><th>Request</th><th>Change</th><th>Decides</th><th>State</th><th>Asked</th></tr></thead><tbody>' +
                    own.map(function (r) {
                        return '<tr><td>' + M.esc(r.title) + '</td><td>' + M.badge('class', KIND_TEXT[r.kind]) + '</td><td>Market Risk</td><td>' +
                            M.badge('pending', 'Pending') + '</td><td>' + M.relTime(r.at) + '</td></tr>';
                    }).join('') + '</tbody></table>') +
                M.notice('info', 'Nothing waits for you to decide.');
        }
        var cols = [
            { id: 'title', h: 'Request', cell: function (r) { return M.esc(r.title); }, val: function (r) { return r.title; } },
            { id: 'by', h: 'Asked by', cell: function (r) { return M.esc(who(r.by)); }, val: function (r) { return who(r.by); } },
            { id: 'kind', h: 'Change', cell: function (r) { return M.badge('class', KIND_TEXT[r.kind]); }, val: function (r) { return r.kind; } },
            { id: 'at', h: 'Waiting', cell: function (r) { return M.relTime(r.at); }, val: function (r) { return r.at; } }
        ];
        var rows = queue().filter(function (r) {
            return (S.filter === 'all' || r.kind === S.filter) && (!S.search || (r.title + ' ' + who(r.by)).toLowerCase().indexOf(S.search.toLowerCase()) >= 0);
        });
        rows = M.sortRows(rows, cols, S.sort);
        S.shown = rows.slice(S.pg.page * S.pg.size, (S.pg.page + 1) * S.pg.size);
        return '<h2>Requests waiting for Market Risk</h2><p class="lede">Changes that other people asked for. Your own requests are not listed.</p>' +
            '<div class="listtools"><div class="searchbox">' + M.icon('search') + '<input type="text" placeholder="Search requests" data-input="search" value="' + M.esc(S.search) + '"></div>' +
            M.select('filter', [{ v: 'all', t: 'All changes' }].concat(Object.keys(KIND_TEXT).map(function (k) { return { v: k, t: KIND_TEXT[k] }; })), S.filter, { inline: true }) + '</div>' +
            (rows.length === 0 ? '<div class="tblstate">No requests match the search. ' + M.btn('clear', 'Clear', { cls: 'small' }) + '</div>'
                : M.table(cols, S.shown, { sort: S.sort })) + M.pager(S.pg, rows.length) +
            M.gap('A convention has no approval kind, so the queue lists no convention request today. The permission to decide for Market Risk is seeded, and the kind that carries it for a convention is not.');
    }

    function changeBody(r) {
        if (r.kind === 'set') {
            return '<dl class="plain"><dt>Name</dt><dd>' + M.esc(r.set.name) + '</dd><dt>Purpose</dt><dd>' + M.esc(r.set.purpose) + '</dd><dt>Description</dt><dd>' + M.esc(r.set.description) + '</dd></dl>' +
                '<h3>Documents the set names</h3><table class="rows"><thead><tr><th>Kind</th><th>Document</th><th>Terms in words</th></tr></thead><tbody>' +
                r.set.docs.map(function (p) {
                    var d = M.doc(p[0], p[1]);
                    return '<tr><td>' + M.esc(M.kind(p[0]).label) + '</td><td class="key">' + M.esc(p[1]) + '</td><td class="dim">' + M.esc(d ? M.words(d) : '') + '</td></tr>';
                }).join('') + '</tbody></table>';
        }
        var cols = M.COLUMNS[r.target];
        var base = r.kind === 'new' ? { terms: r.terms } : M.doc(r.target, r.key);
        return '<table class="diff"><thead><tr><th>Term</th><th>' + (r.kind === 'new' ? 'Now' : 'Now') + '</th><th>' +
            (r.kind === 'delete' ? 'After the delete' : 'Asked for') + '</th></tr></thead><tbody>' + cols.map(function (c) {
                var now = r.kind === 'new' ? '' : (base.terms[c.id] || '');
                var next = r.kind === 'new' ? (r.terms[c.id] || '') : r.kind === 'delete' ? '' : (r.diff && r.diff[c.id] ? r.diff[c.id][1] : now);
                var changed = r.kind === 'new' ? true : r.kind === 'delete' ? true : (r.diff && r.diff[c.id]);
                var nowCell = r.kind === 'new' ? '<span class="muted-dash">—</span>' : (changed && r.kind === 'change' ? '<span class="was">' + M.esc(now) + '</span>' : M.dash(now));
                var nextCell = r.kind === 'delete' ? '<span class="muted-dash">Not read by runs</span>' : (changed ? '<span class="now">' + M.esc(next) + '</span>' : M.dash(next));
                return '<tr class="' + (changed ? 'changed' : '') + '"><td>' + M.esc(c.h) + '</td><td>' + nowCell + '</td><td>' + nextCell + '</td></tr>';
            }).join('') + '</tbody></table>';
    }

    function changeStep() {
        var r = req();
        var tenant = r.target && !M.kind(r.target).party;
        var h = '<h2>' + M.esc(r.title) + '</h2><p class="lede">' + M.badge('pending', 'Pending') + ' Asked by ' + M.esc(who(r.by)) + ' ' + M.relTime(r.at) + '. Expires in ' + (14 - Math.round((M.NOW - Date.parse(r.at)) / 86400000)) + ' days.</p>' +
            '<h3 style="margin-top:0">Reason given</h3><p>' + M.esc(r.reason) + '</p><h3>The change</h3>' + changeBody(r);
        h += '<h3>What it reaches</h3><dl class="plain"><dt>Read by</dt><dd>' + (r.kind === 'set' ? 'Every report that chooses the set' :
            (tenant ? 'Every party of the tenant' : 'Every definition of ACME, through the party’s conventions file')) + '</dd>' +
            '<dt>Runs affected</dt><dd class="faint">Not recorded</dd></dl>';
        h += '<h3>The rule</h3><p class="dim">1 approval from Market Risk. The person who asked cannot decide. Decisions so far: none.</p>';
        h += M.gap('A request holds a kind and a reason and no pending change, so the change panel reads a fixture. The pending row needs a home: the request, or a staged row of the convention table.');
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back to the queue', { cls: 'ghost', arg: 0 }) + '<span class="ml-auto"></span>' + M.btn('goto', 'Decide', { cls: 'primary', arg: 2 }) + '</div>';
    }

    function choice(v, name, sub) {
        return '<div class="choice' + (S.decision === v ? ' on' : '') + '" role="radio" aria-checked="' + (S.decision === v) + '" tabindex="0" data-act="decide" data-arg="' + v + '"><span class="dot"></span>' +
            '<span><span class="nm">' + name + '</span><br><span class="sub">' + sub + '</span></span></div>';
    }

    function decisionStep() {
        var r = req();
        var needs = S.decision === 'decline';
        var bad = S.tried && needs && !S.comment.trim();
        var ok = S.decision && !(needs && !S.comment.trim());
        return '<h2>Decide: ' + M.esc(r.title) + '</h2><p class="lede">Your decision is recorded under your name.</p>' +
            '<div role="radiogroup" aria-label="Decision">' + choice('approve', 'Approve', 'The change applies when the last required approval arrives.') +
            choice('decline', 'Decline', 'The request ends and the record keeps its version. A comment is required.') +
            choice('hold', 'Hold', 'The request stays in the queue and the person who asked is told.') + '</div>' +
            '<div class="field' + (bad ? ' bad' : '') + '" style="margin-top:16px"><span class="lbl">Comment' + (needs ? ' <span class="req">(required)</span>' : '') + '</span>' +
            '<textarea data-input="comment">' + M.esc(S.comment) + '</textarea>' + (bad ? '<p class="msg">Say why you decline, so the requester can act.</p>' : '') + '</div>' +
            '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 1 }) + '<span class="ml-auto"></span>' +
            M.btn('send', 'Send the decision', { cls: 'primary', disabled: !S.decision }) + '</div>';
    }

    function outcomeStep() {
        var r = req();
        var d = S.done;
        var h = '<h2>' + M.esc(r.title) + '</h2><p class="lede">' + (d === 'approve' ? M.badge('ok', 'Approved') : d === 'decline' ? M.badge('bad', 'Declined') : M.badge('caution', 'On hold')) +
            ' Decided by ' + M.esc(S.person.name) + ' just now.</p>';
        if (d === 'approve') {
            h += M.notice('success', '<b>The change is applied.</b> ' + (r.kind === 'set' ? 'The set exists and a report can choose it.' :
                r.kind === 'delete' ? 'Runs of ACME no longer read the document. Version ' + (r.version + 1) + ' records the delete.' :
                r.kind === 'new' ? 'The document exists as version 1.' : 'Version ' + (r.version + 1) + ' of ' + r.key + ' records it.'));
            h += M.gap('Nothing applies a convention change when the last decider approves, so an approval changes nothing today. The apply must write the row against the version it read, and refuse if the row changed after the request was raised.');
        } else if (d === 'decline') {
            h += M.notice('error', '<b>The request is declined.</b> ' + (r.kind === 'new' || r.kind === 'set' ? 'Nothing was created.' : 'The record keeps version ' + r.version + '.') + ' ' + M.esc(who(r.by)) + ' reads your comment: “' + M.esc(S.comment) + '”');
        } else {
            h += M.notice('warn', '<b>The request is on hold.</b> It stays in the queue until someone resumes it, and ' + M.esc(who(r.by)) + ' is told.');
        }
        return h + '<div class="stepfoot"><span class="ml-auto"></span>' + M.btn('again', 'Back to the queue', { cls: 'primary' }) + '</div>';
    }

    /* ------------------------------------------------------------ */

    function render() {
        var steps = decider() ? STEPS : [STEPS[0]];
        var body = S.step === 0 ? queueStep() : S.step === 1 ? changeStep() : S.step === 2 ? decisionStep() : outcomeStep();
        document.getElementById('app').innerHTML = M.shell('Requests', S.person) + '<div class="page">' +
            M.crumbs([{ t: 'Requests', act: 'goto0' }].concat(S.step === 0 ? [{ t: decider() ? 'Queue' : 'My requests' }] : [{ t: 'Queue', act: 'goto0' }, { t: req().title }])) +
            M.pagehead('Requests', 'Every request, your own, and the queue of those you decide.', '') +
            '<div class="journey">' + M.rail(steps, S.step) + '<div class="card">' + body + '</div></div></div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: 'j.ruiz', t: 'J. Ruiz (Market Risk)' }, { v: 't.brandt', t: 'T. Brandt (Market Risk)' },
            { v: 'm.okafor', t: 'M. Okafor (Operations)' }, { v: 's1', t: 'Change' }, { v: 's2', t: 'Decision' }], S.person.id);
        M.wireSelects(document.getElementById('app'), function (id, v) {
            if (id === 'filter') { S.filter = v; S.pg.page = 0; }
            if (id === 'pagesize') { S.pg.size = Number(v); S.pg.page = 0; }
            render();
        });
        var si = document.querySelector('[data-input=search]');
        if (si) { si.addEventListener('input', function () { S.search = si.value; S.pg.page = 0; var pos = si.selectionStart; render(); var n = document.querySelector('[data-input=search]'); n.focus(); n.setSelectionRange(pos, pos); }); }
        var ci = document.querySelector('[data-input=comment]');
        if (ci) { ci.addEventListener('input', function () { S.comment = ci.value; var ok = S.decision && !(S.decision === 'decline' && !S.comment.trim()); void ok; }); }
    }

    function go(n) { S.step = n; render(); }

    document.addEventListener('click', function (e) {
        var tr = e.target.closest('tr[data-row]');
        if (tr && S.step === 0 && S.shown) { S.reqId = S.shown[Number(tr.getAttribute('data-row'))].id; go(1); return; }
        var b = e.target.closest('[data-act]');
        if (!b || b.closest('.sel')) { return; }
        var name = b.getAttribute('data-act');
        var arg = b.getAttribute('data-arg');
        if (name === 'bar') { return bar(arg); }
        if (name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (name === 'goto') { go(Number(arg)); }
        else if (name === 'goto0') { go(0); }
        else if (name === 'decide') { S.decision = arg; render(); }
        else if (name === 'send') {
            S.tried = true;
            if (S.decision === 'decline' && !S.comment.trim()) { render(); return; }
            S.done = S.decision; go(3);
        }
        else if (name === 'again') { S.decision = ''; S.comment = ''; S.tried = false; S.reqId = null; go(0); }
        else if (name === 'sort') { S.sort = { id: arg, dir: S.sort.id === arg ? -S.sort.dir : 1 }; render(); }
        else if (name === 'pg') { M.pageStep(S.pg, arg, queue().length); render(); }
        else if (name === 'clear') { S.search = ''; S.filter = 'all'; render(); }
    });
    document.addEventListener('keydown', function (e) {
        if (e.key !== 'Enter' && e.key !== ' ') { return; }
        var t = e.target;
        if (t.closest && t.closest('tr[data-row]')) { t.closest('tr[data-row]').click(); }
        else if (t.getAttribute && t.getAttribute('role') === 'radio') { e.preventDefault(); t.click(); }
    });

    function bar(arg) {
        if (M.PEOPLE[arg]) { S.person = M.PEOPLE[arg]; S.reqId = null; S.step = 0; render(); return; }
        if (!S.reqId) { S.reqId = queue()[0].id; }
        S.step = arg === 's1' ? 1 : 2;
        render();
    }

    render();
})();

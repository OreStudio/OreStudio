/* Retire a convention.
 *
 * Four steps on one rail: the document as a record page, what reads it, the
 * request that waits for Market Risk, and the history. The delete is a proposal,
 * so nothing changes until a second person approves. The reach panel states
 * what it cannot read, and does not show an empty list as if it meant none. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();
    var STEPS = ['The document', 'What reads it', 'The request', 'The history'];
    var KINDS = ['swap', 'deposit', 'ibor_index'];

    var kind = KINDS.indexOf(Q.get('kind')) >= 0 ? Q.get('kind') : 'swap';
    var S = {
        step: Number(Q.get('step') || 0),
        kind: kind,
        key: Q.get('key') || (kind === 'swap' ? 'AED-EIBOR-6M-SWAP' : M.docs(kind)[0].key),
        outcome: Q.get('outcome') || 'waiting',
        tab: 'details',
        raised: Number(Q.get('step') || 0) >= 2,
        withdrawn: false,
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor'],
        form: { reason: '', commentary: '', tried: false }
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }
    if (!M.doc(S.kind, S.key)) { S.key = M.docs(S.kind)[0].key; }

    function doc() { return M.doc(S.kind, S.key); }
    function canWrite() { return S.person.part === 'Operations' || S.person.part === 'Market Risk'; }
    function isDeleted() { return S.raised && S.outcome === 'approved' && !S.withdrawn; }

    /* Fixtures: the curated swap is the one document a set names. */
    function reach() {
        var named = doc().key === 'AED-EIBOR-6M-SWAP';
        return { sets: named ? ['Standard market'] : [], reports: named ? 4 : 0 };
    }
    function twins() { return M.docs(S.kind).filter(function (x) { return x.key !== S.key && M.sameTerms(x, doc()); }); }

    /* The versions of the document, newest first. */
    function versions() {
        var d = doc();
        var v = [];
        if (isDeleted()) {
            v.push({ n: d.version + 1, by: S.person.id, at: '2026-10-19T11:42:00Z', reason: 'Deleted', code: 'common.obsolete',
                note: 'Replaced by a newer swap document', diff: [['Valid to', 'open', '2026-10-19']], del: true });
        }
        if (d.version >= 2) {
            v.push({ n: 2, by: 'm.okafor', at: '2026-10-12T09:20:00Z', reason: 'Rectification', code: 'common.rectification',
                note: 'Corrected the float frequency', diff: [['Float frequency', 'Quarterly', 'Semi-Annual']] });
        }
        v.push({ n: 1, by: d.modifiedBy, at: d.recordedAt, reason: d.reason, code: d.reason, note: d.commentary, diff: null });
        return v;
    }

    /* ------------------------------------------------------------ */

    function recordPage() {
        var d = doc();
        var gone = isDeleted();
        var acts = canWrite() && !gone ? M.btn('goto', 'Delete', { cls: 'danger', icon: 'delete', arg: 1 }) : '';
        var details = '<dl class="fields">' + (M.COLUMNS[S.kind] || []).map(function (c) {
            return '<div><dt>' + M.esc(c.h) + '</dt><dd>' + M.dash(d.terms[c.id]) + '</dd></div>';
        }).join('') + '</dl><div class="lastchanged">Last changed: version ' + (gone ? d.version + 1 : d.version) + ' · ' +
            M.esc(gone ? 'm.okafor' : d.modifiedBy) + ' · ' + M.relTime(gone ? '2026-10-19T11:42:00Z' : d.recordedAt) + '</div>';
        return M.pagehead(d.key, d.key + ' · version ' + (gone ? d.version + 1 : d.version),
            (gone ? M.badge('inactive', 'Deleted') : '') + M.accessMark(canWrite() && !gone) + acts).replace('<h1>', '<h1 class="mono">') +
            '<div class="tabs"><button class="on">Details</button><button data-act="goto" data-arg="3">History</button></div>' + details +
            '<div class="stepfoot"><span class="ml-auto"></span>' + (canWrite() && !gone ? M.btn('goto', 'Next: what reads it', { cls: 'primary', arg: 1 }) : '') + '</div>';
    }

    function readers() {
        var d = doc();
        var r = reach();
        var tw = twins();
        var k = M.kind(S.kind);
        var rows = [
            ['ACME’s conventions file', 'Carries every ' + k.label.toLowerCase() + ' document of the party', '<span class="num">' + M.docs(S.kind).length + ' documents</span>'],
            ['Convention sets', 'Name this document', r.sets.length ? '<span>' + r.sets.map(M.esc).join(', ') + '</span>' : M.dash('')],
            ['Report definitions', 'Bound to those sets', '<span class="num">' + r.reports + '</span>'],
            ['Trades', 'Carry their terms inline', M.dash('')],
            ['Runs', 'Read the document the file carries', '<span class="faint">Not recorded</span>']
        ];
        var h = '<h2>What reads ' + M.esc(d.key) + '</h2><p class="lede">Read this before you delete the document.</p>';
        if (!k.party) { h += M.notice('warn', '<b>Every party reads this document.</b> It is held once for the tenant.'); }
        if (r.sets.length) {
            h += M.notice('warn', '<b>' + r.sets.length + ' set names this document.</b> After the delete the set names a row that is gone.');
        }
        h += '<table class="rows"><thead><tr><th>Reader</th><th>What it does</th><th>Reads this document</th></tr></thead><tbody>' +
            rows.map(function (x) { return '<tr><td>' + x[0] + '</td><td class="dim">' + x[1] + '</td><td>' + x[2] + '</td></tr>'; }).join('') + '</tbody></table>';
        h += '<h3>Documents with the same terms</h3>' + (tw.length === 0
            ? '<p class="dim">None yet.</p>'
            : '<p class="dim">' + tw.length + (tw.length === 1 ? ' other document states' : ' other documents state') + ' the same terms, so a run keeps these terms after the delete.</p><p class="mono">' +
              tw.slice(0, 4).map(function (x) { return M.esc(x.key); }).join(', ') + '</p>');
        h += M.gap('A set holds no rows and no run records the version it read, so the last two readers cannot be listed. The delete does not check that a document is unreferenced.');
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 0 }) + '<span class="ml-auto"></span>' +
            M.btn('delete', 'Delete', { cls: 'danger', icon: 'delete' }) + '</div>';
    }

    function deleteDialog() {
        var d = doc();
        var f = S.form;
        var bad = f.tried && !f.reason;
        var html = '<h2>Delete ' + M.esc(d.key) + '?</h2><p class="lede">Runs of ACME stop reading this document. Its history stays, and the sets that name it keep a name that points at a deleted row.</p>' +
            '<div class="field' + (bad ? ' bad' : '') + '"><span class="lbl">Reason <span class="req">(required)</span></span>' +
            M.select('reason', M.CHANGE_REASONS.delete, f.reason, { placeholder: 'Choose a reason' }) +
            (bad ? '<p class="msg">Choose a reason for the delete.</p>' : '') + '</div>' +
            '<div class="field"><span class="lbl">Commentary</span><textarea data-input="commentary">' + M.esc(f.commentary) + '</textarea></div>' +
            M.notice('info', 'Market Risk decides this change. Nothing changes until it approves, and you cannot approve your own request.') +
            '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('propose', 'Propose', { cls: 'primary' }) + '</div>';
        M.dialog(html);
        M.wireSelects(document.getElementById('dialog-scrim'), function (id, v) {
            S.form.reason = v;
            S.form.commentary = (document.querySelector('[data-input=commentary]') || {}).value || S.form.commentary;
            deleteDialog();
        });
    }

    function requestStep() {
        var d = doc();
        var st = S.withdrawn ? ['inactive', 'Withdrawn'] : S.outcome === 'approved' ? ['ok', 'Approved'] : S.outcome === 'declined' ? ['bad', 'Declined'] : ['pending', 'Pending'];
        var h = '<h2>Your request</h2><p class="lede">A change that touches a convention needs a second person.</p>' +
            '<dl class="plain"><dt>Request</dt><dd>Delete <span class="mono">' + M.esc(d.key) + '</span></dd>' +
            '<dt>State</dt><dd>' + M.badge(st[0], st[1]) + '</dd><dt>Asked by</dt><dd>' + M.esc(S.person.name) + '</dd>' +
            '<dt>Decides</dt><dd>Market Risk</dd><dt>Reason</dt><dd>' + M.esc(S.form.reason ? (M.CHANGE_REASONS.delete.filter(function (r) { return r.v === S.form.reason; })[0] || {}).t : 'Obsolete') + '</dd>' +
            '<dt>Asked</dt><dd>' + M.relTime('2026-10-19T11:42:00Z') + '</dd></dl><div style="height:16px"></div>';
        if (S.withdrawn) { h += M.notice('info', '<b>You withdrew this request.</b> The document is unchanged.'); }
        else if (S.outcome === 'waiting') { h += M.notice('info', '<b>Waiting for Market Risk.</b> The document stays as it is until a decider approves.'); }
        else if (S.outcome === 'approved') { h += M.notice('success', '<b>Market Risk approved the delete.</b> Version ' + (d.version + 1) + ' records it, and runs of ACME no longer read the document.'); }
        else { h += M.notice('error', '<b>Market Risk declined the delete.</b> “A twin of this document is not in the party yet.” The document keeps version ' + d.version + '. Add the twin, then ask again.'); }
        h += M.gap('No approval kind exists for a convention, a request holds no pending delete, and nothing applies the delete when the last decider approves. This step draws the request that the journey needs.');
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back', { cls: 'ghost', arg: 1 }) + '<span class="ml-auto"></span>' +
            (S.outcome === 'waiting' && !S.withdrawn ? M.btn('withdraw', 'Withdraw', { cls: 'ghost' }) : '') +
            M.btn('goto', 'History', { cls: 'primary', icon: 'history', arg: 3 }) + '</div>';
    }

    function historyStep() {
        var d = doc();
        var vs = versions();
        var h = '<h2>History</h2><p class="lede">Every version of <span class="mono">' + M.esc(d.key) + '</span>, newest first.</p><ol class="timeline">' +
            vs.map(function (v, i) {
                var cur = i === 0;
                return '<li><div class="when">' + M.relTime(v.at) + '</div><div><div class="head"><b>' + (v.del ? 'Deleted' : (v.n === 1 ? 'Created' : 'Changed')) +
                    '</b><span class="mono">' + M.esc(d.key) + ' v' + v.n + '</span><span class="mono dim">' + M.esc(v.code) + '</span><span class="dim">' + M.esc(v.by) + '</span>' +
                    (cur ? '<span class="cur" title="Current version">Current version</span>' : '') +
                    (!cur && canWrite() && !isDeleted() ? M.btn('revert', 'Revert to this version', { cls: 'small ghost', icon: 'arrow_rotate_counterclockwise', arg: v.n }) : '') +
                    '</div><p class="note">' + M.esc(v.note) + '</p>' +
                    (v.diff ? '<details><summary>Details</summary><table class="diff"><thead><tr><th>Field</th><th>Was</th><th>Now</th></tr></thead><tbody>' +
                        v.diff.map(function (r) { return '<tr class="changed"><td>' + M.esc(r[0]) + '</td><td class="was">' + M.esc(r[1]) + '</td><td class="now">' + M.esc(r[2]) + '</td></tr>'; }).join('') + '</tbody></table></details>' : '') +
                    '</div></li>';
            }).join('') + '</ol>';
        return h + '<div class="stepfoot">' + M.btn('goto', 'Back to the document', { cls: 'ghost', arg: 0 }) + '</div>';
    }

    /* ------------------------------------------------------------ */

    function render() {
        var d = doc();
        var body = S.step === 0 ? recordPage() : S.step === 1 ? readers() : S.step === 2 ? requestStep() : historyStep();
        document.getElementById('app').innerHTML = M.shell('Reference data', S.person) + '<div class="page">' +
            M.crumbs([{ t: 'Home' }, { t: 'Reference data' }, { t: 'Retire a convention' }], '../see-the-conventions-a-party-uses/?kind=' + S.kind + '&key=' + encodeURIComponent(d.key)) +
            '<div class="journey">' + M.rail(STEPS, S.step) + '<div class="card">' + body + '</div></div></div>';
        document.getElementById('proto-bar').innerHTML = M.bar([{ v: '0', t: 'Document' }, { v: '1', t: 'What reads it' },
            { v: 'dialog', t: 'Delete dialog' }, { v: 'waiting', t: 'Waiting' }, { v: 'approved', t: 'Approved' }, { v: 'declined', t: 'Declined' },
            { v: '3', t: 'History' }], S.step === 2 ? S.outcome : String(S.step));
        M.wireSelects(document.getElementById('app'), function () { render(); });
    }

    function go(n) { S.step = n; render(); }

    document.addEventListener('click', function (e) {
        var b = e.target.closest('[data-act]');
        if (!b) { return; }
        var name = b.getAttribute('data-act');
        var arg = b.getAttribute('data-arg');
        if (b.closest('.sel')) { return; }
        if (name === 'bar' || name === 'gaps') { return bar(name, arg); }
        if (name === 'goto') { go(Number(arg)); }
        else if (name === 'delete') { S.form.tried = false; deleteDialog(); }
        else if (name === 'cancel') { M.closeDialog(); }
        else if (name === 'propose') {
            var ta = document.querySelector('[data-input=commentary]');
            if (ta) { S.form.commentary = ta.value; }
            S.form.tried = true;
            if (!S.form.reason) { deleteDialog(); return; }
            M.closeDialog();
            S.raised = true; S.outcome = 'waiting'; S.withdrawn = false;
            go(2);
        }
        else if (name === 'withdraw') { S.withdrawn = true; render(); }
        else if (name === 'revert') { revertDialog(Number(arg)); }
        else if (name === 'revert-send') { M.closeDialog(); }
    });

    function revertDialog(n) {
        M.dialog('<h2>Revert to version ' + n + '?</h2><p class="lede">The values of version ' + n + ' are written back as a new version. No version is lost.</p>' +
            M.notice('info', 'Market Risk decides this change. Nothing changes until it approves.') +
            '<div class="foot">' + M.btn('cancel', 'Cancel', { cls: 'ghost', icon: 'dismiss' }) + M.btn('revert-send', 'Propose', { cls: 'primary' }) + '</div>');
    }

    function bar(name, arg) {
        if (name === 'gaps') { document.body.classList.toggle('nogaps'); render(); return; }
        if (arg === 'dialog') { S.withdrawn = false; S.step = 1; render(); S.form.tried = false; deleteDialog(); return; }
        if (arg === 'waiting' || arg === 'approved' || arg === 'declined') {
            S.raised = true; S.withdrawn = false; S.outcome = arg; S.step = 2; render(); return;
        }
        S.step = Number(arg);
        if (S.step < 2) { S.raised = false; }
        render();
    }

    render();
    if (Q.get('dialog') === '1') { S.step = 1; render(); deleteDialog(); }
})();

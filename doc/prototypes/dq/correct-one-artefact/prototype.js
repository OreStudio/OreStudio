/* Correct one artefact.
 *
 * Two screens: the staged rows of one dataset, and one row as a record page.
 * The correction is two acts, not one — save the staged row, then republish the
 * dataset — because a publish takes a dataset. The screen states the reach of
 * each act, so the person is never surprised that the target tables still hold
 * the old value after a save. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    M.NOW = Date.parse('2026-11-22T12:00:00Z');
    M.LOADED = '2026-11-22T11:58:00+00:00';

    var DATASET = { code: 'refdata.conventions', name: 'ORE Instrument Conventions', asOf: '2026-11-20' };

    var ROWS = [
        { key: 'AED-EIBOR-1M-SWAP', convention: 'modfollow', calendars: 'AED, AEB', frequency: 'Monthly', version: 1 },
        { key: 'AED-EIBOR-3M-SWAP', convention: 'modfollow', calendars: 'AED, AEB', frequency: 'Quarterly', version: 1 },
        { key: 'AED-EIBOR-6M-SWAP', convention: 'modfollow', calendars: 'AED, AEB', frequency: 'Semiannual', version: 3 }
    ];

    var CONVENTIONS = [
        ['modfollow', 'Modified following'],
        ['following', 'Following'],
        ['preceding', 'Preceding'],
        ['modpreceding', 'Modified preceding']
    ];

    var REASONS = [
        ['vendor_correction', 'Correction to the vendor list'],
        ['review_finding', 'A review found the value wrong'],
        ['transcription', 'Transcription error']
    ];

    var S = {
        row: Q.get('row') || 'AED-EIBOR-6M-SWAP',
        edit: Q.get('edit') === '1',
        saved: Q.get('saved') === '1',
        republished: Q.get('republished') === '1',
        convention: Q.get('convention') || 'modfollow',
        reason: Q.get('reason') || 'vendor_correction',
        comment: Q.get('comment') || '',
        filter: Q.get('q') || '',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function current() {
        return ROWS.filter(function (r) { return r.key === S.row; })[0] || ROWS[2];
    }

    function go(changes) {
        var q = new URLSearchParams(window.location.search);
        Object.keys(changes).forEach(function (k) {
            if (changes[k] === null) { q.delete(k); } else { q.set(k, changes[k]); }
        });
        window.location.search = q.toString();
    }

    function rowList() {
        var rows = ROWS.filter(function (r) {
            return !S.filter || r.key.toUpperCase().indexOf(S.filter.toUpperCase()) >= 0;
        });
        var body = rows.map(function (r) {
            return '<tr class="pick' + (r.key === S.row ? ' on' : '') + '" tabindex="0" data-row="' + M.esc(r.key) + '">' +
                '<td class="key">' + M.esc(r.key) + '</td>' +
                '<td>' + M.esc(r.convention) + '</td>' +
                '<td>' + M.esc(r.calendars) + '</td>' +
                '<td>' + M.esc(r.frequency) + '</td>' +
                '<td class="num">' + r.version + '</td></tr>';
        }).join('');
        return '<div class="listtools"><div class="searchbox">' + M.icon('search') +
            '<input type="search" id="q" placeholder="Search by key" value="' + M.esc(S.filter) + '"></div>' +
            '<span class="listnote">Read ' + rows.length + ' rows of this kind.</span></div>' +
            (rows.length
                ? '<table class="rows"><thead><tr><th>Key</th><th>Business day</th><th>Calendars</th>' +
                  '<th>Frequency</th><th class="right">Version</th></tr></thead><tbody>' + body + '</tbody></table>'
                : '<p class="tblstate">No row of this kind states that key.</p>');
    }

    function editForm() {
        var convention = M.select('convention', CONVENTIONS.map(function (c) {
            return { v: c[0], t: c[0] + ' — ' + c[1] };
        }), S.convention);
        var reason = M.select('reason', REASONS.map(function (r) {
            return { v: r[0], t: r[1] };
        }), S.reason);
        return '<h3>Change</h3>' +
            '<div class="grid2">' +
            '<div class="field"><span class="lbl">Business day convention</span>' + convention + '</div>' +
            '<div class="field"><span class="lbl">Reason</span>' + reason + '</div>' +
            '<div class="field span2"><span class="lbl">Commentary <span class="req">· required for a correction</span></span>' +
            '<textarea id="comment" placeholder="Why the value moved">' + M.esc(S.comment) + '</textarea></div>' +
            '</div>' +
            M.notice('warn', '<b>Reach of this change.</b> Saving writes the staged row only. The target ' +
                'tables keep the old value until <b>' + M.esc(DATASET.code) + '</b> is published again, ' +
                'because a publish takes a dataset and not a row.') +
            '<div class="stepfoot">' + M.btn('cancel', 'Cancel', { cls: 'ghost' }) +
            M.btn('save', 'Save the row', { cls: 'primary' }) + '</div>';
    }

    function recordPage() {
        var r = current();
        var changed = r.convention !== S.convention;
        var html = '<div class="cardhead"><h2>' + M.esc(r.key) + '</h2>' +
            '<span class="access">' + M.icon(S.saved ? 'lock_closed' : 'edit') +
            (S.saved ? 'Read only until the republish' : 'Editable') + '</span></div>';

        if (S.republished) {
            html += M.notice('success', '<b>Republished.</b> ' + DATASET.code +
                ' returned 1 updated and 10,990 skipped. The corrected value is in ' +
                'ores_refdata_conventions_tbl and the other rows were left as they were.') +
                '<dl class="plain"><dt>Workflow instance</dt><dd class="mono">9a21-55cd-1f07</dd>' +
                '<dt>Published by</dt><dd>' + M.person(S.person.id) + '</dd></dl>';
        } else if (S.saved) {
            html += M.notice('info', '<b>The staged row is saved.</b> Nothing in the target tables has ' +
                'changed yet. Publishing the dataset is what carries it.') +
                M.btn('republish', 'Republish ' + DATASET.code, { cls: 'primary' });
        }

        html += '<h3>Its values</h3>' +
            '<dl class="fields">' +
            '<div><dt>Business day convention</dt><dd class="mono">' +
            M.esc(S.saved || S.edit ? S.convention : r.convention) + '</dd></div>' +
            '<div><dt>Calendars</dt><dd>' + M.esc(r.calendars) + '</dd></div>' +
            '<div><dt>Fixed frequency</dt><dd>' + M.esc(r.frequency) + '</dd></div>' +
            '</dl>' +
            '<h3>Where it came from</h3>' +
            '<dl class="fields">' +
            '<div><dt>Dataset</dt><dd class="mono">' + M.esc(DATASET.code) + '</dd></div>' +
            '<div><dt>Reason code</dt><dd class="mono">system.external_data_import</dd></div>' +
            '<div><dt>As of</dt><dd class="mono">' + M.esc(DATASET.asOf) + '</dd></div>' +
            '<div><dt>Version</dt><dd class="mono">' + r.version + '</dd></div>' +
            '<div><dt>Recorded</dt><dd>' + M.relTime('2026-11-22T10:10:00Z') + '</dd></div>' +
            '<div><dt>Staged row</dt><dd class="mono">version ' + r.version + ' · ' + M.esc(DATASET.code) + '</dd></div>' +
            '</dl>';

        if (S.edit && !S.saved) { html += editForm(); }
        else if (!S.saved && !S.republished) {
            html += '<div class="stepfoot">' + M.btn('edit', 'Change one value', { cls: 'primary' }) + '</div>';
        }
        if (changed && S.saved) {
            html += '<p class="hint">The change you saved: business day convention ' +
                M.esc(r.convention) + ' → ' + M.esc(S.convention) + '.</p>';
        }
        return html;
    }

    function gaps() {
        if (document.body.classList.contains('nogaps')) { return ''; }
        return M.gap('Nothing reads or writes <b>ores_dq_*_artefact_tbl</b> over the bus. DQ publishes ' +
            'artefact types and datasets, and never the rows they stage, so this screen cannot list a row, ' +
            'read it or write it today.') +
            M.gap('The preview that says "one row updated, the rest skipped" is the bundle preview, which ' +
            'is per bundle and unserved. A per-dataset preview is what this journey needs.');
    }

    function render() {
        document.getElementById('app').innerHTML =
            M.shell('Reference data', S.person) +
            '<main class="page">' +
            M.crumbs([
                { t: 'Home', href: '#' },
                { t: 'Reference data', href: '#' },
                { t: 'Reference data coverage', href: '../reference-data-coverage/index.html' },
                { t: 'Correct an artefact' }
            ]) +
            M.pagehead('Correct one artefact',
                'refdata.conventions · one staged row, then the republish that carries it',
                M.refresh(false)) +
            '<div class="card cardgap"><div class="cardhead"><h2>The staged rows</h2>' +
            '<span class="dim">Read from the artefact table, which no subject serves</span></div>' +
            rowList() + '</div>' +
            '<div class="card cardgap">' + recordPage() + '</div>' + gaps() + '</main>';

        document.querySelectorAll('tr[data-row]').forEach(function (tr) {
            tr.addEventListener('click', function () {
                go({ row: tr.getAttribute('data-row'), edit: null, saved: null, republished: null });
            });
        });
        var search = document.getElementById('q');
        if (search) {
            search.addEventListener('change', function () { go({ q: search.value || null }); });
        }
        bind('edit', function () { go({ edit: '1' }); });
        bind('cancel', function () { go({ edit: null }); });
        bind('save', function () {
            var comment = document.getElementById('comment');
            go({ saved: '1', edit: null, comment: comment ? comment.value : null });
        });
        bind('republish', function () { go({ republished: '1' }); });
        M.wireSelects(document.getElementById('app'), function (id, value) {
            var change = {};
            change[id] = value;
            go(change);
        });

        document.getElementById('proto-bar').innerHTML = M.bar([
            { v: 'row', t: 'A row' },
            { v: 'edit', t: 'Change one value' },
            { v: 'saved', t: 'Saved, not published' },
            { v: 'done', t: 'Republished' }
        ], S.republished ? 'done' : (S.saved ? 'saved' : (S.edit ? 'edit' : 'row')));
        wireBar();
    }

    function bind(act, fn) {
        document.querySelectorAll('[data-act="' + act + '"]').forEach(function (b) {
            b.addEventListener('click', fn);
        });
    }

    function wireBar() {
        var bar = document.getElementById('proto-bar');
        bar.querySelectorAll('[data-act="bar"]').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var v = btn.getAttribute('data-arg');
                if (v === 'row') { go({ edit: null, saved: null, republished: null }); }
                if (v === 'edit') { go({ edit: '1', saved: null, republished: null }); }
                if (v === 'saved') { go({ edit: null, saved: '1', republished: null }); }
                if (v === 'done') { go({ saved: '1', republished: '1', edit: null }); }
            });
        });
        bar.querySelector('[data-act="gaps"]').addEventListener('click', function () {
            var q = new URLSearchParams(window.location.search);
            if (document.body.classList.contains('nogaps')) { q.delete('gaps'); } else { q.set('gaps', '0'); }
            window.location.search = q.toString();
        });
    }

    render();
})();

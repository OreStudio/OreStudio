/* Publish a dataset.
 *
 * The narrow door beside the bundle: one catalogue, its destination read from
 * its artefact type, the mode, the preview and the receipt. The destination is
 * never offered as a control, because the level that decides the tenant is a
 * property of the bundle the dataset belongs to — and the direct verb cannot
 * check it, which the page states. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    M.NOW = Date.parse('2026-11-22T12:00:00Z');
    M.LOADED = '2026-11-22T11:58:00+00:00';

    var STEPS = ['The dataset', 'The mode', 'The preview', 'The receipt'];

    var DATASET = {
        code: 'refdata.conventions',
        name: 'ORE Instrument Conventions',
        type: 'conventions',
        source: 'Vendor conventions list',
        asOf: '2026-11-20',
        artefactTable: 'ores_dq_conventions_artefact_tbl',
        targetTable: 'ores_refdata_conventions_tbl',
        subject: 'refdata.v1.ops.publish_conventions_from_dq',
        staged: 10991,
        published: 10991
    };

    var MODES = [
        ['upsert', 'Upsert', 'Insert what is missing and update what differs.'],
        ['insert_only', 'Insert only', 'Insert what is missing and leave the rows already there.'],
        ['replace_all', 'Replace all', 'Clear the target table and write the staged rows again.']
    ];

    var S = {
        step: Math.min(3, Math.max(0, Number(Q.get('step') || '1') - 1)),
        mode: MODES.some(function (m) { return m[0] === Q.get('mode'); }) ? Q.get('mode') : 'upsert',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function counts() {
        if (S.mode === 'replace_all') { return { insert: DATASET.staged, update: 0, skip: 0 }; }
        return { insert: Math.max(0, DATASET.staged - DATASET.published), update: 0, skip: DATASET.published };
    }

    function go(changes) {
        var q = new URLSearchParams(window.location.search);
        Object.keys(changes).forEach(function (k) {
            if (changes[k] === null) { q.delete(k); } else { q.set(k, changes[k]); }
        });
        window.location.search = q.toString();
    }

    function stepDataset() {
        return '<h2>What is being published</h2>' +
            '<p class="lede">One catalogue, on its own. The wide door is the bundle; this is the one that fixes a single dataset.</p>' +
            '<dl class="plain">' +
            '<dt>Code</dt><dd class="mono">' + M.esc(DATASET.code) + '</dd>' +
            '<dt>Name</dt><dd>' + M.esc(DATASET.name) + '</dd>' +
            '<dt>Artefact type</dt><dd class="mono">' + M.esc(DATASET.type) + '</dd>' +
            '<dt>Source</dt><dd>' + M.esc(DATASET.source) + ' · as of ' + M.esc(DATASET.asOf) + '</dd>' +
            '</dl>' +
            '<h3>Destination, read from the artefact type</h3>' +
            '<dl class="plain">' +
            '<dt>Staging table</dt><dd class="mono">' + M.esc(DATASET.artefactTable) + '</dd>' +
            '<dt>Target table</dt><dd class="mono">' + M.esc(DATASET.targetTable) + '</dd>' +
            '<dt>Subject</dt><dd class="mono">' + M.esc(DATASET.subject) + '</dd>' +
            '<dt>Staged</dt><dd class="mono">' + DATASET.staged + '</dd>' +
            '<dt>Published</dt><dd class="mono">' + DATASET.published + '</dd>' +
            '</dl>' +
            '<p class="hint">Staged counts the artefact table. Published counts the rows whose provenance ' +
            'names this dataset, and no subject offers that count.</p>';
    }

    function stepMode() {
        var modes = MODES.map(function (m) {
            return '<div class="choice' + (S.mode === m[0] ? ' on' : '') + '" role="radio" aria-checked="' +
                (S.mode === m[0]) + '" tabindex="0" data-mode="' + M.esc(m[0]) + '"><span class="dot"></span>' +
                '<span><span class="nm">' + M.esc(m[1]) + '</span><br><span class="sub">' +
                M.esc(m[2]) + '</span></span></div>';
        }).join('');
        return '<h2>How the run behaves</h2>' +
            '<p class="lede">The mode decides what happens to a row that is already there.</p>' +
            '<div role="radiogroup" aria-label="Mode">' + modes + '</div>' +
            M.notice('warn', '<b>This verb names no level.</b> The bundle verb compares the caller against ' +
                'the bundle\'s level and refuses a tenant that may not publish a world bundle. This one takes ' +
                'dataset identifiers, and a dataset carries no level, so there is nothing to compare. Until ' +
                'that is fixed, the target tenant is whatever the caller\'s context says.');
    }

    function stepPreview() {
        var c = counts();
        return '<h2>What the run would write</h2>' +
            '<p class="lede">Read before the run. A dataset whose rows agree reports every row skipped.</p>' +
            M.table([
                { id: 'dataset', h: 'Dataset', cell: function () { return '<span class="key">' + M.esc(DATASET.code) + '</span>'; } },
                { id: 'staged', h: 'Staged', cls: 'num', cell: function () { return String(DATASET.staged); } },
                { id: 'insert', h: 'Insert', cls: 'num', cell: function () { return String(c.insert); } },
                { id: 'update', h: 'Update', cls: 'num', cell: function () { return String(c.update); } },
                { id: 'skip', h: 'Skip', cls: 'num', cell: function () { return String(c.skip); } }
            ], [{}], { plain: true }) +
            M.notice('info', '<b>' + (c.insert + c.update) + ' rows would change</b> under the ' + S.mode +
                ' mode. A zero inserted count is the evidence that the catalogue agrees with the target.');
    }

    function stepReceipt() {
        var c = counts();
        return '<h2>What the run wrote</h2>' +
            '<p class="lede">The four counts the publish function returns.</p>' +
            '<dl class="plain">' +
            '<dt>Workflow instance</dt><dd class="mono">7f3c-4a91-9d20</dd>' +
            '<dt>Published by</dt><dd>' + M.person(S.person.id) + '</dd>' +
            '<dt>Mode</dt><dd>' + S.mode + '</dd>' +
            '</dl>' +
            M.table([
                { id: 'insert', h: 'Inserted', cls: 'num', cell: function () { return String(c.insert); } },
                { id: 'update', h: 'Updated', cls: 'num', cell: function () { return String(c.update); } },
                { id: 'skip', h: 'Skipped', cls: 'num', cell: function () { return String(c.skip); } },
                { id: 'deleted', h: 'Deleted', cls: 'num', cell: function () { return '0'; } }
            ], [{}], { plain: true }) +
            M.notice('warn', '<b>The receipt is the whole record.</b> No history row was written, because ' +
                'nothing on the publish path writes the run log.');
    }

    var BODIES = [stepDataset, stepMode, stepPreview, stepReceipt];

    function gaps() {
        if (document.body.classList.contains('nogaps')) { return ''; }
        return M.gap('No subject counts a dataset\'s staged or published rows, and none previews a run ' +
            'for one dataset. The preview that exists is per bundle, and it too is unserved.') +
            M.gap('The history is absent for the reason every publish journey gives: the run log is ' +
            'modelled and unwritten.');
    }

    function render() {
        document.getElementById('app').innerHTML =
            M.shell('Reference data', S.person) +
            '<main class="page">' +
            M.crumbs([
                { t: 'Home', href: '#' },
                { t: 'Reference data', href: '#' },
                { t: 'Reference data coverage', href: '../reference-data-coverage/index.html' },
                { t: 'Publish ' + DATASET.code }
            ]) +
            M.pagehead('Publish ' + DATASET.code,
                'One dataset · artefact type ' + DATASET.type + ' · the target is read, not chosen',
                M.refresh(false)) +
            '<div class="journey">' + M.rail(STEPS, S.step) +
            '<div class="card">' + BODIES[S.step]() +
            '<div class="stepfoot">' +
            M.btn('goto', 'Back', { cls: 'ghost', arg: S.step - 1, disabled: S.step === 0 }) +
            '<span class="ml-auto"></span>' +
            (S.step < 3 ? M.btn('goto', 'Continue', { cls: 'primary', arg: S.step + 1 })
                : M.btn('goto', 'Start again', { cls: 'primary', arg: 0 })) +
            '</div></div></div>' + gaps() + '</main>';

        document.querySelectorAll('[data-act="goto"]').forEach(function (b) {
            b.addEventListener('click', function () {
                go({ step: String(Number(b.getAttribute('data-arg')) + 1) });
            });
        });
        document.querySelectorAll('[data-mode]').forEach(function (d) {
            d.addEventListener('click', function () { go({ mode: d.getAttribute('data-mode'), step: '2' }); });
        });

        document.getElementById('proto-bar').innerHTML = M.bar(MODES.map(function (m) {
            return { v: m[0], t: m[1] };
        }), S.mode);
        var bar = document.getElementById('proto-bar');
        bar.querySelectorAll('[data-act="bar"]').forEach(function (btn) {
            btn.addEventListener('click', function () { go({ mode: btn.getAttribute('data-arg') }); });
        });
        bar.querySelector('[data-act="gaps"]').addEventListener('click', function () {
            var q = new URLSearchParams(window.location.search);
            if (document.body.classList.contains('nogaps')) { q.delete('gaps'); } else { q.set('gaps', '0'); }
            window.location.search = q.toString();
        });
    }

    render();
})();

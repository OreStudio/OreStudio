/* Publish a bundle.
 *
 * A journey of five steps on one rail: the bundle's contents in the order the
 * dependency closure gives them, the optional members and the mode, the preview
 * of what the run would write, the run, and the receipt. The target tenant is
 * read from the bundle's level and never chosen: a tenant that could choose
 * would recreate the second copy the model forbids. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    /* The demo clock, so every relative time reads against this page's data. */
    M.NOW = Date.parse('2026-11-22T12:00:00Z');
    M.LOADED = '2026-11-22T11:58:00+00:00';

    var STEP_NAMES = ['The bundle', 'The options', 'The preview', 'The run', 'The receipt'];

    var MEMBERS = [
        { order: 10, dataset: 'refdata.conventions', type: 'conventions', optional: false, staged: 10991, published: 10991 },
        { order: 20, dataset: 'refdata.currency_pair_conventions', type: 'currency_pair_conventions', optional: false, staged: 88, published: 84 },
        { order: 30, dataset: 'refdata.currency_pair_convention_calendars', type: 'currency_pair_convention_calendars', optional: true, staged: 34, published: 0 },
        { order: 40, dataset: 'lei.parties', type: 'parties', optional: true, staged: 1200, published: 0, needs: 'a root LEI' }
    ];

    var MODES = [
        ['upsert', 'Upsert', 'Insert what is missing and update what differs.'],
        ['insert_only', 'Insert only', 'Insert what is missing and leave the rows already there.'],
        ['replace_all', 'Replace all', 'Clear the target table and write the staged rows again.']
    ];

    var S = {
        step: Math.min(4, Math.max(0, Number(Q.get('step') || '1') - 1)),
        optedIn: Q.get('optional') === '1',
        mode: MODES.some(function (m) { return m[0] === Q.get('mode'); }) ? Q.get('mode') : 'upsert',
        fails: Q.get('fail') === '1',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function included(m) { return !m.optional || S.optedIn; }

    function counts(m) {
        if (!included(m)) { return { insert: 0, update: 0, skip: 0 }; }
        if (S.mode === 'replace_all') { return { insert: m.staged, update: 0, skip: 0 }; }
        return { insert: Math.max(0, m.staged - m.published), update: 0, skip: m.published };
    }

    function go(changes) {
        var q = new URLSearchParams(window.location.search);
        Object.keys(changes).forEach(function (k) {
            if (changes[k] === null) { q.delete(k); } else { q.set(k, changes[k]); }
        });
        window.location.search = q.toString();
    }

    /* ------------------------------------------------------------ */

    function stepBundle() {
        return '<h2>What this bundle carries</h2>' +
            '<p class="lede">Four members, published in the order the dependency closure gives them.</p>' +
            '<dl class="plain">' +
            '<dt>Bundle</dt><dd class="mono">party_essentials</dd>' +
            '<dt>Level</dt><dd>Tenant</dd>' +
            '<dt>Writes into</dt><dd>the tenant that asked — the level decides it, the caller does not</dd>' +
            '<dt>Members</dt><dd>4 · 2 always, 2 optional</dd>' +
            '</dl>' +
            '<h3>Its members</h3>' +
            M.table([
                { id: 'order', h: 'Order', cls: 'num', cell: function (m) { return String(m.order); } },
                { id: 'dataset', h: 'Dataset', cell: function (m) {
                    return '<span class="key">' + M.esc(m.dataset) + '</span><span class="sub">' + M.esc(m.type) + '</span>'; } },
                { id: 'included', h: 'Included', cell: function (m) {
                    return '<span class="dim">' + (m.optional ? 'optional' : 'always') + '</span>'; } },
                { id: 'staged', h: 'Staged', cls: 'num', cell: function (m) { return String(m.staged); } },
                { id: 'published', h: 'Published', cls: 'num', cell: function (m) { return String(m.published); } }
            ], MEMBERS, { plain: true }) +
            '<p class="hint">The order is the closure\'s. The bundle\'s display order only breaks ties, so a ' +
            'member another member needs is published first whatever the list says.</p>';
    }

    function stepOptions() {
        var modes = MODES.map(function (m) {
            return '<div class="choice' + (S.mode === m[0] ? ' on' : '') + '" role="radio" aria-checked="' +
                (S.mode === m[0]) + '" tabindex="0" data-mode="' + M.esc(m[0]) + '">' +
                '<span class="dot"></span><span><span class="nm">' + M.esc(m[1]) + '</span><br>' +
                '<span class="sub">' + M.esc(m[2]) + '</span></span></div>';
        }).join('');

        var opts = MEMBERS.filter(function (m) { return m.optional; }).map(function (m) {
            return '<div class="choice' + (S.optedIn ? ' on' : '') + '" role="checkbox" aria-checked="' +
                S.optedIn + '" tabindex="0" data-opt="1"><span class="dot"></span><span>' +
                '<span class="nm mono">' + M.esc(m.dataset) + '</span><br><span class="sub">' +
                (m.needs
                    ? 'publishes nothing by default · needs ' + M.esc(m.needs)
                    : 'publishes nothing by default') + '</span></span></div>';
        }).join('');

        return '<h2>How the run behaves</h2>' +
            '<p class="lede">The mode decides what happens to a row that is already there.</p>' +
            '<h3>Mode</h3><div role="radiogroup" aria-label="Mode">' + modes + '</div>' +
            '<h3>Optional members</h3><div role="group" aria-label="Optional members">' + opts + '</div>' +
            '<p class="hint">An optional member publishes only when it is asked for, so a large catalogue ' +
            'does not arrive with every small one.</p>' +
            M.notice('info', 'The target is read from the bundle\'s level: <b>Tenant</b> writes into the ' +
                'tenant that asked, and a platform or world bundle is published only by the system tenant, ' +
                'because a tenant reads those rows where they lie.');
    }

    function stepPreview() {
        var total = 0;
        MEMBERS.forEach(function (m) { total += counts(m).insert; });
        return '<h2>What the run would write</h2>' +
            '<p class="lede">Read before the run, not after it. A member that cannot publish is listed ' +
            'with zero counts rather than left out.</p>' +
            M.table([
                { id: 'dataset', h: 'Dataset', cell: function (m) {
                    return '<span class="key">' + M.esc(m.dataset) + '</span>'; } },
                { id: 'staged', h: 'Staged', cls: 'num', cell: function (m) { return String(m.staged); } },
                { id: 'insert', h: 'Insert', cls: 'num', cell: function (m) { return String(counts(m).insert); } },
                { id: 'update', h: 'Update', cls: 'num', cell: function (m) { return String(counts(m).update); } },
                { id: 'skip', h: 'Skip', cls: 'num', cell: function (m) { return String(counts(m).skip); } },
                { id: 'run', h: 'This run', cell: function (m) {
                    return '<span class="' + (included(m) ? '' : 'dim') + '">' +
                        (included(m) ? 'included' : 'not included') + '</span>'; } }
            ], MEMBERS, { plain: true }) +
            M.notice('info', '<b>' + total + ' rows would be inserted</b> under the ' + S.mode +
                ' mode. A zero inserted count is the evidence that a catalogue agrees; a zero updated ' +
                'count is not, because a function that rewrites its rows returns updates instead.');
    }

    function stepRun() {
        var haltAt = S.fails ? 2 : -1;
        var items = MEMBERS.filter(included).map(function (m, i) {
            var halted = i === haltAt;
            var done = haltAt === -1 || i < haltAt;
            var badge = halted ? M.badge('bad', 'halted') : done ? M.badge('ok', 'done') : M.badge('pending', 'queued');
            var note = halted
                ? '<p class="note">The publish function raised: 4 staged rows name no artefact type.</p>'
                : done ? '<p class="note">Four counts returned.</p>' : '';
            return '<li><span class="when">step ' + (i + 1) + '</span><div>' +
                '<div class="head"><span class="key">' + M.esc(m.dataset) + '</span>' + badge + '</div>' +
                note + '</div></li>';
        }).join('');

        return '<h2>The run</h2>' +
            '<p class="lede">Each step is its own function call, so a failure halts the run and leaves the ' +
            'steps before it in place.</p>' +
            '<dl class="plain">' +
            '<dt>Workflow instance</dt><dd class="mono">7f3c-4a91-9d20</dd>' +
            '<dt>Started by</dt><dd>' + M.person(S.person.id) + '</dd>' +
            '<dt>Mode</dt><dd>' + S.mode + '</dd>' +
            '</dl>' +
            '<ul class="timeline">' + items + '</ul>' +
            (S.fails
                ? M.notice('error', 'The run halted at step 3. The two members before it stay published; ' +
                    'there is no compensation step, because the functions are idempotent upserts and a ' +
                    're-run is safe.')
                : M.notice('success', 'The run completed. Its counts are on the next step.'));
    }

    function stepReceipt() {
        return '<h2>What the run wrote</h2>' +
            '<p class="lede">The four counts each publish function returns, member by member.</p>' +
            M.table([
                { id: 'dataset', h: 'Dataset', cell: function (m) {
                    return '<span class="key">' + M.esc(m.dataset) + '</span>'; } },
                { id: 'insert', h: 'Inserted', cls: 'num', cell: function (m) { return String(counts(m).insert); } },
                { id: 'update', h: 'Updated', cls: 'num', cell: function (m) { return String(counts(m).update); } },
                { id: 'skip', h: 'Skipped', cls: 'num', cell: function (m) { return String(counts(m).skip); } },
                { id: 'deleted', h: 'Deleted', cls: 'num', cell: function () { return '0'; } }
            ], MEMBERS.filter(included), { plain: true }) +
            M.notice('warn', '<b>The receipt is the whole record.</b> No history row was written, because ' +
                'nothing on the publish path writes the run log. What survives the run is the workflow ' +
                'instance and the provenance stamped on each published row.');
    }

    var BODIES = [stepBundle, stepOptions, stepPreview, stepRun, stepReceipt];

    function gaps() {
        if (document.body.classList.contains('nogaps')) { return ''; }
        return M.gap('The preview counts are computed by <b>ores_dq_bundle_preview_fn</b>, and no subject ' +
            'serves it. The receipt is the four counts the publish functions return, read from the ' +
            'workflow step result.') +
            M.gap('Nothing writes the run log, so the history this journey would show is empty: the table ' +
            'is modelled and unwritten.');
    }

    function foot() {
        return '<div class="stepfoot">' +
            M.btn('goto', 'Back', { cls: 'ghost', arg: S.step - 1, disabled: S.step === 0 }) +
            '<span class="ml-auto"></span>' +
            (S.step < 4
                ? M.btn('goto', S.step === 3 ? 'Read the receipt' : 'Continue', { cls: 'primary', arg: S.step + 1 })
                : M.btn('goto', 'Start again', { cls: 'primary', arg: 0 })) +
            '</div>';
    }

    function render() {
        var html = M.shell('Reference data', S.person) +
            '<main class="page">' +
            M.crumbs([
                { t: 'Home', href: '#' },
                { t: 'Reference data', href: '#' },
                { t: 'Reference data coverage', href: '../reference-data-coverage/index.html' },
                { t: 'Publish party_essentials' }
            ]) +
            M.pagehead('Publish party_essentials',
                'Tenant level · writes into the tenant that asked · 4 members',
                M.refresh(false)) +
            '<div class="journey">' +
            M.rail(STEP_NAMES, S.step) +
            '<div class="card">' + BODIES[S.step]() + foot() + '</div>' +
            '</div>' + gaps() + '</main>';

        document.getElementById('app').innerHTML = html;

        document.querySelectorAll('[data-act="goto"]').forEach(function (b) {
            b.addEventListener('click', function () {
                go({ step: String(Number(b.getAttribute('data-arg')) + 1) });
            });
        });
        document.querySelectorAll('[data-mode]').forEach(function (d) {
            d.addEventListener('click', function () { go({ mode: d.getAttribute('data-mode'), step: '2' }); });
        });
        document.querySelectorAll('[data-opt]').forEach(function (d) {
            d.addEventListener('click', function () { go({ optional: S.optedIn ? '0' : '1', step: '2' }); });
        });

        document.getElementById('proto-bar').innerHTML = M.bar(MODES.map(function (m) {
            return { v: m[0], t: m[1] };
        }), S.mode);
        wireBar();
    }

    function wireBar() {
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

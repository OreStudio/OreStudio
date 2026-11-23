/* Reference data coverage.
 *
 * One list screen: the bundles the installation knows, grouped by the level
 * they publish at, and the members of the chosen one underneath. The level
 * decides the tenant the rows land in, so it is the grouping and not a column.
 * A member that cannot publish states which of four reasons applies, because
 * each reason has a different person to fix it. */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    /* The demo clock, so every relative time reads against this page's data. */
    M.NOW = Date.parse('2026-11-22T12:00:00Z');
    M.LOADED = '2026-11-22T11:58:00+00:00';

    var PERSON = M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor'];

    var LEVELS = [
        ['Platform', 'One copy, in the system tenant, published at bring-up.'],
        ['World', 'One copy, in the system tenant, published at bring-up.'],
        ['Tenant', 'Written into the tenant that asked, when it asks.']
    ];

    var BUNDLES = [
        { level: 'Platform', code: 'system_core', name: 'System core', datasets: 12, checked: '2026-11-22T09:40:00Z', members: [] },
        { level: 'World', code: 'world', name: 'World', datasets: 41, checked: '2026-11-22T09:40:00Z', members: [
            { dataset: 'refdata.countries', type: 'countries', subject: 'refdata.v1.ops.publish_countries_from_dq', staged: 249, published: 249 },
            { dataset: 'refdata.currencies', type: 'currencies', subject: 'refdata.v1.ops.publish_currencies_from_dq', staged: 179, published: 179 },
            { dataset: 'refdata.calendars', type: 'calendars', subject: 'refdata.v1.ops.publish_calendars_from_dq', staged: 35, published: 35, optional: true }
        ] },
        { level: 'World', code: 'crypto', name: 'Crypto', datasets: 9, checked: null, members: [] },
        { level: 'Tenant', code: 'tenant_defaults', name: 'Tenant defaults', datasets: 3, checked: '2026-11-22T10:02:00Z', members: [] },
        { level: 'Tenant', code: 'party_essentials', name: 'Party essentials', datasets: 2, checked: '2026-11-22T10:14:00Z', members: [
            { dataset: 'refdata.conventions', type: 'conventions', subject: 'refdata.v1.ops.publish_conventions_from_dq', staged: 10991, published: 10991 },
            { dataset: 'refdata.currency_pair_convention_calendars', type: 'currency_pair_convention_calendars', subject: 'refdata.v1.ops.publish_currency_pair_convention_calendars_from_dq', staged: 0, published: 0, reason: 'no staged rows' }
        ] },
        { level: 'Tenant', code: 'risk_management', name: 'Risk management', datasets: 4, checked: '2026-11-22T10:14:00Z', members: [
            { dataset: 'testdata.business_units', type: 'business_units', subject: 'refdata.v1.ops.publish_business_units_from_dq', staged: 12, published: 12 },
            { dataset: 'testdata.portfolios', type: 'portfolios', subject: 'refdata.v1.ops.publish_portfolios_from_dq', staged: 4, published: 0 },
            { dataset: 'testdata.books', type: 'books', subject: null, staged: 88, published: 0, reason: 'no target subject' },
            { dataset: 'ore.report_definitions', type: 'report_definitions', subject: 'reporting.v1.ops.publish_report_definitions_from_dq', staged: 28, published: 28 }
        ] }
    ];

    var REASONS = [
        ['no dataset', 'The bundle lists a member that no dataset row names.', 'DQ'],
        ['no artefact type', 'The dataset names an artefact type that does not exist.', 'DQ'],
        ['no target subject', 'The artefact type names no target subject, so nothing can be dispatched.', 'DQ'],
        ['no staged rows', 'The artefact table holds nothing, so there is nothing to publish.', 'the importer']
    ];

    var EMPTY = Q.get('empty') === '1';
    var SELECTED = Q.get('bundle') || (EMPTY ? 'world' : 'party_essentials');
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function bundleState(b) {
        if (EMPTY) { return 'none'; }
        if (b.members.some(function (m) { return m.reason; })) { return 'bad'; }
        if (b.members.length === 0) { return b.checked ? 'ok' : 'none'; }
        var ready = b.members.filter(function (m) { return !m.reason; }).length;
        return ready === b.members.length ? 'ok' : 'bad';
    }

    var STATE = {
        ok:   { kind: 'ok',       text: 'Published' },
        part: { kind: 'caution',  text: 'Part published' },
        none: { kind: 'inactive', text: 'Not published' },
        bad:  { kind: 'bad',      text: 'Not ready' }
    };

    function published(m) { return EMPTY ? 0 : m.published; }

    function memberBadge(m) {
        if (m.reason) { return M.badge('bad', m.reason); }
        var p = published(m);
        if (p === 0 && m.staged > 0) { return M.badge('caution', 'nothing published'); }
        if (p < m.staged) { return M.badge('caution', (m.staged - p) + ' skipped'); }
        return M.badge('ok', 'published');
    }

    function bundleRows() {
        var out = [];
        LEVELS.forEach(function (lvl) {
            var inLevel = BUNDLES.filter(function (b) { return b.level === lvl[0]; });
            out.push({ group: lvl[0], note: lvl[1], count: inLevel.length });
            inLevel.forEach(function (b) {
                out.push({ bundle: b, state: bundleState(b) });
            });
        });
        return out;
    }

    function bundlesTable() {
        var html = '<table class="rows grouped"><thead><tr>' +
            '<th>Bundle</th><th>Datasets</th><th>Checked</th><th>State</th>' +
            '</tr></thead><tbody>';
        bundleRows().forEach(function (r) {
            if (r.group) {
                html += '<tr class="grp"><th colspan="4">' + M.esc(r.group) +
                    ' <span class="dim">· ' + M.esc(r.note) + '</span></th></tr>';
                return;
            }
            var b = r.bundle;
            var st = STATE[r.state];
            html += '<tr class="pick' + (b.code === SELECTED ? ' on' : '') + '" tabindex="0" data-bundle="' + M.esc(b.code) + '">' +
                '<td><span class="key">' + M.esc(b.code) + '</span><span class="sub">' + M.esc(b.name) + '</span></td>' +
                '<td class="num">' + b.datasets + '</td>' +
                '<td>' + (b.checked ? M.relTime(b.checked) : M.dash('')) + '</td>' +
                '<td>' + M.badge(st.kind, st.text) + '</td></tr>';
        });
        return html + '</tbody></table>';
    }

    function membersTable(b) {
        if (!b) { return '<p class="tblstate">No bundle is chosen.</p>'; }
        if (b.members.length === 0) {
            return '<p class="tblstate">The mock members of ' + M.esc(b.code) +
                ' are not drawn. Its count is still stated above.</p>';
        }
        var cols = [
            { id: 'dataset', h: 'Dataset', cell: function (m) {
                return '<span class="key">' + M.esc(m.dataset) + '</span><span class="sub">' + M.esc(m.type) + '</span>'; } },
            { id: 'subject', h: 'Target subject', cell: function (m) { return M.dash(m.subject); } },
            { id: 'staged', h: 'Staged', cls: 'num', cell: function (m) { return String(m.staged); } },
            { id: 'published', h: 'Published', cls: 'num', cell: function (m) { return String(published(m)); } },
            { id: 'state', h: 'State', cell: function (m) { return memberBadge(m); } }
        ];
        return M.table(cols, b.members, { plain: true });
    }

    function reasonsTable() {
        return '<table class="rows"><thead><tr><th>Reason</th><th>What it means</th><th>Whose to fix</th></tr></thead><tbody>' +
            REASONS.map(function (r) {
                return '<tr><td class="key">' + M.esc(r[0]) + '</td><td>' + M.esc(r[1]) + '</td><td>' + M.esc(r[2]) + '</td></tr>';
            }).join('') + '</tbody></table>';
    }

    function gaps() {
        if (document.body.classList.contains('nogaps')) { return ''; }
        return M.gap('The coverage is computed by <b>ores_dq_bundles_list_fn</b>, ' +
            '<b>ores_dq_bundle_datasets_list_fn</b> and <b>ores_dq_bundle_preview_fn</b>. ' +
            'The publish path already calls two of them; no subject serves any of them.') +
            M.gap('The published count has no operation. Every published row names its dataset and no ' +
            'service counts by it, so the column here is mock data.');
    }

    function render() {
        var b = BUNDLES.filter(function (x) { return x.code === SELECTED; })[0];
        var areas = M.shell('Reference data', PERSON);
        var crumbs = M.crumbs([
            { t: 'Home', href: '#' },
            { t: 'Reference data', href: '#' },
            { t: 'Reference data coverage' }
        ]);
        var head = M.pagehead('Reference data coverage',
            'What this installation holds, by level, and why a member of a bundle is not ready.',
            M.refresh(false) + M.workflowMenu([
                { t: 'Publish a bundle', d: 'Read the run before it happens.', href: '../publish-a-bundle/index.html' },
                { t: 'Publish datasets', d: 'One member instead of a whole bundle.', href: '../publish-a-bundle/index.html?step=1' }
            ]));

        var notice = EMPTY
            ? M.notice('warn', '<b>A fresh database looks like this.</b> Nothing has been published into ' +
                'the installation yet, and a recreate that fails to publish leaves exactly this screen. ' +
                'The Qt client could not show it.')
            : '';

        var html = areas +
            '<main class="page">' + crumbs + head + notice +
            '<div class="card cardgap"><div class="cardhead"><h2>Bundles</h2>' +
            '<span class="dim">One copy of each shared row, where it lies</span></div>' +
            bundlesTable() + '</div>' +
            '<div class="card cardgap"><div class="cardhead"><h2>' +
            (b ? M.esc(b.name) + ' <span class="dim">· ' + M.esc(b.code) + '</span>' : 'Members') +
            '</h2><span class="dim">' + (b ? M.esc(b.level) + ' level' : '') + '</span></div>' +
            membersTable(b) +
            '<p class="hint">Staged counts the artefact table. Published counts the rows whose provenance ' +
            'names this dataset, which is the only record a run leaves.</p></div>' +
            '<div class="card cardgap"><div class="cardhead"><h2>Why a member is not ready</h2>' +
            '<span class="dim">One reason each, because each has a different owner</span></div>' +
            reasonsTable() + '</div>' +
            gaps() +
            '</main>';

        document.getElementById('app').innerHTML = html;

        document.querySelectorAll('tr[data-bundle]').forEach(function (tr) {
            tr.addEventListener('click', function () { pick(tr.getAttribute('data-bundle')); });
            tr.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' || e.key === ' ') { pick(tr.getAttribute('data-bundle')); }
            });
        });

        document.getElementById('proto-bar').innerHTML = M.bar([
            { v: 'tenant', t: 'This tenant' },
            { v: 'fresh', t: 'Fresh database' }
        ], EMPTY ? 'fresh' : 'tenant');
        wire();
    }

    function pick(code) {
        var q = new URLSearchParams(window.location.search);
        q.set('bundle', code);
        window.location.search = q.toString();
    }

    function wire() {
        var bar = document.getElementById('proto-bar');
        bar.querySelectorAll('[data-act="bar"]').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var q = new URLSearchParams(window.location.search);
                if (btn.getAttribute('data-arg') === 'fresh') { q.set('empty', '1'); }
                else { q.delete('empty'); }
                window.location.search = q.toString();
            });
        });
        bar.querySelector('[data-act="gaps"]').addEventListener('click', function () {
            var q = new URLSearchParams(window.location.search);
            if (document.body.classList.contains('nogaps')) { q.delete('gaps'); }
            else { q.set('gaps', '0'); }
            window.location.search = q.toString();
        });
    }

    render();
})();

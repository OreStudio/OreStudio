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
            { dataset: 'refdata.countries', name: 'Countries', type: 'countries', subject: 'refdata.v1.ops.publish_countries_from_dq', staged: 249, published: 249 },
            { dataset: 'refdata.currencies', name: 'Currencies', type: 'currencies', subject: 'refdata.v1.ops.publish_currencies_from_dq', staged: 179, published: 179 },
            { dataset: 'refdata.calendars', name: 'Calendars', type: 'calendars', subject: 'refdata.v1.ops.publish_calendars_from_dq', staged: 35, published: 35, optional: true }
        ] },
        { level: 'World', code: 'crypto', name: 'Crypto', datasets: 9, checked: null, members: [] },
        { level: 'Tenant', code: 'tenant_defaults', name: 'Tenant defaults', datasets: 3, checked: '2026-11-22T10:02:00Z', members: [] },
        { level: 'Tenant', code: 'party_essentials', name: 'Party essentials', datasets: 2, checked: '2026-11-22T10:14:00Z', members: [
            { dataset: 'refdata.conventions', name: 'Conventions', type: 'conventions', subject: 'refdata.v1.ops.publish_conventions_from_dq', staged: 10991, published: 10991 },
            { dataset: 'refdata.currency_pair_convention_calendars', name: 'Currency pair convention calendars', type: 'currency_pair_convention_calendars', subject: 'refdata.v1.ops.publish_currency_pair_convention_calendars_from_dq', staged: 0, published: 0, reason: 'no staged rows' }
        ] },
        { level: 'Tenant', code: 'risk_management', name: 'Risk management', datasets: 4, checked: '2026-11-22T10:14:00Z', members: [
            { dataset: 'testdata.business_units', name: 'Business units', type: 'business_units', subject: 'refdata.v1.ops.publish_business_units_from_dq', staged: 12, published: 12 },
            { dataset: 'testdata.portfolios', name: 'Portfolios', type: 'portfolios', subject: 'refdata.v1.ops.publish_portfolios_from_dq', staged: 4, published: 0 },
            { dataset: 'testdata.books', name: 'Books', type: 'books', subject: null, staged: 88, published: 0, reason: 'no target subject' },
            { dataset: 'ore.report_definitions', name: 'Report definitions', type: 'report_definitions', subject: 'reporting.v1.ops.publish_report_definitions_from_dq', staged: 28, published: 28 }
        ] }
    ];

    /* The publication log, one list per dataset. The run log has no writer and
     * no outcome column today, so every row here is mock data; the prototype
     * marks it on the page. A row says what a run wrote and what it said. */
    var PUBLICATIONS = {
        'refdata.conventions': [
            { at: '2026-11-22T10:14:00Z', by: 'm.okafor', mode: 'upsert', target: 'refdata_conventions_tbl',
              ins: 10991, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] },
            { at: '2026-11-14T16:02:00Z', by: 'm.okafor', mode: 'upsert', target: 'refdata_conventions_tbl',
              ins: 10960, upd: 31, skip: 0, del: 0, outcome: 'ok', said: [
                { sev: 'warn', text: '31 rows replaced: their business day convention had changed.' }] }
        ],
        'refdata.currency_pair_convention_calendars': [
            { at: '2026-11-14T16:04:00Z', by: 'm.okafor', mode: 'upsert',
              target: 'refdata_currency_pair_convention_calendars_tbl',
              ins: 0, upd: 0, skip: 0, del: 0, outcome: 'failed', said: [
                { sev: 'error', text: 'The artefact table holds no row for this dataset, so there was nothing to publish.' }] }
        ],
        'refdata.countries': [
            { at: '2026-11-22T09:40:00Z', by: 'system', mode: 'upsert', target: 'refdata_countries_tbl',
              ins: 249, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] }
        ],
        'refdata.currencies': [
            { at: '2026-11-22T09:40:00Z', by: 'system', mode: 'upsert', target: 'refdata_currencies_tbl',
              ins: 179, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] }
        ],
        'refdata.calendars': [
            { at: '2026-11-22T09:40:00Z', by: 'system', mode: 'upsert', target: 'refdata_calendars_tbl',
              ins: 35, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] }
        ],
        'testdata.business_units': [
            { at: '2026-11-22T10:14:00Z', by: 'm.okafor', mode: 'upsert', target: 'refdata_business_units_tbl',
              ins: 12, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] }
        ],
        'testdata.portfolios': [
            { at: '2026-11-22T10:14:00Z', by: 'm.okafor', mode: 'upsert', target: 'refdata_portfolios_tbl',
              ins: 0, upd: 0, skip: 4, del: 0, outcome: 'ok', said: [
                { sev: 'warn', text: '4 rows were skipped: the report containing them had not run yet.' }] }
        ],
        'testdata.books': [
            { at: '2026-11-22T10:14:00Z', by: 'm.okafor', mode: 'upsert', target: 'refdata_books_tbl',
              ins: 0, upd: 0, skip: 0, del: 0, outcome: 'failed', said: [
                { sev: 'error', text: 'The artefact type names no target subject, so the publish could not be dispatched.' }] }
        ],
        'ore.report_definitions': [
            { at: '2026-11-22T10:14:00Z', by: 'm.okafor', mode: 'upsert', target: 'reporting_report_definitions_tbl',
              ins: 28, upd: 0, skip: 0, del: 0, outcome: 'ok', said: [] }
        ]
    };

    var REASONS = [
        ['no dataset', 'The bundle lists a member that no dataset row names.', 'DQ'],
        ['no artefact type', 'The dataset names an artefact type that does not exist.', 'DQ'],
        ['no target subject', 'The artefact type names no target subject, so nothing can be dispatched.', 'DQ'],
        ['no staged rows', 'The artefact table holds nothing, so there is nothing to publish.', 'the importer']
    ];

    var EMPTY = Q.get('empty') === '1';
    var SELECTED = Q.get('bundle') || (EMPTY ? 'world' : 'party_essentials');
    /* The librarian layout draws the same model as a tree, a table and a card,
     * because the Qt Data Librarian was the screen this area replaces. */
    var LIBRARIAN = Q.get('layout') === 'librarian';
    var MEMBER = Q.get('member') || '';
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

    /* -------------------------------------------------------------- */
    /* The librarian layout                                             */
    /* -------------------------------------------------------------- */

    function tree() {
        var html = '<div class="lib-tree">';
        LEVELS.forEach(function (lvl) {
            var inLevel = BUNDLES.filter(function (b) { return b.level === lvl[0]; });
            html += '<div class="grp">' + M.esc(lvl[0]) +
                '<span class="note">' + M.esc(lvl[1]) + '</span></div>';
            inLevel.forEach(function (b) {
                html += '<button type="button" class="node' +
                    (b.code === SELECTED ? ' on' : '') + '" data-bundle="' +
                    M.esc(b.code) + '" aria-pressed="' + (b.code === SELECTED) + '">' +
                    '<span class="key">' + M.esc(b.code) + '</span>' +
                    '<span class="count">' + b.datasets + '</span>' +
                    '<span class="sub">' + M.esc(b.name) + '</span></button>';
            });
        });
        return html + '</div>';
    }

    function libColumns() {
        return [
            { id: 'dataset', h: 'Dataset', cell: function (m) {
                return '<span class="key">' + M.esc(m.dataset) + '</span><span class="sub">' +
                    M.esc(m.type) + '</span>'; } },
            { id: 'staged', h: 'Staged', cls: 'num', cell: function (m) { return String(m.staged); } },
            { id: 'published', h: 'Published', cls: 'num', cell: function (m) { return String(published(m)); } },
            { id: 'state', h: 'State', cell: function (m) { return memberBadge(m); } }
        ];
    }

    /** The members of the chosen bundle, under the text and state filters. */
    function shownMembers(b) {
        var text = (Q.get('q') || '').toLowerCase();
        var state = Q.get('state') || 'all';
        if (!b) { return []; }
        return b.members.filter(function (m) {
            if (text && (m.dataset + ' ' + m.type).toLowerCase().indexOf(text) < 0) { return false; }
            if (state === 'all') { return true; }
            if (state === 'bad') { return !!m.reason; }
            if (state === 'unpublished') { return !m.reason && published(m) < m.staged; }
            if (state === 'published') { return !m.reason && published(m) >= m.staged; }
            return true;
        });
    }

    function membersPane(b) {
        var rows = shownMembers(b);
        var tools = '<div class="lib-tools">' +
            '<input type="search" id="q" placeholder="Filter by dataset or artefact type" value="' +
            M.esc(Q.get('q') || '') + '" aria-label="Filter the datasets">' +
            '<select id="state" aria-label="Filter by state">' +
            [['all', 'Every state'], ['bad', 'Not ready'], ['unpublished', 'Not fully published'],
             ['published', 'Published']].map(function (o) {
                return '<option value="' + o[0] + '"' +
                    ((Q.get('state') || 'all') === o[0] ? ' selected' : '') + '>' + o[1] + '</option>';
            }).join('') + '</select></div>';

        var body;
        if (!b) {
            body = '<p class="lib-empty">No bundle is chosen.</p>';
        } else if (b.members.length === 0) {
            body = '<p class="lib-empty">The mock members of ' + M.esc(b.code) +
                ' are not drawn. Its count of ' + b.datasets + ' is still stated in the tree.</p>';
        } else if (rows.length === 0) {
            body = '<p class="lib-empty">No member matches this filter.</p>';
        } else {
            body = '<div class="lib-table">' + M.table(libColumns(), rows, {
                plain: true,
                isOn: function (m) { return m.dataset === MEMBER; },
                rowClass: function (m) { return 'pick'; }
            }) + '</div>';
        }
        return tools + body;
    }

    /* -------------------------------------------------------------- */
    /* The dataset's own screen                                         */
    /* -------------------------------------------------------------- */

    /** The bundle that carries a dataset, and the member row itself. */
    function memberOf(code) {
        var found = null;
        BUNDLES.forEach(function (b) {
            b.members.forEach(function (m) {
                if (m.dataset === code) { found = { bundle: b, member: m }; }
            });
        });
        return found;
    }

    function outcomeBadge(p) {
        if (p.outcome === 'failed') { return M.badge('bad', 'failed'); }
        if (p.said.some(function (s) { return s.sev === 'warn'; })) {
            return M.badge('caution', 'published with warnings');
        }
        if (p.skip > 0) { return M.badge('caution', 'published, ' + p.skip + ' skipped'); }
        return M.badge('ok', 'published');
    }


    /* The tables a dataset stages into. An artefact type names a table or a family
     * of them, and every family carries its own columns beside the two all of them
     * share, dataset_id and tenant_id. So the tab lists the tables first, and a
     * table's rows are drawn when one is opened. */
    var CONTENTS = {
        'refdata.countries': [
            {
                table: 'dq_countries_artefact_tbl',
                rows: [
                    ['GB', 'GBR', '826', 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland'],
                    ['US', 'USA', '840', 'United States', 'United States of America'],
                    ['JP', 'JPN', '392', 'Japan', 'Japan'],
                    ['CH', 'CHE', '756', 'Switzerland', 'Swiss Confederation']
                ],
                columns: ['alpha2_code', 'alpha3_code', 'numeric_code', 'name', 'official_name'],
                staged: 249
            }
        ],
        'refdata.currencies': [
            {
                table: 'dq_currencies_artefact_tbl',
                columns: ['iso_code', 'numeric_code', 'name', 'minor_unit', 'is_spot'],
                rows: [
                    ['GBP', '826', 'Pound Sterling', '2', 'yes'],
                    ['USD', '840', 'US Dollar', '2', 'yes'],
                    ['JPY', '392', 'Yen', '0', 'yes'],
                    ['XAU', '959', 'Gold', '0', 'no']
                ],
                staged: 160
            },
            {
                table: 'dq_currency_aliases_artefact_tbl',
                columns: ['iso_code', 'alias', 'locale'],
                rows: [
                    ['GBP', 'sterling', 'en-GB'],
                    ['USD', 'greenback', 'en-US']
                ],
                staged: 12
            }
        ],
        'refdata.calendars': [
            {
                table: 'dq_calendars_artefact_tbl',
                columns: ['calendar_code', 'name', 'business_day_convention', 'holidays'],
                rows: [
                    ['GBLO', 'London', 'Modified Following', '8'],
                    ['USNY', 'New York', 'Following', '10'],
                    ['JPTO', 'Tokyo', 'Following', '16']
                ],
                staged: 35
            }
        ]
    };

    /* The dependency edges a dataset states, in both directions. The store holds
     * one row per edge: dataset_code depends on dependency_code for a stated role.
     * Upstream is therefore "needs" and downstream is "needed by", and both come
     * from the same 196 rows. */
    var LINEAGE = {
        'refdata.conventions': {
            needs: [
                { code: 'iso.currencies', name: 'ISO 4217 Currency Codes',
                  role: 'currency_reference' }
            ],
            neededBy: [
                { code: 'acme.acme_group.pricing_engines', name: 'Pricing Engines',
                  role: 'convention_source' },
                { code: 'acme.acme_group.todays_market', name: "Today's Market",
                  role: 'convention_source' },
                { code: 'acme.acme_group.curve_configuration', name: 'Curve Configuration',
                  role: 'convention_source' },
                { code: 'acme.acme_group.report_definitions', name: 'Report Definitions',
                  role: 'configuration_source' },
                { code: 'acme.acme_uk.pricing_engines', name: 'Pricing Engines',
                  role: 'convention_source' },
                { code: 'acme.acme_hk.engine_settings', name: 'Engine Settings',
                  role: 'convention_source' }
            ],
            more: 26
        },
        'refdata.currencies': {
            needs: [
                { code: 'iso.coding_schemes', name: 'ISO Coding Schemes',
                  role: 'coding_scheme' },
                { code: 'assets.country_flags', name: 'Country Flags',
                  role: 'visual_assets' }
            ],
            neededBy: [
                { code: 'refdata.conventions', name: 'Conventions',
                  role: 'currency_reference' }
            ],
            more: 0
        },
        'refdata.calendars': {
            needs: [
                { code: 'iso.countries', name: 'ISO 3166 Countries',
                  role: 'country_reference' },
                { code: 'refdata.calendar_types', name: 'Calendar Types',
                  role: 'calendar_type_reference' }
            ],
            neededBy: [],
            more: 0
        },
        'refdata.countries': {
            needs: [],
            neededBy: [
                { code: 'refdata.calendars', name: 'Calendars',
                  role: 'country_reference' }
            ],
            more: 0
        }
    };

    /** The datasets one dataset needs, drawn as the small graph the Qt client drew. */
    function lineageNeeds(code) {
        var held = LINEAGE[code];
        if (!held || held.needs.length === 0) {
            return '<p class="tblstate">This dataset states no dependency. Nothing has to ' +
                'publish before it does.</p>';
        }
        var here = memberOf(code);
        return '<div class="lineage-graph"><svg class="lineage-wires"></svg>' +
            '<div class="lineage-col" data-side="needs">' +
            held.needs.map(function (n) {
                return '<button class="lineage-node" data-node="needs" data-role="' +
                    M.esc(n.role) + '" data-dataset="' + M.esc(n.code) + '">' +
                    '<span class="key">' + M.esc(n.code) + '</span>' +
                    '<span class="sub">' + M.esc(n.name) + '</span></button>';
            }).join('') + '</div><div class="lineage-col" data-side="here">' +
            '<div class="lineage-node here" data-node="here">' +
            '<span class="key">' + M.esc(code) + '</span><span class="sub">' +
            M.esc(here ? here.member.name : code) + '</span></div></div></div>';
    }

    /**
     * Draws one wire from each need to the dataset, with the role on it.
     *
     * The nodes are laid out by the browser, so the wires are drawn from where
     * they land rather than from a model of the layout: the graph is small, and
     * a wire that misses its node is worse than no wire.
     */
    function drawWires() {
        document.querySelectorAll('.lineage-graph').forEach(function (graph) {
            var svg = graph.querySelector('.lineage-wires');
            var here = graph.querySelector('[data-node="here"]');
            if (!svg || !here) return;
            var box = graph.getBoundingClientRect();
            var target = here.getBoundingClientRect();
            var targetX = target.left - box.left;
            var targetY = target.top - box.top + target.height / 2;
            svg.setAttribute('viewBox', '0 0 ' + box.width + ' ' + box.height);
            svg.setAttribute('width', box.width);
            svg.setAttribute('height', box.height);
            var drawn = '';
            graph.querySelectorAll('[data-node="needs"]').forEach(function (node) {
                var n = node.getBoundingClientRect();
                var x1 = n.right - box.left;
                var y1 = n.top - box.top + n.height / 2;
                var mid = (x1 + targetX) / 2;
                drawn += '<path d="M' + x1 + ' ' + y1 + ' C' + mid + ' ' + y1 + ' ' + mid + ' ' +
                    targetY + ' ' + targetX + ' ' + targetY + '" fill="none" ' +
                    'stroke="currentColor" stroke-width="1"/>' +
                    '<text x="' + mid + '" y="' + ((y1 + targetY) / 2 - 4) + '" text-anchor="middle" ' +
                    'class="lineage-role">' + M.esc(node.getAttribute('data-role')) + '</text>';
            });
            svg.innerHTML = drawn;
        });
    }

    /** The datasets that need this one, one row each, with the role it plays for them. */
    function lineageNeededBy(code) {
        var held = LINEAGE[code];
        if (!held || held.neededBy.length === 0) {
            return '<p class="tblstate">Nothing needs this dataset. A change to it reaches ' +
                'no other dataset.</p>';
        }
        var html = '<table class="rows"><thead><tr><th>Dataset</th><th>Name</th>' +
            '<th>The role it plays for them</th></tr></thead><tbody>';
        html += held.neededBy.map(function (d) {
            return '<tr class="pick" tabindex="0" data-dataset="' + M.esc(d.code) + '">' +
                '<td><span class="key">' + M.esc(d.code) + '</span></td>' +
                '<td class="dim">' + M.esc(d.name) + '</td>' +
                '<td class="mono">' + M.esc(d.role) + '</td></tr>';
        }).join('');
        html += '</tbody></table>';
        if (held.more > 0) {
            html += '<p class="hint">And ' + held.more + ' more. A family publish is the ' +
                'common case, so this list is read by role rather than one row per dataset.</p>';
        }
        return html;
    }

    /** The tables one dataset stages into, or the state a family holds no mock for. */
    function tablesOf(code) {
        var held = CONTENTS[code];
        if (!held) {
            return '<p class="tblstate">The tables of ' + M.esc(code) + ' are not drawn. Its ' +
                'artefact type names a family, and this prototype holds mock rows for three ' +
                'datasets.</p>';
        }
        var html = '<table class="rows"><thead><tr><th>Table</th><th>Columns</th>' +
            '<th class="num">Staged</th></tr></thead><tbody>';
        held.forEach(function (t) {
            html += '<tr class="pick" tabindex="0" data-table="' + M.esc(t.table) + '">' +
                '<td><span class="key">' + M.esc(t.table) + '</span></td>' +
                '<td class="dim">' + M.esc(t.columns.join(', ')) + '</td>' +
                '<td class="num">' + t.staged + '</td></tr>';
        });
        return html + '</tbody></table>';
    }

    /** The rows of one table, drawn as its own columns state them. */
    function rowsOfTable(code, table) {
        var held = (CONTENTS[code] || []).filter(function (t) { return t.table === table; })[0];
        if (!held) {
            return '<p class="tblstate">No table of ' + M.esc(code) + ' is named ' +
                M.esc(table) + '.</p>';
        }
        var head = '<table class="rows"><thead><tr>' +
            held.columns.map(function (c) { return '<th>' + M.esc(c) + '</th>'; }).join('') +
            '</tr></thead><tbody>';
        var body = held.rows.map(function (r) {
            return '<tr>' + r.map(function (v) { return '<td class="mono">' + M.esc(v) + '</td>'; })
                .join('') + '</tr>';
        }).join('');
        return head + body + '</tbody></table>';
    }

    function publicationsTable(rows) {
        if (rows.length === 0) {
            return '<p class="tblstate">None yet. Nothing has published this dataset.</p>';
        }
        var cols = [
            { id: 'at', h: 'Published', cell: function (p) { return M.relTime(p.at); } },
            { id: 'by', h: 'By', cell: function (p) { return M.esc(p.by); } },
            { id: 'mode', h: 'Mode', cell: function (p) { return M.esc(p.mode); } },
            { id: 'ins', h: 'Inserted', cls: 'num', cell: function (p) { return String(p.ins); } },
            { id: 'upd', h: 'Updated', cls: 'num', cell: function (p) { return String(p.upd); } },
            { id: 'skip', h: 'Skipped', cls: 'num', cell: function (p) { return String(p.skip); } },
            { id: 'del', h: 'Deleted', cls: 'num', cell: function (p) { return String(p.del); } },
            { id: 'outcome', h: 'Outcome', cell: function (p) { return outcomeBadge(p); } }
        ];
        var said = rows.filter(function (p) { return p.said.length > 0; });
        var messages = said.length === 0 ? '' :
            '<div class="cardgap"><h3>What the runs said</h3>' +
            said.map(function (p) {
                return p.said.map(function (msg) {
                    return '<p class="hint"><b>' + M.relTime(p.at) + '</b> · ' +
                        M.badge(msg.sev === 'error' ? 'bad' : 'caution', msg.sev) + ' ' +
                        M.esc(msg.text) + '</p>';
                }).join('');
            }).join('') + '</div>';
        return M.table(cols, rows, { plain: true }) + messages;
    }

    function detailsFields(b, m) {
        var last = (PUBLICATIONS[m.dataset] || [])[0];
        var reason = m.reason;
        var fields = [
            ['Name', M.esc(m.name), ''],
            ['Code', '<span class="mono">' + M.esc(m.dataset) + '</span>', ''],
            ['Bundle', M.esc(b.name) + ' <span class="dim">· ' + M.esc(b.code) + '</span>',
             M.esc(b.level) + ' level'],
            ['Artefact type', '<span class="mono">' + M.esc(m.type) + '</span>', ''],
            ['Artefact table', '<span class="mono">' + M.esc(m.type) + '_artefact_tbl</span>',
             'Where the rows are staged'],
            ['Target subject', '<span class="mono">' + M.dash(m.subject) + '</span>',
             'The bus subject a publish dispatches to'],
            ['Staged', String(m.staged), 'Rows in the artefact table'],
            ['Published', String(published(m)), 'Rows whose provenance names this dataset'],
            ['Optional', m.optional ? 'yes' : 'no', 'Publishes only when a caller opts in'],
            ['Last published', last ? M.relTime(last.at) + ' · ' + M.esc(last.by) : M.dash(''),
             last ? 'Mode ' + last.mode : 'Never published'],
            ['Readiness', reason ? M.badge('bad', reason) : M.badge('ok', 'ready'),
             reason ? (reason === 'no staged rows' ? 'The importer fixes it.' : 'Data quality fixes it.')
                    : 'Every fact a publish needs is present']
        ];
        return '<dl class="fields">' + fields.map(function (f) {
            return '<div><dt>' + f[0] + '</dt><dd>' + f[1] +
                (f[2] ? '<span class="hint" style="display:block">' + f[2] + '</span>' : '') +
                '</dd></div>';
        }).join('') + '</dl>' +
            '<div class="lastchanged">Staged and published are the counts the roster serves. ' +
            'The artefact table is the roster\'s, not a read of its own.</div>';
    }

    function renderDataset() {
        var code = Q.get('dataset') || 'refdata.conventions';
        var found = memberOf(code);
        var areas = M.shell('Data quality', PERSON);
        var tab = Q.get('tab') || 'details';
        if (!found) {
            document.getElementById('app').innerHTML = areas +
                '<main class="page">' + M.crumbs([
                    { t: 'Home', href: '#' }, { t: 'Data quality', href: '#' },
                    { t: 'Reference data coverage', href: 'index.html' }, { t: code }
                ]) + '<div class="card"><p class="tblstate">No bundle carries ' +
                M.esc(code) + '.</p></div></main>';
            return;
        }
        var b = found.bundle;
        var m = found.member;
        var pubs = PUBLICATIONS[code] || [];
        var warned = pubs.filter(function (p) {
            return p.outcome === 'failed' || p.said.length > 0;
        }).length;
        var crumbs = M.crumbs([
            { t: 'Home', href: '#' },
            { t: 'Data quality', href: '#' },
            { t: 'Reference data coverage', href: 'index.html?layout=librarian&bundle=' + encodeURIComponent(b.code) },
            { t: code }
        ]);
        var head = M.pagehead(M.esc(m.name), M.esc(code) + ' · ' + M.esc(b.level) + ' level',
            '<a class="btn small" href="../publish-a-dataset/index.html">Publish</a>');
        var tabs = '<div class="tabs">' + [
            ['details', 'Details'],
            ['publications', 'Publications' + (pubs.length ? ' (' + pubs.length + ')' : '') +
                (warned ? ' ⚠' : '')],
            ['contents', 'Contents'],
            ['lineage', 'Lineage'],
            ['history', 'History']
        ].map(function (t) {
            return '<button class="' + (tab === t[0] ? 'on' : '') + '" data-act="tab" data-arg="' +
                t[0] + '">' + t[1] + '</button>';
        }).join('') + '</div>';
        var body;
        if (tab === 'contents') {
            var table = Q.get('table') || '';
            var tables = CONTENTS[code] || [];
            body = table === ''
                ? '<div class="cardhead"><h2>Contents</h2><span class="dim">Artefact type ' +
                  M.esc(m.type) + ' · the tables it names</span></div>' + tablesOf(code) +
                  '<p class="hint">A dataset stages into the table or the family of tables its ' +
                  'artefact type names. Each table carries its own columns, so a table is opened ' +
                  'to read its rows.</p>'
                : '<div class="cardhead"><h2>' + M.esc(table) + '</h2><span class="dim">' +
                  M.esc(code) + '</span></div>' + rowsOfTable(code, table) +
                  '<p class="hint"><a href="#" data-act="back-to-tables">All tables</a></p>';
            if (table === '' && tables.length === 0) {
                body = '<div class="cardhead"><h2>Contents</h2></div>' + tablesOf(code);
            }
        } else if (tab === 'lineage') {
            body = '<div class="cardhead"><h2>Lineage</h2><span class="dim">What publishes ' +
                'before this dataset, and what waits on it</span></div>' +
                '<h3 class="lineage-head">Needs</h3>' + lineageNeeds(code) +
                '<h3 class="lineage-head">Needed by</h3>' + lineageNeededBy(code) +
                '<p class="hint">A dependent really is published after the dataset it needs: ' +
                'the publish order resolves these edges before it dispatches. The role states ' +
                'what the dependent takes, not the order.</p>';
        } else if (tab === 'publications') {
            body = '<div class="cardhead"><h2>Publications</h2><span class="dim">One row per ' +
                'publish of this dataset</span></div>' + publicationsTable(pubs) +
                '<p class="hint">A run records what it wrote and, when a row could not be written, ' +
                'what it said. A warning is a row the run chose not to write; an error is a row it ' +
                'could not.</p>';
        } else if (tab === 'history') {
            body = '<div class="cardhead"><h2>History</h2><span class="dim">The versions of the ' +
                'dataset row</span></div><p class="tblstate">None yet. A dataset is versioned, and ' +
                'its versions are not drawn here.</p>';
        } else {
            body = detailsFields(b, m);
        }
        document.getElementById('app').innerHTML = areas + '<main class="page">' + crumbs + head +
            '<div class="card cardgap">' + tabs + body + '</div>' +
            M.gap('The rows a dataset has staged now have a read over the bus, ' +
                'dq.v1.ops.dataset_contents, which carries the columns with the rows. This ' +
                'prototype still draws mock rows because it reads no server.') +
            M.gap('The published count has no operation. Every published row names its dataset and no ' +
                'service counts by it, so the figure here is mock data.') +
            M.gap('The dependencies have no read over the bus. The store holds every edge in ' +
                'ores_dq_dataset_dependencies_tbl and the publish order already resolves them, ' +
                'but no operation answers them for a screen; until one does, this tab draws mock ' +
                'edges.') +
            '</main>';
        wireDataset();
    }

    function wireDataset() {
        document.querySelectorAll('.tabs button').forEach(function (btn) {
            btn.addEventListener('click', function () {
                goto({ tab: btn.getAttribute('data-arg'), table: null });
            });
        });
        document.querySelectorAll('[data-dataset]').forEach(function (el) {
            el.addEventListener('click', function () {
                var here = Q.get('dataset') || 'refdata.conventions';
                if (el.getAttribute('data-dataset') !== here) {
                    goto({ dataset: el.getAttribute('data-dataset'), tab: null, table: null });
                }
            });
        });
        document.querySelectorAll('tr[data-table]').forEach(function (tr) {
            tr.addEventListener('click', function () {
                goto({ table: tr.getAttribute('data-table') });
            });
        });
        document.querySelectorAll('[data-act="back-to-tables"]').forEach(function (a) {
            a.addEventListener('click', function (e) {
                e.preventDefault();
                goto({ table: null });
            });
        });
        drawWires();
        document.getElementById('proto-bar').innerHTML = M.bar([
            { v: 'tenant', t: 'This tenant' },
            { v: 'fresh', t: 'Fresh database' },
            { v: 'back', t: 'Back to the list' }
        ], EMPTY ? 'fresh' : 'tenant');
        document.querySelectorAll('[data-act="bar"]').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var arg = btn.getAttribute('data-arg');
                if (arg === 'back') {
                    goto({ view: null, dataset: null, tab: null, member: null,
                        layout: 'librarian', bundle: SELECTED });
                } else if (arg === 'fresh') { goto({ empty: '1' }); }
                else { goto({ empty: null }); }
            });
        });
        var g = document.querySelector('[data-act="gaps"]');
        if (g) {
            g.addEventListener('click', function () {
                goto(document.body.classList.contains('nogaps') ? { gaps: null } : { gaps: '0' });
            });
        }
    }

    function renderLibrarian() {
        if (Q.get('view') === 'dataset') { renderDataset(); return; }
        var b = BUNDLES.filter(function (x) { return x.code === SELECTED; })[0];
        var areas = M.shell('Data quality', PERSON);
        var crumbs = M.crumbs([
            { t: 'Home', href: '#' },
            { t: 'Data quality', href: '#' },
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
                'the installation yet, and a recreate that fails to publish leaves exactly this screen.')
            : '';
        var html = areas + '<main class="page">' + crumbs + head + notice +
            '<div class="lib">' +
            '<section class="lib-pane"><div class="lib-head"><h2>Bundles</h2>' +
            '<span class="dim">' + BUNDLES.length + '</span></div><div class="lib-body">' +
            tree() + '</div></section>' +
            '<section class="lib-pane"><div class="lib-head"><h2>' +
            (b ? M.esc(b.name) : 'Datasets') + '</h2><span class="dim">' +
            (b ? M.esc(b.code) + ' · ' + b.datasets + ' datasets · ' + M.esc(b.level) + ' level' : '') +
            '</span></div>' + membersPane(b) +
            '<p class="hint" style="padding: 0 12px 12px;">Staged counts the artefact table. ' +
            'Published counts the rows whose provenance names this dataset. A row opens the ' +
            "dataset's own screen.</p></section>" +
            '</div>' + gaps() + '</main>';
        document.getElementById('app').innerHTML = html;
        document.getElementById('proto-bar').innerHTML = M.bar([
            { v: 'tenant', t: 'This tenant' },
            { v: 'fresh', t: 'Fresh database' },
            { v: 'table', t: 'Table layout' }
        ], EMPTY ? 'fresh' : 'tenant');
        wireLibrarian();
    }

    function goto(params) {
        var q = new URLSearchParams(window.location.search);
        Object.keys(params).forEach(function (k) {
            if (params[k] === null) { q.delete(k); } else { q.set(k, params[k]); }
        });
        window.location.search = q.toString();
    }

    function wireLibrarian() {
        document.querySelectorAll('.lib-tree .node').forEach(function (btn) {
            btn.addEventListener('click', function () {
                goto({ bundle: btn.getAttribute('data-bundle'), member: null });
            });
        });
        document.querySelectorAll('.lib-table tr[data-row]').forEach(function (tr) {
            tr.addEventListener('click', function () {
                var rows = shownMembers(BUNDLES.filter(function (x) { return x.code === SELECTED; })[0]);
                var m = rows[Number(tr.getAttribute('data-row'))];
                if (m) { goto({ view: 'dataset', dataset: m.dataset, tab: null, member: null }); }
            });
            tr.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' || e.key === ' ') {
                    var rows = shownMembers(BUNDLES.filter(function (x) { return x.code === SELECTED; })[0]);
                    var m = rows[Number(tr.getAttribute('data-row'))];
                    if (m) { goto({ view: 'dataset', dataset: m.dataset, tab: null, member: null }); }
                }
            });
        });
        var input = document.getElementById('q');
        if (input) {
            input.addEventListener('change', function () { goto({ q: input.value || null }); });
        }
        var select = document.getElementById('state');
        if (select) {
            select.addEventListener('change', function () { goto({ state: select.value }); });
        }
        wire();
    }

    function render() {
        if (LIBRARIAN) { renderLibrarian(); return; }
        var b = BUNDLES.filter(function (x) { return x.code === SELECTED; })[0];
        var areas = M.shell('Data quality', PERSON);
        var crumbs = M.crumbs([
            { t: 'Home', href: '#' },
            { t: 'Data quality', href: '#' },
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
            { v: 'fresh', t: 'Fresh database' },
            { v: 'layout', t: 'Librarian layout' }
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
                var arg = btn.getAttribute('data-arg');
                if (arg === 'layout') { q.set('layout', 'librarian'); }
                else if (arg === 'table') { q.delete('layout'); }
                else if (arg === 'fresh') { q.set('empty', '1'); }
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

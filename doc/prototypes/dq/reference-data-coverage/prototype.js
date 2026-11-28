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
                    ['GB', 'GBR', '826', 'United Kingdom', 'gb'],
                    ['US', 'USA', '840', 'United States', 'us'],
                    ['JP', 'JPN', '392', 'Japan', 'jp'],
                    ['CH', 'CHE', '756', 'Switzerland', 'ch']
                ],
                // A column states its kind, and an image column is drawn rather
                // than printed: the read answers the image's code, not the uuid
                // the artefact table holds, so the screen never resolves one.
                columns: [
                    'alpha2_code', 'alpha3_code', 'numeric_code', 'name',
                    { name: 'image', kind: 'image' }
                ],
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

    /* What the dataset row itself states, beside the roster's publish-facing
     * facts. The store holds all of it and the generated dataset read already
     * answers it; this prototype draws it from a mock so the shape can be read
     * before a screen reads the row. */
    var FACTS = {
        'refdata.conventions': {
            version: 3,
            description: 'The curated index and instrument convention set every tenant ' +
                'reads: the conventions of the ORE examples, the index conventions ORE ' +
                'defines in code, and the conventions OreStudio adds, one row per (kind, id), ' +
                'each with its source in conventions.tsv.',
            business_context: 'The conventions trade builders, curve bootstraps and ORE runs ' +
                'resolve an index or an instrument by.',
            domain: 'Reference Data',
            subject: 'Conventions',
            catalog: 'ORE Studio',
            origin: 'Primary',
            nature: 'Actual',
            treatment: 'Raw',
            source_system: 'ORESTUDIO',
            license: 'Internal Use Only',
            coding_scheme: 'NONE',
            upstream_derivation: '',
            lineage_depth: 0,
            as_of: '2026-10-19T00:00:00Z',
            ingestion: '2026-10-19T09:12:00Z',
            recorded_at: '2026-10-19T09:12:00Z',
            modified_by: 'system',
            change_reason: 'Initial Load',
            change_commentary: 'Seeded from conventions.tsv.',
            methodology: {
                id: 'a9b384aa-aa6f-4067-9947-d1ab0e4d798a',
                name: 'Convention Curation Methodology',
                version: 1,
                logic: 'https://github.com/OreStudio/OreStudio/tree/main/projects/ores.seeder/' +
                    'datasets/conventions',
                description: 'How the one convention set is curated from the ORE examples, the ' +
                    'ORE engine and the index administrators, and how a change or an ORE ' +
                    'upgrade reaches it.',
                steps: [
                    '1. SOURCE OF TRUTH',
                    'conventions.tsv is the dataset: one row per (kind, id), its terms in ' +
                    'ORE\u2019s own notation, and the source they are taken from. The populate ' +
                    'script is generated from the table and never edited by hand.',
                    '2. SOURCES',
                    'A row names one of four sources: an ORE example, the ORE engine, an ' +
                    'administrator publication, or OreStudio itself, and the source column ' +
                    'cites the file or the publication the terms follow.',
                    '3. CHANGING A ROW',
                    'Edit conventions.tsv, state the source of the new terms, and run ' +
                    'generate_sql.py. A change to a row that came from an ORE source moves it ' +
                    'to the source its new terms follow.',
                    '4. AN ORE UPGRADE',
                    'After a new ORE release is vendored, its examples are extracted and its ' +
                    'engine mined, then compared with the dataset: the report names each ' +
                    'convention ORE holds that the dataset lacks, and each whose terms differ.'
                ]
            }
        },
        'refdata.currencies': {
            version: 2,
            description: 'ISO 4217 currency codes, their minor units and the currencies the ' +
                'deployment trades.',
            business_context: 'Every amount, curve and trade names a currency from this set.',
            domain: 'Reference Data', subject: 'Currencies', catalog: 'ISO 4217',
            origin: 'Primary', nature: 'Actual', treatment: 'Cleaned',
            source_system: 'ISO', license: 'Internal Use Only', coding_scheme: 'ISO 4217',
            upstream_derivation: '', lineage_depth: 0,
            as_of: '2026-10-19T00:00:00Z', ingestion: '2026-10-19T09:12:00Z',
            recorded_at: '2026-10-19T09:12:00Z', modified_by: 'system',
            change_reason: 'Initial Load', change_commentary: 'Seeded from the ISO list.',
            methodology: {
                name: 'ISO List Ingestion',
                version: 1,
                logic: 'projects/ores.seeder/datasets/currencies',
                description: 'How the ISO 4217 list is read and its minor units stated.',
                steps: ['The published list is read as published.',
                        'A currency the list withdraws is kept while a trade names it.']
            }
        },
        'refdata.calendars': {
            version: 1,
            description: 'The business calendars a date rolls on, with their holidays.',
            business_context: 'Every schedule and settlement date resolves against one of these.',
            domain: 'Reference Data', subject: 'Calendars', catalog: 'ORE Studio',
            origin: 'Derived', nature: 'Actual', treatment: 'Enriched',
            source_system: 'ORESTUDIO', license: 'Internal Use Only', coding_scheme: 'NONE',
            upstream_derivation: 'iso.countries', lineage_depth: 1,
            as_of: '2026-10-19T00:00:00Z', ingestion: '2026-10-19T09:12:00Z',
            recorded_at: '2026-10-19T09:12:00Z', modified_by: 'system',
            change_reason: 'Initial Load', change_commentary: '',
            methodology: {
                name: 'Calendar Assembly',
                version: 2,
                logic: 'projects/ores.seeder/datasets/calendars',
                description: 'How each calendar is assembled from its country and type.',
                steps: ['The calendar type states the weekend and the rule.',
                        'The country states which holidays the calendar takes.']
            }
        },
        'refdata.countries': {
            version: 1,
            description: 'ISO 3166 countries, their codes and their names.',
            business_context: 'Parties, calendars and holidays name a country from this set.',
            domain: 'Reference Data', subject: 'Countries', catalog: 'ISO 3166',
            origin: 'Primary', nature: 'Actual', treatment: 'Raw',
            source_system: 'ISO', license: 'Internal Use Only', coding_scheme: 'ISO 3166',
            upstream_derivation: '', lineage_depth: 0,
            as_of: '2026-10-19T00:00:00Z', ingestion: '2026-10-19T09:12:00Z',
            recorded_at: '2026-10-19T09:12:00Z', modified_by: 'system',
            change_reason: 'Initial Load', change_commentary: '',
            methodology: null
        }
    };

    /** One dataset's own row, or nothing when the prototype holds no mock for it. */
    function factsOf(code) {
        return FACTS[code] || null;
    }

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
    /* The page sizes every list offers, and the size a list starts at. Both are
     * the record screen standard's; the contents view is a list and pages like
     * one, rather than capping itself at the first fifty rows. */
    var PAGE_SIZES = [15, 25, 50, 100, 200, 500];
    var DEFAULT_PAGE_SIZE = 15;

    /** The standard pager: where this page sits in the whole, and the way on. */
    function pager(offset, shown, total, size) {
        var first = shown === 0 ? 0 : offset + 1;
        var last = offset + shown;
        var lastOffset = Math.max(0, Math.floor((total - 1) / size) * size);
        var atStart = offset === 0;
        var atEnd = offset + shown >= total;
        function btn(label, target, disabled) {
            return '<button class="btn small" data-act="page" data-arg="' + target + '"' +
                (disabled ? ' disabled' : '') + '>' + label + '</button>';
        }
        function step(delta) {
            return Math.max(0, Math.min(lastOffset, offset + delta));
        }
        return '<div class="pager"><span>Rows ' + first + '\u2013' + last + ' of ' +
            total.toLocaleString() + '</span><span class="pager-acts">' +
            btn('First', 0, atStart) + btn('Previous', step(-size), atStart) +
            btn('Next', step(size), atEnd) + btn('Last', lastOffset, atEnd) +
            '<label>Page size <select data-act="size">' +
            PAGE_SIZES.map(function (n) {
                return '<option value="' + n + '"' +
                    (n === size ? ' selected' : '') + '>' + n + '</option>';
            }).join('') + '</select></label>' +
            (total <= 1000 && total > size ?
                '<button class="btn small ghost" data-act="size" data-arg="' + total +
                '">Load all</button>' : '') +
            '</span></div>';
    }

    /**
     * One table's rows as the standard list draws them: the columns the rows
     * carry, then the pager.
     *
     * The prototype holds a handful of mock rows, so a page past the first says
     * so rather than pretending the table ended.
     */
    /** A column's name, whether it is stated as a string or as a name and kind. */
    function columnName(c) {
        return typeof c === 'string' ? c : c.name;
    }

    /** What a column holds, which decides how its cell is drawn. */
    function columnKind(c) {
        return typeof c === 'string' ? 'text' : (c.kind || 'text');
    }

    /** One cell, drawn as its column's kind: an image is shown, not printed. */
    function cell(c, value) {
        if (columnKind(c) === 'image') {
            return '<td class="imgcell"><img src="flag-' + M.esc(value) + '.svg" alt="' +
                M.esc(value) + '" title="' + M.esc(value) + '"></td>';
        }
        return '<td class="mono">' + M.esc(value) + '</td>';
    }

    function rowsOfTable(code, table) {
        var held = (CONTENTS[code] || []).filter(function (t) { return t.table === table; })[0];
        if (!held) {
            return '<p class="tblstate">No table of ' + M.esc(code) + ' is named ' +
                M.esc(table) + '.</p>';
        }
        var offset = Math.max(0, Number(Q.get('page') || 0));
        var size = Number(Q.get('size') || DEFAULT_PAGE_SIZE);
        var head = '<table class="rows"><thead><tr>' +
            held.columns.map(function (c) { return '<th>' + M.esc(columnName(c)) + '</th>'; })
                .join('') + '</tr></thead><tbody>';
        var shown = Math.min(size, held.rows.length);
        var visible = held.rows.slice(offset, offset + size);
        var body = visible.length === 0
            ? '<tr><td class="tblstate" colspan="' + held.columns.length + '">The prototype ' +
              'holds mock rows for the first page only.</td></tr>'
            : visible.map(function (r) {
                return '<tr>' + r.map(function (v, i) { return cell(held.columns[i], v); })
                    .join('') + '</tr>';
            }).join('');
        return head + body + '</tbody></table>' +
            pager(offset, visible.length === 0 ? 0 : shown, held.staged, size);
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

    /** One titled card, as the Qt dialog drew its sections. */
    function dcard(title, body, note) {
        return '<section class="dcard"><h3>' + title +
            (note ? ' <span class="dim">' + note + '</span>' : '') + '</h3>' + body + '</section>';
    }

    /** A label and value list inside a card, one row each. */
    function kv(pairs) {
        return '<dl class="kv">' + pairs.map(function (p) {
            return '<div><dt>' + p[0] + '</dt><dd>' + p[1] +
                (p[2] ? '<span class="hint" style="display:block">' + p[2] + '</span>' : '') +
                '</dd></div>';
        }).join('') + '</dl>';
    }

    /**
     * What the dataset is: its own row, in the cards the Qt client drew for it.
     *
     * The cards answer different questions — what it is, how it is classified,
     * where it came from, what a publish does with it — and a single grid of
     * seventeen values answered none of them, which is why the review could not
     * find the dataset in its own page.
     */
    function detailsFields(b, m) {
        var last = (PUBLICATIONS[m.dataset] || [])[0];
        var reason = m.reason;
        var f = factsOf(m.dataset) || {};
        var main = dcard('Description', f.description ?
                '<p>' + M.esc(f.description) + '</p>' : '<p class="muted-dash">—</p>') +
            dcard('Business context', f.business_context ?
                '<p>' + M.esc(f.business_context) + '</p>' : '<p class="muted-dash">—</p>') +
            dcard('Commentary', (f.change_reason ?
                '<p><b>' + M.esc(f.change_reason) + '</b> ' +
                M.esc(f.change_commentary || '') + '</p>' : '<p class="muted-dash">—</p>')) +
            dcard('Lifecycle', kv([
                ['As of date', M.relTime(f.as_of),
                 'The date the rows state, not when they reached the store'],
                ['Ingestion', M.relTime(f.ingestion), 'When the rows reached the store']
            ])) +
            dcard('Publish', kv([
                ['Artefact type', '<span class="mono">' + M.esc(m.type) + '</span>', ''],
                ['Artefact table',
                 '<span class="mono">ores_dq_' + M.esc(m.type) + '_artefact_tbl</span>',
                 'Where the rows are staged'],
                ['Target subject', '<span class="mono">' + M.dash(m.subject) + '</span>',
                 'The bus subject a publish dispatches to'],
                ['Staged', String(m.staged), 'Rows in the artefact table'],
                ['Published', String(published(m)),
                 'Rows whose provenance names this dataset'],
                ['Optional', m.optional ? 'yes' : 'no',
                 'Publishes only when a caller opts in'],
                ['Last published',
                 last ? M.relTime(last.at) + ' \u00b7 ' + M.esc(last.by) : M.dash(''),
                 last ? 'Mode ' + last.mode : 'Never published'],
                ['Readiness', reason ? M.badge('bad', reason) : M.badge('ok', 'ready'),
                 reason ? (reason === 'no staged rows' ? 'The importer fixes it.'
                                                      : 'Data quality fixes it.')
                        : 'Every fact a publish needs is present']
            ]));
        var meth = f.methodology;
        var side = dcard('Classification', kv([
                ['Domain', M.esc(f.domain || '-'), ''],
                ['Subject area', M.esc(f.subject || '-'), ''],
                ['Catalog', M.esc(f.catalog || '-'), ''],
                ['Origin', M.badge('class', f.origin || '-'), 'Where the rows come from'],
                ['Nature', M.badge('class', f.nature || '-'), 'How the values are arrived at'],
                ['Treatment', M.badge('class', f.treatment || '-'), 'What was done to them']
            ])) +
            dcard('Source and governance', kv([
                ['Source system', '<span class="mono">' + M.esc(f.source_system || '-') +
                    '</span>', ''],
                ['License', M.esc(f.license || '-'), ''],
                ['Coding scheme', '<span class="mono">' + M.esc(f.coding_scheme || 'NONE') +
                    '</span>', '']
            ])) +
            dcard('Methodology', meth ? kv([
                ['Name', M.esc(meth.name), ''],
                ['Version', String(meth.version), ''],
                ['Logic reference', '<a class="mono" href="' + M.esc(meth.logic) + '">' +
                    M.esc(meth.logic) + '</a>', 'Where the logic is written down']
            ]) : '<p class="tblstate">No methodology is assigned.</p>') +
            dcard('Audit', kv([
                ['Version', String(f.version || '-'), ''],
                ['Modified by', '<span class="mono">' + M.esc(f.modified_by || '-') +
                    '</span>', ''],
                ['Recorded at', M.relTime(f.recorded_at), '']
            ]));
        return '<div class="dossier"><div class="dossier-main">' + main +
            '</div><aside class="dossier-side">' + side + '</aside></div>' +
            '<div class="lastchanged">Version ' + (f.version || '-') + ' \u00b7 modified by ' +
                M.esc(f.modified_by || '-') + ' \u00b7 recorded ' + M.relTime(f.recorded_at) +
                ' \u00b7 ' + M.esc(f.change_reason || '-') +
                (f.change_commentary ? ' \u00b7 ' + M.esc(f.change_commentary) : '') + '</div>';
    }

    /**
     * The provenance of one dataset: where its row came from, and the methodology
     * its values are derived by.
     *
     * The Qt client split this from the overview for the same reason: the overview
     * answers what the dataset is, and this answers how its values came to be. A
     * methodology is a record of its own, so its steps are read, not summarised.
     */
    function provenancePanel(code) {
        var f = factsOf(code);
        if (!f) {
            return '<p class="tblstate">No dataset row is drawn for ' + M.esc(code) + '.</p>';
        }
        var meth = f.methodology;
        var main = dcard('Methodology',
            meth ? (meth.description ? '<p>' + M.esc(meth.description) + '</p>' : '') +
                '<div class="meth-body">' + meth.steps.map(function (step, i) {
                    // A numbered step is a heading in the source; every other
                    // line is prose under it.
                    return /^\d+\./.test(step) ?
                        '<h4>' + M.esc(step) + '</h4>' : '<p>' + M.esc(step) + '</p>';
                }).join('') + '</div>' :
                '<p class="tblstate">No methodology is assigned. Nothing states how the ' +
                'values are derived.</p>',
            meth ? M.esc(meth.name) + ' \u00b7 version ' + meth.version : '');
        var side = dcard('Source', kv([
                ['Source system', '<span class="mono">' + M.esc(f.source_system || '-') +
                    '</span>', 'The system the rows were read from'],
                ['License', M.esc(f.license || '-'), 'What the deployment may do with the rows'],
                ['Coding scheme', '<span class="mono">' + M.esc(f.coding_scheme || 'NONE') +
                    '</span>', '']
            ])) +
            dcard('Lineage metrics', kv([
                ['Upstream derivation',
                 f.upstream_derivation ?
                    '<a class="mono" href="#" data-dataset="' +
                    M.esc(f.upstream_derivation) + '">' + M.esc(f.upstream_derivation) + '</a>' :
                    M.dash(''),
                 'The dataset this one is derived from'],
                ['Lineage depth', String(f.lineage_depth === undefined ? '-' : f.lineage_depth),
                 'How many derivations away from a source']
            ])) +
            (meth ? dcard('Methodology info', kv([
                ['Name', M.esc(meth.name), ''],
                ['Version', String(meth.version), ''],
                ['ID', '<span class="mono">' + M.esc(meth.id) + '</span>', ''],
                ['Logic reference', '<a class="mono" href="' + M.esc(meth.logic) + '">' +
                    M.esc(meth.logic) + '</a>', '']
            ])) : '') +
            dcard('Audit', kv([
                ['Modified by', '<span class="mono">' + M.esc(f.modified_by || '-') +
                    '</span>', ''],
                ['Recorded at', M.relTime(f.recorded_at), ''],
                ['Change reason', M.esc(f.change_reason || '-'), '']
            ]));
        return '<div class="dossier"><div class="dossier-main">' + main +
            '</div><aside class="dossier-side">' + side + '</aside></div>';
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
            ['provenance', 'Provenance'],
            ['publications', 'Publications' + (pubs.length ? ' (' + pubs.length + ')' : '') +
                (warned ? ' ⚠' : '')],
            ['contents', 'Contents'],
            ['dependencies', 'Dependencies'],
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
        } else if (tab === 'provenance') {
            body = '<div class="cardhead"><h2>Provenance and methodology</h2>' +
                '<span class="dim">Where the row came from, and how its values are derived' +
                '</span></div>' + provenancePanel(code) +
                '<p class="hint">A methodology is a record of its own, so it is read rather ' +
                'than summarised: its steps are the derivation, and a version of it is what a ' +
                'value was produced under.</p>';
        } else if (tab === 'dependencies') {
            body = '<div class="cardhead"><h2>Dependencies</h2><span class="dim">What publishes ' +
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
            M.gap('The methodology has no read over HTTP. The store holds 23 of them and the ' +
                'dataset row names the one it was produced under, but the generated entity has ' +
                'no route, so this tab draws mock steps; the answer is one joined read of the ' +
                'dataset and its methodology, not a catalogue the page sorts through.') +
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
        document.querySelectorAll('[data-act="page"]').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var target = Number(btn.getAttribute('data-arg'));
                goto({ page: target === 0 ? null : String(target) });
            });
        });
        document.querySelectorAll('[data-act="size"]').forEach(function (el) {
            el.addEventListener('change', function () {
                goto({ size: el.value, page: null });
            });
            el.addEventListener('click', function () {
                var arg = el.getAttribute('data-arg');
                if (arg) { goto({ size: arg, page: null }); }
            });
        });
        document.querySelectorAll('tr[data-table]').forEach(function (tr) {
            tr.addEventListener('click', function () {
                goto({ table: tr.getAttribute('data-table'), page: null });
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

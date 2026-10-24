/* A report and everything it runs. Self-contained: plain JavaScript, mock data,
 * no framework, no build step, and nothing that outlives the page.
 *
 * One page for one report definition, on six tabs. Each tab has one owner and
 * says what a change on it reaches:
 *
 *   Definition          what the report is, with its report currency
 *   Scope               the portfolios and books it reads, and the book type rule
 *   Analytics           what it computes, and how the engine runs it
 *   Market and pricing  the market configuration it prices against, and the three
 *                       configurations every report of the party shares
 *   Conventions         the market terms it reads, and the proposed set choice
 *   History             the standard history of the definition, and the versions
 *                       of what it runs with
 *
 * The owner chose the four-tab layout, then asked for the run settings to be
 * split: a run settings configuration is not one thing. It holds a currency, a
 * market data choice, a few engine settings, and two children, the scope and the
 * analytics. So the scope and the analytics are tabs of their own, the currency
 * is the report's, and the rest sits with the market and the engine.
 *
 * What is the system as it is, and what is proposed, is marked on the page with
 * the words "today" and "proposed". The convention counts are the rows of the 26
 * convention tables of the bright_faraday development database on 2026-10-21;
 * every other name, set and purpose is a fixture.
 *
 *   ?tab=definition|scope|analytics|market|conventions|history
 *   ?conv=today|sets  ?type=none|Trading|Banking  ?hand=no|yes  ?shared=no|yes
 *   ?md=eod|intraday  ?role=admin|reader
 */

(function () {
    'use strict';

    // ------------------------------------------------------------ mock data

    var PARTY = 'ACME Corporation HK Ltd';

    var DEFINITION = {
        id: 'daf18b08',
        name: 'NPV',
        description: 'Net present value of every trade in the entity, in the report currency.',
        reportType: 'risk',
        schedule: 'At 18:30, Monday through Friday',
        cron: '30 18 * * 1-5',
        concurrency: 'Skip',
        official: true,
        version: 3,
        health: 'Completed',
        latest: { id: 'a2e6c04d', at: '2026-10-21 17:05', took: '9 s', trades: 8, state: 'Completed' }
    };

    var CURRENCY = { code: 'HKD', name: 'Hong Kong dollar', flag: '🇭🇰' };

    var RUN_SETTINGS = {
        name: 'Valuation - Global Portfolio',
        sharedWith: ['Cashflows', 'Intraday Risk Monitor'],
        analytics: [
            { code: 'npv', what: 'Net present value of every trade', on: true, order: 1 },
            { code: 'cashflow', what: 'The cash flows of every trade', on: false, order: 2 },
            { code: 'sensitivity', what: 'Bump and revalue sensitivities', on: false, order: 3 },
            { code: 'simulation', what: 'The Monte Carlo cube', on: false, order: 4 },
            { code: 'pfe', what: 'Potential future exposure, reads the cube', on: false, order: 5 },
            { code: 'xva', what: 'CVA, DVA and FVA', on: false, order: 6 }
        ],
        scopeLines: [{ name: 'Global Portfolio', kind: 'portfolio', books: 8, trading: 6, banking: 2 }],
        handBook: { name: 'Rates Desk 1', kind: 'book', type: 'Trading' },
        engine: [
            { group: 'Run', rows: [
                ['Threads', 'nThreads', null],
                ['Observation model', 'observationModel', null],
                ['Continue on error', 'continueOnError', null],
                ['Build failed trades', 'buildFailedTrades', null],
                ['Lazy market building', 'lazyMarketBuilding', null],
                ['Use analytics', 'useAnalytics', null]
            ] },
            { group: 'Dates', rows: [
                ['Accrual date', 'accrualDate', null],
                ['Fixing cutoff', 'fixingCutoff', null],
                ['Date calendar', 'dateCalendar', null],
                ['Date convention', 'dateConvention', null]
            ] },
            { group: 'Fixings and cash flows', rows: [
                ['Imply today’s fixings', 'implyTodaysFixings', 'false'],
                ['Include today’s cash flows', 'includeTodaysCashFlows', null],
                ['Include reference date events', 'includeReferenceDateEvents', null],
                ['Enrich index fixings', 'enrichIndexFixings', null],
                ['Ignore fixing lag', 'ignoreFixingLag', null]
            ] },
            { group: 'Files', rows: [
                ['Input path', 'inputPath', 'Input'],
                ['Market input path', 'inputPathMarket', null],
                ['Portfolio input path', 'inputPathPortfolio', null],
                ['Output path', 'outputPath', 'Output'],
                ['Log file', 'logFile', 'log.txt'],
                ['Portfolio recurses into folders', 'portfolioRecurseIntoSubDirectories', null],
                ['CSV header comment', 'csvCommentReportHeader', null],
                ['Default mapping to identity', 'defaultMappingToIdentity', null],
                ['Additional results', 'additionalResults', null]
            ] },
            { group: 'Logging', rows: [
                ['Log mask', 'logMask', '31']
            ] }
        ],
        market: [
            ['Pricing market', 'default']
        ]
    };

    var SHARED = [
        {
            type: 'Pricing engines',
            name: 'HK pricing engines',
            what: 'Which model prices each product',
            used: 22,
            screen: 'prototype',
            href: '../product-pricing/'
        },
        {
            type: 'Today’s market',
            name: 'HK today’s market',
            what: 'Which curves and quotes make the market of the day',
            used: 22,
            screen: 'none'
        },
        {
            type: 'Curve configuration',
            name: 'HK curve configuration',
            what: 'How each curve is built from its quotes',
            used: 22,
            screen: 'none'
        }
    ];

    // Measured. One row per convention kind: the documents the database holds.
    var KINDS = [
        ['commodity_future', 3950], ['swap', 1600], ['swap_index', 1215], ['cross_currency_basis', 1035],
        ['fx_option', 865], ['deposit', 815], ['ois', 260], ['fra', 210], ['zero', 190],
        ['generic', 185], ['currency_pair', 179], ['cross_currency_fix_float', 120],
        ['inflation_swap', 110], ['overnight_index', 48], ['zero_inflation_index', 45],
        ['future', 35], ['average_ois', 25], ['bond_yield', 25], ['cds', 20],
        ['commodity_forward', 20], ['tenor_basis_two_swap', 20], ['ibor_index', 17],
        ['bma_basis_swap', 15], ['intraday_power_load', 10], ['tenor', 8], ['cms_spread_option', 5]
    ];

    var SETS = [
        { id: 'none', name: 'Every document (today)', purpose: 'No set. A run reads all the documents the party holds.', kinds: 26, documents: 0, used: 22, current: true },
        { id: 'set-1', name: 'ACME HK desk default', purpose: 'The terms the Hong Kong desks book and are marked on.', kinds: 26, documents: 26, used: 18, current: false },
        { id: 'set-2', name: 'Dubai swaps desk', purpose: 'AED swaps on ACT/360 with the desk fixing calendar.', kinds: 9, documents: 9, used: 0, current: false },
        { id: 'set-3', name: 'LIBOR fallback review', purpose: 'Fallback terms for the IBOR indices, for the review of old trades.', kinds: 4, documents: 4, used: 1, current: false }
    ];

    var TOTAL_DOCUMENTS = KINDS.reduce(function (n, k) { return n + k[1]; }, 0);
    SETS[0].documents = TOTAL_DOCUMENTS;

    // The history is one stream, as the entity screen specification draws it: the
    // versions of the definition and of the configurations it binds share one rail,
    // and each entry names its kind and its record.
    var PEOPLE = {
        tenant_admin: { name: 'Tenant Admin', initials: 'TA', service: false },
        system: { name: 'ores_bright_faraday_iam_service', initials: 'IS', service: true }
    };

    var KINDS_OF_ENTRY = { raised: 'Raised', changed: 'Changed' };

    var ENTRIES = [
        {
            id: 'e1', at: '2026-10-21T14:40:12', kind: 'changed', record: 'Report definition NPV', version: 3,
            actor: 'tenant_admin', reason: 'common.non_material_update', current: true, revertable: true,
            note: 'The description now says the report currency.',
            diffs: [['Description', 'Net present value of every trade in the entity, in the entity currency.',
                'Net present value of every trade in the entity, in the report currency.']]
        },
        {
            id: 'e2', at: '2026-10-21T14:12:40', kind: 'changed', record: 'Run settings Valuation - Global Portfolio', version: 2,
            actor: 'tenant_admin', reason: 'common.non_material_update', current: true, revertable: true,
            note: 'The scope is the Global Portfolio.',
            diffs: [['Scope portfolio', '', 'Global Portfolio']]
        },
        {
            id: 'e3', at: '2026-10-21T11:02:05', kind: 'changed', record: 'Report definition NPV', version: 2,
            actor: 'tenant_admin', reason: 'common.non_material_update', current: false, revertable: true,
            note: 'Moved after the Hong Kong close.',
            diffs: [['Schedule expression', '0 6 * * 1-5', '30 18 * * 1-5']]
        },
        {
            id: 'e4', at: '2026-10-21T10:23:19', kind: 'raised', record: 'HK pricing engines', version: 1,
            actor: 'system', reason: 'system.external_data_import', current: true, revertable: false,
            note: 'Published from DQ dataset',
            diffs: [['Name', '', 'HK pricing engines'], ['Configuration type', '', 'Pricing engines']]
        },
        {
            id: 'e5', at: '2026-10-21T10:23:18', kind: 'raised', record: 'HK today\u2019s market', version: 1,
            actor: 'system', reason: 'system.external_data_import', current: true, revertable: false,
            note: 'Published from DQ dataset',
            diffs: [['Name', '', 'HK today\u2019s market'], ['Configuration type', '', 'Today\u2019s market']]
        },
        {
            id: 'e6', at: '2026-10-21T10:23:17', kind: 'raised', record: 'Run settings Valuation - Global Portfolio', version: 1,
            actor: 'system', reason: 'system.external_data_import', current: false, revertable: false,
            note: 'Published from DQ dataset',
            diffs: [['Name', '', 'Valuation - Global Portfolio'], ['Report currency', '', 'HKD'], ['Market data', '', 'End of day']]
        },
        {
            id: 'e7', at: '2026-10-21T10:23:16', kind: 'raised', record: 'Report definition NPV', version: 1,
            actor: 'system', reason: 'system.external_data_import', current: false, revertable: false,
            note: 'Published from DQ dataset',
            diffs: [['Name', '', 'NPV'], ['Schedule expression', '', '0 6 * * 1-5'], ['Concurrency policy', '', 'skip']]
        }
    ];

    // --------------------------------------------------------------- state

    function query(name, fallback) {
        var m = new RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
        return m ? decodeURIComponent(m[1]) : fallback;
    }

    var TABS = [
        ['definition', 'Definition'], ['scope', 'Scope'], ['analytics', 'Analytics'],
        ['market', 'Market and pricing'], ['conventions', 'Conventions'], ['history', 'History']
    ];

    var S = {
        tab: query('tab', 'definition'),
        conv: query('conv', 'today'),
        type: query('type', 'none'),
        hand: query('hand', 'no'),
        shared: query('shared', 'no'),
        md: query('md', 'eod'),
        role: query('role', 'admin'),
        hideSystem: false,
        revertOf: null,
        flash: null,
        chosen: 'none',
        requested: false,
        reverted: false,
        dialog: false,
        dialogType: null,
        dialogMd: null,
        dialogError: null,
        showKinds: false,
        showState: false
    };

    function sync() {
        var p = [];
        ['tab', 'conv', 'type', 'hand', 'shared', 'md', 'role'].forEach(function (k) {
            p.push(k + '=' + encodeURIComponent(S[k]));
        });
        window.history.replaceState(null, '', '?' + p.join('&'));
    }

    // ------------------------------------------------------------- helpers

    function esc(s) {
        return String(s).replace(/[&<>"]/g, function (c) {
            return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
        });
    }

    function plural(n, word) {
        return n + ' ' + word + (n === 1 ? '' : 's');
    }

    function number(n) {
        return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    function kindName(code) {
        return code.replace(/_/g, ' ').replace(/\bois\b/, 'OIS').replace(/\bfx\b/, 'FX')
            .replace(/\bcds\b/, 'CDS').replace(/\bfra\b/, 'FRA').replace(/\bcms\b/, 'CMS')
            .replace(/\bibor\b/, 'IBOR').replace(/\bbma\b/, 'BMA').replace(/^./, function (c) {
                return c.toUpperCase();
            });
    }

    function mdWord() {
        return S.md === 'intraday' ? 'Intraday' : 'End of day';
    }

    // Each run reads the market data of its own time point. The definition says only
    // which kind of time point, and states no date.
    function mdMeaning() {
        return S.md === 'intraday'
            ? 'Each run reads the market data as of the time the run starts. The definition states no date: the run carries its own time.'
            : 'Each run reads the end of day market data of its business day. The definition states no date: the run carries its own.';
    }

    function asOf() {
        return S.md === 'intraday' ? '2026-10-21 17:05 (the time of the run)' : '2026-10-20, the close of the business day';
    }

    function typeWord(type) {
        return type === 'Trading' ? 'Trading book' : type === 'Banking' ? 'Banking book' : 'Every type';
    }

    function canWrite() {
        return S.role === 'admin';
    }

    function field(label, value) {
        return '<div><span class="lbl">' + label + '</span><span class="val">' + value + '</span></div>';
    }

    // The currency, drawn with its flag as the application draws a currency.
    function currencyCell() {
        return '<span class="flag" title="' + esc(CURRENCY.name) + '">' + CURRENCY.flag + '</span> ' +
            '<span class="mono">' + CURRENCY.code + '</span> <span class="faint">' + esc(CURRENCY.name) + '</span>';
    }

    // What a change to the run settings configuration reaches. The scope, the
    // analytics, the currency and the engine settings are one configuration.
    function reachNote(what) {
        if (S.shared === 'yes') {
            return '<div class="notice warn"><b>Shared by ' + (RUN_SETTINGS.sharedWith.length + 1) + ' reports.</b> ' +
                esc(what) + ' belongs to the run settings <span class="mono">' + esc(RUN_SETTINGS.name) +
                '</span>, so a change here reaches NPV, ' + esc(RUN_SETTINGS.sharedWith.join(', ')) +
                '. Past runs keep the configuration they ran with.</div>';
        }
        return '<div class="notice">' + esc(what) + ' belongs to the run settings <span class="mono">' +
            esc(RUN_SETTINGS.name) + '</span>, used by this report alone. A change here reaches no other report, ' +
            'and past runs keep the configuration they ran with.</div>';
    }

    // The books the report reads under its scope and its type.
    function reach() {
        var line = RUN_SETTINGS.scopeLines[0];
        var read = S.type === 'Trading' ? line.trading : S.type === 'Banking' ? line.banking : line.books;
        return { all: line.books, trading: line.trading, banking: line.banking, read: read };
    }

    // ------------------------------------------------------------- sections

    function typeMeaning() {
        if (S.type === 'Trading') {
            return 'The report reads trading books only. A scope that names a banking book is refused, and a portfolio in the scope is read for its trading books.';
        }
        if (S.type === 'Banking') {
            return 'The report reads banking books only. A scope that names a trading book is refused, and a portfolio in the scope is read for its banking books.';
        }
        return 'Empty means the report reads books of every type. A type limits it to the books of that regulatory book type.';
    }

    function definitionSection() {
        var d = DEFINITION;
        var edit = canWrite()
            ? '<button class="btn small" data-act="edit">Edit</button>'
            : '<span class="tag muted">Read only</span>';
        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Definition</h2><div class="meta">' + edit + '</div></div>' +
            '<div class="statgrid">' +
            field('Name', esc(d.name) + ' <span class="faint mono">' + d.id + '</span>') +
            field('Party', esc(PARTY)) +
            field('Report type', 'Risk report') +
            field('Schedule', esc(d.schedule) + ' <span class="faint mono">' + esc(d.cron) + '</span>') +
            field('Concurrency', esc(d.concurrency)) +
            field('Official', d.official ? 'Yes' : 'No') +
            field('Description', esc(d.description)) +
            field('Version', String(d.version)) +
            field('Report currency', currencyCell()) +
            field('Market data', '<span class="tag accent">' + esc(mdWord()) + '</span>') +
            field('Book type', '<span class="tag ' + (S.type === 'none' ? 'muted' : 'accent') + '">' + esc(typeWord(S.type)) + '</span>') +
            '</div>' +
            '</section>' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Latest run</h2><div class="meta"><a class="btn small linklike" href="../read-what-a-run-produced/">Open the run</a></div></div>' +
            '<div class="runs"><div class="runrow"><span class="rid">' + d.latest.id + '</span>' +
            '<span class="rat">' + d.latest.at + '</span>' +
            '<span class="rstate">' + d.latest.state + ' in ' + d.latest.took + ', ' + d.latest.trades +
            ' trades <span class="tag ok">Completed</span></span></div>' +
            '<div class="runrow"><span class="rid">market data</span><span class="rat">as of</span><span class="rstate">' + esc(asOf()) + '</span></div></div>' +
            '</section>';
    }

    function scopeSection() {
        var rs = RUN_SETTINGS;
        var r = reach();
        var named = S.hand === 'yes';
        var rows = rs.scopeLines.map(function (l) {
            return '<tr class="' + (named ? 'unreachable' : '') + '"><td>' + esc(l.name) + '</td><td>' + l.kind + ' with its subtree</td>' +
                '<td class="num">' + (named ? 'not read' : l.books + ' books') + '</td>' +
                '<td class="faint">' + (named
                    ? 'a book named by hand replaces the portfolio lines'
                    : l.trading + ' trading, ' + l.banking + ' banking') + '</td></tr>';
        }).join('');
        if (named) {
            rows += '<tr><td>' + esc(rs.handBook.name) + '</td><td>book, named by hand</td>' +
                '<td class="num">1 book</td><td class="faint">' + rs.handBook.type + '</td></tr>';
        }
        var reads = named
            ? 'This report reads the 1 book the scope names, ' + rs.handBook.name + ', a ' + rs.handBook.type + ' book.'
            : S.type === 'none'
                ? 'This report reads all ' + r.all + ' books.'
                : 'This report reads ' + r.read + ' of the ' + r.all + ' books: the ' +
                  (S.type === 'Trading' ? 'trading' : 'banking') + ' books only.';
        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Scope</h2><div class="meta">' +
            '<span class="tag accent">Editor proposed</span>' +
            (canWrite() ? '<button class="btn small" data-act="scope">Edit scope</button>' : '') + '</div></div>' +
            reachNote('The scope') +
            '<div class="table-wrap" style="margin-top:10px"><table><thead><tr><th>Scope line</th><th>Kind</th><th>Resolves to</th><th>Of which</th></tr></thead><tbody>' +
            rows + '</tbody></table></div>' +
            '<div class="notice ' + (S.type === 'none' ? '' : 'ok') + '" style="margin-top:10px">' + esc(reads) + '</div>' +
            '<div class="statgrid" style="margin-top:12px">' +
            field('Book type of the report', '<span class="tag ' + (S.type === 'none' ? 'muted' : 'accent') + '">' + esc(typeWord(S.type)) + '</span>') +
            '</div>' +
            '</section>';
    }

    function settingsTable(rows) {
        return '<div class="table-wrap"><table><tbody>' + rows.map(function (r) {
            return '<tr><td class="faint" style="width:14rem">' + esc(r[0]) + '</td><td>' + esc(r[1]) + '</td></tr>';
        }).join('') + '</tbody></table></div>';
    }

    // A setting the report leaves unset shows the engine default in a quiet tone.
    function engineGroups(groups) {
        return groups.map(function (g) {
            var rows = g.rows.map(function (r) {
                var value = r[2] === null
                    ? '<span class="faint">Engine default</span>'
                    : esc(r[2]);
                return '<tr><td style="width:15rem">' + esc(r[0]) + '</td><td class="code faint" style="width:17rem">' +
                    esc(r[1]) + '</td><td>' + value + '</td></tr>';
            }).join('');
            return '<h4 class="grouphead">' + esc(g.group) + '</h4>' +
                '<div class="table-wrap"><table><tbody>' + rows + '</tbody></table></div>';
        }).join('');
    }

    function analyticsSection() {
        var rs = RUN_SETTINGS;
        var rows = rs.analytics.map(function (a) {
            return '<tr class="' + (a.on ? '' : 'unreachable') + '"><td class="code">' + a.code + '</td>' +
                '<td class="wrap">' + esc(a.what) + '</td><td>' +
                (a.on ? '<span class="tag ok">On</span>' : '<span class="tag muted">Off</span>') +
                '</td><td class="num">' + a.order + '</td></tr>';
        }).join('');
        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Analytics</h2><div class="meta">' +
            '<a class="btn small linklike" href="../manage-the-analytics/">Manage the analytics</a></div></div>' +
            reachNote('The analytics') +
            '<div class="table-wrap" style="margin-top:10px"><table><thead><tr><th>Analytic</th><th>Produces</th><th>State</th><th>Order</th></tr></thead><tbody>' +
            rows + '</tbody></table></div>' +
            '<p class="rowfoot">1 of ' + rs.analytics.length + ' is on. An analytic is turned off by setting it inactive, never by deleting it.</p>' +
            '</section>' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Engine settings</h2><div class="meta"><span class="tag muted">Setup parameters of the engine</span></div></div>' +
            engineGroups(rs.engine) +
            '</section>';
    }

    function marketSection() {
        var rs = RUN_SETTINGS;
        var rows = SHARED.map(function (c) {
            var open = c.screen === 'prototype'
                ? '<a class="btn small linklike" href="' + c.href + '">Open (prototype)</a>'
                : '<span class="tag muted">No screen yet</span>';
            return '<tr><td class="typename">' + esc(c.type) + '</td><td class="code">' + esc(c.name) + '</td>' +
                '<td class="wrap">' + esc(c.what) + '</td><td class="num">' + plural(c.used, 'report') + '</td><td>' + open + '</td></tr>';
        }).join('');
        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Market configuration of the run</h2><div class="meta"><span class="tag muted">Part of the run settings</span></div></div>' +
            reachNote('The pricing market') +
            '<div style="margin-top:10px">' + settingsTable(rs.market) + '</div>' +
            '</section>' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Configurations of the party</h2><div class="meta"><span class="tag muted">Shared by every report of the party</span></div></div>' +
            '<div class="notice warn">These three belong to the party. A change to one reaches all 22 reports of ' + esc(PARTY) + '.</div>' +
            '<div class="table-wrap" style="margin-top:10px"><table><thead><tr><th>Kind</th><th>Name</th><th>Supplies</th><th>Read by</th><th></th></tr></thead><tbody>' +
            rows + '</tbody></table></div>' +
            '<p class="rowfoot">Today the page lists them and opens none: the application has a read-only page for a run settings configuration and no page for these three.</p>' +
            '</section>';
    }

    function kindsTable(limit) {
        var shown = S.showKinds ? KINDS : KINDS.slice(0, limit);
        var rows = shown.map(function (k) {
            return '<tr><td>' + esc(kindName(k[0])) + '</td><td class="num">' + number(k[1]) + '</td>' +
                '<td class="faint">All of them are read</td></tr>';
        }).join('');
        return '<div class="table-wrap"><table><thead><tr><th>Kind</th><th>Documents held</th><th>Which one counts</th></tr></thead><tbody>' +
            rows + '</tbody></table></div>' +
            '<p class="rowfoot"><button class="btn small ghost" data-act="kinds">' +
            (S.showKinds ? 'Show fewer kinds' : 'Show all ' + KINDS.length + ' kinds') + '</button></p>';
    }

    function conventionsSection() {
        var head = '<div class="sectionhead"><h2>Conventions</h2><div class="meta">' +
            '<span class="mono">HK conventions</span><span class="tag muted">Read by ' + plural(22, 'report') + '</span></div></div>';
        if (S.conv === 'today') {
            return '' +
                '<section class="card">' + head +
                '<div class="notice warn"><b>Today the binding chooses nothing.</b> The report binds a configuration called ' +
                '<span class="mono">HK conventions</span>, which is a name and holds no document. A run reads every convention ' +
                'document the party can see: <b>' + number(TOTAL_DOCUMENTS) + ' documents in ' + KINDS.length + ' kinds</b>. ' +
                'No document is marked as the one that counts, so a kind with ' + number(KINDS[0][1]) +
                ' documents is read whole.</div>' +
                '<div class="sectionhead" style="margin-top:12px"><h3>What a run reads</h3><div class="meta">' +
                '<span class="tag muted">Measured, 2026-10-21</span></div></div>' +
                kindsTable(8) +
                '<div class="sectionhead" style="margin-top:14px"><h3>Change what the report reads</h3><div class="meta">' +
                '<span class="tag warn">Not possible today</span></div></div>' +
                '<p class="dim" style="font-size:13px;max-width:90ch">Nothing on this page can change the terms. A document is changed one kind at a time, ' +
                'for the whole party, on a screen that is not this one, and a report cannot pick a different document. ' +
                'Choosing a set is the proposal: switch the bar to <b>With sets</b> to see it.</p>' +
                '<p class="rowfoot"><a href="../../conventions/see-the-conventions-a-party-uses/">See the conventions a party uses</a> · ' +
                '<a href="../../conventions/find-a-convention-by-its-terms/">Find a convention by its terms</a></p>' +
                '</section>';
        }
        var sets = SETS.map(function (s) {
            var chosen = S.chosen === s.id;
            return '<label class="setrow' + (chosen ? ' chosen' : '') + '">' +
                '<input type="radio" name="set" value="' + s.id + '" data-act="choose"' + (chosen ? ' checked' : '') +
                (canWrite() ? '' : ' disabled') + '>' +
                '<span class="setname">' + esc(s.name) + '</span>' +
                '<span class="setpurpose">' + esc(s.purpose) + '</span>' +
                '<span class="setmeta">' + s.kinds + ' of ' + KINDS.length + ' kinds · ' + plural(s.used, 'report') +
                (s.current ? ' · <b>current</b>' : '') + '</span></label>';
        }).join('');
        var picked = SETS.filter(function (s) { return s.id === S.chosen; })[0];
        var effect = '';
        if (picked && picked.id !== 'none') {
            var missing = KINDS.length - picked.kinds;
            effect = '<div class="reach" style="margin-top:12px">' +
                '<div class="does"><h3>What the choice does</h3><ul>' +
                '<li>The report reads ' + picked.documents + ' documents instead of ' + number(TOTAL_DOCUMENTS) + '.</li>' +
                '<li>Only this report changes: a set is chosen for each report.</li>' +
                '<li>Past runs keep the terms they read.</li></ul></div>' +
                '<div class="not"><h3>What needs a decision</h3><ul>' +
                (missing > 0
                    ? '<li>' + missing + ' kinds have no document in this set. A run that needs one of them fails with a message that names the kind.</li>'
                    : '<li>The set covers every kind.</li>') +
                '<li>Market Risk decides the change. The person who raises it cannot decide it.</li></ul></div></div>';
        }
        var action = S.requested
            ? '<div class="notice ok" style="margin-top:12px"><b>Request raised.</b> Market Risk decides it. This report keeps reading ' +
              'the current terms until it is approved. It is under <a href="#">My requests</a>.</div>'
            : '<div class="cellgroup" style="margin-top:12px">' +
              '<button class="btn primary" data-act="request"' + (picked && picked.id !== 'none' && canWrite() ? '' : ' disabled') + '>Ask for approval</button>' +
              (canWrite() ? '' : '<span class="hint">You may read this page and may not change the conventions.</span>') + '</div>';
        return '' +
            '<section class="card">' + head +
            '<div class="notice"><b>Proposed.</b> A convention set groups one document of each kind under a name and a purpose. ' +
            'The report chooses a set, and a run reads the documents of that set and no others. ' +
            'The binding then means something.</div>' +
            '<div class="sectionhead" style="margin-top:12px"><h3>Choose the set this report reads</h3><div class="meta">' +
            '<span class="tag accent">Fixture</span></div></div>' +
            '<div class="setlist">' + sets + '</div>' + effect + action +
            '<p class="rowfoot">Writing a set is its own screen, <a href="#">Name a convention set</a>, and is not drawn here. ' +
            'Changing one document of one kind is <a href="../../conventions/see-the-conventions-a-party-uses/">on the conventions screen</a>.</p>' +
            '</section>';
    }

    // The history tab is the shared Timeline of the entity screen specification:
    // an event chart over time, the legend with the system activity toggle, and a
    // rail of entries newest first under day headings. Each entry is one header
    // line, its note on a line of its own, and its detail collapsed.
    function initials(who) {
        return PEOPLE[who] ? PEOPLE[who].initials : who.slice(0, 2).toUpperCase();
    }

    function shownEntries() {
        return ENTRIES.filter(function (e) { return !(S.hideSystem && PEOPLE[e.actor].service); });
    }

    function chartOf(list) {
        if (!list.length) { return ''; }
        var HOUR = 3600000;
        var times = list.map(function (e) { return new Date(e.at).getTime(); });
        var lo = Math.floor(Math.min.apply(null, times) / HOUR) * HOUR;
        var hi = Math.max.apply(null, times);
        var bars = '';
        for (var b = lo; b <= hi; b += HOUR) {
            var inBucket = list.filter(function (e) {
                var t = new Date(e.at).getTime();
                return t >= b && t < b + HOUR;
            });
            var counts = {};
            inBucket.forEach(function (e) { counts[e.kind] = (counts[e.kind] || 0) + 1; });
            var segs = Object.keys(counts).map(function (k) {
                return '<i class="k-' + k + '" style="height:' + (counts[k] * 18) + 'px" title="' + KINDS_OF_ENTRY[k] + ': ' + counts[k] + '"></i>';
            }).join('');
            var label = new Date(b).toISOString().slice(0, 13).replace('T', ' ') + ':00';
            bars += '<div class="bar" data-act="jump" data-time="' + (b + HOUR / 2) + '" title="' +
                (inBucket.length ? label + ': ' + inBucket.length : '') + '">' + segs + '</div>';
        }
        var first = list[list.length - 1].at.slice(0, 16).replace('T', ' ');
        var last = list[0].at.slice(0, 16).replace('T', ' ');
        return '<div class="chart" role="img" aria-label="Events over time">' + bars + '</div>' +
            '<div class="axis"><span>' + first + '</span><span>' + last + '</span></div>';
    }

    function entryHtml(e) {
        var who = PEOPLE[e.actor];
        var diffs = e.diffs && e.diffs.length
            ? '<details class="det"><summary>Details</summary><div class="diff">' + e.diffs.map(function (x) {
                return (x[1] !== '' ? '<div class="dl old"><span class="sign">\u2212</span><span class="fname">' + esc(x[0]) + '</span><span>' + esc(x[1]) + '</span></div>' : '') +
                    '<div class="dl new"><span class="sign">+</span><span class="fname">' + esc(x[0]) + '</span><span>' + esc(x[2]) + '</span></div>';
            }).join('') + '</div></details>'
            : '<span class="faint small">No changes</span>';
        var revert = e.revertable && !e.current && canWrite()
            ? '<button class="btn small" data-act="revertopen" data-entry="' + e.id + '">Revert</button>'
            : '';
        return '<div class="entry' + (e.current ? ' cur' : '') + (S.flash === e.id ? ' flash' : '') + '" data-entry="' + e.id + '" data-at="' + new Date(e.at).getTime() + '">' +
            '<div class="at" title="' + e.at.replace('T', ' ') + '">' + e.at.slice(11, 19) + '</div>' +
            '<div class="dot"><i title="' + (e.current ? 'Current version' : '') + '"></i></div>' +
            '<div class="main"><span class="avatar' + (who.service ? ' service' : '') + '" title="' + esc(who.name) + '">' + initials(e.actor) + '</span>' +
            '<div><div class="line1"><span class="chip ' + (e.kind === 'raised' ? 'acc' : 'warn') + '">' + KINDS_OF_ENTRY[e.kind] + '</span>' +
            '<b>' + esc(e.record) + '</b><span class="chip v">v' + e.version + '</span>' +
            '<span>' + (who.service ? esc(who.name) : '<a href="#">' + esc(who.name) + '</a>') + '</span>' +
            '<span class="faint small">' + esc(e.reason) + '</span>' + revert + '</div>' +
            '<div class="note">' + esc(e.note) + '</div>' + diffs + '</div></div></div>';
    }

    function historySection() {
        var list = shownEntries().slice().sort(function (a, b) { return b.at.localeCompare(a.at); });
        var kinds = {};
        list.forEach(function (e) { kinds[e.kind] = (kinds[e.kind] || 0) + 1; });
        var legend = '<div class="legend">' + Object.keys(kinds).map(function (k) {
            return '<span><i class="k-' + k + '"></i>' + KINDS_OF_ENTRY[k] + ' ' + kinds[k] + '</span>';
        }).join('') + '<span class="sp" style="flex:1"></span>' +
            '<button class="btn small ghost" data-act="hidesys">' + (S.hideSystem ? 'Show system activity' : 'Hide system activity') + '</button></div>';
        var rail = '';
        var lastDay = '';
        list.forEach(function (e) {
            var day = e.at.slice(0, 10);
            if (day !== lastDay) {
                rail += '<div class="day">' + new Date(day).toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' }) + '</div>';
                lastDay = day;
            }
            rail += entryHtml(e);
        });
        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>History</h2><div class="meta"><span class="tag muted">The shared timeline</span></div></div>' +
            (S.reverted ? '<div class="notice ok" style="margin-bottom:10px"><b>Reverted.</b> The values of ' + esc(S.reverted) +
                ' were written back as a new version of the definition. No version was lost.</div>' : '') +
            '<p class="hint" style="margin-bottom:10px">One rail for the definition and for the configurations it binds. Each entry names its record. ' +
            'The configurations are the proposed addition: the definition\u2019s own history does not show that a figure changed because a configuration did.</p>' +
            chartOf(list) + legend + '<div class="rail">' + rail + '</div>' +
            '<p class="rowfoot">Which version of a configuration a run used is not recorded on the run, so the rail cannot say which runs a change reached.</p>' +
            '</section>';
    }

    function revertDialog() {
        if (S.revertOf === null) { return ''; }
        var e = ENTRIES.filter(function (x) { return x.id === S.revertOf; })[0];
        return '<div class="modal"><div class="modalbox card">' +
            '<div class="sectionhead"><h2>Revert to v' + e.version + '</h2></div>' +
            '<p style="font-size:13px">The values of version ' + e.version + ' of <b>' + esc(e.record) + '</b> are written back as a new version, v4. ' +
            'No version is lost.</p>' +
            '<label class="formrow" style="margin-top:10px"><span class="lbl">Reason</span><select>' +
            '<option>common.rectification \u2014 Rectification</option><option>common.non_material_update \u2014 Non-material update</option></select></label>' +
            '<label class="formrow"><span class="lbl">Comment</span><input type="text" value="Back to the schedule before the close moved" style="background:var(--bg);color:var(--ink);border:1px solid var(--line);border-radius:6px;padding:6px 8px;font:inherit"></label>' +
            '<p class="hint">The write is made against the current version, so a record somebody else changed in between is refused.</p>' +
            '<div class="cellgroup" style="margin-top:12px"><button class="btn primary" data-act="revertdo">Revert</button>' +
            '<button class="btn ghost" data-act="revertcancel">Cancel</button></div></div></div>';
    }

    // ------------------------------------------------------------- layout

    function bodyFor(tab) {
        switch (tab) {
            case 'definition': return definitionSection();
            case 'scope': return scopeSection();
            case 'analytics': return analyticsSection();
            case 'market': return marketSection();
            case 'conventions': return conventionsSection();
            case 'history': return historySection();
        }
        return definitionSection();
    }

    function layout() {
        if (!TABS.some(function (t) { return t[0] === S.tab; })) { S.tab = 'definition'; }
        return '<nav class="tabs" role="tablist">' + TABS.map(function (t) {
            return '<a href="#" role="tab" data-act="tab" data-tab="' + t[0] + '" class="' + (S.tab === t[0] ? 'on' : '') + '">' + t[1] + '</a>';
        }).join('') + '</nav>' + bodyFor(S.tab);
    }

    function dialog() {
        if (!S.dialog) { return ''; }
        var cur = S.dialogType === null ? S.type : S.dialogType;
        var curMd = S.dialogMd === null ? S.md : S.dialogMd;
        var mdOpts = [['eod', 'End of day'], ['intraday', 'Intraday']].map(function (o) {
            return '<option value="' + o[0] + '"' + (curMd === o[0] ? ' selected' : '') + '>' + o[1] + '</option>';
        }).join('');
        var opts = [['none', 'Every type (empty)'], ['Trading', 'Trading book'], ['Banking', 'Banking book']].map(function (o) {
            return '<option value="' + o[0] + '"' + (cur === o[0] ? ' selected' : '') + '>' + o[1] + '</option>';
        }).join('');
        return '<div class="modal"><div class="modalbox card">' +
            '<div class="sectionhead"><h2>Edit NPV</h2></div>' +
            '<label class="formrow"><span class="lbl">Market data</span><select data-act="dialogmd">' + mdOpts + '</select></label>' +
            '<p class="hint">' + esc(mdMeaning()) + '</p>' +
            '<label class="formrow" style="margin-top:8px"><span class="lbl">Book type</span><select data-act="dialogtype">' + opts + '</select></label>' +
            '<p class="hint">' + esc(cur === 'Trading'
                ? 'The report reads trading books only.'
                : cur === 'Banking' ? 'The report reads banking books only.' : 'The report reads books of every type.') + '</p>' +
            (S.dialogError ? '<div class="notice err" style="margin-top:10px">' + esc(S.dialogError) + '</div>' : '') +
            '<div class="cellgroup" style="margin-top:12px"><button class="btn primary" data-act="save">Save</button>' +
            '<button class="btn ghost" data-act="cancel">Cancel</button></div>' +
            '</div></div>';
    }

    // ----------------------------------------------------------------- page

    function gaps() {
        return '<section class="card gaps"><div class="sectionhead"><h2>What this prototype cannot show</h2></div><dl>' +
            '<dt>Convention sets do not exist yet</dt><dd>The conventions of a party are 26 tables of documents. A set, with a name and a purpose and one document for each kind, is not stored anywhere. The sets drawn here are fixtures.</dd>' +
            '<dt>A run ignores the conventions binding</dt><dd>The packaging reads every convention row of the party. Choosing a set therefore needs the packaging to read the set, and that is a change to the run, not only a screen.</dd>' +
            '<dt>The market data kind is held by the run settings</dt><dd>The run settings store market_data_type, with the values eod and date, and a date. The page puts the kind on the definition, as end of day or intraday, and shows no date: the date of a run is the run\u2019s own. How a run finds its time point is not drawn.</dd>' +
            '<dt>The currency is held by the run settings</dt><dd>The column is called base_currency and belongs to the run settings configuration, which several reports can share. The page calls it the report currency, because a report has one.</dd>' +
            '<dt>Three configurations have no screen</dt><dd>Pricing engines have a prototype and no page in the application. Today’s market and the curve configuration have neither.</dd>' +
            '<dt>The scope has no editor</dt><dd>The scope of a run settings configuration is written by the seed and the shell. The button here is the proposal.</dd>' +
            '</dl></section>';
    }

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype · mock data · ' + S.tab + ' · conventions ' + S.conv;
        var d = DEFINITION;
        document.getElementById('app').innerHTML = '' +
            '<div class="shell"><header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="#">Home</a><a class="here" href="#">Reporting</a><a href="#">Trading</a><a href="#">Reference data</a></nav>' +
            '<span class="modechip">' + esc(PARTY) + '</span></div></header>' +
            '<main>' +
            '<div class="crumbs"><span>Home</span><span>/</span><span>Reporting</span><span>/</span><span>Reports</span><span>/</span><span>' + esc(d.name) + '</span></div>' +
            '<div class="pagehead"><div><h1>' + esc(d.name) + '</h1>' +
            '<p class="lede">Risk report of ' + esc(PARTY) + ' · <span class="mono">' + d.id + '</span> · version ' + d.version +
            ' · <span class="tag ok">' + d.health + '</span></p></div></div>' +
            layout() + gaps() + '</main></div>' + dialog() + revertDialog();
        renderBar();
        bind();
    }

    function renderBar() {
        function group(label, key, opts) {
            return '<span class="label"><b>' + label + '</b></span>' + opts.map(function (o) {
                return '<button data-set="' + key + '" data-value="' + o[0] + '" class="' + (S[key] === o[0] ? 'on' : '') + '">' + o[1] + '</button>';
            }).join('');
        }
        document.getElementById('proto-bar').innerHTML = '' +
            '<div class="bar-line"><span class="tag-prototype">Prototype</span>' +
            group('Conventions', 'conv', [['today', 'Today'], ['sets', 'With sets']]) +
            '<span class="sep">|</span>' +
            group('Book type', 'type', [['none', 'None'], ['Trading', 'Trading'], ['Banking', 'Banking']]) +
            '<span class="sep">|</span>' +
            group('Scope', 'hand', [['no', 'Portfolio only'], ['yes', 'Names a trading book']]) + '</div>' +
            '<div class="bar-line">' +
            group('Market data', 'md', [['eod', 'End of day'], ['intraday', 'Intraday']]) +
            '<span class="sep">|</span>' +
            group('Run settings', 'shared', [['no', 'This report only'], ['yes', 'Shared']]) +
            '<span class="sep">|</span>' +
            group('Role', 'role', [['admin', 'Administrator'], ['reader', 'Reader']]) +
            '<span class="sep">|</span><button data-act="state" class="' + (S.showState ? 'on' : '') + '">State</button></div>' +
            (S.showState ? '<div class="state-panel"><div class="state-grid">' +
                ['tab', 'conv', 'type', 'hand', 'shared', 'md', 'role'].map(function (k) {
                    return '<div>' + k + ' <b>' + esc(S[k]) + '</b></div>';
                }).join('') +
                '</div><div class="state-note">The convention counts are measured on 2026-10-21 (' + number(TOTAL_DOCUMENTS) +
                ' documents in ' + KINDS.length + ' kinds). Everything else is a fixture.</div></div>' : '');
    }

    // --------------------------------------------------------------- events

    function bind() {
        var app = document.getElementById('app');
        app.onclick = function (e) {
            var t = e.target.closest('[data-act]');
            if (!t) { return; }
            var act = t.getAttribute('data-act');
            if (act === 'tab') { e.preventDefault(); S.tab = t.getAttribute('data-tab'); sync(); render(); }
            else if (act === 'edit') { S.dialog = true; S.dialogType = null; S.dialogMd = null; S.dialogError = null; render(); }
            else if (act === 'cancel') { S.dialog = false; render(); }
            else if (act === 'save') { save(); }
            else if (act === 'kinds') { S.showKinds = !S.showKinds; render(); }
            else if (act === 'request') { S.requested = true; render(); }
            else if (act === 'scope') { window.alert('A new screen: not built. Proposed.'); }
            else if (act === 'hidesys') { S.hideSystem = !S.hideSystem; render(); }
            else if (act === 'revertopen') { S.revertOf = t.getAttribute('data-entry'); render(); }
            else if (act === 'revertcancel') { S.revertOf = null; render(); }
            else if (act === 'revertdo') {
                var e = ENTRIES.filter(function (x) { return x.id === S.revertOf; })[0];
                S.reverted = e.record + ' v' + e.version;
                S.revertOf = null;
                render();
            }
            else if (act === 'jump') {
                var mid = Number(t.getAttribute('data-time'));
                var near = shownEntries().slice().sort(function (a, b) {
                    return Math.abs(new Date(a.at).getTime() - mid) - Math.abs(new Date(b.at).getTime() - mid);
                })[0];
                if (near) {
                    var el = document.querySelector('[data-entry="' + near.id + '"]');
                    if (el) { el.scrollIntoView({ block: 'center' }); el.classList.add('flash'); window.setTimeout(function () { el.classList.remove('flash'); }, 1500); }
                }
            }
        };
        app.onchange = function (e) {
            var t = e.target;
            if (t.getAttribute('data-act') === 'choose') { S.chosen = t.value; S.requested = false; render(); }
            if (t.getAttribute('data-act') === 'dialogtype') { S.dialogType = t.value; S.dialogError = null; render(); }
            if (t.getAttribute('data-act') === 'dialogmd') { S.dialogMd = t.value; render(); }
        };
        var bar = document.getElementById('proto-bar');
        bar.onclick = function (e) {
            var t = e.target.closest('button');
            if (!t) { return; }
            if (t.getAttribute('data-act') === 'state') { S.showState = !S.showState; renderBar(); bind(); return; }
            var key = t.getAttribute('data-set');
            if (key) { S[key] = t.getAttribute('data-value'); S.requested = false; sync(); render(); }
        };
    }

    // The rule the server applies, here with the wording the screen should give.
    function save() {
        var next = S.dialogType === null ? S.type : S.dialogType;
        if (next === 'Banking' && S.hand === 'yes') {
            S.dialogError = 'Refused. The scope of "' + RUN_SETTINGS.name + '" names the book "' + RUN_SETTINGS.handBook.name +
                '", a Trading book, and NPV would read Banking books only. Remove the book from the scope, or keep another type.';
            render();
            return;
        }
        S.type = next;
        if (S.dialogMd !== null) { S.md = S.dialogMd; }
        S.dialogMd = null;
        S.dialog = false;
        sync();
        render();
    }

    render();
})();

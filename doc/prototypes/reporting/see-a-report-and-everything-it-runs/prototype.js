/* A report and everything it runs. Self-contained: plain JavaScript, mock data,
 * no framework, no build step, and nothing that outlives the page.
 *
 * One page for one report definition. It answers three questions, and each is a
 * section of the page: what the report is (the definition), what it runs with
 * (the configuration) and which market terms it reads (the conventions). Three
 * layouts arrange the same sections:
 *
 *   A  three tabs: Definition, Configuration, Conventions. The proposal.
 *   B  four tabs: the configuration split into run settings and market and pricing.
 *   C  one page, an index at the top, no tabs.
 *
 * The sections are the same in every layout. Only the arrangement differs, so a
 * reader compares how much has to be held in the head at once and not what is
 * drawn.
 *
 * What is the system as it is, and what is proposed, is marked on the page with
 * the words "today" and "proposed". The measured figures are the rows of the 26
 * convention tables of the bright_faraday development database on 2026-10-21;
 * every other name, set and purpose is a fixture.
 *
 *   ?variant=A|B|C  ?tab=definition|configuration|runsettings|shared|conventions
 *   ?conv=today|sets  ?type=none|Trading|Banking  ?hand=no|yes  ?shared=no|yes
 *   ?role=admin|reader
 */

(function () {
    'use strict';

    // ------------------------------------------------------------ mock data

    var PARTY = 'ACME Corporation HK Ltd';

    var DEFINITION = {
        id: 'daf18b08',
        name: 'NPV',
        description: 'Net present value of every trade in the entity, in the entity currency.',
        reportType: 'risk',
        schedule: 'At 18:30, Monday through Friday',
        cron: '30 18 * * 1-5',
        concurrency: 'skip',
        official: true,
        preProcessing: 'execute',
        postProcessing: 'execute',
        version: 3,
        health: 'Completed',
        latest: { id: 'a2e6c04d', at: '2026-10-21 17:05', took: '9 s', trades: 8, state: 'Completed' }
    };

    var RUN_SETTINGS = {
        name: 'Valuation - Global Portfolio',
        currency: 'HKD',
        analytics: [
            { code: 'npv', what: 'Net present value of every trade', on: true, order: 1 },
            { code: 'cashflow', what: 'The cash flows of every trade', on: false, order: 2 },
            { code: 'sensitivity', what: 'Bump and revalue sensitivities', on: false, order: 3 },
            { code: 'simulation', what: 'The Monte Carlo cube', on: false, order: 4 },
            { code: 'pfe', what: 'Potential future exposure, reads the cube', on: false, order: 5 },
            { code: 'xva', what: 'CVA, DVA and FVA', on: false, order: 6 }
        ],
        sharedWith: ['Cashflows', 'Intraday Risk Monitor'],
        scopeLines: [{ name: 'Global Portfolio', kind: 'portfolio', books: 8, trading: 6, banking: 2 }],
        handBook: { name: 'Rates Desk 1', kind: 'book', type: 'Trading' }
    };

    var SHARED = [
        {
            type: 'Pricing engines',
            name: 'HK pricing engines',
            what: 'Which model prices each product',
            used: 22,
            screen: 'prototype',
            href: '../see-the-pricing-engines-a-party-holds/'
        },
        {
            type: "Today's market",
            name: "HK today's market",
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
        {
            id: 'none',
            name: 'Every document (today)',
            purpose: 'No set. A run reads all the documents the party holds.',
            kinds: 26,
            documents: 0,
            used: 22,
            current: true
        },
        {
            id: 'set-1',
            name: 'ACME HK desk default',
            purpose: 'The terms the Hong Kong desks book and are marked on.',
            kinds: 26,
            documents: 26,
            used: 18,
            current: false
        },
        {
            id: 'set-2',
            name: 'Dubai swaps desk',
            purpose: 'AED swaps on ACT/360 with the desk fixing calendar.',
            kinds: 9,
            documents: 9,
            used: 0,
            current: false
        },
        {
            id: 'set-3',
            name: 'LIBOR fallback review',
            purpose: 'Fallback terms for the IBOR indices, for the review of old trades.',
            kinds: 4,
            documents: 4,
            used: 1,
            current: false
        }
    ];

    var TOTAL_DOCUMENTS = KINDS.reduce(function (n, k) { return n + k[1]; }, 0);
    SETS[0].documents = TOTAL_DOCUMENTS;

    // --------------------------------------------------------------- state

    function query(name, fallback) {
        var m = new RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
        return m ? decodeURIComponent(m[1]) : fallback;
    }

    var S = {
        variant: query('variant', 'A'),
        tab: query('tab', 'definition'),
        conv: query('conv', 'today'),
        type: query('type', 'none'),
        hand: query('hand', 'no'),
        shared: query('shared', 'no'),
        role: query('role', 'admin'),
        chosen: 'none',
        requested: false,
        dialog: false,
        dialogType: null,
        dialogError: null,
        showKinds: false,
        showState: false
    };

    function sync() {
        var p = [];
        ['variant', 'tab', 'conv', 'type', 'hand', 'shared', 'role'].forEach(function (k) {
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

    function typeWord(type) {
        return type === 'Trading' ? 'Trading book' : type === 'Banking' ? 'Banking book' : 'Every type';
    }

    function canWrite() {
        return S.role === 'admin';
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
            '<section class="card" id="sec-definition">' +
            '<div class="sectionhead"><h2>Definition</h2><div class="meta">' + edit + '</div></div>' +
            '<div class="statgrid">' +
            field('Name', esc(d.name) + ' <span class="faint mono">' + d.id + '</span>') +
            field('Party', esc(PARTY)) +
            field('Report type', 'Risk report') +
            field('Schedule', esc(d.schedule) + ' <span class="faint mono">' + esc(d.cron) + '</span>') +
            field('Concurrency', 'Skip') +
            field('Official', d.official ? 'Yes' : 'No') +
            field('Book type', '<span class="tag ' + (S.type === 'none' ? 'muted' : 'accent') + '">' +
                esc(typeWord(S.type)) + '</span>') +
            field('Processing', 'Execute, then execute') +
            '</div>' +
            '<p class="hint" style="margin-top:10px">' + esc(typeMeaning()) + '</p>' +
            '<p class="hint" style="margin-top:6px">' + esc(d.description) + '</p>' +
            '<p class="rowfoot">A change on this section reaches this report only. Version ' + d.version + '.</p>' +
            '</section>' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Latest run</h2><div class="meta"><a class="btn small linklike" href="../read-what-a-run-produced/">Open the run</a></div></div>' +
            '<div class="runs"><div class="runrow"><span class="rid">' + d.latest.id + '</span>' +
            '<span class="rat">' + d.latest.at + '</span>' +
            '<span class="rstate">' + d.latest.state + ' in ' + d.latest.took + ', ' + d.latest.trades +
            ' trades <span class="tag ok">Completed</span></span></div></div>' +
            '</section>';
    }

    function field(label, value) {
        return '<div><span class="lbl">' + label + '</span><span class="val">' + value + '</span></div>';
    }

    function runSettingsSection() {
        var rs = RUN_SETTINGS;
        var r = reach();
        var shared = S.shared === 'yes';
        var rows = rs.analytics.map(function (a) {
            return '<tr class="' + (a.on ? '' : 'unreachable') + '"><td class="code">' + a.code + '</td>' +
                '<td class="wrap">' + esc(a.what) + '</td><td>' +
                (a.on ? '<span class="tag ok">On</span>' : '<span class="tag muted">Off</span>') +
                '</td><td class="num">' + a.order + '</td></tr>';
        }).join('');
        var named = S.hand === 'yes';
        var scope = rs.scopeLines.map(function (l) {
            return '<tr class="' + (named ? 'unreachable' : '') + '"><td>' + esc(l.name) + '</td><td>' + l.kind + ' with its subtree</td>' +
                '<td class="num">' + (named ? 'not read' : l.books + ' books') + '</td>' +
                '<td class="faint">' + (named
                    ? 'a book named by hand replaces the portfolio lines'
                    : l.trading + ' trading, ' + l.banking + ' banking') + '</td></tr>';
        }).join('');
        if (S.hand === 'yes') {
            scope += '<tr><td>' + esc(rs.handBook.name) + '</td><td>book, named by hand</td>' +
                '<td class="num">1 book</td><td class="faint">' + rs.handBook.type + '</td></tr>';
        }
        var reads = named
            ? 'This report reads the 1 book the scope names, ' + rs.handBook.name + ', a ' + rs.handBook.type + ' book.'
            : S.type === 'none'
                ? 'This report reads all ' + r.all + ' books.'
                : 'This report reads ' + r.read + ' of the ' + r.all + ' books: the ' +
                  (S.type === 'Trading' ? 'trading' : 'banking') + ' books only.';
        var reachNote = shared
            ? '<div class="notice warn"><b>Shared by ' + (rs.sharedWith.length + 1) + ' reports.</b> A change here reaches ' +
              'NPV, ' + esc(rs.sharedWith.join(', ')) + '. Past runs keep the configuration they ran with.</div>'
            : '<div class="notice">Used by this report alone. A change here reaches no other report, and past runs keep the configuration they ran with.</div>';
        return '' +
            '<section class="card" id="sec-runsettings">' +
            '<div class="sectionhead"><h2>Run settings</h2><div class="meta">' +
            '<span class="mono">' + esc(rs.name) + '</span>' +
            (shared ? '<span class="tag warn">Shared</span>' : '<span class="tag muted">This report only</span>') +
            '</div></div>' +
            reachNote +
            '<div class="statgrid" style="margin-top:10px">' +
            field('Base currency', rs.currency) +
            field('Analytics on', '1 of ' + rs.analytics.length) +
            '</div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>Analytics</h3><div class="meta">' +
            '<a class="btn small linklike" href="../manage-the-analytics/">Manage the analytics</a></div></div>' +
            '<div class="table-wrap"><table><thead><tr><th>Analytic</th><th>Produces</th><th>State</th><th>Order</th></tr></thead><tbody>' +
            rows + '</tbody></table></div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>Scope</h3><div class="meta">' +
            '<span class="tag accent">Proposed</span>' +
            (canWrite() ? '<button class="btn small" data-act="scope">Edit scope</button>' : '') +
            '</div></div>' +
            '<div class="table-wrap"><table><thead><tr><th>Scope line</th><th>Kind</th><th>Resolves to</th><th>Of which</th></tr></thead><tbody>' +
            scope + '</tbody></table></div>' +
            '<div class="notice ' + (S.type === 'none' ? '' : 'ok') + '" style="margin-top:10px">' + esc(reads) + '</div>' +
            '<p class="rowfoot">Editing the scope is a screen that does not exist today: the scope is set by the seed and the shell.</p>' +
            '</section>';
    }

    function sharedSection() {
        var rows = SHARED.map(function (c) {
            var open = c.screen === 'prototype'
                ? '<a class="btn small linklike" href="' + c.href + '">Open (prototype)</a>'
                : '<span class="tag muted">No screen yet</span>';
            return '<tr><td class="typename">' + esc(c.type) + '</td><td class="code">' + esc(c.name) + '</td>' +
                '<td class="wrap">' + esc(c.what) + '</td><td class="num">' + c.used + ' reports</td><td>' + open + '</td></tr>';
        }).join('');
        return '' +
            '<section class="card" id="sec-shared">' +
            '<div class="sectionhead"><h2>Market and pricing</h2><div class="meta"><span class="tag muted">Shared by every report of the party</span></div></div>' +
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
                '<td class="faint">' + (S.conv === 'today' ? 'All of them are read' : 'One, from the set') + '</td></tr>';
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
                '<section class="card" id="sec-conventions">' + head +
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
            '<section class="card" id="sec-conventions">' + head +
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

    // ------------------------------------------------------------- layouts

    function tabsFor() {
        if (S.variant === 'B') {
            return [['definition', 'Definition'], ['runsettings', 'Run settings'],
                ['shared', 'Market and pricing'], ['conventions', 'Conventions']];
        }
        return [['definition', 'Definition'], ['configuration', 'Configuration'], ['conventions', 'Conventions']];
    }

    function bodyFor(tab) {
        switch (tab) {
            case 'definition': return definitionSection();
            case 'configuration': return runSettingsSection() + sharedSection();
            case 'runsettings': return runSettingsSection();
            case 'shared': return sharedSection();
            case 'conventions': return conventionsSection();
        }
        return definitionSection();
    }

    function layout() {
        if (S.variant === 'C') {
            var idx = ['definition', 'runsettings', 'shared', 'conventions'];
            var names = { definition: 'Definition', runsettings: 'Run settings', shared: 'Market and pricing', conventions: 'Conventions' };
            return '<nav class="tabs index">' + idx.map(function (t) {
                return '<a href="#sec-' + t + '">' + names[t] + '</a>';
            }).join('') + '</nav>' + idx.map(bodyFor).join('');
        }
        var tabs = tabsFor();
        var valid = tabs.some(function (t) { return t[0] === S.tab; });
        if (!valid) { S.tab = 'definition'; }
        return '<nav class="tabs" role="tablist">' + tabs.map(function (t) {
            return '<a href="#" role="tab" data-act="tab" data-tab="' + t[0] + '" class="' + (S.tab === t[0] ? 'on' : '') + '">' + t[1] + '</a>';
        }).join('') + '</nav>' + bodyFor(S.tab);
    }

    function dialog() {
        if (!S.dialog) { return ''; }
        var cur = S.dialogType === null ? S.type : S.dialogType;
        var opts = [['none', 'Every type (empty)'], ['Trading', 'Trading book'], ['Banking', 'Banking book']].map(function (o) {
            return '<option value="' + o[0] + '"' + (cur === o[0] ? ' selected' : '') + '>' + o[1] + '</option>';
        }).join('');
        return '<div class="modal"><div class="modalbox card">' +
            '<div class="sectionhead"><h2>Edit NPV</h2></div>' +
            '<label class="formrow"><span class="lbl">Book type</span><select data-act="dialogtype">' + opts + '</select></label>' +
            '<p class="hint">' + esc(cur === 'Trading'
                ? 'The report reads trading books only.'
                : cur === 'Banking' ? 'The report reads banking books only.' : 'The report reads books of every type.') + '</p>' +
            (S.dialogError ? '<div class="notice err" style="margin-top:10px">' + esc(S.dialogError) + '</div>' : '') +
            '<div class="cellgroup" style="margin-top:12px"><button class="btn primary" data-act="save">Save</button>' +
            '<button class="btn ghost" data-act="cancel">Cancel</button></div>' +
            '<p class="rowfoot">The other fields of the definition are edited as they are today and are not drawn here. ' +
            'The refusal names the book and the report. Today the database says only which definition it refused.</p>' +
            '</div></div>';
    }

    // ----------------------------------------------------------------- page

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype · mock data · layout ' + S.variant + ' · conventions ' + S.conv;
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
            layout() + gaps() + '</main></div>' + dialog();
        renderBar();
        bind();
    }

    function gaps() {
        return '<section class="card gaps"><div class="sectionhead"><h2>What this prototype cannot show</h2></div><dl>' +
            '<dt>Convention sets do not exist yet</dt><dd>The conventions of a party are 26 tables of documents. A set, with a name and a purpose and one document for each kind, is not stored anywhere. The sets drawn here are fixtures.</dd>' +
            '<dt>A run ignores the conventions binding</dt><dd>The packaging reads every convention row of the party. Choosing a set therefore needs the packaging to read the set, and that is a change to the run, not only a screen.</dd>' +
            '<dt>Three configurations have no screen</dt><dd>Pricing engines have a prototype and no page in the application. Today\'s market and the curve configuration have neither.</dd>' +
            '<dt>The scope has no editor</dt><dd>The scope of a run settings configuration is written by the seed and the shell. The button here is the proposal.</dd>' +
            '</dl></section>';
    }

    function renderBar() {
        function group(label, key, opts) {
            return '<span class="label"><b>' + label + '</b></span>' + opts.map(function (o) {
                return '<button data-set="' + key + '" data-value="' + o[0] + '" class="' + (S[key] === o[0] ? 'on' : '') + '">' + o[1] + '</button>';
            }).join('');
        }
        document.getElementById('proto-bar').innerHTML = '' +
            '<div class="bar-line"><span class="tag-prototype">Prototype</span>' +
            group('Layout', 'variant', [['A', 'A · three tabs'], ['B', 'B · four tabs'], ['C', 'C · one page']]) +
            '<span class="sep">|</span>' +
            group('Conventions', 'conv', [['today', 'Today'], ['sets', 'With sets']]) +
            '<span class="sep">|</span>' +
            group('Book type', 'type', [['none', 'None'], ['Trading', 'Trading'], ['Banking', 'Banking']]) + '</div>' +
            '<div class="bar-line">' +
            group('Scope', 'hand', [['no', 'Portfolio only'], ['yes', 'Names a trading book']]) +
            '<span class="sep">|</span>' +
            group('Run settings', 'shared', [['no', 'This report only'], ['yes', 'Shared']]) +
            '<span class="sep">|</span>' +
            group('Role', 'role', [['admin', 'Administrator'], ['reader', 'Reader']]) +
            '<span class="sep">|</span><button data-act="state" class="' + (S.showState ? 'on' : '') + '">State</button></div>' +
            (S.showState ? '<div class="state-panel"><div class="state-grid">' +
                ['variant', 'tab', 'conv', 'type', 'hand', 'shared', 'role'].map(function (k) {
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
            else if (act === 'edit') { S.dialog = true; S.dialogType = null; S.dialogError = null; render(); }
            else if (act === 'cancel') { S.dialog = false; render(); }
            else if (act === 'save') { save(); }
            else if (act === 'kinds') { S.showKinds = !S.showKinds; render(); }
            else if (act === 'request') { S.requested = true; render(); }
            else if (act === 'scope') { window.alert('A new screen: not built. Proposed.'); }
        };
        app.onchange = function (e) {
            var t = e.target;
            if (t.getAttribute('data-act') === 'choose') { S.chosen = t.value; S.requested = false; render(); }
            if (t.getAttribute('data-act') === 'dialogtype') { S.dialogType = t.value; S.dialogError = null; render(); }
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
        S.dialog = false;
        sync();
        render();
    }

    render();
})();

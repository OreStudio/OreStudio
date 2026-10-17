/* Change what a report runs. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * The change what a report runs journey as journey_change_what_a_report_runs.org
 * states it: a market-risk owner or a tenant administrator opens a definition,
 * reads the five configurations it binds, reads the analytics its run settings
 * configuration carries, and reads what a change there does and does not reach.
 *
 * The prototype no longer draws the analytics. Manage the analytics owns them
 * (doc/prototypes/reporting/manage-the-analytics/): the switchboard, a step for
 * each analytic that is on, every parameter, the refused pfe and simulation
 * pair, and the read-back. This page keeps the wizard over the definition and
 * the configurations, one step that summarises the bound configuration's
 * analytics with the display_order and the incompatibility state, and a last
 * step that states the reach. Step 2 links to the shared screen.
 *
 * Three panels, not a rail: the standard steps UI, its marks and its step foot
 * belong to the shared prototype, and this journey is three questions about one
 * definition rather than a walk through a set.
 *
 *   ?step=1|2|3   which panel opens
 *
 * Nothing here reads a server and nothing is written. Every value on the page is
 * a fixture, and the page says so. Two statements on it are not fixture: an
 * analytic is turned off by setting active false and never by deleting the row,
 * and pfe cannot run without simulation, which the ACME seeder enforces at
 * check_exposure_needs_simulation and no service does.
 */

(function () {
    'use strict';

    // ------------------------------------------------------------ mock data

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };

    var DEFINITION = {
        id: 'rpt-8f31a0c4',
        name: 'Group Consolidated Exposure',
        reportType: 'risk'
    };

    var CONFIGURATION = { id: 'cfg-2d61f0a8', name: 'Group risk configuration' };

    var SHARED_WITH = ['Board Risk Dashboard', 'Group DV01 by book', 'Treasury Sensitivity'];

    /* The conventions set, for the read-only summary this step draws. Twenty-six
       kinds. ACME is provisioned: the tenant holds 10,991 convention rows and
       every kind is bound, 25 of them from a DQ publish. docs is the party's own
       documents; the four kinds whose table has no party column are held by the
       tenant and say so. */
    var CONVENTIONS_KINDS = [
        { name: 'Deposit', docs: 163, prov: 'dq' },
        { name: 'FRA', docs: 42, prov: 'dq' },
        { name: 'Future', docs: 7, prov: 'dq' },
        { name: 'OIS', docs: 52, prov: 'dq' },
        { name: 'Average OIS', docs: 5, prov: 'dq' },
        { name: 'Swap', docs: 320, prov: 'dq' },
        { name: 'Tenor basis swap', docs: 37, prov: 'dq' },
        { name: 'Tenor basis two swap', docs: 4, prov: 'dq' },
        { name: 'Cross-currency basis', docs: 207, prov: 'dq' },
        { name: 'Cross-currency fix-float', docs: 24, prov: 'dq' },
        { name: 'Bond yield', docs: 5, prov: 'dq' },
        { name: 'CDS', docs: 4, prov: 'dq' },
        { name: 'CMS spread option', docs: 1, prov: 'dq' },
        { name: 'Commodity forward', docs: 4, prov: 'dq' },
        { name: 'Commodity future', docs: 790, prov: 'dq' },
        { name: 'FX option', docs: 173, prov: 'dq' },
        { name: 'BMA basis swap', docs: 3, prov: 'dq' },
        { name: 'Zero', docs: 38, prov: 'dq' },
        { name: 'Inflation swap', docs: 22, prov: 'dq' },
        { name: 'Intraday power load', docs: 2, prov: 'dq' },
        { name: 'IBOR index', docs: 10, prov: 'dq', tenantWide: true },
        { name: 'Overnight index', docs: 23, prov: 'dq', tenantWide: true },
        { name: 'Named swap index', docs: 243, prov: 'dq' },
        { name: 'Currency pair', docs: 179, prov: 'pair', tenantWide: true },
        { name: 'Zero inflation index', docs: 9, prov: 'dq' },
        { name: 'Tenor', docs: 4, prov: 'system', tenantWide: true }
    ];

    /* A count a reader reads: 2155 is 2,155. */
    function num(value) {
        return String(value).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    /* Where the set came from, in one place: the two publish subjects, and the
       one kind no publish reaches. */
    var CONVENTIONS_PUBLISH = {
        dq: { subject: 'refdata.v1.ops.publish_conventions_from_dq',
              dataset: 'ORE Instrument Conventions' },
        pair: { subject: 'refdata.v1.ops.publish_currency_pair_conventions_from_dq',
                dataset: 'Standard Currency Pair Conventions' }
    };

    /* The five configuration types a definition binds. Only conventions has a
       screen in the application; PATHS.conventions is /refdata/conventions. The
       other four are stated as missing. The conventions row points at the shared
       prototype, which owns the interaction; the application route is named
       beside it, because it is still the only real route. */
    var CONFIG_TYPES = [
        {
            type: 'conventions',
            name: 'Group conventions',
            supplies: 'The set of 26 convention kinds the run reads. Every kind is bound: ' +
                '25 of them from a DQ publish and tenor copied from the system tenant. ' +
                'The terms, not the market data, the curves or the engines.',
            route: {
                path: '../manage-the-conventions/?from=change-what-a-report-runs',
                screen: 'Manage the conventions',
                hint: 'shared prototype \u00b7 the application route is PATHS.conventions, ' +
                    '/refdata/conventions'
            }
        },
        {
            type: 'pricing_engines',
            name: 'ACME pricing engines',
            supplies: 'Which engine prices which product family, and the settings each engine carries.'
        },
        {
            type: 'todays_market',
            name: 'London close 2026-10-16',
            supplies: 'The market data of the valuation date: curves, volatility surfaces and FX spots.'
        },
        {
            type: 'curve_configuration',
            name: 'ACME curve set',
            supplies: 'How each curve is built: its builder, its interpolation and its pillars.'
        },
        {
            type: 'risk_report_config',
            name: 'Group risk configuration',
            supplies: 'The analytics this report runs, their display order and their parameters.',
            shared: true
        }
    ];

    /* The fourteen analytic types, in the order the vocabulary lists them, with
       the one-line statement of what each produces and the parameter rows the
       configuration carries for it. What each analytic is, how it is calculated
       and every control its parameters take are owned by Manage the analytics. */
    var ANALYTICS = [
        { code: 'npv', name: 'Net present value',
          produces: 'One present value per trade, netted to book and to portfolio.',
          params: ['discount_curve', 'include_cashflows'] },
        { code: 'sensitivity', name: 'Sensitivity',
          produces: 'The change in value for a small shift in each market factor, such as DV01 per curve.',
          params: ['shift_size', 'shift_type'] },
        { code: 'cashflow', name: 'Cashflows',
          produces: 'The projected cashflow rows of every trade, bucketed by date.',
          params: ['bucket', 'include_fixings'] },
        { code: 'curves', name: 'Curves',
          produces: 'The built curves themselves, with their pillars and their discount factors.',
          params: [] },
        { code: 'simulation', name: 'Simulation',
          produces: 'Simulated future paths of the market, and the exposure cube pfe reads.',
          params: ['number_of_paths', 'seed', 'time_grid'] },
        { code: 'saccr', name: 'SA-CCR',
          produces: 'The standardised counterparty credit exposure of each netting set.',
          params: ['reporting_currency', 'include_collateral'] },
        { code: 'parametricVar', name: 'Parametric VaR',
          produces: 'One value at risk figure, computed from the sensitivities and a covariance matrix.',
          params: ['confidence', 'horizon_days', 'volatility_period_days'] },
        { code: 'portfolioDetails', name: 'Portfolio details',
          produces: 'How many trades and how much notional each book and portfolio carries.',
          params: ['grouping', 'include_empty_books'] },
        { code: 'creditcurves', name: 'Credit curves',
          produces: 'The survival probability and hazard rate curve of each counterparty.',
          params: ['hazard_rate_source', 'recovery_rate'] },
        { code: 'pfe', name: 'Potential future exposure',
          produces: 'An exposure profile per counterparty and date, and one figure at the horizon.',
          params: ['horizon', 'quantile', 'exposure_cube'] },
        { code: 'calibration', name: 'Calibration',
          produces: 'The fitted model parameters, and how far the fit missed the market.',
          params: ['model', 'max_iterations', 'tolerance'] },
        { code: 'pnlExplain', name: 'P&L explain',
          produces: 'Yesterday\u2019s profit and loss split by factor: rates, credit, FX, and the rest.',
          params: ['decomposition', 'threshold'] },
        { code: 'historicalSimulationVar', name: 'Historical simulation VaR',
          produces: 'One value at risk figure, read from how the book would have moved on past days.',
          params: ['confidence', 'lookback_days', 'holding_period_days', 'use_log_returns'] },
        { code: 'xva', name: 'XVA',
          produces: 'The valuation adjustments: CVA, DVA and the funding cost, per counterparty.',
          params: ['base_currency', 'methodology', 'include_cva', 'include_dva', 'netting_threshold'] }
    ];

    /* The configuration as stored today. Eight types carry a row and six carry
       none, so "on the configuration" and "on" are two different things. */
    var ROWS = [
        { code: 'npv', active: true, order: 1 },
        { code: 'sensitivity', active: true, order: 2 },
        { code: 'cashflow', active: false, order: 3 },
        { code: 'portfolioDetails', active: true, order: 4 },
        { code: 'simulation', active: true, order: 5 },
        { code: 'pfe', active: false, order: 6 },
        { code: 'saccr', active: false, order: 7 },
        { code: 'calibration', active: false, order: 8 }
    ];

    var VERSIONS = [
        { version: 6, at: '2026-09-30 09:12', note: 'the analytics as they are stored now' },
        { version: 5, at: '2026-08-14 16:40', note: 'the schedule change' },
        { version: 4, at: '2026-07-02 11:05', note: 'the first official release' }
    ];

    var NEXT_VERSION = 7;

    var RUNS = [
        { id: 'run 41c9f0a7', at: '2026-10-16 06:04', state: 'completed', errors: 0 },
        { id: 'run 5b02e6d3', at: '2026-10-15 06:03', state: 'completed', errors: 2 },
        { id: 'run 9d13b7e0', at: '2026-10-14 06:04', state: 'completed', errors: 0 }
    ];

    /* The three panels. */
    var STEPS = [
        { n: 1, title: 'The definition and the configurations it binds' },
        { n: 2, title: 'The analytics' },
        { n: 3, title: 'What a change reaches' }
    ];

    var NO_DELETE = 'An analytic is turned off by setting active to false, not by deleting the row. ' +
        'The row keeps its display_order and its parameters and reads as off, so the history shows ' +
        'the report used to produce it. No control anywhere removes a row.';

    var PFE_RULE = 'pfe reads a cube that simulation writes, so pfe without simulation cannot work. ' +
        'The rule lives in the ACME seeder, in generate_risk_config_data.py, function ' +
        'check_exposure_needs_simulation, and in no service. Nothing validates it when a ' +
        'configuration is written, so the server would accept the set. Manage the analytics refuses ' +
        'it itself, and a build would have to do the same or state the failure it cannot prevent.';

    var PAST_RUNS = 'A run keeps the configuration it ran with. Its outputs, its errors and its ' +
        'figures are that run\u2019s own record, and they do not change when this configuration ' +
        'changes. The runs already produced stay listed.';

    var INCOMPARABLE = 'A run made after a change is not comparable with one made before it. The two ' +
        'runs did not produce the same measures, so the difference between them would be a difference ' +
        'of configuration and not a difference of the book. The results screen compares a run with a ' +
        'previous run of the same definition and marks the comparison partial where a side carried ' +
        'errors; it cannot mark this one, because no operation says which configuration version a run ' +
        'used. This page places a run by its own start time against the definition\u2019s version ' +
        'history, and that is an inference, not a fact.';

    var SHARED = 'A configuration is shared. Three other definitions bind Group risk configuration, so ' +
        'the next run of each of them produces the changed set too, and none of them was opened here.';

    var NO_CONFIG_SCREEN = 'No screen exists for four of the five configuration types. Only ' +
        'conventions has a route, PATHS.conventions, which is /refdata/conventions. The other four ' +
        'are named here and stated as missing rather than drawn as working links.';

    var HANDED_OFF = 'The analytics are not drawn on this page. Manage the analytics owns them: the ' +
        'switchboard of every analytic type, a step for each one that is on, every parameter with a ' +
        'control chosen for its shape, the refused pfe and simulation pair, and the read-back. This ' +
        'page reads the set that screen leaves and states what a change to it reaches.';

    var GAPS = [
        {
            title: 'The analytics are managed on another screen',
            body: HANDED_OFF + ' That is a consolidation, not a gap in the journey: one screen owns ' +
                'the interaction and the two that used to draw it hand the work there. What it costs ' +
                'is a step out of this wizard, and the person returns to step 2 when the shared ' +
                'screen is done.'
        },
        {
            title: 'No screen exists for four of the five configuration types',
            body: 'Step 1 names the five configurations the definition binds and links each to the ' +
                'screen that holds its data. Only one such screen exists: conventions, at ' +
                'PATHS.conventions, /refdata/conventions. pricing_engines, todays_market, ' +
                'curve_configuration and risk_report_config have no screen in the application, and ' +
                'this page states that instead of drawing a link that would not resolve.'
        },
        {
            title: 'Nothing validates the pfe rule',
            body: 'pfe needs simulation, and that rule lives in the ACME seeder and nowhere else. ' +
                'reporting.v1.configuration_analytics.put_many accepts pfe without simulation, so the ' +
                'failure arrives in the run and not in the write. This page marks the pair on the ' +
                'summary and names the rule; a build cannot rely on the server marking either.'
        },
        {
            title: 'No operation says which runs used which configuration',
            body: 'A run keeps the configuration it ran with, and nothing reads that configuration ' +
                'back with the run. The page can only compare a run\u2019s start time with the ' +
                'definition\u2019s version history, which is a history of a different row: the ' +
                'analytics live on the configuration. "This run predates my change" is an inference ' +
                'here and nowhere a fact.'
        },
        {
            title: 'No read of a configuration\u2019s version history',
            body: 'A configuration carries a version, and no operation in this journey\u2019s set ' +
                'returns the versions of one configuration, or the version a definition bound at a ' +
                'time. report_definitions_versions reads the definition, and the analytics are not ' +
                'on the definition.'
        },
        {
            title: 'The comparison is marked partial for errors, not for a changed set',
            body: 'The results screen marks a comparison partial where a side carried errors. It ' +
                'cannot mark it invalid because the analytics changed, because nothing tells it that ' +
                'they did. A person reading across the change reads two different sets of measures ' +
                'side by side.'
        },
        {
            title: 'No route and no screen for reporting',
            body: 'No BFF route serves reporting and the application has no reporting UI. The wizard, ' +
                'the summary and the shared analytics screen are new work, and these pages are the ' +
                'first drawing of them.'
        },
        {
            title: 'The allowed values of a parameter are not readable here',
            body: 'A parameter row is a parameter_name and a value, and the kind of value comes from ' +
                'the definition behind it. No operation in this journey\u2019s set returns that ' +
                'definition\u2019s value domain, so Manage the analytics draws a select only where ' +
                'the fixture knows the enumeration, and a text field where it does not. A run is ' +
                'where a wrong value shows up.'
        }
    ];

    var S = {
        step: 1,
        showState: false,
        activity: 'rest',
        log: []
    };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function analyticOf(code) {
        var found = ANALYTICS.filter(function (entry) { return entry.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function storedRows() {
        return ROWS.slice().sort(function (a, b) { return a.order - b.order; });
    }

    function activeRows() {
        return storedRows().filter(function (row) { return row.active; });
    }

    function keptOffRows() {
        return storedRows().filter(function (row) { return !row.active; });
    }

    function ruleState() {
        var pfe = activeRows().some(function (row) { return row.code === 'pfe'; });
        var sim = activeRows().some(function (row) { return row.code === 'simulation'; });
        if (pfe && !sim) return 'violated';
        if (pfe && sim) return 'satisfied';
        return 'open';
    }

    function logStep(text) {
        S.log.push(text);
    }

    // --------------------------------------------------------- shared parts

    function appHeader() {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Prototypes</a>' +
            '<a href="#" class="here">Reporting</a></nav>' +
            '<span class="modechip">' + esc(PARTY.code) + ' \u00b7 ' + esc(DEFINITION.reportType) +
            ' report</span>' +
            '</div></header>';
    }

    function pageHead() {
        return '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>/</span>' +
            '<span>Reporting</span><span>/</span><span>Change what a report runs</span></div>' +
            '<h1>What this report runs</h1>' +
            '<p class="lede">' + esc(DEFINITION.name) + ' is a <b>' + esc(DEFINITION.reportType) +
            '</b> report of party <b>' + esc(PARTY.code) + '</b>. The wizard reads the five ' +
            'configurations it binds, then the analytics of <b>' + esc(CONFIGURATION.name) + '</b>, ' +
            'the shared configuration they belong to, then what a change there reaches. The ' +
            'analytics themselves are set on <b>Manage the analytics</b>, which step 2 opens. ' +
            'Changing the set makes every run made after it incomparable with the runs before it.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Fixture clock 2026-10-17 10:20</span>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>PROTOTYPE, MOCK DATA.</b> ' +
            'Every analytic, parameter, run, version and date on this page is a fixture, and nothing ' +
            'reads a server. Two statements on it are not fixture: <b>an analytic is turned off by ' +
            'setting active to false, not by deleting the row</b>, and <b>pfe cannot run without ' +
            'simulation</b>. The first is how the model works; the second is a rule the ACME seeder ' +
            'enforces and no service does. <b>No operation says which runs were made with which ' +
            'configuration version</b>: the runs are placed by their own start time against the ' +
            'definition\u2019s version history, which is an inference. There is no BFF route for ' +
            'reporting and no reporting UI in the application.</div>';
    }

    function stepper() {
        var items = STEPS.map(function (step) {
            var cls = 'wstep';
            if (step.n === S.step) cls += ' current';
            else if (step.n < S.step) cls += ' done';
            var tag = step.n === 2 && ruleState() === 'violated'
                ? '<span class="tag bad">the set cannot run</span>' : '';
            return '<li class="' + cls + '">' +
                '<button data-act="step" data-step="' + step.n + '">' +
                '<span class="num">' + step.n + '</span>' +
                '<span class="tt">' + esc(step.title) + '</span>' + tag +
                '</button></li>';
        }).join('');
        return '<nav class="wizard" aria-label="Wizard steps"><ol>' + items + '</ol>' +
            '<span class="wmeta">Step <b>' + S.step + '</b> of ' + STEPS.length +
            ' \u00b7 ' + esc(STEPS[S.step - 1].title) + '</span></nav>';
    }

    function stepNav(nextLabel, nextStep, backStep) {
        var back = backStep
            ? '<button class="btn" data-act="step" data-step="' + backStep + '">Back</button>'
            : '<span></span>';
        var next = nextStep
            ? '<button class="btn primary" data-act="step" data-step="' + nextStep + '">' +
              esc(nextLabel) + '</button>'
            : '<span></span>';
        return '<div class="stepnav">' + back + next + '</div>';
    }

    // ------------------------------------------------------------- step one

    function configRow(binding) {
        var where;
        if (binding.route) {
            where = '<a class="route" href="' + esc(binding.route.path) + '" title="' +
                esc(binding.route.path) + '">' +
                '<span class="mono">' + esc(binding.route.hint ? 'shared screen' :
                    binding.route.path) + '</span>' +
                '<span class="arrow">\u2192</span>' + esc(binding.route.screen) + '</a>' +
                '<div class="hint">' + esc(binding.route.hint ||
                    ('real route \u00b7 ' + binding.route.symbol)) + '</div>';
        } else {
            where = '<span class="tag muted">no screen</span>' +
                '<div class="hint">no route exists</div>';
        }
        return '<div class="configrow' + (binding.shared ? ' shared' : '') + '">' +
            '<span class="cftype">' + esc(binding.type) + '</span>' +
            '<span class="cfname">' + esc(binding.name) +
            (binding.shared ? '<span class="tag accent">carries the analytics</span>' : '') + '</span>' +
            '<span class="cfwhat">' + esc(binding.supplies) + '</span>' +
            '<span class="cfwhere">' + where + '</span>' +
            '</div>';
    }


    /* The conventions row is an entry point. This card reads the set the bound
       configuration carries, states that every kind is bound and where the
       documents came from, and hands the work to the shared prototype. It draws
       no picker. */
    function conventionsRow(kind) {
        var held = '<span class="tag ok">' + kind.docs +
            (kind.docs === 1 ? ' document' : ' documents') + '</span>' +
            (kind.tenantWide ? ' <span class="tag muted">tenant-wide</span>' : '');
        var prov = kind.prov === 'system'
            ? '<span class="tag warn">copied from the system tenant</span> No DQ route for ' +
              'this kind.'
            : '<span class="tag ok">DQ publish</span> ' +
              esc(CONVENTIONS_PUBLISH[kind.prov].dataset);
        return '<tr><td>' + esc(kind.name) + '</td>' +
            '<td class="code">' + held + '</td>' +
            '<td class="wrap">' + prov + '</td></tr>';
    }

    function conventionsCard() {
        var partyDocs = CONVENTIONS_KINDS.reduce(function (sum, kind) {
            return kind.tenantWide ? sum : sum + kind.docs;
        }, 0);
        var tenantDocs = CONVENTIONS_KINDS.reduce(function (sum, kind) {
            return kind.tenantWide ? sum + kind.docs : sum;
        }, 0);
        return '<div class="card" style="margin-top:12px;background:var(--bg)">' +
            '<div class="sectionhead"><h3>The conventions set this report binds</h3>' +
            '<span class="meta">refdata.v1.conventions_documents.get \u00b7 fixture</span></div>' +
            '<div class="notice ok"><b>ACME is provisioned and every kind is bound.</b> The ' +
            'tenant holds 10,991 convention rows across the 26 kinds; a party holds ' +
            num(partyDocs) + ' of them, and the tenant holds ' + num(tenantDocs) + ' more in the four ' +
            'kinds whose table carries no party column. <b>25 of the 26 kinds came from a DQ ' +
            'publish</b>; tenor did not, and its 4 rows were copied from the system tenant.</div>' +
            '<table style="margin-top:12px"><thead><tr>' +
            '<th>Kind</th><th>Documents ACME holds</th><th>Where they came from</th>' +
            '</tr></thead><tbody>' + CONVENTIONS_KINDS.map(conventionsRow).join('') +
            '</tbody></table>' +
            '<p class="rowfoot">A published document is a copy of a row of the DQ dataset, so a ' +
            'kind holds many documents rather than one: swap 320, commodity future 790. The name ' +
            'of a convention is derived from its terms, because no table stores one; the shared ' +
            'prototype names each kind\u2019s documents and says how many it holds. ' +
            '<b>Nothing records which of them a run reads</b>, and the per-kind binding is a ' +
            'fixture: the reporting model names no document per kind.</p>' +
            '<div class="draftrow" style="margin-top:10px">' +
            '<a class="btn primary" href="../manage-the-conventions/?from=change-what-a-report-runs">' +
            'Manage the conventions \u2192</a>' +
            '<span class="hint">One screen owns the 26 rows, the page for each kind and the ' +
            'create route. This wizard reads the set and links there.</span></div></div>';
    }

    function stepOne() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The definition and the configurations it binds</h2>' +
            '<span class="meta">reporting.v1.report_definitions.get \u00b7 ' +
            'reporting.v1.report_configurations.list \u00b7 fixture</span></header>' +
            '<div class="defline"><b>' + esc(DEFINITION.name) + '</b>' +
            '<span class="id">' + esc(DEFINITION.id) + '</span>' +
            '<span class="tag accent">' + esc(DEFINITION.reportType) + '</span>' +
            '<span class="tag muted">party ' + esc(PARTY.code) + '</span></div>' +
            '<div class="cfhead"><span>type</span><span>configuration</span>' +
            '<span>what it supplies</span><span>where that data lives</span></div>' +
            '<div class="configs">' + CONFIG_TYPES.map(configRow).join('') + '</div>' +
            conventionsCard() +
            '<div class="notice" style="margin-top:12px">' + esc(NO_CONFIG_SCREEN) +
            ' The conventions link opens the shared prototype, which stands in for ' +
            '<span class="mono">PATHS.conventions</span>; the other four rows stay stated as ' +
            'missing.</div>' +
            '<div class="notice" style="margin-top:8px"><b>' + esc(CONFIGURATION.name) + '</b> ' +
            '<span class="mono">' + esc(CONFIGURATION.id) + '</span> carries the analytics, and four ' +
            'definitions bind it: this one, ' + esc(SHARED_WITH.join(', ')) + '. ' + esc(SHARED) +
            ' The analytics are not on the definition, so a change to them reaches all four.</div>' +
            '<p class="rowfoot">A definition binds configurations of exactly these five types. To ' +
            'change another configuration, or to rebind the definition to a different one, is a ' +
            'different change, and this wizard does not offer it.</p>' +
            stepNav('Next: the analytics', 2, null) +
            '</section>';
    }

    // ------------------------------------------------------------- step two

    function ruleStrip() {
        var state = ruleState();
        var cls = state === 'violated' ? ' err' : (state === 'satisfied' ? ' ok' : '');
        var title;
        if (state === 'violated') {
            title = '<span class="tag bad">this set cannot run</span> pfe is on and simulation is off';
        } else if (state === 'satisfied') {
            title = '<span class="tag ok">rule satisfied</span> pfe and simulation are both on';
        } else {
            title = '<span class="tag warn">rule between analytics</span> pfe is off, so the rule ' +
                'does not bind';
        }
        return '<div class="rule' + cls + '">' +
            '<div class="rtitle">' + title + '</div>' +
            '<div class="rbody"><b>pfe reads a cube that simulation writes.</b> ' + esc(PFE_RULE) +
            ' Both rows are marked below while the pair is broken, and Manage the analytics refuses ' +
            'to write the pair at all.</div></div>';
    }

    /* One row: the on or off state, the name and code, the display_order, a
       one-line statement of what it produces and the count of parameter rows.
       There is no control here. */
    function summaryRowHtml(row) {
        var entry = analyticOf(row.code);
        var name = entry ? entry.name : row.code;
        var produces = entry ? entry.produces : '';
        var settings = entry ? entry.params.length : 0;
        var conflict = ruleState() === 'violated' &&
            (row.code === 'pfe' || row.code === 'simulation');
        var cls = 'anrow' + (row.active ? '' : ' isoff') + (conflict ? ' conflict' : '');
        return '<div class="' + cls + '"><div class="an-top">' +
            '<span class="swnone">' + (row.active ? 'on' : 'off') + '</span>' +
            '<span class="an-name">' + esc(name) +
            '<span class="codename">' + esc(row.code) + '</span>' +
            (conflict && row.code === 'pfe'
                ? '<span class="tag bad">needs simulation \u00b7 cannot run</span>' : '') +
            (conflict && row.code === 'simulation'
                ? '<span class="tag bad">pfe is on and this is off</span>' : '') +
            '</span>' +
            '<span class="an-order">order ' + row.order + '</span>' +
            (row.active ? '<span class="tag ok">on</span>'
                : '<span class="tag muted">active = false</span>') +
            '<span class="an-summary">' + esc(produces) +
            ' <span class="an-count">\u00b7 ' + (settings === 0
                ? 'no parameter row'
                : settings + ' parameter row' + (settings === 1 ? '' : 's')) + '</span></span>' +
            '</div></div>';
    }

    function stepTwo() {
        var on = activeRows();
        var rows = storedRows();
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The analytics of ' + esc(CONFIGURATION.name) + '</h2>' +
            '<span class="meta"><span class="tag accent">read only</span>' +
            '<span class="mono">reporting.v1.configuration_analytics.list_by_configuration_id</span>' +
            ' \u00b7 <b>' + on.length + ' on</b> of ' + rows.length + ' row' +
            (rows.length === 1 ? '' : 's') + ' \u00b7 fixture</span></header>' +
            ruleStrip() +
            '<div class="summary" style="margin-top:12px">' +
            '<p class="rowfoot" style="margin-top:0">' + on.length + ' analytic' +
            (on.length === 1 ? '' : 's') + ' on, in this display order: ' +
            (on.length === 0
                ? 'none, so a run of this report produces nothing.'
                : esc(on.map(function (row) { return row.code + ' at ' + row.order; }).join(', ')) +
                  '. The six types with no row are absent from the configuration, which is not the ' +
                  'same as off.') + '</p>' +
            '<div class="anlist">' + rows.map(summaryRowHtml).join('') + '</div>' +
            '</div>' +
            '<p class="rowfoot">' + esc(NO_DELETE) + ' ' + (ANALYTICS.length - rows.length) +
            ' of the ' + ANALYTICS.length + ' analytic types carry no row on this configuration, ' +
            'and each of them is offered on Manage the analytics as a type with no row yet.</p>' +
            '<div class="analyticslink">' +
            '<div><b>Manage the analytics</b>' +
            '<p class="hint">The switchboard of all fourteen types, a step for each analytic that ' +
            'is on, every parameter with a control chosen for its shape and a note on its normal ' +
            'value, the refused pfe and simulation pair, and the read-back. It belongs to ' +
            esc(CONFIGURATION.name) + ', not to this definition, so a change there reaches all four ' +
            'definitions bound to it.</p></div>' +
            '<a class="btn primary" href="../manage-the-analytics/?from=change-what-a-report-runs">' +
            'Open Manage the analytics</a></div>' +
            '<p class="rowfoot">' + esc(HANDED_OFF) + '</p>' +
            stepNav('Next: what a change reaches', 3, 1) +
            '</section>';
    }

    // ----------------------------------------------------------- step three

    function reachCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>What a change does and does not reach</h2>' +
            '<span class="meta">stated before anything is written \u00b7 fixture</span></header>' +
            '<div class="reach">' +
            '<div class="does"><h3>What a change reaches</h3><ul>' +
            '<li>The next run of every definition bound to ' + esc(CONFIGURATION.name) + ', including ' +
            esc(SHARED_WITH.join(', ')) + '. None of them was opened here.</li>' +
            '<li>The configuration\u2019s analytic rows and their parameter rows.</li>' +
            '<li>The comparison between a run made after it and a run made before it, which stops ' +
            'being a comparison.</li></ul></div>' +
            '<div class="not"><h3>What it does not reach</h3><ul>' +
            '<li>A run already produced: <b>it keeps the configuration it ran with</b>, with its ' +
            'outputs and its errors.</li>' +
            '<li>The definition\u2019s schedule, concurrency policy, processing phases and its other ' +
            'four configurations.</li>' +
            '<li>The configuration\u2019s market roles, which this journey reads and does not write.</li>' +
            '<li>Any run in flight: no operation in this journey stops one.</li></ul></div>' +
            '</div>' +
            '<div class="notice warn" style="margin-top:12px"><b>A run either side of a change is ' +
            'not a comparison.</b> ' + esc(INCOMPARABLE) + '</div>' +
            '<p class="rowfoot">' + esc(PAST_RUNS) + '</p></section>';
    }

    function runsCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The runs a change strands</h2>' +
            '<span class="meta">reporting.v1.report_definitions_versions.list \u00b7 placed by start ' +
            'time \u00b7 inference</span></header>' +
            '<div class="versions">' + VERSIONS.map(function (version) {
                return '<div class="vrow"><span class="vno">v' + version.version + '</span>' +
                    '<span class="vat">' + esc(version.at) + '</span>' +
                    '<span class="vnote">' + esc(version.note) + '</span></div>';
            }).join('') +
            '<div class="vrow"><span class="vno">v' + NEXT_VERSION + '</span>' +
            '<span class="vat">not yet written</span>' +
            '<span class="vnote">the change, once the shared screen writes it</span></div></div>' +
            '<div class="runs" style="margin-top:12px">' + RUNS.map(function (run) {
                return '<div class="runrow">' +
                    '<span class="rid">' + esc(run.id) + '</span>' +
                    '<span class="rat">' + esc(run.at) + '</span>' +
                    '<span class="rstate">' + esc(run.state) + ', ' + run.errors + ' error(s) \u00b7 ' +
                    'started after v6, so it ran the stored set (an inference)' +
                    '<span class="tag warn">becomes incomparable</span></span></div>';
            }).join('') + '</div>' +
            '<p class="rowfoot">A run is placed here by its own start time against the ' +
            'definition\u2019s version history. No operation says which configuration version a run ' +
            'used, so the placement is an inference and the panel says so.</p></section>';
    }

    function handedOffCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Where the set is changed</h2>' +
            '<span class="meta">configuration_analytics.put_many \u00b7 ' +
            'configuration_analytic_parameters.put \u00b7 no write from this page</span></header>' +
            '<p class="rowfoot" style="margin-top:0">This page stages nothing and writes nothing. ' +
            'The set is sent from Manage the analytics, in one call, so a turned-on analytic, a ' +
            'turned-off one and a changed parameter land together and a half-written configuration is ' +
            'not left behind. A setting alone is written one row at a time. No write deletes a row, ' +
            'and after it the definition carries a new version while the older versions stay ' +
            'readable.</p>' +
            '<div class="notice" style="margin-top:8px">The rows are read here with ' +
            '<span class="mono">reporting.v1.configuration_analytics.list_by_configuration_id</span> ' +
            'and <span class="mono">' +
            'reporting.v1.configuration_analytic_parameters.list_by_configuration_analytic_id' +
            '</span>, the vocabulary with <span class="mono">reporting.v1.analytic_types.list</span>, ' +
            'and the definitions bound to the configuration with ' +
            '<span class="mono">reporting.v1.report_configurations.list</span>.</div>' +
            '<div class="analyticslink">' +
            '<div><b>Manage the analytics</b>' +
            '<p class="hint">Bookmark it, or come back to step 2 when the set is what you meant.</p>' +
            '</div>' +
            '<a class="btn primary" href="../manage-the-analytics/?from=change-what-a-report-runs">' +
            'Open Manage the analytics</a></div>' +
            stepNav(null, null, 2) +
            '</section>';
    }

    function stepThree() {
        return reachCard() + runsCard() + handedOffCard();
    }

    function gapCard() {
        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>What this screen cannot show</h2>' +
            '<span class="faint" style="font-size:12px">the journey\u2019s own list of what is ' +
            'missing</span></header>' +
            '<dl>' + GAPS.map(function (gap) {
                return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    // --------------------------------------------------------------- bars

    function renderBar() {
        var steps = STEPS.map(function (step) {
            return '<button data-act="step" data-step="' + step.n + '"' +
                (S.step === step.n ? ' class="on"' : '') + '>' + step.n + ' ' +
                esc(step.title) + '</button>';
        }).join('');
        var stateLine = '<span class="label">step <b>' + S.step + '</b> \u00b7 state: <b>' +
            esc(S.activity) + '</b></span>';
        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>analytic types: <b>' + ANALYTICS.length + '</b></span>' +
            '<span>rows on the configuration: <b>' + storedRows().length + '</b></span>' +
            '<span>analytics on: <b>' + activeRows().length + '</b></span>' +
            '<span>rows kept off: <b>' + keptOffRows().length + '</b></span>' +
            '<span>pfe / simulation: <b>' +
            (activeRows().some(function (r) { return r.code === 'pfe'; }) ? 'on' : 'off') + ' / ' +
            (activeRows().some(function (r) { return r.code === 'simulation'; }) ? 'on' : 'off') +
            '</b></span>' +
            '<span>rule: <b>' + ruleState() + '</b></span>' +
            '<span>definition version: <b>6, and no change is staged here</b></span>' +
            '<span>configuration screens: <b>1 of 5</b></span>' +
            '</div>' +
            '<p class="state-note">The rows, the parameters, the runs and the versions are a fixture. ' +
            'The rules are not: active false keeps the row, and pfe needs simulation. No service ' +
            'checks the second, and no operation says which configuration version a run used.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + steps +
            '<span class="sep">|</span>' + stateLine +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') +
            '</button></div>' + statePanel;
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 step ' + S.step +
            ' \u00b7 state ' + S.activity + ' \u00b7 ' + DEFINITION.name + ', party ' + PARTY.code;
    }

    function render() {
        var body = pageHead() + fixtureNote() + stepper() +
            (S.step === 1 ? stepOne() : (S.step === 2 ? stepTwo() : stepThree())) +
            (S.step === 3 ? gapCard() : '');
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() + '<main>' + body + '</main></div>';
        renderNote();
        renderBar();
        writeParams();
    }

    // ------------------------------------------------------------- params

    function readParams() {
        var wanted = parseInt(new URLSearchParams(window.location.search).get('step'), 10);
        if (wanted >= 1 && wanted <= STEPS.length) S.step = wanted;
    }

    function writeParams() {
        window.history.replaceState(null, '',
            window.location.pathname + '?step=' + S.step);
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (act === 'noop') {
            event.preventDefault();
            return;
        }
        if (element.tagName !== 'BUTTON') return;
        event.preventDefault();
        if (act === 'step') {
            var wanted = parseInt(element.getAttribute('data-step'), 10);
            if (wanted >= 1 && wanted <= STEPS.length) {
                S.step = wanted;
                S.activity = 'step ' + wanted + ' \u00b7 ' + STEPS[wanted - 1].title;
                logStep('opened step ' + wanted);
                render();
            }
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        }
    });

    readParams();
    render();
}());

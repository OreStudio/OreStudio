/* Bring data in: one entry screen for every import journey. Self-contained:
 * plain JavaScript, mock data, no framework, no build step, and nothing that
 * outlives the page.
 *
 * The argument: every import journey starts the same way — name a source and a
 * scope. What differs is only the trigger (a defect, a seed, a promotion, a
 * feed, a correction), the payload (a bundle, a sample tree, two market files,
 * one conventions file) and who may act. So this is one screen with those three
 * things as variants, not seven screens.
 *
 * States come from the query string:
 *   ?case=onboard|seed|defect|feed|one-dataset|promote|party
 *   ?actor=admin|ops|maintainer|support
 *   ?state=source|families|refused
 *   ?family=<key>                       the family row left open
 *   ?scan=1|0                           hide the scanner result (seed only)
 *   ?target=<sandbox id>                the sandbox the import targets
 *
 * The sources are the shell verbs the journeys name today: ore upload, ore
 * import, ore import-run, marketdata import, compute download-input. The
 * screen is the one the journeys say does not exist.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTY = 'Acme Trading';

    var ACTORS = {
        admin: {
            id: 'j.smith', name: 'J. Smith', part: 'Tenant administrator', party: PARTY,
            wildcard: true, holds: ['every permission in the tenant'],
            can: { trades: true, conventions: true, curves: true, market: true, run: true },
            note: 'The role carries a wildcard, so every import is reachable. This is the only actor who can run all of them today.'
        },
        ops: {
            id: 'k.mori', name: 'K. Mori', part: 'Operations user', party: PARTY,
            wildcard: false, holds: ['refdata::conventions:write'],
            can: { trades: false, conventions: true, curves: false, market: false, run: false },
            note: 'An operations user holds the convention write and nothing else the imports need, so the portfolio, run and market imports are refused. The journey says "may import" is not a permission the product has.'
        },
        maintainer: {
            id: 'a.tanaka', name: 'A. Tanaka', part: 'Reference-data maintainer', party: PARTY,
            wildcard: false, holds: ['refdata::conventions:write', 'refdata::curve_configuration_documents:write'],
            can: { trades: false, conventions: true, curves: true, market: false, run: true },
            note: 'Holds the conventions and curve writes, so the one-file import works and the portfolio import does not.'
        },
        support: {
            id: 'd.osei', name: 'D. Osei', part: 'Support engineer', party: PARTY,
            wildcard: true, holds: ['every permission in the development installation'],
            can: { trades: true, conventions: true, curves: true, market: true, run: true },
            note: 'Holds every import permission in development, which is why reproducing a defect there is possible while a party-scoped user cannot do the same in production.'
        }
    };

    /* The convention families ORE's conventions.xml carries, with the store
     * each one lands in and the merge policy the design draft names. */
    var FAMILIES = [
        { key: 'swap', name: 'Swap', ent: 'swap_conventions', rows: 214, live: 1891, policy: 'replace_party' },
        { key: 'ois', name: 'OIS', ent: 'ois_conventions', rows: 38, live: 402, policy: 'replace_party' },
        { key: 'deposit', name: 'Deposit', ent: 'deposit_conventions', rows: 96, live: 733, policy: 'replace_party' },
        { key: 'fra', name: 'FRA', ent: 'fra_conventions', rows: 22, live: 168, policy: 'replace_party' },
        { key: 'future', name: 'Future', ent: 'future_conventions', rows: 12, live: 94, policy: 'replace_party' },
        { key: 'fx', name: 'FX', ent: 'fx_conventions', rows: 6, live: 0, policy: 'skip' },
        { key: 'bond_yield', name: 'Bond yield', ent: 'bond_yield_conventions', rows: 18, live: 212, policy: 'replace_party' },
        { key: 'cds', name: 'CDS', ent: 'cds_conventions', rows: 9, live: 61, policy: 'add_missing_world' },
        { key: 'cross_currency_basis', name: 'Cross-currency basis', ent: 'cross_currency_basis_conventions', rows: 14, live: 187, policy: 'replace_party' },
        { key: 'inflation_swap', name: 'Inflation swap', ent: 'inflation_swap_conventions', rows: 7, live: 54, policy: 'replace_party' },
        { key: 'commodity_forward', name: 'Commodity forward', ent: 'commodity_forward_conventions', rows: 5, live: 37, policy: 'replace_party' },
        { key: 'average_ois', name: 'Average OIS', ent: 'average_ois_conventions', rows: 3, live: 18, policy: 'replace_party' }
    ];

    var CASES = {
        onboard: {
            title: 'Onboard a party',
            journey: 'Onboard a party with its conventions and portfolio',
            gist: 'The tenant administrator brings one party\u2019s conventions and portfolio, and nothing else.',
            sourceKind: 'ORE input directory',
            sourceName: 'acme-trading/onboarding/20261017',
            env: 'Another installation',
            bytes: 41873920, parts: 2, trades: 64, portfolios: 3,
            partKeys: ['trades', 'conventions'],
            holder: 'First National Bank',
            files: [
                { name: 'ore.xml', kind: 'Run settings', rows: '2 setup files', use: 'accepted' },
                { name: 'conventions.xml', kind: 'Conventions document', rows: '12 families \u00b7 344 rows', use: 'accepted' },
                { name: 'portfolio.xml', kind: 'Portfolio and trades', rows: '3 portfolios \u00b7 64 trades', use: 'accepted' },
                { name: 'curveconfig.xml', kind: 'Curves', rows: 'empty shelf', use: 'ignored', why: 'not asked for: this journey must bring no curve' },
                { name: 'todaysmarket.xml', kind: "Today's market", rows: 'not read', use: 'ignored', why: 'not asked for' },
                { name: 'pricingengine.xml', kind: 'Pricing engines', rows: 'not read', use: 'ignored', why: 'not asked for' }
            ],
            include: ['swap', 'ois', 'deposit', 'fra', 'future', 'bond_yield', 'cds',
                'cross_currency_basis', 'inflation_swap', 'commodity_forward', 'average_ois'],
            sections: null,
            gaps: [
                'No standalone conventions import. One conventions.xml travels only inside a run directory and a report definition that must already exist.',
                'No family selector. The run import stores every document the run names, so the person edits the input by hand.',
                'No per-family result and no preview. The family counts are visible only after the save.'
            ]
        },
        seed: {
            title: 'Seed a fresh environment',
            journey: 'Seed a fresh environment from ORE sample directories',
            gist: 'The operations user loads three ORE sample trees into an empty party so it starts from known data.',
            sourceKind: 'Folder of sample trees',
            sourceName: 'ore-samples/Example_1 \u2026 Example_3',
            env: 'ORE 2016 sample fixtures',
            bytes: 96468992, parts: 3, trades: 288, portfolios: 3,
            partKeys: ['trades', 'conventions', 'curves', 'market'],
            holder: 'Sample data',
            files: [
                { name: 'Example_1/Input/portfolio.xml', kind: 'Portfolio and trades', rows: '3 portfolios \u00b7 96 trades', use: 'accepted' },
                { name: 'Example_1/Input/conventions.xml', kind: 'Conventions document', rows: '12 families \u00b7 344 rows', use: 'accepted' },
                { name: 'Example_1/Input/market.txt', kind: 'Quotes', rows: '18,442 observations', use: 'accepted' },
                { name: 'Example_1/Input/fixings.txt', kind: 'Fixings', rows: '1,204 fixings', use: 'accepted' },
                { name: 'Example_1/Output/portfolio.xml', kind: 'Result, not input', rows: 'not read', use: 'ignored', why: 'the Output folder holds results' },
                { name: 'Example_2/Input/curveconfig.xml', kind: 'Curves', rows: '14 curves', use: 'accepted' },
                { name: 'Example_3/Input/portfolio.xml', kind: 'Portfolio and trades', rows: '3 portfolios \u00b7 96 trades', use: 'accepted' },
                { name: 'Example_3/Input/notes.csv', kind: 'Unknown root', rows: 'not read', use: 'ignored', why: 'the scanner classifies by root element and this one is not an ORE root' }
            ],
            include: ['swap', 'ois', 'deposit', 'fra', 'future', 'bond_yield', 'cds',
                'cross_currency_basis', 'inflation_swap', 'commodity_forward', 'average_ois'],
            sections: null,
            gaps: [
                'No batch seed. Each sample is two or three separate commands, and nothing takes a root folder of samples in one run.',
                'The scan is invisible. The person cannot list the files the scanner found and the ones it ignored before the import.',
                'No trade identity check. A second run creates new trade ids and doubles the trades.',
                'No dry run. The counts are available only after the writes.'
            ]
        },
        defect: {
            title: 'Reproduce a defect',
            journey: 'Reproduce a defect from an exported environment',
            gist: 'The support engineer brings the reporting environment\u2019s trades, conventions, market data and the job\u2019s input bundle into development.',
            sourceKind: 'Export bundle (.orebundle)',
            sourceName: 'reporting-2026-10-16.orebundle',
            env: 'Reporting (production)',
            bytes: 432013312, parts: 6, trades: 1642, portfolios: 47,
            partKeys: ['trades', 'conventions', 'curves', 'market', 'run'],
            holder: 'First National Bank',
            files: [
                { name: 'manifest.json', kind: 'Bundle manifest', rows: '6 parts \u00b7 read 14:31Z', use: 'accepted' },
                { name: 'trades/portfolio.xml', kind: 'Portfolio and trades', rows: '47 portfolios \u00b7 1,642 trades', use: 'accepted' },
                { name: 'run/ore.xml', kind: 'Run settings', rows: '5 documents', use: 'accepted' },
                { name: 'run/conventions.xml', kind: 'Conventions document', rows: '12 families \u00b7 344 rows', use: 'accepted' },
                { name: 'run/curveconfig.xml', kind: 'Curves', rows: '84 curves', use: 'accepted' },
                { name: 'market/market.txt', kind: 'Quotes', rows: '18,442 observations', use: 'accepted' },
                { name: 'job/run_4f2c-input.tar.gz', kind: 'Job input bundle', rows: '2.1 GB unpacked', use: 'accepted' },
                { name: 'job/engine-6.2.1.tar.gz', kind: 'Engine package', rows: 'sha256 checked', use: 'accepted' }
            ],
            include: ['swap', 'ois', 'deposit', 'fra', 'future', 'bond_yield', 'cds',
                'cross_currency_basis', 'inflation_swap', 'commodity_forward', 'average_ois'],
            sections: null,
            gaps: [
                'No whole-environment export. The person runs four export verbs and writes down what each produced; there is no dated bundle and no manifest.',
                'No job export. The job definition is rebuilt in the target.',
                'No cross-component transaction. Trades, conventions and market data land in three services and three imports.',
                'No preview. The import reports what it saved after the fact.'
            ]
        },
        feed: {
            title: 'Load a vendor feed',
            journey: 'Import market data from an external feed',
            gist: 'The market-data maintainer loads the vendor\u2019s quotes and fixings, tagged with the source, and reads what was refused.',
            sourceKind: 'Two market-data files',
            sourceName: 'vendor/2026-10-16/market.txt + fixings.txt',
            env: 'Vendor drop folder',
            bytes: 1932735, parts: 2, trades: 0, portfolios: 0,
            partKeys: ['market'],
            holder: 'Vendor',
            files: [
                { name: 'market.txt', kind: 'Quotes', rows: '312 series \u00b7 18,442 observations', use: 'accepted' },
                { name: 'fixings.txt', kind: 'Fixings', rows: '7 indices \u00b7 1,204 fixings', use: 'accepted' },
                { name: 'vendor-readme.pdf', kind: 'Not ORE text', rows: 'not read', use: 'ignored', why: 'only the two tab-separated files are read' }
            ],
            include: [],
            sections: [
                { key: 'quotes', name: 'Quotes', rows: 18442, series: 312, policy: 'replace_party' },
                { key: 'fixings', name: 'Fixings', rows: 1204, series: 7, policy: 'replace_party' }
            ],
            gaps: [
                'No native CSV or JSON import. Every vendor file is converted to ORE text outside the platform first, so the conversion is outside its audit.',
                'No vendor connector. A feed binding starts a synthetic feed, not a vendor drop folder.',
                'No per-section result. The counts are totals and the errors are strings.',
                'No dry run and no import record beyond the source tag on each row.'
            ]
        },
        'one-dataset': {
            title: 'Replace one dataset',
            journey: 'Import one dataset in isolation',
            gist: 'The reference-data maintainer replaces one corrected conventions family and leaves every other dataset alone.',
            sourceKind: 'One ORE file',
            sourceName: 'fixes/swap_conventions_20261016.xml',
            env: 'Vendor correction',
            bytes: 262144, parts: 1, trades: 0, portfolios: 0,
            partKeys: ['conventions'],
            holder: 'Vendor',
            files: [
                { name: 'swap_conventions_20261016.xml', kind: 'Conventions family', rows: '1 family \u00b7 214 rows', use: 'accepted' },
                { name: 'ore.xml', kind: 'Minimal run settings', rows: '1 setup file', use: 'accepted' }
            ],
            include: ['swap'],
            sections: null,
            gaps: [
                'No standalone file import. Every dataset travels in a run directory, so the person builds a fake run to import one file.',
                'No family scope. The person cannot say "store only the swap family" and leave the rest of the file as context.',
                'No per-family result and no merge preview. The screen cannot say which rows would be replaced before the save.'
            ]
        },
        promote: {
            title: 'Promote an environment',
            journey: 'Promote an environment between systems',
            gist: 'The release manager lands the accepted development set in staging, in dependency order, with a record of the hop.',
            sourceKind: 'Promotion bundle',
            sourceName: 'promote/dev-to-staging/20261016-1430',
            env: 'Development',
            bytes: 226492416, parts: 7, trades: 1642, portfolios: 47,
            partKeys: ['trades', 'conventions', 'curves', 'market', 'run'],
            holder: 'First National Bank',
            files: [
                { name: 'manifest.json', kind: 'Bundle manifest', rows: '7 parts \u00b7 dev @ 14:30Z', use: 'accepted' },
                { name: '1-currencies.xml', kind: 'Currencies', rows: '31 currencies', use: 'accepted' },
                { name: '2-portfolio.xml', kind: 'Portfolio and trades', rows: '47 portfolios \u00b7 1,642 trades', use: 'accepted' },
                { name: '3-conventions.xml', kind: 'Conventions document', rows: '12 families \u00b7 344 rows', use: 'accepted' },
                { name: '4-curveconfig.xml', kind: 'Curves', rows: '84 curves', use: 'accepted' },
                { name: '5-market.txt', kind: 'Quotes', rows: '18,442 observations', use: 'accepted' },
                { name: '6-run.xml', kind: 'Run documents', rows: '5 bindings', use: 'accepted' }
            ],
            include: ['swap', 'ois', 'deposit', 'fra', 'future', 'bond_yield', 'cds',
                'cross_currency_basis', 'inflation_swap', 'commodity_forward', 'average_ois'],
            sections: null,
            gaps: [
                'No environment bundle and no manifest. The person assembles the parts and writes the manifest by hand.',
                'No promotion record. The ticket is the only record of which environment received what.',
                'No ordering guard. A wrong order fails late, after the earlier parts landed.',
                'No idempotent promotion. Re-running the portfolio import duplicates the trades.'
            ]
        },
        party: {
            title: 'Import into my party',
            journey: 'Import as a party-scoped user',
            gist: 'The operations user brings the same file set into the party they are signed in to, and sees only that party afterwards.',
            sourceKind: 'ORE input directory',
            sourceName: 'acme-trading/backfill/20261017',
            env: 'Another installation',
            bytes: 52428800, parts: 2, trades: 64, portfolios: 3,
            partKeys: ['trades', 'conventions'],
            holder: 'First National Bank',
            files: [
                { name: 'conventions.xml', kind: 'Conventions document', rows: '12 families \u00b7 344 rows', use: 'accepted' },
                { name: 'portfolio.xml', kind: 'Portfolio and trades', rows: '3 portfolios \u00b7 64 trades', use: 'accepted' }
            ],
            include: ['swap', 'ois', 'deposit', 'fra', 'future', 'bond_yield', 'cds',
                'cross_currency_basis', 'inflation_swap', 'commodity_forward', 'average_ois'],
            sections: null,
            gaps: [
                'No party argument on the import verbs. The session is the only scope, so a tenant administrator cannot stage a party\u2019s import without switching to it.',
                'No import permission of its own. Each import borrows the write permission of every row it creates.',
                'No per-party import history. Nothing says who imported what, into which party.',
                'No screen. The party scope is not visible on any import surface because there is no import surface.'
            ]
        }
    };

    var CASE_ORDER = ['onboard', 'seed', 'defect', 'feed', 'one-dataset', 'promote', 'party'];
    var ACTOR_ORDER = ['admin', 'ops', 'maintainer', 'support'];

    /* The proposed definition this import would run under, and the budgets a
     * definition states from the workflow API. The engine holds no such
     * workflow today: ore_import_workflow and run_configuration_import_workflow
     * declare one step each. */
    var WF_TYPE = 'data_exchange_import_workflow';
    var DATA_STEP = 900;
    var WRITE_STEP = 120;

    var PART_STEPS = {
        trades: { name: 'stage_trades', label: 'Stage trades and portfolios', subject: 'trading.v1.steps.stage_trades' },
        curves: { name: 'stage_curves', label: 'Stage curve configurations', subject: 'refdata.v1.steps.stage_curve_configurations' },
        market: { name: 'stage_market', label: 'Stage quotes and fixings', subject: 'marketdata.v1.steps.stage_market_data' },
        run: { name: 'stage_run', label: 'Stage run documents', subject: 'configure.v1.steps.stage_run_documents' }
    };

    /* The book tree, official and sandbox. An import publishes, and it
     * publishes inside a sandbox: a sandbox is not a node of the official
     * portfolio tree, it is anchored at one official portfolio and owns a
     * subtree of its own. Only a sandbox is an import target. The model keeps
     * the two trees apart: a book and its portfolio share one sandbox, a
     * sandbox's root portfolios have no parent, and a virtual book carries no
     * ledger reference and is not sweepable. */
    var OFFICIAL = [
        { id: 'GLOBAL', name: 'Global Trading', parent: null, purpose: 'Risk', ccy: 'USD' },
        { id: 'RATES', name: 'Global Rates', parent: 'GLOBAL', purpose: 'Risk', ccy: 'USD' },
        { id: 'RATES.EUR', name: 'EUR Rates', parent: 'RATES', purpose: 'Risk', ccy: 'EUR' },
        { id: 'RATES.USD', name: 'USD Rates', parent: 'RATES', purpose: 'Risk', ccy: 'USD' },
        { id: 'CREDIT', name: 'APAC Credit', parent: 'GLOBAL', purpose: 'Regulatory', ccy: 'USD' },
        { id: 'CREDIT.CN', name: 'China Credit', parent: 'CREDIT', purpose: 'Regulatory', ccy: 'CNY' }
    ];

    var OFFICIAL_BOOKS = [
        { name: 'EUR_SWAPS_01', portfolio: 'RATES.EUR', gl: 'GL-10150-IRS' },
        { name: 'EUR_VOL_01', portfolio: 'RATES.EUR', gl: 'GL-10150-VOL' },
        { name: 'USD_SWAPS_01', portfolio: 'RATES.USD', gl: 'GL-10150-USD' },
        { name: 'USD_FUNDING', portfolio: 'RATES.USD', gl: 'GL-10160-FND' },
        { name: 'CN_CREDIT_01', portfolio: 'CREDIT.CN', gl: 'GL-10200-CN' },
        { name: 'SWEEP_TARGET', portfolio: 'GLOBAL', gl: 'GL-10000-SWP' }
    ];

    /* A sandbox's own subtree. Every portfolio here carries the sandbox's id,
     * so a root portfolio has no parent, and every book is virtual. */
    var SANDBOXES = [
        {
            id: 'sb-import-repro', name: 'Reproduce 2026-10-16', anchor: 'RATES.EUR',
            purpose: 'import', status: 'open', visibility: 'private', owner: 'D. Osei',
            roots: [
                { id: 'sb-repro-root', name: 'Repro 2026-10-16', parent: null, ccy: 'EUR',
                  books: [{ name: 'EUR_SWAPS_SB_01' }] },
                { id: 'sb-repro-defect', name: 'Defect capture', parent: 'sb-repro-root', ccy: 'EUR',
                  books: [{ name: 'EUR_VOL_SB_02' }] }
            ]
        },
        {
            id: 'sb-import-onboard', name: 'Onboard Acme 2026-10-17', anchor: 'RATES',
            purpose: 'import', status: 'open', visibility: 'shared', owner: 'J. Smith',
            roots: [
                { id: 'sb-onboard-root', name: 'Acme onboarding 2026-10-17', parent: null, ccy: 'EUR',
                  books: [{ name: 'ACME_EUR_SWAPS_SB_01' }] }
            ]
        },
        {
            id: 'sb-sample-seed', name: 'Seed samples 2026-10', anchor: 'GLOBAL',
            purpose: 'sample', status: 'open', visibility: 'private', owner: 'K. Mori',
            roots: [
                { id: 'sb-seed-root', name: 'ORE samples 2026-10', parent: null, ccy: 'USD',
                  books: [{ name: 'SAMPLE_EUR_SWAPS_SB_01' }, { name: 'SAMPLE_USD_SWAPS_SB_02' }] }
            ]
        },
        {
            id: 'sb-import-feed', name: 'Vendor feed 2026-10-17', anchor: 'RATES.USD',
            purpose: 'import', status: 'open', visibility: 'shared', owner: 'J. Smith',
            roots: [
                { id: 'sb-feed-root', name: 'Commodity feed 2026-10-17', parent: null, ccy: 'USD',
                  books: [{ name: 'USD_FEED_SB_01' }] }
            ]
        },
        {
            id: 'sb-archived-feed', name: 'Vendor feed 2026-10-09', anchor: 'RATES.USD',
            purpose: 'sample', status: 'archived', visibility: 'private', owner: 'K. Mori',
            roots: [
                { id: 'sb-archived-root', name: 'Commodity feed 2026-10-09', parent: null, ccy: 'USD',
                  books: [{ name: 'USD_FEED_SB_00' }] }
            ]
        }
    ];

    var TARGET_BY_CASE = {
        onboard: 'sb-import-onboard', seed: 'sb-sample-seed', defect: 'sb-import-repro',
        feed: 'sb-import-feed', 'one-dataset': 'sb-import-onboard',
        promote: 'sb-import-repro', party: 'sb-import-onboard'
    };

    /* What a completed import creates inside the sandbox. The number of books
     * follows the file, not the number of portfolios. */
    var CREATES = {
        onboard: { portfolios: 3, books: 4 }, seed: { portfolios: 3, books: 12 },
        defect: { portfolios: 47, books: 103 }, feed: { portfolios: 0, books: 0 },
        'one-dataset': { portfolios: 0, books: 0 }, promote: { portfolios: 47, books: 103 },
        party: { portfolios: 3, books: 4 }
    };

    var S = readQuery();

    function readQuery() {
        var q = {};
        window.location.search.replace(/^\?/, '').split('&').forEach(function (pair) {
            if (!pair) return;
            var i = pair.indexOf('=');
            var k = decodeURIComponent(i < 0 ? pair : pair.slice(0, i));
            var v = decodeURIComponent(i < 0 ? '' : pair.slice(i + 1));
            q[k] = v;
        });
        var name = CASES[q['case']] ? q['case'] : 'onboard';
        var wanted = sandboxById(q.target);
        var state = {
            'case': name,
            actor: ACTORS[q.actor] ? q.actor : null,
            step: q.state === 'families' || q.state === 'refused' ? q.state : 'source',
            family: q.family || null,
            scan: q.scan !== '0',
            target: wanted && wanted.status === 'open' ? wanted.id : TARGET_BY_CASE[name],
            refuse: null
        };
        if (!state.actor) state.actor = state['case'] === 'party' ? 'ops' : 'admin';
        return state;
    }

    function go(patch) {
        Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
        var params = [];
        if (S['case'] !== 'onboard') params.push('case=' + S['case']);
        if (S.actor !== (S['case'] === 'party' ? 'ops' : 'admin')) params.push('actor=' + S.actor);
        if (S.step !== 'source') params.push('state=' + S.step);
        if (S.family) params.push('family=' + S.family);
        if (!S.scan) params.push('scan=0');
        if (S.target !== TARGET_BY_CASE[S['case']]) params.push('target=' + S.target);
        var url = window.location.pathname + (params.length ? '?' + params.join('&') : '');
        window.history.replaceState(null, '', url);
        render();
    }

    function esc(v) {
        return String(v === undefined || v === null ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;');
    }

    function n(v) { return Number(v).toLocaleString('en-GB'); }

    function bytes(v) {
        if (v >= 1073741824) return (v / 1073741824).toFixed(1) + ' GB';
        if (v >= 1048576) return (v / 1048576).toFixed(1) + ' MB';
        if (v >= 1024) return (v / 1024).toFixed(0) + ' KB';
        return v + ' B';
    }

    function cas() { return CASES[S['case']]; }
    function actor() { return ACTORS[S.actor]; }

    function families() {
        var c = cas();
        if (c.sections) return [];
        var forced = S['case'] === 'one-dataset' ? ['swap'] : c.include;
        return FAMILIES.map(function (f) {
            var on = forced.indexOf(f.key) >= 0;
            if (S['case'] === 'onboard' && f.key === 'fx') {
                return Object.assign({}, f, { on: false, policy: 'skip', why: 'the tenant holds no FX conventions store, so the file\u2019s FX family is listed and skipped' });
            }
            return Object.assign({}, f, { on: on });
        });
    }

    function stageable() {
        var c = cas(), a = actor();
        if (S['case'] === 'feed') return a.can.market;
        if (S['case'] === 'one-dataset') return true;
        if (S['case'] === 'onboard') return a.can.conventions || a.can.trades;
        return a.can.trades && a.can.conventions && a.can.market && a.can.run;
    }

    /* --------------------------------------------------------- the book tree */

    function sandboxById(id) {
        return SANDBOXES.filter(function (s) { return s.id === id; })[0];
    }

    function officialById(id) {
        return OFFICIAL.filter(function (p) { return p.id === id; })[0];
    }

    function officialChildren(id) {
        return OFFICIAL.filter(function (p) { return p.parent === id; });
    }

    function officialBooks(id) {
        return OFFICIAL_BOOKS.filter(function (b) { return b.portfolio === id; });
    }

    function sandboxesAt(id) {
        return SANDBOXES.filter(function (s) { return s.anchor === id; });
    }

    function sandboxBooks(sb) {
        var out = [];
        sb.roots.forEach(function (p) { out = out.concat(p.books || []); });
        return out;
    }

    function anchorPath(sb) {
        var names = [], p = officialById(sb.anchor);
        while (p) { names.unshift(p.name); p = officialById(p.parent); }
        return names.join(' \u203a ');
    }

    function targetPath(sb) { return anchorPath(sb) + ' \u203a ' + sb.name; }

    function plural(count, one, many) { return count + ' ' + (count === 1 ? one : many); }

    function selectedSandbox() { return sandboxById(S.target); }

    var OPEN = {};

    function treeOpen(id) { return OPEN[id] !== false; }

    function bookTree() {
        var html = '';

        function pad(depth) { return 'padding-left:' + (8 + depth * 16) + 'px'; }

        function walk(id, depth) {
            var p = officialById(id);
            var kids = officialChildren(id);
            var books = officialBooks(id);
            var open = treeOpen(id);
            var hasKids = kids.length > 0 || books.length > 0 || sandboxesAt(id).length > 0;
            html += '<div class="trow official" style="' + pad(depth) + '" data-act="pick-official" data-node="' + esc(p.name) + '">' +
                '<button type="button" class="twisty' + (hasKids ? '' : ' leaf') + '" data-act="tree-toggle" data-node="' + esc(id) + '">' +
                (open ? '\u25be' : '\u25b8') + '</button>' +
                '<span class="tname">' + esc(p.name) + '</span>' +
                '<span class="count">' + (officialBooks(id).length ? officialBooks(id).length + ' books' : '') + '</span>' +
                '<span class="tmeta"><span class="tag off">official</span> not an import target</span></div>';
            if (!open) return;
            kids.forEach(function (c) { walk(c.id, depth + 1); });
            books.forEach(function (b) {
                html += '<div class="trow bk official" style="' + pad(depth + 1) + '" data-act="pick-official" data-node="' + esc(b.name) + '">' +
                    '<span class="twisty leaf">\u25b8</span>' +
                    '<span class="tname">' + esc(b.name) + '</span>' +
                    '<span class="tmeta">official \u00b7 ledger ' + esc(b.gl) + '</span></div>';
            });
            sandboxesAt(id).forEach(function (sb) { sandboxBranch(sb, depth + 1); });
        }

        function sandboxBranch(sb, depth) {
            var open = treeOpen(sb.id);
            var locked = sb.status === 'archived';
            var books = sandboxBooks(sb);
            html += '<div class="trow sandbox' + (S.target === sb.id ? ' on' : '') + (locked ? ' locked' : '') +
                '" style="' + pad(depth) + '" ' + (locked ? 'data-act="pick-archived"' : 'data-act="pick-sandbox"') +
                ' data-sb="' + esc(sb.id) + '">' +
                '<button type="button" class="twisty" data-act="tree-toggle" data-node="' + esc(sb.id) + '">' +
                (open ? '\u25be' : '\u25b8') + '</button>' +
                '<span class="tname">' + esc(sb.name) + '</span>' +
                '<span class="tag ' + (locked ? 'lock' : 'sb') + '">' +
                (locked ? 'archived \u00b7 read-only' : 'sandbox \u00b7 import target') + '</span>' +
                '<span class="tmeta">' + esc(sb.purpose) + ' \u00b7 ' + plural(sb.roots.length, 'portfolio', 'portfolios') +
                ' \u00b7 ' + plural(books.length, 'virtual book', 'virtual books') + '</span></div>';
            if (!open) return;
            sb.roots.forEach(function (p) {
                var parent = p.parent ? sb.roots.filter(function (x) { return x.id === p.parent; })[0] : null;
                html += '<div class="trow sandbox" style="' + pad(depth + 1) + '" data-act="pick-inside" data-node="' + esc(p.name) + '">' +
                    '<span class="twisty leaf">\u25b8</span><span class="tname">' + esc(p.name) + '</span>' +
                    '<span class="tag off">' + (parent ? 'sandbox portfolio' : 'root \u00b7 no parent') + '</span>' +
                    '<span class="tmeta">' + esc(p.ccy) + (parent ? ' \u00b7 under ' + esc(parent.name) : '') + '</span></div>';
                (p.books || []).forEach(function (b) {
                    html += '<div class="trow bk sandbox" style="' + pad(depth + 2) + '" data-act="pick-inside" data-node="' + esc(b.name) + '">' +
                        '<span class="twisty leaf">\u25b8</span><span class="tname">' + esc(b.name) + '</span>' +
                        '<span class="tmeta">virtual \u00b7 no ledger ref \u00b7 not sweepable</span></div>';
                });
            });
        }

        OFFICIAL.filter(function (p) { return p.parent === null; }).forEach(function (p) { walk(p.id, 0); });

        return '<div class="tree scroll" role="tree" aria-label="Book tree">' + html +
            '<div class="treelegend">' +
            '<span><span class="tag sb">sandbox</span> selectable: the import writes inside it</span>' +
            '<span><span class="tag off">official</span> not selectable: nothing official changes</span>' +
            '<span><span class="tag lock">archived</span> read-only by the insert trigger</span>' +
            '<span>A sandbox you cannot see is absent, not empty.</span>' +
            '</div></div>';
    }

    function refusalNotice() {
        if (!S.refuse) return '';
        var why;
        if (S.refuse.kind === 'official') {
            why = 'An official node is read-only for an import. An import publishes inside a sandbox, so no official book and no official portfolio moves. ' +
                'The target is a sandbox branch.';
        } else if (S.refuse.kind === 'archived') {
            why = 'This sandbox is archived. The insert trigger refuses a portfolio or a book in an archived sandbox, so it cannot receive an import. ' +
                'Choose an open sandbox.';
        } else {
            why = 'The import target is the sandbox, not one of its portfolios or books. The import creates its own portfolios inside the sandbox.';
        }
        return '<div class="notice error mt"><span class="title">' + esc(S.refuse.name) + ' is not a target</span>' + why + '</div>';
    }

    /* What the import will create in the selected sandbox, and what stays
     * official. The counts are the case's; the sandbox fields are the model's. */
    function targetDetail() {
        var sb = selectedSandbox();
        var created = CREATES[S['case']];
        var books = sandboxBooks(sb), pfs = sb.roots;
        var out = '<div class="notice success"><span class="title">Selected sandbox</span>' +
            '<span class="sbpath"><b>' + esc(targetPath(sb)) + '</b></span><br>' +
            '<span class="faint">Anchored at ' + esc(anchorPath(sb)) + '. The sandbox is not a node of the official tree.</span></div>' +
            '<table class="kv">' +
            '<tr><td>Sandbox</td><td class="mono">' + esc(sb.id) + '</td></tr>' +
            '<tr><td>Purpose</td><td>' + esc(sb.purpose) + ' \u00b7 ' + esc(sb.status) + ' \u00b7 ' + esc(sb.visibility) + '</td></tr>' +
            '<tr><td>Owner</td><td>' + esc(sb.owner) + '</td></tr>' +
            '<tr><td>Anchor</td><td>' + esc(anchorPath(sb)) + '</td></tr>' +
            '<tr><td>Root portfolio</td><td>' + esc(pfs[0].name) + ' <span class="faint">\u2014 no parent</span></td></tr>' +
            '<tr><td>In the sandbox now</td><td>' + plural(pfs.length, 'portfolio', 'portfolios') + ' \u00b7 ' +
            plural(books.length, 'virtual book', 'virtual books') + '</td></tr>' +
            '</table>';
        if (created.books || created.portfolios) {
            out += '<div class="makerbox mt"><span class="who">This import creates, inside ' + esc(sb.name) + '</span>' +
                '<ul class="sbcontents">' +
                '<li><b>' + created.portfolios + ' virtual portfolios</b> \u2014 each written with <span class="mono">sandbox_id = ' + esc(sb.id) + '</span>; the root it creates has no parent.</li>' +
                '<li><b>' + created.books + ' virtual books</b> \u2014 each written with <span class="mono">sandbox_id = ' + esc(sb.id) + '</span>, no ledger reference, not sweepable.</li>' +
                '<li><b>0 official rows</b> \u2014 no official book, portfolio or ledger balance changes.</li>' +
                '</ul></div>';
        } else {
            out += '<div class="notice warn mt"><span class="title">This file creates nothing in the sandbox</span>' +
                'It carries no books and no portfolios. The sandbox target covers the book-tree families; this file carries none of them, ' +
                'so the sandbox keeps the rows it already holds.</div>';
        }
        out += '<div class="notice warn"><span class="title">The conventions and market families have no sandbox column</span>' +
            'The model gives <span class="mono">sandbox_id</span> to books and portfolios only. The convention, curve, market and run families ' +
            'in this file still publish to the party, not into the sandbox. The selected sandbox governs the book-tree families alone.</div>';
        return out;
    }

    function targetCard() {
        return '<div class="card mt"><h2>The target: a sandbox on the book tree</h2>' +
            '<p class="lead">An import always publishes. It publishes inside a sandbox, never into the official tree, so the person picks a ' +
            'sandbox branch and sees the official nodes it hangs from. A sandbox is anchored at one official portfolio; it is not a node of the official tree.</p>' +
            '<div class="split"><div>' + bookTree() + refusalNotice() +
            '<div class="notice info mt mb0"><span class="title">The two trees cannot mix</span>' +
            'A book and its portfolio belong to the same sandbox or both to none. A sandbox\u2019s root portfolios have no parent. ' +
            'A virtual book carries no ledger reference and is not sweepable \u2014 the store refuses one that does. ' +
            'The sandbox\u2019s rows are read only by the account that may see the sandbox.</div></div>' +
            '<div>' + targetDetail() + '</div></div>' +
            '<div class="proto-hint">Mock: the tree, the sandbox ids and the counts are invented. The rules are the model\u2019s: a sandbox is anchored ' +
            'at an official portfolio, an official book never sits under a sandbox portfolio, and a book and its portfolio share one sandbox.</div>' +
            '</div>';
    }

    /* ------------------------------------------------------------- render */

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 Bring data in \u00b7 mock data, no backend, no live rows written';
        document.getElementById('app').innerHTML =
            '<div class="page wide">' + head() + (S.step === 'families' ? familyStep() :
                S.step === 'refused' ? refusedStep() : sourceStep()) + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
    }

    function head() {
        var c = cas();
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Signed in as <b>' + esc(actor().name) + '</b> \u00b7 ' +
            esc(actor().part) + ' \u00b7 party <b>' + esc(actor().party) + '</b></div>' +
            '<h1>Bring data in</h1>' +
            '<p class="lede">' + esc(c.gist) + ' The screen names the source, the scope, the families and the sandbox target before anything is staged. ' +
            'Staging writes no live row.</p>' +
            gapbanner() +
            stepstrip() +
            strip() +
            gapForCase();
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>No import screen exists in the product today. The entry points are the shell verbs ' +
            '<span class="mono">ore upload</span>, <span class="mono">ore import</span>, <span class="mono">ore import-run</span>, ' +
            '<span class="mono">marketdata import</span> and <span class="mono">compute download-input</span>, and the web UI has no import route. ' +
            'This screen is where that gap is made visible. Its count of steps is a proposal, not a description.</div>';
    }

    function stepstrip() {
        var step = S.step;
        function s(nm, key) {
            var cls = step === key ? 'now' : (step === 'families' && key === 'source') || (step === 'refused' && key !== 'refused') ? 'done' : '';
            return '<span class="s ' + cls + '">' + nm + '</span>';
        }
        return '<div class="stepstrip">' + s('1 Choose the source', 'source') +
            '<span class="arrow">\u203a</span>' + s('2 Scope the families', 'families') +
            '<span class="arrow">\u203a</span><span class="s">3 Stage for review</span>' +
            '<span class="arrow">\u203a</span><span class="s">4 Review and publish</span></div>';
    }

    function strip() {
        var c = cas(), rows = 0, live = 0;
        if (c.sections) {
            c.sections.forEach(function (s) { rows += s.rows; });
        } else {
            families().forEach(function (f) {
                if (c.include.indexOf(f.key) >= 0 || S['case'] === 'one-dataset') { if (f.key !== 'fx') rows += f.rows; }
                if (f.on) live += f.live;
            });
        }
        return '<div class="strip">' +
            cell('Source', c.sourceKind, c.sourceName) +
            cell('Received', bytes(c.bytes), c.parts + ' parts \u00b7 ' + c.env) +
            cell('Carries', c.sections ? rows.toLocaleString('en-GB') + ' rows' : rows + ' convention rows', c.sections ? '2 sections' : families().filter(function (f) { return f.on; }).length + ' families of 27') +
            cell('Live now', live.toLocaleString('en-GB'), c.sections ? 'rows across the series' : 'rows in this party') +
            '</div>';
    }

    function cell(k, v, s) {
        return '<div class="cell"><div class="k">' + esc(k) + '</div><div class="v">' + esc(v) + '</div><div class="s">' + esc(s) + '</div></div>';
    }

    function gapForCase() {
        var c = cas();
        return '<div class="notice info"><span class="title">What this case needs, and what no entry point offers today</span>' +
            '<ul style="margin:6px 0 0 18px;padding:0">' +
            c.gaps.map(function (g) { return '<li>' + esc(g) + '</li>'; }).join('') + '</ul></div>';
    }

    function sourceStep() {
        return '<div class="split">' +
            '<div>' + scopeCard() + sourceCard() + '</div>' +
            '<div>' + whoCard() + orderCard() + '</div>' +
            '</div>' + targetCard() + workflowCard() + foot();
    }

    /* The definition the import will run under, and the steps it declares.
     * The engine builds the list from the request at start and rebuilds the
     * same list on every advance, so this is what the person can expect before
     * anything runs. */
    function wfSteps() {
        var c = cas();
        var out = [{
            name: 'begin_import', label: 'Begin the import',
            subject: 'refdata.v1.steps.begin_conventions_import', budget: WRITE_STEP
        }];
        if (c.sections) {
            c.sections.forEach(function (s) {
                out.push({
                    name: 'stage_' + s.key,
                    label: 'Stage ' + (s.key === 'quotes' ? 'quotes (market.txt)' : 'fixings (fixings.txt)'),
                    subject: 'marketdata.v1.steps.stage_market_' + s.key, budget: DATA_STEP
                });
            });
        } else {
            FAMILIES.forEach(function (f) {
                if (S['case'] === 'one-dataset' && f.key !== 'swap') return;
                var listed = c.include.indexOf(f.key) >= 0;
                if (!listed && f.policy !== 'skip') return;
                out.push({
                    name: 'stage_' + f.key, label: 'Stage ' + f.name,
                    subject: 'refdata.v1.steps.stage_conventions_' + f.key,
                    budget: DATA_STEP, skipped: !listed
                });
            });
        }
        (c.partKeys || []).forEach(function (key) {
            if (key === 'conventions') return;
            if (key === 'market' && c.sections) return;
            var st = PART_STEPS[key];
            out.push({ name: st.name, label: st.label, subject: st.subject, budget: DATA_STEP });
        });
        out.push({
            name: 'commit_import', label: 'Commit every family in one transaction',
            subject: 'refdata.v1.steps.commit_conventions_import', budget: DATA_STEP
        });
        out.push({
            name: 'record_import_outcome', label: 'Record the outcome and tell the person',
            subject: 'refdata.v1.steps.record_import_outcome', budget: WRITE_STEP
        });
        return out;
    }

    function workflowCard() {
        var steps = wfSteps();
        return '<div class="card mt"><h2>The workflow this import starts</h2>' +
            '<p class="lead">The import runs as one instance of a registered definition. The engine builds the step list from ' +
            'the request at start, and every advance rebuilds the same list, so these are the steps the person can expect.</p>' +
            '<div class="strip">' +
            '<div class="cell"><div class="k">Workflow type</div><div class="v mono" style="font-size:13px">' + WF_TYPE + '</div>' +
            '<div class="s">proposed \u00b7 not registered</div></div>' +
            '<div class="cell"><div class="k">Start message</div><div class="v mono" style="font-size:13px">workflow.v1.ops.start_workflow</div>' +
            '<div class="s">committed to the stream before the caller is answered</div></div>' +
            '<div class="cell"><div class="k">Failure policy</div><div class="v">stop</div>' +
            '<div class="s">a failed step parks the run in failed, completed steps kept</div></div>' +
            '<div class="cell"><div class="k">Steps declared</div><div class="v">' + steps.length + '</div>' +
            '<div class="s">built from the request at start</div></div>' +
            '</div>' +
            '<table class="grid famtable"><thead><tr><th class="chk">#</th><th>Step</th><th>Subject</th><th>Budget</th><th>State</th></tr></thead><tbody>' +
            steps.map(function (s, i) {
                return '<tr><td class="n">' + (i + 1) + '</td>' +
                    '<td class="nm">' + esc(s.label) + '<span class="ent">' + esc(s.name) + '</span></td>' +
                    '<td class="mono">' + esc(s.subject) + '</td>' +
                    '<td class="n">' + (s.budget === DATA_STEP ? '15 min' : '2 min') + '</td>' +
                    '<td>' + (s.skipped ? '<span class="pill warn">skipped</span>' : '<span class="pill">pending</span>') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="notice info mt"><span class="title">Today no such definition exists.</span>' +
            'The engine registers <span class="mono">run_configuration_import_workflow</span> and ' +
            '<span class="mono">ore_import_workflow</span>, one step each, so an import is one step and a retry redoes all of it. ' +
            'The family-per-step list above follows the draft design, and no run can show it yet.</div>' +
            '<div class="stepfoot"><span class="hint">Progress lives on the run screen, because the run is the attempt and the ' +
            'engine exposes one attempt per instance.</span>' +
            '<span class="ml-auto"><a class="btn ghost" href="../one-import-run/index.html?case=' + esc(S['case']) +
            '&amp;target=' + esc(S.target) + '&amp;wf=staged">Follow the run</a></span></div>' +
            '<div class="proto-hint">Mock: the type, the step subjects and the budgets are a proposal. The state names and the ' +
            'progress read are the engine\u2019s own, and the run screen draws them.</div>' +
            '</div>';
    }

    function scopeCard() {
        var c = cas(), a = actor();
        return '<div class="card"><h2>The scope</h2>' +
            '<p class="lead">The session decides the party. The file does not carry one.</p>' +
            '<div class="grid2">' +
            field('Party', a.party + ' (the session\u2019s party)', true) +
            field('Source', c.env) +
            field('Source kind', c.sourceKind) +
            field('Content', c.sourceName) +
            '</div>' +
            '<div class="notice warn mb0"><span class="title">A party named in the command is refused</span>' +
            'The shell refuses <span class="mono">--party-id</span> for any party other than the session\u2019s and names the session\u2019s party. ' +
            'A tenant administrator who needs to stage another party switches to it first. The prototype keeps that rule.</div>' +
            '</div>';
    }

    function field(label, value, ro) {
        return '<div class="field' + (ro ? ' ro' : '') + '"><span class="lbl">' + esc(label) + '</span>' +
            '<input value="' + esc(value) + '"' + (ro ? ' readonly' : '') + '></div>';
    }

    function sourceCard() {
        var c = cas();
        var kind = '<div class="toolbar"><label class="faint" for="src">Source</label>' +
            '<select id="src" data-act="source">' +
            ['ORE input directory', 'Export bundle (.orebundle)', 'Promotion bundle', 'Folder of sample trees', 'Two market-data files', 'One ORE file']
                .map(function (k) { return '<option' + (k === c.sourceKind ? ' selected' : '') + '>' + esc(k) + '</option>'; }).join('') +
            '</select><span class="grow"></span><span class="pill info">read only, nothing staged</span></div>';

        var scan = S['case'] === 'seed' && !S.scan ? '<div class="notice warn">The scanner result is hidden. ' +
            'The journey says the scan is invisible today, so the switch shows what the person gets now. ' +
            '<button class="btn small ghost" data-act="scan" data-on="1">Show the scan</button></div>' : scanTable(c);

        return '<div class="card mt"><h2>The source</h2>' +
            '<p class="lead">What was received, what the reader will read, and what it will ignore.</p>' +
            kind + dropzone(c) + scan + '</div>';
    }

    function dropzone(c) {
        return '<div class="dropzone"><b>' + esc(c.sourceName) + '</b><br>' +
            bytes(c.bytes) + ' \u00b7 ' + c.parts + ' parts \u00b7 ' + esc(c.env) +
            '<br><span class="mono">sha256 ' + (S['case'] === 'defect' ? '9f2c\u2026e41a' : '4b71\u2026c08d') + '</span></div>';
    }

    function scanTable(c) {
        if (S['case'] === 'feed') {
            return '<div class="scan"><div class="scanrow head"><span>File</span><span>Kind</span><span>Rows</span><span>Disposition</span></div>' +
                c.files.map(function (f) {
                    return '<div class="scanrow' + (f.use === 'ignored' ? ' ignored' : '') + '"><span class="fn">' + esc(f.name) +
                        (f.why ? '<span class="sub">' + esc(f.why) + '</span>' : '') + '</span><span>' + esc(f.kind) +
                        '</span><span>' + esc(f.rows) + '</span><span>' + pill(f.use) + '</span></div>';
                }).join('') + '</div>';
        }
        return '<div class="scan"><div class="scanrow head"><span>File</span><span>Kind</span><span>Rows</span><span>Disposition</span></div>' +
            c.files.map(function (f) {
                return '<div class="scanrow' + (f.use === 'ignored' ? ' ignored' : '') + '"><span class="fn">' + esc(f.name) +
                    (f.why ? '<span class="sub">' + esc(f.why) + '</span>' : '') + '</span><span>' + esc(f.kind) +
                    '</span><span>' + esc(f.rows) + '</span><span>' + pill(f.use) + '</span></div>';
            }).join('') + '</div>';
    }

    function pill(use) {
        return use === 'accepted' ? '<span class="pill ok">read</span>' : '<span class="pill warn">ignored</span>';
    }

    function whoCard() {
        var a = actor();
        var rows = [
            ['Conventions', a.can.conventions, 'refdata::conventions:write'],
            ['Trades and portfolios', a.can.trades, 'trading::trade:write'],
            ['Curves', a.can.curves, 'refdata::curve_configuration_documents:write'],
            ['Market data', a.can.market, 'marketdata::import:write'],
            ['Run documents', a.can.run, 'configure::run_document:write']
        ];
        return '<div class="card"><h2>Who may act</h2>' +
            '<p class="lead">The same subject, a narrower scope: a party-scoped user needs a permission the seeded roles do not grant.</p>' +
            '<table class="kv">' + rows.map(function (r) {
                return '<tr><td>' + esc(r[0]) + '</td><td><span class="mono">' + esc(r[2]) + '</span> ' +
                    (r[1] ? '<span class="pill ok">held</span>' : '<span class="pill bad">not held</span>') + '</td></tr>';
            }).join('') + '</table>' +
            '<div class="notice ' + (a.can.trades && a.can.conventions && a.can.market && a.can.run ? 'success' : 'warn') + ' mt mb0">' +
            esc(a.note) + '</div>' +
            '<div class="proto-hint">No role today means "may import for a party". An import borrows the write permission of every row it creates, ' +
            'so granting the import grants every write. The owner must decide whether an import permission of its own is added.</div>' +
            '</div>';
    }

    function orderCard() {
        var c = cas();
        if (c.sections) {
            return '<div class="card mt"><h2>What the file carries</h2>' +
                '<p class="lead">Two sections, one source tag. A duplicate in one section must never discard the other.</p>' +
                '<table class="grid"><thead><tr><th>Section</th><th>Series</th><th>Rows</th><th>Merge</th><th>Live</th></tr></thead><tbody>' +
                c.sections.map(function (s) {
                    return '<tr><td>' + esc(s.name) + '</td><td class="n">' + n(s.series) + '</td><td class="n">' + n(s.rows) +
                        '</td><td><span class="pill">' + esc(s.policy) + '</span></td><td class="n">' + (s.key === 'quotes' ? '18,204' : '1,190') + '</td></tr>';
                }).join('') + '</tbody></table>' +
                '<div class="notice warn mt mb0">A file may repeat rows. The person passes <span class="mono">--duplicates-are-errors</span> ' +
                'to refuse them; without it, the store reports what it kept. The screen must say which of the two ran.</div>' +
                '</div>';
        }
        var ordered = families().filter(function (f) { return f.on; });
        var total = ordered.reduce(function (a, f) { return a + f.rows; }, 0);
        return '<div class="card mt"><h2>What the file carries</h2>' +
            '<p class="lead">' + ordered.length + ' families, ' + n(total) + ' rows, applied parent before part.</p>' +
            '<table class="grid famtable"><thead><tr><th>Family</th><th>Rows</th><th>Live</th><th>Merge</th></tr></thead><tbody>' +
            ordered.map(function (f) {
                return '<tr><td class="nm">' + esc(f.name) + '<span class="ent">refdata.' + esc(f.ent) + '</span></td>' +
                    '<td class="n">' + n(f.rows) + '</td><td class="n">' + n(f.live) + '</td>' +
                    '<td><span class="pill' + (f.policy === 'skip' ? ' warn' : '') + '">' + esc(f.policy) + '</span></td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="notice info mt mb0"><span class="title">Order is a promise this screen makes</span>' +
            'A conventions family precedes the curves that read it, and a portfolio precedes the trades under it. ' +
            'The proposed import applies families in registry order inside one transaction; today the person must know the order, and a wrong one fails late.</div>' +
            '</div>';
    }

    function foot() {
        var a = actor(), can = stageable();
        return '<div class="stepfoot"><span class="hint">Nothing is written yet. Staging keeps the rows in the import\u2019s own area; ' +
            'a reader of the party sees no change.</span>' +
            '<span class="ml-auto"><button class="btn ghost" data-act="case" data-case="' +
            esc(CASE_ORDER[(CASE_ORDER.indexOf(S['case']) + 1) % CASE_ORDER.length]) + '">Another trigger</button> ' +
            '<button class="btn primary" data-act="families"' + (can ? '' : ' disabled') + '>Choose the families</button></span></div>' +
            (can ? '' : '<div class="notice error mt">This actor cannot stage this file. The refusal names the first write the session ' +
                'cannot make; it is not an empty success. The prototype keeps the refusal instead of hiding the control.</div>');
    }

    function familyStep() {
        var c = cas(), a = actor();
        if (c.sections) return feedFamilyStep();
        var list = families();
        var chosen = list.filter(function (f) { return f.on; });
        return '<div class="split">' +
            '<div class="card"><h2>Families in the file</h2>' +
            '<p class="lead">Every family is shown. A family left out is left out of the import, not deleted from the party.</p>' +
            '<table class="grid famtable"><thead><tr><th class="chk"></th><th>Family</th><th>Rows</th><th>Live</th><th>Policy</th></tr></thead><tbody>' +
            list.map(function (f) {
                var disabled = S['case'] === 'one-dataset' || f.policy === 'skip';
                return '<tr class="' + (f.on ? '' : 'off') + '"><td class="chk">' +
                    '<input type="checkbox" data-act="toggle" data-key="' + esc(f.key) + '"' + (f.on ? ' checked' : '') +
                    (disabled ? ' disabled' : '') + '></td>' +
                    '<td class="nm">' + esc(f.name) + '<span class="ent">refdata.' + esc(f.ent) + '</span>' +
                    (f.why ? '<span class="ent">' + esc(f.why) + '</span>' : '') + '</td>' +
                    '<td class="n">' + n(f.rows) + '</td><td class="n">' + n(f.live) + '</td>' +
                    '<td><span class="pill' + (f.policy === 'skip' ? ' warn' : f.policy === 'add_missing_world' ? ' info' : '') + '">' +
                    esc(f.policy) + '</span></td></tr>';
            }).join('') + '</tbody></table>' +
            (S['case'] === 'one-dataset' ? '<div class="notice info mt mb0">This journey chooses one dataset, so the scope is fixed to the file\u2019s one family. ' +
                'The other families are not in the file and stay untouched. Today there is no family scope at all.</div>' : '') +
            '</div>' +
            '<div>' + summaryCard(chosen) + '</div>' +
            '</div>' + workflowCard() +
            '<div class="stepfoot"><button class="btn ghost" data-act="back">Back to the source</button>' +
            '<span class="ml-auto"><button class="btn ghost" data-act="refused">Refuse a row</button> ' +
            '<button class="btn primary" data-act="stage">Stage for review</button></span></div>';
    }

    function summaryCard(chosen) {
        var rows = chosen.reduce(function (a, f) { return a + f.rows; }, 0);
        var sb = selectedSandbox(), created = CREATES[S['case']];
        return '<div class="card"><h2>What will be staged</h2>' +
            '<p class="lead">A staged import is durable and costs nothing to retry. Nothing here touches the live tables.</p>' +
            '<div class="notice info"><span class="title">It publishes inside ' + esc(sb.name) + '</span>' +
            '<span class="sbpath">' + esc(targetPath(sb)) + '</span><br>' +
            'The book-tree part creates ' + created.portfolios + ' virtual portfolios and ' + created.books +
            ' virtual books there, every row with <span class="mono">sandbox_id = ' + esc(sb.id) + '</span>. ' +
            'No official row changes. <button class="btn small ghost" data-act="back">Change the sandbox</button></div>' +
            '<div class="countgrid"><div class="c"><div class="k">Families</div><div class="v">' + chosen.length + '</div></div>' +
            '<div class="c"><div class="k">Staged rows</div><div class="v">' + n(rows) + '</div></div>' +
            '<div class="c"><div class="k">Live rows changed</div><div class="v">0</div></div></div>' +
            '<div class="notice info mt"><span class="title">Nothing is visible to a reader</span>' +
            'A staged row lives in the import\u2019s own area until the commit. The commit applies every family in one transaction, so a failure leaves no row of any family written.</div>' +
            '<div class="proto-hint">Gap: there is no staging area today, no begin, stage, commit or discard verb, and no expiry or sweep. ' +
            'The draft design proposes them; the journeys ask for them. This screen is the first place they would show.</div>' +
            '</div>';
    }

    function feedFamilyStep() {
        var c = cas();
        return '<div class="split"><div class="card"><h2>Sections in the files</h2>' +
            '<p class="lead">A section is the unit the store keeps: a duplicate in one must not discard the other.</p>' +
            '<table class="grid"><thead><tr><th>Section</th><th>File</th><th>Series</th><th>Rows</th><th>Live</th></tr></thead><tbody>' +
            c.sections.map(function (s) {
                return '<tr><td class="nm">' + esc(s.name) + '</td><td class="mono">' + (s.key === 'quotes' ? 'market.txt' : 'fixings.txt') +
                    '</td><td class="n">' + n(s.series) + '</td><td class="n">' + n(s.rows) + '</td><td class="n">' +
                    (s.key === 'quotes' ? '18,204' : '1,190') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="notice warn mt">The source tag is not in the file. The person names it or it stays empty; the import never invents one.</div></div>' +
            '<div class="card"><h2>What will be staged</h2>' +
            '<div class="countgrid"><div class="c"><div class="k">Sections</div><div class="v">2</div></div>' +
            '<div class="c"><div class="k">Staged rows</div><div class="v">19,646</div></div>' +
            '<div class="c"><div class="k">Live rows changed</div><div class="v">0</div></div></div>' +
            '<div class="field mt"><span class="lbl">Source tag</span><input value="vendor-acme-2026-10-16" readonly></div>' +
            '<div class="proto-hint">Gap: no native CSV or JSON import, no vendor connector, no per-section result and no dry run.</div></div></div>' +
            workflowCard() +
            '<div class="stepfoot"><button class="btn ghost" data-act="back">Back to the source</button>' +
            '<span class="ml-auto"><button class="btn primary" data-act="stage">Stage for review</button></span></div>';
    }

    function refusedStep() {
        var a = actor();
        return '<div class="card"><h2>Staged with refusals</h2>' +
            '<p class="lead">The stage accepted every row, and the validator names the rows it cannot apply. Nothing is live.</p>' +
            '<div class="notice error"><span class="title">2 rows will be refused at the commit</span>' +
            '<span class="mono">swap_conventions \u00b7 GBP-12M-SWAP</span> names business day convention <span class="mono">MODFOLLOWINGX</span>, ' +
            'which the list no longer holds.<br><span class="mono">deposit_conventions \u00b7 JPY-TONAR</span> names currency ' +
            '<span class="mono">XJPY</span>, which the tenant does not hold.</div>' +
            '<div class="notice warn"><span class="title">A skipped family is reported, not dropped</span>' +
            'The file carries 6 FX conventions. The target has no FX conventions store, so the family is skipped and listed. ' +
            'Today the reply carries <span class="mono">fx_skipped</span> and no screen and no shell line prints it.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="back">Back to the families</button>' +
            '<span class="ml-auto"><button class="btn primary" data-act="stage" disabled>The commit will refuse the file</button></span></div>' +
            '<div class="proto-hint" style="margin-top:18px">The refusal is reported before the commit, which is the review journey\u2019s whole demand. ' +
            'Today the person discovers it while writing, one family at a time, with no family name in the reply.</div>' +
            '</div>';
    }

    /* ---------------------------------------------------------------- bar */

    function bar() {
        var out = '<span class="label">Trigger <b>' + esc(cas().title) + '</b></span>';
        CASE_ORDER.forEach(function (k) {
            out += '<button data-act="case" data-case="' + k + '"' + (k === S['case'] ? ' class="on"' : '') + '>' + esc(CASES[k].title) + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">Actor</span>';
        ACTOR_ORDER.forEach(function (k) {
            out += '<button data-act="actor" data-actor="' + k + '"' + (k === S.actor ? ' class="on"' : '') + '>' + esc(ACTORS[k].part) + '</button>';
        });
        out += '<span class="sep">|</span>';
        out += '<button data-act="step" data-step="source"' + (S.step === 'source' ? ' class="on"' : '') + '>Source</button>';
        out += '<button data-act="step" data-step="families"' + (S.step === 'families' ? ' class="on"' : '') + '>Families</button>';
        if (S['case'] === 'seed') out += '<button data-act="scan" data-on="' + (S.scan ? '0' : '1') + '">' + (S.scan ? 'Hide scan' : 'Show scan') + '</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    document.addEventListener('input', function (e) {
        var t = e.target;
        if (t.dataset && t.dataset.act === 'source') {
            var map = {
                'ORE input directory': 'onboard', 'Export bundle (.orebundle)': 'defect',
                'Promotion bundle': 'promote', 'Folder of sample trees': 'seed',
                'Two market-data files': 'feed', 'One ORE file': 'one-dataset'
            };
            if (map[t.value]) go({ 'case': map[t.value], step: 'source', family: null, refuse: null, target: TARGET_BY_CASE[map[t.value]] });
        }
    });

    document.addEventListener('change', function (e) {
        var t = e.target;
        if (t.dataset && t.dataset.act === 'toggle') {
            var on = t.checked;
            var key = t.dataset.key;
            var c = cas();
            if (on && c.include.indexOf(key) < 0) c.include.push(key);
            if (!on) c.include = c.include.filter(function (k) { return k !== key; });
            render();
        }
    });

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t) return;
        var act = t.dataset.act;
        if (act === 'case') go({ 'case': t.dataset.case, step: 'source', family: null, refuse: null, target: TARGET_BY_CASE[t.dataset.case] });
        else if (act === 'actor') go({ actor: t.dataset.actor });
        else if (act === 'step') go({ step: t.dataset.step });
        else if (act === 'families' && !t.disabled) go({ step: 'families' });
        else if (act === 'back') go({ step: 'source' });
        else if (act === 'refused') go({ step: 'refused' });
        else if (act === 'scan') go({ scan: t.dataset.on === '1' });
        else if (act === 'stage') {
            window.location.href = '../stage-an-import/index.html?case=' + encodeURIComponent(S['case']) +
                '&target=' + encodeURIComponent(S.target) +
                (S['case'] === 'party' ? '&actor=ops' : '');
        }
        else if (act === 'tree-toggle') {
            OPEN[t.dataset.node] = !treeOpen(t.dataset.node);
            render();
        }
        else if (act === 'pick-sandbox') {
            go({ target: t.dataset.sb, refuse: null });
        }
        else if (act === 'pick-official') {
            S.refuse = { kind: 'official', name: t.dataset.node };
            render();
        }
        else if (act === 'pick-archived') {
            S.refuse = { kind: 'archived', name: sandboxById(t.dataset.sb).name };
            render();
        }
        else if (act === 'pick-inside') {
            S.refuse = { kind: 'inside', name: t.dataset.node };
            render();
        }
    });

    render();
}());

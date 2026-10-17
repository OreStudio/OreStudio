/* Manage the analytics. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * This page owns the analytic interaction of a run settings configuration. The
 * two prototypes that used to repeat it hand the work here:
 *
 *   doc/prototypes/reporting/create-a-report/            step 7 links here
 *   doc/prototypes/reporting/change-what-a-report-runs/  its analytics step links
 *                                                       here
 *
 * The rail is computed from state: a switchboard step, then one step per
 * analytic that is on in display_order, then a final step that reads the set
 * back and offers the way out. Turning an analytic on adds its step and
 * renumbers the ones after it; turning one off removes it and asks first,
 * because its parameter values go with it.
 *
 * The analytics belong to the bind run settings configuration, not to a
 * definition. Every step says so.
 *
 * Three readings of the per-analytic step only, behind ?variant=A|B|C. The
 * switchboard and the rail are identical across the three.
 *
 *   ?step=N                               numeric over the current rail
 *   ?analytic=<code>                      that analytic's step, whatever its number
 *   ?variant=A|B|C                        which reading of the analytic step
 *   ?guide=N                              the setting variant C is showing
 *   ?pfe=without-simulation               open the broken pair
 *   ?from=create-a-report|change-what-a-report-runs
 *                                         where "Return to the report" goes
 */

(function () {
    'use strict';

    // ------------------------------------------------------------ mock data

    var NOW = new Date(2026, 9, 20, 14, 32, 0);

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };

    var TENANT = 'ACME Group (the session\u2019s tenant)';

    var PERSON = { name: 'Anneke Vos', role: 'Market-risk owner' };

    /* The bound run settings configuration. The analytic rows hang off this
       row, so this page changes what every definition bound to it runs. */
    var CONFIGURATION = {
        id: 'cfg-2d61f0a8',
        name: 'Group risk configuration',
        detail: 'base currency GBP, 4 threads, market data London close 2026-10-16',
        scope: 'Group Treasury Portfolio',
        boundBy: 4
    };

    /* The definitions bound to that configuration, from
       reporting.v1.report_configurations.list. The page names them, because
       "change this report" and "change these four reports" are different
       decisions. */
    var BOUND_DEFINITIONS = [
        'Group Consolidated Exposure',
        'Board Risk Dashboard',
        'Group DV01 by book',
        'Treasury Sensitivity'
    ];

    /* The fourteen analytic types, in the order the page draws them. Each
       carries what it produces, what it is, how it is calculated in plain
       words, what turning it on reaches, the analytic it needs, and every
       parameter it takes with the control its shape asks for.

       `curves` carries no parameter, which is why the page can show an analytic
       that takes none: it writes the curves the run already built, and which
       curves those are is decided by the curve configuration the definition
       binds. */
    var ANALYTICS = [
        {
            code: 'npv',
            name: 'Net present value',
            produces: 'One present value per trade, netted to book and to portfolio.',
            what: 'Net present value is what the book is worth today. Every trade is priced against ' +
                'today\u2019s curves and the results are added up the book tree.',
            how: 'Each trade\u2019s future cashflows are projected, discounted back at the discount ' +
                'curve, and summed. The sums roll up from trade to book to portfolio.',
            effect: 'Adds the headline number a desk reads first. It is the cheapest analytic here, ' +
                'and every other analytic on this configuration assumes it is produced.',
            params: [
                {
                    name: 'discount_curve',
                    shape: 'text',
                    value: 'ACME discount curve',
                    help: 'The curve the cashflows are discounted at. It names a curve in the ' +
                        'curve configuration bound beside this one.',
                    normal: 'A curve name, usually the party\u2019s own discount curve.'
                },
                {
                    name: 'include_cashflows',
                    shape: 'toggle',
                    value: 'false',
                    help: 'Write each projected cashflow as well as the total.',
                    normal: 'Off. Turn it on only when someone is going to read the cashflow rows.'
                }
            ]
        },
        {
            code: 'sensitivity',
            name: 'Sensitivity',
            produces: 'The change in value for a small shift in each market factor, such as DV01 per curve.',
            what: 'A sensitivity says how much the book moves when one market factor moves a little and ' +
                'everything else stays still. It is what a risk reader uses to see which factor the ' +
                'book is exposed to.',
            how: 'Every factor is shifted up and down by the same small amount, the book is repriced ' +
                'twice, and the difference is divided by the shift. The pair of numbers states a slope.',
            effect: 'Turns one repricing into two or more. It is the first analytic that costs a run ' +
                'measurably, and the one a limit conversation starts from.',
            params: [
                {
                    name: 'shift_size',
                    shape: 'number',
                    value: '0.0001',
                    help: 'The bump applied to every market factor, as a decimal.',
                    normal: '0.0001, which is one basis point.'
                },
                {
                    name: 'shift_type',
                    shape: 'select',
                    options: ['Absolute', 'Relative'],
                    value: 'Absolute',
                    help: 'Whether the bump is a number in the factor\u2019s own units or a share of ' +
                        'its level.',
                    normal: 'Absolute for rates and credit, Relative for prices.'
                }
            ]
        },
        {
            code: 'cashflow',
            name: 'Cashflows',
            produces: 'The projected cashflow rows of every trade, bucketed by date.',
            what: 'Cashflows are the money the book is contracted to pay and receive, listed by date. ' +
                'They are the raw material every other analytic is built from.',
            how: 'Each trade is projected to its payment dates, the amounts are placed in date buckets, ' +
                'and the buckets are netted per currency.',
            effect: 'Produces the rows a treasury reader works from. It is large: a run that carries ' +
                'cashflows writes far more output than one that does not.',
            params: [
                {
                    name: 'bucket',
                    shape: 'select',
                    options: ['1D', '1W', '1M'],
                    value: '1D',
                    help: 'How wide each date bucket is.',
                    normal: '1D for a daily risk report, 1M for a treasury view.'
                },
                {
                    name: 'include_fixings',
                    shape: 'toggle',
                    value: 'true',
                    help: 'Include the fixings already known for the valuation date.',
                    normal: 'On, unless the reader wants the forward-looking set alone.'
                }
            ]
        },
        {
            code: 'curves',
            name: 'Curves',
            produces: 'The built curves themselves, with their pillars and their discount factors.',
            what: 'A curve is a table of rates by tenor that the other analytics discount and project ' +
                'with. Reading the built curve is how a person checks that today\u2019s market data was ' +
                'read the way they expected.',
            how: 'The curve is built from its configuration and today\u2019s market data: the pillars are ' +
                'fitted, the interpolation is applied, and discount factors are written at each pillar.',
            effect: 'Writes a diagnostic table beside the figures. It changes no other analytic; it is ' +
                'the one place where a curve that was built wrongly can be seen.',
            params: []
        },
        {
            code: 'simulation',
            name: 'Simulation',
            produces: 'Simulated future paths of the market, and the exposure cube pfe reads.',
            what: 'Simulation draws many possible futures of the market, prices the book in each one, ' +
                'and keeps the results. It is the groundwork for pfe and for the VaR that is not ' +
                'parametric.',
            how: 'The market is moved forward along a set of paths, each path is priced at a grid of ' +
                'dates, and the values are stored as a cube of path by date by trade.',
            effect: 'The heaviest analytic here. It writes a cube, it makes the run take minutes rather ' +
                'than seconds, and pfe cannot run without it.',
            paramEditor: 'deferred',
            editorNeeds: 'the shift it applies and the time grid it writes the cube at',
            params: [
                {
                    name: 'number_of_paths',
                    shape: 'number',
                    value: '1000',
                    help: 'How many simulated paths the run draws.',
                    normal: '1000 for a development run, several thousand for a risk figure.'
                },
                {
                    name: 'seed',
                    shape: 'number',
                    value: '42',
                    help: 'The seed of the random draw, so a run can be repeated.',
                    normal: 'Any whole number, held fixed while a result is being compared.'
                },
                {
                    name: 'time_grid',
                    shape: 'select',
                    options: ['1D', '1W', '1M'],
                    value: '1D',
                    help: 'The step of the dates the cube is written at.',
                    normal: '1D for a daily exposure view.',
                    deferred: 'The time grid is drawn here as a plain select. It needs a ' +
                        'purpose-built editor on a later task, like the cube it feeds.'
                }
            ]
        },
        {
            code: 'saccr',
            name: 'SA-CCR',
            produces: 'The standardised counterparty credit exposure of each netting set.',
            what: 'SA-CCR is the regulatory formula for how much credit exposure a derivative book ' +
                'carries against a counterparty. It is not a simulation: it is a calculation on the ' +
                'trades as they stand.',
            how: 'Trades are grouped into netting sets, each set\u2019s replacement cost and potential ' +
                'future exposure are computed by the regulatory formula, and the two are added.',
            effect: 'Produces the regulatory number. It reads the trades and the netting sets and needs ' +
                'no other analytic.',
            params: [
                {
                    name: 'reporting_currency',
                    shape: 'text',
                    value: 'USD',
                    help: 'The currency the exposure is restated into.',
                    normal: 'The currency the report is read in, USD here.'
                },
                {
                    name: 'include_collateral',
                    shape: 'toggle',
                    value: 'true',
                    help: 'Reduce the exposure by the collateral held against each netting set.',
                    normal: 'On when the party has collateral agreements, which this one does.'
                }
            ]
        },
        {
            code: 'parametricVar',
            name: 'Parametric VaR',
            produces: 'One value at risk figure, computed from the sensitivities and a covariance matrix.',
            what: 'Parametric VaR answers "how much could this book lose in a day". It assumes the ' +
                'moves are normally distributed, so it is fast and it is wrong exactly when markets ' +
                'are not normal.',
            how: 'The sensitivities are combined through a covariance matrix of the factors, and the ' +
                'result is scaled to the holding period and read at the confidence level.',
            effect: 'Produces a number in the shape a board paper wants. It needs sensitivity to be on, ' +
                'because the sensitivities are its input.',
            params: [
                {
                    name: 'confidence',
                    shape: 'number',
                    value: '0.99',
                    help: 'The quantile the figure is read at.',
                    normal: '0.99 for a regulatory view, 0.95 for an internal one.'
                },
                {
                    name: 'horizon_days',
                    shape: 'number',
                    value: '1',
                    help: 'The holding period the figure covers, in days.',
                    normal: '1 for a trading desk, 10 for a regulatory figure.'
                },
                {
                    name: 'volatility_period_days',
                    shape: 'number',
                    value: '250',
                    help: 'How many days of history the covariance matrix is built from.',
                    normal: '250, which is one trading year.'
                }
            ]
        },
        {
            code: 'portfolioDetails',
            name: 'Portfolio details',
            produces: 'How many trades and how much notional each book and portfolio carries.',
            what: 'The size of the book, stated beside its value. A person reading a figure needs to ' +
                'know whether it covers three trades or three thousand.',
            how: 'The trades are counted and their notionals summed per book, and the books roll up ' +
                'through the portfolio tree.',
            effect: 'Produces the population figures. It is cheap, and a reader who does not see it ' +
                'cannot tell an empty book from a fully hedged one.',
            params: [
                {
                    name: 'grouping',
                    shape: 'select',
                    options: ['Book', 'Portfolio'],
                    value: 'Book',
                    help: 'The level the rows are drawn at.',
                    normal: 'Book, with the portfolios as the roll-up above them.'
                },
                {
                    name: 'include_empty_books',
                    shape: 'toggle',
                    value: 'false',
                    help: 'List books that carry no trade.',
                    normal: 'Off. On when the person is looking for a book that should have trades.'
                }
            ]
        },
        {
            code: 'creditcurves',
            name: 'Credit curves',
            produces: 'The survival probability and hazard rate curve of each counterparty.',
            what: 'A credit curve says how likely a counterparty is to be there in a year. It is what ' +
                'the credit analytics discount with.',
            how: 'Spreads observed in the market, or a recovery rate assumption, are fitted to a curve ' +
                'of survival probabilities by tenor.',
            effect: 'Produces the credit input the exposure analytics read. Without it a credit figure ' +
                'comes from a default assumption nobody stated.',
            params: [
                {
                    name: 'hazard_rate_source',
                    shape: 'select',
                    options: ['Spread', 'CDS'],
                    value: 'Spread',
                    help: 'Where the hazard rate comes from.',
                    normal: 'Spread where the party has quoted spreads, CDS otherwise.'
                },
                {
                    name: 'recovery_rate',
                    shape: 'number',
                    value: '0.4',
                    help: 'The share of the exposure recovered if the counterparty defaults.',
                    normal: '0.4, which is the common senior unsecured assumption.'
                }
            ]
        },
        {
            code: 'pfe',
            name: 'Potential future exposure',
            produces: 'An exposure profile per counterparty and date, and one figure at the horizon.',
            what: 'Potential future exposure is how large the book could become against a counterparty ' +
                'in the future. It is read against a limit, so it is the analytic a credit officer ' +
                'opens first.',
            how: 'It reads the cube simulation writes, takes a quantile across the simulated paths at ' +
                'each date, and keeps the profile and the figure at the horizon.',
            effect: 'Produces the exposure a limit is measured against. It cannot run unless simulation ' +
                'is on, because the cube is its whole input.',
            requires: 'simulation',
            paramEditor: 'deferred',
            editorNeeds: 'the cube it reads, which simulation writes',
            params: [
                {
                    name: 'horizon',
                    shape: 'number',
                    value: '10',
                    help: 'How many days forward the exposure is projected.',
                    normal: '10 days, or the maturity of the longest trade for a lifetime figure.'
                },
                {
                    name: 'quantile',
                    shape: 'number',
                    value: '0.95',
                    help: 'The confidence the exposure is read at.',
                    normal: '0.95. A limit conversation usually quotes 0.95 and 0.99 together.'
                },
                {
                    name: 'exposure_cube',
                    shape: 'text',
                    value: 'simulation-cube-01',
                    help: 'The cube this reads. The cube is written by simulation and named here.',
                    normal: 'The cube the simulation on this configuration wrote.',
                    deferred: 'The cube is drawn here as a plain text field. Choosing, comparing and ' +
                        'reusing a cube needs a purpose-built editor, which is a later task.'
                }
            ]
        },
        {
            code: 'calibration',
            name: 'Calibration',
            produces: 'The fitted model parameters, and how far the fit missed the market.',
            what: 'Calibration is fitting a model to what the market quotes. It is a diagnostic: a ' +
                'model that fits badly will misprice everything that reads it.',
            how: 'The model is repriced against the market quotes, the parameters are moved until the ' +
                'difference is under the tolerance or the iterations run out, and the residual is ' +
                'written.',
            effect: 'Produces the fit quality. It changes no figure; it tells a person whether to trust ' +
                'the figures beside it.',
            params: [
                {
                    name: 'model',
                    shape: 'select',
                    options: ['Hull-White', 'Black-Karasinski'],
                    value: 'Hull-White',
                    help: 'The model the parameters are fitted for.',
                    normal: 'Hull-White for the rates book.'
                },
                {
                    name: 'max_iterations',
                    shape: 'number',
                    value: '100',
                    help: 'How many fitting steps are allowed before the fit is abandoned.',
                    normal: '100. A fit that reaches it did not converge.'
                },
                {
                    name: 'tolerance',
                    shape: 'number',
                    value: '0.000001',
                    help: 'The fit is good enough when the worst quote is within this amount.',
                    normal: '0.000001, a millionth of the quote\u2019s own units.'
                }
            ]
        },
        {
            code: 'pnlExplain',
            name: 'P&L explain',
            produces: 'Yesterday\u2019s profit and loss split by factor: rates, credit, FX, and the rest.',
            what: 'P&L explain answers why the number moved. It splits the day\u2019s change into the ' +
                'part each market factor caused and the part nothing explains.',
            how: 'The book is repriced at yesterday\u2019s market and at today\u2019s, factor by factor, ' +
                'and each factor\u2019s contribution is taken as a difference. What remains is unexplained.',
            effect: 'Produces the explanation of a move. It is read the morning after, so a change to it ' +
                'is noticed a day late.',
            params: [
                {
                    name: 'decomposition',
                    shape: 'select',
                    options: ['Delta-gamma', 'Sensitivity'],
                    value: 'Delta-gamma',
                    help: 'How the change is split across factors.',
                    normal: 'Delta-gamma, which keeps the second-order term.'
                },
                {
                    name: 'threshold',
                    shape: 'number',
                    value: '0.01',
                    help: 'A contribution smaller than this is gathered into "other".',
                    normal: '0.01 in reporting currency.'
                }
            ]
        },
        {
            code: 'historicalSimulationVar',
            name: 'Historical simulation VaR',
            produces: 'One value at risk figure, read from how the book would have moved on past days.',
            what: 'Historical simulation VaR replays the book against what the market actually did and ' +
                'reads the loss at the confidence level. It keeps the fat tails parametric VaR loses.',
            how: 'The book is repriced at each of the past days\u2019 market moves, the results are sorted, ' +
                'and the loss at the confidence quantile is taken.',
            effect: 'Produces a second VaR beside the parametric one. The two differing is itself the ' +
                'reading: it says the distribution is not normal.',
            params: [
                {
                    name: 'confidence',
                    shape: 'number',
                    value: '0.99',
                    help: 'The quantile the figure is read at.',
                    normal: '0.99.'
                },
                {
                    name: 'lookback_days',
                    shape: 'number',
                    value: '250',
                    help: 'How many past days are replayed.',
                    normal: '250, which is one trading year.'
                },
                {
                    name: 'holding_period_days',
                    shape: 'number',
                    value: '1',
                    help: 'The holding period the figure covers.',
                    normal: '1 for a desk, 10 for a regulatory figure.'
                },
                {
                    name: 'use_log_returns',
                    shape: 'toggle',
                    value: 'true',
                    help: 'Replay the moves as log returns rather than simple returns.',
                    normal: 'On. It keeps a large move from crossing zero.'
                }
            ]
        },
        {
            code: 'xva',
            name: 'XVA',
            produces: 'The valuation adjustments: CVA, DVA and the funding cost, per counterparty.',
            what: 'XVA is the difference between the risk-free value of a book and what it is really ' +
                'worth once the counterparty might default, the party might default, and the funding ' +
                'has a cost.',
            how: 'The exposure profile is combined with the counterparty\u2019s credit curve and the ' +
                'party\u2019s own, weighted by the probability of default at each date, and summed.',
            effect: 'Produces the adjustment a trading desk is measured on after the trade is booked. It ' +
                'is the last analytic to run and it reads the exposure profile.',
            params: [
                {
                    name: 'base_currency',
                    shape: 'text',
                    value: 'USD',
                    help: 'The currency every adjustment is restated into.',
                    normal: 'The reporting currency, USD here.'
                },
                {
                    name: 'methodology',
                    shape: 'select',
                    options: ['Semi-analytic', 'Nested Monte Carlo'],
                    value: 'Semi-analytic',
                    help: 'How the adjustment is computed.',
                    normal: 'Semi-analytic. Nested Monte Carlo costs many times more.'
                },
                {
                    name: 'include_cva',
                    shape: 'toggle',
                    value: 'true',
                    help: 'Charge the counterparty\u2019s possible default.',
                    normal: 'On.'
                },
                {
                    name: 'include_dva',
                    shape: 'toggle',
                    value: 'false',
                    help: 'Credit the party\u2019s own possible default.',
                    normal: 'Off for a group view, on for a desk that is measured on it.'
                },
                {
                    name: 'netting_threshold',
                    shape: 'number',
                    value: '1000000',
                    help: 'The exposure below which no adjustment is taken for a netting set.',
                    normal: '1000000 in reporting currency.'
                }
            ]
        }
    ];

    /* The configuration as stored today. Six types carry a row and eight carry
       none, so "on the configuration" and "on" are two different things. */
    var ROWS = [
        { code: 'npv', active: true, order: 1 },
        { code: 'sensitivity', active: true, order: 2 },
        { code: 'curves', active: true, order: 3 },
        { code: 'portfolioDetails', active: true, order: 4 },
        { code: 'simulation', active: true, order: 5 },
        { code: 'pfe', active: true, order: 6 },
        { code: 'cashflow', active: false, order: 7 },
        { code: 'saccr', active: false, order: 8 }
    ];

    /* The three readings of the per-analytic step. Nothing else differs. */
    var VARIANTS = [
        {
            id: 'A',
            name: 'Document',
            gist: 'The explanation as a short page, with the parameters in one column below it, ' +
                'each with its control and its note.'
        },
        {
            id: 'B',
            name: 'Two columns',
            gist: 'Explanation left, settings and effect right, so a person reads and sets at the ' +
                'same time.'
        },
        {
            id: 'C',
            name: 'Guided',
            gist: 'One parameter at a time with the explanation beside it, advancing through the ' +
                'settings and ending on a summary.'
        }
    ];

    var PFE_RULE = 'pfe reads a cube that simulation writes, so pfe without simulation cannot work. ' +
        'The rule lives in the ACME seeder, in generate_risk_config_data.py at ' +
        'check_exposure_needs_simulation, and in no service: nothing validates it when a ' +
        'configuration is written. This page refuses it itself, and a build would have to carry ' +
        'the rule again.';

    var NO_DELETE = 'An analytic is turned off by setting active to false, not by deleting the row. ' +
        'The row keeps its display_order and its parameter rows and reads as off, so a later reader ' +
        'can see the configuration used to produce it. No control on this page deletes a row.';

    var DEFERRED_EDITORS = 'simulation and pfe each need a purpose-built editor: one for the cube, ' +
        'one for the shift and one for the time grid. This page draws those settings as plain fields ' +
        'and says so on the parameter itself.';

    var WC = { switchboard: 'The switchboard', last: 'What the analytics are now' };

    var S = {
        step: 'switchboard',
        variant: 'A',
        guide: 0,
        rows: [],
        pendingOff: null,
        refused: null,
        focus: null,
        broken: false,
        from: 'create-a-report',
        activity: 'the fixture, nothing written',
        showState: false,
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

    function variantOf(id) {
        var found = VARIANTS.filter(function (entry) { return entry.id === id; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function fixtureClock() {
        var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
        var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        function pad(n) { return (n < 10 ? '0' : '') + n; }
        return days[NOW.getDay()] + ' ' + NOW.getDate() + ' ' + months[NOW.getMonth()] + ' ' +
            NOW.getFullYear() + ' ' + pad(NOW.getHours()) + ':' + pad(NOW.getMinutes()) +
            ' installation clock';
    }

    // --------------------------------------------------------------- state

    function defaultParams(code) {
        return analyticOf(code).params.map(function (param, index) {
            return { name: param.name, value: param.value, position: index + 1 };
        });
    }

    function seedRows() {
        return ROWS.map(function (row) {
            return {
                code: row.code,
                active: row.active,
                order: row.order,
                params: defaultParams(row.code)
            };
        });
    }

    function rowOf(code) {
        var found = S.rows.filter(function (row) { return row.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function activeRows() {
        return S.rows.filter(function (row) { return row.active; })
            .sort(function (a, b) { return a.order - b.order; });
    }

    function keptOffRows() {
        return S.rows.filter(function (row) { return !row.active; })
            .sort(function (a, b) { return a.order - b.order; });
    }

    function nextOrder() {
        return S.rows.reduce(function (acc, row) { return Math.max(acc, row.order); }, 0) + 1;
    }

    function isOn(code) {
        var row = rowOf(code);
        return !!(row && row.active);
    }

    function pairBroken() {
        return isOn('pfe') && !isOn('simulation');
    }

    function paramValue(code, name) {
        var row = rowOf(code);
        var list = row ? row.params : defaultParams(code);
        var found = list.filter(function (param) { return param.name === name; });
        return found.length === 1 ? found[0].value : '';
    }

    // --------------------------------------------------------------- rail

    /* The rail, computed from state: the switchboard, then one step per
       analytic that is on in display_order, then the final step. Everything
       that navigates reads this one function, so the rail, the numbering and
       the step foot cannot disagree. */
    function rail() {
        var analyticSteps = activeRows().map(function (row) {
            var entry = analyticOf(row.code);
            return {
                id: 'analytic-' + row.code,
                title: entry.name,
                code: entry.code,
                analytic: row.code,
                order: row.order,
                lead: 'What ' + entry.name + ' is, how it is calculated, and every setting it takes. ' +
                    'These are rows of ' + CONFIGURATION.name + ', the run settings configuration this ' +
                    'report binds, not of the definition. Its display_order is ' + row.order +
                    ', which is where this step sits, and turning it off on the switchboard removes ' +
                    'this step.'
            };
        });
        return [
            {
                id: 'switchboard',
                title: WC.switchboard,
                lead: 'The fourteen analytic types, one row each. The analytics belong to ' +
                    CONFIGURATION.name + ', the run settings configuration a definition binds, so a ' +
                    'change here reaches every definition bound to it. Turning a type on adds its step ' +
                    'to the rail in display_order; turning one off asks first and keeps the row.'
            }
        ].concat(analyticSteps).concat([
            {
                id: 'what-they-are-now',
                title: WC.last,
                lead: 'The set as it now reads, the settings it carries, and what a save would write. ' +
                    'Nothing is written from this page.',
                final: true
            }
        ]);
    }

    function stepNumberOf(code) {
        var list = rail();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === 'analytic-' + code) return i + 1;
        }
        return 0;
    }

    function railSummary() {
        var on = activeRows().length;
        return 'the switchboard, ' + on + ' analytic' + (on === 1 ? '' : 's') +
            ' in display order, and what the analytics are now \u00b7 ' +
            (on + 2) + ' step' + (on + 2 === 1 ? '' : 's');
    }

    // --------------------------------------------------------- the rule

    function ruleState() {
        if (pairBroken()) return 'violated';
        if (isOn('pfe') && isOn('simulation')) return 'satisfied';
        return 'open';
    }

    function ruleStrip() {
        var state = ruleState();
        var cls = state === 'violated' ? ' err' : (state === 'satisfied' ? ' ok' : '');
        var title;
        if (state === 'violated') {
            title = '<span class="tag bad">this set cannot run</span> pfe is on and simulation is off';
        } else if (state === 'satisfied') {
            title = '<span class="tag ok">rule satisfied</span> pfe and simulation are both on';
        } else if (isOn('simulation')) {
            title = '<span class="tag warn">rule between analytics</span> pfe is off, so the rule does ' +
                'not bind';
        } else {
            title = '<span class="tag warn">rule between analytics</span> neither pfe nor simulation ' +
                'is on, so the rule does not bind';
        }
        var action = '';
        if (state === 'violated') {
            action = '<div class="ractions">' +
                '<button type="button" class="btn small" data-act="repair">Turn simulation on</button>' +
                '<span class="hint">the one write that makes this set run again</span></div>';
        }
        return '<div class="rule' + cls + '" id="rule-strip">' +
            '<div class="rtitle">' + title + '</div>' +
            '<div class="rbody"><b>pfe reads a cube that simulation writes.</b> ' + esc(PFE_RULE) +
            ' Both rows are marked while the pair is broken, the toggle that would write either row ' +
            'is refused, and the last step refuses to read the set back.</div>' +
            action + '</div>';
    }

    // ------------------------------------------------------- switchboard

    /* The two controls a switchboard row can carry. A switch writes active and
       never deletes; an add button writes the row a type has never had. While
       the pair cannot run, both of its rows draw their switch refused. */
    function switchFor(code) {
        var row = rowOf(code);
        var on = !!(row && row.active);
        var pair = code === 'pfe' || code === 'simulation';
        var refused = pairBroken() && pair;
        var title = refused
            ? 'Refused: pfe is on and simulation is off, so the set cannot run. Repair the pair first.'
            : (on
                ? 'Turning it off asks first: it writes active false, keeps the row and removes its step.'
                : 'Turning it on writes the row with its default settings and adds a step to the rail.');
        return '<button type="button" class="sw" role="switch"' +
            ' aria-checked="' + (on ? 'true' : 'false') + '"' +
            (refused ? ' aria-disabled="true"' : '') +
            ' data-act="toggle" data-code="' + esc(code) + '" title="' + esc(title) + '">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (refused ? 'refused' : (on ? 'on' : 'off')) + '">' +
            (refused ? 'refused' : (on ? 'on' : 'off')) + '</span></button>';
    }

    /* One row of the switchboard: the switch, the name, the display_order, a
       one-line summary, and the state the row is in. A type the configuration
       carries no row for says so and offers to add it. */
    function analyticRowHtml(code) {
        var entry = analyticOf(code);
        var row = rowOf(code);
        var conflict = pairBroken() && (code === 'pfe' || code === 'simulation');
        var cls = 'anrow';
        if (!row) cls += ' norow';
        else if (!row.active) cls += ' isoff';
        if (conflict) cls += ' conflict';
        if (S.focus === code) cls += ' focusrow';

        var settings = entry.params.length;
        var badges = [];
        if (!row) {
            badges.push('<span class="tag muted">no row on this configuration \u00b7 turning it on ' +
                'writes one at <span class="mono">display_order ' + nextOrder() + '</span></span>');
        } else if (!row.active) {
            badges.push('<span class="tag muted">off \u00b7 the row is kept at ' +
                '<span class="mono">display_order ' + row.order + '</span> with its settings</span>');
        } else {
            badges.push('<span class="tag ok">step ' + stepNumberOf(code) + ' of the rail</span>');
        }
        if (conflict && code === 'pfe') {
            badges.push('<span class="tag bad">needs simulation \u00b7 cannot run</span>');
        }
        if (conflict && code === 'simulation') {
            badges.push('<span class="tag bad">pfe is on and this is off \u00b7 the pair cannot run</span>');
        }
        if (entry.params.length === 0) {
            badges.push('<span class="tag muted">no setting</span>');
        }

        var act = '';
        if (row && row.active) {
            act = '<button type="button" class="btn small" data-act="go-analytic" data-code="' +
                esc(code) + '">Step ' + stepNumberOf(code) + ' \u2192</button>';
        } else {
            act = '<button type="button" class="btn small" data-act="add" data-code="' + esc(code) +
                '">' + (row ? 'Turn on' : 'Add it') + ' at order ' +
                (row ? row.order : nextOrder()) + '</button>';
        }

        var footer = '';
        if (S.pendingOff === code) footer = offConfirmHtml(code);
        if (S.refused === code) footer = refusalHtml(code);

        return '<div class="' + cls + '">' +
            '<div class="an-top">' + switchFor(code) +
            '<span class="an-name">' + esc(entry.name) +
            '<span class="codename">' + esc(entry.code) + '</span></span>' +
            '<span class="an-order">' + (row ? 'order ' + row.order : 'order \u2014') + '</span>' +
            '<span class="an-act">' + act + '</span>' +
            '<span class="an-summary">' + esc(entry.produces) +
            ' <span class="an-count">\u00b7 ' + (settings === 0
                ? 'no setting'
                : settings + ' setting' + (settings === 1 ? '' : 's')) + '</span></span>' +
            '</div>' +
            (badges.length > 0 ? '<div class="cellgroup an-badges">' + badges.join('') + '</div>' : '') +
            footer + '</div>';
    }

    function switchboard() {
        var on = activeRows();
        var withRow = ANALYTICS.filter(function (entry) { return !!rowOf(entry.code); }).length;
        return ruleStrip() +
            '<div class="sectionhead" style="margin-top:14px"><h3>Every analytic type, one row each</h3>' +
            '<span class="meta">reporting.v1.analytic_types.list \u00b7 14 types \u00b7 ' + withRow +
            ' carry a row \u00b7 ' + on.length + ' on \u00b7 fixture</span></div>' +
            '<div class="anlist">' + ANALYTICS.map(function (entry) {
                return analyticRowHtml(entry.code);
            }).join('') + '</div>' +
            '<div class="reviewbar">' +
            '<span class="count">These rows hang off <b>' + esc(CONFIGURATION.name) + '</b>, the run ' +
            'settings configuration, and not off a definition: <b>' + CONFIGURATION.boundBy +
            '</b> definitions bind it, so a change here reaches every one of them, and a set that ' +
            'must differ belongs to a configuration of its own. The switch is how an analytic is ' +
            'added: turning one on writes the row with its default settings and gives it a step in ' +
            'display_order. Turning one off removes its step and renumbers the steps after it.</span>' +
            '<span class="hint">' + esc(railSummary()) + '</span></div>' +
            '<p class="rowfoot">' + esc(NO_DELETE) + ' Turning one off asks first, because the ' +
            'parameter values leave the rail with the step.</p>';
    }

    /* The confirmation a switch raises before it writes active false. */
    function offConfirmHtml(code) {
        var row = rowOf(code);
        var settings = row ? row.params.length : analyticOf(code).params.length;
        var wouldBreak = code === 'simulation' && isOn('pfe');
        if (wouldBreak) {
            return '<div class="offbox refused">' +
                '<div class="offtitle"><span class="tag bad">refused</span> simulation cannot be ' +
                'turned off while pfe is on</div>' +
                '<p class="offq">pfe reads the cube simulation writes, so this write would leave a ' +
                'set that cannot run. <b>The toggle is refused here rather than accepted and ' +
                'failed later, because no service validates the pair.</b> Turn pfe off first, and ' +
                'then simulation.</p>' +
                '<div class="draftrow">' +
                '<button type="button" class="btn small" data-act="off-cancel">Keep simulation on</button>' +
                '<button type="button" class="btn small" data-act="off-ask" data-code="pfe">Turn ' +
                'pfe off instead</button></div></div>';
        }
        return '<div class="offbox">' +
            '<p class="offq"><b>Turn ' + esc(code) + ' off?</b> Its step leaves the rail and the steps ' +
            'after it renumber. ' +
            (settings === 0
                ? 'It carries no setting, so nothing else goes with it. '
                : 'Its ' + settings + ' parameter value' + (settings === 1 ? '' : 's') +
                  ' go with the step, and this page shows none of them while it is off. ') +
            'The row is set to <span class="mono">active = false</span> and is not deleted: it keeps ' +
            '<span class="mono">display_order ' + (row ? row.order : '\u2014') + '</span> and its ' +
            'parameter rows, so a later reader can see the configuration used to produce it.</p>' +
            '<div class="draftrow">' +
            '<button type="button" class="btn small" data-act="off-confirm" data-code="' + esc(code) +
            '">Turn ' + esc(code) + ' off</button>' +
            '<button type="button" class="btn ghost small" data-act="off-cancel">Keep it on</button>' +
            '</div></div>';
    }

    /* The refusal a switch draws when the write it would send cannot work. It is
       drawn where the switch is, with the rule, so the refusal is a state and
       not a click that does nothing. */
    function refusalHtml(code) {
        var entry = analyticOf(code);
        var why;
        if (code === 'pfe' && !isOn('simulation')) {
            why = 'pfe reads the cube simulation writes, and simulation is off. This page refuses to ' +
                'write pfe on without it, because the set would read as complete and fail in the run.';
        } else {
            why = 'The pair pfe and simulation cannot run as it stands, so this write is refused.';
        }
        return '<div class="offbox refused">' +
            '<div class="offtitle"><span class="tag bad">refused</span> ' + esc(entry.code) +
            ' is not turned on</div>' +
            '<p class="offq">' + esc(why) + ' <b>' + esc(PFE_RULE) + '</b></p>' +
            '<div class="draftrow">' +
            '<button type="button" class="btn small" data-act="repair">Turn simulation on</button>' +
            '<button type="button" class="btn ghost small" data-act="refuse-cancel">Close</button>' +
            '</div></div>';
    }

    // -------------------------------------------- one step per analytic

    /* One control per parameter, chosen for the shape of its value: a number, a
       text field, a select for an enumeration, or a switch for a boolean. */
    function control(code, param) {
        var value = paramValue(code, param.name);
        var common = ' data-act="param" data-code="' + esc(code) +
            '" data-name="' + esc(param.name) + '" aria-label="' + esc(param.name) + ' of ' +
            esc(code) + '"';
        if (param.shape === 'number') {
            return '<input type="number" step="any" class="ctl" value="' + esc(value) + '"' +
                common + '>';
        }
        if (param.shape === 'text') {
            return '<input type="text" class="ctl" value="' + esc(value) + '"' + common + '>';
        }
        if (param.shape === 'select') {
            return '<select class="ctl"' + common + '>' + param.options.map(function (option) {
                return '<option' + (option === value ? ' selected' : '') + '>' + esc(option) +
                    '</option>';
            }).join('') + '</select>';
        }
        var on = value === 'true';
        return '<button type="button" class="sw" role="switch" aria-checked="' + (on ? 'true' : 'false') +
            '"' + common + ' data-toggle="1">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (on ? 'on' : 'off') + '">' + (on ? 'on' : 'off') +
            '</span></button>';
    }

    function paramCard(code, param) {
        var deferred = param.deferred
            ? '<div class="deferred"><span class="tag warn">deferred editor</span> ' +
              esc(param.deferred) + '</div>'
            : '';
        return '<div class="pc">' +
            '<div class="pch"><span class="pname">' + esc(param.name) + '</span>' +
            '<span class="tag muted">' + esc(param.shape) + '</span></div>' +
            '<div class="pcrow">' + control(code, param) + '</div>' +
            '<p class="phelp">' + esc(param.help) + ' <span class="pnormal">A normal value is ' +
            esc(param.normal.replace(/^[A-Z]/, function (first) { return first.toLowerCase(); })) +
            '</span></p>' + deferred + '</div>';
    }

    function deferredNotice(entry) {
        if (entry.paramEditor !== 'deferred') return '';
        return '<div class="notice warn"><b>' + esc(entry.code) + ' needs a purpose-built editor, ' +
            'and this page does not draw it.</b> ' + esc(entry.code) + ' carries ' +
            esc(entry.editorNeeds) + ', which a plain field cannot show well. ' +
            DEFERRED_EDITORS.charAt(0).toUpperCase() + esc(DEFERRED_EDITORS.slice(1)) + '</div>';
    }

    function ownershipNotice() {
        return '<div class="notice"><b>The settings below are rows of ' + esc(CONFIGURATION.name) +
            ', not of this definition.</b> ' + CONFIGURATION.boundBy + ' definitions bind that ' +
            'configuration \u2014 ' + esc(BOUND_DEFINITIONS.join(', ')) + ' \u2014 so a value changed ' +
            'here reaches their next run too, and none of them was opened. A set that must differ ' +
            'belongs to a configuration of its own. An analytic is turned off by setting ' +
            '<span class="mono">active</span> to false, never by deleting the row.</div>';
    }

    function explainBlock(entry, heading) {
        return '<div class="explain">' +
            '<h3>' + esc(heading || ('What ' + entry.name.toLowerCase() + ' is')) + '</h3>' +
            '<p>' + esc(entry.what) + '</p>' +
            '<h3>How it is calculated</h3>' +
            '<p>' + esc(entry.how) + '</p>' +
            '<p class="hint mono">' + esc(entry.code) + ' \u00b7 ' +
            esc(entry.produces) + '</p></div>';
    }

    function settingsBlock(entry) {
        if (entry.params.length === 0) {
            return '<div class="sectionhead"><h3>The settings of ' + esc(entry.code) + '</h3>' +
                '<span class="meta">no setting \u00b7 it is either on or off</span></div>' +
                '<p class="nosettings">This analytic takes no setting. It is either on or off, and no ' +
                '<span class="mono">configuration_analytic_parameter</span> row is written for it. ' +
                'Which curves it writes is decided by the curve configuration the definition binds, ' +
                'not by a setting here.</p>';
        }
        return '<div class="sectionhead"><h3>The settings of ' + esc(entry.code) + '</h3>' +
            '<span class="meta">' + entry.params.length + ' setting' +
            (entry.params.length === 1 ? '' : 's') +
            ' \u00b7 each control follows the shape of its value \u00b7 ' +
            'reporting.v1.configuration_analytic_parameters.put</span></div>' +
            '<div class="pcol">' + entry.params.map(function (param) {
                return paramCard(entry.code, param);
            }).join('') + '</div>';
    }

    function effectBlock(entry) {
        return '<div class="effect"><h3>What turning ' + esc(entry.code) + ' on reaches</h3>' +
            '<p>' + esc(entry.effect) + '</p></div>';
    }

    function pairNotice(entry) {
        if (entry.code !== 'pfe') return '';
        return '<div class="notice ' + (pairBroken() ? 'err' : 'ok') + '">' +
            '<b>pfe reads a cube that simulation writes.</b> ' +
            (pairBroken()
                ? 'simulation is off, so this analytic cannot run and the last step refuses to read ' +
                  'the set back. '
                : 'simulation is on, so the rule is satisfied. ') +
            esc(PFE_RULE) + '</div>';
    }

    function stepNav(code) {
        var entry = analyticOf(code);
        var row = rowOf(code);
        return '<div class="stepnav">' +
            '<span class="hint">Step ' + stepNumberOf(code) + ' of ' + rail().length +
            ' \u00b7 <span class="mono">display_order ' + (row ? row.order : nextOrder()) +
            '</span> on ' + esc(CONFIGURATION.name) + '</span>' +
            (S.pendingOff === code
                ? ''
                : '<button type="button" class="btn" data-act="off-ask" data-code="' + esc(code) +
                  '">Turn ' + esc(entry.code) + ' off</button>') +
            '</div>' + (S.pendingOff === code ? offConfirmHtml(code) : '');
    }

    function variantTag() {
        var v = variantOf(S.variant);
        return '<p class="varianttag"><span class="tag accent">variant ' + esc(v.id) + '</span>' +
            '<b>' + esc(v.name) + '</b><span>' + esc(v.gist) + '</span></p>';
    }

    /* A: the explanation as a short page, the parameters in one column below. */
    function analyticA(code) {
        var entry = analyticOf(code);
        return '<div class="vframe">' + variantTag() +
            '<div class="doc">' + explainBlock(entry) + '</div>' +
            reachNote(entry) +
            ownershipNotice() + pairNotice(entry) + deferredNotice(entry) +
            settingsBlock(entry) + effectBlock(entry) + stepNav(code) + '</div>';
    }

    /* B: explanation left, settings and effect right. */
    function analyticB(code) {
        var entry = analyticOf(code);
        return '<div class="vframe">' + variantTag() +
            '<div class="vsplit">' +
            '<div class="vside">' + explainBlock(entry) + effectBlock(entry) + '</div>' +
            '<div class="vside">' + reachNote(entry) +
            ownershipNotice() + pairNotice(entry) + deferredNotice(entry) +
            settingsBlock(entry) + '</div></div>' +
            stepNav(code) + '</div>';
    }

    /* C: one parameter at a time with the explanation beside it, and a summary at
       the end. An analytic with no parameter goes straight to its explanation
       and summary. */
    function analyticC(code) {
        var entry = analyticOf(code);
        var total = entry.params.length;
        var at = Math.min(Math.max(S.guide, 0), total);
        var list = [];
        entry.params.forEach(function (param, index) {
            var cls = index === at ? 'current' : (index < at ? 'done' : 'ahead');
            list.push('<div class="gstep ' + cls + '"><span class="n">' + (index + 1) + '</span>' +
                '<span class="mono">' + esc(param.name) + '</span></div>');
        });
        list.push('<div class="gstep ' + (at === total ? 'current' : '') + '">' +
            '<span class="n">\u2713</span><span>Summary</span></div>');

        var main;
        if (total === 0) {
            main = '<div class="gmain">' +
                '<p class="gcount">this analytic takes no setting</p>' +
                '<p class="nosettings">This analytic takes no setting. It is either on or off, and no ' +
                '<span class="mono">configuration_analytic_parameter</span> row is written for it.</p>' +
                guidedSummary(entry) + reachNote(entry) + '</div>';
        } else if (at < total) {
            var param = entry.params[at];
            main = '<div class="gmain">' +
                '<p class="gcount">setting ' + (at + 1) + ' of ' + total + '</p>' +
                paramCard(code, param) +
                '<div class="stepnav">' +
                '<button type="button" class="btn ghost" data-act="guide-prev"' +
                (at === 0 ? ' disabled' : '') + '>Previous</button>' +
                '<button type="button" class="btn primary ml-auto" data-act="guide-next">' +
                (at === total - 1 ? 'Finish and summarise' : 'Next setting') + '</button></div></div>';
        } else {
            main = '<div class="gmain">' + guidedSummary(entry) + reachNote(entry) + '</div>';
        }

        return '<div class="vframe">' + variantTag() +
            '<div class="guided">' +
            '<div class="gsteps">' + list.join('') + '</div>' +
            '<div><div class="doc">' + explainBlock(entry) + '</div>' + main + '</div>' +
            '</div>' + ownershipNotice() + pairNotice(entry) + deferredNotice(entry) +
            stepNav(code) + '</div>';
    }

    function guidedSummary(entry) {
        var rows = entry.params.map(function (param) {
            return '<div class="grow"><span class="gn">' + esc(param.name) + '</span>' +
                '<span class="gv">' + esc(paramValue(entry.code, param.name)) + '</span>' +
                '<button type="button" class="btn ghost small" data-act="guide-edit"' +
                ' data-index="' + entry.params.indexOf(param) + '">Change</button></div>';
        }).join('');
        return '<div class="sectionhead"><h3>The summary</h3>' +
            '<span class="meta">what ' + esc(entry.code) + ' will hold \u00b7 ' +
            'reporting.v1.configuration_analytic_parameters.put</span></div>' +
            (entry.params.length === 0
                ? '<p class="nosettings">No setting was set, because this analytic takes none.</p>'
                : '<div class="gsummary">' + rows + '</div>') +
            (entry.params.length === 0
                ? ''
                : '<div class="stepnav"><button type="button" class="btn ghost" data-act="guide-prev"' +
                  ' data-index="' + (entry.params.length - 1) + '">Back to the last setting</button>' +
                  '</div>') +
            effectBlock(entry);
    }

    /* The one place a variant would differ by more than layout: every one of the
       three says the same thing about where the values reach. */
    function reachNote(entry) {
        var writes = entry.params.length === 0
            ? 'Turning ' + esc(entry.code) + ' on writes one ' +
              '<span class="mono">configuration_analytic</span> row with ' +
              '<span class="mono">reporting.v1.configuration_analytics.put</span>, and no ' +
              '<span class="mono">configuration_analytic_parameter</span> row, because it takes no ' +
              'setting.'
            : 'Turning ' + esc(entry.code) + ' on writes one ' +
              '<span class="mono">configuration_analytic</span> row and ' + entry.params.length +
              ' <span class="mono">configuration_analytic_parameter</span> row' +
              (entry.params.length === 1 ? '' : 's') + ' with ' +
              '<span class="mono">reporting.v1.configuration_analytics.put</span> and ' +
              '<span class="mono">reporting.v1.configuration_analytic_parameters.put</span>.';
        return '<div class="notice">' + writes + ' A run made after the change is not comparable ' +
            'with one made before it, because the two runs did not produce the same measures.</div>';
    }

    function analyticStepBody(code) {
        if (S.variant === 'B') return analyticB(code);
        if (S.variant === 'C') return analyticC(code);
        return analyticA(code);
    }

    // ------------------------------------------------- the final step

    function setTable() {
        var rows = activeRows().map(function (row) {
            return '<tr><td class="code">' + esc(String(row.order)) + '</td>' +
                '<td>' + esc(row.code) + '</td>' +
                '<td>' + esc(analyticOf(row.code).name) + '</td>' +
                '<td class="code">active = true</td>' +
                '<td>' + (row.params.length === 0 ? 'no setting'
                    : row.params.length + ' setting' + (row.params.length === 1 ? '' : 's')) + '</td>' +
                '<td class="code">step ' + stepNumberOf(row.code) + '</td></tr>';
        }).join('');
        return '<table><thead><tr><th>display_order</th><th>analytic</th><th>name</th>' +
            '<th>active</th><th>settings</th><th>its step</th></tr></thead><tbody>' + rows +
            '</tbody></table>';
    }

    function keptTable() {
        var rows = keptOffRows();
        if (rows.length === 0) return '';
        return '<div class="writeblock">' +
            '<div class="op"><span class="what">The rows kept off, with their order and their settings</span>' +
            '<span class="subject">reporting.v1.configuration_analytics.list_by_configuration_id</span></div>' +
            '<table><tbody>' + rows.map(function (row) {
                return '<tr class="off"><td class="code">' + esc(String(row.order)) + '</td>' +
                    '<td>' + esc(row.code) + '</td><td class="code">active = false</td>' +
                    '<td>' + row.params.length + ' parameter row' +
                    (row.params.length === 1 ? '' : 's') + ' kept</td></tr>';
            }).join('') + '</tbody></table>' +
            '<p class="hint">' + esc(NO_DELETE) + '</p></div>';
    }

    function parameterTable() {
        var rows = [];
        activeRows().forEach(function (row) {
            row.params.forEach(function (param) {
                rows.push('<tr><td class="code">' + esc(row.code + '.' + param.name) + '</td>' +
                    '<td class="val">' + esc(param.value) + '</td>' +
                    '<td class="code">' + esc(String(param.position)) + '</td></tr>');
            });
        });
        return '<div class="writeblock">' +
            '<div class="op"><span class="what">Every parameter row this set carries</span>' +
            '<span class="subject">' +
            'reporting.v1.configuration_analytic_parameters.list_by_configuration_analytic_id</span></div>' +
            (rows.length === 0
                ? '<p class="hint">No analytic on this set carries a parameter.</p>'
                : '<table><thead><tr><th>parameter</th><th>value</th><th>position</th></tr></thead>' +
                  '<tbody>' + rows.join('') + '</tbody></table>') + '</div>';
    }

    function writeBlock() {
        return '<div class="writeblock">' +
            '<div class="op"><span class="what">What turning a switch here would write</span>' +
            '<span class="subject">reporting.v1.configuration_analytics.put_many</span></div>' +
            '<p class="hint">The whole set in one call, so turned-on, turned-off and reordered rows ' +
            'land together and a half-written configuration is not left behind. One setting alone is ' +
            'written with <span class="mono">reporting.v1.configuration_analytic_parameters.put</span>, ' +
            'because a setting is one row with one value. No write here deletes a row.</p></div>';
    }

    function reachBlock() {
        return '<div class="writeblock">' +
            '<div class="op"><span class="what">The definitions that read this set</span>' +
            '<span class="subject">reporting.v1.report_configurations.list</span></div>' +
            '<p class="hint">' + CONFIGURATION.boundBy + ' definitions bind ' +
            esc(CONFIGURATION.name) + ': ' + esc(BOUND_DEFINITIONS.join(', ')) + '. Their next run ' +
            'produces the set this page leaves, and none of them was opened.</p></div>';
    }

    function returnHref() {
        if (S.from === 'change-what-a-report-runs') {
            return '../change-what-a-report-runs/?step=2';
        }
        return '../create-a-report/?step=7';
    }

    function returnText() {
        if (S.from === 'change-what-a-report-runs') {
            return 'Return to the report, on Change what a report runs';
        }
        return 'Return to the report, on Create a report';
    }

    function finalBody() {
        if (pairBroken()) {
            return '<div class="notice err"><b>This page will not read this set back, because it ' +
                'cannot run.</b> pfe is on and simulation is off, so the cube pfe reads is never ' +
                'written. Reading the set back would state a configuration as complete that would ' +
                'fail in the run. ' + esc(PFE_RULE) + '</div>' +
                '<div class="draftrow" style="margin-bottom:14px">' +
                '<button type="button" class="btn" data-act="repair">Turn simulation on</button>' +
                '<button type="button" class="btn ghost" data-act="go-step" data-step="switchboard">' +
                'Back to the switchboard</button></div>' +
                ruleStrip() +
                '<p class="rowfoot">The set below is what the rail carries for a person who repairs ' +
                'the pair. Nothing on it is a reading of a configuration that can run.</p>';
        }
        return '<div class="readback" style="display:grid;gap:14px">' +
            '<div class="banner"><span class="tag ok">read back</span>' +
            '<span>' + activeRows().length + ' analytics on, ' + keptOffRows().length +
            ' rows kept off, and their settings, on ' + esc(CONFIGURATION.name) + '.</span></div>' +
            '<div class="review">' +
            '<div class="writeblock">' +
            '<div class="op"><span class="what">The analytics this configuration now runs</span>' +
            '<span class="subject">' +
            'reporting.v1.configuration_analytics.list_by_configuration_id</span></div>' +
            setTable() + '</div>' +
            keptTable() + parameterTable() + writeBlock() + reachBlock() +
            '</div>' +
            '<div class="notice"><b>The analytics belong to ' + esc(CONFIGURATION.name) + ', the run ' +
            'settings configuration, not to the definition.</b> ' + CONFIGURATION.boundBy +
            ' definitions bind it, so this is the set each of their next runs produces. A definition ' +
            'that must run a different set binds a configuration of its own, and nothing on this page ' +
            'creates one: the configurations offered here are the ones the party holds, read with ' +
            '<span class="mono">reporting.v1.configurations.list</span> and ' +
            '<span class="mono">reporting.v1.configurations.list_by_configuration_type_code</span>.</div>' +
            '</div>' +
            '<div class="reach">' +
            '<div class="does"><h3>What this reaches</h3><ul>' +
            '<li>The next run of all ' + CONFIGURATION.boundBy + ' definitions bound to ' +
            esc(CONFIGURATION.name) + '.</li>' +
            '<li>The scope those runs read, which is <span class="mono">' + esc(CONFIGURATION.scope) +
            '</span> and is a column of this configuration, not of a definition.</li>' +
            '<li>Every figure built on an analytic that was added, removed or setting-changed.</li>' +
            '</ul></div>' +
            '<div class="not"><h3>What it does not reach</h3><ul>' +
            '<li>A run already produced: a run keeps the configuration it ran with.</li>' +
            '<li>The definition itself, its schedule, its concurrency policy or its other four ' +
            'configurations.</li>' +
            '<li>A comparison across the change: two runs either side of it did not produce the same ' +
            'measures, so the difference between them is the difference of configuration.</li>' +
            '</ul></div></div>' +
            '<p class="hint">' + esc(DEFERRED_EDITORS) + '</p>';
    }

    // ------------------------------------------------------- page parts

    function appHeader() {
        return '<div class="appheader"><div class="appheader-inner">' +
            '<div class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></div>' +
            '<nav class="appnav">' +
            '<a href="#" data-act="noop">Parties</a>' +
            '<a href="#" data-act="noop">Books</a>' +
            '<a href="#" class="here">Reporting</a>' +
            '<a href="#" data-act="noop">Compute</a>' +
            '</nav>' +
            '<span class="modechip">' + esc(PARTY.code) + ' \u00b7 ' + esc(PERSON.role) + '</span>' +
            '</div></div>';
    }

    function pageHead() {
        return '<div class="pagehead"><div>' +
            '<div class="crumbs"><span>Reporting</span><span>\u203a</span>' +
            '<span>' + esc(CONFIGURATION.name) + '</span><span>\u203a</span>' +
            '<span>Analytics</span></div>' +
            '<h1>Manage the analytics</h1>' +
            '<p class="lede">The analytics of <b>' + esc(CONFIGURATION.name) + '</b>, a run settings ' +
            'configuration the party holds. This page owns the whole interaction: the switchboard, a ' +
            'step for each analytic that is on, and a last step that reads the set back. Create a ' +
            'report and Change what a report runs link here instead of drawing it again.</p></div>' +
            '<div class="actions">' +
            '<a class="btn linklike" href="' + esc(returnHref()) + '">Back to the report</a>' +
            '<span class="updated">' + esc(fixtureClock()) + '</span>' +
            '</div></div>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>This page is a prototype, and every value on it is a ' +
            'fixture.</b> Nothing reads a server and nothing is written. The configuration, its ' +
            'scope, the four definitions bound to it and every analytic setting are mock values. ' +
            'The subjects named are the reporting service\u2019s own; no subject is invented for a ' +
            'gap.</div>';
    }

    function stepHeaderHtml() {
        return '<div class="stepheader"><div class="metaline">' +
            '<span class="nm">' + esc(CONFIGURATION.name) + '</span>' +
            '<span class="sub">' + esc(CONFIGURATION.detail) + ' \u00b7 scope ' +
            esc(CONFIGURATION.scope) + '</span></div></div>';
    }

    // ------------------------------------------------------------- rail

    function railHtml(list, at) {
        var firstAnalytic = -1;
        var firstFinal = -1;
        list.forEach(function (entry, index) {
            if (firstAnalytic < 0 && entry.analytic) firstAnalytic = index;
            if (firstFinal < 0 && entry.final) firstFinal = index;
        });
        return '<div class="railcol">' +
            '<p class="railsum">' + esc(railSummary()) + '</p>' +
            '<nav class="railnav" aria-label="Steps"><ol>' +
            list.map(function (entry, index) {
                var cls = index === at ? 'current' : (index < at ? 'done' : 'ahead');
                var group = index === firstAnalytic || index === firstFinal ? ' railstarts' : '';
                return '<li class="railentry ' + cls + group + '"' +
                    (index === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' +
                    (index < at ? '\u2713' : String(index + 1)) + '</span>' +
                    '<span class="railtext">' + esc(entry.title) +
                    (entry.code ? ' <span class="railcode">' + esc(entry.code) + '</span>' : '') +
                    (entry.final ? ' <span class="railflag">final</span>' : '') +
                    '</span></li>';
            }).join('') + '</ol></nav></div>';
    }

    function nextOf(list, at) {
        if (at >= list.length - 1) return null;
        return { label: 'Next: ' + list[at + 1].title.toLowerCase(), enabled: true };
    }

    /* Going back is refused at the first step, and at a step that is final:
       once the set has been read back there is no earlier state to return to. */
    function backAllowed(list, at) {
        return at > 0 && list[at - 1].final !== true && list[at].final !== true;
    }

    function footHtml(step, at) {
        var list = rail();
        var next = nextOf(list, at);
        var back = backAllowed(list, at);
        if (step.final) {
            return '<div class="stepfoot">' +
                '<span class="hint">this step is final, so it offers no Back</span>' +
                '<a class="btn primary ml-auto" href="' + esc(returnHref()) + '">' +
                esc(returnText()) + '</a></div>';
        }
        if (next === null && !back) return '';
        return '<div class="stepfoot">' +
            (back ? '<button type="button" class="btn ghost" data-act="back">Back</button>' : '') +
            (next !== null
                ? '<button type="button" class="btn primary ml-auto" data-act="next"' +
                  (next.enabled ? '' : ' disabled') + '>' + esc(next.label) + '</button>'
                : '') + '</div>';
    }

    function stepBody(step) {
        if (step.id === 'switchboard') return switchboard();
        if (step.analytic) return analyticStepBody(step.analytic);
        return finalBody();
    }

    function at() {
        var list = rail();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === S.step) return i;
        }
        return -1;
    }

    function pos() {
        var index = at();
        return index < 0 ? 0 : index;
    }

    function goTo(index) {
        var list = rail();
        var target = Math.min(list.length - 1, Math.max(0, index));
        S.step = list[target].id;
        S.pendingOff = null;
        S.refused = null;
        S.focus = null;
        S.guide = 0;
        setActivity(list[target].final
            ? 'reading the set back'
            : 'step ' + (target + 1) + ' of ' + list.length + ' \u00b7 ' + list[target].title);
        render();
    }

    // ------------------------------------------------------------- bars

    function renderNote() {
        var position = pos();
        var list = rail();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 fixture data, nothing reads a server, nothing is written \u00b7 ' +
            'step ' + (position + 1) + ' of ' + list.length + ', ' + list[position].title +
            ' \u00b7 variant ' + S.variant + ' \u00b7 ' + S.activity +
            ' \u00b7 ' + PERSON.name + ', ' + PERSON.role + ', party ' + PARTY.code;
    }

    function renderBar() {
        var position = pos();
        var list = rail();
        var steps = list.map(function (step, index) {
            return '<button data-act="step" data-step="' + esc(step.id) + '"' +
                (position === index ? ' class="on"' : '') + '>' + (index + 1) + ' ' +
                esc(step.title) + '</button>';
        }).join('');
        var variants = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + esc(v.id) + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.id) + ' ' +
                esc(v.name.toLowerCase()) + '</button>';
        }).join('');
        var stateLine = '<span class="label">step <b>' + (position + 1) + ' of ' + list.length +
            '</b> \u00b7 ' + esc(list[position].title) + ' \u00b7 state: <b>' +
            esc(S.activity) + '</b></span>';
        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fixtureClock()) + '</b></span>' +
            '<span>configuration: <b>' + esc(CONFIGURATION.name) + '</b></span>' +
            '<span>definitions bound: <b>' + CONFIGURATION.boundBy + '</b></span>' +
            '<span>analytics on: <b>' + activeRows().length + ' of 14</b></span>' +
            '<span>rows kept off: <b>' + keptOffRows().length + '</b></span>' +
            '<span>rail: <b>' + esc(railSummary()) + '</b></span>' +
            '<span>this step: <b>' + esc(S.step) + '</b></span>' +
            '<span>variant: <b>' + S.variant + '</b></span>' +
            '<span>pfe without simulation: <b>' + (pairBroken() ? 'yes' : 'no') + '</b></span>' +
            '<span>return goes to: <b>' + esc(S.from) + '</b></span>' +
            '</div>' +
            '<p class="state-note">One party, because a configuration belongs to one party. The ' +
            'configuration, its scope, its binding count and every analytic setting are a fixture. ' +
            'Nothing reads a server, the pfe rule lives in the seeder, and no service validates it.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + steps +
            '<span class="sep">|</span>' +
            '<span class="label">variant</span>' + variants +
            '<span class="sep">|</span>' + stateLine +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') +
            '</button></div>' + statePanel;
    }

    function scrollRail() {
        var nav = document.querySelector('.railnav');
        var current = nav && nav.querySelector('.railentry.current');
        if (!nav || !current || nav.scrollHeight <= nav.clientHeight) return;
        var box = nav.getBoundingClientRect();
        var entry = current.getBoundingClientRect();
        if (entry.top < box.top) nav.scrollTop -= box.top - entry.top + 8;
        else if (entry.bottom > box.bottom) nav.scrollTop += entry.bottom - box.bottom + 8;
    }

    function render() {
        if (at() < 0) S.step = 'switchboard';
        var list = rail();
        var position = pos();
        var step = list[position];
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() +
            '<main>' + pageHead() + fixtureNote() +
            '<div class="journey">' + railHtml(list, position) +
            '<section class="card">' + stepHeaderHtml() +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(step.lead) + '</p>' +
            stepBody(step) + footHtml(step, position) +
            '</section></div>' +
            '</main></div>';
        renderNote();
        renderBar();
        scrollRail();
        writeParams();
    }

    // --------------------------------------------------------- actions

    function setActivity(text) { S.activity = text; }

    function logStep(text) {
        S.log.unshift(text);
        if (S.log.length > 12) S.log.pop();
    }

    /* The switch is how an analytic is added. Turning one on writes its row with
       the default settings, or restores a row that was turned off with the
       settings it kept, and gives the analytic a step of its own. */
    function turnOn(code) {
        var entry = analyticOf(code);
        var row = rowOf(code);
        var added = !row;
        if (code === 'pfe' && !isOn('simulation')) {
            S.refused = 'pfe';
            logStep('refused to set pfe.active true \u2014 simulation is off');
            setActivity('refused: pfe cannot be on without simulation');
            render();
            return;
        }
        if (!row) {
            row = { code: code, active: true, order: nextOrder(), params: defaultParams(code) };
            S.rows.push(row);
        }
        row.active = true;
        S.pendingOff = null;
        S.refused = null;
        S.focus = code;
        var stepNo = stepNumberOf(code);
        logStep((added ? 'wrote ' : 'set ') + code + '.active true' + (added
            ? ' at display_order ' + row.order + ' with ' + entry.params.length + ' default setting(s)'
            : ', keeping display_order ' + row.order + ' and its settings') +
            ' \u2014 its step is ' + stepNo + ' of ' + rail().length + ', and nothing is deleted');
        setActivity('turned ' + code + ' on \u00b7 it owns step ' + stepNo + ' of ' + rail().length);
        render();
    }

    /* Turning one off asks first, so this runs only after the confirmation. It
       sets active false, which removes the step and renumbers the rail. */
    function turnOff(code) {
        var row = rowOf(code);
        if (!row) return;
        row.active = false;
        S.pendingOff = null;
        S.refused = null;
        if (S.step === 'analytic-' + code) S.step = 'switchboard';
        logStep('set ' + code + '.active false \u2014 the row stays at display_order ' + row.order +
            ' with its settings, nothing is deleted, and its step leaves the rail');
        setActivity(code + ' is off \u00b7 its step is gone and the steps after it renumber \u00b7 ' +
            'the row is kept at active false');
        render();
    }

    function askOff(code) {
        var row = rowOf(code);
        if (!row || !row.active) return;
        S.pendingOff = code;
        S.refused = null;
        setActivity('turning ' + code + ' off \u2014 it asks first, because its values go with the ' +
            'step');
        render();
    }

    function repair() {
        turnOn('simulation');
        logStep('turned simulation on to repair the pair \u2014 pfe reads the cube it writes');
    }

    function setParam(code, name, value) {
        var row = rowOf(code);
        if (!row) return;
        row.params.forEach(function (param) {
            if (param.name === name) param.value = value;
        });
        logStep('set ' + code + '.' + name + ' = ' + value + ' \u2014 a row of ' +
            CONFIGURATION.name + ', not of a definition');
        setActivity('editing ' + code + '.' + name + ' on ' + CONFIGURATION.name);
        render();
    }

    /* Deep links. ?step=N is numeric over the current rail, so it moves when an
       analytic is turned on or off. ?analytic=<code> names the analytic, and
       resolves to its step whatever its number. */
    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var from = params.get('from');
        if (from === 'create-a-report' || from === 'change-what-a-report-runs') {
            S.from = from;
        }
        var variant = params.get('variant');
        if (VARIANTS.some(function (v) { return v.id === variant; })) S.variant = variant;
        if (params.get('pfe') === 'without-simulation') {
            var simulation = rowOf('simulation');
            if (simulation) simulation.active = false;
            S.broken = true;
            setActivity('pfe on without simulation: the set cannot run');
        }
        var guide = parseInt(params.get('guide'), 10);
        if (guide >= 0) S.guide = guide;
        var code = params.get('analytic');
        var wanted = parseInt(params.get('step'), 10);
        if (code && analyticOf(code)) {
            if (isOn(code)) {
                S.step = 'analytic-' + code;
            } else {
                S.step = 'switchboard';
                S.focus = code;
                setActivity(code + ' is off, so it has no step: the switchboard is where it is ' +
                    'turned on');
            }
        } else if (wanted >= 1 && wanted <= rail().length) {
            S.step = rail()[wanted - 1].id;
        }
        var refuse = params.get('refuse');
        if (refuse && analyticOf(refuse)) S.refused = refuse;
    }

    function writeParams() {
        var params = new URLSearchParams();
        var list = rail();
        var position = pos();
        params.set('step', String(position + 1));
        if (list[position].analytic) params.set('analytic', list[position].analytic);
        params.set('variant', S.variant);
        if (list[position].analytic && S.variant === 'C') params.set('guide', String(S.guide));
        if (S.broken && pairBroken()) params.set('pfe', 'without-simulation');
        params.set('from', S.from);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (element.tagName !== 'BUTTON' && element.tagName !== 'A') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        if (act === 'param') return;
        if (element.tagName === 'A') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        event.preventDefault();
        var code = element.getAttribute('data-code');
        var list = rail();

        if (act === 'step') {
            var wanted = element.getAttribute('data-step');
            for (var i = 0; i < list.length; i += 1) {
                if (list[i].id === wanted) { goTo(i); return; }
            }
        } else if (act === 'go-step') {
            for (var j = 0; j < list.length; j += 1) {
                if (list[j].id === element.getAttribute('data-step')) { goTo(j); return; }
            }
        } else if (act === 'back') {
            goTo(pos() - 1);
        } else if (act === 'next') {
            goTo(pos() + 1);
        } else if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            S.guide = 0;
            setActivity('variant ' + S.variant + ': ' + variantOf(S.variant).name.toLowerCase() +
                ' \u2014 the switchboard and the rail are the same in all three');
            render();
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        } else if (act === 'toggle') {
            if (pairBroken() && (code === 'pfe' || code === 'simulation')) {
                S.refused = code;
                setActivity('refused: the pair cannot run, so neither row is written');
                render();
                return;
            }
            var row = rowOf(code);
            if (row && row.active) askOff(code);
            else turnOn(code);
        } else if (act === 'add') {
            turnOn(code);
        } else if (act === 'off-ask') {
            askOff(code);
        } else if (act === 'off-cancel') {
            var kept = S.pendingOff;
            S.pendingOff = null;
            S.refused = null;
            setActivity('kept ' + (kept || 'it') + ' as it was \u00b7 nothing was written');
            render();
        } else if (act === 'off-confirm') {
            turnOff(code);
        } else if (act === 'refuse-cancel') {
            S.refused = null;
            setActivity('the refusal is closed \u00b7 nothing was written');
            render();
        } else if (act === 'repair') {
            repair();
        } else if (act === 'go-analytic') {
            S.pendingOff = null;
            S.refused = null;
            S.focus = null;
            S.guide = 0;
            S.step = 'analytic-' + code;
            setActivity(code + ' \u00b7 step ' + stepNumberOf(code) + ' of ' + rail().length);
            render();
        } else if (act === 'guide-next') {
            S.guide = S.guide + 1;
            render();
        } else if (act === 'guide-prev') {
            var back = element.getAttribute('data-index');
            S.guide = back !== null ? parseInt(back, 10) : Math.max(0, S.guide - 1);
            render();
        } else if (act === 'guide-edit') {
            S.guide = parseInt(element.getAttribute('data-index'), 10);
            render();
        }
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (act === 'param' && element.getAttribute('data-toggle') !== '1') {
            setParam(element.getAttribute('data-code'), element.getAttribute('data-name'),
                element.value);
        }
    });

    /* A parameter switch reads its new value on click, so it writes without
       going through change. */
    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act="param"][data-toggle="1"]');
        if (!element) return;
        event.preventDefault();
        var code = element.getAttribute('data-code');
        var name = element.getAttribute('data-name');
        setParam(code, name, paramValue(code, name) === 'true' ? 'false' : 'true');
    });

    S.rows = seedRows();
    readParams();
    render();
}());

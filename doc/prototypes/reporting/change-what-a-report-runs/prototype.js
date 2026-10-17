/* What this report runs. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_change_what_a_report_runs.org.
 *
 * The screen is a three step wizard:
 *
 *   1  the definition and the configurations it binds
 *   2  the analytics, as a switchboard of every analytic type
 *   3  review and save
 *
 * Adding an analytic is a flow of its own, because setting one up inline in the
 * switchboard is too busy: first the list of the fourteen types, then the
 * detail with one control per parameter, then the effect. The detail screen is
 * the novel part, so the three variants -- A document, B two columns, C guided
 * -- are three readings of that screen and nothing else. The wizard and the
 * switchboard are identical across all three.
 *
 * Facts the copy never hides:
 *
 *   - An analytic is turned off by setting `active` to false, never by deleting
 *     the row, so the history shows it was turned off.
 *   - pfe reads a cube that simulation writes, so pfe on without simulation
 *     cannot work. No service refuses it: the rule lives in the ACME seeder, so
 *     this page refuses it itself and says the server would not.
 *   - A configuration is shared, so the change reaches every definition bound to
 *     it, not only the one the person opened.
 *   - A run keeps the configuration it ran with.
 *   - A run made after the change is not comparable with one made before it, and
 *     nothing says which configuration version a run used, so the run placement
 *     here is an inference from start times.
 *   - Only conventions has a screen today. The other four configuration types
 *     are stated as missing rather than drawn as working routes. */

(function () {
    'use strict';

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };

    var DEFINITION = {
        id: 'rpt-8f31a0c4',
        name: 'Group Consolidated Exposure',
        reportType: 'risk'
    };

    var CONFIGURATION = { id: 'cfg-2d61f0a8', name: 'Group risk configuration' };

    var SHARED_WITH = ['Board Risk Dashboard', 'Group DV01 by book', 'Treasury Sensitivity'];

    /* The five configuration types a definition binds. Only conventions has a
       screen in the application today; PATHS.conventions is
       '/refdata/conventions'. The other four are stated as missing. */
    var CONFIG_TYPES = [
        {
            type: 'conventions',
            name: 'ORE conventions 2026',
            supplies: 'The calendars, day-count rules, business-day rules and currency pairs every ' +
                'analytic reads.',
            route: {
                path: '/refdata/conventions',
                screen: 'Conventions',
                symbol: 'PATHS.conventions'
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

    /* The fourteen analytic types, in the order the page draws them. Each carries
       what it produces, what it is, how it is calculated in plain words, what
       turning it on does, the analytic it needs, and every parameter it takes
       with the control its shape asks for.
     */
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
                        'curve_configuration bound beside this one.',
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
            params: [
                {
                    name: 'curve_set',
                    shape: 'text',
                    value: 'ACME curve set',
                    help: 'The curves to write. It names the curve_configuration bound beside this one.',
                    normal: 'The party\u2019s own curve set.'
                },
                {
                    name: 'build_on_the_fly',
                    shape: 'toggle',
                    value: 'false',
                    help: 'Build the curves inside this analytic instead of reading the built ones.',
                    normal: 'Off. On is for checking a builder, not for a production run.'
                }
            ]
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
            what: 'A credit curve says how likely a counterparty is to still be there in a year. It is ' +
                'what the credit analytics discount with.',
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

    /* The configuration as stored today. Eight types carry a row; the other six
       carry none, so "on the configuration" and "on" are two different things. */
    var ROWS = [
        { code: 'npv', active: 'Y', order: 1, params: [] },
        {
            code: 'sensitivity', active: 'Y', order: 2, params: [
                { name: 'shift_size', value: '0.0001' },
                { name: 'shift_type', value: 'Absolute' }
            ]
        },
        { code: 'cashflow', active: 'N', order: 3, params: [] },
        { code: 'portfolioDetails', active: 'Y', order: 4, params: [] },
        {
            code: 'simulation', active: 'Y', order: 5, params: [
                { name: 'number_of_paths', value: '1000' },
                { name: 'seed', value: '42' },
                { name: 'time_grid', value: '1D' }
            ]
        },
        {
            code: 'pfe', active: 'N', order: 6, params: [
                { name: 'horizon', value: '10' },
                { name: 'quantile', value: '0.95' },
                { name: 'exposure_cube', value: 'simulation-cube-01' }
            ]
        },
        { code: 'saccr', active: 'N', order: 7, params: [] },
        { code: 'calibration', active: 'N', order: 8, params: [] }
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

    /* The wizard's steps. The screen is titled "What this report runs" and the
       steps are the same for every variant. */
    var STEPS = [
        { n: 1, title: 'The definition and the configurations it binds' },
        { n: 2, title: 'The analytics' },
        { n: 3, title: 'Review and save' }
    ];

    /* The three variants read the add-analytic detail screen. Nothing else
       differs: the wizard and the switchboard are identical across them. */
    var VARIANTS = [
        {
            id: 'A',
            name: 'Document',
            gist: 'The analytic\u2019s explanation as a short readable page, with the parameters in a ' +
                'single column below it, each with its control and its note.'
        },
        {
            id: 'B',
            name: 'Two columns',
            gist: 'Explanation and calculation on the left, parameters and effect on the right, so a ' +
                'person reads and sets at the same time.'
        },
        {
            id: 'C',
            name: 'Guided',
            gist: 'One parameter at a time, with the explanation beside it, advancing through the ' +
                'settings and ending on a summary.'
        }
    ];

    var NO_DELETE = 'An analytic is turned off by setting active to false, not by deleting the row. ' +
        'The row keeps its display_order and its parameters and reads as off, so the history shows the ' +
        'report used to produce it. No control on this page removes a row.';

    var PFE_RULE = 'pfe reads a cube that simulation writes, so turning pfe on without simulation cannot ' +
        'work. The rule lives in the ACME seeder, in generate_risk_config_data.py, function ' +
        'check_exposure_needs_simulation, and in no service. Nothing validates it when a configuration is ' +
        'written, so the server would accept this set. This page refuses it itself, and a build would have ' +
        'to do the same or state the failure it cannot prevent.';

    var PAST_RUNS = 'A run keeps the configuration it ran with. Its outputs, its errors and its figures ' +
        'are that run\u2019s own record, and they do not change when this configuration changes. The runs ' +
        'already produced stay listed.';

    var INCOMPARABLE = 'A run made after this change is not comparable with one made before it. The two ' +
        'runs did not produce the same measures, so the difference between them would be a difference of ' +
        'configuration and not a difference of the book. The results screen compares a run with a previous ' +
        'run of the same definition and marks the comparison partial where a side carried errors; it cannot ' +
        'mark this one, because no operation says which configuration version a run used. This page places ' +
        'a run by its own start time against the definition\u2019s version history, and that is an ' +
        'inference, not a fact.';

    var SHARED = 'A configuration is shared. Three other definitions bind Group risk configuration, so the ' +
        'next run of each of them produces the new set too, and none of them was opened here.';

    var NO_CONFIG_SCREEN = 'No screen exists for four of the five configuration types. Only conventions ' +
        'has a route, PATHS.conventions, which is /refdata/conventions. The other four are named here and ' +
        'stated as missing rather than drawn as working links: a person reading what their report runs can ' +
        'see where the data lives and cannot yet open it.';

    var DEFERRED_EDITORS = 'Simulation and pfe each need a purpose-built editor: one for the cube, one ' +
        'for the shift and one for the time grid. This page draws those settings as plain fields, and ' +
        'says so on the parameter itself. A plain field is not the final design.';

    var GAPS = [
        {
            title: 'No screen exists for four of the five configuration types',
            body: 'The wizard names the five configurations the definition binds and links each to the ' +
                'screen that holds its data. Only one such screen exists: conventions, at ' +
                'PATHS.conventions, /refdata/conventions. pricing_engines, todays_market, ' +
                'curve_configuration and risk_report_config have no screen in the application, and this ' +
                'page states that instead of drawing a link that would not resolve.'
        },
        {
            title: 'The complex editors for simulation and pfe are deferred',
            body: 'The cube pfe reads, the shift simulation applies and the time grid it writes are drawn ' +
                'as plain fields here. Each needs a purpose-built editor that shows the grid, the key ' +
                'dates and the cube they name. That is a later task, and this page says so on the ' +
                'parameter rather than presenting a text field as the design.'
        },
        {
            title: 'Nothing validates the pfe rule',
            body: 'pfe needs simulation, and that rule lives in the ACME seeder and nowhere else. ' +
                'reporting.v1.configuration_analytics.put_many accepts pfe without simulation, so the ' +
                'failure arrives in the run and not in the write. This page marks the pair on the ' +
                'switchboard and refuses the step; a build cannot rely on the server doing either.'
        },
        {
            title: 'No operation says which runs used which configuration',
            body: 'A run keeps the configuration it ran with, and nothing reads that configuration back ' +
                'with the run. The page can only compare a run\u2019s start time with the definition\u2019s ' +
                'version history, which is a history of a different row: the analytics live on the ' +
                'configuration. "This run predates my change" is an inference here and nowhere a fact.'
        },
        {
            title: 'No read of a configuration\u2019s version history',
            body: 'A configuration carries a version, and no operation in this journey\u2019s set returns ' +
                'the versions of one configuration, or the version a definition bound at a time. ' +
                'report_definitions_versions reads the definition, and the analytics are not on the ' +
                'definition.'
        },
        {
            title: 'The comparison is marked partial for errors, not for a changed set',
            body: 'The results screen marks a comparison partial where a side carried errors. It cannot ' +
                'mark it invalid because the analytics changed, because nothing tells it that they did. A ' +
                'person reading across this change reads two different sets of measures side by side and ' +
                'sees the difference as a change in the figures.'
        },
        {
            title: 'No route and no screen for reporting',
            body: 'No BFF route serves reporting and the application has no reporting UI. The wizard, the ' +
                'switchboard, the add-analytic flow and the review are new work, and this page is the ' +
                'first drawing of them.'
        },
        {
            title: 'The allowed values of a parameter are not readable here',
            body: 'A parameter row is a parameter_name and a value. The kind of value comes from the ' +
                'definition behind it, and no operation in this journey\u2019s set returns that ' +
                'definition\u2019s value domain. This page draws a select where it knows the enumeration ' +
                'from its fixture, and a text field where it does not.'
        }
    ];

    var S = {
        step: 1,
        variant: 'A',
        flow: null,
        draft: {},
        staged: false,
        showState: false,
        confirm: null,
        refusal: null,
        saved: false,
        readback: null,
        activity: 'rest',
        log: []
    };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function clone(value) {
        return JSON.parse(JSON.stringify(value));
    }

    function analyticOf(code) {
        var found = ANALYTICS.filter(function (entry) { return entry.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function variant() {
        var found = VARIANTS.filter(function (entry) { return entry.id === S.variant; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function storedRow(code) {
        var found = S.rows.filter(function (row) { return row.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function proposedRow(code) {
        var held = S.draft[code];
        if (held) return held;
        return storedRow(code);
    }

    function nextOrder() {
        var top = 0;
        ANALYTICS.forEach(function (entry) {
            var row = proposedRow(entry.code);
            if (row && row.order > top) top = row.order;
        });
        return top + 1;
    }

    function activeOf(code) {
        var row = proposedRow(code);
        return row !== null && row.active === 'Y';
    }

    /* The one rule between analytics, read off the set as it would be. */
    function ruleState() {
        var pfeOn = activeOf('pfe');
        var simOn = activeOf('simulation');
        if (pfeOn && !simOn) return 'violated';
        if (pfeOn && simOn) return 'satisfied';
        return 'idle';
    }

    function ruleBrokenFor(code) {
        var entry = analyticOf(code);
        if (!entry || !entry.requires) return false;
        var wantOn = proposedRow(code) !== null && proposedRow(code).active === 'Y';
        if (!wantOn) return false;
        return !activeOf(entry.requires);
    }

    function paramDiff(stored, proposed) {
        var changed = [];
        (proposed ? proposed.params : []).forEach(function (param) {
            var was = null;
            (stored ? stored.params : []).forEach(function (old) {
                if (old.name === param.name) was = old.value;
            });
            if (was !== param.value) {
                changed.push({ name: param.name, was: was, will: param.value });
            }
        });
        return changed;
    }

    /* The difference between the stored set and the proposed one. A row that is
       turned off is "removed" from what the run produces and not from the
       configuration, which is the distinction the page exists to make. */
    function diff() {
        var changes = [];
        ANALYTICS.forEach(function (entry) {
            var stored = storedRow(entry.code);
            var proposed = proposedRow(entry.code);
            if (!proposed) return;
            if (!stored) {
                changes.push({ kind: 'added', code: entry.code, proposed: proposed });
                return;
            }
            if (stored.active !== proposed.active) {
                changes.push({
                    kind: proposed.active === 'Y' ? 'turnedOn' : 'turnedOff',
                    code: entry.code,
                    proposed: proposed
                });
                return;
            }
            var params = paramDiff(stored, proposed);
            if (params.length > 0) {
                changes.push({ kind: 'parameter', code: entry.code, params: params });
            }
        });
        return changes;
    }

    function writes() {
        var list = [];
        diff().forEach(function (change) {
            var stored = storedRow(change.code);
            if (change.kind === 'parameter') {
                change.params.forEach(function (param) {
                    list.push({
                        entity: 'configuration_analytic_parameter',
                        field: change.code + '.' + param.name,
                        was: param.was === null ? '\u2014' : param.was,
                        will: param.will
                    });
                });
                return;
            }
            list.push({
                entity: 'configuration_analytic',
                field: change.code + '.active',
                was: change.kind === 'added' ? 'no row' : (stored ? stored.active : 'no row'),
                will: change.kind === 'turnedOff'
                    ? 'N, row kept at display_order ' + change.proposed.order
                    : 'Y, display_order ' + change.proposed.order
            });
            (change.proposed.params || []).forEach(function (param) {
                var was = null;
                (stored ? stored.params : []).forEach(function (old) {
                    if (old.name === param.name) was = old.value;
                });
                if (was === param.value) return;
                list.push({
                    entity: 'configuration_analytic_parameter',
                    field: change.code + '.' + param.name,
                    was: was === null ? '\u2014' : was,
                    will: param.value
                });
            });
        });
        return list;
    }

    function logStep(text) {
        S.log.push(text);
    }

    // ------------------------------------------------------------ staging

    function refuse(message) {
        S.refusal = message;
        logStep('refused: ' + message);
        S.activity = 'refused';
    }

    function stageToggle(code) {
        S.refusal = null;
        var proposed = proposedRow(code);
        var wantOn = proposed === null || proposed.active !== 'Y';

        var pfeOn = code === 'pfe' ? wantOn : activeOf('pfe');
        var simOn = code === 'simulation' ? wantOn : activeOf('simulation');
        if (pfeOn && !simOn) {
            refuse(code + ' cannot be on without simulation');
            render();
            return;
        }

        if (!wantOn && storedRow(code) === null) {
            delete S.draft[code];
            S.activity = 'removed the staged add of ' + code + ' \u00b7 nothing was stored';
            logStep('unstaged the new row for ' + code);
            render();
            return;
        }

        var row;
        if (proposed === null) {
            row = { code: code, active: 'Y', order: nextOrder(), params: defaultParams(code) };
        } else {
            row = clone(proposed);
            row.active = wantOn ? 'Y' : 'N';
        }
        setDraft(code, row);
        S.activity = (wantOn ? 'turned on ' : 'turned off ') + code + ' \u00b7 not yet written';
        logStep('staged ' + code + ' active=' + row.active);
        render();
    }

    function defaultParams(code) {
        var entry = analyticOf(code);
        if (!entry) return [];
        return entry.params.map(function (param) {
            return { name: param.name, value: param.value };
        });
    }

    /* A draft entry equal to the stored row is not a change, so it is dropped and
       the difference stays honest. */
    function setDraft(code, row) {
        var stored = storedRow(code);
        if (stored && stored.active === row.active) {
            var params = paramDiff(stored, row);
            var reorder = row.order !== stored.order;
            if (params.length === 0 && !reorder) {
                delete S.draft[code];
                return;
            }
        }
        S.draft[code] = row;
    }

    /* As a fixture the page opens with two conflicting changes staged, so the
       refusal is on screen rather than two clicks away. */
    function seed() {
        if (S.staged) return;
        S.draft.pfe = {
            code: 'pfe', active: 'Y', order: 6, params: defaultParams('pfe')
        };
        S.draft.simulation = {
            code: 'simulation', active: 'N', order: 5, params: defaultParams('simulation')
        };
        S.staged = true;
        logStep('the page opens with pfe staged on and simulation staged off, as a fixture');
    }

    function save() {
        var list = writes();
        Object.keys(S.draft).forEach(function (code) {
            var row = S.draft[code];
            var stored = storedRow(code);
            if (stored) {
                stored.active = row.active;
                stored.order = row.order;
                stored.params = clone(row.params);
            } else {
                S.rows.push(clone(row));
            }
        });
        S.draft = {};
        S.staged = true;
        S.readback = { rows: S.rows.length, version: NEXT_VERSION, list: list };
        S.saved = true;
        S.confirm = null;
        S.step = 3;
        S.activity = 'written \u00b7 ' + list.length + ' row(s), definition version ' + NEXT_VERSION;
        logStep('would send reporting.v1.configuration_analytics.put_many and ' +
            'configuration_analytic_parameters.put \u2014 nothing is sent');
        render();
    }

    // ------------------------------------------------------------ the flow

    function startFlow(screen, code) {
        S.flow = { screen: screen, code: code || null, values: {}, at: 0 };
        if (code) fillValues(code);
        S.refusal = null;
        S.activity = 'add analytic \u00b7 ' + screen;
        logStep('opened the add-analytic flow at ' + screen + (code ? ' for ' + code : ''));
    }

    function fillValues(code) {
        var stored = storedRow(code);
        var entry = analyticOf(code);
        S.flow.values = {};
        entry.params.forEach(function (param) {
            var value = param.value;
            (stored ? stored.params : []).forEach(function (old) {
                if (old.name === param.name) value = old.value;
            });
            S.flow.values[param.name] = value;
        });
        S.flow.at = 0;
    }

    function closeFlow() {
        S.flow = null;
        S.refusal = null;
        S.activity = 'rest';
    }

    function flowValuesAsParams(code) {
        var entry = analyticOf(code);
        return entry.params.map(function (param) {
            return { name: param.name, value: S.flow.values[param.name] };
        });
    }

    function commitable(code) {
        return !ruleBrokenFor(code) && !(code === 'pfe' && !activeOf('simulation'));
    }

    function commitAnalytic(code) {
        var wantOn = code === 'pfe' && !activeOf('simulation');
        if (wantOn) {
            refuse(code + ' cannot be on without simulation');
            render();
            return;
        }
        var stored = storedRow(code);
        var row = {
            code: code,
            active: 'Y',
            order: stored ? stored.order : nextOrder(),
            params: flowValuesAsParams(code)
        };
        setDraft(code, row);
        var message = (stored ? 'staged new settings for ' : 'staged ') + code + ' \u00b7 not yet written';
        closeFlow();
        S.activity = message;
        logStep(message);
        S.step = 2;
        render();
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
            'the shared configuration they belong to, then the write. Turning one off writes ' +
            '<b>active = false</b> and keeps the row. Changing the set makes every run made after it ' +
            'incomparable with the runs before it.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Fixture clock 2026-10-17 10:20</span>' +
            '<button class="btn" data-act="toggle-state">Show state</button>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>PROTOTYPE, MOCK DATA.</b> ' +
            'Every analytic, parameter, run, version and date on this page is a fixture, and nothing ' +
            'reads a server. Two statements on it are not fixture: <b>an analytic is turned off by ' +
            'setting active to false, not by deleting the row</b>, and <b>pfe cannot run without ' +
            'simulation</b>. The first is how the model works; the second is a rule the ACME seeder ' +
            'enforces and no service does, so this page enforces it itself. The page opens with pfe ' +
            'staged on and simulation staged off, so the refusal is visible as a fixture. <b>No ' +
            'operation says which runs were made with which configuration version</b>: the runs are ' +
            'placed by their own start time against the definition\u2019s version history, which is an ' +
            'inference. There is no BFF route for reporting and no reporting UI in the application.</div>';
    }

    function stepper() {
        var items = STEPS.map(function (step) {
            var cls = 'wstep';
            if (step.n === S.step) cls += ' current';
            else if (step.n < S.step) cls += ' done';
            var tag = '';
            if (step.n === 3 && ruleState() === 'violated') {
                tag = '<span class="tag bad">the set cannot run</span>';
            }
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
        var next = '<button class="btn primary" data-act="step" data-step="' + nextStep + '">' +
            esc(nextLabel) + '</button>';
        return '<div class="stepnav">' + back + next + '</div>';
    }

    // ------------------------------------------------------------- step one

    function configRow(binding) {
        var where;
        if (binding.route) {
            where = '<a class="route" href="' + esc(binding.route.path) + '" title="' +
                esc(binding.route.screen) + ' in the application">' +
                '<span class="mono">' + esc(binding.route.path) + '</span>' +
                '<span class="arrow">\u2192</span>' + esc(binding.route.screen) + '</a>' +
                '<div class="hint">real route \u00b7 ' + esc(binding.route.symbol) + '</div>';
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
            '<div class="notice" style="margin-top:12px">' + esc(NO_CONFIG_SCREEN) +
            ' In this static page the one link points at the application route and does not resolve ' +
            'from file://; it is drawn because the route is real.</div>' +
            '<div class="notice" style="margin-top:8px"><b>' + esc(CONFIGURATION.name) + '</b> ' +
            '<span class="mono">' + esc(CONFIGURATION.id) + '</span> carries the analytics, and four ' +
            'definitions bind it: this one, ' + esc(SHARED_WITH.join(', ')) + '. ' + esc(SHARED) +
            ' The analytics are not on the definition, so everything the next two steps change reaches ' +
            'all four.</div>' +
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
            title = '<span class="tag warn">rule between analytics</span> pfe is off, so the rule does ' +
                'not bind yet';
        }
        return '<div class="rule' + cls + '">' +
            '<div class="rtitle">' + title + '</div>' +
            '<div class="rbody"><b>pfe reads a cube that simulation writes</b>, so turning pfe on ' +
            'without simulation cannot work. The rule lives in the ACME seeder, in ' +
            '<span class="mono">generate_risk_config_data.py</span>, function ' +
            '<span class="mono">check_exposure_needs_simulation</span>, and in no service: nothing ' +
            'validates it when a configuration is written. This page marks the pair and refuses the ' +
            'step. A build that does not refuse it has to say that the run will fail instead of stating ' +
            'a validation it does not have.</div></div>';
    }

    function switchFor(code) {
        var row = proposedRow(code);
        if (!row) {
            return '<span class="swnone">no row</span>';
        }
        var on = row.active === 'Y';
        return '<button class="sw" role="switch" aria-checked="' + (on ? 'true' : 'false') + '" ' +
            'data-act="toggle" data-code="' + esc(code) + '" title="Writes active ' + (on ? 'N' : 'Y') +
            '. The row is never deleted.">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (on ? 'on' : 'off') + '">' + (on ? 'Y' : 'N') + '</span></button>';
    }

    function paramsSummary(row) {
        if (!row || row.params.length === 0) {
            return '<span class="hint">no parameters</span>';
        }
        return '<div class="an-params">' + row.params.map(function (param) {
            return '<span class="p"><span class="pn">' + esc(param.name) + '</span>' +
                '<span class="pv">' + esc(param.value) + '</span></span>';
        }).join('') + '</div>';
    }

    function analyticRow(code) {
        var stored = storedRow(code);
        var row = proposedRow(code);
        var change = diff().filter(function (entry) { return entry.code === code; });
        var conflict = ruleState() === 'violated' && (code === 'pfe' || code === 'simulation');
        var cls = 'anrow';
        if (!row) cls += ' norow';
        else if (row.active !== 'Y') cls += ' isoff';
        if (change.length > 0) cls += ' changed';
        if (conflict) cls += ' conflict';

        var entry = analyticOf(code);
        var badges = [];
        if (!row) {
            badges.push('<span class="tag muted">no row on this configuration</span>');
        } else if (row.active !== 'Y') {
            badges.push('<span class="tag muted">off \u00b7 row kept at display_order ' + row.order +
                '</span>');
        }
        if (change.length > 0) badges.push('<span class="tag accent">staged</span>');
        if (conflict && code === 'pfe') {
            badges.push('<span class="tag bad">needs simulation \u00b7 cannot run</span>');
        }
        if (conflict && code === 'simulation') {
            badges.push('<span class="tag bad">pfe is on \u00b7 turning this off breaks it</span>');
        }

        return '<div class="' + cls + '">' +
            '<div class="an-top">' +
            switchFor(code) +
            '<span class="an-name">' + esc(entry.name) +
            '<span class="codename">' + esc(entry.code) + '</span>' +
            '<span class="an-produces">' + esc(entry.produces) + '</span></span>' +
            '<span class="an-order">' + (row ? 'order ' + row.order : '\u2014') + '</span>' +
            '<span class="an-summary">' + paramsSummary(row) + '</span>' +
            '<span class="an-act"><button class="btn small" data-act="settings" data-code="' +
            esc(code) + '">' + (row ? 'Settings' : 'Add it') + '</button></span>' +
            '</div>' +
            (badges.length > 0 ? '<div class="cellgroup an-badges">' + badges.join('') + '</div>' : '') +
            '</div>';
    }

    function switchboardCard() {
        var onCount = ANALYTICS.filter(function (entry) { return activeOf(entry.code); }).length;
        var rowCount = ANALYTICS.filter(function (entry) { return proposedRow(entry.code); }).length;
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The switchboard: every analytic type, one row each</h2>' +
            '<span class="meta">reporting.v1.configuration_analytics.list_by_configuration_id \u00b7 ' +
            '14 types \u00b7 ' + rowCount + ' rows \u00b7 ' + onCount + ' on \u00b7 fixture</span></header>' +
            ruleStrip() + refusalNote() +
            '<div class="anlist">' + ANALYTICS.map(function (entry) {
                return analyticRow(entry.code);
            }).join('') + '</div>' +
            '<div class="reviewbar">' +
            '<span class="count">The switchboard summarises each analytic\u2019s parameters and keeps ' +
            'the on/off control and the order. The controls for the settings live on the add-analytic ' +
            'flow, which configures one analytic at a time.</span>' +
            '<button class="btn primary" data-act="add-analytic">Add analytic</button>' +
            '</div>' +
            '<p class="rowfoot">' + esc(NO_DELETE) + ' A type with no row has no switch: adding it is ' +
            'the <b>Add analytic</b> flow, which sets its parameters and then writes the row ' +
            '(reporting.v1.configuration_analytics.put). Every switch shown writes an existing row. ' +
            esc(DEFERRED_EDITORS) + '</p>' +
            '</section>';
    }

    function refusalNote() {
        if (!S.refusal) return '';
        return '<div class="notice err"><b>Refused here, not by the server.</b> ' +
            esc(S.refusal) + '. ' + esc(PFE_RULE) + '</div>';
    }

    function stepTwo() {
        return (S.flow ? flowCard() : switchboardCard()) + stepNav('Next: review and save', 3, 1);
    }

    // ------------------------------------------------- the add-analytic flow

    function flowCrumbs() {
        var screen = S.flow.screen;
        var order = { list: 1, detail: 2, effect: 3 };
        return '<div class="flowhead"><div class="crumbs">' +
            '<button class="linkbtn" data-act="flow-close">The analytics</button><span>/</span>' +
            '<button class="linkbtn" data-act="flow-list">Add an analytic</button>' +
            (screen === 'list' ? '' : '<span>/</span><span>' +
                (screen === 'detail' ? 'The detail' : 'The effect') + '</span>') +
            '</div><div class="flowsteps">' +
            '<span class="fs' + (screen === 'list' ? ' on' : '') + '">1 the list</span>' +
            '<span class="fs' + (screen === 'detail' ? ' on' : '') + '">2 the detail</span>' +
            '<span class="fs' + (screen === 'effect' ? ' on' : '') + '">3 the effect</span>' +
            '</div></div>';
    }

    function rowStateTag(code) {
        var row = proposedRow(code);
        var stored = storedRow(code);
        var staged = S.draft[code] ? ' \u00b7 staged' : '';
        if (!row) return '<span class="tag muted">no row</span>';
        if (row.active !== 'Y') {
            return '<span class="tag muted">off' + (stored ? '' : ' \u00b7 added') + staged + '</span>';
        }
        return '<span class="tag ok">on' + (stored ? '' : ' \u00b7 new row') + staged + '</span>';
    }

    function flowList() {
        var rows = ANALYTICS.map(function (entry) {
            return '<div class="pickrow">' +
                '<span class="pickname">' + esc(entry.name) +
                '<span class="codename">' + esc(entry.code) + '</span></span>' +
                '<span class="pickwhat">' + esc(entry.produces) + '</span>' +
                '<span class="pickstate">' + rowStateTag(entry.code) + '</span>' +
                '<button class="btn small" data-act="choose" data-code="' + esc(entry.code) +
                '">Choose</button></div>';
        }).join('');
        return '<section class="card">' + flowCrumbs() +
            '<header class="sectionhead"><h2>Add an analytic: the fourteen types</h2>' +
            '<span class="meta">reporting.v1.analytic_types.list \u00b7 fixture</span></header>' +
            '<p class="hint" style="margin-bottom:10px">Each type states what it produces and whether ' +
            'this configuration already carries it. "On the configuration" and "on" are different ' +
            'things: a row that is off still exists, and a type with no row was never asked for.</p>' +
            '<div class="picklist">' + rows + '</div>' +
            '<div class="stepnav"><button class="btn" data-act="flow-close">Cancel</button>' +
            '<span></span></div>' +
            '</section>';
    }

    function control(name, value) {
        var entry = analyticOf(S.flow.code);
        var param = entry.params.filter(function (p) { return p.name === name; })[0];
        var common = ' data-pact="param" data-name="' + esc(param.name) + '" aria-label="' +
            esc(param.name) + ' of ' + esc(S.flow.code) + '"';
        if (param.shape === 'number') {
            return '<input type="number" step="any" class="ctl" value="' + esc(value) + '"' + common + '>';
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
        return '<button class="sw" role="switch" aria-checked="' + (on ? 'true' : 'false') + '"' +
            common + ' data-toggle="1">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (on ? 'on' : 'off') + '">' + (on ? 'on' : 'off') +
            '</span></button>';
    }

    function paramCard(param) {
        var value = S.flow.values[param.name];
        var deferred = param.deferred
            ? '<div class="deferred"><span class="tag warn">deferred editor</span> ' +
              esc(param.deferred) + '</div>'
            : '';
        return '<div class="pc">' +
            '<div class="pch"><span class="pname">' + esc(param.name) + '</span>' +
            '<span class="tag muted">' + esc(param.shape) + '</span></div>' +
            '<div class="pcrow">' + control(param.name, value) + '</div>' +
            '<p class="phelp">' + esc(param.help) + ' <span class="pnormal">A normal value is ' +
            esc(param.normal.replace(/^[A-Z]/, function (first) { return first.toLowerCase(); })) +
            '</span></p>' + deferred +
            '</div>';
    }

    function paramsColumn() {
        var entry = analyticOf(S.flow.code);
        return '<div class="pcol">' + entry.params.map(paramCard).join('') + '</div>';
    }

    function explanationBlock() {
        var entry = analyticOf(S.flow.code);
        return '<div class="explain">' +
            '<h3>What ' + esc(entry.name.toLowerCase()) + ' is</h3>' +
            '<p>' + esc(entry.what) + '</p>' +
            '<h3>How it is calculated</h3>' +
            '<p>' + esc(entry.how) + '</p>' +
            '<p class="hint mono">' + esc(entry.code) + '</p>' +
            '</div>';
    }

    function effectBlock(inline) {
        var entry = analyticOf(S.flow.code);
        var needs = entry.requires
            ? '<div class="' + (ruleBrokenFor(entry.code) ? 'notice err' : 'notice ok') +
              '"><b>' + esc(entry.code) + ' needs ' + esc(entry.requires) + '.</b> ' +
              (ruleBrokenFor(entry.code)
                  ? 'It is off in the set on the table, so this analytic cannot be turned on. '
                  : 'It is on in the set on the table, so the rule is satisfied. ') +
              esc(PFE_RULE) + '</div>'
            : '<div class="notice"><b>No other analytic has to be on.</b> The one rule between ' +
              'analytics binds pfe to simulation and nothing else, so this analytic stands alone.</div>';
        var body = '<div class="effect">' +
            '<h3>What turning ' + esc(entry.code) + ' on does</h3>' +
            '<p>' + esc(entry.effect) + '</p>' +
            '<ul class="effects">' +
            '<li>The next run of every definition bound to ' + esc(CONFIGURATION.name) + ' produces it: ' +
            'this definition and ' + esc(SHARED_WITH.join(', ')) + '.</li>' +
            '<li>It writes one analytic row: <span class="mono">' + esc(entry.code) +
            '.active = Y</span>, and one parameter row per parameter below.</li>' +
            '<li>The runs already produced keep what they ran with. A run after this change is not ' +
            'comparable with one before it.</li>' +
            '</ul>' + needs + '</div>';
        return body;
    }

    function detailActions() {
        return '<div class="stepnav">' +
            '<button class="btn" data-act="flow-list">Back to the list</button>' +
            '<button class="btn primary" data-act="flow-effect">Continue to the effect</button>' +
            '</div>';
    }

    function deferredNotice() {
        var entry = analyticOf(S.flow.code);
        if (entry.paramEditor !== 'deferred') return '';
        return '<div class="notice warn"><b>' + esc(entry.code) + ' needs a purpose-built editor, ' +
            'and this page does not draw it.</b> ' + esc(entry.code) + ' carries ' +
            esc(entry.editorNeeds) + ', which a plain field cannot show well. The settings below are ' +
            'drawn as plain fields for now, and the purpose-built editor is a later task. ' +
            esc(DEFERRED_EDITORS) + '</div>';
    }

    /* A, document: the explanation as a short readable page, the parameters in a
       single column below it. */
    function detailA() {
        return '<section class="card">' + flowCrumbs() +
            '<header class="sectionhead"><h2>Add ' + esc(analyticOf(S.flow.code).name) +
            ': what it is, and every setting it takes</h2>' +
            '<span class="meta">variant A \u00b7 document \u00b7 ' +
            'reporting.v1.configuration_analytic_parameters.list_by_configuration_analytic_id \u00b7 ' +
            'fixture</span></header>' +
            '<div class="doc">' + explanationBlock() + deferredNotice() + '</div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>The parameters of ' +
            esc(S.flow.code) + '</h3><span class="hint">' +
            analyticOf(S.flow.code).params.length + ' setting(s) \u00b7 one column</span></div>' +
            paramsColumn() + detailActions() + '</section>';
    }

    /* B, two columns: explanation and calculation on the left, parameters and the
       effect on the right. */
    function detailB() {
        return '<section class="card">' + flowCrumbs() +
            '<header class="sectionhead"><h2>Add ' + esc(analyticOf(S.flow.code).name) +
            ': read on the left, set on the right</h2>' +
            '<span class="meta">variant B \u00b7 two columns \u00b7 fixture</span></header>' +
            '<div class="twocol">' +
            '<div class="left">' + explanationBlock() + deferredNotice() + '</div>' +
            '<div class="right">' +
            '<div class="sectionhead"><h3>The parameters</h3><span class="hint">' +
            analyticOf(S.flow.code).params.length + ' setting(s)</span></div>' +
            paramsColumn() + effectBlock() + '</div>' +
            '</div>' + detailActions() + '</section>';
    }

    /* C, guided: one parameter at a time, the explanation beside it, advancing,
       and a summary at the end. */
    function detailC() {
        var entry = analyticOf(S.flow.code);
        var at = S.flow.at;
        var total = entry.params.length;
        var onSummary = at >= total;
        var side = '<div class="left">' + explanationBlock() + '</div>';
        var step;
        if (onSummary) {
            step = '<div class="guided"><div class="gbar"><span class="gnum">Summary</span>' +
                '<span class="hint">' + total + ' setting(s), as they will be written</span></div>' +
                '<div class="summary">' + entry.params.map(function (param) {
                    return '<div class="srow"><span class="sname">' + esc(param.name) + '</span>' +
                        '<span class="svalue">' + esc(S.flow.values[param.name]) + '</span></div>';
                }).join('') + '</div>' +
                '<p class="hint">Nothing is written yet. The effect screen states what this does to ' +
                'the next run and to the runs already produced.</p>' +
                '<div class="gnav">' +
                '<button class="btn" data-act="guided-back">Back</button>' +
                '<button class="btn primary" data-act="flow-effect">Continue to the effect</button>' +
                '</div></div>';
        } else {
            var param = entry.params[at];
            step = '<div class="guided">' +
                '<div class="gbar"><span class="gnum">Parameter ' + (at + 1) + ' of ' + total + '</span>' +
                '<span class="hint">one at a time \u00b7 ' + esc(param.shape) + '</span></div>' +
                '<div class="gp">' + paramCard(param) + '</div>' +
                '<div class="gnav">' +
                (at === 0
                    ? '<button class="btn" data-act="flow-list">Back to the list</button>'
                    : '<button class="btn" data-act="guided-back">Back</button>') +
                '<button class="btn primary" data-act="guided-next">' +
                (at + 1 === total ? 'Finish and review' : 'Next parameter') + '</button>' +
                '</div></div>';
        }
        return '<section class="card">' + flowCrumbs() +
            '<header class="sectionhead"><h2>Add ' + esc(entry.name) +
            ': one setting at a time</h2>' +
            '<span class="meta">variant C \u00b7 guided \u00b7 fixture</span></header>' +
            '<div class="twocol">' + side + '<div class="right">' + step + '</div></div>' +
            '</section>';
    }

    function flowDetail() {
        if (S.variant === 'B') return detailB();
        if (S.variant === 'C') return detailC();
        return detailA();
    }

    function flowEffect() {
        var entry = analyticOf(S.flow.code);
        var stored = storedRow(S.flow.code);
        var refused = ruleBrokenFor(entry.code) || (entry.code === 'pfe' && !activeOf('simulation'));
        var refusedPanel = refused
            ? '<div class="rule err"><div class="rtitle">' +
              '<span class="tag bad">refused \u00b7 cannot be turned on</span>' +
              esc(entry.code) + ' needs ' + esc(entry.requires) + ', and it is off in the set on the ' +
              'table</div><div class="rbody">This step is refused here with the rule stated above. ' +
              'Nothing on the server would refuse it: the write would be accepted and the run would ' +
              'fail later. The rule lives in the ACME seeder, in ' +
              '<span class="mono">check_exposure_needs_simulation</span>, and in no service.' +
              '</div></div>' +
              '<div class="draftrow">' +
              (entry.requires
                  ? '<button class="btn" data-act="fix-requires">Turn ' + esc(entry.requires) +
                    ' on as well</button>'
                  : '') +
              '<button class="btn" data-act="flow-detail">Back to the settings</button>' +
              '<button class="btn ghost" data-act="flow-close">Cancel</button></div>'
            : '<div class="draftrow">' +
              '<button class="btn" data-act="flow-detail">Back to the settings</button>' +
              '<button class="btn ghost" data-act="flow-close">Cancel</button>' +
              '<button class="btn primary" data-act="commit" data-code="' + esc(entry.code) + '">' +
              (stored ? 'Save these settings' : 'Turn ' + esc(entry.code) + ' on') + '</button></div>';
        return '<section class="card">' + flowCrumbs() +
            '<header class="sectionhead"><h2>The effect of adding ' + esc(entry.name) + '</h2>' +
            '<span class="meta">what it reaches, and what it cannot do \u00b7 fixture</span></header>' +
            effectBlock() +
            '<div class="sectionhead" style="margin-top:12px"><h3>Exactly what this write adds</h3>' +
            '<span class="meta">configuration_analytics.put_many \u00b7 ' +
            'configuration_analytic_parameters.put</span></div>' +
            flowWrites() +
            (refused ? '<div style="margin-top:12px">' + refusedPanel + '</div>' : refusedPanel) +
            '</section>';
    }

    function flowWrites() {
        var entry = analyticOf(S.flow.code);
        var stored = storedRow(entry.code);
        var list = [{
            entity: 'configuration_analytic',
            field: entry.code + '.active',
            was: stored ? stored.active : 'no row',
            will: 'Y, display_order ' + (stored ? stored.order : nextOrder())
        }];
        entry.params.forEach(function (param) {
            var value = S.flow.values[param.name];
            var was = null;
            (stored ? stored.params : []).forEach(function (old) {
                if (old.name === param.name) was = old.value;
            });
            if (was === value) return;
            list.push({
                entity: 'configuration_analytic_parameter',
                field: entry.code + '.' + param.name,
                was: was === null ? '\u2014' : was,
                will: value
            });
        });
        return '<div class="writes">' +
            '<div class="wh"><span class="ent">entity</span><span class="fld">field</span>' +
            '<span class="was">was</span><span class="will">will be</span></div>' +
            list.map(function (write) {
                return '<div class="w"><span class="ent">' + esc(write.entity) + '</span>' +
                    '<span class="fld">' + esc(write.field) + '</span>' +
                    '<span class="was">' + esc(write.was) + '</span>' +
                    '<span class="will"><span class="arrow">\u2192 </span>' + esc(write.will) +
                    '</span></div>';
            }).join('') + '</div>';
    }

    function flowCard() {
        if (S.flow.screen === 'list') return flowList();
        if (S.flow.screen === 'detail') return flowDetail();
        return flowEffect();
    }

    // ----------------------------------------------------------- step three

    function writesBlock() {
        var list = writes();
        if (list.length === 0) {
            return '<div class="notice">No change is staged. Turn an analytic on or off on the ' +
                'switchboard, or add one, and the write appears here before anything is sent.</div>';
        }
        return '<div class="writes">' +
            '<div class="wh"><span class="ent">entity</span><span class="fld">field</span>' +
            '<span class="was">was</span><span class="will">will be</span></div>' +
            list.map(function (write) {
                return '<div class="w"><span class="ent">' + esc(write.entity) + '</span>' +
                    '<span class="fld">' + esc(write.field) + '</span>' +
                    '<span class="was">' + esc(write.was) + '</span>' +
                    '<span class="will"><span class="arrow">\u2192 </span>' + esc(write.will) +
                    '</span></div>';
            }).join('') + '</div>';
    }

    function reviewBar() {
        var list = writes();
        var rows = list.filter(function (write) { return write.entity === 'configuration_analytic'; })
            .length;
        var params = list.length - rows;
        return '<div class="reviewbar">' +
            '<span class="count"><b>' + rows + '</b> analytic row(s) and <b>' + params +
            '</b> parameter row(s) will be written to ' + esc(CONFIGURATION.name) +
            '. No row is deleted and no run is started.</span>' +
            '<button class="btn primary" data-act="ask-save"' +
            (list.length === 0 || ruleState() === 'violated' ? ' disabled' : '') +
            '>Write these analytics</button>' +
            '<button class="btn ghost" data-act="reset"' + (list.length === 0 ? ' disabled' : '') +
            '>Discard</button></div>';
    }

    function readbackNote() {
        if (!S.readback) return '';
        return '<div class="notice ok"><b>Read back from the configuration.</b> ' +
            S.readback.rows + ' analytic rows are stored. ' +
            'The screen reads the configuration again and draws the stored values, not the values the ' +
            'person typed. The write would be recorded as definition version ' + S.readback.version +
            '; the older versions stay readable. No run has produced the new set yet.</div>';
    }

    function reachCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>What this change does and does not reach</h2>' +
            '<span class="meta">stated before the write \u00b7 fixture</span></header>' +
            '<div class="reach">' +
            '<div class="does"><h3>What the change reaches</h3><ul>' +
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
            '<li>The configuration\u2019s market roles, which this page reads and does not write.</li>' +
            '<li>Any run in flight: no operation in this journey stops one.</li></ul></div>' +
            '</div>' +
            '<div class="notice warn" style="margin-top:12px"><b>A run either side of this change is ' +
            'not a comparison.</b> ' + esc(INCOMPARABLE) + '</div>' +
            '<p class="rowfoot">' + esc(PAST_RUNS) + '</p></section>';
    }

    function runsCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The runs this change strands</h2>' +
            '<span class="meta">report_definitions_versions.list \u00b7 placed by start time \u00b7 ' +
            'inference</span></header>' +
            '<div class="versions">' + VERSIONS.map(function (version) {
                return '<div class="vrow"><span class="vno">v' + version.version + '</span>' +
                    '<span class="vat">' + esc(version.at) + '</span>' +
                    '<span class="vnote">' + esc(version.note) + '</span></div>';
            }).join('') +
            '<div class="vrow"><span class="vno">v' + NEXT_VERSION + '</span>' +
            '<span class="vat">not yet written</span>' +
            '<span class="vnote">this change, if it is saved</span></div></div>' +
            '<div class="runs" style="margin-top:12px">' + RUNS.map(function (run) {
                return '<div class="runrow">' +
                    '<span class="rid">' + esc(run.id) + '</span>' +
                    '<span class="rat">' + esc(run.at) + '</span>' +
                    '<span class="rstate">' + esc(run.state) + ', ' + run.errors + ' error(s) \u00b7 ' +
                    'started after v6, so it ran the stored set (an inference)' +
                    '<span class="tag ' + (S.saved ? 'bad' : 'warn') + '">' +
                    (S.saved ? 'incomparable' : 'becomes incomparable') + '</span></span></div>';
            }).join('') + '</div></section>';
    }

    function stepThree() {
        var refusal = ruleState() === 'violated'
            ? '<div class="rule err" style="margin-bottom:10px"><div class="rtitle">' +
              '<span class="tag bad">not writable yet</span> this set cannot run, so the wizard ' +
              'refuses the write</div><div class="rbody">' + esc(PFE_RULE) + ' Go back to step 2 and ' +
              'turn simulation on, or turn pfe off.</div></div>'
            : '';
        return reachCard() + runsCard() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Exactly what will be written</h2>' +
            '<span class="meta">configuration_analytics.put_many \u00b7 ' +
            'configuration_analytic_parameters.put \u00b7 fixture</span></header>' +
            refusal + writesBlock() + reviewBar() + readbackNote() +
            '<p class="rowfoot">The set is sent in one call, so a turned-on analytic, a turned-off one ' +
            'and a changed parameter land together. A build then reads the configuration back and draws ' +
            'the stored values. No row is deleted, and the definition gets a new version while the ' +
            'older versions stay readable.</p></section>' +
            '<div class="stepnav"><button class="btn" data-act="step" data-step="2">Back to the ' +
            'analytics</button><span></span></div>';
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

    // ------------------------------------------------------------ dialog

    function dialog() {
        if (S.confirm !== 'save') return '';
        var list = writes();
        return '<div class="backdrop"><div class="dialog" role="dialog" aria-modal="true">' +
            '<h2>Write these analytics?</h2>' +
            '<p class="confirmline"><b>' + esc(DEFINITION.name) + '</b> \u00b7 def ' +
            '<span class="mono">' + esc(DEFINITION.id) + '</span> \u00b7 party <b>' + esc(PARTY.code) +
            '</b> \u00b7 configuration <b>' + esc(CONFIGURATION.name) + '</b> ' +
            '<span class="mono">' + esc(CONFIGURATION.id) + '</span></p>' +
            '<div class="sectionhead"><h3>Exactly what will be written</h3>' +
            '<span class="hint">' + list.length + ' row(s)</span></div>' +
            writesBlock() +
            '<div class="notice">' + esc(NO_DELETE) + '</div>' +
            '<div class="notice">' + esc(SHARED) + '</div>' +
            '<div class="notice warn">' + esc(PAST_RUNS) + ' ' + esc(INCOMPARABLE) + '</div>' +
            '<div class="notice">Rule between analytics: ' + (ruleState() === 'violated'
                ? '<b>pfe is on and simulation is off, so this page refuses the change.</b>'
                : (ruleState() === 'satisfied'
                    ? 'pfe and simulation are both on, so it is satisfied.'
                    : 'pfe is off, so it does not bind.')) + ' ' + esc(PFE_RULE) + '</div>' +
            '<p class="hint">Operation: ' +
            '<span class="mono">reporting.v1.configuration_analytics.put_many</span> for the analytic ' +
            'rows, and <span class="mono">reporting.v1.configuration_analytic_parameters.put</span> for ' +
            'the parameter rows. This prototype sends nothing.</p>' +
            '<div class="actions">' +
            '<button class="btn" data-act="confirm-cancel">Cancel</button>' +
            '<button class="btn primary" data-act="confirm-go"' +
            (ruleState() === 'violated' ? ' disabled' : '') + '>Confirm write</button>' +
            '</div></div></div>';
    }

    // --------------------------------------------------------------- bars

    function renderBar() {
        var variantButtons = VARIANTS.map(function (entry) {
            return '<button data-act="variant" data-variant="' + entry.id + '"' +
                (S.variant === entry.id ? ' class="on"' : '') + '>' + esc(entry.id) + '</button>';
        }).join('');

        var stateLine = '<span class="label">step <b>' + S.step + '</b> \u00b7 variant <b>' +
            esc(S.variant) + '</b> ' + esc(variant().name) +
            (S.flow ? ' \u00b7 flow: <b>' + esc(S.flow.screen) + '</b>' +
                (S.flow.code ? ' (' + esc(S.flow.code) + ')' : '') : '') +
            ' \u00b7 state: <b>' + esc(S.activity) + '</b></span>';

        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>analytic types: <b>' + ANALYTICS.length + '</b></span>' +
            '<span>rows stored: <b>' + S.rows.length + '</b></span>' +
            '<span>staged writes: <b>' + writes().length + '</b></span>' +
            '<span>pfe / simulation: <b>' + (activeOf('pfe') ? 'on' : 'off') + ' / ' +
            (activeOf('simulation') ? 'on' : 'off') + '</b></span>' +
            '<span>rule: <b>' + ruleState() + '</b></span>' +
            '<span>definition version: <b>' + (S.saved ? NEXT_VERSION : 'not written') + '</b></span>' +
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
            '<span class="tag-prototype">Prototype</span>' + variantButtons +
            '<span class="sep">|</span>' + stateLine +
            '<span class="gist">' + esc(variant().gist) + '</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') +
            '</button></div>' + statePanel;
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 step ' + S.step + ' \u00b7 variant ' +
            S.variant + ' \u00b7 state ' + S.activity + ' \u00b7 ' + DEFINITION.name + ', party ' +
            PARTY.code;
    }

    function render() {
        var body = pageHead() + fixtureNote() + stepper() +
            (S.step === 1 ? stepOne() : (S.step === 2 ? stepTwo() : stepThree())) +
            (S.step === 3 ? gapCard() : '');
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() + '<main>' + body + '</main></div>' + dialog();
        renderNote();
        renderBar();
    }

    // ------------------------------------------------------------- params

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var wanted = (params.get('variant') || '').toUpperCase();
        if (VARIANTS.some(function (entry) { return entry.id === wanted; })) S.variant = wanted;

        var step = parseInt(params.get('step'), 10);
        if (step >= 1 && step <= STEPS.length) {
            S.step = step;
        } else {
            S.step = params.get('variant') ? 2 : 1;
        }

        var screen = params.get('screen');
        var code = params.get('analytic');
        var known = code && analyticOf(code) ? code : null;
        if ((screen === 'list' || screen === 'detail' || screen === 'effect') || known) {
            S.step = 2;
            startFlow(screen || 'detail', known || 'xva');
        } else if (params.get('variant') && !params.get('step')) {
            S.step = 2;
            startFlow('detail', 'xva');
        }
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('step', String(S.step));
        params.set('variant', S.variant);
        if (S.flow) {
            params.set('screen', S.flow.screen);
            if (S.flow.code) params.set('analytic', S.flow.code);
        }
        try {
            window.history.replaceState(null, '',
                window.location.pathname + '?' + params.toString());
        } catch (error) {
            S.activity = S.activity;
        }
    }

    // ------------------------------------------------------------ actions

    function applyParam(element) {
        var name = element.getAttribute('data-name');
        if (!S.flow || !S.flow.values.hasOwnProperty(name)) return;
        if (element.getAttribute('data-toggle') === '1') {
            S.flow.values[name] = S.flow.values[name] === 'true' ? 'false' : 'true';
        } else {
            S.flow.values[name] = element.value;
        }
        S.activity = 'set ' + S.flow.code + '.' + name + ' = ' + S.flow.values[name] +
            ' \u00b7 not yet written';
        logStep('set ' + S.flow.code + '.' + name + ' = ' + S.flow.values[name]);
        render();
    }

    document.addEventListener('click', function (event) {
        var control = event.target.closest('[data-pact]');
        if (control && control.tagName === 'BUTTON') {
            event.preventDefault();
            applyParam(control);
            return;
        }
        var element = event.target.closest('[data-act]');
        if (!element) return;
        if (element.tagName !== 'BUTTON' && !element.classList.contains('linkbtn')) return;
        event.preventDefault();
        var act = element.getAttribute('data-act');
        var code = element.getAttribute('data-code');

        if (act === 'step') {
            S.step = parseInt(element.getAttribute('data-step'), 10);
            S.flow = null;
            S.confirm = null;
            logStep('went to step ' + S.step);
        } else if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            S.confirm = null;
            S.refusal = null;
            S.step = 2;
            startFlow('detail', S.flow && S.flow.code ? S.flow.code : 'xva');
            if (S.flow) fillValues(S.flow.code);
            logStep('switched to variant ' + S.variant + ', reading the detail screen');
        } else if (act === 'toggle') {
            stageToggle(code);
            writeParams();
            return;
        } else if (act === 'add-analytic') {
            S.step = 2;
            startFlow('list', null);
        } else if (act === 'settings') {
            S.step = 2;
            startFlow('detail', code);
        } else if (act === 'choose') {
            S.step = 2;
            startFlow('detail', code);
        } else if (act === 'flow-list') {
            startFlow('list', null);
        } else if (act === 'flow-detail') {
            S.flow.screen = 'detail';
            S.activity = 'add analytic \u00b7 detail';
        } else if (act === 'flow-effect') {
            S.flow.screen = 'effect';
            S.activity = 'add analytic \u00b7 effect';
            logStep('read the effect of ' + S.flow.code);
        } else if (act === 'flow-close') {
            closeFlow();
            S.step = 2;
        } else if (act === 'guided-next') {
            S.flow.at = Math.min(S.flow.at + 1, analyticOf(S.flow.code).params.length);
            S.activity = 'guided \u00b7 step ' + (S.flow.at + 1);
        } else if (act === 'guided-back') {
            S.flow.at = Math.max(0, S.flow.at - 1);
            S.activity = 'guided \u00b7 step ' + (S.flow.at + 1);
        } else if (act === 'fix-requires') {
            var requires = analyticOf(S.flow.code).requires;
            var stored = storedRow(requires);
            setDraft(requires, {
                code: requires,
                active: 'Y',
                order: stored ? stored.order : nextOrder(),
                params: defaultParams(requires)
            });
            S.refusal = null;
            S.activity = 'staged ' + requires + ' on, so ' + S.flow.code + ' can run \u00b7 not yet written';
            logStep('staged ' + requires + ' active=Y to satisfy the rule');
        } else if (act === 'commit') {
            commitAnalytic(code);
            writeParams();
            return;
        } else if (act === 'ask-save') {
            if (writes().length === 0 || ruleState() === 'violated') return;
            S.confirm = 'save';
            S.activity = 'reviewing the write';
            logStep('opened the review of ' + writes().length + ' row(s)');
        } else if (act === 'confirm-cancel') {
            S.confirm = null;
            S.activity = 'rest';
            logStep('cancelled, nothing sent');
            render();
            return;
        } else if (act === 'confirm-go') {
            save();
            writeParams();
            return;
        } else if (act === 'reset') {
            S.draft = {};
            S.confirm = null;
            S.refusal = null;
            S.readback = null;
            S.saved = false;
            S.activity = 'rest';
            logStep('discarded the staged change');
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
            return;
        }
        render();
        writeParams();
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-pact]');
        if (!element || element.tagName === 'BUTTON') return;
        applyParam(element);
        writeParams();
    });

    document.addEventListener('keydown', function (event) {
        if (event.key !== 'Escape') return;
        if (S.confirm === null) return;
        S.confirm = null;
        S.activity = 'rest';
        render();
    });

    S.rows = clone(ROWS);
    readParams();
    seed();
    render();
    writeParams();
}());

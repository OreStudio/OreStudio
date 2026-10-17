/* Create a report. Self-contained: plain JavaScript, mock data, no framework,
 * no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_create_a_report.org.
 *
 * The shape is decided: this screen is the standard steps UI, the one the
 * system bootstrap journey uses. The rail, its done/current/ahead marks and the
 * step foot are copied from doc/prototypes/setup/first-run/, which copies
 * JourneyPage.tsx.
 *
 * The rail is computed from the draft, not written out. Seven fixed steps, one
 * step per analytic that is on, in display_order, and the final review:
 *
 *   1  What it is                  the name, the description, the report type,
 *                                  is_official, and the concurrency policy
 *   2  The conventions it binds    one step per configuration type, because
 *   3  The pricing engines it binds  binding a configuration means something
 *   4  The market data it binds      different for each of the five
 *   5  The curve configuration it binds
 *   6  The run settings it binds
 *   7  What it runs                the fourteen analytic types, one row each:
 *                                  the switch, the order and a one-line summary.
 *                                  The index of the analytic steps that follow.
 *   then one step per analytic that is on, in display_order, each titled with
 *        the analytic's name and carrying what it is, how it is calculated and
 *        every parameter with a control chosen for its shape.
 *   final  Review and save        every field value, the bindings, the analytics
 *                                  with their settings, then report_definitions.put
 *                                  and the read-back. It is final, so it draws no
 *                                  Back.
 *
 * Turning an analytic on is how one is added: it writes the row with its default
 * settings and its step appears in the rail, renumbering the steps after it.
 * Turning one off asks first, sets active false, and removes the step. The rail,
 * the step numbering and the foot bar are one list, so they agree at every
 * moment.
 *
 * The five configuration steps carry the same four parts: what binding a
 * configuration of that type means, where its data lives, the picker for the
 * configuration this report binds, and what the binding reaches and does not.
 *
 * Deep links: ?step=N is numeric over the current rail, and ?analytic=<code>
 * resolves to that analytic's step whatever its number. Navigation rewrites both,
 * so a reload lands on the same step. ?pfe=without-simulation opens the draft
 * with the rule broken.
 *
 * Three absences the copy never hides:
 *
 *  - the portfolio scope is a column of the bound risk report config and no
 *    operation in this journey's set writes it, so the step reads the scope
 *    off the configuration rather than offering a choice;
 *  - pfe reads a cube simulation writes, and the rule that pairs them lives in
 *    the ACME seeder (generate_risk_config_data.py,
 *    check_exposure_needs_simulation) and not in the reporting service, so the
 *    refusal on this page is the screen's own rule;
 *  - no screen exists for four of the five configuration types. Only
 *    conventions has a route, PATHS.conventions = /refdata/conventions.
 *
 * Nothing here sends a message. The operations named are the journey's own. */

(function () {
    'use strict';

    // ------------------------------------------------------------- fixture

    var NOW = new Date(2026, 9, 20, 14, 32, 0);

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };
    var TENANT = 'ACME Group (the session\u2019s tenant)';
    var PERSON = { name: 'Anneke Vos', role: 'Tenant administrator' };

    /* The whole configuration vocabulary: five types, and no more, in the order
       report_type_configuration_types states and the seeder writes. Each type
       owns one step of the rail, and every one of those steps carries the same
       parts: `means` says what binding a configuration of the type decides,
       `data` says where the data lives, `where` is the screen that holds it,
       and `reaches` and `doesNot` say what the binding does and does not touch.
       `#bound` is the count of definitions that already bind the configuration
       this report picks, so the step states the reach rather than implying it.
       Only conventions has a screen. */
    var CONFIG_TYPES = [
        {
            code: 'conventions',
            title: 'The conventions it binds',
            sub: 'one document per convention kind',
            lead: 'A conventions configuration names the market terms every trade in the run ' +
                'is read with. Choose the set this report reads.',
            means: [
                'A convention is the set of market terms an instrument is built on: the day ' +
                'count, the calendars, the payment frequency, the index, and the other terms ' +
                'its instrument family fixes before a trade exists.',
                'A conventions configuration holds that set as one document per convention kind. ' +
                'Binding it decides what every trade in the run means. The same trade read ' +
                'under two convention sets gives two different numbers, and neither of them is ' +
                'a mistake.'
            ],
            data: [
                'projects/ores.refdata/modeling/ores.refdata.<instrument>_convention.org \u2014 ' +
                'one document per instrument family, 25 of them, from ' +
                'ores.refdata.deposit_convention.org to ' +
                'ores.refdata.cross_currency_basis_convention.org.',
                'ores.refdata.bond_yield_convention.org, ores.refdata.cds_convention.org and ' +
                'ores.refdata.average_ois_convention.org are three of the 25. A conventions ' +
                'configuration names one such document for each convention kind.',
                'doc/knowledge/journeys/conventions/ states what a convention is, and which ' +
                'reference data lists it picks from.'
            ],
            where: { symbol: 'PATHS.conventions', path: '/refdata/conventions', name: 'Conventions' },
            reaches: [
                'Every instrument family the set covers, on every run of every definition ' +
                'binding this configuration.',
                'The day count and the calendars every analytic below reads through it, so npv ' +
                'and everything built on npv move with the choice.',
                '#bound definitions already bind this configuration, so a change to it reaches ' +
                'their next run too.'
            ],
            doesNot: [
                'It creates no convention and edits none. The reference data lists a convention ' +
                'picks from, such as calendars, tenors and classifications, are untouched.',
                'It names no market data. The market data and the curves are the two steps after ' +
                'this one.',
                'It does not make a trade readable. A trade whose family the set does not cover ' +
                'has no terms in this run.'
            ]
        },
        {
            code: 'pricing_engines',
            title: 'The pricing engines it binds',
            sub: 'one model and one engine per product',
            lead: 'A pricing engine configuration names the model and the engine each product is ' +
                'priced with. Choose the one this report prices through.',
            means: [
                'A pricing engine configuration is ORE\u2019s pricingengine.xml. For each product ' +
                'it names the pricing model, the numerical engine that evaluates the model, and ' +
                'the parameters the two take.',
                'Binding it decides how a trade is priced, not what it is priced from. A European ' +
                'swaption mapped to BlackBachelier and a Grid engine carries a different number ' +
                'from the same swaption mapped to LGM and AMC, and the two are not expected to ' +
                'agree.',
                'A product the configuration carries no row for has no model in this run.'
            ],
            data: [
                'projects/ores.analytics/modeling/ores.analytics.pricing_model_config.org \u2014 ' +
                'the document: one row per named configuration, which is ORE\u2019s ' +
                'pricingengine.xml.',
                'projects/ores.analytics/modeling/ores.analytics.pricing_model_product.org \u2014 ' +
                'one row per pricing engine type, naming the model (DiscountedCashflows, LGM, ' +
                'BlackBachelier, CrossAssetModel) and the engine (DiscountingSwapEngine, Grid, ' +
                'AMC).',
                'projects/ores.analytics/modeling/' +
                'ores.analytics.pricing_model_product_parameter.org \u2014 the model, engine and ' +
                'global parameters as name and value rows, each in a scope.'
            ],
            reaches: [
                'Every analytic that prices a trade: npv first, then sensitivity, VaR and every ' +
                'exposure figure built on them.',
                'The model parameters of every product in the document, which no screen anywhere ' +
                'in the application shows today.',
                '#bound definitions already bind this configuration, so their next run prices ' +
                'the same way.'
            ],
            doesNot: [
                'It does not choose the market data or the curves. A model fits to the curves the ' +
                'run builds, and those come from the two steps after this one.',
                'It does not make a product priceable. A product with no row here is priced by no ' +
                'model, and the run fails when it reaches it.'
            ]
        },
        {
            code: 'todays_market',
            title: 'The market data it binds',
            sub: 'collections of quotes, by market role',
            lead: 'A today\u2019s market configuration names the data a run prices against, and a ' +
                'block of collections for each market role. Choose the one this report reads.',
            means: [
                'A today\u2019s market configuration is ORE\u2019s todaysmarket.xml. It holds the ' +
                'data a run prices against as up to twenty-four collections, such as ' +
                'DiscountingCurves, FxSpots and SwaptionVolatilities. Each collection holds its ' +
                'own ordered entries.',
                'It also holds named configuration blocks. A block selects one collection of each ' +
                'kind by id, so a report names a coherent set of curves and volatilities instead ' +
                'of restating them.',
                'Binding it decides which block each market role reads. The role is ORE\u2019s own, ' +
                'pricing and simulation among them. Reporting holds the binding, not the data it ' +
                'names.'
            ],
            data: [
                'projects/ores.analytics/modeling/ores.analytics.todays_market_collection.org ' +
                '\u2014 one collection, with its identity, its kind and its entries. A document ' +
                'may write the same kind twice under different ids, which is how one block ' +
                'selects a curve set and another selects a second.',
                'projects/ores.analytics/modeling/ores.analytics.todays_market_configuration.org ' +
                'and ores.analytics.todays_market_configuration_binding.org \u2014 one ' +
                'configuration block, and the one collection of each kind it selects.',
                'projects/ores.reporting/modeling/ores.reporting.configuration_market_binding.org ' +
                '\u2014 the market role a run binds, written with ' +
                'reporting.v1.configuration_market_bindings.put.'
            ],
            reaches: [
                'Every price the run produces, because every analytic reads the market through it.',
                'The curves as they are built: the quotes a curve recipe names come from here, so ' +
                'this step and the curve step together decide the curve.',
                '#bound definitions already bind this configuration, so their next run reads the ' +
                'same market.'
            ],
            doesNot: [
                'It holds references by name, not prices. The recipe that turns a quote into a ' +
                'curve is the curve configuration, which is the next step.',
                'It does not name the data of a role nothing binds. A role with no binding reads ' +
                'no market data, and the run fails where it needs one.',
                'It does not fix a valuation date. The date the run reads is a setting of the last ' +
                'configuration step.'
            ]
        },
        {
            code: 'curve_configuration',
            title: 'The curve configuration it binds',
            sub: 'the recipes the run builds its curves from',
            lead: 'A curve configuration names the recipes a run builds its curves from, with ' +
                'their bootstrapping, segments and quotes. Choose the one this report builds.',
            means: [
                'A curve configuration is ORE\u2019s curveconfig.xml: the recipes a run builds its ' +
                'curves from.',
                'A recipe names a curve, its definition, the segments it is built from, the quotes ' +
                'that feed each segment, and how it is bootstrapped. The document also writes the ' +
                'volatility and correlation sections that the option analytics read.',
                'Binding it decides which recipes this run has. Without the recipe the curve is not ' +
                'built, whatever the market quotes.'
            ],
            data: [
                'projects/ores.refdata/modeling/ores.refdata.curve_configuration.org \u2014 the ' +
                'document: its identity and the sections it writes. The curves themselves are ' +
                'rows of curve_definition, and the segments and quotes hang off those.',
                'projects/ores.refdata/modeling/ores.refdata.curve_configuration_section.org ' +
                '\u2014 one row per section element the document writes, empty or not. ' +
                'BaseCorrelations and Correlations appear empty fifteen times each in the corpus, ' +
                'and the export writes them back.',
                'A curve configuration is registered as a reporting configuration, which is how a ' +
                'definition binds it. The document belongs to refdata; the name belongs to ' +
                'reporting.'
            ],
            reaches: [
                'Every analytic that discounts, projects or reads a volatility: npv, cashflow, ' +
                'curves, simulation, and the exposure analytics above them.',
                'What the curves analytic writes, which is the one place where a curve built the ' +
                'wrong way can be seen.',
                '#bound definitions already bind this configuration, so their next run builds the ' +
                'same curves.'
            ],
            doesNot: [
                'It holds no quote. The quotes a segment names are entries of the today\u2019s ' +
                'market configuration.',
                'It builds nothing. The run builds the curves from the recipe, and this page ' +
                'creates no run.',
                'It does not choose the model that reads a volatility. That is the pricing engine ' +
                'configuration.'
            ]
        },
        {
            code: 'risk_report_config',
            title: 'The run settings it binds',
            sub: 'the settings, the analytics and the scope',
            lead: 'A risk report configuration carries the run settings the party shares, the ' +
                'analytics the run produces and the portfolio scope it reads. Choose the one this ' +
                'report shares.',
            means: [
                'A risk report configuration holds the ORE-level run settings a party shares: the ' +
                'base currency, the observation model, the number of threads, the market data ' +
                'type and date, and the ORE flags such as continue_on_error and ' +
                'imply_todays_fixings. It also carries the VaR and SIMM settings.',
                'Binding it decides how the run behaves rather than what it reads: the currency ' +
                'the figures are stated in, how many threads price them, and what happens to a ' +
                'trade that fails to price.',
                'It also carries the analytics this report runs, and the portfolio and book scope. ' +
                'The scope lives here and not on the definition, which is why the definition ' +
                'chooses no portfolio.'
            ],
            data: [
                'projects/ores.reporting/modeling/ores.reporting.risk_report_config.org \u2014 the ' +
                'settings above, one row per named configuration of a party. Its name is also the ' +
                'name of the reporting configuration a definition binds.',
                'projects/ores.reporting/modeling/ores.reporting.configuration.org \u2014 the ' +
                'named, reusable configuration a report binds. The kind of configuration it is ' +
                'comes from configuration_type by foreign key, not from a text code.',
                'The scope is two junction tables keyed by the settings row: ' +
                'ores_reporting_risk_report_config_portfolios_tbl and ' +
                'ores_reporting_risk_report_config_books_tbl.'
            ],
            reaches: [
                'The behaviour of every run of every definition bound to it: #bound definitions ' +
                'today.',
                'The analytics the run produces, which the next step changes, and the portfolio ' +
                'and book scope the run reads.',
                'Every figure in the report, because the base currency the settings name is the ' +
                'currency a reader sees.'
            ],
            doesNot: [
                'It carries no market data, no curve and no model. Those are the four steps before ' +
                'this one.',
                'No operation in this journey\u2019s set writes these settings or the scope. Both ' +
                'come from the seeder, and this step reads them rather than offering a choice.',
                'It does not give this report a scope of its own. Two definitions that bind one ' +
                'configuration read one scope, and the definition holds no portfolio field at all.'
            ]
        }
    ];

    /* The report types the type table holds. A type is a category row, and the
       row also names the workflow that runs the report. One is registered
       today, and more are expected: the control is a combo box over the table
       rather than a fixed pair of choices, so a type added as a row appears
       without a code change. */
    var REPORT_TYPES = [
        { code: 'risk', what: 'the full ORE risk analytics run', workflow: 'report_execution_workflow' }
    ];

    /* The fourteen analytic types, in the order the page draws them, with the
       one-line summary the index shows and the parameter rows the bound
       configuration carries for the type.

       What each analytic is, how it is calculated, and a control for every
       parameter it takes are owned by
       doc/prototypes/reporting/manage-the-analytics/, which step 7 links to.
       This page reads the set and states it; it does not draw the set a second
       time, and nothing here edits an analytic or a parameter. */
    var ANALYTICS = [
        { code: 'npv', name: 'Net present value',
          produces: 'One present value per trade, netted to book and to portfolio.',
          params: [
              { name: 'discount_curve', value: 'ACME discount curve' },
              { name: 'include_cashflows', value: 'false' }
          ] },
        { code: 'sensitivity', name: 'Sensitivity',
          produces: 'The change in value for a small shift in each market factor, such as DV01 per curve.',
          params: [
              { name: 'shift_size', value: '0.0001' },
              { name: 'shift_type', value: 'Absolute' }
          ] },
        { code: 'cashflow', name: 'Cashflows',
          produces: 'The projected cashflow rows of every trade, bucketed by date.',
          params: [
              { name: 'bucket', value: '1D' },
              { name: 'include_fixings', value: 'true' }
          ] },
        { code: 'curves', name: 'Curves',
          produces: 'The built curves themselves, with their pillars and their discount factors.',
          params: [] },
        { code: 'simulation', name: 'Simulation',
          produces: 'Simulated future paths of the market, and the exposure cube pfe reads.',
          params: [
              { name: 'number_of_paths', value: '1000' },
              { name: 'seed', value: '42' },
              { name: 'time_grid', value: '1D' }
          ] },
        { code: 'saccr', name: 'SA-CCR',
          produces: 'The standardised counterparty credit exposure of each netting set.',
          params: [
              { name: 'reporting_currency', value: 'USD' },
              { name: 'include_collateral', value: 'true' }
          ] },
        { code: 'parametricVar', name: 'Parametric VaR',
          produces: 'One value at risk figure, computed from the sensitivities and a covariance matrix.',
          params: [
              { name: 'confidence', value: '0.99' },
              { name: 'horizon_days', value: '1' },
              { name: 'volatility_period_days', value: '250' }
          ] },
        { code: 'portfolioDetails', name: 'Portfolio details',
          produces: 'How many trades and how much notional each book and portfolio carries.',
          params: [
              { name: 'grouping', value: 'Book' },
              { name: 'include_empty_books', value: 'false' }
          ] },
        { code: 'creditcurves', name: 'Credit curves',
          produces: 'The survival probability and hazard rate curve of each counterparty.',
          params: [
              { name: 'hazard_rate_source', value: 'Spread' },
              { name: 'recovery_rate', value: '0.4' }
          ] },
        { code: 'pfe', name: 'Potential future exposure',
          produces: 'An exposure profile per counterparty and date, and one figure at the horizon.',
          params: [
              { name: 'horizon', value: '10' },
              { name: 'quantile', value: '0.95' },
              { name: 'exposure_cube', value: 'simulation-cube-01' }
          ] },
        { code: 'calibration', name: 'Calibration',
          produces: 'The fitted model parameters, and how far the fit missed the market.',
          params: [
              { name: 'model', value: 'Hull-White' },
              { name: 'max_iterations', value: '100' },
              { name: 'tolerance', value: '0.000001' }
          ] },
        { code: 'pnlExplain', name: 'P&L explain',
          produces: 'Yesterday\u2019s profit and loss split by factor: rates, credit, FX, and the rest.',
          params: [
              { name: 'decomposition', value: 'Delta-gamma' },
              { name: 'threshold', value: '0.01' }
          ] },
        { code: 'historicalSimulationVar', name: 'Historical simulation VaR',
          produces: 'One value at risk figure, read from how the book would have moved on past days.',
          params: [
              { name: 'confidence', value: '0.99' },
              { name: 'lookback_days', value: '250' },
              { name: 'holding_period_days', value: '1' },
              { name: 'use_log_returns', value: 'true' }
          ] },
        { code: 'xva', name: 'XVA',
          produces: 'The valuation adjustments: CVA, DVA and the funding cost, per counterparty.',
          params: [
              { name: 'base_currency', value: 'USD' },
              { name: 'methodology', value: 'Semi-analytic' },
              { name: 'include_cva', value: 'true' },
              { name: 'include_dva', value: 'false' },
              { name: 'netting_threshold', value: '1000000' }
          ] }
    ];

    /* Configurations the party holds, by type. A risk report config carries the
       analytic set and the scope, which is why the analytics belong to the
       configuration in the slot and not to the definition. */
    var CONFIGS = [
        { id: 'cfg-conv-group', type: 'conventions', name: 'Group conventions', detail: '25 convention kinds', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-pe-group', type: 'pricing_engines', name: 'Group pricing engines', detail: '42 products', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-tm-group', type: 'todays_market', name: 'Group today\u2019s market', detail: '3 collections, 118 entries', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-cc-group', type: 'curve_configuration', name: 'Group curve configuration', detail: '2 sections, 61 curves', scope: null, boundBy: 31, analytics: null },
        {
            id: 'cfg-rrc-exposure-gtp', type: 'risk_report_config',
            name: 'Exposure simulation - Group Treasury Portfolio',
            detail: 'base currency GBP, 4 threads', scope: 'Group Treasury Portfolio', boundBy: 6,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'simulation', active: true, order: 2 },
                { code: 'pfe', active: true, order: 3 }
            ]
        },
        {
            id: 'cfg-rrc-exposure-tb', type: 'risk_report_config',
            name: 'Exposure simulation - Trading Book',
            detail: 'base currency GBP, 4 threads', scope: 'Trading Book', boundBy: 3,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'simulation', active: true, order: 2 },
                { code: 'pfe', active: true, order: 3 },
                { code: 'saccr', active: false, order: 4 }
            ]
        },
        {
            id: 'cfg-rrc-intraday-gtp', type: 'risk_report_config',
            name: 'Intraday risk - Group Treasury Portfolio',
            detail: 'base currency GBP, 2 threads', scope: 'Group Treasury Portfolio', boundBy: 4,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'sensitivity', active: true, order: 2 },
                { code: 'parametricVar', active: true, order: 3 }
            ]
        }
    ];

    /* The names the party already uses. A definition's name is unique within its
       party, so the screen refuses one of these. */
    var TAKEN_NAMES = [
        'Exposure', 'CVA/DVA/FVA', 'Intraday Risk Monitor', 'Large Exposures',
        'Board Risk Dashboard', 'Headline Position', 'Bucketed DV01', 'Stressed VaR',
        'Leverage Ratio', 'Cashflows'
    ];

    var CONCURRENCY = [
        { code: 'skip', label: 'Skip: a firing is skipped while another run is in flight' },
        { code: 'queue', label: 'Queue: a firing waits until the run in flight ends' },
        { code: 'fail', label: 'Fail: a firing is recorded as failed, and does not run' }
    ];

    /* The seven fixed steps, in the order they always take. A step carries an
       id, a title for the rail, a lead sentence and a body; `final` marks the
       step the person may not walk back from. The five configuration steps are
       drawn from CONFIG_TYPES, so the rail, the configuration order and the
       seeder cannot disagree. */
    function fixedSteps() {
        return [
            {
                id: 'what-it-is',
                title: 'What it is',
                lead: 'Name the report, describe it in one sentence, choose the report type it ' +
                    'draws its requirements from, and say whether it is official and what happens ' +
                    'when two runs collide.'
            }
        ].concat(CONFIG_TYPES.map(function (type) {
            return {
                id: 'binds-' + type.code,
                title: type.title,
                lead: type.lead,
                configType: type.code
            };
        })).concat([
            {
                id: 'what-it-runs',
                title: 'What it runs',
                lead: 'The analytics of the run settings configuration you bound, not of this ' +
                    'definition. They are rows on that configuration, so every definition bound to ' +
                    'it runs them too, and a set that must differ belongs to a configuration of ' +
                    'its own. This step reads the set the configuration carries. Changing it is ' +
                    'Manage the analytics, which this step links to.'
            },
            {
                id: 'review',
                title: 'Review and save',
                lead: 'Every field value the save will write, the five bindings and the analytics ' +
                    'with their settings, then the write and the row read back.',
                final: true
            }
        ]);
    }

    /* The rail. The seven fixed steps and the review, and nothing else: this
       page no longer draws a step for each analytic that is on, because the
       analytics are managed on doc/prototypes/reporting/manage-the-analytics/
       and this page reads the set that screen leaves. Everything that
       navigates reads this one list, so the rail, the step numbering and the
       foot bar cannot disagree. */
    function rail() {
        return fixedSteps();
    }

    function railSummary() {
        var on = activeAnalytics().length;
        return fixedSteps().length + ' steps: 7 fixed, then the final review \u00b7 the ' + on +
            ' analytic' + (on === 1 ? '' : 's') + ' that are on are read on step 7 and managed on ' +
            'Manage the analytics';
    }

    var PFE_RULE = 'pfe reads a cube that simulation writes, so a report that enables pfe without ' +
        'simulation cannot work. The rule lives in the ACME seeder, in ' +
        'generate_risk_config_data.py at check_exposure_needs_simulation, and not in the reporting ' +
        'service: nothing validates it when a definition is written. This screen refuses it itself, ' +
        'and a build would have to carry the rule again.';

    var NO_DELETE = 'An analytic is turned off by setting active to false, not by deleting the ' +
        'row. The row keeps its display_order and its settings and reads as off, so a later reader ' +
        'can see the report used to produce it.';

    var NO_CONFIG_SCREEN = 'Only conventions has a screen. PATHS.conventions is ' +
        '/refdata/conventions. pricing_engines, todays_market, curve_configuration and ' +
        'risk_report_config have no screen in the application, and this page states that rather ' +
        'than drawing a link that would not resolve.';

    var DEFERRED_EDITORS = 'simulation and pfe each need a purpose-built editor: one for the cube, ' +
        'one for the shift and one for the time grid. Manage the analytics draws those settings as ' +
        'plain fields and says so on the parameter itself.';

    var WAIVER = 'report_type_configuration_types.list names the configuration types a report type ' +
        'cannot start without, and that is all. Nothing answers what a report needs in order to run ' +
        'beyond that: not the analytics a run must enable, not the inputs each analytic reads.';

    var S = {
        step: 'what-it-is',
        draft: null,
        saved: null,
        activity: 'new, nothing written',
        showState: false,
        log: []
    };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function configById(id) {
        var found = CONFIGS.filter(function (entry) { return entry.id === id; });
        return found.length === 1 ? found[0] : null;
    }

    function configsOf(type) {
        return CONFIGS.filter(function (entry) { return entry.type === type; });
    }

    function analyticOf(code) {
        var found = ANALYTICS.filter(function (entry) { return entry.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function fixtureClock() {
        var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
        var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        function pad(n) { return (n < 10 ? '0' : '') + n; }
        return days[NOW.getDay()] + ' ' + NOW.getDate() + ' ' + months[NOW.getMonth()] + ' ' +
            NOW.getFullYear() + ' ' + pad(NOW.getHours()) + ':' + pad(NOW.getMinutes()) +
            ' installation clock';
    }

    // --------------------------------------------------------- the draft

    function paramsFor(code) {
        return analyticOf(code).params.map(function (param, index) {
            return { name: param.name, value: param.value, position: index + 1 };
        });
    }

    function copyAnalytics(config) {
        if (!config || !config.analytics) return [];
        return config.analytics.map(function (row) {
            return { code: row.code, active: row.active, order: row.order, params: paramsFor(row.code) };
        });
    }

    function newDraft() {
        var draft = {
            name: '',
            description: '',
            reportType: 'risk',
            official: true,
            concurrency: 'skip',
            binds: {},
            analytics: []
        };
        CONFIG_TYPES.forEach(function (type) {
            var candidates = configsOf(type.code);
            draft.binds[type.code] = candidates.length > 0 ? candidates[0].id : null;
        });
        draft.analytics = copyAnalytics(configById(draft.binds.risk_report_config));
        return draft;
    }

    function analyticsRow(code) {
        var found = S.draft.analytics.filter(function (row) { return row.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function activeAnalytics() {
        return S.draft.analytics.filter(function (row) { return row.active; })
            .sort(function (a, b) { return a.order - b.order; });
    }

    function pfeWithoutSimulation() {
        var pfe = analyticsRow('pfe');
        var simulation = analyticsRow('simulation');
        return !!(pfe && pfe.active) && !(simulation && simulation.active);
    }

    function boundRiskConfig() {
        return configById(S.draft.binds.risk_report_config);
    }

    function boundScope() {
        var config = boundRiskConfig();
        return config && config.scope ? config.scope : null;
    }

    // ------------------------------------------------------------ checks

    /* What a save is refused on. This is the list the screen can check, and the
       review states it: it is not what a run needs. */
    function checks() {
        var nameTaken = TAKEN_NAMES.some(function (taken) {
            return taken.toLowerCase() === String(S.draft.name || '').trim().toLowerCase();
        });
        var list = [
            {
                id: 'name',
                what: 'It has a name the party does not already use',
                met: String(S.draft.name || '').trim() !== '' && !nameTaken,
                state: String(S.draft.name || '').trim() === ''
                    ? 'no name yet'
                    : (nameTaken ? 'the party already uses this name' : 'named')
            },
            {
                id: 'description',
                what: 'It says what it is for',
                met: String(S.draft.description || '').trim() !== '',
                state: String(S.draft.description || '').trim() === '' ? 'no description yet' : 'described'
            },
            {
                id: 'type',
                what: 'It is a report of a registered type',
                met: true,
                state: S.draft.reportType
            },
            {
                id: 'official',
                what: 'It states whether it is official',
                met: true,
                state: S.draft.official ? 'official' : 'not official, and may read a sandbox'
            },
            {
                id: 'concurrency',
                what: 'It says what happens when two runs collide',
                met: !!S.draft.concurrency,
                state: S.draft.concurrency || 'not chosen'
            }
        ];

        CONFIG_TYPES.forEach(function (type) {
            var bound = configById(S.draft.binds[type.code]);
            list.push({
                id: 'slot-' + type.code,
                what: 'It binds a ' + type.code + ' configuration',
                met: !!bound,
                state: bound ? bound.name : 'nothing bound'
            });
        });

        var active = activeAnalytics();
        list.push({
            id: 'analytics',
            what: 'It runs at least one analytic',
            met: active.length > 0,
            state: active.length === 0 ? 'no analytic is on' : active.length + ' on: ' +
                active.map(function (row) { return row.code; }).join(', ')
        });
        list.push({
            id: 'pfe',
            what: 'pfe is not enabled without simulation',
            met: !pfeWithoutSimulation(),
            state: pfeWithoutSimulation() ? 'pfe is on and simulation is off, so a save is refused'
                : (analyticsRow('pfe') && analyticsRow('pfe').active
                    ? 'pfe and simulation are both on'
                    : 'pfe is off, so the rule does not apply')
        });

        var emptySettings = active.filter(function (row) {
            return row.params.some(function (param) { return String(param.value).trim() === ''; });
        });
        list.push({
            id: 'settings',
            what: 'The analytics that carry settings carry them',
            met: emptySettings.length === 0,
            state: emptySettings.length === 0 ? 'every setting has a value'
                : 'a value is empty for ' + emptySettings.map(function (row) { return row.code; }).join(', ')
        });

        list.push({
            id: 'scope',
            what: 'The bound configuration carries a portfolio scope',
            met: !!boundScope(),
            state: boundScope() ? boundScope() : 'the bound configuration is scoped to no portfolio'
        });

        return list;
    }

    function openItems() {
        return checks().filter(function (item) { return !item.met; });
    }

    function ready() {
        return openItems().length === 0;
    }

    // ------------------------------------------------------------- write plan

    /* What a save writes, in the words of the operations. Every field of the
       definition is named, including the ones that stay empty. */
    function definitionFields() {
        return [
            ['id', 'a new identifier, assigned by the service', false],
            ['tenant_id', TENANT, false],
            ['name', S.draft.name || '', !S.draft.name],
            ['party_id', PARTY.code + ' (from the session)', false],
            ['description', S.draft.description || '', !S.draft.description],
            ['report_type', S.draft.reportType, false],
            ['fsm_state_id', 'empty until first activation', true],
            ['schedule_expression', 'empty: the definition is on demand', true],
            ['concurrency_policy', S.draft.concurrency, false],
            ['scheduler_job_id', 'empty: no schedule', true],
            ['run_grant_id', 'empty: created at admission, by scheduling or by a person asking', true],
            ['pre_processing', 'execute', false],
            ['prepared_input_key', 'empty: set only when pre_processing is substitute', true],
            ['post_processing', 'execute', false],
            ['is_official', S.draft.official ? 'true' : 'false', false]
        ];
    }

    function bindingRows() {
        return CONFIG_TYPES.map(function (type) {
            var config = configById(S.draft.binds[type.code]);
            return [type.code, config ? config.name : '', !config];
        });
    }

    function analyticRows() {
        return S.draft.analytics.slice().sort(function (a, b) { return a.order - b.order; })
            .map(function (row) {
                return [row.code, String(row.order), row.active ? 'true' : 'false'];
            });
    }

    function parameterRows() {
        var rows = [];
        activeAnalytics().forEach(function (row) {
            row.params.forEach(function (param) {
                rows.push([row.code + '.' + param.name, param.value, param.position]);
            });
        });
        return rows;
    }

    // --------------------------------------------------------- components

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
        return '<div class="pagehead">' +
            '<div>' +
            '<div class="crumbs">Reporting / Reports / <span class="mono">new definition</span></div>' +
            '<h1>Create a report</h1>' +
            '<p class="lede">The standard rail, eight steps: what it is, one step for each of the ' +
            'five configurations it binds, the analytics the bound configuration carries, and a ' +
            'final review that states every value before it is written. The analytics are read ' +
            'here and managed on Manage the analytics, which the analytics step links to.</p>' +
            '</div>' +
            '<div class="actions">' +
            '<span class="tag warn">fixture</span>' +
            '<span class="updated">' + esc(fixtureClock()) + '</span>' +
            '</div></div>';
    }

    function fixtureNote() {
        return '<div class="notice warn">This page is a fixture. Nothing reads a server and nothing ' +
            'is written. The party, the configurations, the analytics and their settings are mock ' +
            'data drawn from the ACME seed. <b>No BFF route serves reporting today</b>, and nothing ' +
            'in the application creates a report.</div>';
    }

    function stepHeaderHtml() {
        return '<div class="stepheader"><div class="metaline">' +
            '<span class="nm">' + esc(S.draft.name || 'A new report definition') + '</span>' +
            '<span class="sub">party ' + esc(PARTY.code) + ' \u00b7 ' +
            esc(S.saved ? 'written, ' + S.saved.id : 'nothing written') + '</span>' +
            '</div></div>';
    }

    function nameField() {
        var takenBy = TAKEN_NAMES.some(function (taken) {
            return taken.toLowerCase() === String(S.draft.name || '').trim().toLowerCase();
        });
        return '<div class="field">' +
            '<label for="f-name">Name</label>' +
            '<input id="f-name" type="text" data-act="name" value="' + esc(S.draft.name) + '"' +
            (takenBy ? ' class="bad"' : '') + ' placeholder="Intraday Exposure">' +
            '<span class="note">' + (takenBy
                ? 'The party already uses this name.'
                : 'Unique within ' + esc(PARTY.code) + '.') + '</span>' +
            '</div>';
    }

    function descriptionField() {
        return '<div class="field">' +
            '<label for="f-desc">Description</label>' +
            '<input id="f-desc" type="text" data-act="description" value="' + esc(S.draft.description) + '"' +
            ' placeholder="Exposure profiles for the trading book">' +
            '<span class="note">One sentence a reader sees beside the name.</span>' +
            '</div>';
    }

    function typeField() {
        var chosen = REPORT_TYPES.filter(function (one) {
            return one.code === S.draft.reportType;
        })[0];
        return '<div class="field">' +
            '<label for="f-type">Report type</label>' +
            '<select id="f-type" data-act="type" class="combo">' +
            REPORT_TYPES.map(function (one) {
                return '<option value="' + one.code + '"' +
                    (S.draft.reportType === one.code ? ' selected' : '') + '>' +
                    one.code + ' \u2014 ' + one.what + '</option>';
            }).join('') +
            '</select>' +
            '<span class="note">A combo box over <span class="mono">ores_reporting_report_types_tbl</span>, ' +
            'so a type added as a row appears here without a code change. One is registered today, ' +
            '<span class="mono">risk</span>, and its row names the workflow that runs the report: ' +
            '<span class="mono">' + esc(chosen ? chosen.workflow : '') + '</span>.</span>' +
            '</div>';
    }

    function concurrencyField() {
        return '<div class="field">' +
            '<label for="f-conc">Concurrency policy</label>' +
            '<select id="f-conc" data-act="concurrency">' +
            CONCURRENCY.map(function (entry) {
                return '<option value="' + entry.code + '"' +
                    (S.draft.concurrency === entry.code ? ' selected' : '') + '>' +
                    entry.code + ' \u2014 ' + esc(entry.label) + '</option>';
            }).join('') +
            '</select>' +
            '<span class="note">What a second firing does while a run is in flight.</span>' +
            '</div>';
    }

    function officialField() {
        return '<div class="field wide">' +
            '<label class="check"><input type="checkbox" data-act="official"' +
            (S.draft.official ? ' checked' : '') + '> Is official</label>' +
            '<span class="note">An official report never reads a sandbox: its scope cannot name a ' +
            'sandbox portfolio or a virtual book. The party is ' + esc(PARTY.code) +
            ', from the session, and is not a choice.</span>' +
            '</div>';
    }

    function stepOneBody() {
        return '<div class="formgrid">' + nameField() + descriptionField() + typeField() +
            concurrencyField() + officialField() + '</div>';
    }

    // ------------------------------------- steps two to six: the five slots

    /* The screen that holds a configuration type's data. Only conventions has
       one; the other four are stated as missing rather than drawn as a link
       that would not resolve. */
    function whereHtml(type) {
        if (!type.where) {
            return '<span class="noscr">No screen holds a <span class="mono">' +
                esc(type.code) + '</span> configuration. The application has no route that ' +
                'reads or edits one, so this page states that rather than drawing a link that ' +
                'would not resolve.</span>';
        }
        return '<a href="#" data-act="noop" class="route">' + esc(type.where.name) + ' \u00b7 ' +
            esc(type.where.symbol) + ' = ' + esc(type.where.path) + '</a>';
    }

    function slotIndex(type) {
        for (var i = 0; i < CONFIG_TYPES.length; i += 1) {
            if (CONFIG_TYPES[i].code === type.code) return i + 1;
        }
        return 0;
    }

    function slotHtml(type) {
        var bound = configById(S.draft.binds[type.code]);
        var candidates = configsOf(type.code);
        return '<div class="slot">' +
            '<div class="type"><code>' + esc(type.code) + '</code>' +
            '<span class="req">required by ' + esc(S.draft.reportType) + ' \u00b7 slot ' + slotIndex(type) +
            ' of 5 \u00b7 ' + esc(type.sub) + '</span></div>' +
            '<div class="where">' + whereHtml(type) + '</div>' +
            '<div class="pick">' +
            '<select data-act="bind" data-slot="' + esc(type.code) + '">' +
            '<option value=""' + (bound ? '' : ' selected') + '>not bound</option>' +
            candidates.map(function (config) {
                return '<option value="' + esc(config.id) + '"' +
                    (bound && bound.id === config.id ? ' selected' : '') + '>' +
                    esc(config.name) + '</option>';
            }).join('') +
            '</select>' +
            '<span class="shared">' + (bound
                ? esc(bound.detail) + ' \u00b7 bound by <b>' + bound.boundBy + ' definitions</b>'
                : 'nothing is bound in this slot, and a ' + esc(type.code) + ' configuration is required') +
            '</span>' +
            '</div></div>';
    }

    function scopeReadout() {
        var config = boundRiskConfig();
        return '<div class="card" style="margin-top:16px;background:var(--bg)">' +
            '<div class="sectionhead"><h3>The scope the report reads</h3>' +
            '<span class="meta"><span class="tag warn">no operation writes it</span></span></div>' +
            '<div class="statgrid">' +
            '<div><span class="lbl">portfolio</span><span class="val">' +
            esc(boundScope() || 'none: the bound configuration is scoped to no portfolio') + '</span></div>' +
            '<div><span class="lbl">carried by</span><span class="val">' +
            esc(config ? config.name : 'no configuration is bound') + '</span></div>' +
            '<div><span class="lbl">written by</span><span class="val">' +
            '<span class="tag warn">no operation</span></span></div>' +
            '</div>' +
            '<p class="rowfoot">The scope is not a field of the definition. It is a column of the ' +
            'risk report config in this slot, held in that row\u2019s portfolio and book junction ' +
            'tables, so choosing a configuration chooses the portfolio the report reads. ' +
            '<b>No operation in this journey\u2019s set writes the scope</b>, so the scope on this ' +
            'page is the seeder\u2019s.</p></div>';
    }

    function reachList(items, bound) {
        return '<ul>' + items.map(function (item) {
            return '<li>' + esc(item).replace(/#bound/g, String(bound ? bound.boundBy : 0)) +
                '</li>';
        }).join('') + '</ul>';
    }

    /* One configuration step. Every one of the five carries the same four parts,
       so a reader comparing two steps compares like with like. */
    function configStepBody(type) {
        var bound = configById(S.draft.binds[type.code]);
        return '<div class="doc"><div class="explain">' +
            '<h3>What binding a ' + esc(type.code) + ' configuration means</h3>' +
            type.means.map(function (paragraph) {
                return '<p>' + esc(paragraph) + '</p>';
            }).join('') +
            '<h3>Where the data lives</h3>' +
            type.data.map(function (paragraph) {
                return '<p class="mono">' + esc(paragraph) + '</p>';
            }).join('') +
            '</div></div>' +
            '<div class="sectionhead" style="margin-top:16px">' +
            '<h3>The configuration this report binds</h3>' +
            '<span class="meta">reporting.v1.configurations.list_by_configuration_type_code ' +
            '\u00b7 fixture</span></div>' +
            '<div class="slots">' + slotHtml(type) + '</div>' +
            (type.code === 'risk_report_config' ? scopeReadout() : '') +
            '<div class="reach" style="margin-top:18px">' +
            '<div class="does"><h3>What the binding reaches</h3>' + reachList(type.reaches, bound) +
            '</div>' +
            '<div class="not"><h3>What it does not</h3>' + reachList(type.doesNot, bound) +
            '</div></div>' +
            '<p class="rowfoot">The configuration types come from ' +
            '<span class="mono">reporting.v1.report_type_configuration_types.list</span> for ' +
            esc(S.draft.reportType) + ', the candidates in this slot from ' +
            '<span class="mono">reporting.v1.configurations.list_by_configuration_type_code</span>, ' +
            'and the binding is written with ' +
            '<span class="mono">reporting.v1.report_configurations.put</span>. ' +
            (type.where
                ? 'The data is reached through ' + esc(type.where.symbol) + ' today, which is ' +
                  esc(type.where.path) + '.'
                : 'The data has no screen today. This page states that rather than drawing a ' +
                  'link that would not resolve.') +
            (type.code === 'risk_report_config'
                ? ' The analytics this configuration carries are changed on the next step.'
                : '') +
            '</p>';
    }

    // ------------------------------------------ step seven: the analytics

    function ruleState() {
        if (pfeWithoutSimulation()) return 'violated';
        var pfe = analyticsRow('pfe');
        var simulation = analyticsRow('simulation');
        return pfe && pfe.active && simulation && simulation.active ? 'satisfied' : 'open';
    }

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
            ' The pair is marked here and again on Manage the analytics, which refuses the write ' +
            'itself.</div></div>';
    }

    /* One row of the summary: the analytic's name and code, its display_order,
       a one-line statement of what it produces, and the number of parameter rows
       the configuration carries for it. It carries no control. A row that is on
       the configuration and off reads as kept off, because an analytic is turned
       off by setting active false and never by deleting the row. */
    function summaryRowHtml(row) {
        var entry = analyticOf(row.code);
        var name = entry ? entry.name : row.code;
        var produces = entry ? entry.produces : '';
        var settings = entry ? entry.params.length : 0;
        var cls = 'anrow' + (row.active ? '' : ' isoff');
        if (row.active && ruleState() === 'violated' && (row.code === 'pfe' || row.code === 'simulation')) {
            cls += ' conflict';
        }
        return '<div class="' + cls + '"><div class="an-top">' +
            '<span class="swnone">' + (row.active ? 'on' : 'off') + '</span>' +
            '<span class="an-name">' + esc(name) +
            '<span class="codename">' + esc(row.code) + '</span></span>' +
            '<span class="an-order">order ' + row.order + '</span>' +
            '<span class="an-act">' + (row.active ? '<span class="tag ok">on</span>'
                : '<span class="tag muted">active = false</span>') + '</span>' +
            '<span class="an-summary">' + esc(produces) +
            ' <span class="an-count">\u00b7 ' + (settings === 0
                ? 'no parameter row'
                : settings + ' parameter row' + (settings === 1 ? '' : 's')) + '</span></span>' +
            '</div></div>';
    }

    /* Step seven is the entry point to the analytics, not a second
       implementation of them. It reads the set the bound run settings
       configuration carries: the count that is on, the list with the display
       order, the rows kept off, and the incompatibility state when the pair
       cannot run. Every control lives on the shared prototype it links to. */
    function analyticsSummary() {
        var config = boundRiskConfig();
        var rows = S.draft.analytics.slice().sort(function (a, b) { return a.order - b.order; });
        var on = activeAnalytics();
        return ruleStrip() +
            '<div class="sectionhead" style="margin-top:14px">' +
            '<h3>The analytics this configuration carries</h3>' +
            '<span class="meta"><span class="tag accent">read only</span>' +
            '<span class="mono">' + esc(config ? config.name : 'nothing bound') + '</span>' +
            '<span class="mono">reporting.v1.configuration_analytics.list_by_configuration_id</span>' +
            ' \u00b7 <b>' + on.length + ' on</b> of ' + rows.length + ' row' +
            (rows.length === 1 ? '' : 's') + ' \u00b7 fixture</span></div>' +
            '<p class="rowfoot" style="margin-top:0;margin-bottom:10px">' + on.length +
            ' analytic' + (on.length === 1 ? '' : 's') + ' on, in this display order: ' +
            (on.length === 0
                ? 'none, so a run of this report produces nothing.'
                : esc(on.map(function (row) {
                    return row.code + ' at ' + row.order;
                }).join(', ')) + '.') + '</p>' +
            (rows.length === 0
                ? '<p class="nosettings">The bound configuration carries no analytic row at all.</p>'
                : '<div class="anlist">' + rows.map(summaryRowHtml).join('') + '</div>') +
            '<p class="rowfoot">' + esc(NO_DELETE) + '</p>' +
            '<div class="notice" style="margin-top:12px"><b>The analytics belong to ' +
            esc(config ? config.name : 'the run settings configuration') + ', not to this ' +
            'definition.</b> ' + (config && config.boundBy > 0
                ? config.boundBy + ' definitions bind that configuration, so a change there ' +
                  'reaches their next run too, and none of them was opened.'
                : 'It is the run settings configuration this report binds.') +
            ' A set that must differ belongs to a configuration of its own. This step reads the ' +
            'set; it writes none of it.</div>' +
            '<div class="analyticslink">' +
            '<div><b>Manage the analytics</b>' +
            '<p class="hint">The switchboard of all fourteen types, a step for each analytic that ' +
            'is on, every parameter with a control chosen for its shape, and the read-back. It is ' +
            'the one screen that owns the analytic interaction, and the other prototype that used ' +
            'to repeat it links there too.</p></div>' +
            '<a class="btn primary" href="../manage-the-analytics/?from=create-a-report">' +
            'Open Manage the analytics</a></div>' +
            '<p class="rowfoot">' + esc(DEFERRED_EDITORS) + '</p>';
    }

    // --------------------------------------------------------- step four

    function reviewPanel() {
        function fieldTable(rows) {
            return '<table><tbody>' + rows.map(function (row) {
                return '<tr><td>' + esc(row[0]) + '</td><td class="val' + (row[2] ? ' empty' : '') + '">' +
                    esc(row[1] || '\u2014') + '</td></tr>';
            }).join('') + '</tbody></table>';
        }

        var definition = '<div class="writeblock">' +
            '<div class="op"><span class="what">The definition</span>' +
            '<span class="subject">reporting.v1.report_definitions.put</span></div>' +
            fieldTable(definitionFields()) + '</div>';

        var bindings = '<div class="writeblock">' +
            '<div class="op"><span class="what">One binding for each of the five configuration types</span>' +
            '<span class="subject">reporting.v1.report_configurations.put</span></div>' +
            '<table><thead><tr><th>configuration_type_code</th><th>configuration</th></tr></thead><tbody>' +
            bindingRows().map(function (row) {
                return '<tr><td class="code">' + esc(row[0]) + '</td><td class="val' + (row[2] ? ' empty' : '') + '">' +
                    esc(row[1] || 'nothing bound') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<p class="hint">The bindings hang off the definition. The analytics below hang off the ' +
            'risk report config, which other definitions bind too.</p></div>';

        var analytics = '<div class="writeblock">' +
            '<div class="op"><span class="what">Every analytic row the configuration carries, ' +
            'read here and written on Manage the analytics</span>' +
            '<span class="subject">' +
            'reporting.v1.configuration_analytics.list_by_configuration_id</span></div>' +
            (S.draft.analytics.length === 0
                ? '<p class="dim">No analytic row is on the configuration.</p>'
                : '<table><thead><tr><th>analytic_type_code</th><th>display_order</th><th>active</th>' +
                  '</tr></thead><tbody>' +
                  analyticRows().map(function (row) {
                      return '<tr><td class="code">' + esc(row[0]) + '</td><td class="code">' + esc(row[1]) +
                          '</td><td class="code">' + esc(row[2]) + '</td></tr>';
                  }).join('') + '</tbody></table>') +
            '<p class="hint">These rows belong to the bound configuration and none is deleted. The ' +
            'rows that are active, in this display_order, are what the next run of every definition ' +
            'bound to the configuration produces: ' + esc(activeAnalytics().length === 0
                ? 'none, so a run produces nothing.'
                : activeAnalytics().map(function (row) {
                    return row.code + ' at ' + row.order;
                }).join('; ') + '.') + ' This step reads them and writes none: Manage the ' +
            'analytics is the screen that changes them.</p></div>';

        var params = parameterRows();
        var settings = '<div class="writeblock">' +
            '<div class="op"><span class="what">The settings of the analytics that are on, read ' +
            'here and written on Manage the analytics</span>' +
            '<span class="subject">' +
            'reporting.v1.configuration_analytic_parameters.list_by_configuration_analytic_id' +
            '</span></div>' +
            (params.length === 0
                ? '<p class="dim">No analytic that is on carries a setting.</p>'
                : '<table><thead><tr><th>parameter_name</th><th>value</th><th>position</th></tr></thead><tbody>' +
                  params.map(function (row) {
                      return '<tr><td class="code">' + esc(row[0]) + '</td><td class="val">' + esc(row[1]) +
                          '</td><td class="code">' + esc(row[2]) + '</td></tr>';
                  }).join('') + '</tbody></table>') +
            '</div>';

        var scope = '<div class="writeblock">' +
            '<div class="op"><span class="what">The scope, which the configuration carries</span>' +
            '<span class="subject">no operation</span></div>' +
            '<table><tbody><tr><td>portfolio</td><td class="val' + (boundScope() ? '' : ' empty') + '">' +
            esc(boundScope() || 'none') + '</td></tr>' +
            '<tr><td>carried by</td><td class="val">' +
            esc(boundRiskConfig() ? boundRiskConfig().name : 'no configuration bound') +
            '</td></tr></tbody></table>' +
            '<p class="hint">No operation in this journey\u2019s set writes or reads the scope: it ' +
            'comes with the configuration, and the seeder builds that configuration.</p></div>';

        var open = openItems();
        var openBox = open.length === 0
            ? '<div class="notice ok"><b>Ready to save.</b> Everything the screen can check is met. ' +
              'That is not everything a run needs: ' + esc(WAIVER) + '</div>'
            : '<div class="notice err"><b>Not ready.</b> ' + open.length + ' item' +
              (open.length === 1 ? '' : 's') + ' still open: ' +
              open.map(function (item) { return esc(item.what); }).join('; ') + '. A save is refused ' +
              'while any is open, because the definition would read back unlike this review.</div>';

        return '<div class="review">' + openBox + definition + bindings + analytics + settings + scope +
            '<p class="hint">' + esc(WAIVER) + '</p></div>';
    }

    function readbackPanel() {
        if (!S.saved) return '';
        var saved = S.saved;
        return '<div class="card readback" style="margin-top:16px;background:var(--bg)">' +
            '<div class="sectionhead"><h3>Read back after the write</h3>' +
            '<span class="meta"><span class="tag ok">stored, fixture</span>' +
            '<span class="mono">reporting.v1.report_definitions.get</span></span></div>' +
            '<div class="banner"><b>' + esc(saved.name) + '</b> reads back as written. ' +
            '<span class="mono">' + esc(saved.id) + '</span> \u00b7 version 1</div>' +
            '<div class="statgrid" style="margin-top:14px">' +
            '<div><span class="lbl">report_type</span><span class="val mono">' + esc(saved.reportType) + '</span></div>' +
            '<div><span class="lbl">party_id</span><span class="val mono">' + esc(PARTY.code) + '</span></div>' +
            '<div><span class="lbl">schedule_expression</span><span class="val dim">empty, on demand</span></div>' +
            '<div><span class="lbl">scheduler_job_id</span><span class="val dim">empty</span></div>' +
            '<div><span class="lbl">run_grant_id</span><span class="val dim">empty until admission</span></div>' +
            '<div><span class="lbl">fsm_state_id</span><span class="val dim">empty until first activation</span></div>' +
            '<div><span class="lbl">is_official</span><span class="val mono">' + (saved.official ? 'true' : 'false') + '</span></div>' +
            '<div><span class="lbl">concurrency_policy</span><span class="val mono">' + esc(saved.concurrency) + '</span></div>' +
            '<div><span class="lbl">runs</span><span class="val"><span class="tag muted">never run</span></span></div>' +
            '</div>' +
            '<table class="kv" style="margin-top:14px"><tbody>' +
            saved.bindings.map(function (row) {
                return '<tr><td>bound ' + esc(row[0]) + '</td><td>' + esc(row[1]) + '</td></tr>';
            }).join('') +
            '<tr><td>portfolio scope</td><td>' + esc(saved.scope || 'none') +
            ', carried by the bound configuration</td></tr>' +
            '<tr><td>analytic rows</td><td>' + saved.analytics + '</td></tr>' +
            '<tr><td>parameter rows</td><td>' + saved.params + '</td></tr>' +
            '</tbody></table>' +
            '<p class="rowfoot">The definition is bitemporal: an edit writes a new version and no past ' +
            'run changes. It has never run, so the front door draws it as never run. ' +
            '<b>Nothing was sent.</b></p>' +
            '<div style="display:flex;gap:8px;margin-top:12px;flex-wrap:wrap">' +
            '<button class="btn" data-act="leave">Leave it on demand</button>' +
            '<button class="btn" data-act="schedule">Choose when it runs</button>' +
            '<button class="btn ghost" data-act="discard">Start another report</button>' +
            '</div></div>';
    }

    function stepFourBody() {
        return reviewPanel() + readbackPanel();
    }

    function reachCard() {
        return '<div class="card"><div class="sectionhead"><h2>What a save reaches, and what it does not</h2></div>' +
            '<div class="reach">' +
            '<div class="does"><h3>It writes</h3><ul>' +
            '<li>the definition, on demand, with no schedule expression and no scheduler job</li>' +
            '<li>one binding for each of the five configuration types</li>' +
            '</ul></div>' +
            '<div class="not"><h3>It does not</h3><ul>' +
            '<li>write an analytic row or a parameter value: those belong to the configuration and ' +
            'are managed on Manage the analytics</li>' +
            '<li>create a run, or produce anything</li>' +
            '<li>write a schedule, a scheduler job or a grant</li>' +
            '<li>write the portfolio scope, which the configuration carries</li>' +
            '<li>write the run settings inside a risk report config</li>' +
            '<li>change a run that has already happened</li>' +
            '</ul></div></div></div>';
    }

    function gapCard() {
        return '<div class="card gaps"><div class="sectionhead"><h2>What this page cannot show</h2></div>' +
            '<dl>' +
            '<dt>The analytic rule is the screen\u2019s own</dt>' +
            '<dd>' + esc(PFE_RULE) + ' The rule strip on What it runs and the marks on both rows ' +
            'are drawn here so a broken set is visible before the report is saved. The refusal of ' +
            'the write itself belongs to Manage the analytics, and a build would have to carry the ' +
            'rule in the screen.</dd>' +
            '<dt>No operation answers what a report needs</dt>' +
            '<dd>' + esc(WAIVER) + '</dd>' +
            '<dt>No operation writes the scope</dt>' +
            '<dd>The scope lives on the risk report config, in its portfolio and book junction ' +
            'tables, and no operation in this set touches them. Every configuration on this page, ' +
            'with its scope, is the seeder\u2019s, so step 6 reads the scope rather than offering a ' +
            'choice.</dd>' +
            '<dt>Only one configuration type has a screen</dt>' +
            '<dd>' + esc(NO_CONFIG_SCREEN) + '</dd>' +
            '<dt>No route, and nothing creates a report</dt>' +
            '<dd>No BFF route serves reporting, and nothing in the application creates a report ' +
            'today: every definition that exists was seeded.</dd>' +
            '<dt>No write permission is named</dt>' +
            '<dd>No permission code names the right to create a definition or to change a shared ' +
            'configuration\u2019s analytics. The tenant administrator holds *, and reaches both.</dd>' +
            '</dl></div>';
    }

    // ------------------------------------------------------------ the rail

    /* The rail, copied from doc/prototypes/setup/first-run/prototype.js: one
       entry per step, done / current / ahead, the mark a tick once a step is
       behind the person. The list is the computed rail, so an analytic that is
       turned on adds an entry and every entry after it renumbers. An analytic
       entry carries its code, and the analytic run and the final review open
       with a divider, so a long rail shows its three parts. */
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
                    (entry.final ? ' <span class="tag muted">final</span>' : '') +
                    '</span></li>';
            }).join('') + '</ol></nav></div>';
    }

    function nextOf(list, at) {
        if (at === list.length - 1) {
            return S.saved ? null : { label: 'Save the report', enabled: ready() };
        }
        return { label: 'Next: ' + list[at + 1].title.toLowerCase(), enabled: true };
    }

    /* Going back is refused at the first step, and at a step that is final. This
       journey's last step is the one that writes, so it carries final and draws
       no Back either: once the review has stated every value, there is no
       earlier state to return to. */
    function backAllowed(list, at) {
        return at > 0 && list[at - 1].final !== true && list[at].final !== true;
    }

    function footHtml(step, at) {
        var list = rail();
        var next = nextOf(list, at);
        var back = backAllowed(list, at);
        if (next === null && !back) return '';
        return '<div class="stepfoot">' +
            (back ? '<button type="button" class="btn ghost" data-act="back">Back</button>' : '') +
            (next !== null
                ? '<button type="button" class="btn primary ml-auto" data-act="next"' +
                  (next.enabled ? '' : ' disabled') + '>' + esc(next.label) + '</button>'
                : '') + '</div>';
    }

    function stepBody(step) {
        if (step.id === 'what-it-is') return stepOneBody();
        if (step.configType) {
            var type = CONFIG_TYPES.filter(function (one) {
                return one.code === step.configType;
            })[0];
            return configStepBody(type);
        }
        if (step.id === 'what-it-runs') return analyticsSummary();
        return stepFourBody();
    }

    /* Where the current step sits in the rail. A step id that is not in the rail
       cannot be current, so render moves the person to the analytics step rather
       than drawing a step the rail does not carry. */
    function at() {
        var list = rail();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === S.step) return i;
        }
        return -1;
    }

    function pos() {
        var index = at();
        return index < 0 ? 6 : index;
    }

    function goTo(index) {
        var list = rail();
        var target = Math.min(list.length - 1, Math.max(0, index));
        S.step = list[target].id;
        setActivity(list[target].final
            ? 'reviewing what will be written'
            : 'step ' + (target + 1) + ' of ' + list.length + ' \u00b7 ' + list[target].title);
        render();
    }

    // --------------------------------------------------------------- bars

    function renderNote() {
        var position = pos();
        var list = rail();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 fixture data, nothing reads a server, nothing is written \u00b7 ' +
            'step ' + (position + 1) + ' of ' + list.length + ', ' + list[position].title +
            ' \u00b7 ' + S.activity +
            ' \u00b7 ' + PERSON.name + ', ' + PERSON.role + ', party ' + PARTY.code;
    }

    function renderBar() {
        var position = pos();
        var list = rail();
        var states = list.map(function (step, index) {
            return '<button data-act="step" data-step="' + step.id + '"' +
                (position === index ? ' class="on"' : '') + '>' + (index + 1) + ' ' +
                esc(step.title) + '</button>';
        }).join('');
        var stateLine = '<span class="label">step <b>' + (position + 1) + ' of ' + list.length +
            '</b> \u00b7 ' + esc(list[position].title) + ' \u00b7 state: <b>' +
            esc(S.activity) + '</b></span>';
        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fixtureClock()) + '</b></span>' +
            '<span>report type: <b>' + esc(S.draft.reportType) + '</b></span>' +
            '<span>name: <b>' + esc(S.draft.name || 'none') + '</b></span>' +
            '<span>slots bound: <b>' + CONFIG_TYPES.filter(function (type) {
                return !!S.draft.binds[type.code];
            }).length + ' of 5</b></span>' +
            '<span>analytics on: <b>' + activeAnalytics().length + ' of 14</b></span>' +
            '<span>rail: <b>' + esc(railSummary()) + '</b></span>' +
            '<span>this step: <b>' + esc(S.step) + '</b></span>' +
            '<span>scope: <b>' + esc(boundScope() || 'none') + '</b></span>' +
            '<span>save open: <b>' + openItems().length + '</b></span>' +
            '<span>pfe without simulation: <b>' + (pfeWithoutSimulation() ? 'yes' : 'no') + '</b></span>' +
            '</div>' +
            '<p class="state-note">One party, because a definition belongs to one party. The ' +
            'configurations, their scopes and every analytic setting are a fixture. Nothing reads a ' +
            'server, no operation writes the scope, the pfe rule lives in the seeder, and only ' +
            'conventions has a screen among the five configuration types.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' +
            states +
            '<span class="sep">|</span>' + stateLine +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>' +
            '</div>' + statePanel;
    }

    /* The rail scrolls once it is longer than the column, so the current entry
       is brought inside it after every render. */
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
        if (at() < 0) S.step = 'what-it-runs';
        var list = rail();
        var position = pos();
        var step = list[position];
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() +
            '<main>' + pageHead() + fixtureNote() +
            '<div class="journey">' + railHtml(list, position) +
            '<section class="card">' +
            stepHeaderHtml() +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(step.lead) + '</p>' +
            stepBody(step) + footHtml(step, position) +
            '</section></div>' +
            reachCard() + gapCard() +
            '</main></div>';
        renderNote();
        renderBar();
        scrollRail();
        writeParams();
    }

    // ------------------------------------------------------------ actions

    function setActivity(text) {
        S.activity = text;
    }

    function logStep(text) {
        S.log.unshift(text);
        if (S.log.length > 12) S.log.pop();
    }

    function touch() {
        S.saved = null;
        if (pfeWithoutSimulation()) {
            setActivity('pfe on without simulation: a save is refused');
        } else {
            setActivity('editing ' + (S.draft.name || 'a new report'));
        }
    }

    function bindSlot(slot, id) {
        S.draft.binds[slot] = id || null;
        var config = configById(id);
        if (slot === 'risk_report_config') {
            S.draft.analytics = copyAnalytics(config);
            logStep('bound ' + (config ? config.name : 'nothing') + ' in risk_report_config \u2014 ' +
                'it carries the analytic set and the scope ' + (config && config.scope ? config.scope : 'none'));
        } else {
            logStep('bound ' + (config ? config.name : 'nothing') + ' in ' + slot);
        }
        touch();
        render();
    }

    function save() {
        var open = openItems();
        if (open.length > 0) {
            logStep('save refused: ' + open.map(function (item) { return item.id; }).join(', '));
            setActivity('save refused');
            render();
            return;
        }
        var config = boundRiskConfig();
        S.saved = {
            id: 'rpt-' + (0x1000000 + Math.floor(Math.random() * 0xeffffff)).toString(16),
            name: S.draft.name,
            reportType: S.draft.reportType,
            official: S.draft.official,
            concurrency: S.draft.concurrency,
            scope: boundScope(),
            bindings: bindingRows().map(function (row) { return [row[0], row[1]]; }),
            analytics: S.draft.analytics.length + ' rows on ' + (config ? config.name : 'no configuration'),
            params: parameterRows().length + ' settings'
        };
        logStep('would send reporting.v1.report_definitions.put for ' + S.draft.name +
            ' \u2014 nothing is sent');
        logStep('would send reporting.v1.report_configurations.put for five slots');
        logStep('would read the analytics with ' +
            'reporting.v1.configuration_analytics.list_by_configuration_id and write none of them');
        logStep('would read it back with reporting.v1.report_definitions.get');
        if (config && config.boundBy > 0) {
            logStep(config.boundBy + ' other definitions bind ' + config.name +
                ', and their next run reads the set this report was saved against');
        }
        setActivity('saved (nothing sent) \u00b7 never run');
        render();
    }

    function discard() {
        S.draft = newDraft();
        S.saved = null;
        S.step = 'what-it-is';
        logStep('started again from the fixture');
        setActivity('new, nothing written');
        render();
    }

    // --------------------------------------------------------------- boot

    /* Deep links. ?step=N is numeric over the eight steps, and it is stable
       because the rail does not move. ?pfe=without-simulation opens the draft
       with the rule broken. */
    function readParams() {
        var params = new URLSearchParams(window.location.search);
        if (params.get('pfe') === 'without-simulation') {
            S.draft.binds.risk_report_config = 'cfg-rrc-exposure-tb';
            S.draft.analytics = copyAnalytics(configById('cfg-rrc-exposure-tb'));
            var simulation = analyticsRow('simulation');
            if (simulation) simulation.active = false;
            setActivity('pfe on without simulation: a save is refused');
        }
        var wanted = parseInt(params.get('step'), 10);
        if (wanted >= 1 && wanted <= rail().length) {
            S.step = rail()[wanted - 1].id;
        }
    }

    function writeParams() {
        var params = new URLSearchParams();
        var list = rail();
        var position = pos();
        params.set('step', String(position + 1));
        if (S.seededBroken === true && pfeWithoutSimulation()) params.set('pfe', 'without-simulation');
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (element.tagName !== 'BUTTON') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        if (act === 'param') return;
        event.preventDefault();
        var code = element.getAttribute('data-code');

        if (act === 'step') {
            var wanted = element.getAttribute('data-step');
            var list = rail();
            for (var i = 0; i < list.length; i += 1) {
                if (list[i].id === wanted) {
                    goTo(i);
                    return;
                }
            }
        } else if (act === 'back') {
            goTo(pos() - 1);
        } else if (act === 'next') {
            if (rail()[pos()].final) {
                save();
            } else {
                goTo(pos() + 1);
            }
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        } else if (act === 'save') {
            save();
        } else if (act === 'discard') {
            discard();
        } else if (act === 'leave') {
            logStep('left ' + (S.saved ? S.saved.name : 'the definition') + ' on demand \u2014 the ' +
                'schedule journey is where a recurrence starts');
            setActivity('left on demand');
            render();
        } else if (act === 'schedule') {
            logStep('would hand ' + (S.saved ? S.saved.name : 'the definition') +
                ' to Choose when a report runs \u2014 nothing is sent');
            setActivity('handed to the schedule journey');
            render();
        }
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        var code = element.getAttribute('data-code');

        if (act === 'name') {
            S.draft.name = element.value;
            touch();
            render();
        } else if (act === 'description') {
            S.draft.description = element.value;
            touch();
            render();
        } else if (act === 'type') {
            S.draft.reportType = element.value;
            logStep('report type ' + element.value + ' \u2014 it decides what the report is');
            touch();
            render();
        } else if (act === 'official') {
            S.draft.official = element.checked;
            logStep('is_official ' + element.checked);
            touch();
            render();
        } else if (act === 'concurrency') {
            S.draft.concurrency = element.value;
            logStep('concurrency_policy ' + element.value);
            touch();
            render();
        } else if (act === 'bind') {
            bindSlot(element.getAttribute('data-slot'), element.value);
        }
    });

    S.draft = newDraft();
    S.seededBroken = new URLSearchParams(window.location.search).get('pfe') === 'without-simulation';
    readParams();
    render();
}());

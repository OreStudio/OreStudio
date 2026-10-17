/* Create a report. Self-contained: plain JavaScript, mock data, no framework,
 * no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_create_a_report.org.
 *
 * The shape is decided: this screen is eight steps on the standard steps UI, the
 * one the system bootstrap journey uses. The rail, its done/current/ahead marks
 * and the step foot are copied from doc/prototypes/setup/first-run/, which
 * copies JourneyPage.tsx. It is not restyled here.
 *
 *   1  What it is                  the name, the description, the report type,
 *                                  is_official, and the concurrency policy
 *   2  The conventions it binds    one step per configuration type, because
 *   3  The pricing engines it binds  binding a configuration means something
 *   4  The market data it binds      different for each of the five
 *   5  The curve configuration it binds
 *   6  The run settings it binds
 *   7  What it runs                the fourteen analytic types with active
 *                                  toggles, and the add-analytic flow the change
 *                                  journey carries
 *   8  Review and save             every field value, the bindings and the
 *                                  analytics, then report_definitions.put and
 *                                  the read-back. It is the final step, so it
 *                                  draws no Back.
 *
 * The five configuration steps carry the same four parts: what binding a
 * configuration of that type means, where its data lives, the picker for the
 * configuration this report binds, and what the binding reaches and does not.
 *
 * The three structural variants (?variant=A|B|C: one form, a wizard, a
 * checklist) were dropped when the product owner chose the standard steps UI.
 * The design note records that. ?step=1..8 still lands on a step, and
 * ?pfe=without-simulation still opens the draft with the rule broken.
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

    /* The eight steps, in the order the rail draws them. A step carries an id, a
       title for the rail, a lead sentence and a body; `final` marks the step the
       person may not walk back from. The five configuration steps are drawn
       from CONFIG_TYPES, so the rail, the configuration order and the seeder
       cannot disagree. */
    var STEPS = [
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
                'definition. They are rows on that configuration, so every definition bound to it ' +
                'runs them too, and a set that must differ belongs to a configuration of its own.'
        },
        {
            id: 'review',
            title: 'Review and save',
            lead: 'Every field value the save will write, the five bindings and the analytics ' +
                'with their settings, then the write and the row read back.',
            final: true
        }
    ]);

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
        'one for the shift and one for the time grid. This page draws those settings as plain ' +
        'fields and says so on the parameter itself.';

    var WAIVER = 'report_type_configuration_types.list names the configuration types a report type ' +
        'cannot start without, and that is all. Nothing answers what a report needs in order to run ' +
        'beyond that: not the analytics a run must enable, not the inputs each analytic reads.';

    var S = {
        step: 1,
        draft: null,
        saved: null,
        flow: null,
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

    function nextOrder() {
        return S.draft.analytics.reduce(function (acc, row) {
            return Math.max(acc, row.order);
        }, 0) + 1;
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
            '<p class="lede">Eight steps on the standard rail: what the report is, the five ' +
            'configurations it binds \u2014 one step each \u2014 what it runs, and a review that ' +
            'states every value before it is written.</p>' +
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

    // -------------------------------------------------------- step three

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
                'does not bind yet';
        }
        return '<div class="rule' + cls + '">' +
            '<div class="rtitle">' + title + '</div>' +
            '<div class="rbody"><b>pfe reads a cube that simulation writes.</b> ' + esc(PFE_RULE) +
            ' This is the authority of a person who creates a definition, and no operation in the ' +
            'journey\u2019s set can carry it.</div></div>';
    }

    function switchFor(code) {
        var row = analyticsRow(code);
        if (!row) return '<span class="swnone">no row</span>';
        var on = row.active;
        return '<button class="sw" role="switch" aria-checked="' + (on ? 'true' : 'false') + '" ' +
            'data-act="toggle" data-code="' + esc(code) + '" title="Writes active ' + (on ? 'false' : 'true') +
            '. The row is never deleted.">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (on ? 'on' : 'off') + '">' + (on ? 'on' : 'off') + '</span></button>';
    }

    function paramsSummary(row) {
        if (!row || row.params.length === 0) return '<span class="hint">no settings</span>';
        return '<div class="an-params">' + row.params.map(function (param) {
            return '<span class="p"><span class="pn">' + esc(param.name) + '</span>' +
                '<span class="pv">' + esc(param.value) + '</span></span>';
        }).join('') + '</div>';
    }

    function analyticRowHtml(code) {
        var entry = analyticOf(code);
        var row = analyticsRow(code);
        var conflict = ruleState() === 'violated' && (code === 'pfe' || code === 'simulation');
        var cls = 'anrow';
        if (!row) cls += ' norow';
        else if (!row.active) cls += ' isoff';
        if (conflict) cls += ' conflict';

        var badges = [];
        if (!row) {
            badges.push('<span class="tag muted">no row on this configuration</span>');
        } else if (!row.active) {
            badges.push('<span class="tag muted">off \u00b7 row kept at display_order ' + row.order + '</span>');
        }
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

    function switchboard() {
        var onCount = ANALYTICS.filter(function (entry) {
            var row = analyticsRow(entry.code);
            return !!(row && row.active);
        }).length;
        var rowCount = ANALYTICS.filter(function (entry) {
            return !!analyticsRow(entry.code);
        }).length;
        return ruleStrip() +
            '<div class="sectionhead" style="margin-top:14px"><h3>Every analytic type, one row each</h3>' +
            '<span class="meta">reporting.v1.analytic_types.list \u00b7 14 types \u00b7 ' +
            rowCount + ' rows \u00b7 ' + onCount + ' on \u00b7 fixture</span></div>' +
            '<div class="anlist">' + ANALYTICS.map(function (entry) {
                return analyticRowHtml(entry.code);
            }).join('') + '</div>' +
            '<div class="reviewbar">' +
            '<span class="count">The rows hang off the bound configuration, ' +
            esc(boundRiskConfig() ? boundRiskConfig().name : 'and none is bound') + ', which other ' +
            'definitions bind too. The switchboard keeps the on/off control and the order; the ' +
            'settings are set in the add-analytic flow, one analytic at a time.</span>' +
            '<button class="btn primary" data-act="flow-open">Add an analytic</button>' +
            '</div>' +
            '<p class="rowfoot">' + esc(NO_DELETE) + ' A type with no row has no switch: adding it ' +
            'is the <b>Add an analytic</b> flow, which sets its settings and then writes the row. ' +
            'Every switch shown writes an existing row.</p>';
    }

    // ------------------------------------------------ the add-analytic flow

    function flowCrumbs() {
        var screen = S.flow.screen;
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
        var row = analyticsRow(code);
        if (!row) return '<span class="tag muted">no row</span>';
        if (!row.active) return '<span class="tag muted">off</span>';
        return '<span class="tag ok">on</span>';
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
        return flowCrumbs() +
            '<div class="sectionhead"><h3>Add an analytic: the fourteen types</h3>' +
            '<span class="meta">reporting.v1.analytic_types.list \u00b7 fixture</span></div>' +
            '<p class="hint" style="margin-bottom:10px">Each type states what it produces and whether ' +
            'the bound configuration already carries it. "On the configuration" and "on" are ' +
            'different things: a row that is off still exists, and a type with no row was never ' +
            'asked for.</p>' +
            '<div class="picklist">' + rows + '</div>' +
            '<div class="stepnav"><button class="btn" data-act="flow-close">Cancel</button>' +
            '<span></span></div>';
    }

    function control(param) {
        var value = S.flow.values[param.name];
        var common = ' data-act="param" data-pact="param" data-name="' + esc(param.name) +
            '" aria-label="' + esc(param.name) + ' of ' + esc(S.flow.code) + '"';
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
            '<div class="pcrow">' + control(param) + '</div>' +
            '<p class="phelp">' + esc(param.help) + ' <span class="pnormal">A normal value is ' +
            esc(param.normal.replace(/^[A-Z]/, function (first) { return first.toLowerCase(); })) +
            '</span></p>' + deferred +
            '</div>';
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

    function deferredNotice() {
        var entry = analyticOf(S.flow.code);
        if (entry.paramEditor !== 'deferred') return '';
        return '<div class="notice warn" style="margin-top:10px"><b>' + esc(entry.code) +
            ' needs a purpose-built editor, and this page does not draw it.</b> ' + esc(entry.code) +
            ' carries ' + esc(entry.editorNeeds) + ', which a plain field cannot show well. ' +
            DEFERRED_EDITORS.charAt(0).toUpperCase() + esc(DEFERRED_EDITORS.slice(1)) +
            '</div>';
    }

    function flowDetail() {
        var entry = analyticOf(S.flow.code);
        return flowCrumbs() +
            '<div class="sectionhead"><h3>Add ' + esc(entry.name) +
            ': what it is, and every setting it takes</h3>' +
            '<span class="meta">reporting.v1.configuration_analytic_parameters.' +
            'list_by_configuration_analytic_id \u00b7 fixture</span></div>' +
            '<div class="doc">' + explanationBlock() + deferredNotice() + '</div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>The settings of ' +
            esc(S.flow.code) + '</h3><span class="hint">' + entry.params.length +
            ' setting(s) \u00b7 each control follows the shape of its value</span></div>' +
            '<div class="pcol">' + (entry.params.length === 0
                ? '<p class="dim" style="font-size:13px">This analytic takes no setting. It is ' +
                  'either on or off.</p>'
                : entry.params.map(paramCard).join('')) + '</div>' +
            '<div class="stepnav">' +
            '<button class="btn" data-act="flow-list">Back to the list</button>' +
            '<button class="btn primary" data-act="flow-effect">Continue to the effect</button>' +
            '</div>';
    }

    /* The rule as it stands for the analytic on the table. */
    function ruleBrokenHere() {
        var entry = analyticOf(S.flow.code);
        if (entry.code === 'pfe') {
            var simulation = analyticsRow('simulation');
            return !(simulation && simulation.active);
        }
        return false;
    }

    function effectBlock() {
        var entry = analyticOf(S.flow.code);
        var config = boundRiskConfig();
        var needs;
        if (entry.requires) {
            needs = '<div class="' + (ruleBrokenHere() ? 'notice err' : 'notice ok') + '">' +
                '<b>' + esc(entry.code) + ' needs ' + esc(entry.requires) + '.</b> ' +
                (ruleBrokenHere()
                    ? 'It is off in the set on the table, so this analytic cannot be turned on. '
                    : 'It is on in the set on the table, so the rule is satisfied. ') +
                esc(PFE_RULE) + '</div>';
        } else {
            needs = '<div class="notice"><b>No other analytic has to be on.</b> The one rule ' +
                'between analytics binds pfe to simulation and nothing else, so this analytic ' +
                'stands alone.</div>';
        }
        return '<div class="effect">' +
            '<h3>What turning ' + esc(entry.code) + ' on does</h3>' +
            '<p>' + esc(entry.effect) + '</p>' +
            '<ul class="effects">' +
            '<li>It writes one analytic row on ' + esc(config ? config.name : 'the bound configuration') +
            ': <span class="mono">' + esc(entry.code) + '.active = true</span>, and one parameter row ' +
            'per setting below.</li>' +
            '<li>' + (config && config.boundBy > 0
                ? config.boundBy + ' other definitions bind that configuration, so their next run ' +
                  'produces this analytic too, and none of them was opened here.'
                : 'No other definition binds that configuration today.') + '</li>' +
            '<li>The definition being created runs nothing until a run is asked for. Nothing here ' +
            'creates a run.</li>' +
            '</ul>' + needs + '</div>';
    }

    function flowWrites() {
        var entry = analyticOf(S.flow.code);
        var row = analyticsRow(entry.code);
        var list = [{
            entity: 'configuration_analytic',
            field: entry.code + '.active',
            was: row ? String(row.active) : 'no row',
            will: 'true, display_order ' + (row ? row.order : nextOrder())
        }];
        entry.params.forEach(function (param) {
            var was = null;
            (row ? row.params : []).forEach(function (old) {
                if (old.name === param.name) was = old.value;
            });
            var value = S.flow.values[param.name];
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

    function flowEffect() {
        var entry = analyticOf(S.flow.code);
        var refused = ruleBrokenHere();
        var actions;
        if (refused) {
            actions = '<div class="draftrow">' +
                '<button class="btn" data-act="fix-requires">Turn simulation on as well</button>' +
                '<button class="btn" data-act="flow-detail">Back to the settings</button>' +
                '<button class="btn ghost" data-act="flow-close">Cancel</button></div>';
        } else {
            actions = '<div class="draftrow">' +
                '<button class="btn" data-act="flow-detail">Back to the settings</button>' +
                '<button class="btn ghost" data-act="flow-close">Cancel</button>' +
                '<button class="btn primary" data-act="commit" data-code="' + esc(entry.code) + '">' +
                (analyticsRow(entry.code) ? 'Save these settings' : 'Turn ' + esc(entry.code) + ' on') +
                '</button></div>';
        }
        return flowCrumbs() +
            '<div class="sectionhead"><h3>The effect of adding ' + esc(entry.name) + '</h3>' +
            '<span class="meta">what it reaches, and what it cannot do \u00b7 fixture</span></div>' +
            effectBlock() +
            '<div class="sectionhead" style="margin-top:14px"><h3>Exactly what this write adds</h3>' +
            '<span class="meta">configuration_analytics.put_many \u00b7 ' +
            'configuration_analytic_parameters.put</span></div>' +
            flowWrites() +
            (refused
                ? '<div class="rule err" style="margin-top:12px">' +
                  '<div class="rtitle"><span class="tag bad">refused \u00b7 cannot be turned on</span>' +
                  'pfe needs simulation, and simulation is off in the set on the table</div>' +
                  '<div class="rbody">This refusal is the screen\u2019s own. Nothing on the server ' +
                  'would refuse the write: <span class="mono">reporting.v1.configuration_analytics.' +
                  'put_many</span> accepts pfe without simulation and the run fails later. The rule ' +
                  'lives in the ACME seeder, in ' +
                  '<span class="mono">check_exposure_needs_simulation</span>, and in no service. ' +
                  'A build would have to carry it in the screen, as this page does.</div></div>'
                : '') +
            actions;
    }

    function stepThreeBody() {
        if (S.flow) {
            if (S.flow.screen === 'list') return flowList();
            if (S.flow.screen === 'detail') return flowDetail();
            return flowEffect();
        }
        return switchboard();
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
            '<div class="op"><span class="what">Every analytic row the configuration carries</span>' +
            '<span class="subject">reporting.v1.configuration_analytics.put_many</span></div>' +
            (S.draft.analytics.length === 0
                ? '<p class="dim">No analytic row is on the configuration.</p>'
                : '<table><thead><tr><th>analytic_type_code</th><th>display_order</th><th>active</th>' +
                  '</tr></thead><tbody>' +
                  analyticRows().map(function (row) {
                      return '<tr><td class="code">' + esc(row[0]) + '</td><td class="code">' + esc(row[1]) +
                          '</td><td class="code">' + esc(row[2]) + '</td></tr>';
                  }).join('') + '</tbody></table>') +
            '<p class="hint">A row is written for every analytic the person turned on or off. ' +
            'Nothing is deleted.</p></div>';

        var params = parameterRows();
        var settings = '<div class="writeblock">' +
            '<div class="op"><span class="what">The settings of the analytics that are on</span>' +
            '<span class="subject">reporting.v1.configuration_analytic_parameters.put</span></div>' +
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
            '<li>the analytic rows of the bound risk report config</li>' +
            '<li>the settings of the analytics that are on</li>' +
            '</ul></div>' +
            '<div class="not"><h3>It does not</h3><ul>' +
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
            '<dd>' + esc(PFE_RULE) + ' The refusal on step 7 is drawn because the journey asks for ' +
            'it, and a build would have to carry the rule in the screen.</dd>' +
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

    /* The rail, verbatim from doc/prototypes/setup/first-run/prototype.js: one
       entry per step, done / current / ahead, the mark a tick once a step is
       behind the person. The title carries a "final" tag on the last step,
       because a final step is where the rail refuses to go back. */
    function railHtml(list, at) {
        return '<nav class="railnav" aria-label="Steps"><ol>' +
            list.map(function (s, i) {
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.title) + (s.final ? ' <span class="tag muted">final</span>' : '') + '</li>';
            }).join('') + '</ol></nav>';
    }

    function nextOf(step, at) {
        if (at === STEPS.length - 1) {
            return S.saved ? null : { label: 'Save the report', enabled: ready() };
        }
        return { label: 'Next: ' + STEPS[at + 1].title.toLowerCase(), enabled: true };
    }

    /* Going back is refused at the first step, and at a step that is final. This
       journey's last step is the one that writes, so it carries final and draws
       no Back either: once the review has stated every value, there is no
       earlier state to return to. */
    function backAllowed(at) {
        return at > 0 && STEPS[at - 1].final !== true && STEPS[at].final !== true;
    }

    function footHtml(step, at) {
        var next = nextOf(step, at);
        var back = backAllowed(at);
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
        if (step.id === 'what-it-runs') return stepThreeBody();
        return stepFourBody();
    }

    function at() {
        for (var i = 0; i < STEPS.length; i += 1) {
            if (STEPS[i].id === S.step) return i;
        }
        return 0;
    }

    // --------------------------------------------------------------- bars

    function renderNote() {
        var position = at();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 fixture data, nothing reads a server, nothing is written \u00b7 ' +
            'step ' + (position + 1) + ' of ' + STEPS.length + ', ' + STEPS[position].title +
            ' \u00b7 ' + S.activity +
            ' \u00b7 ' + PERSON.name + ', ' + PERSON.role + ', party ' + PARTY.code;
    }

    function renderBar() {
        var position = at();
        var states = STEPS.map(function (step, index) {
            return '<button data-act="step" data-step="' + step.id + '"' +
                (position === index ? ' class="on"' : '') + '>' + (index + 1) + ' ' +
                esc(step.title) + '</button>';
        }).join('');
        var stateLine = '<span class="label">step <b>' + (position + 1) + ' of ' + STEPS.length + '</b> \u00b7 ' +
            esc(STEPS[position].title) + ' \u00b7 state: <b>' + esc(S.activity) + '</b></span>';
        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fixtureClock()) + '</b></span>' +
            '<span>report type: <b>' + esc(S.draft.reportType) + '</b></span>' +
            '<span>name: <b>' + esc(S.draft.name || 'none') + '</b></span>' +
            '<span>slots bound: <b>' + CONFIG_TYPES.filter(function (type) {
                return !!S.draft.binds[type.code];
            }).length + ' of 5</b></span>' +
            '<span>analytics on: <b>' + activeAnalytics().length + ' of 14</b></span>' +
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

    function render() {
        var position = at();
        var step = STEPS[position];
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() +
            '<main>' + pageHead() + fixtureNote() +
            '<div class="journey">' + railHtml(STEPS, position) +
            '<section class="card">' +
            stepHeaderHtml() +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(step.lead) + '</p>' +
            stepBody(step) + footHtml(step, position) +
            '</section></div>' +
            reachCard() + gapCard() +
            '</main></div>';
        renderNote();
        renderBar();
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

    function toggleAnalytic(code) {
        var row = analyticsRow(code);
        if (!row) return;
        row.active = !row.active;
        logStep('set ' + code + ' active ' + row.active + ' \u2014 the row stays, nothing is deleted');
        touch();
        render();
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

    /* The flow's state: the screen it is on, the analytic it is about, and one
       value per setting, seeded from the row when the configuration already
       carries it and from the type's own default when it does not. */
    function flowState(screen, code) {
        var values = {};
        if (code) {
            var entry = analyticOf(code);
            var row = analyticsRow(code);
            entry.params.forEach(function (param) {
                var value = param.value;
                (row ? row.params : []).forEach(function (old) {
                    if (old.name === param.name) value = old.value;
                });
                values[param.name] = value;
            });
        }
        return { screen: screen, code: code || null, values: values };
    }

    function openFlow(screen, code) {
        S.flow = flowState(screen, code);
        setActivity('add analytic \u00b7 ' + screen + (code ? ' \u00b7 ' + code : ''));
        render();
    }

    function closeFlow() {
        S.flow = null;
        setActivity('editing ' + (S.draft.name || 'a new report'));
        render();
    }

    function commitAnalytic(code) {
        var entry = analyticOf(code);
        var row = analyticsRow(code);
        if (!row) {
            row = { code: code, active: true, order: nextOrder(), params: [] };
            S.draft.analytics.push(row);
        }
        row.active = true;
        row.params = entry.params.map(function (param, index) {
            return { name: param.name, value: S.flow.values[param.name], position: index + 1 };
        });
        logStep('wrote ' + code + '.active true at display_order ' + row.order +
            (entry.params.length > 0 ? ' with ' + entry.params.length + ' setting(s)' : ', no settings'));
        S.flow = null;
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
        logStep('would send reporting.v1.configuration_analytics.put_many for ' +
            S.draft.analytics.length + ' rows on ' + (config ? config.name : 'no configuration'));
        logStep('would send reporting.v1.configuration_analytic_parameters.put for ' +
            parameterRows().length + ' settings');
        logStep('would read it back with reporting.v1.report_definitions.get');
        if (config && config.boundBy > 0) {
            logStep(config.boundBy + ' other definitions bind ' + config.name +
                ', and run the analytic set this save wrote');
        }
        setActivity('saved (nothing sent) \u00b7 never run');
        render();
    }

    function discard() {
        S.draft = newDraft();
        S.saved = null;
        S.flow = null;
        S.step = 'what-it-is';
        logStep('started again from the fixture');
        setActivity('new, nothing written');
        render();
    }

    // --------------------------------------------------------------- boot

    function stepIndex(id) {
        for (var i = 0; i < STEPS.length; i += 1) {
            if (STEPS[i].id === id) return i;
        }
        return -1;
    }

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var wanted = parseInt(params.get('step'), 10);
        if (wanted >= 1 && wanted <= STEPS.length) S.step = STEPS[wanted - 1].id;
        if (params.get('pfe') === 'without-simulation') {
            S.draft.binds.risk_report_config = 'cfg-rrc-exposure-tb';
            S.draft.analytics = copyAnalytics(configById('cfg-rrc-exposure-tb'));
            var simulation = analyticsRow('simulation');
            if (simulation) simulation.active = false;
            setActivity('pfe on without simulation: a save is refused');
        }
        var screen = params.get('screen');
        var analytic = params.get('analytic');
        if (screen === 'list' || ((screen === 'detail' || screen === 'effect') && analyticOf(analytic))) {
            S.step = 'what-it-runs';
            S.flow = flowState(screen, analytic);
        }
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('step', String(stepIndex(S.step) + 1));
        if (pfeWithoutSimulation() && S.seededBroken === true) params.set('pfe', 'without-simulation');
        if (S.flow) {
            params.set('screen', S.flow.screen);
            if (S.flow.code) params.set('analytic', S.flow.code);
        }
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
        event.preventDefault();
        var code = element.getAttribute('data-code');

        if (act === 'step') {
            S.step = element.getAttribute('data-step');
            S.flow = null;
            setActivity(stepIndex(S.step) === STEPS.length - 1 ? 'reviewing what will be written'
                : 'step ' + (stepIndex(S.step) + 1) + ' of ' + STEPS.length);
            render();
        } else if (act === 'back') {
            S.step = STEPS[Math.max(0, at() - 1)].id;
            S.flow = null;
            setActivity('step ' + (at() + 1) + ' of ' + STEPS.length);
            render();
        } else if (act === 'next') {
            if (STEPS[at()].id === 'review') {
                save();
            } else {
                S.step = STEPS[Math.min(STEPS.length - 1, at() + 1)].id;
                S.flow = null;
                setActivity('step ' + (at() + 1) + ' of ' + STEPS.length);
                render();
            }
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        } else if (act === 'toggle') {
            toggleAnalytic(code);
        } else if (act === 'settings') {
            openFlow('detail', code);
        } else if (act === 'flow-open') {
            openFlow('list');
        } else if (act === 'flow-list') {
            openFlow('list');
        } else if (act === 'flow-close') {
            closeFlow();
        } else if (act === 'choose') {
            openFlow('detail', code);
        } else if (act === 'flow-detail') {
            openFlow('detail', S.flow.code);
        } else if (act === 'flow-effect') {
            S.flow.screen = 'effect';
            setActivity('add analytic \u00b7 effect \u00b7 ' + S.flow.code);
            render();
        } else if (act === 'fix-requires') {
            var simulation = analyticsRow('simulation');
            if (simulation) {
                simulation.active = true;
                logStep('turned simulation on \u2014 pfe now has the cube it reads');
            }
            S.flow = null;
            touch();
            render();
        } else if (act === 'commit') {
            commitAnalytic(code);
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
        } else if (act === 'param') {
            var name = element.getAttribute('data-name');
            if (element.getAttribute('data-toggle') === '1') {
                S.flow.values[name] = S.flow.values[name] === 'true' ? 'false' : 'true';
                render();
            } else {
                S.flow.values[name] = element.value;
            }
        }
    });

    /* A switch reads its new value on click, so the toggle control in the flow
       re-renders without going through change. */
    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-pact="param"][data-toggle="1"]');
        if (!element) return;
        event.preventDefault();
        var name = element.getAttribute('data-name');
        S.flow.values[name] = S.flow.values[name] === 'true' ? 'false' : 'true';
        render();
    });

    S.draft = newDraft();
    S.seededBroken = new URLSearchParams(window.location.search).get('pfe') === 'without-simulation';
    readParams();
    render();
}());

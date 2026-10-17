/* Read what a run produced prototype. Self-contained: plain JavaScript, mock
 * data, no framework, no build step, nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_read_what_a_run_produced.org.
 *
 * The decision this prototype serves: how the five reporting journeys become
 * one reporting surface without a screen per journey. For this journey the
 * question is what "the result" of a run is, and how much of it belongs on the
 * screen before a person goes looking for a file.
 *
 *   A, figures-first  -- the run's numbers are the screen.
 *   B, artefact-first -- the run produced files, and the screen is a manifest.
 *   C, narrative-first-- a sentence about the run, with the files at the end.
 *
 * Every fact about a run -- its output message, its output_storage_key, its
 * state, its errors -- is a field reporting already stores, but no screen
 * reads it today. Every figure and every file row is a fixture: nothing here
 * reads a server, no operation lists the files inside an archive, and no
 * operation returns the archive, the package, or a parsed result. The amber
 * note and the gaps panel say so on the page, because a prototype that draws
 * a file list must not imply the server served it.
 *
 * The figures are not free: a report's numbers are the outputs of the
 * analytics its configuration names, and this file says which ones on every
 * run. Headline Position configures npv and sensitivity, so its measures are
 * NPV, Delta and Gamma and its archive holds npv.csv and sensitivity.csv and
 * no saccr.csv. Board Risk Dashboard configures npv, parametricVar and saccr;
 * Large Exposures configures npv and saccr. The manifest follows the same
 * rule, so a reader can see why these figures and not others.
 *
 * Field shapes are the model's: output_message, output_storage_key,
 * reporting/runs/<instance-id>/output.tar.gz, report error scope codes
 * Analytics / Configuration / Report, and severities error / warning. The
 * engine messages are drawn from real log_structured.json lines.
 */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'A',
            name: 'A · figures',
            kind: 'figures-first',
            gist: 'The run\u2019s numbers are the screen: the figures the configured analytics wrote, a results table, and the archive as a modest footer action.'
        },
        {
            id: 'B',
            name: 'B · artefacts',
            kind: 'artefact-first',
            gist: 'The run produced files: the archive, a manifest of what is inside it with row counts and sizes, and a download per file. The numbers are a preview of one file.'
        },
        {
            id: 'C',
            name: 'C · narrative',
            kind: 'narrative-first',
            gist: 'A written summary of what the run did and what it means, figures in the prose and the files at the end, for a reader who wants the sentence before the spreadsheet.'
        }
    ];

    /* The engine's output directory, as a real run writes it. Names are the
       fixture's; a listing of a real archive is the operation that does not
       exist. rows/bytes are for the largest of the mock runs and are scaled
       per run, because three completed runs need three sizes and inventing
       three full tables would add noise and no honesty.

       Each run names the subset it writes. The subset follows the analytics
       its configuration runs: npv.csv and pricingstats.csv come from npv,
       sensitivity.csv from sensitivity, saccr.csv and saccrdetail.csv from
       saccr. Files whose analytic the run does not configure -- cashflow.csv,
       capital_crif.csv, todaysmarketcalibration_cashflows.csv -- are absent
       from that run's manifest. */
    var BASE_FILES = [
        {
            name: 'marketdata.csv', kind: 'csv', rows: 1208, bytes: 96432,
            preview: {
                columns: ['datumDate', 'datumId', 'datumValue'],
                sample: [
                    ['2026-10-16', 'FX/RATE/EUR/USD', '1.1706982778'],
                    ['2026-10-16', 'FX/RATE/GBP/USD', '1.3412775600'],
                    ['2026-10-16', 'IR/RATE/GBP/6M', '0.0412875000']
                ]
            }
        },
        {
            name: 'cashflow.csv', kind: 'csv', rows: 84512, bytes: 11800000,
            preview: {
                columns: ['TradeId', 'LegNo', 'PayDate', 'FlowType', 'Notional', 'Amount', 'Currency'],
                sample: [
                    ['8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', '1', '2027-01-15', 'Interest', '25000000', '412884.20', 'GBP'],
                    ['8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', '1', '2027-07-15', 'Interest', '25000000', '398114.60', 'GBP'],
                    ['c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', '2', '2027-01-15', 'Interest', '10000000', '182004.10', 'EUR']
                ]
            }
        },
        {
            name: 'log_structured.json', kind: 'json', rows: 11, bytes: 7040,
            preview: {
                lines: [
                    '{ "category": "Error", "group": "Analytics", "message": "Failed to build market: There are no quotes available for date October 16th, 2026", "sub_fields": [ { "name": "analyticType", "value": "PRICING" }, { "name": "exceptionType", "value": "Failed Analytic" } ] }',
                    '{ "category": "Error", "group": "Analytics", "message": "Portfolio does not contain any built trades, context is \'analytic/PRICING\'", "sub_fields": [ { "name": "analyticType", "value": "PRICING" }, { "name": "exceptionType", "value": "Failed Analytic" } ] }',
                    '{ "category": "Warning", "group": "Configuration", "message": "No volatility surface is bound for GBP; the equity analytic ran with a flat surface", "sub_fields": [ { "name": "warningType", "value": "MissingConfiguration" } ] }'
                ]
            }
        },
        {
            name: 'runtimes.csv', kind: 'csv', rows: 0, bytes: 1122,
            /* One row per phase and per analytic the run configured, built in
               filesFor because the analytic list is the run's, not a
               constant. */
            preview: {
                columns: ['Key', 'Total', 'Count', 'Max', 'Min', 'Average'],
                sample: []
            }
        },
        {
            name: 'todaysmarketcalibration.csv', kind: 'csv', rows: 2846, bytes: 9400000,
            preview: {
                columns: ['MarketObjectType', 'MarketObjectId', 'ResultId', 'ResultKey1', 'ResultKey2', 'ResultKey3', 'ResultType', 'ResultValue'],
                sample: [
                    ['yieldCurve', 'GBP1D', 'dayCounter', '', '', '', 'string', 'Actual/365 (Fixed)'],
                    ['yieldCurve', 'GBP1D', 'discountFactor', '1Y', '', '', 'number', '0.958114'],
                    ['swaptionVolatility', 'GBP', 'atmForward', '5Y', '10Y', '', 'number', '0.0114']
                ]
            }
        },
        {
            name: 'log.txt', kind: 'log', rows: 0, bytes: 34117,
            preview: {
                lines: [
                    'NOTICE   [2026-Oct-17 09:02:11.965677]    (OREAnalytics/orea/app/parameters.cpp:139) : Parameters:',
                    'NOTICE   [2026-Oct-17 09:02:11.965799]    (OREAnalytics/orea/app/parameters.cpp:144) : group = markets : pricing = default',
                    'ERROR    [2026-Oct-17 09:05:58.104210]    (OREAnalytics/orea/app/oreapp.cpp:441) : Failed to build market: There are no quotes available for date October 16th, 2026'
                ]
            }
        },
        {
            name: 'npv.csv', kind: 'csv', rows: 18204, bytes: 3900000,
            preview: {
                /* No Greek column: Delta and Gamma are sensitivity.csv's, and a
                   run that did not configure sensitivity has neither. */
                columns: ['TradeId', 'TradeType', 'Npv', 'Npv (USD)'],
                sample: [
                    ['8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', 'Swap', '-184204.18', '-240812.44'],
                    ['c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', 'Swap', '88204.55', '115284.18'],
                    ['2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', 'Bond', '612402.00', '612402.00'],
                    ['6b19d702-3c4a-4e18-8f2b-1d7c9a0e5b36', 'Swap', '44102.80', '57664.06'],
                    ['0a8e4f21-7b3d-4c95-9e61-4f2a8b0d7c53', 'FXOption', '-9204.40', '-12038.20']
                ]
            }
        },
        {
            name: 'sensitivity.csv', kind: 'csv', rows: 21842, bytes: 4800000,
            preview: {
                columns: ['TradeId', 'TradeType', 'RiskType', 'RiskFactor', 'Shift', 'Base', 'Shifted', 'Sensitivity'],
                sample: [
                    ['8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', 'Swap', 'IR', 'IR/RATE/GBP/10Y', '0.0001', '-184204.18', '-184481.02', '-2768400.00'],
                    ['c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', 'Swap', 'FX', 'FX/RATE/EUR/USD', '0.01', '88204.55', '88420.18', '21563.00'],
                    ['2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', 'Bond', 'IR', 'IR/RATE/GBP/5Y', '0.0001', '612402.00', '612430.88', '288800.00']
                ]
            }
        },
        {
            name: 'saccrdetail.csv', kind: 'csv', rows: 62318, bytes: 18200000,
            preview: {
                columns: ['NettingSetId', 'TradeId', 'IM', 'RC', 'AddOn', 'EAD', 'Currency'],
                sample: [
                    ['NS-ACMCOR-001', '8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', '184203.40', '42008.12', '110284.66', '336496.18', 'GBP'],
                    ['NS-ACMCOR-001', 'c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', '88402.10', '18402.00', '62004.18', '168808.28', 'GBP'],
                    ['NS-ACCOUK-004', '2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', '54002.00', '12004.60', '38002.14', '104008.74', 'EUR']
                ]
            }
        },
        {
            name: 'saccr.csv', kind: 'csv', rows: 4102, bytes: 1600000,
            preview: {
                columns: ['NettingSetId', 'IM', 'RC', 'PFE', 'EAD', 'Currency'],
                sample: [
                    ['NS-ACMCOR-001', '272605.50', '60410.12', '184208.88', '517224.50', 'GBP'],
                    ['NS-ACCOUK-004', '54002.00', '12004.60', '74008.14', '140014.74', 'EUR'],
                    ['NS-ACCOHK-002', '118204.20', '24008.40', '96002.18', '238214.78', 'HKD']
                ]
            }
        },
        {
            name: 'fixings.csv', kind: 'csv', rows: 132, bytes: 6200,
            preview: {
                columns: ['fixingDate', 'fixingId', 'fixingValue'],
                sample: [
                    ['2026-10-15', 'IR_SWAP/RATE/GBP/0D/1D/1M', '0.0412000000'],
                    ['2026-10-15', 'IR_SWAP/RATE/EUR/0D/1D/1M', '0.0300200000'],
                    ['2026-10-15', 'FX/RATE/EUR/USD', '1.1706982778']
                ]
            }
        },
        {
            name: 'capital_crif.csv', kind: 'csv', rows: 3904, bytes: 1200000,
            preview: {
                columns: ['Date', 'PortfolioId', 'EE', 'IM', 'EAD', 'RWA', 'Capital'],
                sample: [
                    ['2026-10-17', 'ACMCOR-RATES', '884204.18', '272605.50', '1156809.68', '694085.81', '55526.86'],
                    ['2026-10-17', 'ACMCOR-CREDIT', '412884.00', '118204.20', '531088.20', '318652.92', '25492.23'],
                    ['2026-10-17', 'ACMCOR-FX', '284118.40', '74402.00', '358520.40', '215112.24', '17208.98']
                ]
            }
        },
        {
            name: 'pricingstats.csv', kind: 'csv', rows: 32, bytes: 2800,
            preview: {
                columns: ['TradeId', 'TradeType', 'NumberOfPricings', 'CumulativeTiming', 'AverageTiming'],
                sample: [
                    ['8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', 'Swap', '6', '0', '0'],
                    ['c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', 'Swap', '6', '0', '0'],
                    ['2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', 'Bond', '2', '0', '0']
                ]
            }
        },
        {
            name: 'dividends.csv', kind: 'csv', rows: 218, bytes: 24600,
            preview: {
                columns: ['dividendExDate', 'equityId', 'dividendRate', 'dividendPaymentDate'],
                sample: [
                    ['2026-11-12', 'EQ/GB/HSBA', '0.1000000000', '2026-11-27'],
                    ['2026-11-19', 'EQ/GB/BP', '0.0800000000', '2026-12-04']
                ]
            }
        },
        {
            name: 'todaysmarketcalibration_cashflows.csv', kind: 'csv', rows: 418204, bytes: 62100000,
            preview: {
                columns: ['MarketObjectType', 'MarketObjectId', 'PillarDate', 'RateHelperType', 'CashflowNo', 'PayDate', 'FlowType', 'Amount', 'Currency'],
                sample: [
                    ['yieldCurve', 'GBP1D', '2026-11-17', 'OIS', '1', '2026-11-17', 'Interest', '2584.2342278253', 'GBP'],
                    ['yieldCurve', 'GBP1D', '2026-11-17', 'OIS', '2', '2026-12-17', 'Interest', '2618.1120042000', 'GBP'],
                    ['yieldCurve', 'EUR1D', '2026-11-17', 'OIS', '1', '2026-11-17', 'Interest', '1184.2040000000', 'EUR']
                ]
            }
        }
    ];

    var DEFAULT_RUN = '3f7a91c4';

    /* Five runs of four definitions, across the four parties. One completed
       clean, one completed carrying 8 Analytics errors and 3 Configuration
       warnings, one failed, one in flight, and one substituting run that
       completed clean. The errors use the model's own field names: severity,
       scope_code, subject, subject_id and the engine's words in message. */
    var RUNS = [
        {
            id: '4b7e1a02-8d61-4f3c-b2a7-9c1e5d0f7a63',
            short: '4b7e1a02',
            report: 'Board Risk Dashboard',
            definition: 'Board Risk Dashboard',
            party: 'ACMCOR',
            kind: 'production',
            reportType: 'risk',
            state: 'completed',
            startedAt: '2026-10-17 08:12:44',
            completedAt: '2026-10-17 08:15:02',
            duration: '2 m 18 s',
            outputMessage: 'Report execution completed.',
            key: 'reporting/runs/4b7e1a02-8d61-4f3c-b2a7-9c1e5d0f7a63/output.tar.gz',
            preProcessing: 'execute',
            analytics: ['npv', 'parametricVar', 'saccr'],
            files: ['npv.csv', 'saccr.csv', 'saccrdetail.csv', 'pricingstats.csv',
                'marketdata.csv', 'todaysmarketcalibration.csv', 'fixings.csv',
                'dividends.csv', 'runtimes.csv', 'log.txt', 'log_structured.json'],
            fileScale: 0.62,
            figures: [
                { value: 'USD 12,884,102', label: 'NPV', note: 'npv, base currency USD' },
                { value: 'USD 41.2 m', label: 'VaR 95%, one day', note: 'parametricVar, across the six entities' },
                { value: 'USD 2.76 bn', label: 'EAD', note: 'saccr, sum of the rows' },
                { value: '11,286', label: 'positions priced', note: 'the rows npv.csv carries, ACMCOR' }
            ],
            table: {
                title: 'NPV, VaR and EAD by legal entity',
                note: 'A reading of npv.csv and saccr.csv, which nothing parses yet. VaR does not add across rows: parametricVar applies the covariance matrix.',
                columns: ['Legal entity', 'Positions', 'NPV (USD)', 'VaR 95% (USD)', 'EAD (USD)'],
                rows: [
                    ['ACME BANK PLC', '2,840', '4,118,204', '12,884,204', '884,204,118'],
                    ['NORTHGATE CLEARING', '3,120', '3,904,118', '9,204,118', '764,118,402'],
                    ['MERIDIAN CAPITAL', '2,104', '2,884,020', '7,118,902', '512,884,204'],
                    ['HANSA PFANDBRIEF', '1,420', '1,204,118', '4,884,118', '284,118,904'],
                    ['PACIFIC TRUST', '1,104', '612,402', '3,402,884', '198,204,118'],
                    ['KESTREL ASSURANCE', '698', '161,240', '2,884,402', '118,884,020']
                ]
            },
            errors: [],
            story: [
                'Board Risk Dashboard finished at <span class="fig">08:15:02</span>, <span class="fig">two minutes and eighteen seconds</span> after ACMCOR asked for it. It was a production run: pre-processing gathered the party\u2019s trades and market data, the engine priced them on the grid, and retrieval put the result at <span class="key">reporting/runs/4b7e1a02-8d61-4f3c-b2a7-9c1e5d0f7a63/output.tar.gz</span>.',
                'Its configuration names three analytics \u2014 <span class="mono">npv</span>, <span class="mono">parametricVar</span> and <span class="mono">saccr</span> \u2014 and there is no measure on this page that is not one of their outputs. npv priced <span class="fig">11,286 positions</span> and returned NPV of <span class="fig">USD 12,884,102</span>; parametricVar returned one-day 95% VaR of <span class="fig">USD 41.2 m</span>; saccr returned total EAD of <span class="fig">USD 2.76 bn</span>.',
                'ACME BANK PLC was the largest legal entity on all three measures. VaR is a portfolio figure and does not add down the column: the analytic applies the covariance matrix, so the whole is not the sum of the six rows.',
                'The engine reported nothing. No error and no warning sits beside this output, so the archive is the whole account of the run.',
                'The archive is the run\u2019s answer: <span class="fig">{{files}} files</span>, <span class="fig">{{bytes}}</span> before compression. The figures above are a reading of npv.csv and saccr.csv, not a table this screen can query \u2014 the result store that would parse the archive is designed and not built. A run that configures no cashflow analytic writes no cashflow.csv, and this manifest holds none.'
            ]
        },
        {
            id: '3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3',
            short: '3f7a91c4',
            report: 'Headline Position',
            definition: 'Headline Position',
            party: 'ACCOUK',
            kind: 'production',
            reportType: 'risk',
            state: 'completed',
            startedAt: '2026-10-17 09:02:11',
            completedAt: '2026-10-17 09:06:40',
            duration: '4 m 29 s',
            outputMessage: 'Report execution completed.',
            key: 'reporting/runs/3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3/output.tar.gz',
            preProcessing: 'execute',
            analytics: ['npv', 'sensitivity'],
            files: ['npv.csv', 'sensitivity.csv', 'pricingstats.csv', 'marketdata.csv',
                'todaysmarketcalibration.csv', 'fixings.csv', 'dividends.csv',
                'runtimes.csv', 'log.txt', 'log_structured.json'],
            fileScale: 1,
            figures: [
                { value: 'USD 88,402,114', label: 'NPV', note: 'npv, base currency USD, all books' },
                { value: 'USD 1,846,818', label: 'Delta', note: 'sensitivity, summed over the books' },
                { value: 'USD 52,350', label: 'Gamma', note: 'sensitivity, summed over the books' },
                { value: '18,204', label: 'positions priced', note: 'the rows npv.csv carries' },
                { value: '24', label: 'books', note: 'the grouping the engine applied' }
            ],
            table: {
                title: 'NPV, Delta and Gamma by book',
                note: 'A reading of npv.csv and sensitivity.csv, which nothing parses yet. Delta and Gamma are the NPV change for a one basis point shift.',
                columns: ['Book', 'Positions', 'NPV (USD)', 'Delta (USD)', 'Gamma (USD)'],
                rows: [
                    ['GBP Rates', '4,218', '41,204,118', '612,404', '18,204'],
                    ['EUR Rates', '3,904', '28,118,402', '488,120', '14,118'],
                    ['USD Rates', '5,128', '12,884,020', '412,884', '9,204'],
                    ['FX Options', '1,820', '4,204,880', '168,204', '6,118'],
                    ['Inflation', '1,104', '1,118,204', '94,118', '2,884'],
                    ['Credit', '1,420', '612,402', '58,204', '1,420'],
                    ['Equity', '610', '260,088', '12,884', '402']
                ]
            },
            errors: [
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'PRICING', message: 'Failed to build market: There are no quotes available for date October 16th, 2026' },
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'PRICING', message: 'Portfolio does not contain any built trades, context is \'analytic/PRICING\'' },
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'SENSITIVITY', message: 'Failed to build market: There are no quotes available for date October 15th, 2026' },
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'SENSITIVITY', message: 'Sensitivity run skipped 412 positions: the base scenario has no price to bump' },
                { severity: 'error', scope: 'Analytics', subject: 'trade', subjectId: '8f2c1a04-9b7e-4d31-a05c-2e6f8d1b3c47', message: 'Failed to build trade: no index curve covers GBP-SONIA beyond 2031-04-15' },
                { severity: 'error', scope: 'Analytics', subject: 'trade', subjectId: 'c41b7e90-2d8a-4f61-b7e3-9a0c5d2f8e14', message: 'Failed to build trade: missing credit curve for issuer XS0298371204' },
                { severity: 'error', scope: 'Analytics', subject: 'trade', subjectId: '2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', message: 'Failed to build trade: fixing for GBP-SONIA on 2026-10-15 is not in the fixings set' },
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'SENSITIVITY', message: 'Failed to compute delta for risk factor IR/RATE/GBP/10Y: the shifted scenario did not price' },
                { severity: 'warning', scope: 'Configuration', subject: 'configuration', subjectId: 'EUR', message: 'Market data binding for currency EUR has no credit curve set; the run used the default calibration' },
                { severity: 'warning', scope: 'Configuration', subject: 'configuration', subjectId: 'GBP', message: 'No volatility surface is bound for GBP; the equity analytic ran with a flat surface' },
                { severity: 'warning', scope: 'Configuration', subject: 'configuration', subjectId: 'portfolio', message: 'Risk report configuration states no portfolio filter; the run priced every trade in ACCOUK' }
            ],
            story: [
                'Headline Position finished at <span class="fig">09:06:40</span>, <span class="fig">four minutes and twenty-nine seconds</span> after ACCOUK asked for it. It was a production run: pre-processing gathered the party\u2019s trades and market data, the engine priced them on the grid, and retrieval put the result at <span class="key">reporting/runs/3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3/output.tar.gz</span>.',
                'Its configuration names two analytics, <span class="mono">npv</span> and <span class="mono">sensitivity</span>, and no others. npv priced <span class="fig">18,204 positions</span> across <span class="fig">24 books</span>; sensitivity took the Delta and Gamma of each book. The Greek set is configurable \u2014 the report page calls these a deliberately shallow and wide first look \u2014 and this run took Delta and Gamma.',
                'NPV was <span class="fig">USD 88,402,114</span>. Delta was <span class="fig">USD 1,846,818</span> and Gamma <span class="fig">USD 52,350</span>, summed over the books, both for a one basis point shift. GBP Rates was the largest book, at <span class="fig">USD 41,204,118</span> of NPV.',
                'Eleven problems came with it: <span class="fig">8 Analytics errors</span> and <span class="fig">3 Configuration warnings</span>. The engine could not build a market for PRICING or SENSITIVITY because no quotes were available for 16 October 2026, and three trades would not build at all, so 412 positions carry an NPV and no Delta. The warnings name the market binding and the portfolio filter. None of them stopped the archive; all of them narrow what it can answer.',
                'The archive is the run\u2019s whole answer: <span class="fig">{{files}} files</span>, <span class="fig">{{bytes}}</span> before compression. Two of them are the analytics\u2019 own output \u2014 npv.csv and sensitivity.csv \u2014 and the rest are the market data the run read and the logs it wrote. A run of this configuration writes no cashflow.csv and no saccr.csv. The figures above are a reading of those two files, not a table this screen can query. Read the errors before the figures.'
            ]
        },
        {
            id: '8c1d2e47-0a35-4b6f-9e12-5d4c3a2b1f90',
            short: '8c1d2e47',
            report: 'NPV',
            definition: 'NPV',
            party: 'ACCOUS',
            kind: 'production',
            reportType: 'risk',
            state: 'failed',
            startedAt: '2026-10-17 09:18:02',
            completedAt: '2026-10-17 09:21:40',
            duration: '3 m 38 s',
            outputMessage: 'Run failed: submission could not find a compute app version visible to party ACCOUS.',
            key: null,
            preProcessing: 'execute',
            analytics: ['npv'],
            files: [],
            fileScale: 1,
            figures: [],
            table: null,
            errors: [
                { severity: 'error', scope: 'Report', subject: null, subjectId: null, message: 'report_execution: submit_compute failed: no compute app version is visible to party ACCOUS.' }
            ],
            story: [
                'NPV failed at <span class="fig">09:21:40</span>, <span class="fig">three minutes and thirty-eight seconds</span> after ACCOUS asked for it. It prepared its input and then stopped: submission could not find a compute app version visible to the party, so the engine never ran.',
                'Its configuration names one analytic, <span class="mono">npv</span>. There is no NPV on this page because no npv analytic ran, and no npv.csv because no archive came back. Nothing came out, and the run\u2019s one error row says why. The system put it in its own words: \u201cRun failed: submission could not find a compute app version visible to party ACCOUS.\u201d',
                'That is the whole answer this journey can give for this run: nothing to take, and a reason. The error is scope <span class="fig">Report</span>, because no engine saw the failure.'
            ]
        },
        {
            id: '6e5b0f38-2b14-4c8a-8d71-3f9e2a0c6b54',
            short: '6e5b0f38',
            report: 'Large Exposures',
            definition: 'Large Exposures',
            party: 'ACCOHK',
            kind: 'production',
            reportType: 'risk',
            state: 'running',
            startedAt: '2026-10-17 09:44:12',
            completedAt: null,
            duration: null,
            outputMessage: '',
            key: null,
            preProcessing: 'execute',
            analytics: ['npv', 'saccr'],
            files: [],
            fileScale: 1,
            figures: [],
            table: null,
            errors: [],
            story: [
                'Large Exposures has been running since <span class="fig">09:44:12</span>. Its configuration names <span class="mono">npv</span> and <span class="mono">saccr</span>, and neither has written anything yet: it has no output message, no archive and no key, because output does not exist until retrieval. The run writes its <span class="key">output_storage_key</span> only once its results come back.',
                'This page will fill when the run completes. It does not draw an empty archive, because an empty folder and a run that has not finished look the same on a screen and mean opposite things.',
                'What a reader has today is the run: its definition, its analytics, its party, its state and the time it started.'
            ]
        },
        {
            id: '9a20c7d1-4f68-4e93-b7c5-1d8e6f2a4b03',
            short: '9a20c7d1',
            report: 'Large Exposures',
            definition: 'Large Exposures',
            party: 'ACCOHK',
            kind: 'production',
            reportType: 'risk',
            state: 'completed',
            startedAt: '2026-10-16 18:02:11',
            completedAt: '2026-10-16 18:05:40',
            duration: '3 m 29 s',
            outputMessage: 'Report execution completed.',
            key: 'reporting/runs/9a20c7d1-4f68-4e93-b7c5-1d8e6f2a4b03/output.tar.gz',
            preProcessing: 'substitute',
            preparedInputKey: 'external/ore/packages/large-exposures/input.tar.gz',
            analytics: ['npv', 'saccr'],
            files: ['npv.csv', 'saccr.csv', 'saccrdetail.csv', 'pricingstats.csv',
                'marketdata.csv', 'todaysmarketcalibration.csv', 'fixings.csv',
                'dividends.csv', 'runtimes.csv', 'log.txt', 'log_structured.json'],
            fileScale: 0.18,
            figures: [
                { value: 'USD 383,853,078', label: 'NPV', note: 'npv, base currency USD' },
                { value: 'USD 2.14 bn', label: 'EAD', note: 'saccr, sum of the rows' },
                { value: '2,292', label: 'positions priced', note: 'the rows npv.csv carries' },
                { value: '6', label: 'counterparties in scope', note: 'the grouping saccr applied' }
            ],
            table: {
                title: 'NPV and EAD by counterparty',
                note: 'A reading of npv.csv and saccr.csv, which nothing parses yet.',
                columns: ['Counterparty', 'Positions', 'NPV (USD)', 'EAD (USD)'],
                rows: [
                    ['ACME BANK PLC', '684', '118,204,118', '612,404,118'],
                    ['NORTHGATE CLEARING', '512', '96,118,402', '488,118,402'],
                    ['MERIDIAN CAPITAL', '428', '88,204,118', '412,884,204'],
                    ['HANSA PFANDBRIEF', '302', '41,118,204', '284,118,904'],
                    ['PACIFIC TRUST', '218', '28,204,118', '198,204,118'],
                    ['KESTREL ASSURANCE', '148', '12,004,118', '144,270,254']
                ]
            },
            errors: [],
            story: [
                'Large Exposures finished at <span class="fig">18:05:40</span>, <span class="fig">three minutes and twenty-nine seconds</span> after ACCOHK asked for it. It resolved a prepared input rather than preparing one: pre-processing was substituted, and the archive it submitted is the one its definition already names, <span class="key">external/ore/packages/large-exposures/input.tar.gz</span>.',
                'Its configuration names two analytics, <span class="mono">npv</span> and <span class="mono">saccr</span>. npv priced <span class="fig">2,292 positions</span> and returned NPV of <span class="fig">USD 383,853,078</span>; saccr returned an EAD of <span class="fig">USD 2.14 bn</span> over <span class="fig">6 counterparties</span>. ACME BANK PLC carried the largest EAD, at <span class="fig">USD 612.4 m</span>.',
                'The engine reported nothing. The output is at <span class="key">reporting/runs/9a20c7d1-4f68-4e93-b7c5-1d8e6f2a4b03/output.tar.gz</span>, and the figures above are a reading of npv.csv and saccr.csv that nothing yet parses. This configuration writes no cashflow.csv and no capital_crif.csv, and the archive holds neither.'
            ]
        }
    ];

    var GAPS = [
        {
            title: 'No operation lists the archive\u2019s contents',
            body: 'The run names one object, reporting/runs/<instance-id>/output.tar.gz. No operation opens it, so no screen can say which CSVs are inside. Variant B\u2019s manifest is a drawing of that missing read, not a server answer, and a person who wants one file takes the archive and opens it.'
        },
        {
            title: 'No read of the archive\u2019s bytes',
            body: 'Reporting states the key and does not serve the object. No reporting operation and no BFF route returns it, so every download control here is drawn and not wired. The journey suggests GET /api/reporting/runs/<run-id>/outputs and a second route that streams the object by key; neither exists.'
        },
        {
            title: 'No parsed results to read',
            body: 'The result store that would parse the archive is designed in Report post-processing and not built, so A\u2019s figures, A\u2019s table and C\u2019s numbers have no read behind them. Which figures they are is decided by the analytics the configuration names \u2014 npv and sensitivity write NPV, Delta and Gamma; saccr writes EAD \u2014 and that list is a fixture here too, because no operation returns the configuration either. The archive is the only result that exists, and it exists as bytes the screen cannot fetch.'
        },
        {
            title: 'No read of the ORE package',
            body: 'ore.v1.ops.prepare_ore_package prepares the engine\u2019s input, and no operation returns it or lists its files. A run that substitutes pre-processing is no better off: its definition names the prepared_input_key, not the run, so a later edit to the definition changes what the screen reports for an earlier run.'
        },
        {
            title: 'The error codes sit above the desk',
            body: 'reporting::report_error_events:read and reporting::report_error_scopes:read are held by no role below TenantAdmin and SuperAdmin. The trading desk can take the archive and cannot read the errors that say what is missing from it \u2014 the one warning this screen exists to give.'
        },
        {
            title: 'The run does not name the compute job',
            body: 'report_instance carries no batch or workunit id, so the archive cannot be traced to the grid job whose output_uri it came from. The key is on the run; the lineage to the grid is not.'
        }
    ];

    var DEPENDENCY = {
        A: 'A needs the result store. Every figure and every row above is a reading of the output of the two analytics this run configured, and nothing parses an archive: post-processing is designed and not built. Built with today\u2019s operations, A is the archive footer with an empty screen above it.',
        B: 'B is the only variant buildable from today\u2019s reads: the state, the output message, the storage key and the errors are all fields of the run. What B cannot have is its own centre \u2014 the manifest \u2014 and any download, because no operation lists or serves the archive. Its file list is also inferred from the configured analytics, not observed inside the tar.',
        C: 'C needs what A needs \u2014 the parsed numbers \u2014 and then a writer trusted to keep a figure and its caveat in the same sentence. It is the easiest of the three to make wrong and the hardest to check: a column can be inspected, a sentence is believed.'
    };

    var S = { variant: 'A', runId: DEFAULT_RUN, file: 'npv.csv', showErrors: false, note: null };

    // ------------------------------------------------------------- helpers

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function variant() {
        return variantById(S.variant) || VARIANTS[0];
    }

    function runById(id) {
        return RUNS.filter(function (r) {
            return r.id === id || r.short === id;
        })[0];
    }

    function currentRun() {
        return runById(S.runId) || runById(DEFAULT_RUN);
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function num(n) {
        return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    function bytesText(n) {
        if (n < 1024) return n + ' B';
        if (n < 1024 * 1024) return (n / 1024).toFixed(1) + ' KB';
        return (n / (1024 * 1024)).toFixed(1) + ' MB';
    }

    function countBy(errors, severity) {
        return errors.filter(function (e) { return e.severity === severity; }).length;
    }

    function scopeCounts(errors, severity) {
        var scopeOrder = ['Trade', 'Analytics', 'Curve', 'Configuration', 'Report'];
        var counts = {};
        errors.forEach(function (e) {
            if (e.severity !== severity) return;
            counts[e.scope] = (counts[e.scope] || 0) + 1;
        });
        return scopeOrder.filter(function (s) { return counts[s]; }).map(function (s) {
            /* The scope code is the engine's own label, so it is not
               pluralised: "8 Analytics", "3 Configuration". */
            return counts[s] + ' ' + s;
        }).join(', ');
    }

    function stateLabel(run) {
        if (run.state === 'running') return 'in flight, no output yet';
        if (run.state === 'failed') return 'failed, nothing to take';
        var e = countBy(run.errors, 'error');
        var w = countBy(run.errors, 'warning');
        if (!e && !w) return 'completed clean';
        return 'completed carrying ' + e + ' error' + (e === 1 ? '' : 's') +
            ' and ' + w + ' warning' + (w === 1 ? '' : 's');
    }

    function shortState(run) {
        if (run.state === 'running') return 'running';
        if (run.state === 'failed') return 'failed';
        if (!run.errors.length) return run.preProcessing === 'substitute' ? 'clean · substitute' : 'clean';
        return run.errors.length + ' problems';
    }

    function stateSentence(run) {
        if (run.state === 'running') {
            return 'This run is in flight. Output does not exist until retrieval, so there is nothing to read yet \u2014 and the screen does not dress that as an empty archive.';
        }
        if (run.state === 'failed') {
            return 'This run failed. It produced no archive, no ORE package and no figures, so the honest answer to \u201cwhat did it produce\u201d is nothing, and the errors are why.';
        }
        if (run.errors.length) {
            return 'This run completed and carries ' + run.errors.length +
                ' problems. The archive exists and the engine\u2019s own log says what is missing from it: read that before the figures.';
        }
        return 'This run completed clean. One archive is its whole output, named by its output_storage_key, and the system said so in one sentence.';
    }

    function statePill(run) {
        if (run.state === 'running') return '<span class="statepill live">running</span>';
        if (run.state === 'failed') return '<span class="statepill bad">failed</span>';
        if (run.errors.length) return '<span class="statepill warn">completed \u00b7 ' + run.errors.length + ' problems</span>';
        return '<span class="statepill ok">completed clean</span>';
    }

    function filesFor(run) {
        var scale = run.fileScale === undefined ? 1 : run.fileScale;
        var names = run.files || [];
        return BASE_FILES.filter(function (f) {
            return names.indexOf(f.name) !== -1;
        }).map(function (f) {
            var preview = previewFor(f, run);
            var rows;
            if (f.kind === 'json') rows = run.errors.length;
            else if (f.kind === 'log') rows = 0;
            else if (f.preview.sample.length === 0) rows = preview.sample.length;
            else rows = Math.round(f.rows * scale);
            return {
                name: f.name,
                kind: f.kind,
                rows: rows,
                bytes: Math.round(f.bytes * scale),
                preview: preview
            };
        });
    }

    /* runtimes.csv carries one row per phase and per analytic, so its first
       rows are the run's own analytics. A fixture, like the rest of the file. */
    function previewFor(f, run) {
        if (f.name !== 'runtimes.csv') return f.preview;
        var sample = [['buildMarket()', '0', '1', '0', '0', '0.00'],
            ['runReporting()', '0', '1', '0', '0', '0.00']];
        (run.analytics || []).forEach(function (a) {
            sample.push([a + '|Run ' + a + 'Analytic', '0', '1', '0', '0', '0.00']);
        });
        return { columns: f.preview.columns, sample: sample };
    }

    function analyticsText(run) {
        return (run.analytics || []).join(', ');
    }

    function analyticsChips(run) {
        return (run.analytics || []).map(function (a) {
            return '<span class="ana-chip">' + esc(a) + '</span>';
        }).join('');
    }

    /* The line every variant carries: a run's figures are only as good as the
       analytics its configuration named, so the screen names them. */
    function analyticsLine(run) {
        return '<div class="analytics-line">' +
            '<span class="lbl">Analytics this run ran</span>' + analyticsChips(run) +
            '<span class="hint">the figures on this screen are the output of these analytics and no others</span>' +
            '</div>';
    }

    function totalBytes(files) {
        return files.reduce(function (sum, f) { return sum + f.bytes; }, 0);
    }

    function selectedFile(run) {
        var files = filesFor(run);
        var found = files.filter(function (f) { return f.name === S.file; })[0];
        return found || files.filter(function (f) { return f.name === 'npv.csv'; })[0] || files[0] ||
            { name: '\u2014', kind: 'csv', rows: 0, bytes: 0, preview: { columns: [], sample: [] } };
    }

    /* A story is written once and prints a file count and a size that belong
       to its own run's manifest; the tokens keep the prose and the manifest
       from disagreeing. */
    function storyFor(run) {
        var files = filesFor(run);
        return (run.story || []).map(function (p) {
            return p.replace(/\{\{files\}\}/g, esc(String(files.length)))
                .replace(/\{\{bytes\}\}/g, esc(bytesText(totalBytes(files))));
        });
    }

    function rowText(f) {
        if (f.kind === 'log') return '\u2014';
        return num(f.rows);
    }

    function setNote(kind, text) {
        S.note = { kind: kind, text: text };
        renderBar(currentRun());
    }

    // ------------------------------------------------------------- chrome

    function appHeader(run) {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav">' +
            '<a href="#" data-act="journey" data-name="See the reports a party runs">Reports</a>' +
            '<a href="#" data-act="journey" data-name="Run a report and watch it finish">Runs</a>' +
            '<a href="#" class="here" data-act="journey" data-name="Read what a run produced">Outputs</a>' +
            '</nav>' +
            '<span class="modechip">Trading desk \u00b7 ' + esc(run.party) + '</span>' +
            '</div></header>';
    }

    function fixtureBar() {
        var chips = RUNS.map(function (r) {
            return '<button class="chipbtn' + (r.id === currentRun().id ? ' on' : '') +
                '" data-act="run" data-run="' + r.id + '">' +
                '<span class="p">' + esc(r.party) + '</span>' + esc(r.report) +
                '<span class="an">' + esc(analyticsText(r)) + '</span>' +
                '<span class="st">\u00b7 ' + esc(shortState(r)) + '</span></button>';
        }).join('');
        return '<div class="fixturebar">' +
            '<span class="lbl">Fixture \u00b7 run in view</span>' + chips +
            '</div>';
    }

    function runHead(run) {
        var rows = [
            ['Party', run.party],
            ['Kind', run.kind],
            ['Report type', run.reportType],
            ['Definition', run.definition],
            ['Run', run.id],
            ['Started', run.startedAt],
            ['Completed', run.completedAt || '\u2014'],
            ['Took', run.duration || '\u2014']
        ];
        var metaline = rows.map(function (pair) {
            return '<span><span class="k">' + pair[0] + '</span> <b>' + esc(pair[1]) + '</b></span>';
        }).join('');

        var navItems = [
            { label: 'Reports', name: 'See the reports a party runs' },
            { label: 'Run', name: 'Run a report and watch it finish' },
            { label: 'Outputs', name: 'Read what a run produced', here: true },
            { label: 'Errors', name: 'Find out why a run went wrong', count: run.errors.length },
            { label: 'Schedule', name: 'Choose when a report runs' }
        ];
        var nav = navItems.map(function (item) {
            return '<button class="' + (item.here ? 'here' : '') + '" data-act="journey" data-name="' +
                esc(item.name) + '">' + esc(item.label) +
                (item.count ? '<span class="count">' + item.count + '</span>' : '') + '</button>';
        }).join('');

        return '<header class="runhead">' +
            '<div class="crumbs">Reporting <span>/</span> Runs <span>/</span> ' +
                esc(run.report) + ' <span>/</span> Outputs</div>' +
            '<div class="top"><div>' +
            '<h1>' + esc(run.report) + ' \u2014 run ' + esc(run.short) + '</h1>' +
            '<p class="lede">' + esc(stateSentence(run)) + '</p>' +
            '</div><div class="actions">' + statePill(run) +
            '<a class="btn linklike" href="../index.org">Back to ../index.org</a>' +
            '</div></div>' +
            '<div class="metaline">' + metaline + '</div>' +
            analyticsLine(run) +
            '<nav class="surfacenav">' + nav +
            '<span class="navhint">one run surface \u00b7 five journeys as sections, not five screens</span>' +
            '</nav>' +
            '</header>';
    }

    function amberNote(run) {
        return '<div class="notice warn"><strong>What is fixture.</strong> ' +
            'Every figure, file name, row count, size and error below is mock, and nothing on this page reads a server. ' +
            'The figures are fixtures standing in for the output of the analytics this run\u2019s configuration names \u2014 ' +
            '<span class="mono">' + esc(analyticsText(run)) + '</span> \u2014 and the result store that would parse the ' +
            'archive and read them out is designed and not built. ' +
            'There is no operation that lists the files inside a run\u2019s archive, so no file list can be served \u2014 ' +
            'variant B draws one as a fixture and says so on it. No operation returns the archive or the ORE package, ' +
            'so every download is drawn, not wired.' +
            '</div>';
    }

    // -------------------------------------------------------- shared parts

    function saidBlock(run) {
        if (run.state === 'running') {
            return '<section class="card tight"><div class="said flat">' +
                '<span class="lbl">What the system said</span>' +
                '<span class="q faint">\u2014 no output message yet</span>' +
                '<span class="hint">output_message is written when the run ends; output_storage_key when retrieval finishes.</span>' +
                '</div></section>';
        }
        return '<section class="card tight"><div class="said flat">' +
            '<span class="lbl">What the system said</span>' +
            '<span class="q">' + esc(run.outputMessage) + '</span>' +
            '<span class="hint">output_message, as recorded. Not paraphrased.</span>' +
            '</div></section>';
    }

    function errorTable(run) {
        var body = run.errors.map(function (e) {
            var subject = e.subject ? esc(e.subject) + ' ' + esc(e.subjectId) : 'the run';
            return '<tr>' +
                '<td class="sev"><span class="tag ' + (e.severity === 'error' ? 'bad' : 'warn') + '">' +
                    esc(e.severity) + '</span></td>' +
                '<td class="code">' + esc(e.scope) + '</td>' +
                '<td class="subj" title="' + subject + '">' + subject + '</td>' +
                '<td class="msg">' + esc(e.message) + '</td>' +
                '</tr>';
        }).join('');
        return '<div class="table-wrap"><table class="errtable"><thead><tr>' +
            '<th>Severity</th><th>Scope</th><th>Subject</th><th>Message</th>' +
            '</tr></thead><tbody>' + body + '</tbody></table></div>';
    }

    function errorBlock(run) {
        var errors = run.errors;
        if (!errors.length) return '';
        var e = countBy(errors, 'error');
        var w = countBy(errors, 'warning');
        var counts = scopeCounts(errors, 'error') + (w ? ' and ' + scopeCounts(errors, 'warning') : '');
        var failed = run.state === 'failed';

        return '<div class="notice err">' +
            '<strong>' + (failed ? 'This run failed.' : 'This run completed, and it still carries errors.') + '</strong> ' +
            (failed
                ? 'The run\u2019s own error rows are the reason; nothing was produced to open.'
                : 'The archive exists, and the engine\u2019s log says it is incomplete. Read this before you trust the numbers.') +
            ' <span class="mono">' + esc(counts) + '</span>' +
            '<div class="inner-actions">' +
            '<button class="btn small" data-act="toggle-errors">' +
                (S.showErrors ? 'Hide the ' : 'Show the ') + errors.length + ' error row' + (errors.length === 1 ? '' : 's') +
            '</button>' +
            '<span class="hint">The full list belongs to <em>Find out why a run went wrong</em>; it opens here as a section of the same surface.</span>' +
            '</div>' +
            (S.showErrors ? errorTable(run) : '') +
            '</div>';
    }

    /* A treats the figures and the table as one thing: the run's result is a
       set of numbers, and the table is where they come from. */
    function resultCard(run) {
        var tiles = run.figures.map(function (f) {
            return '<div class="fig"><div class="v">' + esc(f.value) + '</div>' +
                '<div class="l">' + esc(f.label) + '</div>' +
                '<div class="n">' + esc(f.note) + '</div></div>';
        }).join('');

        var t = run.table;
        var table = '';
        if (t) {
            var head = t.columns.map(function (c, i) {
                return '<th' + (i > 0 ? ' class="num"' : '') + '>' + esc(c) + '</th>';
            }).join('');
            var body = t.rows.map(function (row) {
                return '<tr>' + row.map(function (cell, i) {
                    return '<td' + (i > 0 ? ' class="num"' : '') + '>' + esc(cell) + '</td>';
                }).join('') + '</tr>';
            }).join('');
            table = '<div class="sectionhead" style="margin-top:20px"><h3 class="subhead">' + esc(t.title) + '</h3>' +
                '<span class="meta"><span class="tag warn">fixture</span><span>' + esc(t.note) + '</span></span></div>' +
                '<div class="table-wrap"><table><thead><tr>' + head + '</tr></thead><tbody>' + body + '</tbody></table></div>';
        }

        return '<section class="card">' +
            '<header class="sectionhead"><h2>The result</h2>' +
            '<span class="meta"><span class="tag accent">' + esc(analyticsText(run)) + '</span>' +
            '<span class="tag warn">fixture</span><span>no measure below comes from any other analytic</span></span></header>' +
            analyticsLine(run) +
            '<div class="figs" style="margin-top:16px">' + tiles + '</div>' + table +
            '<p class="rowfoot">No operation returns these figures or rows. They are what the archive says once something ' +
            'parses it \u2014 <span class="mono">' + esc(analyticsText(run)) + '</span> wrote them \u2014 and the result ' +
            'store that would parse it is designed and not built.</p>' +
            '</section>';
    }

    function archiveFooter(run) {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The archive</h2>' +
            '<span class="meta">the run\u2019s whole output</span></header>' +
            '<div class="keyline">' +
            '<span class="key">' + esc(run.key) + '</span>' +
            '<button class="btn small" data-act="copy-key">Copy key</button>' +
            '<button class="btn small" data-act="download-archive">Download archive</button>' +
            '</div>' +
            '<p class="rowfoot">output_storage_key. Its contents are not listed: no operation lists the files inside an ' +
            'archive, and no route serves its bytes \u2014 the download is drawn, not wired.</p>' +
            '</section>';
    }

    function packageBlock(run) {
        var title = 'The ORE package this run read';
        if (run.state === 'running') {
            return '<section class="card"><header class="sectionhead"><h2>' + title + '</h2>' +
                '<span class="tag muted">none yet</span></header>' +
                '<p class="dim">A package exists once pre-processing runs, and this run has not finished. Nothing to open.</p>' +
                '</section>';
        }
        if (run.state === 'failed') {
            return '<section class="card"><header class="sectionhead"><h2>' + title + '</h2>' +
                '<span class="tag bad">none</span></header>' +
                '<p class="dim">This run failed before an engine read an input. It has no package to hand over, and no ' +
                'operation would return one if it had.</p></section>';
        }
        if (run.preProcessing === 'substitute') {
            return '<section class="card"><header class="sectionhead"><h2>' + title + '</h2>' +
                '<span class="meta"><span class="tag accent">pre-processing substituted</span></span></header>' +
                '<p class="dim">This run did not prepare an input. It submitted the archive its definition already names, ' +
                'the definition\u2019s prepared_input_key:</p>' +
                '<div class="keyline" style="margin-top:10px"><span class="key">' + esc(run.preparedInputKey) + '</span>' +
                '<button class="btn small" data-act="open-package">Open package</button></div>' +
                '<p class="rowfoot">The package is the engine\u2019s input, not one of the run\u2019s outputs. The definition ' +
                'names the key and the run does not, so a later edit to the definition changes what this screen reports for ' +
                'this run. No operation returns the package or lists its files.</p></section>';
        }
        return '<section class="card"><header class="sectionhead"><h2>' + title + '</h2>' +
            '<span class="meta"><span class="tag accent">prepared by this run</span></span></header>' +
            '<p class="dim">Pre-processing executed, so this run prepared its own input with ' +
            '<span class="mono">ore.v1.ops.prepare_ore_package</span>. The package is the engine\u2019s input \u2014 the ' +
            'directory of CSV files and the structured log the engine read \u2014 and it is a different object from the ' +
            'run\u2019s output archive above.</p>' +
            '<div class="keyline" style="margin-top:10px"><button class="btn small" data-act="open-package">Open package</button>' +
            '<span class="hint">No operation returns the package or lists its files, and the run records no storage key for it.</span></div>' +
            '</section>';
    }

    function nothingToTake(run) {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Nothing to take</h2>' +
            '<span class="meta"><span class="tag bad">no archive</span><span class="tag muted">no package</span></span></header>' +
            '<p class="dim">A failed run writes no output_storage_key, so there is no archive to download and no figures to ' +
            'read. The screen says that plainly instead of drawing an empty folder. The run\u2019s errors are the answer to ' +
            '\u201cwhat did it produce\u201d: nothing, and why.</p>' +
            '</section>';
    }

    function noOutputYet(run) {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>No output yet</h2>' +
            '<span class="meta"><span class="tag accent">in flight</span><span>started ' + esc(run.startedAt.slice(11)) + '</span></span></header>' +
            '<p class="dim">Output does not exist until retrieval, so this run carries no archive key, no archive and no ' +
            'figures. The panel fills when the run completes; it does not draw an empty archive, which would read as a run ' +
            'that produced nothing.</p>' +
            '<p class="rowfoot">A run in this state is watched on <em>Run a report and watch it finish</em>; the outputs ' +
            'appear here once retrieval writes the key.</p>' +
            '</section>';
    }

    // ------------------------------------------------------------ variant A

    function figuresFirst(run) {
        if (run.state === 'running') {
            return saidBlock(run) + noOutputYet(run) + packageBlock(run);
        }
        if (run.state === 'failed') {
            return saidBlock(run) + errorBlock(run) + nothingToTake(run) + packageBlock(run);
        }
        return saidBlock(run) + errorBlock(run) + resultCard(run) +
            archiveFooter(run) + packageBlock(run);
    }

    // ------------------------------------------------------------ variant B

    function archiveHeroNote(run) {
        var files = filesFor(run);
        return '<header class="sectionhead"><h2>The engine\u2019s output archive</h2>' +
            '<span class="meta"><span class="tag ok">retrieved</span>' +
            '<span>' + files.length + ' files \u00b7 ' + bytesText(totalBytes(files)) + ' of CSV and logs</span></span></header>' +
            '<div class="keyline">' +
            '<span class="key">' + esc(run.key) + '</span>' +
            '<button class="btn small" data-act="copy-key">Copy key</button>' +
            '<button class="btn small primary" data-act="download-archive">Download archive</button>' +
            '</div>';
    }

    function manifestNote(run) {
        return '<div class="notice warn"><strong>No operation lists the archive\u2019s contents.</strong> ' +
            'This manifest is a fixture: one row per file, its row count and size. Nothing here opened the tar, ' +
            'and no read serves a file from inside it. The list follows the configured analytics \u2014 this run\u2019s ' +
            'configuration names <span class="mono">' + esc(analyticsText(run)) + '</span>, so the fixture holds their ' +
            'output files and no file that an analytic it did not run would write.</div>';
    }

    function manifestCard(run) {
        var files = filesFor(run);
        var sel = selectedFile(run).name;
        var body = files.map(function (f) {
            return '<tr class="file-row' + (f.name === sel ? ' sel' : '') + '" data-act="select-file" data-file="' +
                esc(f.name) + '">' +
                '<td><span class="nm">' + esc(f.name) + '</span></td>' +
                '<td><span class="kind">' + (f.kind === 'log' ? 'log' : f.kind) + '</span></td>' +
                '<td class="num">' + rowText(f) + '</td>' +
                '<td class="num">' + bytesText(f.bytes) + '</td>' +
                '<td class="act"><button class="btn small ghost" data-act="download-file" data-file="' +
                    esc(f.name) + '">Download</button></td></tr>';
        }).join('');
        return '<section class="card">' +
            archiveHeroNote(run) +
            analyticsLine(run) +
            '<div class="sectionhead" style="margin-top:16px"><h3 class="subhead">Inside the archive</h3>' +
            '<span class="meta"><span class="tag warn">fixture manifest</span><span>select a file to preview it</span></span></div>' +
            manifestNote(run) +
            '<div class="table-wrap" style="margin-top:12px"><table class="manifest"><thead><tr>' +
            '<th>File</th><th>Type</th><th class="num">Rows</th><th class="num">Size</th><th></th>' +
            '</tr></thead><tbody>' + body + '</tbody></table></div>' +
            '<p class="rowfoot">A download per file is drawn because B argues the file is the unit; no operation serves ' +
            'one, so every control in this column is unwired.</p>' +
            '</section>';
    }

    function previewPanel(run) {
        var f = selectedFile(run);
        var body;
        if (f.preview.lines) {
            body = '<pre class="codeblock">' + esc(f.preview.lines.join('\n')) + '</pre>';
        } else if (!f.preview.sample.length) {
            body = '<p class="emptyline">No rows: the file is empty.</p>';
        } else {
            body = '<div class="table-wrap"><table><thead><tr>' +
                f.preview.columns.map(function (c) { return '<th>' + esc(c) + '</th>'; }).join('') +
                '</tr></thead><tbody>' +
                f.preview.sample.map(function (row) {
                    return '<tr>' + row.map(function (cell) { return '<td>' + esc(cell) + '</td>'; }).join('') + '</tr>';
                }).join('') + '</tbody></table></div>';
        }
        var footer = f.kind === 'log'
            ? 'The last lines of a text log. This preview is fixture like the manifest: nothing read the file.'
            : (f.preview.lines
                ? 'The first ' + f.preview.lines.length + ' structured-log lines, which are the run\u2019s errors. Preview is fixture.'
                : 'First ' + f.preview.sample.length + ' of ' + num(f.rows) + ' rows. The preview is fixture; nothing reads the CSV.');

        return '<section class="card preview">' +
            '<header class="sectionhead"><h2>Preview</h2>' +
            '<span class="meta">the numbers, one file at a time</span></header>' +
            '<div class="fname">' + esc(f.name) + '</div>' +
            '<p class="hint" style="margin-bottom:10px">' + rowText(f) + ' rows \u00b7 ' + bytesText(f.bytes) +
            ' \u00b7 selected from the manifest</p>' +
            body +
            '<p class="rowfoot">' + footer + '</p>' +
            '</section>';
    }

    function artefactFirst(run) {
        if (run.state === 'running') {
            return saidBlock(run) + noOutputYet(run) + packageBlock(run);
        }
        if (run.state === 'failed') {
            return saidBlock(run) + errorBlock(run) + nothingToTake(run) + packageBlock(run);
        }
        return saidBlock(run) + errorBlock(run) +
            '<div class="split">' + manifestCard(run) + previewPanel(run) + '</div>' +
            packageBlock(run);
    }

    // ------------------------------------------------------------ variant C

    function narrativeFirst(run) {
        var paragraphs = storyFor(run).map(function (p) {
            return '<p>' + p + '</p>';
        }).join('');

        var filesCard;
        if (run.state === 'running') {
            filesCard = '<section class="card"><header class="sectionhead"><h2>The files</h2>' +
                '<span class="tag muted">nothing yet</span></header>' +
                '<p class="dim">There are no files to list. The archive key is written when retrieval finishes, and this ' +
                'section will name it then.</p></section>';
        } else if (run.state === 'failed') {
            filesCard = '<section class="card"><header class="sectionhead"><h2>The files</h2>' +
                '<span class="tag bad">none</span></header>' +
                '<p class="dim">Nothing came out. A failed run has no archive to list and no package to take; the errors ' +
                'above are the run\u2019s only product.</p></section>';
        } else {
            var files = filesFor(run);
            var chips = files.map(function (f) {
                var rows = f.kind === 'log' ? '' : rowText(f) + ' rows';
                return '<div class="fc"><span>' + esc(f.name) + '</span>' +
                    '<span class="rw">' + rows + (rows ? ' ' : '') + '<span class="sz">' + bytesText(f.bytes) +
                    '</span></span></div>';
            }).join('');
            filesCard = '<section class="card">' +
                '<header class="sectionhead"><h2>What came out</h2>' +
                '<span class="meta">' + files.length + ' files \u00b7 ' + bytesText(totalBytes(files)) + '</span></header>' +
                '<div class="keyline" style="margin-bottom:12px">' +
                '<span class="key">' + esc(run.key) + '</span>' +
                '<button class="btn small" data-act="copy-key">Copy key</button>' +
                '<button class="btn small" data-act="download-archive">Download archive</button>' +
                '</div>' +
                '<div class="filechips">' + chips + '</div>' +
                '<p class="rowfoot">The list is a description, not a set of links: no operation lists the files inside the ' +
                'archive, and none serves one of them. A reader who wants npv.csv takes the archive and opens it.</p>' +
                '</section>';
        }

        /* The warning comes before the prose, because the prose carries the
           figures: a narrative screen must not let a sentence be trusted
           before the run's own account of what went wrong. */
        return errorBlock(run) +
            '<section class="card">' +
            '<div class="said"><span class="lbl">What the system said</span>' +
            '<span class="q">' + (run.state === 'running' ? '\u2014 no output message yet' : esc(run.outputMessage)) + '</span></div>' +
            analyticsLine(run) +
            '<div class="prose" style="margin-top:18px">' + paragraphs + '</div>' +
            '</section>' +
            filesCard + packageBlock(run);
    }

    // -------------------------------------------------------------- gaps

    function gapsPanel(run) {
        var dependency = '<div class="dep"><div class="t">What variant <b>' + esc(S.variant) + '</b> depends on</div>' +
            '<p class="dim">' + esc(DEPENDENCY[S.variant]) + '</p></div>';

        var items = GAPS.map(function (gap) {
            return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
        }).join('');

        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>Not on this screen yet</h2>' +
            '<span class="faint" style="font-size:12px">the gap this variant lives with, and the one the journey records</span></header>' +
            dependency + '<dl style="margin-top:16px">' + items + '</dl>' +
            '</section>';
    }

    // ------------------------------------------------------------- render

    function render() {
        var run = currentRun();
        var body = fixtureBar() + runHead(run) + amberNote(run) +
            ({ A: figuresFirst, B: artefactFirst, C: narrativeFirst }[S.variant] || figuresFirst)(run) +
            gapsPanel(run);

        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader(run) +
            '<main>' + body + '</main></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data \u00b7 nothing on this page reads a server' +
            ' \u00b7 variant ' + S.variant + ' \u00b7 ' + variant().kind;

        renderBar(run);
    }

    function renderBar(run) {
        var buttons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');

        var active = variant();
        var note = S.note
            ? '<div class="bar-note"><span>' + esc(S.note.text) + '</span>' +
              '<button class="btn small ghost" data-act="dismiss-note">Dismiss</button></div>'
            : '';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + buttons +
            '<span class="gist">' + esc(active.gist) + '</span>' +
            '</div>' + note +
            '<div class="bar-state">' +
            'variant <b>' + esc(S.variant) + '</b> (' + esc(active.kind) + ')' +
            '<span class="dot">\u00b7</span>run <b>' + (RUNS.indexOf(run) + 1) + ' of ' + RUNS.length + '</b>' +
            '<span class="dot">\u00b7</span>' + esc(run.report) + ' (' + esc(run.party) + ')' +
            '<span class="dot">\u00b7</span>' + esc(stateLabel(run)) +
            '</div>';
    }

    // ------------------------------------------------------------- events

    function setVariant(id) {
        if (!variantById(id) || S.variant === id) return;
        S.variant = id;
        S.showErrors = false;
        S.note = null;
        render();
        writeParams();
    }

    function setRun(id) {
        var run = runById(id);
        if (!run || run.id === S.runId) return;
        S.runId = run.id;
        S.showErrors = false;
        S.file = 'npv.csv';
        S.note = null;
        render();
        writeParams();
    }

    function copyKey() {
        var key = currentRun().key;
        if (!key) return;
        var done = function (ok) {
            setNote(ok ? 'info' : 'warn', ok
                ? 'Copied the storage key to the clipboard: ' + key
                : 'The clipboard is not available from file://. Select the key and copy it by hand.');
        };
        if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(key).then(function () { done(true); }, function () { done(false); });
        } else {
            done(false);
        }
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');

        if (act === 'variant') {
            setVariant(el.getAttribute('data-variant'));
        } else if (act === 'run') {
            setRun(el.getAttribute('data-run'));
        } else if (act === 'toggle-errors') {
            S.showErrors = !S.showErrors;
            render();
        } else if (act === 'select-file') {
            S.file = el.getAttribute('data-file');
            render();
        } else if (act === 'download-file') {
            setNote('warn', 'Nothing was downloaded. No operation lists the files inside a run\u2019s archive, let alone ' +
                'serves one of them; ' + el.getAttribute('data-file') + ' is a name from a fixture.');
        } else if (act === 'download-archive') {
            setNote('warn', 'Nothing was downloaded. No reporting operation serves the archive\u2019s bytes and this ' +
                'prototype has no BFF route. The control is drawn, not wired.');
        } else if (act === 'copy-key') {
            copyKey();
        } else if (act === 'open-package') {
            setNote('warn', 'Nothing was opened. ore.v1.ops.prepare_ore_package prepares the engine\u2019s input; no ' +
                'operation returns the package or lists its files.');
        } else if (act === 'journey') {
            setNote('info', 'This prototype draws one section of the run surface. The five reporting journeys are ' +
                'sections of one page, not five screens; the other four are drawn in their own prototypes.');
        } else if (act === 'dismiss-note') {
            S.note = null;
            renderBar(currentRun());
        }
    });

    document.addEventListener('keydown', function (ev) {
        if (ev.target && /^(INPUT|TEXTAREA|SELECT)$/.test(ev.target.tagName)) return;
        if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
        var k = String(ev.key).toLowerCase();
        if (k === 'a' || k === 'b' || k === 'c') setVariant(k.toUpperCase());
    });

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toUpperCase();
        if (variantById(v)) S.variant = v;
        var r = p.get('run');
        if (r && runById(r)) S.runId = runById(r).id;
        var f = p.get('file');
        if (f && filesFor(currentRun()).some(function (x) { return x.name === f; })) S.file = f;
        if (p.get('errors') === '1') S.showErrors = true;
    }

    function writeParams() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('run', currentRun().id);
        if (S.file !== 'npv.csv') p.set('file', S.file);
        try {
            /* A file:// document has a null origin and Chrome refuses to
               replace its history entry. The bar already prints the state,
               so the URL is a convenience, not the record. */
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* Nothing to do: the page works without the URL. */
        }
    }

    readParams();
    render();
    writeParams();
})();

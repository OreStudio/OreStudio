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
                    ['7b3e28d4-1c95-4a72-9f03-6d8e5b1a4c27', 'Bond', '612402.00', '722761.24'],
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
                    ['7b3e28d4-1c95-4a72-9f03-6d8e5b1a4c27', 'Bond', 'IR', 'IR/RATE/GBP/5Y', '0.0001', '612402.00', '612430.88', '288800.00']
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
                    ['7b3e28d4-1c95-4a72-9f03-6d8e5b1a4c27', 'Bond', '2', '0', '0']
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
                { value: '10', label: 'books', note: 'each trade in exactly one' }
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
                'Its configuration names two analytics, <span class="mono">npv</span> and <span class="mono">sensitivity</span>, and no others. npv priced <span class="fig">18,204 positions</span> across <span class="fig">10 books</span>; sensitivity took the Delta and Gamma of each book. The Greek set is configurable \u2014 the report page calls these a deliberately shallow and wide first look \u2014 and this run took Delta and Gamma.',
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

    /* Per-trade results. A real run writes npv.csv into its output archive with
       exactly these columns: TradeId, TradeType, Maturity, MaturityTime, NPV,
       NpvCurrency, NPV(Base), BaseCurrency, Notional, NotionalCurrency,
       Notional(Base), NettingSet, CounterParty. One row per trade, each with its
       own NPV, its base-currency NPV, its notional and its counterparty.

       Six of those columns are drawn below. Every row is a fixture: no
       operation parses the archive, and ores_compute_results_tbl carries only
       the archive's location and an outcome. A trade the engine could not build
       writes no NPV into npv.csv, so the failed row here is keyed to the run's
       own trade-scoped error fixture in RUNS above, joined by the same TradeId;
       it carries no price and the error is its answer. The table is a sample of
       the run's rows, not all of them.

       Each id is the trading trade's id, so trading.v1.trades.get resolves a row
       to the trade itself. These bonds are rates book holdings: a coupon, a
       maturity and a netting set apiece. */
    var TRADES = {
        /* Headline Position, ACCOUK, 18,204 rows in npv.csv. */
        '3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3': [
            { id: '2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', type: 'Bond', maturity: '2031-04-15',
                netting: 'NS-ACCOUK-004', ccy: 'GBP', npv: null, npvBase: null,
                notional: '40,000,000', notionalCcy: 'GBP', cp: 'ACME BANK PLC' },
            { id: '5a17c8e2-9d3b-4f60-8c21-7e4b0a9d2f18', type: 'Bond', maturity: '2034-09-30',
                netting: 'NS-ACCOUK-004', ccy: 'GBP', npv: '1,284,402.00', npvBase: '1,516,004.75',
                notional: '75,000,000', notionalCcy: 'GBP', cp: 'NORTHGATE CLEARING' },
            { id: '7b3e28d4-1c95-4a72-9f03-6d8e5b1a4c27', type: 'Bond', maturity: '2032-06-15',
                netting: 'NS-ACCOUK-001', ccy: 'GBP', npv: '612,402.00', npvBase: '722,761.24',
                notional: '40,000,000', notionalCcy: 'GBP', cp: 'MERIDIAN CAPITAL' },
            { id: '0e4f9a63-8b17-4d25-a3c9-2f7b1e8d5a04', type: 'Bond', maturity: '2036-01-20',
                netting: 'NS-ACCOUK-002', ccy: 'EUR', npv: '884,118.00', npvBase: '959,262.86',
                notional: '50,000,000', notionalCcy: 'EUR', cp: 'HANSA PFANDBRIEF' },
            { id: 'c9d2b571-3a84-4e16-8f57-1b9c6d0e2384', type: 'Bond', maturity: '2029-11-05',
                netting: 'NS-ACCOUK-003', ccy: 'GBP', npv: '-184,204.18', npvBase: '-217,438.86',
                notional: '25,000,000', notionalCcy: 'GBP', cp: 'PACIFIC TRUST' },
            { id: '4f1a6c38-7e2b-4d91-b605-9c3a8e2f7d15', type: 'Bond', maturity: '2030-03-12',
                netting: 'NS-ACCOUK-001', ccy: 'USD', npv: '274,880.60', npvBase: '274,880.60',
                notional: '20,000,000', notionalCcy: 'USD', cp: 'KESTREL ASSURANCE' },
            { id: '8d5b0e74-2c69-4a13-9e88-5f1b7c3d6a92', type: 'Bond', maturity: '2033-08-25',
                netting: 'NS-ACCOUK-002', ccy: 'GBP', npv: '441,082.40', npvBase: '520,668.83',
                notional: '30,000,000', notionalCcy: 'GBP', cp: 'ACME BANK PLC' },
            { id: '1b7c4f20-6a39-4e58-9d02-8c6f2b5a7e31', type: 'Bond', maturity: '2028-12-31',
                netting: 'NS-ACCOUK-003', ccy: 'USD', npv: '96,204.10', npvBase: '96,204.10',
                notional: '10,000,000', notionalCcy: 'USD', cp: 'NORTHGATE CLEARING' }
        ],
        /* Board Risk Dashboard, ACMCOR, 11,286 rows in npv.csv. */
        '4b7e1a02-8d61-4f3c-b2a7-9c1e5d0f7a63': [
            { id: '6a2d9f41-7b58-4c03-9e17-2d4a8b6f0c93', type: 'Bond', maturity: '2039-05-16',
                netting: 'NS-ACMCOR-001', ccy: 'GBP', npv: '2,184,402.60', npvBase: '2,578,204.18',
                notional: '120,000,000', notionalCcy: 'GBP', cp: 'ACME BANK PLC' },
            { id: 'b73f0c85-4e19-4a26-8d70-5c1e9b3a7f24', type: 'Bond', maturity: '2038-11-30',
                netting: 'NS-ACMCOR-002', ccy: 'GBP', npv: '1,904,118.20', npvBase: '2,247,406.18',
                notional: '96,000,000', notionalCcy: 'GBP', cp: 'NORTHGATE CLEARING' },
            { id: '3e9a1d67-2f84-4b50-a6c3-9d7b1e0c5a48', type: 'Bond', maturity: '2037-02-15',
                netting: 'NS-ACMCOR-003', ccy: 'EUR', npv: '1,204,118.00', npvBase: '1,306,468.13',
                notional: '60,000,000', notionalCcy: 'EUR', cp: 'MERIDIAN CAPITAL' },
            { id: '9c4b6e02-5a17-4d83-bf92-7e0a2c8d1546', type: 'Bond', maturity: '2040-07-01',
                netting: 'NS-ACMCOR-001', ccy: 'GBP', npv: '884,204.18', npvBase: '1,043,812.44',
                notional: '44,000,000', notionalCcy: 'GBP', cp: 'HANSA PFANDBRIEF' },
            { id: '07d5a8f3-6c29-4e14-8b35-1f9d2a7c0e61', type: 'Bond', maturity: '2035-09-20',
                netting: 'NS-ACMCOR-004', ccy: 'USD', npv: '612,402.00', npvBase: '612,402.00',
                notional: '32,000,000', notionalCcy: 'USD', cp: 'PACIFIC TRUST' },
            { id: '5f8e3b17-9d02-4a68-9c41-6b3e0d2a8754', type: 'Bond', maturity: '2029-04-11',
                netting: 'NS-ACMCOR-002', ccy: 'GBP', npv: '-204,118.40', npvBase: '-241,062.04',
                notional: '28,000,000', notionalCcy: 'GBP', cp: 'KESTREL ASSURANCE' }
        ],
        /* Large Exposures, ACCOHK, 2,292 rows in npv.csv. */
        '9a20c7d1-4f68-4e93-b7c5-1d8e6f2a4b03': [
            { id: 'd41a0f86-3b97-4c52-8e10-7a5d2c9b6f35', type: 'Bond', maturity: '2036-06-30',
                netting: 'NS-ACCOHK-001', ccy: 'HKD', npv: '8,204,118.00', npvBase: '1,051,810.00',
                notional: '64,000,000', notionalCcy: 'HKD', cp: 'ACME BANK PLC' },
            { id: '2b7e5a09-8c14-4d37-9f62-0e8b3a1d5c74', type: 'Bond', maturity: '2034-12-15',
                netting: 'NS-ACCOHK-001', ccy: 'HKD', npv: '6,118,402.00', npvBase: '784,410.51',
                notional: '48,000,000', notionalCcy: 'HKD', cp: 'NORTHGATE CLEARING' },
            { id: '6c9d2e48-1a75-4b03-8e29-5f7c0b6a3d81', type: 'Bond', maturity: '2033-03-31',
                netting: 'NS-ACCOHK-002', ccy: 'USD', npv: '2,884,020.00', npvBase: '2,884,020.00',
                notional: '36,000,000', notionalCcy: 'USD', cp: 'MERIDIAN CAPITAL' },
            { id: '84f1b6c3-7e20-4a95-9d18-3c5b2f0e7a69', type: 'Bond', maturity: '2038-08-20',
                netting: 'NS-ACCOHK-002', ccy: 'USD', npv: '1,904,118.40', npvBase: '1,904,118.40',
                notional: '24,000,000', notionalCcy: 'USD', cp: 'PACIFIC TRUST' },
            { id: 'a09c3f57-4b18-4e26-8c93-1d6f5a2b0e74', type: 'Bond', maturity: '2031-10-05',
                netting: 'NS-ACCOHK-003', ccy: 'USD', npv: '1,204,118.00', npvBase: '1,204,118.00',
                notional: '18,000,000', notionalCcy: 'USD', cp: 'HANSA PFANDBRIEF' },
            { id: '5e3a7d90-2c64-4b18-af07-9e1d6b8c3a52', type: 'Bond', maturity: '2027-09-14',
                netting: 'NS-ACCOHK-003', ccy: 'USD', npv: '-412,884.20', npvBase: '-412,884.20',
                notional: '20,000,000', notionalCcy: 'USD', cp: 'KESTREL ASSURANCE' }
        ]
    };

    /* The six columns the drill-down draws. The header labels are npv.csv's own,
       because a renamed column would hide the shape of what a run writes. */
    var TRADE_COLUMNS = ['TradeId', 'TradeType', 'NPV', 'NPV(Base)', 'Notional', 'CounterParty'];

    /* The book tree: the third reading of a run's result, beside the aggregate
       figures and the one-row-per-trade table. Positions roll up from a trade to
       its book, from a book to its portfolio, and from a portfolio to the group
       portfolio above it.

       The hierarchy is the live schema's. ores_refdata_portfolios_tbl carries
       parent_portfolio_id, so portfolios nest. ores_refdata_books_tbl carries
       parent_portfolio_id, so a book hangs off a portfolio.
       ores_trading_trade_bookings_tbl carries trade_id and book_id, so a trade is
       booked to a book.

       Every number below is a fixture, and the shape of the fixture is the point.
       The run's archive carries no book column: npv.csv's columns are exactly
       TradeId, TradeType, Maturity, MaturityTime, NPV, NpvCurrency, NPV(Base),
       BaseCurrency, Notional, NotionalCurrency, Notional(Base), NettingSet and
       CounterParty. Attributing a result to a book needs the join from TradeId to
       trade_bookings.book_id through trading.v1.trade_bookings.list, and no
       operation returns results already grouped by book. The portfolio side has
       its own gap: there is no portfolios.list_by_parent_portfolio_id, so a client
       assembles the portfolio tree from portfolios.list, which carries
       parent_portfolio_id.

       A portfolio node writes no trades and no npv of its own: buildNode sums both
       from its children, so a parent that disagrees with its children cannot be
       drawn. A book's trades is its whole population in npv.csv and its npv is the
       sum of those trades' NPV. rows names the fixture rows this page renders that
       are booked to that book; they are a sample of the book, never the book. A
       trade booked to no book is a real state, so Unbooked is a node like any
       other and is drawn rather than dropped. Book and portfolio names are free
       text in the seed, and these are ACME's. */
    var BOOK_TREES = {
        '3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3': {
            name: 'Group Treasury Portfolio',
            children: [
                {
                    name: 'Trading Portfolio',
                    children: [
                        {
                            name: 'Rates Portfolio',
                            children: [
                                { name: 'GBP Rates', trades: 4218, npv: 41204118, delta: 612404, gamma: 18204,
                                    rows: ['c9d2b571-3a84-4e16-8f57-1b9c6d0e2384',
                                        '8d5b0e74-2c69-4a13-9e88-5f1b7c3d6a92'] },
                                { name: 'EUR Rates', trades: 3904, npv: 28118402, delta: 488120, gamma: 14118,
                                    rows: [] },
                                { name: 'USD Rates', trades: 5128, npv: 12884020, delta: 412884, gamma: 9204,
                                    rows: [] }
                            ]
                        },
                        {
                            name: 'Markets Portfolio',
                            children: [
                                { name: 'FX Options', trades: 1820, npv: 4204880, delta: 168204, gamma: 6118,
                                    rows: [] },
                                { name: 'Inflation', trades: 1104, npv: 1118204, delta: 94118, gamma: 2884,
                                    rows: [] },
                                { name: 'Equity', trades: 610, npv: 260088, delta: 12884, gamma: 402,
                                    rows: [] }
                            ]
                        }
                    ]
                },
                { name: 'Liquidity Buffer Bonds', trades: 610, npv: 231118, delta: 24000, gamma: 500,
                    rows: ['2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05',
                        '5a17c8e2-9d3b-4f60-8c21-7e4b0a9d2f18',
                        '7b3e28d4-1c95-4a72-9f03-6d8e5b1a4c27',
                        '0e4f9a63-8b17-4d25-a3c9-2f7b1e8d5a04'] },
                { name: 'Group FX Hedges', trades: 502, npv: 248402, delta: 18000, gamma: 470, rows: [] },
                { name: 'Group Intercompany Loans', trades: 280, npv: 118204, delta: 14000, gamma: 400,
                    rows: [] },
                { name: 'Unbooked', kind: 'unbooked', trades: 28, npv: 14678, delta: 2204, gamma: 50,
                    rows: ['4f1a6c38-7e2b-4d91-b605-9c3a8e2f7d15',
                        '1b7c4f20-6a39-4e58-9d02-8c6f2b5a7e31'] }
            ]
        },
        '4b7e1a02-8d61-4f3c-b2a7-9c1e5d0f7a63': {
            name: 'Group Treasury Portfolio',
            children: [
                {
                    name: 'Group Investment Portfolio',
                    children: [
                        { name: 'Liquidity Buffer Bonds', trades: 3846, npv: 4118204,
                            rows: ['6a2d9f41-7b58-4c03-9e17-2d4a8b6f0c93',
                                'b73f0c85-4e19-4a26-8d70-5c1e9b3a7f24'] },
                        { name: 'Sovereign Ladder', trades: 2358, npv: 3085914,
                            rows: ['3e9a1d67-2f84-4b50-a6c3-9d7b1e0c5a48',
                                '9c4b6e02-5a17-4d83-bf92-7e0a2c8d1546'] }
                    ]
                },
                {
                    name: 'Group Trading Portfolio',
                    children: [
                        { name: 'Group FX Hedges', trades: 2804, npv: 3118402, rows: [] },
                        { name: 'Rates Swaps', trades: 1708, npv: 2299802, rows: [] }
                    ]
                },
                { name: 'Group Intercompany Loans', trades: 540, npv: 248118, rows: [] },
                { name: 'Unbooked', kind: 'unbooked', trades: 30, npv: 13662,
                    rows: ['07d5a8f3-6c29-4e14-8b35-1f9d2a7c0e61',
                        '5f8e3b17-9d02-4a68-9c41-6b3e0d2a8754'] }
            ]
        },
        '9a20c7d1-4f68-4e93-b7c5-1d8e6f2a4b03': {
            name: 'Group Treasury Portfolio',
            children: [
                {
                    name: 'Asia Pacific Portfolio',
                    children: [
                        { name: 'Liquidity Buffer Bonds', trades: 1104, npv: 204118402,
                            rows: ['d41a0f86-3b97-4c52-8e10-7a5d2c9b6f35',
                                '2b7e5a09-8c14-4d37-9f62-0e8b3a1d5c74',
                                '6c9d2e48-1a75-4b03-8e29-5f7c0b6a3d81'] },
                        { name: 'Group FX Hedges', trades: 604, npv: 118204118,
                            rows: ['84f1b6c3-7e20-4a95-9d18-3c5b2f0e7a69'] },
                        { name: 'Group Intercompany Loans', trades: 556, npv: 61402118, rows: [] }
                    ]
                },
                { name: 'Unbooked', kind: 'unbooked', trades: 28, npv: 128440,
                    rows: ['a09c3f57-4b18-4e26-8c93-1d6f5a2b0e74',
                        '5e3a7d90-2c64-4b18-af07-9e1d6b8c3a52'] }
            ]
        }
    };

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
            title: 'No operation parses the archive',
            body: 'Per-trade results are real: npv.csv holds one row per trade with its NPV, its base-currency NPV, its notional and its counterparty, and the archive at the run\u2019s output_storage_key holds that file. What is missing is the read. ores_compute_results_tbl carries the archive\u2019s output_uri, an outcome and an error_message, and no parsed rows. Nothing in the platform parses the archive, so A\u2019s figures, A\u2019s aggregate table, the per-trade table and C\u2019s numbers have no read behind them. The result store that would parse it is designed in Report post-processing and not built, so a screen needs a result store or an archive read before it can serve a trade row. Which figures a run has is decided by the analytics the configuration names \u2014 npv and sensitivity write NPV, Delta and Gamma; saccr writes EAD \u2014 and that list is a fixture here too, because no operation returns the configuration either.'
        },
        {
            title: 'The archive carries no book column',
            body: 'npv.csv\u2019s columns are exactly TradeId, TradeType, Maturity, MaturityTime, NPV, NpvCurrency, NPV(Base), BaseCurrency, Notional, NotionalCurrency, Notional(Base), NettingSet and CounterParty. None of them names a book, and no operation returns results already grouped by book. A built screen needs the join from TradeId to trade_bookings.book_id through trading.v1.trade_bookings.list, and then the book to its portfolio through ores_refdata_books_tbl.parent_portfolio_id. The portfolio tree has its own gap: there is no portfolios.list_by_parent_portfolio_id, so a client assembles the tree from portfolios.list, which carries parent_portfolio_id. Every node figure in A, B and C\u2019s tree is a fixture, because the archive is still unparsed.'
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
        A: 'A needs the result store. Every figure, every row of the aggregate table, every node of the book tree and every row of the per-trade table is a reading of the archive the two configured analytics wrote, and nothing parses an archive: post-processing is designed and not built. The tree needs one read more than the table: the archive has no book column, so a node is only reachable by joining each TradeId to trade_bookings.book_id through trading.v1.trade_bookings.list. Built with today\u2019s operations, A is the archive footer with an empty screen above it.',
        B: 'B is the only variant buildable from today\u2019s reads, and only down to the run: the state, the output message, the storage key and the errors are all fields of the run. What B cannot have is its own centre \u2014 the manifest, the per-trade rows behind npv.csv, the book tree\u2019s grouping and any download \u2014 because no operation lists, parses or serves the archive, and none groups a result by book. Its file list is inferred from the configured analytics, not observed inside the tar.',
        C: 'C needs what A needs \u2014 the parsed numbers, the trade rows and the book grouping \u2014 and then a writer trusted to keep a figure and its caveat in the same sentence. It is the easiest of the three to make wrong and the hardest to check: a column can be inspected, a sentence is believed.'
    };

    var S = {
        variant: 'A', runId: DEFAULT_RUN, file: 'npv.csv', trade: null, showErrors: false, note: null,
        book: null, collapsed: []
    };

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

    /* -------------------------------------------------------- per trade */

    function tradesFor(run) {
        return TRADES[run.id] || [];
    }

    function tradeById(run, id) {
        return tradesFor(run).filter(function (t) { return t.id === id; })[0];
    }

    /* A trade error is a report error whose subject is the trade: the model
       already keys those by subject_id, so the row and the error join on the
       TradeId and nothing is invented to connect them. */
    function tradeErrors(run, id) {
        return (run.errors || []).filter(function (e) {
            return e.subject === 'trade' && e.subjectId === id;
        });
    }

    function npvRows(run) {
        var f = filesFor(run).filter(function (x) { return x.name === 'npv.csv'; })[0];
        return f ? f.rows : 0;
    }

    /* ---------------------------------------------------------- book tree */

    var TREE_CACHE = {};

    function bookTree(run) {
        var spec = BOOK_TREES[run.id];
        if (!spec) return null;
        if (!TREE_CACHE[run.id]) TREE_CACHE[run.id] = buildNode(spec, spec.name);
        return TREE_CACHE[run.id];
    }

    /* A portfolio's trade count and NPV are the sum of its children's, computed
       here and never written down, so the figure a reader sees at a parent is the
       one its children add up to. */
    function buildNode(spec, id) {
        var node = {
            id: id,
            name: spec.name,
            kind: spec.kind || (spec.children && spec.children.length ? 'portfolio' : 'book'),
            children: [],
            rows: spec.rows || [],
            trades: spec.trades || 0,
            npv: spec.npv || 0
        };
        if (spec.delta !== undefined) node.delta = spec.delta;
        if (spec.gamma !== undefined) node.gamma = spec.gamma;
        if (spec.children && spec.children.length) {
            node.children = spec.children.map(function (child) {
                return buildNode(child, id + ' / ' + child.name);
            });
            node.trades = 0;
            node.npv = 0;
            node.rows = [];
            node.children.forEach(function (child) {
                node.trades += child.trades;
                node.npv += child.npv;
                node.rows = node.rows.concat(child.rows);
            });
        }
        return node;
    }

    function findNode(node, id) {
        if (!node) return null;
        if (node.id === id) return node;
        for (var i = 0; i < node.children.length; i += 1) {
            var found = findNode(node.children[i], id);
            if (found) return found;
        }
        return null;
    }

    /* The books and unbooked nodes under a node, in tree order. */
    function bookLeaves(node) {
        if (!node) return [];
        if (!node.children.length) return [node];
        return node.children.reduce(function (all, child) {
            return all.concat(bookLeaves(child));
        }, []);
    }

    function selectedBookNode(run) {
        var root = bookTree(run);
        if (!root) return null;
        return findNode(root, S.book) || root;
    }

    function treeIsRoot(run) {
        var root = bookTree(run);
        return !root || S.book === null || root.id === S.book;
    }

    /* The rendered trade rows under the selected node. At the root that is every
       rendered row; at a book it is the rows booked to it. */
    function tradesInView(run) {
        var trades = tradesFor(run);
        var node = selectedBookNode(run);
        if (!node || treeIsRoot(run)) return trades;
        return trades.filter(function (t) { return node.rows.indexOf(t.id) !== -1; });
    }

    function money(n) {
        if (n === null || n === undefined) return '\u2014';
        return (n < 0 ? '-' : '') + 'USD ' + num(Math.abs(n));
    }

    function share(part, whole) {
        if (!whole) return '0.0%';
        return (part / whole * 100).toFixed(1) + '%';
    }

    function isCollapsed(id) {
        return S.collapsed.indexOf(id) !== -1;
    }

    function treeRows(node, depth) {
        var rows = [];
        var hasChildren = node.children.length > 0;
        var collapsed = isCollapsed(node.id);
        var kind = node.kind === 'unbooked' ? 'no booking' : node.kind;
        rows.push('<div class="bnode ' + esc(node.kind) + (S.book === node.id ? ' sel' : '') +
            '" data-act="book" data-book="' + esc(node.id) + '" style="padding-left:' + (10 + depth * 18) + 'px">' +
            (hasChildren
                ? '<span class="twisty" data-act="twisty" data-book="' + esc(node.id) + '" title="' +
                    (collapsed ? 'Expand' : 'Collapse') + '">' + (collapsed ? '\u25b8' : '\u25be') + '</span>'
                : '<span class="twisty pad"></span>') +
            '<span class="bname">' + esc(node.name) + '</span>' +
            '<span class="bkind">' + esc(kind) + '</span>' +
            '<span class="btrades">' + num(node.trades) + '</span>' +
            '<span class="bnpv">' + esc(money(node.npv)) + '</span>' +
            '</div>');
        if (hasChildren && !collapsed) {
            node.children.forEach(function (child) {
                rows = rows.concat(treeRows(child, depth + 1));
            });
        }
        return rows;
    }

    function treeTable(run) {
        var root = bookTree(run);
        if (!root) return '';
        return '<div class="booktree">' +
            '<div class="bhead" aria-hidden="true">' +
            '<span class="twisty pad"></span>' +
            '<span class="bname">Portfolio, book or trade</span>' +
            '<span class="bkind">Level</span>' +
            '<span class="btrades">Trades</span>' +
            '<span class="bnpv">Aggregate NPV</span>' +
            '</div>' + treeRows(root, 0).join('') + '</div>';
    }

    /* The tree's own notice: the label that says fixture, and the two reads that
       do not exist. Both are said here, on the panel that draws the tree. */
    function bookTreeNotice() {
        return '<div class="notice warn"><strong>The archive carries no book column.</strong> ' +
            'npv.csv\u2019s columns are exactly <span class="mono">TradeId, TradeType, Maturity, ' +
            'MaturityTime, NPV, NpvCurrency, NPV(Base), BaseCurrency, Notional, NotionalCurrency, ' +
            'Notional(Base), NettingSet, CounterParty</span>, so no row of it names a book. A built screen ' +
            'needs the join from <span class="mono">TradeId</span> to ' +
            '<span class="mono">trade_bookings.book_id</span> through ' +
            '<span class="mono">trading.v1.trade_bookings.list</span>, and no operation returns results ' +
            'already grouped by book. This tree is a fixture.</div>';
    }

    function bookTreeSummary(run) {
        var root = bookTree(run);
        var node = selectedBookNode(run);
        if (!root || !node) return '';
        return '<span class="tag warn">fixture</span>' +
            '<span class="tag accent">selected: ' + esc(node.name) + '</span>' +
            '<span>' + num(node.trades) + ' trades \u00b7 ' + esc(money(node.npv)) +
            ' \u00b7 ' + share(node.npv, root.npv) + ' of the run\u2019s NPV \u00b7 ' +
            bookLeaves(node).length + ' book' + (bookLeaves(node).length === 1 ? '' : 's') + ' in this subtree</span>';
    }

    function bookTreeFooter(run) {
        var root = bookTree(run);
        var leaves = bookLeaves(root);
        var unbooked = leaves.filter(function (b) { return b.kind === 'unbooked'; })[0];
        var all = tradesFor(run).length;
        var engineBooks = (run.figures || []).filter(function (f) {
            return f.label === 'books';
        })[0];
        return '<p class="rowfoot">Portfolios nest, a book hangs off a portfolio, and a trade is booked to a ' +
            'book. A portfolio\u2019s figure is the sum of its books and a book\u2019s is the sum of its trades, both ' +
            'rolled up on this page and neither written down. The tree sums the run\u2019s whole population of ' +
            num(root.trades) + ' trades; the table below draws ' + all + ' of those rows, so the tree does not ' +
            'sum them.' + (engineBooks
                ? ' The run\u2019s own figure counts ' + esc(engineBooks.value) + ' books, and this fixture groups the ' +
                  'same trades into ' + leaves.length + ' book nodes, unbooked among them. Every trade sits in one ' +
                  'node, so the root still adds up to the run.'
                : '') + (unbooked
                ? ' ' + num(unbooked.trades) + ' trades are booked to no book. That is a real state, so ' +
                  '<span class="mono">Unbooked</span> is a node like any other and it is drawn, not dropped: a ' +
                  'built screen gets the same node from the trades whose TradeId has no ' +
                  '<span class="mono">trade_bookings</span> row.'
                : '') +
            ' There is also no <span class="mono">portfolios.list_by_parent_portfolio_id</span>, so a client ' +
            'assembles the portfolio tree from <span class="mono">portfolios.list</span>, which carries ' +
            '<span class="mono">parent_portfolio_id</span>.</p>';
    }

    function bookTreeHead(run, tier) {
        var title = 'By book: portfolio, sub-portfolio, book, trade';
        if (tier === 'h3') {
            return '<div class="sectionhead" style="margin-top:14px"><h3 class="subhead">' + title + '</h3>' +
                '<span class="meta">' + bookTreeSummary(run) + '</span></div>';
        }
        return '<header class="sectionhead"><h2>' + title + '</h2>' +
            '<span class="meta">' + bookTreeSummary(run) + '</span></header>';
    }

    function bookTreeBody(run) {
        return bookTreeNotice() + '<div style="margin-top:12px">' + treeTable(run) + '</div>' +
            bookTreeFooter(run);
    }

    /* A draws the tree inside the result card, above the per-trade table; B and C
       give it a card of its own. */
    function bookTreeBlock(run) {
        if (!bookTree(run)) return '';
        return '<div id="booktree">' + bookTreeHead(run, 'h3') + bookTreeBody(run) + '</div>';
    }

    function bookTreeCard(run, blurb) {
        if (!bookTree(run)) return '';
        return '<section class="card" id="booktree">' + bookTreeHead(run, 'h2') +
            (blurb ? '<p class="hint" style="margin-top:8px">' + esc(blurb) + '</p>' : '') +
            bookTreeBody(run) +
            '</section>';
    }

    /* The aggregate tile set. At the root it is the run's figures; below it, the
       figures are the selected node's, read off the tree the screen just drew. */
    function tilesFor(run) {
        var root = bookTree(run);
        var node = selectedBookNode(run);
        if (!root || !node || treeIsRoot(run)) return run.figures;
        var leaves = bookLeaves(node);
        return [
            { value: money(node.npv), label: 'NPV', note: 'the selected node, rolled up from its children' },
            { value: num(node.trades), label: 'trades priced',
                note: 'of the ' + num(root.trades) + ' this run priced' },
            { value: share(node.npv, root.npv), label: 'of the run\u2019s NPV',
                note: 'the selected node against ' + root.name },
            { value: String(leaves.length), label: leaves.length === 1 ? 'book in this subtree' : 'books in this subtree',
                note: 'each book\u2019s figure is the sum of its trades' }
        ];
    }

    /* The by-book table is the tree's books, in tree order, so the aggregate
       table and the tree cannot disagree about a book's name or its figures. */
    function bookTableData(run) {
        var root = bookTree(run);
        if (!root) return run.table;
        var leaves = bookLeaves(root);
        var complete = leaves.every(function (b) {
            return b.delta !== undefined && b.gamma !== undefined;
        });
        if (!complete || !run.table) return run.table;
        return {
            title: run.table.title,
            note: run.table.note,
            columns: run.table.columns,
            rows: leaves.map(function (b) {
                return [b.name, num(b.trades), num(b.npv), num(b.delta), num(b.gamma)];
            })
        };
    }

    function selectedTrade(run) {
        return S.trade ? tradeById(run, S.trade) : null;
    }

    function tradeTable(run) {
        var trades = tradesInView(run);
        if (!trades.length) return '';
        var head = TRADE_COLUMNS.map(function (c, i) {
            return '<th' + (i > 2 && i < 5 ? ' class="num"' : '') + '>' + esc(c) + '</th>';
        }).join('');
        function price(v) {
            return v === null ? '<span class="faint">\u2014</span>' : esc(v);
        }
        var body = trades.map(function (t) {
            var errs = tradeErrors(run, t.id);
            return '<tr class="trade-row' + (S.trade === t.id ? ' sel' : '') + '" data-act="trade" data-trade="' +
                esc(t.id) + '">' +
                '<td><span class="nm">' + esc(t.id) + '</span>' +
                    (errs.length ? ' <span class="tag bad">error</span>' : '') + '</td>' +
                '<td><span class="kind">' + esc(t.type) + '</span></td>' +
                '<td class="num">' + price(t.npv) + '</td>' +
                '<td class="num">' + price(t.npvBase) + '</td>' +
                '<td class="num">' + esc(t.notional) + '</td>' +
                '<td class="cp">' + esc(t.cp) + '</td>' +
                '</tr>';
        }).join('');
        return '<div class="table-wrap"><table class="manifest tradetable"><thead><tr>' + head +
            '</tr></thead><tbody>' + body + '</tbody></table></div>';
    }

    /* The tree's book for a rendered row: the join this page cannot make from
       the archive, so it reads the fixture booking instead. */
    function bookOfTrade(run, tradeId) {
        var root = bookTree(run);
        if (!root) return null;
        var found = bookLeaves(root).filter(function (b) {
            return b.rows.indexOf(tradeId) !== -1;
        })[0];
        return found || null;
    }

    function tradeNotice(run) {
        var all = tradesFor(run).length;
        var view = tradesInView(run).length;
        var node = selectedBookNode(run);
        var scope = (!node || treeIsRoot(run))
            ? ''
            : ' The tree has narrowed it to <span class="mono">' + esc(node.name) +
              '</span>, which holds ' + num(node.trades) + ' trades.';
        return '<div class="notice warn"><strong>No operation parses the archive.</strong> ' +
            'This per-trade table is a fixture, and the columns are npv.csv\u2019s own: ' +
            '<span class="mono">TradeId, TradeType, NPV, NPV(Base), Notional, CounterParty</span>. ' +
            'Per-trade results are real \u2014 a run writes one row per trade into npv.csv inside its archive \u2014 ' +
            'but <span class="mono">ores_compute_results_tbl</span> carries only the archive\u2019s location and an ' +
            'outcome, and no operation parses the archive. A built screen cannot serve these rows until a result ' +
            'store or an archive read exists. It shows ' + view + ' of the ' + all + ' rows this page draws, ' +
            'and this page draws ' + all + ' of the ' + num(npvRows(run)) + ' rows npv.csv carries.' + scope + '</div>';
    }

    /* Selecting a book with no rendered row is an answer, not an empty table: the
       rows this page draws are a sample, and the book's own population is larger. */
    function tradeEmpty(run) {
        var node = selectedBookNode(run);
        if (!node || treeIsRoot(run)) return '';
        return '<p class="emptyline">None of the ' + tradesFor(run).length + ' rows this page draws is booked to ' +
            '<span class="mono">' + esc(node.name) + '</span>. The book holds ' + num(node.trades) +
            ' trades in npv.csv; the tree\u2019s figure for it is their sum, and the table\u2019s sample is not.</p>';
    }

    /* Opening a row answers with the trade's own result and, where the run
       recorded one against this TradeId, the error. The id is the trading
       trade's id, so the row leads to the trade. */
    function tradeDetail(run) {
        var t = selectedTrade(run);
        if (!t) {
            return '<p class="rowfoot">Select a row to read that trade\u2019s result: its NPV, its base-currency ' +
                'NPV, its notional, its counterparty, and any error the run recorded against it.</p>';
        }
        var errs = tradeErrors(run, t.id);
        var book = bookOfTrade(run, t.id);
        var fields = [
            ['TradeType', t.type],
            ['Maturity', t.maturity],
            ['NettingSet', t.netting],
            ['CounterParty', t.cp],
            ['Book', book ? book.name : '\u2014 no booking'],
            ['NPV', t.npv === null ? '\u2014 no price' : t.npv + ' ' + t.ccy],
            ['NPV(Base)', t.npvBase === null ? '\u2014 no price' : t.npvBase + ' USD'],
            ['Notional', t.notional + ' ' + t.notionalCcy],
            ['BaseCurrency', 'USD']
        ];
        var grid = fields.map(function (pair) {
            return '<div><span class="lbl">' + esc(pair[0]) + '</span>' +
                '<span class="val mono">' + esc(pair[1]) + '</span></div>';
        }).join('');

        var verdict = t.npv === null
            ? '<p class="dim">npv.csv carries no row for this bond: the engine could not build it, so it has no ' +
              'NPV and no base-currency NPV. The error the run recorded against it is the answer, and it is below.</p>'
            : '<p class="dim">npv priced this bond' + (errs.length ? ', and the run still recorded a problem against it' : '') +
              '. The trade behind the row is <span class="mono">trading</span>\u2019s trade with this id, and ' +
              '<span class="mono">trading.v1.trades.get</span> is what resolves it.</p>';

        return '<div class="tradedetail">' +
            '<div class="sectionhead" style="margin-top:16px"><h3 class="subhead">Trade ' +
            esc(t.id) + '</h3><span class="meta">' +
            (errs.length ? '<span class="tag bad">' + errs.length + ' error row' + (errs.length === 1 ? '' : 's') + '</span>'
                : '<span class="tag ok">no error recorded</span>') +
            '<span>joined to the run on TradeId</span></span></div>' +
            verdict +
            '<div class="statgrid" style="margin-top:14px">' + grid + '</div>' +
            (errs.length ? '<div style="margin-top:14px">' + tradeErrorTable(errs) + '</div>' : '') +
            '<div class="keyline" style="margin-top:14px">' +
            '<button class="btn small" data-act="open-trade">Open the trade</button>' +
            '<span class="hint">trading.v1.trades.get exists and resolves this id to the trade itself; no screen ' +
            'route calls it, and this prototype reads no server, so the control is drawn.</span>' +
            '</div>' +
            '</div>';
    }

    function tradeErrorTable(errors) {
        return '<div class="table-wrap"><table class="errtable"><thead><tr>' +
            '<th>Severity</th><th>Scope</th><th>Subject</th><th>Message</th>' +
            '</tr></thead><tbody>' + errorRows(errors) + '</tbody></table></div>';
    }

    function tradeFootnote(run) {
        var failed = tradesFor(run).filter(function (t) { return t.npv === null; }).length;
        return '<p class="rowfoot">Six of the thirteen columns npv.csv carries. A trade the engine could not build ' +
            'writes no price into the file, so ' + (failed
                ? 'the ' + failed + ' row with an empty NPV is drawn from the run\u2019s trade-scoped error rather ' +
                  'than from npv.csv, and the error is the answer for it. '
                : 'a failed trade would appear here with an empty NPV and its error. ') +
            'The trade id is the trading trade\u2019s id, so a row leads to the trade: ' +
            '<span class="mono">trading.v1.trades.get</span> is the read that resolves it. A read of one trade ' +
            'exists today; a read of a run\u2019s per-trade results does not.</p>';
    }

    function tradesBlock(run) {
        return tradeNotice(run) + tradeTable(run) + tradeEmpty(run) + tradeDetail(run) + tradeFootnote(run);
    }

    /* The card B and C give the per-trade table. A draws the same block inside
       its result card, under the aggregate figures. */
    function tradesCard(run) {
        var view = tradesInView(run).length;
        var all = tradesFor(run).length;
        return '<section class="card" id="trades">' +
            '<header class="sectionhead"><h2>One row per trade</h2>' +
            '<span class="meta"><span class="tag warn">fixture</span>' + view + ' of ' + all +
            ' drawn rows \u00b7 ' + all + ' of ' + num(npvRows(run)) + ' rows in npv.csv \u00b7 one row per trade ' +
            '\u00b7 select a row</span></header>' +
            tradesBlock(run) +
            '</section>';
    }

    /* The heading A and C put above the block, and the count B puts in its
       preview: one row per trade, drawn from a file no operation parses. */
    function tradesHead(run) {
        var view = tradesInView(run).length;
        var all = tradesFor(run).length;
        var node = selectedBookNode(run);
        var scope = (!node || treeIsRoot(run)) ? 'all books' : esc(node.name);
        return '<div class="sectionhead" style="margin-top:20px"><h3 class="subhead">One row per trade</h3>' +
            '<span class="meta"><span class="tag warn">fixture</span>' +
            '<span>npv.csv, one row per trade \u00b7 ' + view + ' of ' + all + ' drawn rows \u00b7 ' + scope +
            ' \u00b7 select a row to read the trade</span></span></div>';
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
            'Every figure, file name, row count, size, trade row and error below is mock, and nothing on this page ' +
            'reads a server. ' +
            'The figures and the per-trade rows are fixtures standing in for the output of the analytics this run\u2019s ' +
            'configuration names \u2014 <span class="mono">' + esc(analyticsText(run)) + '</span> \u2014 and nothing ' +
            'parses the archive they were written into: ores_compute_results_tbl holds the archive\u2019s location and ' +
            'an outcome, not parsed rows. ' +
            'There is no operation that lists the files inside a run\u2019s archive, so no file list can be served \u2014 ' +
            'variant B draws one as a fixture and says so on it. No operation returns the archive or the ORE package, ' +
            'so every download is drawn, not wired. ' +
            'The book tree is a fixture for one reason more: the archive carries no book column, so a built screen ' +
            'joins each TradeId to trade_bookings.book_id through trading.v1.trade_bookings.list.' +
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

    /* One row per recorded error, so the run's error list and a single trade's
       error list are the same row drawn in the same shape. */
    function errorRows(errors) {
        return errors.map(function (e) {
            var subject = e.subject ? esc(e.subject) + ' ' + esc(e.subjectId) : 'the run';
            return '<tr>' +
                '<td class="sev"><span class="tag ' + (e.severity === 'error' ? 'bad' : 'warn') + '">' +
                    esc(e.severity) + '</span></td>' +
                '<td class="code">' + esc(e.scope) + '</td>' +
                '<td class="subj" title="' + subject + '">' + subject + '</td>' +
                '<td class="msg">' + esc(e.message) + '</td>' +
                '</tr>';
        }).join('');
    }

    function errorTable(run) {
        return '<div class="table-wrap"><table class="errtable"><thead><tr>' +
            '<th>Severity</th><th>Scope</th><th>Subject</th><th>Message</th>' +
            '</tr></thead><tbody>' + errorRows(run.errors) + '</tbody></table></div>';
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

    /* A treats the figures, the book tree and the tables as one thing: the run's
       result is a set of numbers, the tree is how they roll up from a trade to a
       book to a portfolio, and the per-trade table under it is the same result one
       trade at a time. Selecting a node changes the figures and the rows together. */
    function resultCard(run) {
        var tiles = tilesFor(run).map(function (f) {
            return '<div class="fig"><div class="v">' + esc(f.value) + '</div>' +
                '<div class="l">' + esc(f.label) + '</div>' +
                '<div class="n">' + esc(f.note) + '</div></div>';
        }).join('');

        var t = bookTableData(run);
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
            '<div class="figs" style="margin-top:16px">' + tiles + '</div>' +
            bookTreeBlock(run) + table +
            (tradesFor(run).length ? tradesHead(run) + tradesBlock(run) : '') +
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

    /* npv.csv is the one file in the manifest whose rows are trades. B argues
       that the file is the unit, so selecting it offers the trade table rather
       than a handful of preview lines; the table needs six columns, so it draws
       full width below the split. */
    function previewPanel(run) {
        var f = selectedFile(run);
        var body;
        if (f.name === 'npv.csv' && tradesFor(run).length) {
            body = '<p class="dim">npv.csv is not a handful of rows to preview: a run writes one row per trade into ' +
                'it, with the trade\u2019s own NPV, its base-currency NPV, its notional and its counterparty. ' +
                'B answers with the trade table, full width below this panel, because six columns do not fit a ' +
                'preview column.</p>';
        } else if (f.preview.lines) {
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
        var isTrades = f.name === 'npv.csv' && tradesFor(run).length;
        var footer = isTrades
            ? 'The trade rows are a fixture too: no operation parses the archive, so nothing here read npv.csv.'
            : (f.kind === 'log'
                ? 'The last lines of a text log. This preview is fixture like the manifest: nothing read the file.'
                : (f.preview.lines
                    ? 'The first ' + f.preview.lines.length + ' structured-log lines, which are the run\u2019s errors. Preview is fixture.'
                    : 'First ' + f.preview.sample.length + ' of ' + num(f.rows) + ' rows. The preview is fixture; nothing reads the CSV.'));

        return '<section class="card preview">' +
            '<header class="sectionhead"><h2>Preview</h2>' +
            '<span class="meta">' + (isTrades ? 'one row per trade' : 'the numbers, one file at a time') + '</span></header>' +
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
            bookTreeCard(run, 'The tree is the lens over npv.csv: choose a book and the rows read out of it below narrow ' +
                'to that subtree, while the manifest above stays the whole archive.') +
            '<div class="split">' + manifestCard(run) + previewPanel(run) + '</div>' +
            (selectedFile(run).name === 'npv.csv' && tradesFor(run).length ? tradesCard(run) : '') +
            packageBlock(run);
    }

    // ------------------------------------------------------------ variant C

    function narrativeFirst(run) {
        var paragraphs = storyFor(run).map(function (p) {
            return '<p>' + p + '</p>';
        }).join('');

        /* C links to the trade rows instead of reproducing them: a reader who
           wants a number in a column follows the sentence down to the table. */
        var trades = tradesFor(run);
        if (trades.length) {
            var carrying = trades.filter(function (t) { return tradeErrors(run, t.id).length; }).length;
            paragraphs += '<p>The figures above are the run\u2019s aggregate. ' +
                '<a href="#trades" data-act="goto-trades">The per-trade table</a> is the same result one row at a ' +
                'time: each trade\u2019s NPV, its base-currency NPV, its notional and its counterparty, the trade id ' +
                'that leads to the trade, and ' + (carrying
                    ? 'the ' + carrying + ' of these rows that carries an error the run recorded against it'
                    : 'no error against any of these rows') +
                '. It is a fixture, because nothing parses the archive.</p>';
        }

        /* C names the largest book in the prose and links into the tree, rather
           than reproducing a hundred rows in a sentence. */
        var tree = bookTree(run);
        if (tree) {
            var largest = bookLeaves(tree).slice().sort(function (a, b) { return b.npv - a.npv; })[0];
            var unbooked = bookLeaves(tree).filter(function (b) { return b.kind === 'unbooked'; })[0];
            paragraphs += '<p>The trades roll up too: each one is booked to a book, a book hangs off a portfolio, ' +
                'and the portfolios nest. The largest book is <span class="fig">' + esc(largest.name) + '</span>, at ' +
                '<span class="fig">' + num(largest.trades) + ' trades</span> and <span class="fig">' +
                money(largest.npv) + '</span>. ' +
                '<a href="#booktree" data-act="goto-booktree">The book tree</a> rolls the run from ' +
                '<span class="fig">' + esc(tree.name) + '</span> down to a book and then to a trade, and a reader ' +
                'who selects a node narrows the rows below to that subtree.' +
                (unbooked
                    ? ' ' + num(unbooked.trades) + ' of the run\u2019s trades are booked to no book: they sit in an ' +
                      '<span class="mono">Unbooked</span> node rather than vanishing from the tree.'
                    : '') +
                ' It is a fixture. The archive carries no book column, so the numbers in it are drawn, not read.</p>';
        }

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
           before the run's own account of what went wrong. The tree stands
           beside the prose so the sentence and the roll-up it links to are
           read together. */
        return errorBlock(run) +
            '<div class="split">' +
            '<section class="card">' +
            '<div class="said"><span class="lbl">What the system said</span>' +
            '<span class="q">' + (run.state === 'running' ? '\u2014 no output message yet' : esc(run.outputMessage)) + '</span></div>' +
            analyticsLine(run) +
            '<div class="prose" style="margin-top:18px">' + paragraphs + '</div>' +
            '</section>' +
            bookTreeCard(run, 'The prose names the largest book; this tree is where the run rolls up from a trade to ' +
                'its book, a book to its portfolio, and a portfolio to the group portfolio above it.') +
            '</div>' +
            (trades.length ? tradesCard(run) : '') +
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
        S.trade = null;
        S.note = null;
        render();
        writeParams();
    }

    function setRun(id) {
        var run = runById(id);
        if (!run || run.id === S.runId) return;
        S.runId = run.id;
        S.showErrors = false;
        S.trade = null;
        S.file = 'npv.csv';
        S.note = null;
        S.book = null;
        S.collapsed = [];
        render();
        writeParams();
    }

    /* Selecting a node filters the per-trade table to that subtree and, in A,
       moves the aggregate figures onto it. The trade in view may not be under the
       new node, so the selection closes. */
    function setBook(id) {
        var node = findNode(bookTree(currentRun()), id);
        S.book = node ? node.id : null;
        S.trade = null;
        render();
        writeParams();
    }

    function toggleTree(id) {
        var at = S.collapsed.indexOf(id);
        if (at === -1) S.collapsed.push(id);
        else S.collapsed.splice(at, 1);
        render();
    }

    /* Opening a row is a toggle: the same row closes it again, and the trade it
       names is the one the panel answers with. */
    function setTrade(id) {
        S.trade = S.trade === id ? null : id;
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
            S.trade = null;
            render();
        } else if (act === 'trade') {
            setTrade(el.getAttribute('data-trade'));
        } else if (act === 'book') {
            setBook(el.getAttribute('data-book'));
        } else if (act === 'twisty') {
            toggleTree(el.getAttribute('data-book'));
        } else if (act === 'goto-trades') {
            var trades = document.getElementById('trades');
            if (trades && trades.scrollIntoView) {
                trades.scrollIntoView({ behavior: 'smooth', block: 'start' });
            }
        } else if (act === 'goto-booktree') {
            var tree = document.getElementById('booktree');
            if (tree && tree.scrollIntoView) {
                tree.scrollIntoView({ behavior: 'smooth', block: 'start' });
            }
        } else if (act === 'open-trade') {
            setNote('info', 'Nothing was opened. trading.v1.trades.get resolves a trade by its id, and this page ' +
                'reads no server; the row\u2019s trade id is the trading trade\u2019s id.');
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
        var t = p.get('trade');
        if (t && tradeById(currentRun(), t)) S.trade = t;
        var b = p.get('book');
        if (b && findNode(bookTree(currentRun()), b)) S.book = b;
        if (p.get('errors') === '1') S.showErrors = true;
    }

    function writeParams() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('run', currentRun().id);
        if (S.file !== 'npv.csv') p.set('file', S.file);
        if (S.trade) p.set('trade', S.trade);
        if (S.book) p.set('book', S.book);
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

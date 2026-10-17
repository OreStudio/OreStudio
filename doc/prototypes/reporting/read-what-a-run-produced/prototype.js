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
 *
 * One scenario sits beside the six runs: results-with-errors (?scenario=
 * results-with-errors, or its chip). It takes the run that already carries
 * eleven problems and draws the connection between the errors and the results:
 * a banner above the figures saying the results are partial, a marker on the
 * figures themselves, marked tree nodes for the books that lost trades, and one
 * row per failed trade with no price and its error. A completed run can produce
 * an archive and still not produce the whole answer, and the page must not let a
 * total read as if it did. The errors are the run's own rows, read by
 * reporting.v1.report_error_events.list_by_report_instance_id, which exists; no
 * operation is invented for it.
 *
 * The result panel also compares. A completed run carries a control that picks
 * another completed run of the same definition, started before it, or no run at
 * all; the page defaults to the previous one, so a reviewer meets the comparison
 * without hunting for it, and ?baseline=none turns it off. Each aggregate
 * figure, each book row, each tree node and the total row then carries its
 * change and its percentage against that run, and the panel names the baseline
 * by its instance id and its start time.
 *
 * The comparison is arithmetic on two fixtures, and that is the point of it. The
 * second Headline Position run below is a run of the same definition one day
 * earlier, with its own figures per book, so ten books move by ten different
 * amounts. Nothing here multiplies one run's numbers by a constant to invent a
 * baseline, because a per-metric multiplier makes every book move by the same
 * percentage and the change is then fiction. A stress scenario is not a previous
 * run of this definition, and yesterday's official close belongs to a different
 * run, so neither is offered as a baseline: the only sound comparison is another
 * run of the same definition. A run is named by its instance id and its start
 * time, never by a report number.
 *
 * A change is only as honest as the two numbers under it. When either run in the
 * comparison carries errors the change is partial: a total over the trades that
 * survived cannot be compared as if it were the whole answer. The badge carries
 * the mark and the panel says so once in words, and the results-with-errors
 * scenario shows what the current run's errors leave out. The book tree's
 * selected node drives both sides, so selecting a book compares that book across
 * the two runs.
 *
 * A's result panel also plots. The form of each plot follows the shape of its
 * numbers: NPV and each Greek by book are horizontal bars, sorted, signed and
 * drawn from a zero rule; the change against the baseline is bars of its own,
 * because a change and a level are not one series; and the run's NPV across the
 * runs of the same definition is a line, with the current run and the baseline
 * marked. A run that produced no Greek has no Greek plot, a selection of one
 * book has no bar chart, and a single run has no trend, because a plot of one
 * figure states nothing a sentence cannot. Every plot is inline SVG written in
 * this file: a prototype opens from file:// with no build, no CDN and no
 * network, so no chart library is loaded or copied in. Every plot keeps its
 * exact numbers in a table beside it, and a plot whose figure the errors made
 * partial carries the same partial marking the figure carries.
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
            /* n is the figure the value string prints, as a number. A change is
               subtraction on two such numbers and nothing else: no figure is
               scaled to make a baseline out of it. A figure that has no n is one
               the page will not compare. */
            figures: [
                { value: 'USD 88,402,114', n: 88402114, label: 'NPV', note: 'npv, base currency USD, all books' },
                { value: 'USD 1,846,818', n: 1846818, label: 'Delta', note: 'sensitivity, summed over the books' },
                { value: 'USD 52,350', n: 52350, label: 'Gamma', note: 'sensitivity, summed over the books' },
                { value: '18,204', n: 18204, label: 'positions priced', note: 'the rows npv.csv carries' },
                { value: '10', n: 10, label: 'books', note: 'each trade in exactly one' }
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
        },
        {
            /* The run a completed run is compared against. It is a real fixture
               in the only sense this page has: a second run of the same
               definition, a day earlier, with its own figures. Every number
               below is its own. Nothing is derived from the run of 17 October by
               a multiplier, so the change per book is the difference between two
               fixtures and the books move by different amounts. It completed
               clean, so a comparison against it is not against a partial
               baseline. */
            id: '2a58d0f4-7e31-4b96-8c05-6d1e9a3f2b48',
            short: '2a58d0f4',
            report: 'Headline Position',
            definition: 'Headline Position',
            party: 'ACCOUK',
            kind: 'production',
            reportType: 'risk',
            state: 'completed',
            startedAt: '2026-10-16 09:02:11',
            completedAt: '2026-10-16 09:06:02',
            duration: '3 m 51 s',
            outputMessage: 'Report execution completed.',
            key: 'reporting/runs/2a58d0f4-7e31-4b96-8c05-6d1e9a3f2b48/output.tar.gz',
            preProcessing: 'execute',
            analytics: ['npv', 'sensitivity'],
            files: ['npv.csv', 'sensitivity.csv', 'pricingstats.csv', 'marketdata.csv',
                'todaysmarketcalibration.csv', 'fixings.csv', 'dividends.csv',
                'runtimes.csv', 'log.txt', 'log_structured.json'],
            fileScale: 0.94,
            figures: [
                { value: 'USD 87,219,062', n: 87219062, label: 'NPV', note: 'npv, base currency USD, all books' },
                { value: 'USD 1,818,348', n: 1818348, label: 'Delta', note: 'sensitivity, summed over the books' },
                { value: 'USD 51,510', n: 51510, label: 'Gamma', note: 'sensitivity, summed over the books' },
                { value: '18,162', n: 18162, label: 'positions priced', note: 'the rows npv.csv carries' },
                { value: '10', n: 10, label: 'books', note: 'each trade in exactly one' }
            ],
            table: {
                title: 'NPV, Delta and Gamma by book',
                note: 'A reading of npv.csv and sensitivity.csv, which nothing parses yet. Delta and Gamma are the NPV change for a one basis point shift.',
                columns: ['Book', 'Positions', 'NPV (USD)', 'Delta (USD)', 'Gamma (USD)'],
                rows: [
                    ['GBP Rates', '4,180', '39,804,118', '588,204', '17,412'],
                    ['EUR Rates', '3,960', '28,904,118', '502,118', '14,502'],
                    ['USD Rates', '5,080', '12,104,118', '396,204', '8,804'],
                    ['FX Options', '1,842', '4,302,118', '171,204', '6,204'],
                    ['Inflation', '1,046', '984,118', '88,204', '2,704'],
                    ['Equity', '642', '302,118', '14,884', '468'],
                    ['Liquidity Buffer Bonds', '622', '402,118', '26,118', '560'],
                    ['Group FX Hedges', '488', '190,118', '16,204', '428'],
                    ['Group Intercompany Loans', '276', '108,118', '13,204', '386'],
                    ['Unbooked', '26', '118,000', '2,004', '42']
                ]
            },
            errors: [],
            story: [
                'Headline Position finished at <span class="fig">09:06:02</span>, <span class="fig">three minutes and fifty-one seconds</span> after ACCOUK asked for it. It was a production run: pre-processing gathered the party\u2019s trades and market data, the engine priced them on the grid, and retrieval put the result at <span class="key">reporting/runs/2a58d0f4-7e31-4b96-8c05-6d1e9a3f2b48/output.tar.gz</span>.',
                'Its configuration names two analytics, <span class="mono">npv</span> and <span class="mono">sensitivity</span>, and no others. npv priced <span class="fig">18,162 positions</span> across <span class="fig">10 books</span>; sensitivity took the Delta and Gamma of each book.',
                'NPV was <span class="fig">USD 87,219,062</span>. Delta was <span class="fig">USD 1,818,348</span> and Gamma <span class="fig">USD 51,510</span>, summed over the books, both for a one basis point shift. GBP Rates was the largest book, at <span class="fig">USD 39,804,118</span>.',
                'The engine reported nothing. No error and no warning sits beside this output, so the archive is the whole account of the run.',
                'The archive is the run\u2019s whole answer: <span class="fig">{{files}} files</span>, <span class="fig">{{bytes}}</span> before compression. Two of them are the analytics\u2019 own output \u2014 npv.csv and sensitivity.csv \u2014 and the figures above are a reading of those two files, not a table this screen can query. This run completed clean, which is what lets a later run of the same definition be compared against it without a partial marking.'
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
        ],
        /* Headline Position, ACCOUK, 18,162 rows in npv.csv. The run compared
           against the 17 October one. Every row priced: it completed clean. */
        '2a58d0f4-7e31-4b96-8c05-6d1e9a3f2b48': [
            { id: 'b1c7e094-3a62-4d18-9f75-2e8c5a0d3b61', type: 'Bond', maturity: '2031-05-12',
                netting: 'NS-ACCOUK-004', ccy: 'GBP', npv: '1,204,118.00', npvBase: '1,421,204.33',
                notional: '38,000,000', notionalCcy: 'GBP', cp: 'ACME BANK PLC' },
            { id: '0d5a2f87-6b31-4e08-9c74-5a1f8e2d3b90', type: 'Bond', maturity: '2033-10-04',
                netting: 'NS-ACCOUK-001', ccy: 'GBP', npv: '590,882.40', npvBase: '697,606.83',
                notional: '42,000,000', notionalCcy: 'GBP', cp: 'MERIDIAN CAPITAL' },
            { id: '3e8f52a7-9c14-4b80-8d36-5a2e7b1c4f09', type: 'Bond', maturity: '2034-08-22',
                netting: 'NS-ACCOUK-004', ccy: 'GBP', npv: '1,190,402.20', npvBase: '1,405,420.18',
                notional: '70,000,000', notionalCcy: 'GBP', cp: 'NORTHGATE CLEARING' },
            { id: '8a4d1e63-2f75-4c09-b1a8-6d3e0f2c5b97', type: 'Bond', maturity: '2032-05-30',
                netting: 'NS-ACCOUK-001', ccy: 'GBP', npv: '588,204.40', npvBase: '694,446.66',
                notional: '40,000,000', notionalCcy: 'GBP', cp: 'MERIDIAN CAPITAL' },
            { id: 'c6f20b94-7d35-4a18-8e62-1b9c5d0f3a27', type: 'Bond', maturity: '2030-06-17',
                netting: 'NS-ACCOUK-002', ccy: 'USD', npv: '204,118.60', npvBase: '204,118.60',
                notional: '12,000,000', notionalCcy: 'USD', cp: 'PACIFIC TRUST' },
            { id: '74b9e0d3-2c16-4f85-a9d0-3e7b5c8f1a42', type: 'Bond', maturity: '2028-09-29',
                netting: 'NS-ACCOUK-003', ccy: 'USD', npv: '88,402.10', npvBase: '88,402.10',
                notional: '9,000,000', notionalCcy: 'USD', cp: 'KESTREL ASSURANCE' }
        ]
    };

    /* One scenario, and it is the deliberate one. Read the page without it and a
       reader can take the aggregate figures for the whole answer: the run says
       it carries eleven problems, and nothing on the figures, the book tree or
       the trade table repeats that. The scenario draws the connection.

       A completed run's totals can be incomplete for a reason the model already
       records. The engine could not build some trades, so npv.csv carries no row
       for them and no total read from that file includes them; and the
       sensitivity analytic skipped positions that priced, so those carry an NPV
       and no Delta. Both facts are already in the run's own error rows, read by
       reporting.v1.report_error_events.list_by_report_instance_id, which exists.
       No operation is invented to draw this.

       The two errors below are the run's own fixture rows, verbatim. They are
       recorded against the sensitivity analytic, not against a TradeId: an
       analytic-scoped error keys no trade, and the page says so where it shows
       them. The trades are bonds booked to books the tree already draws, so the
       tree marking is exercised on a book and on the portfolios above it. */
    var SCENARIOS = {
        'results-with-errors': {
            id: 'results-with-errors',
            runId: '3f7a91c4-6c2e-4b1d-9a84-0e5c2b7d18f3',
            chip: 'results partial',
            /* The trades the engine could not build. The run's own fixture
               already carries one unbuildable row (2d0f8a31, booked to Liquidity
               Buffer Bonds); these two join it, in two different books. */
            unbuildable: [
                { id: 'e07c4b21-6a95-4d38-bf72-1c8e3a5d9027', type: 'Bond', maturity: '2036-03-18',
                    netting: 'NS-ACCOUK-004', ccy: 'GBP', npv: null, npvBase: null,
                    notional: '35,000,000', notionalCcy: 'GBP', cp: 'NORTHGATE CLEARING',
                    book: 'Liquidity Buffer Bonds' },
                { id: '4d9f0a63-7c21-4b85-9e40-6a2d8f1b3c57', type: 'Bond', maturity: '2035-11-07',
                    netting: 'NS-ACCOUK-001', ccy: 'GBP', npv: null, npvBase: null,
                    notional: '22,000,000', notionalCcy: 'GBP', cp: 'KESTREL ASSURANCE',
                    book: 'GBP Rates' }
            ],
            errors: [
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'SENSITIVITY', message: 'Sensitivity run skipped 412 positions: the base scenario has no price to bump' },
                { severity: 'error', scope: 'Analytics', subject: 'analytic', subjectId: 'SENSITIVITY', message: 'Failed to compute delta for risk factor IR/RATE/GBP/10Y: the shifted scenario did not price' }
            ],
            /* The positions that priced and carry no Delta, because the
               sensitivity analytic skipped them. The figure is the run's own:
               it is the count in the first error row above. */
            noDelta: 412,
            /* The figures a reader must not take as the whole answer. The
               numbers stay; the marker and the footnote are what change. */
            markedFigures: ['NPV', 'Delta', 'Gamma', 'positions priced']
        }
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
        },
        /* The same tree, the same book and portfolio names, and its own figures.
           The shape has to match so that a selected node names the same node in
           both runs; selecting a book then compares that book across the two
           runs. The figures differ book by book: GBP Rates is up, Equity and
           Liquidity Buffer Bonds are down, and the Group FX Hedges book is up by
           thirty per cent, so no two rows share a percentage. */
        '2a58d0f4-7e31-4b96-8c05-6d1e9a3f2b48': {
            name: 'Group Treasury Portfolio',
            children: [
                {
                    name: 'Trading Portfolio',
                    children: [
                        {
                            name: 'Rates Portfolio',
                            children: [
                                { name: 'GBP Rates', trades: 4180, npv: 39804118, delta: 588204, gamma: 17412,
                                    rows: ['b1c7e094-3a62-4d18-9f75-2e8c5a0d3b61',
                                        '0d5a2f87-6b31-4e08-9c74-5a1f8e2d3b90'] },
                                { name: 'EUR Rates', trades: 3960, npv: 28904118, delta: 502118, gamma: 14502,
                                    rows: [] },
                                { name: 'USD Rates', trades: 5080, npv: 12104118, delta: 396204, gamma: 8804,
                                    rows: [] }
                            ]
                        },
                        {
                            name: 'Markets Portfolio',
                            children: [
                                { name: 'FX Options', trades: 1842, npv: 4302118, delta: 171204, gamma: 6204,
                                    rows: [] },
                                { name: 'Inflation', trades: 1046, npv: 984118, delta: 88204, gamma: 2704,
                                    rows: [] },
                                { name: 'Equity', trades: 642, npv: 302118, delta: 14884, gamma: 468,
                                    rows: [] }
                            ]
                        }
                    ]
                },
                { name: 'Liquidity Buffer Bonds', trades: 622, npv: 402118, delta: 26118, gamma: 560,
                    rows: ['3e8f52a7-9c14-4b80-8d36-5a2e7b1c4f09',
                        '8a4d1e63-2f75-4c09-b1a8-6d3e0f2c5b97'] },
                { name: 'Group FX Hedges', trades: 488, npv: 190118, delta: 16204, gamma: 428, rows: [] },
                { name: 'Group Intercompany Loans', trades: 276, npv: 108118, delta: 13204, gamma: 386,
                    rows: [] },
                { name: 'Unbooked', kind: 'unbooked', trades: 26, npv: 118000, delta: 2004, gamma: 42,
                    rows: ['c6f20b94-7d35-4a18-8e62-1b9c5d0f3a27',
                        '74b9e0d3-2c16-4f85-a9d0-3e7b5c8f1a42'] }
            ]
        }
    };

    /* The last completed runs of each definition, oldest first, for the trend
       plot. A series across runs is the one reading a single run cannot give, and
       it is what makes the comparison legible: the baseline is a point on this
       line rather than a control with nothing behind it.

       Every number here is that run's own NPV. The two runs the switcher can open
       appear with the same figures as their own fixtures above; the earlier ones
       are series-only, because a fourth and fifth full run fixture would add a
       book tree apiece and no honesty. A run that carried errors wrote a figure
       over the trades that survived, and `errors` is how many problems it
       recorded, so its point can be marked partial. A definition with one run has
       one point and is not plotted. */
    var RUN_SERIES = {
        'Headline Position': [
            { short: '8d41c2e7', startedAt: '2026-10-13 09:02:09', npv: 84102884, errors: 0 },
            { short: 'c70a9b31', startedAt: '2026-10-14 09:02:14', npv: 85884120, errors: 0 },
            { short: 'e13f6d80', startedAt: '2026-10-15 09:02:07', npv: 86204118, errors: 2 },
            { short: '2a58d0f4', startedAt: '2026-10-16 09:02:11', npv: 87219062, errors: 0 },
            { short: '3f7a91c4', startedAt: '2026-10-17 09:02:11', npv: 88402114, errors: 11 }
        ],
        'Board Risk Dashboard': [
            { short: 'a48e1c02', startedAt: '2026-10-14 08:12:38', npv: 12118402, errors: 0 },
            { short: 'f0b39d74', startedAt: '2026-10-15 08:12:51', npv: 12604118, errors: 0 },
            { short: '17c2e850', startedAt: '2026-10-16 08:12:22', npv: 12318404, errors: 1 },
            { short: '4b7e1a02', startedAt: '2026-10-17 08:12:44', npv: 12884102, errors: 0 }
        ],
        'Large Exposures': [
            { short: '9a20c7d1', startedAt: '2026-10-16 18:02:11', npv: 383853078, errors: 0 }
        ]
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
        A: 'A needs the result store. Every figure, every row of the aggregate table, every node of the book tree and every row of the per-trade table is a reading of the archive the two configured analytics wrote, and nothing parses an archive: post-processing is designed and not built. The tree needs one read more than the table: the archive has no book column, so a node is only reachable by joining each TradeId to trade_bookings.book_id through trading.v1.trade_bookings.list. The comparison needs all of that twice, once for each run, plus the read that lists the runs of a definition so the baseline can be chosen. Built with today\u2019s operations, A is the archive footer with an empty screen above it.',
        B: 'B is the only variant buildable from today\u2019s reads, and only down to the run: the state, the output message, the storage key and the errors are all fields of the run. What B cannot have is its own centre \u2014 the manifest, the per-trade rows behind npv.csv, the book tree\u2019s grouping and any download \u2014 because no operation lists, parses or serves the archive, and none groups a result by book. Its file list is inferred from the configured analytics, not observed inside the tar.',
        C: 'C needs what A needs \u2014 the parsed numbers, the trade rows and the book grouping \u2014 and then a writer trusted to keep a figure and its caveat in the same sentence. It is the easiest of the three to make wrong and the hardest to check: a column can be inspected, a sentence is believed.'
    };

    var S = {
        variant: 'A', runId: DEFAULT_RUN, scenario: null, file: 'npv.csv', trade: null, showErrors: false,
        note: null, book: null, collapsed: [], baseline: undefined
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

    /* The scenario in view, or null. A scenario is a deliberate arrangement of
       the run's own fixtures, so a reader finds it by its chip rather than by
       accident; it never changes a run, it changes what the page says about the
       result the run already carries. */
    function scenario() {
        return S.scenario ? SCENARIOS[S.scenario] : null;
    }

    function scenarioFor(run) {
        var sc = scenario();
        return sc && sc.runId === run.id ? sc : null;
    }

    /* --------------------------------------------------------- comparison */

    /* A comparison is another run of the same definition, and nothing else. A
       stress scenario is not a run of this report, so it has no figures of its
       own to compare with, and yesterday's close belongs to a different run. The
       only baseline that answers "what changed since this report last ran" is a
       completed earlier instance of the same definition, so that is the only
       kind offered. Newest first: the previous run is the one a reader means. */
    function baselineCandidates(run) {
        if (run.state !== 'completed') return [];
        return RUNS.filter(function (r) {
            return r.definition === run.definition && r.id !== run.id &&
                r.state === 'completed' && r.figures.length && BOOK_TREES[r.id] &&
                r.startedAt < run.startedAt;
        }).sort(function (a, b) {
            return a.startedAt < b.startedAt ? 1 : -1;
        });
    }

    /* S.baseline is undefined until a person chooses: undefined is the previous
       run of the definition, null is no comparison, and a run id is that run. The
       page therefore opens with the comparison on, and ?baseline=none turns it
       off. A run with no earlier instance has no baseline whatever the state. */
    function baselineFor(run) {
        if (S.baseline === null) return null;
        var candidates = baselineCandidates(run);
        if (!candidates.length) return null;
        if (typeof S.baseline === 'string') {
            var chosen = candidates.filter(function (r) { return r.id === S.baseline; })[0];
            if (chosen) return chosen;
        }
        return candidates[0];
    }

    /* A change is only as honest as the two numbers under it. A run that carried
       errors wrote a figure over the trades that survived, so a change against
       it, or from it, is partial. The results-with-errors scenario is the
       deliberate case: it says which trades and which sensitivities are missing.
       A baseline that completed clean is not partial. */
    function comparisonPartiality(run, base) {
        if (!base) return null;
        var current = scenarioFor(run);
        if (current) return { side: 'current', run: run, scenario: current };
        var baseScenario = scenarioFor(base);
        if (baseScenario) return { side: 'baseline', run: base, scenario: baseScenario };
        if (base.errors.length) return { side: 'baseline', run: base };
        if (run.errors.length) return { side: 'current', run: run };
        return null;
    }

    function figureNumber(figure) {
        if (!figure) return null;
        return typeof figure.n === 'number' && isFinite(figure.n) ? figure.n : null;
    }

    function changeOf(current, base) {
        if (typeof current !== 'number' || typeof base !== 'number') return null;
        return {
            amount: current - base,
            pct: base === 0 ? null : (current - base) / Math.abs(base) * 100
        };
    }

    /* The percentage divides by the baseline's magnitude, so a fall reads as a
       fall whether the baseline is positive or negative. A share of the run is
       compared in percentage points, which have no percentage of their own. */
    function signedAmount(amount, style) {
        var sign = amount < 0 ? '-' : '+';
        if (style === 'money') return sign + 'USD ' + num(Math.abs(amount));
        if (style === 'points') return sign + Math.abs(amount).toFixed(1) + ' pt';
        return sign + num(Math.abs(amount));
    }

    function signedPct(pct) {
        return (pct < 0 ? '-' : '+') + Math.abs(pct).toFixed(1) + '%';
    }

    function deltaBadge(current, base, style, partial, baseRun) {
        var d = changeOf(current, base);
        if (!d) return '';
        var direction = d.amount > 0 ? 'up' : (d.amount < 0 ? 'down' : 'flat');
        var text = signedAmount(d.amount, style) +
            (style === 'points' || d.pct === null ? '' : ' \u00b7 ' + signedPct(d.pct));
        var title = 'Change against run ' + baseRun.id + ', started ' + baseRun.startedAt +
            (partial ? '. The change is partial.' : '');
        return '<span class="bdg ' + direction + (partial ? ' partial' : '') + '" title="' + esc(title) + '">' +
            '<span class="arrow">' + (direction === 'up' ? '\u25b2' : direction === 'down' ? '\u25bc' : '\u25ac') + '</span>' +
            esc(text) + (partial ? '<span class="bdgtag">partial</span>' : '') + '</span>';
    }

    function baselineFigure(base, label) {
        return (base.figures || []).filter(function (f) { return f.label === label; })[0] || null;
    }

    function tileStyle(label) {
        if (/NPV|Delta|Gamma|VaR|EAD/.test(label)) return 'money';
        if (label.indexOf('of the run') !== -1) return 'points';
        return 'count';
    }

    /* The artefacts link. The icon is Fluent's folder, inlined because the
       prototype stands alone and cannot load the application's asset. The icon
       set carries no box, package or archive glyph, so the folder is the
       nearest meaning: a container of files. The destination is the artefacts
       page, which the application does not have yet. */
    var ARTEFACT_ICON =
        '<svg class="aricon" viewBox="0 0 20 20" width="16" height="16" aria-hidden="true">' +
        '<path fill="currentColor" d="M4.5 3C3.11929 3 2 4.11929 2 5.5V14.5C2 15.8807 3.11929 17 4.5 17H15.5C16.8807 17 18 15.8807 18 14.5V7.5C18 6.11929 16.8807 5 15.5 5H9.70711L8.21967 3.51256C7.89148 3.18437 7.44636 3 6.98223 3H4.5ZM3 5.5C3 4.67157 3.67157 4 4.5 4H6.98223C7.18115 4 7.37191 4.07902 7.51256 4.21967L8.79289 5.5L7.43934 6.85355C7.34557 6.94732 7.21839 7 7.08579 7H3V5.5ZM3 8H7.08579C7.48361 8 7.86514 7.84196 8.14645 7.56066L9.70711 6H15.5C16.3284 6 17 6.67157 17 7.5V14.5C17 15.3284 16.3284 16 15.5 16H4.5C3.67157 16 3 15.3284 3 14.5V8Z"/></svg>';

    function artefactsLink(key, what) {
        return '<a class="artelink" href="/artefacts?package=' + encodeURIComponent(key) + '"' +
            ' title="Open the artefacts for the ' + esc(what) + '"' +
            ' aria-label="Open the artefacts for the ' + esc(what) + '">' +
            ARTEFACT_ICON + '<span>Artefacts</span></a>';
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
        var rows = TRADES[run.id] || [];
        var sc = scenarioFor(run);
        if (sc) rows = rows.concat(sc.unbuildable);
        return rows;
    }

    /* A trade row with no price is one the engine could not build. npv.csv
       carries no row for it, so the file itself says so by omission, and the run
       recorded the reason in its errors. */
    function failedTrades(run) {
        return tradesFor(run).filter(function (t) { return t.npv === null; });
    }

    function tradeById(run, id) {
        return tradesFor(run).filter(function (t) { return t.id === id; })[0];
    }

    /* A trade error is a report error whose subject is the trade: the model
       already keys those by subject_id, so the row and the error join on the
       TradeId and nothing is invented to connect them.

       A scenario's unbuildable trades carry the analytic-scoped rows the run
       recorded about the sensitivity analytic. Those rows key no TradeId, so they
       are not a join: they are the run's account of why positions like this one
       have no price and no Delta, and the trade panel says which of the two kinds
       a row is. */
    function tradeErrors(run, id) {
        var errs = (run.errors || []).filter(function (e) {
            return e.subject === 'trade' && e.subjectId === id;
        });
        var sc = scenarioFor(run);
        if (sc && isUnbuildable(run, id)) errs = errs.concat(sc.errors);
        return errs;
    }

    function isUnbuildable(run, id) {
        return failedTrades(run).some(function (t) { return t.id === id; });
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
        var key = run.id + '|' + (S.scenario || '');
        if (!TREE_CACHE[key]) TREE_CACHE[key] = buildNode(scenarioTreeSpec(spec, run), spec.name);
        return TREE_CACHE[key];
    }

    function copyTree(spec) {
        var node = {
            name: spec.name,
            kind: spec.kind,
            trades: spec.trades || 0,
            npv: spec.npv || 0,
            rows: (spec.rows || []).slice(),
            children: (spec.children || []).map(copyTree)
        };
        if (spec.delta !== undefined) node.delta = spec.delta;
        if (spec.gamma !== undefined) node.gamma = spec.gamma;
        return node;
    }

    /* The scenario books a failed trade to a book the tree already draws, and
       marks every node over it as one whose figure lost trades. A book that lost
       a trade is marked on its own row; a portfolio above it inherits the
       exclusion, because its figure is the sum of its books and it must not
       include a book that lost trades without saying so. */
    function scenarioTreeSpec(spec, run) {
        var copy = copyTree(spec);
        var sc = scenarioFor(run);
        if (!sc) return copy;
        var failed = failedTrades(run).map(function (t) { return t.id; });
        (function walk(node) {
            if (node.children.length) {
                node.children.forEach(walk);
                return;
            }
            (sc.unbuildable || []).forEach(function (t) {
                if (t.book === node.name && node.rows.indexOf(t.id) === -1) node.rows.push(t.id);
            });
            node.lost = node.rows.filter(function (id) { return failed.indexOf(id) !== -1; }).length;
        })(copy);
        return copy;
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
            npv: spec.npv || 0,
            lost: spec.lost || 0,
            lostNames: spec.lost ? [spec.name] : []
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
            node.lost = 0;
            node.lostNames = [];
            node.children.forEach(function (child) {
                node.trades += child.trades;
                node.npv += child.npv;
                node.rows = node.rows.concat(child.rows);
                node.lost += child.lost;
                node.lostNames = node.lostNames.concat(child.lostNames);
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

    /* The exclusion a node carries, in words on the row that draws it. A book
       that lost trades says so about its own figure. A node above it says it
       inherits the exclusion, so the roll-up never includes a book that lost
       trades without the reader being told. */
    function nodeExclusion(node, depth) {
        if (!node.lost) return '';
        var pad = 10 + depth * 18;
        var names = node.lostNames || [];
        var trades = node.lost + ' failed trade' + (node.lost === 1 ? '' : 's');
        var text = node.children.length
            ? 'Inherits the exclusion from ' + esc(names.join(', ')) + ': ' + trades + ' ' +
              (node.lost === 1 ? 'sits' : 'sit') + ' below it and ' +
              (node.lost === 1 ? 'is' : 'are') + ' not in this figure.'
            : 'Excludes ' + trades + ' the engine could not build. npv.csv carries no row for ' +
              (node.lost === 1 ? 'it' : 'them') + ', so neither this count nor this figure includes ' +
              (node.lost === 1 ? 'it' : 'them') + '.';
        return '<div class="bexcl" style="padding-left:' + pad + 'px">' +
            '<span class="tag bad">partial</span><span>' + text + '</span></div>';
    }

    /* cmp is {base, partial} while the comparison is on, and null otherwise. With
       it, every node carries the change of its aggregate NPV against the same
       node of the baseline tree: the node ids are the tree paths, so a book is
       compared with that book and a portfolio with that portfolio. B and C draw
       the plain tree, because the comparison lives on the result panel. */
    function treeRows(node, depth, cmp) {
        var rows = [];
        var hasChildren = node.children.length > 0;
        var collapsed = isCollapsed(node.id);
        var kind = node.kind === 'unbooked' ? 'no booking' : node.kind;
        var lost = node.lost > 0;
        var baseNode = cmp ? findNode(bookTree(cmp.base), node.id) : null;
        var delta = baseNode
            ? deltaBadge(node.npv, baseNode.npv, 'money', cmp.partial, cmp.base)
            : '';
        rows.push('<div class="bnode ' + esc(node.kind) + (lost ? ' partial' : '') +
            (S.book === node.id ? ' sel' : '') +
            '" data-act="book" data-book="' + esc(node.id) + '" style="padding-left:' + (10 + depth * 18) + 'px">' +
            (hasChildren
                ? '<span class="twisty" data-act="twisty" data-book="' + esc(node.id) + '" title="' +
                    (collapsed ? 'Expand' : 'Collapse') + '">' + (collapsed ? '\u25b8' : '\u25be') + '</span>'
                : '<span class="twisty pad"></span>') +
            '<span class="bname">' + esc(node.name) +
                (lost ? ' <span class="tag bad">partial</span>' : '') + '</span>' +
            '<span class="bkind">' + esc(kind) + '</span>' +
            '<span class="btrades">' + num(node.trades) + '</span>' +
            '<span class="bnpv">' + esc(money(node.npv)) + '</span>' +
            (cmp ? '<span class="bdelta">' + delta + '</span>' : '') +
            '</div>');
        node.depth = depth;
        rows.push(nodeExclusion(node, depth));
        if (hasChildren && !collapsed) {
            node.children.forEach(function (child) {
                rows = rows.concat(treeRows(child, depth + 1, cmp));
            });
        }
        return rows;
    }

    function treeTable(run, cmp) {
        var root = bookTree(run);
        if (!root) return '';
        return '<div class="booktree' + (cmp ? ' withdelta' : '') + '">' +
            '<div class="bhead" aria-hidden="true">' +
            '<span class="twisty pad"></span>' +
            '<span class="bname">Portfolio, book or trade</span>' +
            '<span class="bkind">Level</span>' +
            '<span class="btrades">Trades</span>' +
            '<span class="bnpv">Aggregate NPV' + (scenarioFor(run) ? ' \u00b7 partial' : '') + '</span>' +
            (cmp ? '<span class="bdelta">Change' + (cmp.partial ? ' \u00b7 partial' : '') + '</span>' : '') +
            '</div>' + treeRows(root, 0, cmp).join('') + '</div>';
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
            (node.lost ? '<span class="tag bad">partial</span>' : '') +
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
        var sc = scenarioFor(run);
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
            (sc && root.lost
                ? ' A marked node is one whose figure lost trades. ' + root.lost + ' failed trade' +
                  (root.lost === 1 ? '' : 's') + ' ' + (root.lost === 1 ? 'sits' : 'sit') + ' under ' +
                  root.lostNames.join(' and ') + ' and ' + (root.lost === 1 ? 'is' : 'are') + ' not in any ' +
                  'figure on this tree, because npv.csv carries no row for ' +
                  (root.lost === 1 ? 'it' : 'them') + '. A portfolio above a marked book inherits the exclusion: ' +
                  'its figure is the sum of its books, and a book that lost trades is one of them.'
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

    function bookTreeBody(run, cmp) {
        return bookTreeNotice() + '<div style="margin-top:12px">' + treeTable(run, cmp) + '</div>' +
            bookTreeFooter(run);
    }

    /* A draws the tree inside the result card, above the per-trade table; B and C
       give it a card of its own. Only A passes a comparison, so only A's tree
       carries the change column. */
    function bookTreeBlock(run, cmp) {
        if (!bookTree(run)) return '';
        return '<div id="booktree">' + bookTreeHead(run, 'h3') + bookTreeBody(run, cmp) + '</div>';
    }

    function bookTreeCard(run, blurb) {
        if (!bookTree(run)) return '';
        return '<section class="card" id="booktree">' + bookTreeHead(run, 'h2') +
            (blurb ? '<p class="hint" style="margin-top:8px">' + esc(blurb) + '</p>' : '') +
            bookTreeBody(run) +
            '</section>';
    }

    /* The aggregate tile set. At the root it is the run's figures; below it, the
       figures are the selected node's, read off the tree the screen just drew.
       With a baseline in view each tile also carries the baseline's figure and
       the two numbers a change is made of, so a reader can check the subtraction
       by hand. The selected node names the same node in both trees. */
    function tilesFor(run, base) {
        var root = bookTree(run);
        var node = selectedBookNode(run);
        if (!root || !node || treeIsRoot(run)) {
            return (run.figures || []).map(function (f) {
                var bf = base ? baselineFigure(base, f.label) : null;
                return {
                    value: f.value, label: f.label, note: f.note, style: tileStyle(f.label),
                    current: figureNumber(f),
                    base: bf ? figureNumber(bf) : null,
                    baseValue: bf ? bf.value : null
                };
            });
        }
        var leaves = bookLeaves(node);
        var baseRoot = base ? bookTree(base) : null;
        var baseNode = baseRoot ? findNode(baseRoot, node.id) : null;
        var baseLeaves = baseNode ? bookLeaves(baseNode) : null;
        var shareNow = root.npv ? node.npv / root.npv * 100 : 0;
        var shareBase = baseRoot && baseNode && baseRoot.npv
            ? baseNode.npv / baseRoot.npv * 100 : null;
        return [
            {
                value: money(node.npv), label: 'NPV', style: 'money',
                note: 'the selected node, rolled up from its children',
                current: node.npv, base: baseNode ? baseNode.npv : null,
                baseValue: baseNode ? money(baseNode.npv) : null
            },
            {
                value: num(node.trades), label: 'trades priced', style: 'count',
                note: 'of the ' + num(root.trades) + ' this run priced',
                current: node.trades, base: baseNode ? baseNode.trades : null,
                baseValue: baseNode ? num(baseNode.trades) : null
            },
            {
                value: share(node.npv, root.npv), label: 'of the run\u2019s NPV', style: 'points',
                note: 'the selected node against ' + root.name,
                current: shareNow, base: shareBase,
                baseValue: shareBase === null ? null : shareBase.toFixed(1) + '%'
            },
            {
                value: String(leaves.length),
                label: leaves.length === 1 ? 'book in this subtree' : 'books in this subtree',
                style: 'count',
                note: 'each book\u2019s figure is the sum of its trades',
                current: leaves.length, base: baseLeaves ? baseLeaves.length : null,
                baseValue: baseLeaves ? String(baseLeaves.length) : null
            }
        ];
    }

    /* The by-book table is the tree's books, in tree order, so the aggregate
       table and the tree cannot disagree about a book's name or its figures.
       With a baseline in view every row gains its NPV change and percentage, and
       the table gains a total row, so the book rows and the total are one
       arithmetic: the total change is the difference of the two totals, which is
       also the sum of the ten book changes. */
    function bookTableData(run, base) {
        var root = bookTree(run);
        if (!root) return run.table;
        var leaves = bookLeaves(root);
        var complete = leaves.every(function (b) {
            return b.delta !== undefined && b.gamma !== undefined;
        });
        if (!complete || !run.table) return run.table;
        var baseLeaves = base && bookTree(base) ? bookLeaves(bookTree(base)) : null;
        var rows = leaves.map(function (b) {
            var row = [b.name, num(b.trades), num(b.npv), num(b.delta), num(b.gamma)];
            if (baseLeaves) {
                var bb = baseLeaves.filter(function (x) { return x.name === b.name; })[0];
                var d = bb ? changeOf(b.npv, bb.npv) : null;
                row.push(d ? signedAmount(d.amount, 'money') : '\u2014');
                row.push(d && d.pct !== null ? signedPct(d.pct) : '\u2014');
            }
            return row;
        });
        var result = {
            title: run.table.title,
            note: run.table.note,
            columns: run.table.columns.slice(),
            rows: rows
        };
        if (!baseLeaves) return result;
        result.columns = run.table.columns.concat(['NPV change (USD)', 'Change']);
        result.changeAt = run.table.columns.length;
        result.note = run.table.note + ' The last two columns are the change against run ' +
            base.short + ', started ' + base.startedAt + ', and the total row carries the same change.';
        var totalTrades = 0, totalNpv = 0, totalDelta = 0, totalGamma = 0, totalBaseNpv = 0;
        leaves.forEach(function (b) {
            totalTrades += b.trades;
            totalNpv += b.npv;
            totalDelta += b.delta;
            totalGamma += b.gamma;
        });
        baseLeaves.forEach(function (b) { totalBaseNpv += b.npv; });
        var total = changeOf(totalNpv, totalBaseNpv);
        result.total = ['Total', num(totalTrades), num(totalNpv), num(totalDelta), num(totalGamma),
            total ? signedAmount(total.amount, 'money') : '\u2014',
            total && total.pct !== null ? signedPct(total.pct) : '\u2014'];
        return result;
    }

    /* -------------------------------------------------------------- plots */
    /* Every plot on this page is inline SVG written here, because a prototype
       opens from file:// with no build, no CDN and no network: a chart library
       cannot be loaded, and none is copied in either. The form of each plot
       follows the shape of its numbers. A magnitude compared across categories
       is bars, a change over time is a line, and a category set with no
       magnitude to compare is no plot at all.

       Three rules hold every plot on this page.

       - A bar starts at the zero rule, and a negative value draws a bar on the
         other side of it. A line states the range of its value axis and does not
         silently start somewhere else.
       - A value's sign picks the ok or bad token. The accent is spent on
         controls and never on data.
       - A partial result is never plotted as if it were whole. A figure that
         excludes a trade the engine could not build is a floor, so a bar or a
         point drawn from it is dashed, the plot carries the partial tag the
         figure carries, and the caption says what the figure leaves out.

       A plot never replaces the figures a reader checks: the by-book table under
       these plots carries every bar's exact number, the trend carries its own run
       table, and every SVG carries a text alternative in its aria-label. */

    var MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

    function pad2(n) {
        return n < 10 ? '0' + n : String(n);
    }

    function stampMs(stamp) {
        var match = /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})$/.exec(stamp);
        if (!match) return 0;
        return Date.UTC(Number(match[1]), Number(match[2]) - 1, Number(match[3]),
            Number(match[4]), Number(match[5]), Number(match[6]));
    }

    function dayLabel(ms) {
        var d = new Date(ms);
        return d.getUTCDate() + ' ' + MONTHS[d.getUTCMonth()];
    }

    function clockLabel(ms) {
        var d = new Date(ms);
        return pad2(d.getUTCHours()) + ':' + pad2(d.getUTCMinutes());
    }

    /* The value axis, and a bar's value label, in the shortest form that still
       says the unit. The full figure is in the bar's title and in the table. */
    function shortMoney(n) {
        var sign = n < 0 ? '-' : '';
        var a = Math.abs(n);
        if (a >= 1e9) return sign + 'USD ' + (a / 1e9).toFixed(1) + ' bn';
        if (a >= 1e6) return sign + 'USD ' + (a / 1e6).toFixed(1) + ' m';
        if (a >= 1e3) return sign + 'USD ' + Math.round(a / 1e3) + ' k';
        return sign + 'USD ' + Math.round(a);
    }

    function r(n) {
        return Math.round(n * 10) / 10;
    }

    /* A figure set the page will not draw. A bar of nothing, or a bar of one
       thing, is worse than a sentence that says which question the table answers
       instead. */
    function plotAbsent(title, why) {
        return '<div class="plot-absent"><b>' + esc(title) + '</b> ' + esc(why) + '</div>';
    }

    function plotFigure(spec, svg) {
        return '<figure class="plot">' +
            '<div class="plot-head">' +
            '<span class="plot-title">' + esc(spec.title) + '</span>' +
            '<span class="plot-unit">' + esc(spec.unit) + '</span>' +
            (spec.partial ? '<span class="tag bad">partial</span>' : '') +
            '</div>' + svg +
            '<figcaption class="plot-cap"><span>' + esc(spec.caption) + '</span>' +
            (spec.excluded
                ? '<span class="plot-excl"><span class="tag bad">partial</span><span>' +
                  esc(spec.excluded) + '</span></span>'
                : '') +
            '</figcaption>' +
            '</figure>';
    }

    function barsAria(spec) {
        return 'Horizontal bars: ' + spec.title + ', in ' + spec.unit + '. ' +
            spec.data.map(function (d) {
                return d.label + ' ' + money(d.value) + (d.partial ? ' (partial)' : '');
            }).join('; ') + '. Every bar starts at the zero rule.';
    }

    function lineAria(spec, lo, hi) {
        return 'Line: ' + spec.title + ', in ' + spec.unit + '. ' +
            spec.points.map(function (p) {
                return p.title || p.label;
            }).join('; ') + '. The value axis runs from ' + shortMoney(lo) + ' to ' +
            shortMoney(hi) + ' and does not start at zero.';
    }

    /* Horizontal bars. A bar is drawn from the zero rule, so a negative value
       reads as a bar on the other side of it rather than as a short bar. A chart
       that holds a negative value keeps a gutter left of the zero rule, so the
       value label of the most negative bar sits beside the bar and not on top of
       the book names. */
    function barChart(spec) {
        var data = spec.data;
        var W = 820, labelW = 200, valueW = 156, rowH = 26, top = 10, axisH = 28;
        var values = data.map(function (d) { return d.value; });
        var floor = Math.min.apply(null, values.concat([0]));
        var ceil = Math.max.apply(null, values.concat([0]));
        if (ceil === floor) ceil = floor + 1;
        var startX = labelW + (floor < 0 ? 124 : 0);
        var plotW = W - valueW - startX;
        var H = top + data.length * rowH + axisH;
        var axisY = top + data.length * rowH;

        function xOf(v) {
            return startX + (v - floor) / (ceil - floor) * plotW;
        }

        var zero = xOf(0);
        var out = '<svg class="plot-svg" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(barsAria(spec)) + '">';
        out += '<line class="axis" x1="' + startX + '" y1="' + axisY + '" x2="' + (W - valueW) +
            '" y2="' + axisY + '"></line>';
        out += '<line class="zero" x1="' + r(zero) + '" y1="' + (top - 2) + '" x2="' + r(zero) +
            '" y2="' + axisY + '"></line>';

        data.forEach(function (d, i) {
            var y = top + i * rowH;
            var mid = y + rowH / 2 + 4;
            var x1 = xOf(d.value);
            var left = Math.min(zero, x1);
            var width = Math.max(2, Math.abs(x1 - zero));
            var negative = d.value < 0;
            var labelX = negative ? left - 7 : left + width + 7;
            out += '<text class="pl-lbl" x="' + (labelW - 12) + '" y="' + mid +
                '" text-anchor="end">' + esc(d.label) + '</text>';
            out += '<rect class="bar ' + (negative ? 'neg' : 'pos') + (d.partial ? ' partial' : '') +
                '" x="' + r(left) + '" y="' + (y + 6) + '" width="' + r(width) +
                '" height="14" rx="2"><title>' +
                esc(d.label + ': ' + money(d.value) + (d.partial ? ' (partial)' : '')) +
                '</title></rect>';
            out += '<text class="pl-val" x="' + r(labelX) + '" y="' + mid + '" text-anchor="' +
                (negative ? 'end' : 'start') + '">' + esc(money(d.value)) + '</text>';
        });

        var ticks = [{ v: floor, anchor: 'start' }, { v: ceil, anchor: 'end' }];
        if (floor < 0 && ceil > 0) ticks.push({ v: 0, anchor: 'middle' });
        ticks.forEach(function (t) {
            out += '<text class="pl-tick" x="' + r(xOf(t.v)) + '" y="' + (axisY + 15) +
                '" text-anchor="' + t.anchor + '">' + esc(shortMoney(t.v)) + '</text>';
        });

        return plotFigure(spec, out + '</svg>');
    }

    /* The value range of a line, padded so the highest and lowest point sit off
       the frame. A line of levels does not start at zero; the axis states where
       it does start, and so does the caption. */
    function lineDomain(points) {
        var values = points.filter(function (p) {
            return typeof p.value === 'number';
        }).map(function (p) { return p.value; });
        var lo = Math.min.apply(null, values);
        var hi = Math.max.apply(null, values);
        var pad = (hi - lo) * 0.22;
        if (!pad) pad = Math.abs(hi || 1) * 0.05;
        return { lo: lo - pad, hi: hi + pad };
    }

    /* A line across runs. The latest point is marked and named, the baseline is
       marked, a point whose figure is partial is drawn hollow and its segment
       dashed, and a run with no figure breaks the line rather than drawing a
       zero. */
    function lineChart(spec) {
        var points = spec.points;
        var W = 820, left = 100, right = 26, top = 36, plotH = 176, bottom = 34;
        var plotW = W - left - right;
        var H = top + plotH + bottom;
        var axisY = top + plotH;
        var domain = lineDomain(points);
        var lo = domain.lo, hi = domain.hi;
        var times = points.map(function (p) { return p.at; });
        var t0 = Math.min.apply(null, times);
        var t1 = Math.max.apply(null, times);
        if (t0 === t1) { t0 -= 3600000; t1 += 3600000; }

        function xOf(t) { return left + (t - t0) / (t1 - t0) * plotW; }
        function yOf(v) { return top + (hi - v) / (hi - lo) * plotH; }

        var out = '<svg class="plot-svg" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(lineAria(spec, lo, hi)) + '">';

        [lo, (lo + hi) / 2, hi].forEach(function (v) {
            out += '<line class="grid" x1="' + left + '" y1="' + r(yOf(v)) + '" x2="' +
                (left + plotW) + '" y2="' + r(yOf(v)) + '"></line>';
            out += '<text class="pl-tick" x="' + (left - 10) + '" y="' + (r(yOf(v)) + 4) +
                '" text-anchor="end">' + esc(shortMoney(v)) + '</text>';
        });
        out += '<line class="axis" x1="' + left + '" y1="' + axisY + '" x2="' + (left + plotW) +
            '" y2="' + axisY + '"></line>';

        points.forEach(function (p) {
            if (typeof p.value === 'number') return;
            out += '<line class="gap" x1="' + r(xOf(p.at)) + '" y1="' + top + '" x2="' +
                r(xOf(p.at)) + '" y2="' + axisY + '"><title>' +
                esc(p.title || 'No figure for this run') + '</title></line>';
        });

        for (var i = 1; i < points.length; i += 1) {
            var a = points[i - 1], b = points[i];
            if (typeof a.value !== 'number' || typeof b.value !== 'number') continue;
            out += '<line class="seg' + (a.partial || b.partial ? ' partial' : '') +
                '" x1="' + r(xOf(a.at)) + '" y1="' + r(yOf(a.value)) + '" x2="' +
                r(xOf(b.at)) + '" y2="' + r(yOf(b.value)) + '"></line>';
        }

        points.forEach(function (p) {
            if (typeof p.value !== 'number') return;
            var cx = r(xOf(p.at));
            var cy = r(yOf(p.value));
            var cls = 'pt ' + (p.value < 0 ? 'neg' : 'pos') + (p.partial ? ' partial' : '') +
                (p.mark ? ' ' + p.mark : '');
            out += '<circle class="' + cls + '" cx="' + cx + '" cy="' + cy + '" r="' +
                (p.partial ? 4.5 : (p.mark ? 5 : 3.5)) + '"><title>' +
                esc(p.title || p.label) + '</title></circle>';
            if (!p.mark) return;
            var above = p.mark === 'current';
            var anchor = cx < left + 90 ? 'start' : (cx > left + plotW - 90 ? 'end' : 'middle');
            var tx = anchor === 'start' ? cx - 4 : (anchor === 'end' ? cx + 4 : cx);
            out += '<text class="pl-mark" x="' + r(tx) + '" y="' + r(above ? cy - 12 : cy + 19) +
                '" text-anchor="' + anchor + '">' + esc(p.markLabel || p.label) + '</text>';
        });

        var stride = Math.max(1, Math.ceil(points.length / 7));
        points.forEach(function (p, i) {
            if (i % stride !== 0 && i !== points.length - 1) return;
            var anchor = i === 0 ? 'start' : (i === points.length - 1 ? 'end' : 'middle');
            out += '<text class="pl-tick" x="' + r(xOf(p.at)) + '" y="' + (axisY + 16) +
                '" text-anchor="' + anchor + '">' + esc(spec.xLabel(p)) + '</text>';
        });

        return plotFigure(spec, out + '</svg>');
    }

    /* ------------------------------------------------ the form of a plot */
    /* The vocabulary this page draws from, and the rule that picks one form.
       The rule is data, so the table on the page and the choice each plot makes
       cannot disagree: every one of the six forms below carries the shape of
       numbers it answers to, and a form is chosen by the shape rather than by
       taste.

       Bars and lines alone were too narrow. A set of parts that makes up one
       whole is a donut; several wholes each split the same way are stacked
       bars; two series over the same categories are a grouped bar; two measures
       against each other are a scatter. A table stays the right answer for a
       handful of exact numbers, and no chart replaces it. */

    var FORMS = [
        {
            id: 'donut',
            name: 'Donut',
            shape: 'One whole, few parts',
            rule: 'One whole in not more than about six parts becomes a donut. A seventh part and the ones ' +
                'after it join one \'other\' slice with its own legend row, and the hole carries the total. ' +
                'Each part states its percentage and its value in the legend beside the slice, never on it.'
        },
        {
            id: 'bars',
            name: 'Horizontal bars',
            shape: 'One whole, many parts, or exact comparison',
            rule: 'One whole in more than about six parts, or parts that must be compared precisely, ' +
                'is horizontal bars. A pie of fifteen books is unreadable and its angles cannot be compared.'
        },
        {
            id: 'stacked',
            name: 'Stacked bar',
            shape: 'Many wholes, each split the same way',
            rule: 'More than one whole, each decomposing by the same categories, is a stacked bar: one bar ' +
                'per whole and one segment per part. One whole is a donut; several wholes are stacked bars.'
        },
        {
            id: 'grouped',
            name: 'Grouped bar',
            shape: 'Two series over the same categories',
            rule: 'Two series that share the same categories and the same unit are grouped bars, side by ' +
                'side, so two things are compared rather than one stacked on the other.'
        },
        {
            id: 'scatter',
            name: 'Scatter',
            shape: 'Two measures against each other',
            rule: 'Two measures plotted against each other are a scatter, so an outlier is visible as a ' +
                'point away from the rest rather than as two numbers a reader has to find.'
        },
        {
            id: 'line',
            name: 'Line',
            shape: 'A change over time',
            rule: 'A change over time is a line, and a single point is not a trend.'
        },
        {
            id: 'waterfall',
            name: 'Waterfall',
            shape: 'A contribution that sums to a total',
            rule: 'A contribution that adds up to a total is a waterfall, so the parts and the total are one ' +
                'reading. No figure on this page is a contribution to a known total, so none is drawn as one.'
        },
        {
            id: 'table',
            name: 'Table',
            shape: 'Exact and few',
            rule: 'A handful of exact numbers stays a table, and nothing on this page replaces it: a chart ' +
                'of three figures is harder to check than the three figures.'
        }
    ];

    /* The form table, on the page. It is the rule written down so the next person
       does not guess, and every caption below cites the row it followed. */
    function formRule() {
        var rows = FORMS.map(function (form) {
            return '<tr><td class="fshape">' + esc(form.shape) + '</td>' +
                '<td class="fname"><span class="fchip f-' + form.id + '"></span>' + esc(form.name) + '</td>' +
                '<td class="frule">' + esc(form.rule) + '</td></tr>';
        }).join('');
        return '<details class="formrule" open>' +
            '<summary>How the form of a plot is chosen</summary>' +
            '<p>Every plot on this page is chosen by the shape of its numbers, and the rule is the same on ' +
            'every screen. A pie is never used where its angles cannot be compared, and no chart replaces a ' +
            'number a reader has come to check.</p>' +
            '<div class="table-wrap"><table class="formtable"><thead><tr>' +
            '<th>The numbers are</th><th>Form</th><th>The rule that chooses it</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="formfoot">Every donut carries its percentages in the legend beside each slice. A plot ' +
            'whose figure the errors left partial keeps the dashed or hollow treatment, and a donut draws the ' +
            'excluded part as a hatched slice of its own so the whole of the pie is not silently smaller than ' +
            'the run.</p>' +
            '</details>';
    }

    /* -------------------------------------------------------- the palette */
    /* One colour per book and one per scope, the same on every plot of this
       page. The tokens are the application's own, and no token is spent on
       anything that is not data: the hatched slice is the bad token, because an
       exclusion is not a category. */
    function scopeColour(scope) {
        /* One scope, one colour, on every plot of this page and on the next
           screen that draws the same scope: the map is the scope's own name, not
           its position in a count, so a scope cannot change colour because
           another scope gained a row. */
        var map = {
            Analytics: 'var(--accent-bright)',
            Configuration: 'var(--warn)',
            Curve: 'var(--ink-dim)',
            Report: 'var(--bad)',
            Trade: 'color-mix(in srgb, var(--ok) 78%, var(--bg))'
        };
        return map[scope] || 'var(--ink-dim)';
    }

    /* The order alternates a dark token with a light one, so two parts that end
       up beside each other in a legend are never two shades a reader has to
       compare rather than see. */
    var TOKENS = ['var(--accent-bright)', 'var(--bad)', 'var(--accent)',
        'color-mix(in srgb, var(--warn) 46%, var(--ink))',
        'color-mix(in srgb, var(--ok) 78%, var(--bg))',
        'color-mix(in srgb, var(--ok) 42%, var(--bg))',
        'color-mix(in srgb, var(--accent-bright) 46%, var(--ink))',
        'color-mix(in srgb, var(--accent) 46%, var(--ink))',
        'color-mix(in srgb, var(--bad) 46%, var(--ink))',
        'color-mix(in srgb, var(--warn) 78%, var(--bg))'];

    /* A book takes its colour from its name, and the key covers every book of
       the run's tree rather than the selection, so narrowing the tree to a
       subtree does not repaint the books that stay. */
    function bookColourFn(run) {
        var all = bookLeaves(bookTree(run)).map(function (book) { return book.name; }).sort();
        var keys = [];
        all.forEach(function (name) {
            if (keys.indexOf(name) === -1) keys.push(name);
        });
        return function (name) {
            var index = keys.indexOf(name);
            return BOOK_TOKENS[(index === -1 ? keys.length : index) % BOOK_TOKENS.length];
        };
    }

    var BOOK_TOKENS = ['color-mix(in srgb, var(--ok) 78%, var(--bg))',
        'color-mix(in srgb, var(--warn) 46%, var(--ink))',
        'color-mix(in srgb, var(--accent-bright) 46%, var(--ink))',
        'var(--accent)',
        'color-mix(in srgb, var(--ok) 42%, var(--bg))',
        'color-mix(in srgb, var(--warn) 78%, var(--bg))',
        'color-mix(in srgb, var(--accent) 46%, var(--ink))',
        'color-mix(in srgb, var(--bad) 46%, var(--ink))',
        'color-mix(in srgb, var(--ink) 62%, var(--accent))',
        'var(--accent-bright)'];

    function capitalise(text) {
        return text ? text.charAt(0).toUpperCase() + text.slice(1) : text;
    }

    function prettyPct(part, whole) {
        if (!whole) return '0.0%';
        return (part / whole * 100).toFixed(1) + '%';
    }

    function hatching() {
        return '<defs><pattern id="hatch" width="6" height="6" patternUnits="userSpaceOnUse" ' +
            'patternTransform="rotate(45)">' +
            '<rect width="6" height="6" fill="var(--bad)" fill-opacity="0.12"></rect>' +
            '<line x1="0" y1="0" x2="0" y2="6" stroke="var(--bad)" stroke-width="1.6" ' +
            'stroke-opacity="0.7"></line></pattern></defs>';
    }

    /* The legend, beside the donut and under the other forms. It is the exact
       numbers in text, so a slice never has to be measured: each row states the
       part's name, its value with its unit, and its percentage of the whole. */
    function donutLegend(rows) {
        return '<div class="legend"><div class="legendhead"><span>Part</span><span>Value</span>' +
            '<span>Share</span></div>' + rows.map(function (row) {
                var swatch = row.hatched
                    ? '<span class="swatch hatched"></span>'
                    : '<span class="swatch" style="background:' + row.colour + '"></span>';
                return '<div class="lrow' + (row.hatched ? ' excl' : '') + '" title="' +
                    esc(row.title || row.label) + '">' + swatch +
                    '<span class="lname">' + esc(row.label) + '</span>' +
                    '<span class="lval">' + esc(row.valueText) + '</span>' +
                    '<span class="lpct">' + esc(row.pctText) + '</span></div>';
            }).join('') + '</div>';
    }

    function arcPath(cx, cy, rOuter, rInner, from, to) {
        var large = (to - from) > Math.PI ? 1 : 0;
        var x0 = cx + rOuter * Math.cos(from), y0 = cy + rOuter * Math.sin(from);
        var x1 = cx + rOuter * Math.cos(to), y1 = cy + rOuter * Math.sin(to);
        var x2 = cx + rInner * Math.cos(to), y2 = cy + rInner * Math.sin(to);
        var x3 = cx + rInner * Math.cos(from), y3 = cy + rInner * Math.sin(from);
        return 'M' + r(x0) + ' ' + r(y0) +
            ' A' + rOuter + ' ' + rOuter + ' 0 ' + large + ' 1 ' + r(x1) + ' ' + r(y1) +
            ' L' + r(x2) + ' ' + r(y2) +
            ' A' + rInner + ' ' + rInner + ' 0 ' + large + ' 0 ' + r(x3) + ' ' + r(y3) + ' Z';
    }

    /* A donut. It is the form for one whole in a few parts, and the hole is the
       reason it is a donut rather than a pie: the total sits in the middle, where
       a reader looks for it, and the parts keep the rim. A part the errors left
       out is a hatched slice named =excluded=, so the whole of the pie stays the
       whole of the run. Percentages and values are in the legend beside the
       slice, never on it: an angle is a bad place to read a number. */
    function donutChart(spec) {
        var slices = spec.slices.slice();
        var W = 340, H = 300, cx = 170, cy = 150, rOuter = 112, rInner = 66;
        var positive = slices.filter(function (s) { return s.value > 0; });
        var whole = positive.reduce(function (sum, s) { return sum + s.value; }, 0);
        /* An exclusion whose value is missing has no share to draw. A wedge of
           four degrees keeps it visible without claiming a share, and its legend
           row says excluded rather than a percentage. */
        var MIN_EXCLUDED = 4 / 360;

        var out = '<svg class="plot-svg plot-donut" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">' + hatching();
        var angle = -Math.PI / 2;
        var legend = [];
        var sweepTotal = positive.reduce(function (sum, s) {
            return sum + (s.value / whole);
        }, 0) + slices.filter(function (s) { return s.excluded && !(s.value > 0); }).length *
            MIN_EXCLUDED;
        slices.forEach(function (slice) {
            var text = slice.valueText || spec.valueText(slice.value);
            var pct = whole > 0 && slice.value > 0 ? prettyPct(slice.value, whole) : null;
            legend.push({
                label: slice.label,
                colour: slice.colour,
                hatched: !!slice.excluded,
                valueText: text,
                pctText: slice.excluded ? 'excluded' : (pct === null ? '0.0%' : pct),
                title: slice.title || (slice.label + ': ' + text + (pct ? ' (' + pct + ')' : ''))
            });
            var share = slice.value > 0 ? slice.value / whole
                : (slice.excluded ? MIN_EXCLUDED : 0);
            if (!(share > 0)) return;
            var sweep = share / sweepTotal * Math.PI * 2;
            var to = angle + sweep;
            var fill = slice.excluded ? 'url(#hatch)'
                : (slice.colour || 'var(--ink-dim)');
            var stroke = slice.excluded ? 'var(--bad)' : 'var(--bg)';
            out += '<path class="slice' + (slice.excluded ? ' excluded' : '') + '" d="' +
                arcPath(cx, cy, rOuter, rInner, angle, to) + '" fill="' + fill +
                '" stroke="' + stroke + '" stroke-width="1"><title>' +
                esc(slice.title || (slice.label + ': ' + text + (pct ? ' (' + pct + ')' : ''))) +
                '</title></path>';
            angle = to;
        });
        if (!positive.length) {
            out += '<circle cx="' + cx + '" cy="' + cy + '" r="' + (rOuter + rInner) / 2 +
                '" fill="none" stroke="var(--line-strong)" stroke-dasharray="4 4"></circle>';
        }

        out += '<text class="hole-val" x="' + cx + '" y="' + (cy - 2) + '" text-anchor="middle">' +
            esc(spec.holeValue) + '</text>' +
            '<text class="hole-lbl" x="' + cx + '" y="' + (cy + 17) + '" text-anchor="middle">' +
            esc(spec.holeLabel) + '</text></svg>';
        return '<div class="donut-wrap">' + out + donutLegend(legend) + '</div>' +
            (spec.note ? '<p class="plot-note">' + esc(spec.note) + '</p>' : '');
    }

    /* Stacked bars: several wholes, each split the same way. One bar per whole,
       one segment per part, with the same colour for the same part in every bar,
       and the exact counts in a table under the plot. A segment that is an
       exclusion is hatched and named =excluded= in its own legend row. */
    function stackedChart(spec) {
        var bars = spec.bars;
        var W = 700, left = 120, right = 24, top = 24, barH = 26, gap = 18, axisH = 30;
        var H = top + bars.length * (barH + gap) + axisH;
        var plotW = W - left - right;
        var highest = Math.max.apply(null, bars.map(function (bar) {
            return bar.segments.reduce(function (sum, s) { return sum + Math.max(0, s.value); }, 0);
        }).concat([0]));
        if (highest <= 0) highest = 1;

        function xOf(v) { return left + v / highest * plotW; }

        var out = '<svg class="plot-svg plot-stacked" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">' + hatching();
        out += '<line class="axis" x1="' + left + '" y1="' + (H - axisH + 2) + '" x2="' + (left + plotW) +
            '" y2="' + (H - axisH + 2) + '"></line>';
        [0, highest].forEach(function (v, i) {
            out += '<text class="pl-tick" x="' + r(xOf(v)) + '" y="' + (H - axisH + 17) +
                '" text-anchor="' + (i === 0 ? 'start' : 'end') + '">' + esc(num(v)) + '</text>';
        });

        bars.forEach(function (bar, i) {
            var y = top + i * (barH + gap);
            var x = left;
            out += '<text class="pl-lbl" x="' + (left - 10) + '" y="' + (y + barH / 2 + 4) +
                '" text-anchor="end">' + esc(bar.label) + '</text>';
            bar.segments.forEach(function (segment) {
                var width = Math.max(0, Math.min(plotW - (x - left), segment.value / highest * plotW));
                if (width <= 0.6) return;
                out += '<rect class="sbar' + (segment.excluded ? ' excluded' : '') + '" x="' + r(x) +
                    '" y="' + y + '" width="' + r(width) + '" height="' + barH + '" fill="' +
                    (segment.excluded ? 'url(#hatch)' : segment.colour) + '" stroke="' +
                    (segment.excluded ? 'var(--bad)' : 'var(--bg)') + '" stroke-width="1"><title>' +
                    esc(segment.label + ': ' + num(segment.value) + (segment.excluded ? ' (excluded)' : '')) +
                    '</title></rect>';
                x += width;
            });
            if (bar.segments.every(function (s) { return s.value <= 0; })) {
                out += '<text class="pl-tick" x="' + (left + 6) + '" y="' + (y + barH / 2 + 4) + '">' +
                    esc('no problems recorded') + '</text>';
            }
        });
        return out + '</svg>';
    }

    function stackedLegend(parts) {
        return '<div class="legend">' + parts.map(function (part) {
            return '<div class="lrow' + (part.excluded ? ' excl' : '') + '">' +
                (part.excluded ? '<span class="swatch hatched"></span>'
                    : '<span class="swatch" style="background:' + part.colour + '"></span>') +
                '<span class="lname">' + esc(part.label) + (part.excluded ? ' (excluded)' : '') + '</span></div>';
        }).join('') + '</div>';
    }

    /* Grouped bars. Two series over the same categories and the same unit are
       drawn side by side, because the difference between them is what a reader
       came for and a stack would make one series start where the other ends.
       Both grow from the zero rule, and a bar whose figure the errors left
       partial is dashed like every other partial mark. */
    function groupedChart(spec) {
        var rows = spec.categories;
        var W = 820, left = 196, right = 96, top = 20, rowH = 40, axisH = 30;
        var H = top + rows.length * rowH + axisH;
        var plotW = W - left - right;
        var highest = 0;
        rows.forEach(function (row) {
            row.values.forEach(function (value) { highest = Math.max(highest, value); });
        });
        if (highest <= 0) highest = 1;

        function xOf(v) { return left + v / highest * plotW; }

        var out = '<svg class="plot-svg plot-grouped" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">';
        var axisY = H - axisH + 2;
        out += '<line class="axis" x1="' + left + '" y1="' + axisY + '" x2="' + (left + plotW) +
            '" y2="' + axisY + '"></line>';
        [0, highest].forEach(function (v, i) {
            out += '<text class="pl-tick" x="' + r(xOf(v)) + '" y="' + (H - axisH + 17) +
                '" text-anchor="' + (i === 0 ? 'start' : 'end') + '">' + esc(spec.tick(v)) + '</text>';
        });
        rows.forEach(function (row, i) {
            var y = top + i * rowH;
            out += '<text class="pl-lbl" x="' + (left - 10) + '" y="' + (y + rowH / 2 + 8) +
                '" text-anchor="end">' + esc(row.label) + '</text>';
            row.values.forEach(function (value, j) {
                var barH = 15;
                var by = y + 3 + j * (barH + 2);
                out += '<rect class="gbar ' + spec.series[j].id + (spec.series[j].partial ? ' partial' : '') +
                    '" x="' + left + '" y="' + by + '" width="' + r(Math.max(2, xOf(value) - left)) +
                    '" height="' + barH + '" fill="' + spec.series[j].colour + '" rx="2"><title>' +
                    esc(row.label + ' \u00b7 ' + spec.series[j].label + ': ' + spec.tick(value) +
                        (spec.series[j].partial ? ' (partial)' : '')) + '</title></rect>';
                out += '<text class="pl-val" x="' + r(Math.min(xOf(value) + 6, W - right + 4)) + '" y="' +
                    (by + barH - 3) + '">' + esc(spec.tick(value)) + '</text>';
            });
        });
        out += '</svg>';
        return out + seriesLegend(spec.series);
    }

    function seriesLegend(series) {
        return '<div class="legend titled"><span class="lhead">The two series</span>' +
            '<div class="legendrow">' + series.map(function (s) {
                return '<div class="lrow' + (s.partial ? ' excl' : '') + '">' +
                    '<span class="swatch" style="background:' + s.colour + '"></span>' +
                    '<span class="lname">' + esc(s.label) + (s.partial ? ' (partial)' : '') + '</span></div>';
            }).join('') + '</div></div>';
    }

    /* A scatter: two measures against each other, one point per category, so a
       category away from the rest is visible as distance and not as a row to
       find. Each point keeps its category's colour, and a point drawn from a
       partial figure is hollow, the same mark the other plots give it. */
    function scatterChart(spec) {
        var points = spec.points;
        var W = 820, left = 104, right = 40, top = 24, plotH = 250, bottom = 58;
        var H = top + plotH + bottom;
        var plotW = W - left - right;
        var xs = points.map(function (p) { return p.x; });
        var ys = points.map(function (p) { return p.y; });
        /* A scatter of measures that span two orders of magnitude is read on a
           log axis, and the axis says so. A value at zero cannot be placed on a
           log axis, and the page draws none: a zero Greek has no ratio to any
           other. */
        function decade(lo, hi) {
            return {
                lo: Math.floor(Math.log10(Math.max(lo, 1))),
                hi: Math.ceil(Math.log10(Math.max(hi, 10)))
            };
        }
        var xd = decade(Math.min.apply(null, xs), Math.max.apply(null, xs));
        var yd = decade(Math.min.apply(null, ys), Math.max.apply(null, ys));

        function xOf(v) { return left + (Math.log10(v) - xd.lo) / (xd.hi - xd.lo) * plotW; }
        function yOf(v) { return top + (yd.hi - Math.log10(v)) / (yd.hi - yd.lo) * plotH; }

        var out = '<svg class="plot-svg plot-scatter" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">';
        for (var yd1 = yd.lo; yd1 <= yd.hi; yd1 += 1) {
            out += '<line class="grid" x1="' + left + '" y1="' + r(yOf(Math.pow(10, yd1))) + '" x2="' +
                (left + plotW) + '" y2="' + r(yOf(Math.pow(10, yd1))) + '"></line>';
            out += '<text class="pl-tick" x="' + (left - 10) + '" y="' + (r(yOf(Math.pow(10, yd1))) + 4) +
                '" text-anchor="end">' + esc(spec.tickY(Math.pow(10, yd1))) + '</text>';
        }
        for (var xd1 = xd.lo; xd1 <= xd.hi; xd1 += 1) {
            out += '<line class="grid" x1="' + r(xOf(Math.pow(10, xd1))) + '" y1="' + top + '" x2="' +
                r(xOf(Math.pow(10, xd1))) + '" y2="' + (top + plotH) + '"></line>';
            out += '<text class="pl-tick" x="' + r(xOf(Math.pow(10, xd1))) + '" y="' + (top + plotH + 18) +
                '" text-anchor="middle">' + esc(spec.tickX(Math.pow(10, xd1))) + '</text>';
        }
        out += '<line class="axis" x1="' + left + '" y1="' + (top + plotH) + '" x2="' + (left + plotW) +
            '" y2="' + (top + plotH) + '"></line>';
        out += '<text class="pl-axis" x="' + (left + plotW / 2) + '" y="' + (H - 12) +
            '" text-anchor="middle">' + esc(spec.xLabel) + '</text>';
        out += '<text class="pl-axis" transform="translate(20,' + (top + plotH / 2) +
            ') rotate(-90)" text-anchor="middle">' + esc(spec.yLabel) + '</text>';

        /* One label per point, placed where it collides with neither another
           label nor another point's marker. The rule is tried first and the
           alternatives after it, so a crowded middle of the plot stays
           readable instead of stacking four names on one spot. */
        var placed = points.map(function (point) {
            return { point: point, cx: xOf(point.x), cy: yOf(point.y) };
        }).sort(function (a, b) { return a.cy - b.cy; });
        var taken = placed.map(function (item) {
            return { x: item.cx - 7, y: item.cy - 7, w: 14, h: 14 };
        });
        placed.forEach(function (item) {
            var point = item.point;
            var cy = r(item.cy);
            var cx = r(item.cx);
            var fill = point.partial ? 'none' : point.colour;
            out += '<circle class="sp' + (point.partial ? ' partial' : '') + '" cx="' + cx + '" cy="' + cy +
                '" r="6" fill="' + fill + '" stroke="' + (point.partial ? 'var(--bad)' : point.colour) +
                '" stroke-width="1.6"' + (point.partial ? ' stroke-dasharray="3 2"' : '') + '><title>' +
                esc(point.title) + '</title></circle>';
            var options = [
                { dx: 9, dy: 4, anchor: 'start' },
                { dx: -9, dy: 4, anchor: 'end' },
                { dx: 9, dy: -8, anchor: 'start' },
                { dx: -9, dy: -8, anchor: 'end' },
                { dx: 9, dy: 16, anchor: 'start' },
                { dx: -9, dy: 16, anchor: 'end' },
                { dx: -9, dy: -20, anchor: 'end' },
                { dx: 9, dy: -20, anchor: 'start' },
                { dx: -9, dy: -34, anchor: 'end' },
                { dx: 9, dy: -34, anchor: 'start' },
                { dx: -9, dy: 34, anchor: 'end' },
                { dx: 9, dy: 34, anchor: 'start' }
            ];
            var width = Math.max(40, point.label.length * 5.8);
            var box = null;
            for (var i = 0; i < options.length && !box; i += 1) {
                var option = options[i];
                var boxLeft = option.anchor === 'start' ? cx + option.dx : cx + option.dx - width;
                var candidate = {
                    x: boxLeft, y: cy + option.dy - 9, w: width, h: 13,
                    dx: option.dx, dy: option.dy, anchor: option.anchor
                };
                if (candidate.x < left + 2 || candidate.x + width > left + plotW + 34) continue;
                var clear = taken.every(function (other) {
                    return candidate.x + candidate.w < other.x - 2 || other.x + other.w < candidate.x - 2 ||
                        candidate.y + candidate.h < other.y - 1 || other.y + other.h < candidate.y - 1;
                });
                if (clear) box = candidate;
            }
            if (!box) {
                box = { x: cx + 9, y: cy - 4, w: width, h: 13, dx: 9, dy: 4, anchor: 'start' };
            }
            taken.push({ x: box.x, y: box.y, w: box.w, h: box.h });
            if (Math.abs(box.dy) > 20) {
                out += '<line class="leader" x1="' + cx + '" y1="' + cy + '" x2="' +
                    r(cx + box.dx * 0.4) + '" y2="' + r(cy + box.dy * 0.8) + '"></line>';
            }
            out += '<text class="pl-lbl" x="' + r(cx + box.dx) + '" y="' + r(cy + box.dy) +
                '" text-anchor="' + box.anchor + '">' + esc(point.label) + '</text>';
        });
        return out + '</svg>';
    }

    /* The error summary, drawn: one donut for the run's whole of problems and
       one stacked bar for the same parts across the runs in view. The donut
       earns the form because there is one whole, and the stacked bar earns its
       own because the comparison is two wholes, not one. */
    function scopeRows(errors, extra) {
        var order = ['Trade', 'Analytics', 'Curve', 'Configuration', 'Report'];
        var counts = {};
        errors.forEach(function (e) {
            counts[e.scope] = (counts[e.scope] || 0) + 1;
        });
        var rows = order.filter(function (scope) { return counts[scope]; }).map(function (scope) {
            return { label: scope, value: counts[scope], colour: scopeColour(scope) };
        });
        if (extra) rows.push(extra);
        return rows;
    }

    function errorsDonut(run, base) {
        var sc = scenarioFor(run);
        var lost = sc ? failedTrades(run).length : 0;
        var slices = scopeRows(run.errors, lost
            ? { label: 'excluded: ' + lost + ' trade' + (lost === 1 ? '' : 's') + ' not built',
                value: lost, excluded: true }
            : null);
        if (!slices.length) {
            return plotAbsent('No error plot for this run',
                'This run recorded no error and no warning, so there is no whole to divide. A donut of nothing ' +
                'is not drawn, and the empty error list above is the answer.');
        }
        var recorded = run.errors.length;
        var total = recorded + lost;
        var counts = slices.filter(function (s) { return !s.excluded; }).map(function (s) {
            return s.value + ' ' + s.label;
        }).join(', ');
        return plotFigure({
            title: 'Problems the run recorded, by scope' + (base ? ' \u00b7 run ' + run.short : ''),
            unit: 'count of error rows',
            partial: !!sc,
            caption: 'One whole, ' + slices.length + ' part' + (slices.length === 1 ? '' : 's') +
                ': this run\u2019s problems divided by scope, so the donut is the form and the hole carries the ' +
                'total. A bar chart would compare the counts and lose the whole they are parts of. Percentages ' +
                'and counts are in the legend, never on a slice. ' + recorded + ' error row' +
                (recorded === 1 ? '' : 's') + ': ' + counts + '.',
            excluded: lost
                ? 'The hatched slice is the ' + lost + ' trade' + (lost === 1 ? '' : 's') + ' the engine could ' +
                  'not build, drawn as an excluded slice rather than left out, so the whole of the pie is the whole of ' +
                  'what the run met and not a smaller set. ' + capitalise(partialFacts(run)) + '.'
                : ''
        }, donutChart({
            slices: slices,
            valueText: function (value) {
                return value + (value === 1 ? ' row' : ' rows');
            },
            holeValue: String(total),
            holeLabel: lost ? 'problems, incl. ' + lost + ' excluded' : 'problems recorded',
            aria: 'Donut: ' + counts + (lost ? ', and ' + lost + ' excluded trade' + (lost === 1 ? '' : 's') : '') +
                ', ' + total + ' in all. Each part\u2019s percentage and count are in the legend. ' +
                (lost ? 'The excluded trades are a hatched slice, because the whole is not subtly smaller than ' +
                    'the run.' : '')
        })
    );
    }

    function stackedErrors(run, base) {
        /* Every run on this page that recorded a problem, beside the two the
           comparison pairs: a stacked bar of one whole is a donut with a
           worse legend, so the form is earned by having the runs to compare. */
        var runs = [];
        if (base) runs.push(base);
        runs.push(run);
        RUNS.forEach(function (candidate) {
            if (candidate.errors.length) runs.push(candidate);
        });
        runs = runs.filter(function (item, index) {
            return runs.indexOf(item) === index;
        }).sort(function (a, b) { return a.startedAt < b.startedAt ? -1 : 1; });
        var parts = ['Trade', 'Analytics', 'Curve', 'Configuration', 'Report'];
        var bars = runs.map(function (item) {
            var counts = {};
            item.errors.forEach(function (e) { counts[e.scope] = (counts[e.scope] || 0) + 1; });
            return {
                label: item.short + (item === run ? ' \u00b7 this run'
                    : (item === base ? ' \u00b7 baseline' : ' \u00b7 ' + shortState(item))),
                segments: parts.filter(function (scope) { return counts[scope]; }).map(function (scope) {
                    return { label: scope, value: counts[scope], colour: scopeColour(scope) };
                })
            };
        });
        var none = runs.every(function (item) { return !item.errors.length; });
        if (none) return plotAbsent('No error plot for these runs',
            'Neither run in view recorded an error or a warning, so there is no part to stack. A stacked bar ' +
            'of nothing is not drawn, and the empty error list is the answer.');
        var series = parts.filter(function (scope) {
            return bars.some(function (bar) {
                return bar.segments.some(function (s) { return s.label === scope; });
            });
        }).map(function (scope) {
            return { label: scope, colour: scopeColour(scope) };
        });
        var partial = !!scenarioFor(run) || !!(base && base.errors.length);
        var summary = bars.map(function (bar) {
            return bar.label + ': ' +
                (bar.segments.length ? bar.segments.map(function (s) {
                    return s.value + ' ' + s.label;
                }).join(', ') : 'no problems');
        }).join('; ');
        return plotFigure({
            title: 'Problems by scope, run by run',
            unit: 'count of error rows',
            partial: partial,
            caption: 'Stacked bars, because there is more than one whole and each is split the same way: one ' +
                'bar per run, one segment per scope, the same colour for the same scope in both. A donut would ' +
                'show one run and hide the comparison. ' + summary + '.',
            excluded: partial
                ? 'A hatched segment would be a run whose error rows the page cannot count whole; no segment ' +
                  'here is hatched, because every error row of both runs is in the counts. The trades either ' +
                  'run could not build are named in the donut above, where a whole is drawn.' +
                  (scenarioFor(run) ? ' ' + capitalise(partialFacts(run)) + '.' : '')
                : ''
        }, stackedChart({
            bars: bars,
            aria: 'Stacked bars: problems by scope, run by run. ' + summary + '. Each bar is one run and each ' +
                'segment is one scope.'
        }) + stackedLegend(series));
    }

    function greekCompare(run) {
        var leaves = plottedLeaves(run);
        var complete = leaves.every(function (book) {
            return book.delta !== undefined && book.gamma !== undefined;
        });
        if (!complete || leaves.length < 2) return '';
        var sc = scenarioFor(run);
        var categories = leaves.map(function (book) {
            return {
                label: book.name,
                values: [book.delta, book.gamma]
            };
        });
        var summary = leaves.map(function (book) {
            return book.name + ' Delta ' + money(book.delta) + ', Gamma ' + money(book.gamma);
        }).join('; ');
        return plotFigure({
            title: 'Delta against Gamma, by book',
            unit: 'USD per 1 bp shift, both series',
            partial: !!sc,
            caption: 'Grouped bars, because Delta and Gamma share the same books and the same unit and the ' +
                'question is the two side by side: one stack would make Gamma start where Delta ends and the ' +
                'second series would be a length again, not a value. Gamma is the smaller Greek by ' +
                'construction, so its bars are shorter and its figure is printed beside each one. ' + summary + '.',
            excluded: sc
                ? 'Every bar is partial: ' + sc.noDelta + ' positions carry an NPV and no Delta, so both series ' +
                  'cover the positions the sensitivity analytic priced and no more. The bars are dashed for that ' +
                  'reason, and no bar here is the whole answer.'
                : ''
        }, groupedChart({
            categories: categories,
            series: [
                { id: 'delta', label: 'Delta', colour: 'var(--accent-bright)', partial: !!sc },
                { id: 'gamma', label: 'Gamma', colour: 'var(--warn)', partial: !!sc }
            ],
            tick: money,
            aria: 'Grouped bars: Delta and Gamma by book, in USD per 1 bp shift. ' + summary +
                '. Both series start at zero and are drawn side by side.'
        }));
    }

    function deltaGammaScatter(run) {
        var leaves = plottedLeaves(run);
        var complete = leaves.every(function (book) {
            return book.delta !== undefined && book.gamma !== undefined;
        });
        if (!complete || leaves.length < 2) return '';
        var sc = scenarioFor(run);
        var colour = bookColourFn(run);
        var points = leaves.map(function (book) {
            return {
                label: book.name,
                x: book.delta,
                y: book.gamma,
                colour: colour(book.name),
                partial: !!sc,
                title: book.name + ': Delta ' + money(book.delta) + ', Gamma ' + money(book.gamma) +
                    (sc ? ' (partial)' : '')
            };
        });
        var summary = points.map(function (p) {
            return p.label + ' Delta ' + money(p.x) + ', Gamma ' + money(p.y);
        }).join('; ');
        var biggest = points.slice().sort(function (a, b) { return b.x - a.x; })[0];
        var gPerD = points.map(function (p) {
            return { label: p.label, ratio: p.x ? p.y / p.x : 0 };
        }).sort(function (a, b) { return b.ratio - a.ratio; });
        return plotFigure({
            title: 'Gamma against Delta, one point per book',
            unit: 'Delta USD per 1 bp (across) \u00b7 Gamma USD per 1 bp (up)',
            partial: !!sc,
            caption: 'A scatter, because two measures are plotted against each other and the question is ' +
                'shape, not rank: the largest book by Delta is ' + biggest.label + ' at ' + money(biggest.x) +
                ', and ' + gPerD[0].label + ' carries the most Gamma for its Delta. A book away from the line ' +
                'the rest make is visible as distance, which two bar charts cannot show. Both axes are log ' +
                'decades, and the axis titles say so, because the books span two orders of magnitude and a ' +
                'linear axis would put six of the ten on top of each other. Every point keeps its book\u2019s ' +
                'colour from the other plots on this page.',
            excluded: sc
                ? 'Every point is partial and drawn hollow: ' + partialFacts(run) + ', so a point sits at the ' +
                  'Delta the surviving positions priced and not at the book\u2019s whole Delta.'
                : ''
        }, scatterChart({
            points: points,
            xLabel: 'Delta, USD per 1 bp, log scale',
            yLabel: 'Gamma, USD per 1 bp, log scale',
            tickX: shortMoney,
            tickY: shortMoney,
            aria: 'Scatter: Gamma against Delta, one point per book, Delta in USD per 1 bp across and Gamma in ' +
                'USD per 1 bp up, both on a log scale. ' + summary + (sc ? '. Every point is partial.' : '.')
        }));
    }

    /* A book's share of the run's NPV as one whole. It is the form the earlier
       version of this page refused for want of space: five or six books make a
       readable donut, fifteen would not. */
    function shareDonut(run) {
        var leaves = plottedLeaves(run);
        if (leaves.length < 2) return '';
        var sc = scenarioFor(run);
        var lost = sc ? failedTrades(run).length : 0;
        var colour = bookColourFn(run);
        var positive = leaves.filter(function (book) { return book.npv > 0; });
        var negative = leaves.filter(function (book) { return book.npv <= 0; });
        var sorted = positive.slice().sort(function (a, b) { return b.npv - a.npv; });
        var kept = sorted.slice(0, 6);
        var rest = sorted.slice(6);
        var slices = kept.map(function (book) {
            return { label: book.name, value: book.npv, colour: colour(book.name) };
        });
        if (rest.length) {
            slices.push({
                label: 'other: ' + rest.length + ' book' + (rest.length === 1 ? '' : 's'),
                value: rest.reduce(function (sum, book) { return sum + book.npv; }, 0),
                colour: 'var(--ink-faint)'
            });
        }
        if (lost) {
            slices.push({
                label: 'excluded: ' + lost + ' trade' + (lost === 1 ? '' : 's') + ' not built',
                value: 0,
                excluded: true,
                title: 'The engine could not build ' + lost + ' trade' + (lost === 1 ? '' : 's') +
                    ', so no NPV slice can carry their value: it is missing, not zero.'
            });
        }
        var whole = kept.concat(rest).reduce(function (sum, book) { return sum + book.npv; }, 0);
        var scope = plotScope(run);
        return plotFigure({
            title: 'Each book\u2019s share of the run\u2019s NPV' + (scope ? ' in ' + scope : ''),
            unit: 'USD, base currency, each book\u2019s share',
            partial: !!sc || negative.length > 0,
            caption: 'One whole, ' + (kept.length + (rest.length ? 1 : 0)) + ' part' +
                (kept.length + (rest.length ? 1 : 0) === 1 ? '' : 's') + ': the books that make up this ' +
                'run\u2019s NPV, so the donut is the form and the hole carries the total. The bar chart above ' +
                'compares the same books by amount; this one is read for the whole they make. Each book\u2019s ' +
                'percentage and value are in the legend.' +
                (rest.length ? ' The ' + rest.length + ' smallest book' + (rest.length === 1 ? '' : 's') +
                    ' after the sixth join one \'other\' slice rather than crowding the rim.' : '') +
                (negative.length ? ' ' + negative.length + ' book' + (negative.length === 1 ? '' : 's') +
                    ' carry no positive NPV and are not drawn, because a slice cannot be negative; their ' +
                    'figures are in the table below.' : ''),
            excluded: lost
                ? 'The hatched slice is the excluded one: the engine could not build ' + lost + ' trade' +
                  (lost === 1 ? '' : 's') + ', so their value is missing from the whole and is drawn as a ' +
                  'hatched label rather than dropped, and the pie is not silently a smaller set than the run: ' +
                  capitalise(partialFacts(run)) + '.'
                : ''
        }, donutChart({
            slices: slices,
            valueText: money,
            holeValue: shortMoney(whole),
            holeLabel: 'books\u2019 NPV',
            aria: 'Donut: each book\u2019s share of the run\u2019s NPV, in USD. ' +
                kept.map(function (book) {
                    return book.name + ' ' + money(book.npv) + ', ' + prettyPct(book.npv, whole);
                }).join('; ') +
                (rest.length ? '; other ' + money(rest.reduce(function (sum, book) {
                    return sum + book.npv;
                }, 0)) + ', ' + prettyPct(rest.reduce(function (sum, book) {
                    return sum + book.npv;
                }, 0), whole) : '') +
                (lost ? '; and ' + lost + ' excluded trades, drawn as a hatched slice' : '') +
                '. The hole carries the total and every percentage is in the legend.'
        }));
    }

    /* The books under the selected node: the same set the tree narrows to and the
       by-book table lists, so the plot, the tree and the table cannot disagree
       about which books are in view. */
    function plottedLeaves(run) {
        if (!bookTree(run)) return null;
        return bookLeaves(selectedBookNode(run));
    }

    function plotScope(run) {
        var node = selectedBookNode(run);
        return node && !treeIsRoot(run) ? node.name : null;
    }

    function npvPlot(run) {
        var leaves = plottedLeaves(run);
        var data = leaves.map(function (book) {
            return { label: book.name, value: book.npv, partial: book.lost > 0 };
        }).sort(function (a, b) { return b.value - a.value; });
        var lost = leaves.filter(function (book) { return book.lost > 0; });
        var scope = plotScope(run);
        return barChart({
            title: 'NPV by book' + (scope ? ' in ' + scope : ''),
            unit: 'USD, base currency',
            data: data,
            partial: lost.length > 0,
            caption: 'Horizontal bars, because a magnitude compared across books is what bars compare: sorted ' +
                'largest first, each bar from the zero rule, so a negative book would read as a bar left of ' +
                'zero. The book tree selection drives it, so choosing a node narrows the plot to that subtree ' +
                'exactly as it narrows the table.',
            excluded: lost.length
                ? 'The dashed bars are the books whose aggregate NPV excludes trades the engine could not ' +
                  'build: ' + lost.map(function (book) { return book.name; }).join(', ') + '. No bar here is a ' +
                  'book\u2019s whole answer: ' + partialFacts(run) + '.'
                : ''
        });
    }

    /* Delta and Gamma keep the bar form and are drawn as two charts, never stacked
       with NPV: an NPV and a Greek are different units, and so are the two Greeks.
       A run that configured no sensitivity analytic has neither, and says so
       rather than drawing an empty axis. */
    function greeksPlots(run) {
        var leaves = plottedLeaves(run);
        var complete = leaves.every(function (book) {
            return book.delta !== undefined && book.gamma !== undefined;
        });
        if (!complete) {
            return plotAbsent('No plot for Delta or Gamma',
                'This run\u2019s configuration names ' + analyticsText(run) + ', so it wrote no sensitivity.csv ' +
                'and took no Greek. A bar of a measure the run did not produce would be an invention, and the ' +
                'table below states exactly the measures the run does have.');
        }
        var sc = scenarioFor(run);
        var scope = plotScope(run);
        var noDelta = sc ? sc.noDelta : 0;

        function one(label, field, why) {
            var data = leaves.map(function (book) {
                return { label: book.name, value: book[field], partial: !!sc };
            }).sort(function (a, b) { return b.value - a.value; });
            return barChart({
                title: label + ' by book' + (scope ? ' in ' + scope : ''),
                unit: 'USD per 1 bp shift',
                data: data,
                partial: !!sc,
                caption: 'Horizontal bars, because a Greek across books is a magnitude compared across ' +
                    'categories; sorted largest first from the zero rule. Delta and Gamma are two charts and not ' +
                    'one, because they are different units from each other and from NPV. ' + why,
                excluded: sc
                    ? 'Every bar here is partial: ' + noDelta + ' positions carry an NPV and no ' + label +
                      ', because the sensitivity analytic skipped them. A bar covers only the positions that ' +
                      'priced, so no figure above it is the whole answer.'
                    : ''
            });
        }

        return one('Delta', 'delta', 'This is the first-order Greek this run took.') +
            one('Gamma', 'gamma', 'Gamma is the second-order Greek, so its bars are smaller than Delta\u2019s by ' +
                'construction and its axis is its own.');
    }

    /* The comparison, drawn. With a baseline in view the change is a chart of its
       own rather than a second series stacked into the level chart, because the
       two are different magnitudes on different scales; the level chart keeps the
       levels, this one carries the subtraction, and each says which it is. */
    function changePlot(run, base) {
        if (!base) return '';
        var leaves = plottedLeaves(run);
        var baseLeaves = bookLeaves(selectedBookNode(base));
        var partiality = comparisonPartiality(run, base);
        var data = leaves.map(function (book) {
            var before = baseLeaves.filter(function (candidate) {
                return candidate.name === book.name;
            })[0];
            if (!before) return null;
            return {
                label: book.name,
                value: book.npv - before.npv,
                partial: !!partiality || book.lost > 0 || before.lost > 0
            };
        }).filter(Boolean).sort(function (a, b) { return b.value - a.value; });
        var scope = plotScope(run);
        var excluded = '';
        if (partiality) {
            excluded = partiality.side === 'current'
                ? 'Every bar here is partial, because the run in view is a floor rather than a whole answer' +
                  (partiality.scenario ? ': ' + partialFacts(run) + '.' : '.') +
                  ' A change measured from a floor is a floor as well.'
                : 'Every bar here is partial, because the baseline completed carrying ' + base.errors.length +
                  ' problems, so each change is measured against a figure that is not the whole answer.';
        }
        return barChart({
            title: 'NPV change by book against run ' + base.short,
            unit: 'USD, this run minus the baseline',
            data: data,
            partial: !!partiality,
            caption: 'Horizontal bars, because the change of each book is a magnitude compared across books: ' +
                'signed, and drawn from a zero rule that is the point of no change, so a book that fell reads ' +
                'left of it. The level chart above keeps the levels; this one is the comparison' +
                (scope ? ' of ' + scope : ' across the books') + ', so no bar is read as a level.',
            excluded: excluded
        });
    }

    function trendSeries(definition) {
        return RUN_SERIES[definition] || [];
    }

    function seriesEntryRun(entry) {
        return RUNS.filter(function (candidate) {
            return candidate.short === entry.short;
        })[0] || null;
    }

    /* A series point is partial when the figures behind it are: an earlier run
       this page holds only as a series records how many problems it carried, and
       the run in view is partial when the results-with-errors scenario says so,
       which is the same rule the figures above use. */
    function seriesPointPartial(entry) {
        var named = seriesEntryRun(entry);
        if (named) return !!scenarioFor(named);
        return entry.errors > 0;
    }

    /* The exact numbers under the trend: one row per plotted run. A plot never
       replaces them. */
    function trendTable(run, base) {
        var series = trendSeries(run.definition);
        var rows = series.map(function (entry) {
            var named = seriesEntryRun(entry);
            var partial = seriesPointPartial(entry);
            var state;
            if (entry.short === run.short) state = 'this run';
            else if (base && entry.short === base.short) state = 'the baseline';
            else if (named) state = shortState(named);
            else state = entry.errors ? entry.errors + ' problems' : 'completed clean';
            return '<tr>' +
                '<td><span class="nm">' + esc(entry.short) + '</span></td>' +
                '<td class="nowrap">' + esc(entry.startedAt) + '</td>' +
                '<td class="num">' + esc(money(entry.npv)) +
                    (partial ? ' <span class="tag bad">partial</span>' : '') + '</td>' +
                '<td>' + esc(state) + '</td>' +
                '</tr>';
        }).join('');
        return '<div class="table-wrap"><table class="trendtable"><thead><tr>' +
            '<th>Run</th><th>Started</th><th class="num">NPV (USD)</th><th>State</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>';
    }

    function trendPlot(run, base) {
        var series = trendSeries(run.definition);
        if (series.length < 2) {
            return plotAbsent('No trend for this run',
                'This page holds one completed run of ' + run.definition + ', and one point is not a trend: a ' +
                'line through a single run would state a direction the data does not have. The figures above and ' +
                'the table beside them are the answer.');
        }
        var points = series.map(function (entry) {
            var partial = seriesPointPartial(entry);
            var current = entry.short === run.short;
            var isBaseline = !!base && entry.short === base.short;
            return {
                label: entry.short,
                at: stampMs(entry.startedAt),
                value: entry.npv,
                partial: partial,
                mark: current ? 'current' : (isBaseline ? 'baseline' : null),
                markLabel: current
                    ? entry.short + ' \u00b7 this run'
                    : entry.short + ' \u00b7 baseline',
                title: entry.short + ', started ' + entry.startedAt + ': ' + money(entry.npv) +
                    (partial ? ' (partial)' : '')
            };
        });
        var domain = lineDomain(points);
        var first = points[0];
        var last = points[points.length - 1];
        var marked = points.filter(function (point) { return point.mark === 'current'; })[0] || null;
        var partialRuns = series.filter(seriesPointPartial).map(function (entry) {
            return entry.short;
        });
        var excluded = '';
        if (partialRuns.length) {
            excluded = 'The dashed points and segments are runs whose figures are not the whole answer: ' +
                partialRuns.join(', ') + '. ' + (scenarioFor(run)
                    ? 'This run is one of them, and ' + partialFacts(run) + '.'
                    : 'Each of those runs completed carrying errors, so its NPV is a figure over the trades ' +
                      'that survived.') + ' A point plotted without that mark would read as a complete run.';
        }
        return lineChart({
            title: 'NPV across the runs of ' + run.definition,
            unit: 'USD, one run a day, base currency',
            points: points,
            xLabel: function (point) {
                return dayLabel(point.at) + ' ' + clockLabel(point.at);
            },
            caption: 'A line, because the runs of one definition in order are a change over time: ' +
                (marked
                    ? 'the run in view, ' + run.short + ', is the marked and named point'
                    : 'no point is marked, because the run in view is not one of the plotted runs') + ', and ' +
                (base
                    ? 'the baseline ' + base.short + ' is marked on the same axis, so the delta control has a ' +
                      'place in a series rather than an abstraction'
                    : 'with no comparison on, only this run is marked') + '. The points run from ' +
                dayLabel(first.at) + ' to ' + dayLabel(last.at) + ', and the value axis runs from ' +
                shortMoney(domain.lo) + ' to ' + shortMoney(domain.hi) +
                ' and does not start at zero, which the axis states rather than leaving it to the eye.',
            excluded: excluded
        }) + trendTable(run, base);
    }

    /* The plotted result. The bars sit directly above the by-book table that
       carries their numbers, so a plot and the figures a reader checks are one
       block; the trend follows that table with its own run table. A run whose
       figures the page cannot plot says which question the table answers
       instead. */
    function plotsPanel(run, base) {
        var leaves = plottedLeaves(run);
        var blocks = [];
        if (!bookTree(run)) {
            blocks.push(plotAbsent('No plot for this run\u2019s figures',
                'This run\u2019s result is read by legal entity, and a row of its table carries NPV, VaR and EAD ' +
                'in three units at once. No single bar chart compares three units, so the table below is the ' +
                'right answer and the page draws no bar of it.'));
        } else if (leaves.length < 2) {
            blocks.push(plotAbsent('No plot for one book',
                'The tree is narrowed to ' + leaves[0].name + ', and a chart of one bar states nothing a ' +
                'sentence cannot. Its figure is in the tiles above and its exact numbers are in the table below.'));
        } else {
            blocks.push(npvPlot(run));
            blocks.push(shareDonut(run));
            blocks.push(greeksPlots(run));
            blocks.push(greekCompare(run));
            blocks.push(deltaGammaScatter(run));
            blocks.push(changePlot(run, base));
        }
        /* The errors are a result of their own, so they are plotted whether or
           not this run's figures are: a run read by legal entity still recorded
           what went wrong. */
        blocks.push(errorsDonut(run, base));
        blocks.push(stackedErrors(run, base));
        return '<div class="plots" id="plots">' +
            '<div class="sectionhead" style="margin-top:22px"><h3 class="subhead">The result, plotted</h3>' +
            '<span class="meta"><span class="tag warn">fixture</span>' +
            '<span>inline SVG \u00b7 the same fixtures as the table below \u00b7 a plot never replaces the numbers</span>' +
            '</span></div>' + formRule() + blocks.join('') +
            '<p class="rowfoot">Every mark here is drawn from the fixtures the table below carries, and nothing ' +
            'reads a server: that by-book table lists every bar\u2019s exact value. No chart library is loaded or ' +
            'copied in \u2014 a prototype opens from <span class="mono">file://</span> with no build, no CDN and ' +
            'no network \u2014 so each plot is written as inline SVG in the prototype\u2019s own script.</p>' +
            '</div>';
    }

    /* The trend, placed under the by-book table with the run table that is its
       exact numbers. A series across runs answers a question the single run's
       table cannot, and it is what makes the delta control legible. */
    function trendPanel(run, base) {
        return '<div class="plots" id="trend-plots">' +
            '<div class="sectionhead" style="margin-top:22px"><h3 class="subhead">The runs behind this result</h3>' +
            '<span class="meta"><span class="tag warn">fixture</span>' +
            '<span>one point per completed run of ' + esc(run.definition) + ' \u00b7 the table under the plot carries the numbers</span>' +
            '</span></div>' + trendPlot(run, base) + '</div>';
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
        var failed = failedTrades(run).length;
        var scope = (!node || treeIsRoot(run))
            ? ''
            : ' The tree has narrowed it to <span class="mono">' + esc(node.name) +
              '</span>, which holds ' + num(node.trades) + ' trades.';
        var failedLine = (scenarioFor(run) && failed)
            ? ' <b>' + failed + ' of these rows carry no price.</b> Those are the trades the engine could not ' +
              'build: a trade it could not build writes no row into npv.csv, so its <span class="mono">NPV</span> ' +
              'and <span class="mono">NPV(Base)</span> cells are empty by nature and not by omission. The error ' +
              'the run recorded is their answer, and each row carries an <span class="mono">error</span> tag.'
            : '';
        return '<div class="notice warn"><strong>No operation parses the archive.</strong> ' +
            'This per-trade table is a fixture, and the columns are npv.csv\u2019s own: ' +
            '<span class="mono">TradeId, TradeType, NPV, NPV(Base), Notional, CounterParty</span>. ' +
            'Per-trade results are real \u2014 a run writes one row per trade into npv.csv inside its archive \u2014 ' +
            'but <span class="mono">ores_compute_results_tbl</span> carries only the archive\u2019s location and an ' +
            'outcome, and no operation parses the archive. A built screen cannot serve these rows until a result ' +
            'store or an archive read exists. It shows ' + view + ' of the ' + all + ' rows this page draws, ' +
            'and this page draws ' + all + ' of the ' + num(npvRows(run)) + ' rows npv.csv carries.' + scope +
            failedLine + '</div>';
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

        /* An analytic-scoped error keys no TradeId. Where the rows below are of
           that kind the panel says so, rather than implying a join the model does
           not store. */
        var analyticRows = errs.filter(function (e) { return e.subject !== 'trade'; });
        var scopeNote = analyticRows.length
            ? '<p class="dim">' + analyticRows.length + ' of these ' + errs.length + ' row' +
              (errs.length === 1 ? '' : 's') + ' ' + (analyticRows.length === 1 ? 'is' : 'are') +
              ' recorded against the analytic, not against the TradeId: an analytic-scoped error names the ' +
              'analytic that could not finish, so it keys no trade. This row is one of the positions that answer ' +
              'describes, and the panel does not invent a per-trade key for it.</p>'
            : '';

        return '<div class="tradedetail">' +
            '<div class="sectionhead" style="margin-top:16px"><h3 class="subhead">Trade ' +
            esc(t.id) + '</h3><span class="meta">' +
            (errs.length ? '<span class="tag bad">' + errs.length + ' error row' + (errs.length === 1 ? '' : 's') + '</span>'
                : '<span class="tag ok">no error recorded</span>') +
            '<span>joined to the run on TradeId</span></span></div>' +
            verdict + scopeNote +
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
        var failed = failedTrades(run).length;
        return '<p class="rowfoot">Six of the thirteen columns npv.csv carries. A trade the engine could not build ' +
            'writes no price into the file, so ' + (failed
                ? 'the ' + failed + ' row' + (failed === 1 ? '' : 's') + ' with an empty NPV ' +
                  (failed === 1 ? 'is' : 'are') + ' drawn from the run\u2019s own errors rather than from npv.csv, ' +
                  'and the error is the answer for ' + (failed === 1 ? 'it' : 'them') + '. '
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
            return '<button class="chipbtn' + (r.id === currentRun().id && !S.scenario ? ' on' : '') +
                '" data-act="run" data-run="' + r.id + '">' +
                '<span class="p">' + esc(r.party) + '</span>' + esc(r.report) +
                '<span class="an">' + esc(analyticsText(r)) + '</span>' +
                '<span class="st">\u00b7 ' + esc(shortState(r)) + '</span></button>';
        }).join('');
        /* The scenario has a chip of its own, beside the run chips and labelled,
           so a reviewer finds it deliberately rather than by accident. */
        var scenarioChips = Object.keys(SCENARIOS).map(function (id) {
            var sc = SCENARIOS[id];
            var run = runById(sc.runId);
            return '<button class="chipbtn scen' + (S.scenario === id ? ' on' : '') +
                '" data-act="scenario" data-scenario="' + esc(id) + '"' +
                ' title="Turn the scenario on or off">' +
                '<span class="p">scenario</span>' + esc(id) +
                '<span class="an">' + esc(run ? run.report : '') + '</span>' +
                '<span class="st">\u00b7 ' + esc(sc.chip) + '</span></button>';
        }).join('');
        return '<div class="fixturebar">' +
            '<span class="lbl">Fixture \u00b7 run in view</span>' + chips +
            '<span class="lbl">Scenario</span>' + scenarioChips +
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
            (scenarioFor(run) ? '<span class="statepill warn">results partial</span>' : '') +
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
        var sc = scenarioFor(run);
        return '<div class="notice warn"><strong>What is fixture.</strong> ' +
            (sc
                ? '<strong>The scenario is a fixture too.</strong> <span class="mono">' + esc(sc.id) +
                  '</span> is arranged on this page and read from no server: it books ' +
                  failedTrades(run).length + ' unbuildable bonds to books the tree already draws, and shows the ' +
                  'two <span class="mono">SENSITIVITY</span> error rows the run\u2019s own fixture records. ' +
                  'The run, the archive and the errors are the fixture the page already had; the scenario only ' +
                  'reads them together. '
                : '') +
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
            'joins each TradeId to trade_bookings.book_id through trading.v1.trade_bookings.list. ' +
            'The comparison is fixture twice over: the baseline run, its figures and its book tree are drawn from this ' +
            'file like the run\u2019s own, and every change is arithmetic on those two fixtures \u2014 no figure is mixed, ' +
            'scaled or averaged to stand in for another run. ' +
            'The artefacts link beside each package is a requirement on the platform rather than a working ' +
            'link: the application has no artefacts page, no route serves one, and no operation returns the ' +
            'package or the archive the link would open.' +
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

    /* The sentence that says what a partial result is missing, used by every
       variant so the banner, the footnote and the tree cannot disagree about the
       counts. */
    function partialFacts(run) {
        var sc = scenarioFor(run);
        if (!sc) return '';
        var failed = failedTrades(run).length;
        return 'the engine could not build ' + failed + ' trade' + (failed === 1 ? '' : 's') +
            ', so npv.csv carries no row for ' + (failed === 1 ? 'it' : 'them') +
            ', and ' + sc.noDelta + ' positions carry an NPV and no Delta';
    }

    /* The banner. It is not a modal and it carries no dismiss: the fact that
       these results are partial is not something a reader may put away. The
       button only shows the error rows the fact comes from, which is the same
       control the completed-with-errors notice offers. */
    function partialBanner(run) {
        var sc = scenarioFor(run);
        if (!sc) return '';
        var e = countBy(run.errors, 'error');
        var w = countBy(run.errors, 'warning');
        var counts = scopeCounts(run.errors, 'error') + (w ? ' and ' + scopeCounts(run.errors, 'warning') : '');
        var failed = failedTrades(run).length;

        return '<div class="notice partial" role="status">' +
            '<strong>This run completed, and its results are partial.</strong> ' +
            'The engine could not build <b>' + failed + ' trades</b>, and <b>' + sc.noDelta +
            ' positions</b> carry an NPV and no Delta. <b>The totals below exclude them.</b> ' +
            'Read every total on this page as a floor, not as the run\u2019s whole answer. ' +
            'The run recorded <b>' + run.errors.length + '</b> problems, <span class="mono">' + esc(counts) + '</span>:' +
            '<div class="inner-actions">' +
            '<button class="btn small" data-act="toggle-errors">' +
                (S.showErrors ? 'Hide the ' : 'Show the ') + run.errors.length + ' error row' +
                (run.errors.length === 1 ? '' : 's') +
            '</button>' +
            '<span class="hint">The errors that make these results partial belong to ' +
                '<em>Find out why a run went wrong</em>; they open here as a section of the same surface.</span>' +
            '</div>' +
            (S.showErrors ? errorTable(run) : '') +
            '</div>';
    }

    /* The notice above the results. With the scenario in view it is the partial
       banner, which says everything the completed-with-errors notice says and
       what the notice leaves out: that the numbers under it are incomplete. */
    function resultsNotice(run) {
        return scenarioFor(run) ? partialBanner(run) : errorBlock(run);
    }

    /* A marker on the figures, with a footnote. The number stays where it is: a
       reader who needs USD 88,402,114 gets it, and a reader who takes it for the
       whole answer is told otherwise on the figure itself. */
    function partialFiguresLine(run, what) {
        if (!scenarioFor(run)) return '';
        return '<p class="partialline"><span class="tag bad">partial</span>' +
            '<span>' + what + ': ' + partialFacts(run) + '. No total here is the run\u2019s whole answer.</span></p>';
    }

    function marksPartial(run, label) {
        var sc = scenarioFor(run);
        if (!sc || !treeIsRoot(run)) return false;
        return sc.markedFigures.indexOf(label) !== -1;
    }

    /* The comparison control. It offers no comparison and the completed earlier
       runs of this definition, and it prints the chosen baseline's instance id
       and start time beside the control, so a reader never has to guess which
       run a percentage is against. A run with no earlier instance says so rather
       than offering an empty list. */
    function compareBar(run, base, partiality) {
        var candidates = baselineCandidates(run);
        var options = '<option value="">No comparison</option>' + candidates.map(function (r) {
            return '<option value="' + esc(r.id) + '"' + (base && base.id === r.id ? ' selected' : '') + '>' +
                esc(r.id) + ' \u00b7 started ' + esc(r.startedAt) + '</option>';
        }).join('');
        var who;
        if (base) {
            who = '<span class="cmpwho">against run <span class="mono">' + esc(base.id) + '</span>' +
                ' \u00b7 started ' + esc(base.startedAt) + ' \u00b7 completed ' + esc(base.completedAt) + '</span>';
        } else if (candidates.length) {
            who = '<span class="cmpwho dim">No comparison: every change below is off.</span>';
        } else {
            who = '<span class="cmpwho dim">No earlier completed run of this definition, so there is nothing ' +
                'to compare against.</span>';
        }
        return '<div class="cmpbar">' +
            '<span class="lbl">Compare against</span>' +
            '<select class="cmpsel" data-act="baseline" aria-label="Compare against a previous run of this definition"' +
            (candidates.length ? '' : ' disabled') + '>' + options + '</select>' +
            who +
            (partiality ? '<span class="tag bad">partial</span>' : '') +
            '</div>';
    }

    /* Said once, in words, under the figures: what the baseline is, where both
       sets of numbers come from, and, when either side carried errors, that the
       change is partial and why. The percentage alone would not say any of it. */
    function comparisonNote(run, base, partiality) {
        if (!base) return '';
        var words = 'Every change on this panel is this run minus the baseline: run <span class="mono">' +
            esc(base.id) + '</span>, started ' + esc(base.startedAt) + ' and completed ' +
            esc(base.completedAt) + '. Both sides are the fixture figures of two runs of <span class="mono">' +
            esc(run.definition) + '</span>, so a book that moved by a different percentage from another moved by ' +
            'a different amount in the fixture. No figure is scaled to make the baseline: a multiplier per ' +
            'measure would move every book by the same percentage, and the change would be fiction.';
        if (!partiality) return '<p class="cmpnote"><span class="tag ok">both sides complete</span><span>' +
            words + '</span></p>';
        var reason;
        if (partiality.side === 'current' && partiality.scenario) {
            reason = 'The changes are partial. The current run completed carrying ' + run.errors.length +
                ' problems, and ' + partialFacts(run) + '. The banner above calls its totals floors for that ' +
                'reason, and a change measured from a floor is partial too, wherever it appears on this panel.' +
                (base.errors.length || scenarioFor(base)
                    ? ' The baseline carries errors of its own as well.'
                    : ' The baseline completed clean.');
        } else if (partiality.side === 'current') {
            reason = 'The changes are partial. This run completed carrying ' + run.errors.length + ' problems (' +
                errorCountsText(run.errors) + '), so its figures are not the whole answer and a change computed ' +
                'from them cannot be either. The results-with-errors scenario shows what those errors leave out.';
        } else if (partiality.side === 'baseline' && partiality.scenario) {
            reason = 'The changes are partial. The baseline run <span class="mono">' + esc(base.id) +
                '</span> completed carrying ' + base.errors.length + ' problems, and ' + partialFacts(base) +
                ', so every change is measured against a figure that is not the whole answer.';
        } else {
            reason = 'The changes are partial. The baseline run <span class="mono">' + esc(base.id) +
                '</span> completed carrying ' + base.errors.length + ' problems, so every change is measured ' +
                'against a partial baseline.';
        }
        return '<p class="cmpnote"><span class="tag bad">partial</span><span>' + reason + ' ' + words + '</span></p>';
    }

    function errorCountsText(errors) {
        var w = countBy(errors, 'warning');
        return scopeCounts(errors, 'error') + (w ? ' and ' + scopeCounts(errors, 'warning') : '');
    }

    /* A treats the figures, the book tree and the tables as one thing: the run's
       result is a set of numbers, the tree is how they roll up from a trade to a
       book to a portfolio, and the per-trade table under it is the same result one
       trade at a time. Selecting a node changes the figures and the rows together. */
    function tableCells(row, t, isTotal) {
        return row.map(function (cell, i) {
            var classes = i > 0 ? ['num'] : [];
            if (isTotal) classes.push('totalcell');
            if (t.changeAt !== undefined && i >= t.changeAt && /^[+-]/.test(cell)) {
                classes.push(cell.charAt(0) === '-' ? 'down' : 'up');
            }
            return '<td' + (classes.length ? ' class="' + classes.join(' ') + '"' : '') + '>' +
                esc(cell) + '</td>';
        }).join('');
    }

    function resultCard(run) {
        var base = baselineFor(run);
        var partiality = comparisonPartiality(run, base);
        var cmp = base ? { base: base, partial: !!partiality } : null;

        var tiles = tilesFor(run, base).map(function (f) {
            var partial = marksPartial(run, f.label);
            var badge = base ? deltaBadge(f.current, f.base, f.style, !!partiality, base) : '';
            var note = f.note + (base && f.baseValue !== null && f.baseValue !== undefined
                ? ' \u00b7 vs ' + f.baseValue + ' on run ' + base.short
                : '');
            return '<div class="fig' + (partial ? ' partial' : '') + '">' +
                '<div class="v">' + esc(f.value) + '</div>' +
                '<div class="l">' + esc(f.label) + (partial ? ' <span class="tag bad">partial</span>' : '') + '</div>' +
                '<div class="n">' + esc(note) + '</div>' +
                (badge ? '<div class="d">' + badge + '</div>' : '') +
                '</div>';
        }).join('');

        var t = bookTableData(run, base);
        var table = '';
        if (t) {
            var head = t.columns.map(function (c, i) {
                return '<th' + (i > 0 ? ' class="num"' : '') + '>' + esc(c) + '</th>';
            }).join('');
            var body = t.rows.map(function (row) {
                return '<tr>' + tableCells(row, t) + '</tr>';
            }).join('');
            if (t.total) body += '<tr class="totalrow">' + tableCells(t.total, t, true) + '</tr>';
            table = '<div class="sectionhead" style="margin-top:20px"><h3 class="subhead">' + esc(t.title) + '</h3>' +
                '<span class="meta">' + (partiality ? '<span class="tag bad">partial</span>' : '') +
                '<span class="tag warn">fixture</span><span>' + esc(t.note) + '</span></span></div>' +
                '<div class="table-wrap"><table><thead><tr>' + head + '</tr></thead><tbody>' + body + '</tbody></table></div>';
        }

        return '<section class="card">' +
            '<header class="sectionhead"><h2>The result</h2>' +
            '<span class="meta"><span class="tag accent">' + esc(analyticsText(run)) + '</span>' +
            '<span class="tag warn">fixture</span><span>no measure below comes from any other analytic</span></span></header>' +
            analyticsLine(run) +
            compareBar(run, base, partiality) +
            '<div class="figs" style="margin-top:16px">' + tiles + '</div>' +
            comparisonNote(run, base, partiality) +
            partialFiguresLine(run, 'The four figures above are partial') +
            bookTreeBlock(run, cmp) + plotsPanel(run, base) + table + trendPanel(run, base) +
            (tradesFor(run).length ? tradesHead(run) + tradesBlock(run) : '') +
            '<p class="rowfoot">No operation returns these figures or rows. They are what the archive says once something ' +
            'parses it \u2014 <span class="mono">' + esc(analyticsText(run)) + '</span> wrote them \u2014 and the result ' +
            'store that would parse it is designed and not built. A change needs both sides parsed, so it waits on the ' +
            'same read twice.</p>' +
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
            artefactsLink(run.key, 'output package') +
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
                '<button class="btn small" data-act="open-package">Open package</button>' +
                artefactsLink(run.preparedInputKey, 'input package') + '</div>' +
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
            artefactsLink(run.definitionId + '/' + run.id + '/input', 'input package') +
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
        return saidBlock(run) + resultsNotice(run) + resultCard(run) +
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
            artefactsLink(run.key, 'output package') +
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
            partialFiguresLine(run, 'The results in this archive are partial') +
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
        return saidBlock(run) + resultsNotice(run) +
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
                    ? 'the ' + carrying + ' of these rows that ' + (carrying === 1 ? 'carries' : 'carry') +
                      ' an error the run recorded against ' + (carrying === 1 ? 'it' : 'them')
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
        return resultsNotice(run) +
            '<div class="split">' +
            '<section class="card">' +
            '<div class="said"><span class="lbl">What the system said</span>' +
            '<span class="q">' + (run.state === 'running' ? '\u2014 no output message yet' : esc(run.outputMessage)) + '</span></div>' +
            analyticsLine(run) +
            partialFiguresLine(run, 'The figures in this summary are partial') +
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
            (S.scenario ? '<span class="dot">\u00b7</span>scenario <b>' + esc(S.scenario) + '</b>' : '') +
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
        if (!run) return;
        if (run.id === S.runId && !S.scenario) return;
        S.runId = run.id;
        S.scenario = null;
        S.showErrors = false;
        S.trade = null;
        S.file = 'npv.csv';
        S.note = null;
        S.book = null;
        S.collapsed = [];
        render();
        writeParams();
    }

    /* The scenario chip turns the scenario on, and the same chip turns it off
       again, because a reviewer wants the plain run back without hunting for it.
       A scenario names its run, so turning it on brings that run into view. */
    function setScenario(id) {
        if (id && !SCENARIOS[id]) return;
        S.scenario = S.scenario === id ? null : id;
        if (S.scenario) S.runId = SCENARIOS[S.scenario].runId;
        S.showErrors = false;
        S.trade = null;
        S.note = null;
        S.book = null;
        S.collapsed = [];
        S.file = 'npv.csv';
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

    /* Turning the comparison off is a choice the page keeps: S.baseline is null
       and stays null until the person picks a run again. Choosing a run is only
       possible among the candidates the control offered. */
    function setBaseline(id) {
        var run = currentRun();
        if (!id) {
            S.baseline = null;
        } else if (baselineCandidates(run).some(function (r) { return r.id === id; })) {
            S.baseline = id;
        } else {
            return;
        }
        S.note = null;
        render();
        writeParams();
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

    /* A select makes its own click and keyboard behaviour, so it is not taken by
       the click handler; its change event is the one that matters. */
    document.addEventListener('change', function (ev) {
        if (!ev.target.closest || !ev.target.closest('[data-act="baseline"]')) return;
        setBaseline(ev.target.value);
    });

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'baseline') return;
        ev.preventDefault();

        if (act === 'variant') {
            setVariant(el.getAttribute('data-variant'));
        } else if (act === 'run') {
            setRun(el.getAttribute('data-run'));
        } else if (act === 'scenario') {
            setScenario(el.getAttribute('data-scenario'));
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
        /* A scenario names the run it is arranged on, so it wins over ?run=. */
        var sc = p.get('scenario');
        if (sc && SCENARIOS[sc]) {
            S.scenario = sc;
            S.runId = SCENARIOS[sc].runId;
        }
        var f = p.get('file');
        if (f && filesFor(currentRun()).some(function (x) { return x.name === f; })) S.file = f;
        var t = p.get('trade');
        if (t && tradeById(currentRun(), t)) S.trade = t;
        var b = p.get('book');
        if (b && findNode(bookTree(currentRun()), b)) S.book = b;
        /* ?baseline=none turns the comparison off; an instance id picks the run;
           no parameter leaves the previous run in view. */
        var bl = p.get('baseline');
        if (bl === 'none') S.baseline = null;
        else if (bl) S.baseline = bl;
        if (p.get('errors') === '1') S.showErrors = true;
    }

    function writeParams() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('run', currentRun().id);
        if (S.scenario) p.set('scenario', S.scenario);
        if (S.baseline === null) p.set('baseline', 'none');
        else if (typeof S.baseline === 'string') p.set('baseline', S.baseline);
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

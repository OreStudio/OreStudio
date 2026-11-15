// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { datasetQuery, datasetConventions, fxDataset } from './fx-e2e-dataset.mjs';

const turn = (message, tool, { statuses, checks, contains, ...options } = {}) => ({
    message,
    expect: {
        tool,
        statuses,
        checks: Object.entries(checks ?? {}).map(([path, equals]) => ({ path, equals })),
        contains,
    },
    ...options,
});
const ok = (message, tool, checks, contains) =>
    turn(message, tool, { statuses: ['ok'], checks, contains });
const local = (message, contains) => turn(message, undefined, { contains });
const entry = (id, ...turns) => ({ id, turns });
const status = (statuses) => ({ statuses });
const check = (checks) => ({ checks });
const confirmation = (pattern) => ({ capture: 'confirmation', pattern });

export async function nativeCases(f, tenantId, partyId) {
    const fx = fxDataset(tenantId, partyId);
    const spotId = fx.series[0].id;
    const {
        book,
        portfolio,
        counterparty,
        trade,
        forwardTrade,
        bondTrade,
        bondIssue,
        run,
        partialRun,
        riskRun,
        date,
        riskDate,
        definition,
        riskDefinition,
        bookingDate,
        forwardDate,
        swapEndDate,
        ir,
        expected,
        ambiguousBook,
        scenario,
    } = f;
    const lists = [
        entry(
            'configured-currencies',
            turn('List the first three configured currencies.', 'list_currencies', {
                assertions: ['rows'],
            }),
        ),
        entry(
            'books',
            ok('List the books available to my selected party.', 'find_books'),
            ok('Find the book named "' + book + '".', 'find_books'),
        ),
        entry(
            'book-clarification',
            turn(
                'Show trades in book ' + ambiguousBook + '.',
                'find_trades',
                status(['ambiguous']),
            ),
            ok('Use ' + book + '.', 'find_trades'),
        ),
        entry(
            'portfolios',
            ok('List the portfolios available to my selected party.', 'find_portfolios'),
            ok('Find the portfolio named "' + portfolio + '".', 'find_portfolios'),
        ),
        entry(
            'counterparty',
            turn('Find counterparty ' + counterparty + '.', 'find_counterparties', {
                statuses: ['ok', 'partial'],
                assertions: ['rows'],
            }),
        ),
        entry(
            'trade-search',
            ok('Find trade ' + trade + '.', 'find_trades', { 'rows.0.id': trade }),
            ok('Find trades of type FxForward.', 'find_trades'),
            ok('Find the trade with origin reference ' + f.originReference + '.', 'find_trades', {
                'rows.0.id': trade,
            }),
        ),
        entry('book-trades', ok('Show trades in book "' + book + '".', 'find_trades')),
        entry(
            'portfolio-trades',
            ok('Show trades in portfolio "' + portfolio + '".', 'find_trades'),
        ),
        entry(
            'trade-details',
            ok('Show booking details and portfolio links for trade ' + trade + '.', 'read_trade', {
                'trade.id': trade,
            }),
        ),
        entry(
            'forward-economics',
            ok('Show booked economics for trade ' + forwardTrade + '.', 'read_trade', {
                'trade.id': forwardTrade,
                economicsStatus: 'available',
            }),
        ),
        entry(
            'bond-economics',
            ok('Show the issue reference and notional for trade ' + bondTrade + '.', 'read_trade', {
                'trade.id': bondTrade,
                economicsStatus: 'issue_reference_only',
                'economics.issue_id': bondIssue,
            }),
        ),
    ];
    const market = [
        entry(
            'market-series',
            turn('List the first page of stored market data series.', 'list_market_series', {
                assertions: ['rows'],
            }),
        ),
        entry(
            'stored-observations',
            turn(
                'Show the latest observation for market data series ' + spotId + '.',
                'read_market_observations',
                { assertions: ['rows'] },
            ),
        ),
        entry(
            'latest-spot',
            turn('What is the latest recorded EUR/USD spot from agent-e2e?', 'read_fx_spot', {
                statuses: ['stale'],
                checks: { 'observation.value': '1.0552', units: 'USD per 1 EUR' },
            }),
        ),
        entry(
            'dated-spot',
            ok('What was EUR/USD spot from provider agent-e2e on 2016-12-30?', 'read_fx_spot', {
                'observation.value': '1.0552',
                'series.seriesId': spotId,
            }),
        ),
        entry(
            'spot-provider-choice',
            turn('What was EUR/USD spot on 2016-12-30?', 'read_fx_spot', status(['ambiguous'])),
            ok('Please go with agent-e2e for that one.', 'read_fx_spot', {
                'observation.value': '1.0552',
                'series.seriesId': spotId,
            }),
        ),
        entry(
            'rate-series-choice',
            turn(
                'What was the six-month GBP rate on ' + ir.date + '?',
                'read_ir_rate',
                status(['ambiguous']),
            ),
            ok('Use option {{depositOption}}.', 'read_ir_rate', {
                'observation.value': ir.deposit.value,
                'series.seriesId': ir.deposit.id,
            }),
        ),
        entry(
            'swap-rate',
            ok(
                'What was the 5Y GBP SONIA swap rate with floating tenor 3M and forward start 1Y from provider ' +
                    ir.swap.provider +
                    ' on ' +
                    ir.date +
                    '?',
                'read_ir_rate',
                {
                    'observation.value': ir.swap.value,
                    'series.seriesId': ir.swap.id,
                    units: 'decimal rate',
                },
            ),
        ),
        entry(
            'deposit-rate',
            ok(
                'What was the six-month GBP money-market deposit rate from provider ' +
                    ir.deposit.provider +
                    ' on ' +
                    ir.date +
                    '?',
                'read_ir_rate',
                { 'observation.value': ir.deposit.value, 'series.seriesId': ir.deposit.id },
            ),
        ),
        entry(
            'zero-rate',
            ok(
                'What was the six-month GBP zero rate on curve "' +
                    ir.zero.curve +
                    '" from provider ' +
                    ir.zero.provider +
                    ' on ' +
                    ir.date +
                    '?',
                'read_ir_rate',
                {
                    'observation.value': ir.zero.value,
                    'series.seriesId': ir.zero.id,
                    units: 'decimal rate',
                },
            ),
        ),
        entry(
            'discount-factor',
            ok(
                'What was the six-month GBP discount factor on curve "' +
                    ir.discount.curve +
                    '" from provider ' +
                    ir.discount.provider +
                    ' on ' +
                    ir.date +
                    '?',
                'read_ir_rate',
                {
                    'observation.value': ir.discount.value,
                    'series.seriesId': ir.discount.id,
                    units: 'discount factor',
                },
            ),
        ),
        entry(
            'stored-forward-curve',
            turn(datasetQuery, 'build_fx_curve_from_market', status(['selection_required'])),
            turn(datasetConventions, 'build_fx_curve_from_market', {
                statuses: ['ok'],
                assertions: ['fx-reference'],
            }),
        ),
    ];
    market.find((c) => c.id === 'rate-series-choice').turns[0].captureOption = {
        name: 'depositOption',
        seriesId: ir.deposit.id,
    };
    const reports = [
        entry(
            'report-runs',
            turn('List existing report runs.', 'find_report_runs', {
                statuses: ['ok'],
                assertions: ['runs'],
            }),
        ),
        entry(
            'completed-runs',
            turn('List the latest completed report runs.', 'find_report_runs', {
                statuses: ['ok'],
                assertions: ['runs'],
            }),
        ),
        entry(
            'successful-runs',
            turn(
                'List the latest successful report runs named "' +
                    definition +
                    '" on ' +
                    date +
                    '.',
                'find_report_runs',
                { statuses: ['ok'], assertions: ['runs'] },
            ),
        ),
        entry(
            'trade-runs',
            turn(
                'Which report runs contain stored results for trade ' + trade + '?',
                'find_report_runs',
                { statuses: ['ok'], assertions: ['runs'] },
            ),
        ),
        entry(
            'run-status',
            ok(
                'What is the status and actual pricing date of run ' + run + '?',
                'read_run_status',
                { 'run.id': run },
            ),
        ),
        entry(
            'npv-definition-choice',
            turn('Run a new NPV report.', 'review_npv_run', status(['definition_required'])),
        ),
        entry(
            'trade-native-npv',
            turn(
                'What is the native NPV of trade ' + trade + ' from run ' + run + '?',
                'read_trade_results',
                { statuses: ['ok', 'stale'], checks: expected.tradeNative },
            ),
        ),
        entry(
            'trade-base-npv',
            ok(
                'What is the base NPV of trade ' + trade + ' from run ' + run + ' on ' + date + '?',
                'read_trade_results',
                expected.tradeBase,
            ),
        ),
        entry(
            'trade-partial-npv',
            turn(
                'Inspect partial native NPV of trade ' + trade + ' from run ' + partialRun + '.',
                'read_trade_results',
                { statuses: ['partial'], checks: expected.tradePartial },
            ),
        ),
        ...['book', 'portfolio'].flatMap((scope) =>
            ['native', 'base'].map((measure) =>
                entry(
                    scope + '-' + measure + '-npv',
                    ok(
                        'Show the ' +
                            measure +
                            ' NPV for ' +
                            scope +
                            ' "' +
                            f[scope] +
                            '" from run ' +
                            run +
                            ' on ' +
                            date +
                            '.',
                        'summarize_scope_results',
                        expected[scope + (measure === 'base' ? 'Base' : 'Native')],
                    ),
                ),
            ),
        ),
        ...['book', 'portfolio'].map((scope) =>
            entry(
                'latest-' + scope + '-contributors',
                turn(
                    'Show the latest complete NPV for my ' + scope + ' and its top 1 contributor.',
                    'find_' + scope + 's',
                ),
                turn('Use ' + f[scope] + '.', 'summarize_scope_results', {
                    statuses: ['ok', 'stale'],
                    checks: { selectedLatest: true },
                }),
                turn('Next contributors.', 'summarize_scope_results', {
                    statuses: ['ok', 'stale'],
                }),
            ),
        ),
        entry(
            'rate-greeks',
            ok(
                'Show Delta, Gamma and DV01 for book "' +
                    book +
                    '" from run ' +
                    riskRun +
                    ' on ' +
                    riskDate +
                    ' for factor ' +
                    f.rateFactor +
                    '.',
                'summarize_scope_results',
                expected.rateGreeks,
            ),
        ),
        entry(
            'fx-vega',
            turn(
                'Show EUR/USD three-month vega with top 1 contributor for portfolio "' +
                    portfolio +
                    '" from run ' +
                    riskRun +
                    ' on ' +
                    riskDate +
                    '.',
                'summarize_scope_results',
                status(['selection_required']),
            ),
            ok('Use option ' + f.vegaOption + '.', 'summarize_scope_results', expected.vega),
            ok('Next contributors for bucket 1.', 'summarize_scope_results'),
        ),
        entry(
            'greek-buckets',
            ok(
                'Show the stored Greeks for portfolio "' +
                    portfolio +
                    '" from run ' +
                    riskRun +
                    ' on ' +
                    riskDate +
                    ' for scenario "' +
                    scenario +
                    '".',
                'summarize_scope_results',
                expected.scenario,
            ),
            ok('Next buckets.', 'summarize_scope_results'),
        ),
    ];
    const fxDraft =
        'Draft a buy of 20M EUR/USD six-month forward with ' +
        counterparty +
        ' in book "' +
        book +
        '".';
    const workflows = [
        ...['trade', 'book', 'portfolio', 'risk'].map((scope) => {
            const def = scope === 'risk' ? riskDefinition : definition;
            const kind = scope === 'risk' ? 'report' : 'NPV report';
            const request =
                'Run the ' +
                kind +
                ' using definition "' +
                def +
                '" on ' +
                (scope === 'risk' ? riskDate : date) +
                (scope === 'risk'
                    ? ''
                    : ', then show results for ' + scope + ' "' + f[scope] + '".');
            return entry(
                'execute-report-' + scope,
                turn(request, 'review_npv_run', {
                    statuses: ['confirmation_required'],
                    ...confirmation(/Confirm (?:NPV|report) run [a-f0-9-]{36}/i),
                    captureDefinition: true,
                }),
                { ...local('{{confirmation}}', ['OreStudio recorded run']), poll: scope },
                local('Check NPV submission for definition {{definitionId}}', [
                    'no unresolved submission',
                ]),
            );
        }),
        entry(
            'bond-draft',
            turn(
                'Draft a purchase of 10M of bond issue ' +
                    bondIssue +
                    ' at the agreed clean price of 99 percent of par with ' +
                    counterparty +
                    ' in book "' +
                    book +
                    '".',
                'review_bond_draft',
                { checks: { 'issue.issue_id': bondIssue } },
            ),
        ),
        entry(
            'discard-draft',
            turn(fxDraft, 'review_fx_forward_draft', status(['needs_information'])),
            {
                ...local('Discard draft', ['discarded']),
                booking: { product: 'FxForward', delta: 0 },
            },
        ),
        entry(
            'book-fx-forward',
            turn(fxDraft, 'review_fx_forward_draft', status(['needs_information'])),
            turn(
                'Trade date ' +
                    bookingDate +
                    ', agreed value date ' +
                    forwardDate +
                    ', physical settlement, agreed rate 1.0835.',
                'review_fx_forward_draft',
                { statuses: ['ready_for_review'] },
            ),
            {
                ...turn('{{bookingConfirmation}}', 'book_fx_forward', status(['booked'])),
                booking: { product: 'FxForward', delta: 1 },
            },
        ),
        entry(
            'book-sofr-swap',
            turn(
                'Pay fixed, receive floating on a 100M USD five-year SOFR swap with ' +
                    counterparty +
                    ' in book "' +
                    book +
                    '".',
                'review_interest_rate_swap_draft',
                status(['needs_information']),
            ),
            {
                editSwap: {
                    fixedDirection: 'pay',
                    floatDirection: 'receive',
                    notional: '100000000',
                    currency: 'USD',
                    floatingIndex: 'USD-SOFR',
                    fixedRate: '0.0425 decimal',
                    spread: '0 decimal',
                    startDate: bookingDate,
                    maturityDate: swapEndDate,
                    fixedFrequency: 'Annual',
                    floatFrequency: 'Semiannual',
                    fixedDayCount: 'A360',
                    floatDayCount: 'A360',
                    fixedPaymentLag: '0',
                    floatPaymentLag: '0',
                    calendar: f.swapCalendar,
                    adjustment: 'Following',
                    maturityAdjustment: 'Following',
                    scheduleRule: 'Forward',
                    endOfMonth: 'no',
                    fixingDays: '0',
                    fixingCalendar: f.swapCalendar,
                    tradeDate: bookingDate,
                    book,
                    counterparty,
                },
                expect: { tool: 'review_interest_rate_swap_draft', statuses: ['ready_for_review'] },
            },
            {
                ...turn('{{bookingConfirmation}}', 'book_interest_rate_swap', status(['booked'])),
                booking: { product: 'Swap', delta: 1 },
            },
        ),
    ];
    const help = [
        'How do I create a book in the web app?',
        'Where can I download report results?',
    ].map((message, index) =>
        entry('help-' + (index + 1), turn(message, 'search_help', { assertions: ['help'] })),
    );
    const analytics = JSON.parse(
        await readFile(new URL('./fx-analytics.json', import.meta.url), 'utf8'),
    ).cases.map((c) => ({
        id: c.id,
        turns: c.turns.map(({ message, expect }) => ({
            message:
                c.id === 'fx-curve-nats-reference'
                    ? message
                          .replace('11111111-1111-4111-8111-000000000030', spotId)
                          .replace('LABELLED EVALUATION FIXTURE', fx.observations[0].source)
                    : message,
            expect,
            analytics: c.id,
        })),
    }));
    return { reads: [...lists, ...market, ...reports, ...analytics, ...help], workflows };
}

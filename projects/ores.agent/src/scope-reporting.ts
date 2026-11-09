// SPDX-License-Identifier: GPL-3.0-or-later
import { markdownTable } from './markdown.js';
import { z } from 'zod';
import { readPages } from './read-pages.js';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { marketId, observationTime } from './market.js';
import { summarizeSensitivities, formatSensitivities } from './sensitivity.js';
import { decimalSum, decimalParts } from './decimal.js';
export { decimalSum, decimalParts } from './decimal.js';

export const scopeTools = {
    summarize_scope_results: {
        description:
            'Read stored NPV or raw Greek buckets for a book/booking-portfolio subtree at an explicit run start. Preserve literal scope and run UUID. Default npv_base uses npv.csv and explicit base units. Greeks/delta/gamma/vega/dv01 use sensitivity.csv cash finite differences grouped by exact factor, target shifts, scenario and currency. Never sum different buckets or infer missing as zero. Vega selects volatility Delta; dv01 selects rate Delta at a 0.0001 bump with signed engine convention. Optional literal factor/factor2/scenario select buckets; bucketOffset/bucketLimit page buckets; offset/limit page contributors. Totals require one figure per expected trade in a bucket.',
        schema: z
            .object({
                scopeType: z.enum(['book', 'portfolio']),
                scope: z.string().min(1).max(120),
                runId: marketId.transform((v) => v.toLowerCase()).optional(),
                measure: z
                    .enum(['npv', 'npv_base', 'greeks', 'delta', 'gamma', 'vega', 'dv01'])
                    .default('npv_base'),
                currency: z
                    .string()
                    .regex(/^[A-Za-z]{3}$/)
                    .transform((s) => s.toUpperCase())
                    .optional(),
                factor: z.string().min(1).max(256).optional(),
                factor2: z.string().max(256).optional(),
                scenario: z.string().max(120).optional(),
                bucketOffset: z.number().int().min(0).max(2000).default(0),
                bucketLimit: z.number().int().min(1).max(10).default(5),
                offset: z.number().int().min(0).max(1000).default(0),
                limit: z.number().int().min(1).max(25).default(10),
                valuationDate: z.iso.date().optional(),
                maxAgeDays: z.number().int().min(0).max(365).default(3),
            })
            .strict(),
    },
} as const;

const uuid = marketId.transform((v) => v.toLowerCase());
const scoped = { id: uuid, tenant_id: uuid, party_id: uuid };
const dated = { recorded_at: z.string() };
const anchor = z.object({ ...scoped, ...dated });
const book = z.object({ ...scoped, ...dated, name: z.string(), parent_portfolio_id: uuid });
const portfolio = z.object({
    ...scoped,
    ...dated,
    name: z.string(),
    parent_portfolio_id: uuid.nullable(),
});
const booking = z.object({ tenant_id: uuid, trade_id: uuid, book_id: uuid, ...dated });
const runSchema = z.object({
    ...scoped,
    name: z.string(),
    definition_id: marketId,
    started_at: z.string().nullable(),
    completed_at: z.string().nullable(),
    priced_at: z.string().nullable(),
    price_note: z.string(),
    cut_id: marketId.nullable(),
    cut_label: z.string().nullable(),
    cut_timepoint: z.string().nullable(),
    engine_settings_id: marketId.nullable(),
    engine_settings_version: z.number().nullable(),
});
const resultSchema = z.object({
    ...scoped,
    report_instance_id: marketId,
    report_definition_id: marketId,
    as_of: z.string(),
    subject_type: z.string(),
    subject_id: marketId,
    measure: z.string(),
    currency: z.string(),
    figure: z.number().finite(),
    source: z.string(),
});
const metricSchema = z.object({
    ...scoped,
    report_instance_id: uuid,
    report_definition_id: uuid,
    as_of: z.string(),
    subject_type: z.string(),
    subject_id: uuid,
    measure: z.string(),
    currency: z.string(),
    value: z.number().finite(),
    source: z.string(),
    family: z.string(),
    scenario: z.string(),
    factor: z.string(),
    qualifiers: z.string().max(16384),
});
const nil = '00000000-0000-0000-0000-000000000000';
function accepted(raw: unknown) {
    if (z.object({ result: z.object({ outcome: z.string() }) }).parse(raw).result.outcome !== 'ok')
        throw new AppError('scope_read_refused', 'Backend refused the scoped reporting read.', 403);
}
function dateOf(value: string) {
    if (value.length > 10) observationTime(value);
    return z.iso.date().parse(value.slice(0, 10));
}
// Validate calendar/UTC syntax with the shared parser, retaining the backend's
// microseconds for chronology checks instead of rounding to JS milliseconds.
function timestampMicros(value: string): bigint {
    const milliseconds = observationTime(value);
    const fraction = /\.(\d{1,6})Z$/.exec(value)?.[1] ?? '';
    return BigInt(milliseconds) * 1000n + BigInt(fraction.padEnd(6, '0').slice(3));
}

export class ScopeReporting {
    constructor(
        private backend: Backend,
        private now: () => number = Date.now,
    ) {}
    async execute(raw: unknown, session: Session, signal?: AbortSignal): Promise<unknown> {
        const args = scopeTools.summarize_scope_results.schema.parse(raw);
        if (
            ['npv', 'npv_base'].includes(args.measure) &&
            [args.factor, args.factor2, args.scenario].some((v) => v !== undefined)
        )
            throw new AppError(
                'invalid_arguments',
                'Factor/scenario selectors apply only to Greek buckets.',
                400,
            );
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]),
            now = this.now(),
            retrievedAt = new Date(now).toISOString();
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= Date.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        check();
        const request = async (op: Operation, payload: unknown) => {
            check();
            const r = await this.backend.request(op, payload, session, deadline);
            check();
            accepted(r);
            return r;
        };
        const own = (r: { tenant_id: string; party_id: string }) =>
            r.tenant_id === session.tenantId && r.party_id === session.partyId;
        const base = {
            tool: 'summarize_scope_results',
            request: args,
            source: 'OreStudio',
            partyId: session.partyId,
            retrievedAt,
        };
        if (!args.runId)
            return {
                ...base,
                status: 'selection_required',
                field: 'runId',
                reason: 'Select an exact stored report run UUID from the report list or UI, then reply with it. No total was calculated.',
            };
        if (args.valuationDate && args.valuationDate > retrievedAt.slice(0, 10))
            throw new AppError('future_date', 'Future valuation dates are not supported.', 400);
        const scan = async <T extends z.ZodType>(
            op: Operation,
            key: string,
            schema: T,
            filter: unknown,
            asOf?: string,
            identityKey = 'id',
        ): Promise<z.infer<T>[]> =>
            readPages(
                async (offset, limit) => {
                    const raw = await request(op, {
                        offset,
                        limit,
                        order: { field: '', descending: false },
                        filter,
                        ...(asOf ? { as_of: asOf } : {}),
                    });
                    const total = z
                        .object({ total: z.number().int().nonnegative() })
                        .parse(raw).total;
                    const rows = z.object({ [key]: z.array(schema).max(limit) }).parse(raw)[key]!;
                    return { total, rows };
                },
                (row) => String((row as Record<string, unknown>)[identityKey]),
                {
                    code: 'incomplete_scope',
                    message:
                        'Historical/result pages changed, were duplicated/incomplete or exceeded the 2,000-row bound. No complete population or total is claimed.',
                },
            );
        const readRun = async (at: string) => {
            const rows = await scan(
                'reportInstances',
                'instances',
                runSchema,
                { id_one_of: [args.runId] },
                at,
            );
            if (rows.some((r) => r.id !== args.runId))
                throw new AppError(
                    'scope_mismatch',
                    'Different run returned for an exact run read.',
                    403,
                );
            return rows.find(own);
        };
        const run = await readRun(retrievedAt);
        if (!run)
            return {
                ...base,
                status: 'not_found',
                reason: 'Run is not available in the selected party.',
            };
        const health = async () =>
            z
                .object({ health: z.string(), state_name: z.string() })
                .parse(await request('runHealth', { report_instance_id: run.id }));
        const state = await health();
        if (
            state.health !== 'completed clean' ||
            !run.started_at ||
            !run.completed_at ||
            !run.priced_at
        )
            return {
                ...base,
                status: 'unavailable',
                health: state.health,
                reason: 'A clean completed run with start, completion and actual pricing dates is required. No partial total is calculated.',
            };
        const membershipTime = timestampMicros(run.started_at),
            completedTime = timestampMicros(run.completed_at),
            pricedAt = dateOf(run.priced_at);
        if (
            membershipTime > completedTime ||
            completedTime > BigInt(now) * 1000n ||
            pricedAt > retrievedAt.slice(0, 10)
        )
            throw new AppError(
                'invalid_run_dates',
                'Run dates are inconsistent or in the future.',
                422,
            );
        if (args.valuationDate && args.valuationDate !== pricedAt)
            return {
                ...base,
                status: 'unavailable',
                reason: 'The run did not price at the requested valuation date.',
            };
        const membershipAt = run.started_at;
        const historical = <T extends { recorded_at: string }>(rows: T[]) => {
            if (rows.some((r) => timestampMicros(r.recorded_at) > membershipTime))
                throw new AppError(
                    'incomplete_scope',
                    'Backend returned records newer than the historical membership timestamp.',
                    422,
                );
            return rows;
        };
        const books = historical(
            (await scan('books', 'books', book, null, membershipAt)).filter(own),
        );
        const portfolios =
            args.scopeType === 'portfolio'
                ? historical(
                      (
                          await scan('portfolios', 'portfolios', portfolio, null, membershipAt)
                      ).filter(own),
                  )
                : [];
        const catalogue = args.scopeType === 'book' ? books : portfolios,
            query = args.scope.toLowerCase();
        const exact = catalogue.filter(
            (r) => r.id.toLowerCase() === query || r.name.toLowerCase() === query,
        );
        const choices = exact.length
            ? exact
            : catalogue.filter((r) => r.name.toLowerCase().includes(query));
        if (choices.length !== 1)
            return {
                ...base,
                status: choices.length ? 'ambiguous' : 'not_found',
                field: 'scope',
                candidates: choices.slice(0, 25).map((r) => ({ id: r.id, name: r.name })),
                reason: 'Choose an exact selected-party name or UUID from the run-start catalogue. No scope was selected automatically.',
            };
        const selected = choices[0]!;
        const includedBooks = new Set<string>();
        for (const row of books) {
            if (args.scopeType === 'book') {
                if (row.id === selected.id) includedBooks.add(row.id);
                continue;
            }
            const seen = new Set<string>();
            let parent: string | null = row.parent_portfolio_id,
                included = false;
            while (parent && parent !== nil) {
                if (seen.has(parent) || seen.size >= 50)
                    throw new AppError(
                        'incomplete_scope',
                        'Portfolio hierarchy is cyclic or exceeds its depth bound.',
                        422,
                    );
                seen.add(parent);
                if (parent === selected.id) included = true;
                const node = portfolios.find((p) => p.id === parent);
                if (!node)
                    throw new AppError(
                        'incomplete_scope',
                        'A historical parent portfolio is missing or outside the selected party.',
                        422,
                    );
                parent = node.parent_portfolio_id;
            }
            if (included) includedBooks.add(row.id);
        }
        const context = {
            ...base,
            scope: {
                type: args.scopeType,
                id: selected.id,
                name: selected.name,
                bookCount: includedBooks.size,
            },
            run,
            health: state.health,
            membershipBasis: 'booking_at_run_start',
            membershipAt,
        };
        const anchors = historical(
            (await scan('trades', 'trades', anchor, null, membershipAt)).filter(own),
        );
        const ownIds = new Set(anchors.map((r) => r.id));
        const bookings: z.infer<typeof booking>[] = [];
        for (let offset = 0; offset < anchors.length; offset += 500) {
            const chunk = anchors.slice(offset, offset + 500).map((r) => r.id);
            const batch = historical(
                await scan(
                    'tradeBookings',
                    'trade_bookings',
                    booking,
                    { trade_id_one_of: chunk },
                    membershipAt,
                    'trade_id',
                ),
            );
            if (batch.some((r) => !chunk.includes(r.trade_id)))
                throw new AppError(
                    'scope_mismatch',
                    'Booking does not match the requested trade batch.',
                    403,
                );
            bookings.push(...batch);
        }
        if (
            bookings.some(
                (r) =>
                    r.tenant_id !== session.tenantId ||
                    !ownIds.has(r.trade_id) ||
                    !books.some((b) => b.id === r.book_id),
            )
        )
            throw new AppError(
                'scope_mismatch',
                'Booking identity or selected-party book scope cannot be verified.',
                403,
            );
        const expected = new Set(
            bookings.filter((r) => includedBooks.has(r.book_id)).map((r) => r.trade_id),
        );
        if (!expected.size)
            return {
                ...context,
                status: 'empty_scope',
                reason: 'No booked trades at this run-start timestamp. An empty scope is not a zero valuation.',
            };
        if (expected.size > 1000)
            return {
                ...context,
                status: 'incomplete',
                reason: 'Scope exceeds the 1,000-trade result-filter bound; narrow to a smaller book/portfolio. No total calculated.',
            };
        if (!['npv', 'npv_base'].includes(args.measure)) {
            const metrics = await scan(
                'reportMetrics',
                'metrics',
                metricSchema.extend({ result_id: uuid, line: z.number().int().positive() }),
                {
                    report_instance_id: run.id,
                    family: 'sensitivity_report',
                    subject_id_one_of: [...expected],
                    measure_one_of: ['Delta', 'Gamma'],
                },
            );
            for (const metric of metrics) observationTime(metric.as_of);
            const summarized = summarizeSensitivities(metrics, expected, args, {
                tenantId: session.tenantId,
                partyId: session.partyId!,
                runId: run.id,
                definitionId: run.definition_id,
                pricedAt,
            });
            const reread = await readRun(new Date(this.now()).toISOString());
            if (
                JSON.stringify(reread) !== JSON.stringify(run) ||
                (await health()).health !== state.health
            )
                return {
                    ...context,
                    status: 'incomplete',
                    reason: 'Run health or provenance changed during reading; retry. No Greek figures returned.',
                };
            const stale =
                !args.valuationDate &&
                (Date.parse(retrievedAt.slice(0, 10)) - Date.parse(pricedAt)) / 86400000 >
                    args.maxAgeDays;
            return {
                ...context,
                ...summarized,
                stale,
                historicalRequest: Boolean(args.valuationDate),
                warning: stale
                    ? 'Stored historical sensitivities, not a current risk calculation.'
                    : null,
            };
        }
        const filter = Object.fromEntries(
            [
                'party_id',
                'report_instance_id',
                'report_definition_id',
                'subject_type',
                'subject_id',
                'measure',
                'currency',
                'counterparty_id',
                'id_one_of',
                'party_id_one_of',
                'report_instance_id_one_of',
                'report_definition_id_one_of',
                'subject_type_one_of',
                'subject_id_one_of',
                'measure_one_of',
                'currency_one_of',
                'counterparty_id_one_of',
            ].map((k) => [k, null]),
        );
        Object.assign(filter, {
            party_id: session.partyId,
            report_instance_id: run.id,
            subject_type: 'trade',
            subject_id_one_of: [...expected],
            measure: args.measure,
        });
        const rows = await scan('runResults', 'results', resultSchema, filter);
        if (
            rows.some(
                (r) =>
                    !own(r) ||
                    r.report_instance_id !== run.id ||
                    r.report_definition_id !== run.definition_id ||
                    r.subject_type !== 'trade' ||
                    !expected.has(r.subject_id) ||
                    r.measure !== args.measure ||
                    dateOf(r.as_of) !== pricedAt,
            )
        )
            throw new AppError(
                'scope_mismatch',
                'Result identity, date or run provenance disagrees with the requested scope.',
                403,
            );
        const counts = new Map<string, number>();
        for (const row of rows) counts.set(row.subject_id, (counts.get(row.subject_id) ?? 0) + 1);
        const missing = [...expected].filter((id) => !counts.has(id)),
            duplicates = [...counts].filter(([, n]) => n !== 1).map(([id]) => id);
        const coverage = {
            expectedTradeCount: expected.size,
            valuedTradeCount: [...counts].filter(([, n]) => n === 1).length,
            missingTradeCount: missing.length,
            duplicateTradeCount: duplicates.length,
            failedTradeCount: null,
        };
        const covered = { ...context, coverage };
        if (missing.length || duplicates.length)
            return {
                ...covered,
                status: 'incomplete',
                missingTradeIds: missing.slice(0, 10),
                duplicateTradeIds: duplicates.slice(0, 10),
                reason: 'Every expected trade needs exactly one result. Missing values are not zero; no total was calculated. Missing output does not establish a per-trade failure status.',
            };
        if (rows.some((r) => r.source !== 'npv.csv'))
            return {
                ...covered,
                status: 'unsupported',
                reason: 'Only canonical npv.csv NPV rows have a verified additive contract. No total calculated.',
            };
        if (args.measure === 'npv_base') {
            // The projection's base currency may be blank. The same stored CSV
            // row retains BaseCurrency explicitly in its typed metric qualifiers.
            // Join every identity/date/value, never infer it from today's config.
            const metrics = await scan('reportMetrics', 'metrics', metricSchema, {
                report_instance_id: run.id,
                measure: 'NPV(Base)',
                family: 'npv_report',
                subject_id_one_of: [...expected],
            });
            const byTrade = new Map<string, z.infer<typeof metricSchema>>();
            for (const metric of metrics) {
                if (
                    !own(metric) ||
                    metric.report_instance_id !== run.id ||
                    metric.report_definition_id !== run.definition_id ||
                    metric.subject_type !== 'trade' ||
                    !expected.has(metric.subject_id) ||
                    metric.measure !== 'NPV(Base)' ||
                    metric.family !== 'npv_report' ||
                    metric.source !== 'npv.csv' ||
                    metric.scenario ||
                    metric.factor ||
                    dateOf(metric.as_of) !== pricedAt
                )
                    throw new AppError(
                        'inconsistent_currency_metadata',
                        'Typed base-currency provenance disagrees with the stored result.',
                        422,
                    );
                if (byTrade.has(metric.subject_id))
                    return {
                        ...covered,
                        status: 'incomplete',
                        reason: 'Duplicate base-currency metric for a trade; no total calculated.',
                    };
                byTrade.set(metric.subject_id, metric);
            }
            if (byTrade.size !== expected.size)
                return {
                    ...covered,
                    status: 'incomplete',
                    reason: 'Base currency cannot be verified for every expected trade; no total calculated.',
                };
            for (const row of rows) {
                const metric = byTrade.get(row.subject_id)!;
                let currency: string;
                try {
                    currency = z
                        .object({ BaseCurrency: z.string().regex(/^[A-Z]{3}$/) })
                        .parse(JSON.parse(metric.qualifiers)).BaseCurrency;
                } catch {
                    throw new AppError(
                        'invalid_currency_metadata',
                        'Stored CSV BaseCurrency is missing or invalid; no total calculated.',
                        422,
                    );
                }
                if (
                    metric.value !== row.figure ||
                    (row.currency && row.currency !== currency) ||
                    (metric.currency && metric.currency !== currency)
                )
                    throw new AppError(
                        'inconsistent_currency_metadata',
                        'Base figure or currency disagrees between the stored projection and typed metric.',
                        422,
                    );
                row.currency = currency;
            }
        }
        const currencies = [...new Set(rows.map((r) => r.currency))].sort();
        if (
            currencies.length !== 1 ||
            currencies.some((c) => !/^[A-Z]{3}$/.test(c)) ||
            (args.currency && currencies[0] !== args.currency)
        )
            return {
                ...covered,
                status: 'ambiguous',
                field: 'currency',
                currencies: currencies.slice(0, 25),
                reason: 'Currency units are mixed, missing or differ from the request. Use a consistently denominated npv_base run; no FX conversion or currency-filtered partial total is performed.',
            };
        const reread = await readRun(new Date(this.now()).toISOString());
        if (
            JSON.stringify(reread) !== JSON.stringify(run) ||
            (await health()).health !== state.health
        )
            return {
                ...covered,
                status: 'incomplete',
                reason: 'Run health or provenance changed during reading; retry. No total calculated.',
            };
        const parts = rows.map((r) => ({ row: r, ...decimalParts(r.figure) })),
            scale = Math.max(...parts.map((p) => p.scale));
        const magnitude = (p: (typeof parts)[number]) =>
            (p.coefficient < 0n ? -p.coefficient : p.coefficient) * 10n ** BigInt(scale - p.scale);
        parts.sort((a, b) =>
            magnitude(a) > magnitude(b)
                ? -1
                : magnitude(a) < magnitude(b)
                  ? 1
                  : a.row.subject_id.localeCompare(b.row.subject_id),
        );
        const stale =
            !args.valuationDate &&
            (Date.parse(retrievedAt.slice(0, 10)) - Date.parse(pricedAt)) / 86400000 >
                args.maxAgeDays;
        return {
            ...covered,
            status: stale ? 'stale' : 'ok',
            total: {
                measure: args.measure,
                figure: decimalSum(rows.map((r) => r.figure)),
                currency: currencies[0],
                source: 'npv.csv',
                currencyBasis:
                    args.measure === 'npv_base'
                        ? 'typed_metric.BaseCurrency'
                        : 'stored_result.currency',
                arithmetic: 'sum_of_stored_decimal_renderings',
            },
            stale,
            historicalRequest: Boolean(args.valuationDate),
            warning: stale ? 'Stored historical results, not a current valuation.' : null,
            contributors: parts.slice(args.offset, args.offset + args.limit).map(({ row }) => ({
                tradeId: row.subject_id,
                figure: decimalSum([row.figure]),
                currency: row.currency,
                source: row.source,
                asOf: row.as_of,
            })),
            contributorCount: rows.length,
            offset: args.offset,
            limit: args.limit,
            nextOffset: args.offset + args.limit < rows.length ? args.offset + args.limit : null,
        };
    }
}

export function formatScopeReporting(raw: unknown): string | undefined {
    if (!raw || typeof raw !== 'object' || (raw as any).tool !== 'summarize_scope_results') return;
    const r = raw as any;
    if (r.buckets) return formatSensitivities(r);
    if (!r.total)
        return `${r.reason}${r.health ? '\nRun health: ' + r.health : ''}${r.coverage ? `\nCoverage: ${r.coverage.valuedTradeCount}/${r.coverage.expectedTradeCount}; missing ${r.coverage.missingTradeCount}; duplicates ${r.coverage.duplicateTradeCount}.` : ''}${r.candidates?.length ? '\n' + r.candidates.map((c: any) => `${c.name} [${c.id}]`).join('\n') : ''}${r.currencies?.length ? '\nCurrencies: ' + r.currencies.join(', ') : ''}`;
    return `${r.warning ? r.warning + '\n' : ''}## Scope results\n\n${r.scope.type} ${r.scope.name} [${r.scope.id}]\n${r.total.measure === 'npv_base' ? 'Base NPV' : 'NPV'}: ${r.total.figure} ${r.total.currency}\nCoverage: ${r.coverage.valuedTradeCount}/${r.coverage.expectedTradeCount} trades; missing 0; duplicates 0.\nMembership: bookings at ${r.membershipAt}${r.scope.type === 'portfolio' ? ' in the booking-portfolio subtree; direct trade assignments are separate' : ''}.\nRun: ${r.run.name} [${r.run.id}]\nHealth: ${r.health}; actual pricing date: ${r.run.priced_at}\nCompleted: ${r.run.completed_at}\nMarket cut: ${r.run.cut_label ?? 'not supplied'} (${r.run.cut_timepoint ?? 'time not supplied'})\nPricing-date note: ${r.run.price_note || 'none'}\nSource: ${r.total.source}\nContributors by absolute value; showing ${r.contributors.length} from offset ${r.offset} of ${r.contributorCount}:\n\n${markdownTable(
        ['Trade ID', 'Value', 'Currency'],
        r.contributors.map((c: any) => [c.tradeId, c.figure, c.currency]),
    )}\n${r.nextOffset !== null ? '\nNext contributor offset: ' + r.nextOffset : ''}\nStored results only; no repricing or currency conversion.`;
}

// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { readPages } from './read-pages.js';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { marketId, observationTime } from './market.js';
import { markdownText } from './markdown.js';
import { baseNpvCurrency, npvMetric } from './npv-units.js';
export const reportingTools = {
    find_report_runs: {
        description:
            'List or find stored report runs in the selected party. “Show me the last few completed runs” → {"status":"completed"}; omit query when no report name was given. Returns run IDs, health and pricing dates. For one known run use read_run_status; for a trade’s stored NPV use read_trade_results.',
        schema: z
            .object({
                tradeId: marketId
                    .optional()
                    .describe(
                        'Filter by trades with stored result rows. Resolve a trade name or reference with find_trades first. Omit for general run discovery.',
                    ),
                query: z
                    .string()
                    .max(120)
                    .default('')
                    .describe(
                        'Literal text from a report name. Omit for general listings; words like “latest”, “completed” and “report runs” are not names.',
                    ),
                status: z
                    .enum(['all', 'completed', 'clean'])
                    .default('all')
                    .describe(
                        'Use completed for finished runs, including completions with errors. Use clean only for successful or error-free runs. Default all includes other states.',
                    ),
                offset: z.number().int().min(0).max(1000).default(0),
                limit: z.number().int().min(1).max(10).default(5),
                valuationDate: z.iso
                    .date()
                    .optional()
                    .describe(
                        'Actual pricing date, YYYY-MM-DD. Omit when the user did not specify one.',
                    ),
                orderBy: z
                    .enum(['completion', 'pricing'])
                    .optional()
                    .describe(
                        'Use pricing for latest by pricing date. Completed and clean listings default to completion order.',
                    ),
            })
            .strict(),
    },
    read_run_status: {
        description:
            'Check the status, health and provenance of one stored run. “Did run <UUID> finish?” → read_run_status. Use find_report_runs first if only a report name is known. Completed with errors is partial.',
        schema: z
            .object({
                runId: marketId.describe(
                    'Exact report run UUID, distinct from the report definition UUID.',
                ),
            })
            .strict(),
    },
    read_trade_results: {
        description:
            'Read one trade’s stored NPV. Resolve a trade reference with find_trades first. Omit runId to search eligible clean runs. Use summarize_scope_results for book or portfolio totals. This tool reads existing results.',
        schema: z
            .object({
                tradeId: marketId.describe('Exact trade UUID from the request or find_trades.'),
                runId: marketId
                    .optional()
                    .describe(
                        'Exact report run UUID if selected. Omit to discover an eligible run.',
                    ),
                measure: z
                    .enum(['npv', 'npv_base'])
                    .default('npv')
                    .describe('Use npv for native currency, npv_base for report base currency.'),
                currency: z
                    .string()
                    .regex(/^[A-Za-z]{3}$/)
                    .transform((s) => s.toUpperCase())
                    .optional()
                    .describe(
                        'Currency filter if explicitly requested. Omit to read the stored currency.',
                    ),
                valuationDate: z.iso
                    .date()
                    .optional()
                    .describe('Explicit pricing date, YYYY-MM-DD.'),
                allowPartial: z
                    .boolean()
                    .default(false)
                    .describe(
                        'Set true only when the user explicitly requests partial results for the given runId.',
                    ),
                maxAgeDays: z.number().int().min(0).max(365).default(3),
            })
            .strict(),
    },
} as const;
const scope = { id: marketId, tenant_id: marketId, party_id: marketId };
const runSchema = z.object({
    ...scope,
    name: z.string(),
    definition_id: marketId,
    priced_at: z.string().nullable(),
    price_note: z.string(),
    completed_at: z.string().nullable(),
    output_message: z
        .string()
        .transform((message) => message.slice(0, 500))
        .optional(),
    cut_id: marketId.nullable(),
    cut_label: z.string().nullable(),
    cut_timepoint: z.string().nullable(),
    engine_settings_id: marketId.nullable(),
    engine_settings_version: z.number().nullable(),
});
const resultSchema = z.object({
    ...scope,
    report_instance_id: marketId,
    report_definition_id: marketId,
    as_of: z.string(),
    subject_type: z.string(),
    subject_id: marketId,
    measure: z.string(),
    currency: z.string(),
    figure: z.number().finite(),
    source: z.string().min(1),
});
function dateOf(s: string): string {
    const d = z.iso.date().parse(s.slice(0, 10));
    if (s.length > 10) observationTime(s);
    return d;
}
function accepted(raw: unknown) {
    if (z.object({ result: z.object({ outcome: z.string() }) }).parse(raw).result.outcome !== 'ok')
        throw new AppError(
            'report_read_refused',
            'Backend did not complete or authorize the reporting read.',
            403,
        );
}
export class Reporting {
    constructor(
        private backend: Backend,
        private now: () => number = Date.now,
    ) {}
    async execute(
        name: string,
        raw: unknown,
        session: Session,
        signal?: AbortSignal,
    ): Promise<unknown> {
        const args = reportingTools[name as keyof typeof reportingTools].schema.parse(
            raw,
        ) as Record<string, any>;
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]),
            snapshot = new Date(this.now()).toISOString();
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= Date.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        const request = async (op: Operation, data: unknown) => {
            check();
            const r = await this.backend.request(op, data, session, deadline);
            check();
            return r;
        };
        const own = (r: { tenant_id: string; party_id: string }) =>
            r.tenant_id === session.tenantId && r.party_id === session.partyId;
        const scan = async <T extends z.ZodType>(
            op: 'reportInstances' | 'runResults' | 'reportMetrics',
            key: string,
            schema: T,
            filter: unknown,
        ): Promise<z.infer<T>[]> =>
            readPages(
                async (offset, limit) => {
                    const raw = await request(op, {
                        offset,
                        limit,
                        order: { field: '', descending: false },
                        filter,
                        ...(op === 'reportInstances' ? { as_of: snapshot } : {}),
                    });
                    accepted(raw);
                    const total = z
                        .object({ total: z.number().int().nonnegative() })
                        .parse(raw).total;
                    const rows = z.object({ [key]: z.array(schema).max(limit) }).parse(raw)[key]!;
                    return { total, rows };
                },
                (row) => String((row as Record<string, unknown>).id),
                {
                    code: 'incomplete_reporting',
                    message: `Reporting pages changed, were duplicated/incomplete or exceeded the ${op === 'reportInstances' ? '10,000' : '2,000'}-row bound; narrow the query.`,
                    // The run endpoint is tenant-wide, so its catalogue can be
                    // much larger than the party's valuation-result population.
                    maxRows: op === 'reportInstances' ? 10000 : 2000,
                    pageSize: op === 'reportInstances' ? 500 : 50,
                },
            );
        const runs = async (ids?: string[]) =>
            (
                await scan(
                    'reportInstances',
                    'instances',
                    runSchema,
                    ids
                        ? {
                              definition_id: null,
                              kind: null,
                              cut_id: null,
                              id_one_of: ids,
                              definition_id_one_of: null,
                              kind_one_of: null,
                              cut_id_one_of: null,
                          }
                        : null,
                )
            ).filter(own);
        const health = async (run: z.infer<typeof runSchema>) => {
            const r = await request('runHealth', { report_instance_id: run.id });
            accepted(r);
            return z.object({ health: z.string(), state_name: z.string() }).parse(r);
        };
        const ensureTrade = async (id: string) => {
            const r = await request('trade', { key: { id } });
            if (
                z.object({ result: z.object({ outcome: z.string() }) }).parse(r).result.outcome ===
                'missing'
            )
                return false;
            accepted(r);
            const trade = z.object({ trade: z.object(scope).nullable() }).parse(r).trade;
            if (trade && trade.id.toLowerCase() !== id.toLowerCase())
                throw new AppError('invalid_reply', 'Different trade returned for exact identity.');
            return Boolean(trade && own(trade));
        };
        const results = async () => {
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
            ) as Record<string, unknown>;
            Object.assign(filter, {
                party_id: session.partyId,
                subject_type: 'trade',
                subject_id: args.tradeId,
                report_instance_id: args.runId ?? null,
                measure: args.measure ?? null,
                currency: args.measure === 'npv_base' ? null : (args.currency ?? null),
            });
            const rows = await scan('runResults', 'results', resultSchema, filter);
            if (
                rows.some(
                    (r) =>
                        !own(r) ||
                        r.subject_type !== 'trade' ||
                        r.subject_id.toLowerCase() !== args.tradeId.toLowerCase() ||
                        (args.runId &&
                            r.report_instance_id.toLowerCase() !== args.runId.toLowerCase()) ||
                        (args.measure && r.measure !== args.measure) ||
                        (args.currency &&
                            args.measure !== 'npv_base' &&
                            r.currency !== args.currency),
                )
            )
                throw new AppError(
                    'scope_mismatch',
                    'Result does not match the requested identity.',
                    403,
                );
            return rows;
        };
        const base = {
            tool: name,
            request: args,
            source: 'OreStudio',
            partyId: session.partyId,
            retrievedAt: snapshot,
        };
        if (args.valuationDate && args.valuationDate > snapshot.slice(0, 10))
            throw new AppError('future_date', 'Future valuation dates are not supported.', 400);
        if (args.tradeId && !(await ensureTrade(args.tradeId)))
            return {
                ...base,
                status: 'not_found',
                reason: 'Trade is not available in the selected party.',
            };
        if (name === 'read_run_status') {
            const run = (await runs([args.runId])).find(
                (r) => r.id.toLowerCase() === args.runId.toLowerCase(),
            );
            return run
                ? { ...base, status: 'ok', run, ...(await health(run)) }
                : {
                      ...base,
                      status: 'not_found',
                      reason: 'Run is not available in the selected party.',
                  };
        }
        const stored = args.tradeId ? await results() : [];
        const ids = [...new Set(stored.map((r) => r.report_instance_id))];
        if (name === 'find_report_runs') {
            const selected = (args.tradeId ? (ids.length ? await runs(ids) : []) : await runs())
                .filter((r) => r.name.toLowerCase().includes(args.query.toLowerCase()))
                .filter(
                    (r) =>
                        !args.valuationDate ||
                        (r.priced_at && dateOf(r.priced_at) === args.valuationDate),
                )
                .filter(
                    (r) =>
                        args.orderBy !== 'pricing' ||
                        (r.priced_at && dateOf(r.priced_at) <= snapshot.slice(0, 10)),
                )
                .filter(
                    (r) =>
                        args.status === 'all' ||
                        (r.completed_at && observationTime(r.completed_at) <= this.now()),
                )
                .sort(
                    (a, b) =>
                        (args.orderBy === 'pricing'
                            ? dateOf(b.priced_at!).localeCompare(dateOf(a.priced_at!)) ||
                              (b.completed_at && a.completed_at
                                  ? observationTime(b.completed_at) -
                                    observationTime(a.completed_at)
                                  : 0)
                            : args.status !== 'all' || args.orderBy === 'completion'
                              ? observationTime(b.completed_at!) - observationTime(a.completed_at!)
                              : String(b.priced_at).localeCompare(String(a.priced_at))) ||
                        b.id.localeCompare(a.id),
                );
            if (args.status !== 'all') {
                const rows = [];
                let matched = 0,
                    inspected = 0;
                for (let start = 0; start < selected.length; start += 100) {
                    const batch = selected.slice(start, start + 100);
                    const raw = await request('runHealthBatch', {
                        instance_ids: batch.map((run) => run.id),
                    });
                    accepted(raw);
                    const states = z
                        .object({
                            health: z
                                .array(
                                    z.object({
                                        report_instance_id: marketId,
                                        health: z.string(),
                                        state_name: z.string(),
                                    }),
                                )
                                .max(batch.length),
                        })
                        .parse(raw).health;
                    if (
                        states.length !== batch.length ||
                        new Set(states.map((s) => s.report_instance_id)).size !== batch.length ||
                        states.some((s) => !batch.some((run) => run.id === s.report_instance_id))
                    )
                        throw new AppError(
                            'incomplete_reporting',
                            'Batch run health did not match the selected-party run catalogue.',
                        );
                    const byId = new Map(states.map((s) => [s.report_instance_id, s]));
                    for (const run of batch) {
                        const state = byId.get(run.id)!;
                        inspected++;
                        if (
                            args.status === 'clean'
                                ? state.state_name !== 'completed' ||
                                  state.health !== 'completed clean'
                                : !['completed clean', 'completed with errors'].includes(
                                      state.health,
                                  )
                        )
                            continue;
                        if (matched++ >= args.offset) rows.push({ ...run, ...state });
                        if (rows.length === args.limit) break;
                    }
                    if (rows.length === args.limit) break;
                }
                const hasMore = inspected < selected.length;
                return {
                    ...base,
                    status: rows.length ? 'ok' : 'not_found',
                    rows,
                    totalMatches: hasMore ? null : matched,
                    offset: args.offset,
                    limit: args.limit,
                    hasMore,
                    nextOffset: hasMore ? args.offset + rows.length : null,
                    reason: rows.length
                        ? null
                        : 'No matching completed stored run; no report was started.',
                };
            }
            const page = selected.slice(args.offset, args.offset + args.limit),
                rows = [];
            for (const run of page) rows.push({ ...run, ...(await health(run)) });
            return {
                ...base,
                status: rows.length ? 'ok' : 'not_found',
                totalMatches: selected.length,
                rows,
                offset: args.offset,
                limit: args.limit,
                reason: rows.length ? null : 'No matching stored run; no report was started.',
            };
        }
        if (ids.length > 20)
            return {
                ...base,
                status: 'incomplete',
                reason: 'More than 20 candidate runs; specify a run ID or valuation date. No value selected.',
            };
        if (args.allowPartial && !args.runId)
            return {
                ...base,
                status: 'selection_required',
                field: 'runId',
                reason: 'Inspecting partial values requires an explicit run ID.',
            };
        const metadata = args.runId ? await runs([args.runId]) : ids.length ? await runs(ids) : [],
            evaluated = [];
        for (const run of metadata) evaluated.push({ run, ...(await health(run)) });
        const eligible = evaluated.filter(
            (r) =>
                (r.health === 'completed clean' ||
                    (args.allowPartial && r.health === 'completed with errors')) &&
                r.run.completed_at &&
                observationTime(r.run.completed_at) <= this.now() &&
                r.run.priced_at &&
                dateOf(r.run.priced_at) <= snapshot.slice(0, 10) &&
                (!args.valuationDate || dateOf(r.run.priced_at) === args.valuationDate),
        );
        if (!eligible.length)
            return {
                ...base,
                status: 'unavailable',
                reason: !stored.length
                    ? 'No stored value matches this trade, measure and run selection. Check the run status or choose a different completed run.'
                    : 'No eligible completed run matches the requested pricing date and health. A partial result requires an explicit run ID and an explicit request to inspect partial output.',
                candidates: evaluated.map((r) => ({
                    id: r.run.id,
                    name: r.run.name,
                    health: r.health,
                    pricedAt: r.run.priced_at,
                    priceNote: r.run.price_note,
                })),
            };
        // Only compare runs with the same pricing date and configuration. Never silently combine reports.
        eligible.sort(
            (a, b) =>
                dateOf(b.run.priced_at!).localeCompare(dateOf(a.run.priced_at!)) ||
                observationTime(b.run.completed_at!) - observationTime(a.run.completed_at!),
        );
        const newestDate = dateOf(eligible[0]!.run.priced_at!);
        const atDate = eligible.filter((r) => dateOf(r.run.priced_at!) === newestDate);
        const configs = new Set(
            atDate.map((r) =>
                JSON.stringify([
                    r.run.definition_id,
                    r.run.cut_id,
                    r.run.engine_settings_id,
                    r.run.engine_settings_version,
                ]),
            ),
        );
        if (
            configs.size > 1 ||
            (atDate.length > 1 &&
                observationTime(atDate[0]!.run.completed_at!) ===
                    observationTime(atDate[1]!.run.completed_at!))
        )
            return {
                ...base,
                status: 'ambiguous',
                field: 'runId',
                reason: 'Multiple configurations or equally recent runs; specify a run ID.',
                candidates: atDate.map((r) => ({
                    id: r.run.id,
                    name: r.run.name,
                    health: r.health,
                })),
            };
        const chosen = atDate[0]!,
            rows = stored.filter((r) => r.report_instance_id === chosen.run.id);
        if (
            rows.some(
                (r) =>
                    r.report_definition_id !== chosen.run.definition_id ||
                    dateOf(r.as_of) !== newestDate,
            )
        )
            throw new AppError(
                'inconsistent_provenance',
                'Result and run pricing provenance disagree.',
            );
        if (!rows.length)
            return {
                ...base,
                status: 'unavailable',
                reason: 'The selected run contains no matching stored value for this trade and measure. Check whether the report covered the trade and stored this measure.',
                candidates: [
                    {
                        id: chosen.run.id,
                        name: chosen.run.name,
                        health: chosen.health,
                        pricedAt: chosen.run.priced_at,
                    },
                ],
            };
        if (rows.length > 1) {
            const currencies = [...new Set(rows.map((r) => r.currency))];
            const chooseCurrency =
                args.measure === 'npv' &&
                currencies.length === rows.length &&
                currencies.every((currency) => /^[A-Z]{3}$/.test(currency));
            return {
                ...base,
                status: 'ambiguous',
                currencies,
                ...(chooseCurrency
                    ? {
                          field: 'currency',
                          request: { ...args, runId: chosen.run.id },
                          candidates: currencies.map((currency) => ({
                              id: currency,
                              name: currency,
                          })),
                      }
                    : {}),
                reason: chooseCurrency
                    ? 'The selected run contains native NPV in multiple currencies. Choose a currency to inspect its stored value.'
                    : 'The selected run has duplicate or conflicting rows for this trade and measure. Check the stored output or choose a different run.',
            };
        }
        let row = rows[0]!;
        let currencyBasis: 'stored_result' | 'typed_metric.BaseCurrency' = 'stored_result';
        if (!row.currency && row.measure === 'npv_base' && row.source === 'npv.csv') {
            const matches = await scan('reportMetrics', 'metrics', npvMetric, {
                report_instance_id: row.report_instance_id,
                measure: 'NPV(Base)',
                family: 'npv_report',
                subject_id: row.subject_id,
            });
            if (matches.length !== 1)
                throw new AppError(
                    'invalid_result_units',
                    'The stored base NPV has no currency and requires exactly one matching NPV(Base) metric.',
                    422,
                );
            row = { ...row, currency: baseNpvCurrency(row, matches[0]!) };
            currencyBasis = 'typed_metric.BaseCurrency';
        }
        if (!/^[A-Z]{3}$/.test(row.currency))
            throw new AppError(
                'invalid_result_units',
                'The stored NPV row has no verified currency unit.',
                422,
            );
        if (args.currency && row.currency !== args.currency)
            return {
                ...base,
                status: 'unavailable',
                reason: 'The verified unit differs from the requested currency. No currency conversion was performed.',
                currencies: [row.currency],
            };
        const finalHealth = await health(chosen.run);
        if (finalHealth.health !== chosen.health)
            return {
                ...base,
                status: 'incomplete',
                reason: 'Run health changed during reading; retry.',
            };
        const stale =
            !args.valuationDate &&
            (Date.parse(snapshot.slice(0, 10)) - Date.parse(newestDate)) / 86400000 >
                args.maxAgeDays;
        return {
            ...base,
            status: chosen.health === 'completed with errors' ? 'partial' : stale ? 'stale' : 'ok',
            health: chosen.health,
            run: chosen.run,
            result: row,
            currencyBasis,
            historicalRequest: Boolean(args.valuationDate),
            stale,
            warning:
                chosen.health === 'completed with errors'
                    ? 'Incomplete/errored report output. This is not a clean valuation.'
                    : stale
                      ? 'Stored historical result, not a current valuation.'
                      : null,
        };
    }
}
export function formatReporting(raw: unknown): string | undefined {
    if (!raw || typeof raw !== 'object' || !Object.hasOwn(reportingTools, (raw as any).tool))
        return;
    const r = raw as any;
    if (r.result)
        return `${r.warning ? r.warning + '\n' : ''}${r.result.measure}: ${r.result.figure} ${r.result.currency}\nTrade: ${r.result.subject_id}\nRun: ${r.run.name} [${r.run.id}]\nHealth: ${r.health}\nActual pricing date: ${r.run.priced_at}\nResult as-of: ${r.result.as_of}\nCompleted: ${r.run.completed_at}\nMarket cut: ${r.run.cut_label ?? 'not supplied'} (${r.run.cut_timepoint ?? 'time not supplied'})\nPricing-date note: ${r.run.price_note || 'none'}\nSource: ${r.result.source}\nStored result only; no new pricing or report execution.`;
    if (r.tool === 'read_run_status' && r.run)
        return `${r.run.name} [${r.run.id}]\nHealth: ${r.health || 'unavailable'}; state: ${r.state_name || 'unavailable'}\nActual pricing date: ${r.run.priced_at ?? 'not supplied'}\nCompleted: ${r.run.completed_at ?? 'not completed'}\nPricing-date note: ${r.run.price_note || 'none'}${r.run.output_message && (r.health === 'completed with errors' || ['failed', 'skipped', 'cancelled'].includes(r.state_name)) ? '\nRun message: ' + markdownText(r.run.output_message) : ''}`;
    if (r.rows)
        return `${r.totalMatches === null ? 'Completed stored runs' : `${r.totalMatches} stored runs`}; showing ${r.rows.length} from offset ${r.offset}.\n\n${r.rows.map((x: any) => `${x.name} [${x.id}] — ${x.health}; priced ${x.priced_at ?? 'unknown'}; completed ${x.completed_at ?? 'not completed'}`).join('\n\n')}${r.hasMore ? `\n\nMore candidate runs remain. Next offset: ${r.nextOffset}.` : ''}${r.reason ? `\n\n${r.reason}` : ''}`;
    return `${r.reason}${r.candidates?.length ? '\n\n' : ''}${(r.candidates ?? []).map((c: any, index: number) => `${index + 1}. ${c.name}${c.id !== c.name ? ' [' + c.id + ']' : ''}${c.health ? ' — ' + c.health : ''}${c.pricedAt ? '; priced ' + c.pricedAt : ''}${c.priceNote ? '; ' + c.priceNote : ''}`).join('\n')}${r.currencies && r.field !== 'currency' ? '\nCurrencies: ' + r.currencies.join(', ') : ''}`;
}

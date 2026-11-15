// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { MarketLookup, marketId, marketSeries, fxInput, irInput } from './market.js';
import { Discovery, discoveryTools } from './discovery.js';
import { Reporting, reportingTools } from './reporting.js';
import { ScopeReporting, scopeTools } from './scope-reporting.js';
import { FxAnalytics, analyticsTools } from './fx-analytics.js';
import { FxMarketCurve, marketCurveTools } from './fx-market-curve.js';
import { helpTool, searchHelp } from './help.js';
import { currencyInput, readCurrencies } from './currency-reference.js';
import { calculate, calculationTool } from './calculation.js';
import { pythonTool, type PythonSandbox } from './python-sandbox.js';

const page = { offset: 0, order: { field: '', descending: false }, filter: null, as_of: null };
const limit = z.number().int().min(1).max(50).default(10);
export const tools = {
    search_help: helpTool,
    calculate: calculationTool,
    ...marketCurveTools,
    ...analyticsTools,
    ...discoveryTools,
    ...reportingTools,
    ...scopeTools,
    read_fx_spot: {
        description:
            'Read a stored FX spot quote for a directed pair. “EUR/USD spot” → {"baseCurrency":"EUR","quoteCurrency":"USD"}. Omit provider and seriesId unless selected; the tool looks up matching series and reports choices if needed. A stale quote is historical. Does not invert pairs or calculate crosses.',
        schema: fxInput,
    },
    read_ir_rate: {
        description:
            'Read a stored interest-rate or discount point. “USD 5Y swap rate” → {"currency":"USD","term":"5Y","instrument":"ir_swap"}. Omit unspecified provider, index and curve so the tool can look for matches. Returns dated evidence or matching choices. Does not interpolate.',
        schema: irInput,
    },
    list_currencies: {
        description:
            'List configured currencies, not FX quotes or pair availability. “What currency does Japan use?” → {"country":"Japan"}. The tool resolves the country and its linked currencies. Omit both filters to list currencies; use codePrefix only for a requested ISO currency-code prefix.',
        schema: currencyInput,
    },
    list_market_series: {
        description:
            'List available market series and discover their IDs and oresmd URIs. Use for inventory questions; use read_fx_spot or read_ir_rate for a specific quote. Search matches literal text within one page, so paginate before concluding there is no match.',
        schema: z
            .object({
                query: z
                    .string()
                    .max(100)
                    .default('')
                    .describe('Literal series search text. Omit to browse.'),
                limit,
                offset: z.number().int().min(0).max(10000).default(0),
            })
            .strict(),
    },
    read_market_observations: {
        description:
            'Read the latest observations and their dates and sources for one known series. If the series ID is unknown, call list_market_series first. For a point-in-time FX or IR lookup, use the dedicated point tool.',
        schema: z
            .object({
                seriesId: marketId.describe(
                    'Exact series UUID from the user or list_market_series.',
                ),
                limit,
            })
            .strict(),
    },
} as const;
const series = z.object({
    id: marketId,
    oresmd_uri: z.string(),
    provider_code: z.string(),
    series_subclass: z.string(),
});
const observation = z.object({
    observation_datetime: z.string(),
    value: z.string(),
    source: z.string(),
    oresmd_uri: z.string().optional(),
});
const envelope = z.object({
    result: z.object({ outcome: z.string() }),
    total: z.number().int().nonnegative(),
});
export class ToolExecutor {
    private market: MarketLookup;
    private discovery: Discovery;
    private reporting: Reporting;
    private scopeReporting: ScopeReporting;
    private analytics: FxAnalytics;
    private marketCurve: FxMarketCurve;
    constructor(
        private backend: Backend,
        private audit: (event: Record<string, unknown>) => void = (e) =>
            console.log(JSON.stringify(e)),
        private python?: PythonSandbox,
    ) {
        this.market = new MarketLookup(backend);
        this.discovery = new Discovery(backend);
        this.reporting = new Reporting(backend);
        this.scopeReporting = new ScopeReporting(backend);
        this.analytics = new FxAnalytics(backend);
        this.marketCurve = new FxMarketCurve(backend);
    }
    async execute(
        name: string,
        args: unknown,
        session: Session,
        signal?: AbortSignal,
    ): Promise<unknown> {
        if (!session.active || session.expires <= Date.now())
            throw new AppError('session_expired', 'Sign in again.', 401);
        const spec =
            name === 'run_python' && this.python
                ? pythonTool
                : Object.hasOwn(tools, name)
                  ? tools[name as keyof typeof tools]
                  : undefined;
        if (!spec) throw new AppError('unknown_tool', 'Tool is not available.', 400);
        const input = Object.hasOwn(analyticsTools, name)
            ? (args as Record<string, unknown>)
            : spec.schema.parse(args);
        const now = Date.now();
        if (now - session.window > 60000) {
            session.window = now;
            session.calls = 0;
        }
        if (++session.calls > 60 || session.busy >= 2)
            throw new AppError('rate_limited', 'Tool rate or concurrency limit exceeded.', 429);
        session.busy++;
        const start = Date.now();
        let outcome = 'failed';
        try {
            signal?.throwIfAborted();
            if (name === 'run_python' && this.python) {
                const output = await this.python.run(
                    input as z.infer<typeof pythonTool.schema>,
                    signal,
                );
                if (!session.active || session.expires <= Date.now())
                    throw new AppError('session_expired', 'Sign in again.', 401);
                outcome = 'ok';
                return output;
            }
            if (name === 'calculate') {
                const output = calculate(input as z.infer<typeof calculationTool.schema>);
                outcome = 'ok';
                return output;
            }
            if (name === 'search_help') {
                const { query } = input as z.infer<typeof helpTool.schema>;
                outcome = 'ok';
                return searchHelp(query);
            }
            if (name === 'list_currencies') {
                const output = await readCurrencies(
                    this.backend,
                    input as z.infer<typeof currencyInput>,
                    session,
                    signal,
                );
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (Object.hasOwn(marketCurveTools, name)) {
                const output = await this.marketCurve.execute(input, session, signal);
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (Object.hasOwn(analyticsTools, name)) {
                const output = await this.analytics.execute(
                    name as keyof typeof analyticsTools,
                    input,
                    session,
                    signal,
                );
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (Object.hasOwn(scopeTools, name)) {
                const output = await this.scopeReporting.execute(input, session, signal);
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (Object.hasOwn(reportingTools, name)) {
                const output = await this.reporting.execute(name, input, session, signal);
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (Object.hasOwn(discoveryTools, name)) {
                const output = await this.discovery.execute(name, input, session, signal);
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            if (name === 'read_fx_spot' || name === 'read_ir_rate') {
                const output = await this.market.lookup(
                    name === 'read_fx_spot' ? 'fx' : 'ir',
                    input as z.infer<typeof fxInput> | z.infer<typeof irInput>,
                    session,
                    signal,
                );
                if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                    throw new AppError('oversized_result', 'Tool output exceeds its limit.');
                outcome = 'ok';
                return output;
            }
            let raw: unknown;
            if (name === 'list_market_series') {
                const { query: _, ...paging } = input as z.infer<
                    typeof tools.list_market_series.schema
                >;
                raw = await this.backend.request('series', { ...page, ...paging }, session, signal);
            } else {
                const { seriesId, limit } = input as z.infer<
                    typeof tools.read_market_observations.schema
                >;
                raw = await this.backend.request(
                    'observations',
                    {
                        series_id: seriesId,
                        scope: 'direct',
                        offset: 0,
                        limit,
                        order: page.order,
                        filter: null,
                    },
                    session,
                    signal,
                );
            }
            signal?.throwIfAborted();
            if (!session.active) throw new AppError('unauthorized', 'Session was closed.', 401);
            const result = envelope.parse(raw);
            if (result.result.outcome !== 'ok')
                throw new AppError(
                    'backend_denied',
                    'Backend did not authorize or complete this read.',
                    403,
                );
            let rows: unknown[];
            if (name === 'list_market_series') {
                rows = z
                    .object({ market_series: z.array(marketSeries).max(50) })
                    .parse(raw)
                    .market_series.filter(
                        (r) => r.tenant_id === session.tenantId && r.party_id === session.partyId,
                    )
                    .map((r) => series.parse(r));
                const query = (
                    input as z.infer<typeof tools.list_market_series.schema>
                ).query.toLowerCase();
                rows = rows.filter((r) =>
                    (r as z.infer<typeof series>).oresmd_uri.toLowerCase().includes(query),
                );
            } else {
                const scoped = z
                    .object({
                        market_observations: z
                            .array(observation.extend({ tenant_id: marketId, party_id: marketId }))
                            .max(50),
                    })
                    .parse(raw).market_observations;
                if (
                    scoped.some(
                        (r) => r.tenant_id !== session.tenantId || r.party_id !== session.partyId,
                    )
                )
                    throw new AppError(
                        'party_mismatch',
                        'Observation does not belong to the selected party.',
                        403,
                    );
                rows = scoped.map((r) => observation.parse(r));
            }
            const output = {
                tool: name,
                rows,
                total: result.total,
                offset: 'offset' in input ? input.offset : 0,
                source: 'OreStudio',
                retrievedAt: new Date().toISOString(),
                ...(name === 'list_market_series' ? { searchScope: 'this_page' } : {}),
            };
            if (Buffer.byteLength(JSON.stringify(output)) > 32768)
                throw new AppError('oversized_result', 'Tool output exceeds its limit.');
            outcome = 'ok';
            return output;
        } finally {
            session.busy--;
            this.audit({
                event: 'tool',
                tool: name,
                tenantId: session.tenantId,
                partyId: session.partyId,
                outcome,
                durationMs: Date.now() - start,
            });
        }
    }
}

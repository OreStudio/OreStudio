// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { MarketLookup, marketId, marketSeries, fxInput, irInput } from './market.js';
import { Discovery, discoveryTools } from './discovery.js';
import { Reporting, reportingTools } from './reporting.js';
import { ScopeReporting, scopeTools } from './scope-reporting.js';

const page = { offset: 0, order: { field: '', descending: false }, filter: null, as_of: null };
const limit = z.number().int().min(1).max(50).default(10);
export const tools = {
    ...discoveryTools,
    ...reportingTools,
    ...scopeTools,
    read_fx_spot: {
        description:
            'Look up an exact directed FX spot pair in the selected party. Returns value, units, observation date, provider, source and freshness. Ask the user to choose when ambiguous; stale means historical, never a current quote. No inversion or cross-rate calculation.',
        schema: fxInput,
    },
    read_ir_rate: {
        description:
            'Read an exact IR point: currency, instrument (ir_swap/mm/zero/discount) and maturity term, e.g. 5Y. Floating-leg tenor is separate. Returns provider, dated source and freshness; asks for clarification if ambiguous. No interpolation or pricing.',
        schema: irInput,
    },
    list_currencies: {
        description:
            'Read configured currency codes and names from OreStudio. Results may be paginated.',
        schema: z.object({ limit, offset: z.number().int().min(0).max(10000).default(0) }).strict(),
    },
    list_market_series: {
        description:
            'Discover market series IDs and oresmd URIs. Search is a literal case-insensitive match within one page; paginate to continue. Never assume an absent match is globally absent.',
        schema: z
            .object({
                query: z.string().max(100).default(''),
                limit,
                offset: z.number().int().min(0).max(10000).default(0),
            })
            .strict(),
    },
    read_market_observations: {
        description:
            'Read observations for an exact discovered series ID, including date and source. No observations means no quote is available; do not invent values.',
        schema: z.object({ seriesId: marketId, limit }).strict(),
    },
} as const;
const currencies = z.object({
    iso_code: z.string(),
    name: z.string(),
    symbol: z.string().optional(),
});
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
    constructor(
        private backend: Backend,
        private audit: (event: Record<string, unknown>) => void = (e) =>
            console.log(JSON.stringify(e)),
    ) {
        this.market = new MarketLookup(backend);
        this.discovery = new Discovery(backend);
        this.reporting = new Reporting(backend);
        this.scopeReporting = new ScopeReporting(backend);
    }
    async execute(
        name: string,
        args: unknown,
        session: Session,
        signal?: AbortSignal,
    ): Promise<unknown> {
        if (!session.active || session.expires <= Date.now())
            throw new AppError('session_expired', 'Sign in again.', 401);
        if (!Object.hasOwn(tools, name))
            throw new AppError('unknown_tool', 'Tool is not available.', 400);
        const spec = tools[name as keyof typeof tools];
        const input = spec.schema.parse(args);
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
            if (name === 'list_currencies')
                raw = await this.backend.request(
                    'currencies',
                    { ...page, ...input },
                    session,
                    signal,
                );
            else if (name === 'list_market_series') {
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
            if (name === 'list_currencies')
                rows = z.object({ currencies: z.array(currencies).max(50) }).parse(raw).currencies;
            else if (name === 'list_market_series') {
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

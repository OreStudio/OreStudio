// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { markdownText } from './markdown.js';

// Backend deterministic IDs are 128-bit UUID text, not necessarily RFC versioned UUIDs.
export const marketId = z
    .string()
    .regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i);
const currency = z
    .string()
    .regex(/^[A-Za-z]{3}$/)
    .transform((s) => s.toUpperCase());
const date = z.iso.date();
const period = z
    .string()
    .regex(/^[1-9][0-9]{0,3}[DWMY]$/i)
    .transform((s) => s.toUpperCase());
const common = {
    provider: z
        .string()
        .min(1)
        .max(100)
        .optional()
        .describe('Literal provider code if requested or selected. Omit to discover matches.'),
    seriesId: marketId
        .optional()
        .describe('Exact series UUID if selected. Omit to search by the other fields.'),
    asOf: date
        .optional()
        .describe('UTC lookup cutoff date, YYYY-MM-DD. Omit for the latest available observation.'),
    maxAgeHours: z
        .number()
        .min(0)
        .max(8760)
        .default(72)
        .describe('Staleness threshold, default 72 hours; never suppress stale warnings.'),
};
export const fxInput = z
    .object({
        baseCurrency: currency.describe('First currency in the directed pair, EUR in EUR/USD.'),
        quoteCurrency: currency.describe('Second currency in the directed pair, USD in EUR/USD.'),
        ...common,
    })
    .strict();
export const irInput = z
    .object({
        currency,
        instrument: z
            .enum(['ir_swap', 'mm', 'zero', 'discount'])
            .optional()
            .describe(
                'Swap rate = ir_swap, money market = mm, zero rate = zero, discount factor = discount. Omit if unspecified to discover matching instruments.',
            ),
        term: period.describe('Requested point maturity, e.g. 5Y; not the floating-leg tenor.'),
        indexName: z
            .string()
            .min(1)
            .max(100)
            .optional()
            .describe('Literal index name if specified. Omit to look for matching series.'),
        curveId: z
            .string()
            .min(1)
            .max(100)
            .optional()
            .describe('Named curve if specified. Omit to look for matching series.'),
        floatingTenor: period
            .optional()
            .describe('Swap floating-leg tenor, e.g. 3M. Omit unless supplied.'),
        forwardStart: z
            .string()
            .regex(/^(0|[1-9][0-9]{0,3})[DWMY]$/i)
            .transform((s) => s.toUpperCase())
            .optional()
            .describe(
                'Explicit forward start, e.g. 1Y for a 1Y-forward swap. Omit unless supplied.',
            ),
        ...common,
    })
    .strict();
type Fx = z.infer<typeof fxInput>;
type Ir = z.infer<typeof irInput>;
export const marketSeries = z.object({
    id: marketId,
    tenant_id: marketId,
    party_id: marketId,
    oresmd_uri: z.string().max(2048),
    provider_code: z.string().max(100),
    series_subclass: z.string(),
});
type Series = z.infer<typeof marketSeries>;
const observation = z.object({
    series_id: marketId,
    tenant_id: marketId,
    party_id: marketId,
    observation_datetime: z.string().max(100),
    oresmd_uri: z.string().max(2048),
    value: z.string().max(200),
    source: z.string().max(1000),
});
const envelope = z.object({
    result: z.object({ outcome: z.string() }),
    total: z.number().int().nonnegative(),
});
const PAGE = 50,
    MAX_ROWS = 1000;
interface Datum {
    asset: string;
    currency: string;
    params: URLSearchParams;
}
export function parseDatum(text: string): Datum | undefined {
    try {
        const u = new URL(text);
        if (
            u.protocol !== 'oresmd:' ||
            u.username ||
            u.password ||
            u.port ||
            u.hash ||
            !/^\/[A-Z]{3}$/.test(u.pathname)
        )
            return;
        const keys = [...u.searchParams.keys()];
        if (new Set(keys).size !== keys.length) return; // Duplicate identity fields cannot be interpreted safely.
        return { asset: u.hostname, currency: u.pathname.slice(1), params: u.searchParams };
    } catch {
        return;
    }
}
function normalizedPeriod(value: string | null): string | null {
    if (!value) return null;
    const m = /^(\d+)([DWMY])$/i.exec(value);
    if (!m) return value;
    const n = Number(m[1]),
        unit = m[2]!.toUpperCase();
    if (!n) return '0D';
    return unit === 'Y' ? `${n * 12}M` : unit === 'W' ? `${n * 7}D` : `${n}${unit}`;
}
function seriesMatches(row: Series, input: Fx | Ir): boolean {
    const d = parseDatum(row.oresmd_uri);
    if (!d || d.params.get('type') !== 'series' || d.params.get('quote') !== 'rate') return false;
    if (input.provider && row.provider_code !== input.provider) return false;
    if (input.seriesId && row.id.toLowerCase() !== input.seriesId.toLowerCase()) return false;
    if ('baseCurrency' in input)
        return (
            d.asset === 'fx' &&
            d.currency === input.baseCurrency &&
            d.params.get('instrument') === 'fx_spot' &&
            d.params.get('ccy') === input.quoteCurrency
        );
    if (
        d.asset !== 'ir' ||
        d.currency !== input.currency ||
        !['ir_swap', 'mm', 'zero', 'discount'].includes(d.params.get('instrument') ?? '') ||
        (input.instrument && d.params.get('instrument') !== input.instrument)
    )
        return false;
    const index = d.params.get('index_name')?.toLowerCase();
    if (
        input.indexName &&
        index !== input.indexName.toLowerCase() &&
        index !== (input.currency + '-' + input.indexName).toLowerCase()
    )
        return false;
    if (input.curveId && d.params.get('curve_id')?.toLowerCase() !== input.curveId.toLowerCase())
        return false;
    if (
        d.params.has('term') &&
        normalizedPeriod(d.params.get('term')) !== normalizedPeriod(input.term)
    )
        return false;
    if (
        input.floatingTenor &&
        normalizedPeriod(d.params.get('tenor')) !== normalizedPeriod(input.floatingTenor)
    )
        return false;
    return (
        !input.forwardStart ||
        normalizedPeriod(d.params.get('fwd_start')) === normalizedPeriod(input.forwardStart)
    );
}
function pointMatches(point: string, row: Series, input: Fx | Ir): boolean {
    const p = parseDatum(point),
        s = parseDatum(row.oresmd_uri);
    if (
        !p ||
        !s ||
        p.asset !== s.asset ||
        p.currency !== s.currency ||
        p.params.get('type') !== 'quote'
    )
        return false;
    for (const [key, value] of s.params) {
        if (key !== 'type' && p.params.get(key) !== value) return false;
    }
    if ('baseCurrency' in input) return [...p.params.keys()].every((k) => s.params.has(k));
    return (
        normalizedPeriod(p.params.get('term')) === normalizedPeriod(input.term) &&
        [...p.params.keys()].every((k) => k === 'term' || s.params.has(k))
    );
}
export function observationTime(text: string): number {
    // C++ timestamps use a space separator and six fractional digits. Interpret explicitly as UTC.
    if (
        !/^\d{4}-\d{2}-\d{2}[T ](?:[01]\d|2[0-3]):[0-5]\d:[0-5]\d(?:\.\d{1,6})?Z$/.test(text) ||
        !date.safeParse(text.slice(0, 10)).success
    )
        throw new AppError('invalid_market_data', 'Observation has an invalid UTC timestamp.');
    const ms = Date.parse(text.replace(' ', 'T').replace(/(\.\d{3})\d+Z$/, '$1Z'));
    if (!Number.isFinite(ms))
        throw new AppError('invalid_market_data', 'Observation has an invalid UTC timestamp.');
    return ms;
}
function observationMicros(text: string, milliseconds: number) {
    const fraction = /\.(\d{1,6})Z$/.exec(text)?.[1] ?? '';
    return BigInt(milliseconds) * 1000n + BigInt(fraction.padEnd(6, '0').slice(3));
}
const instrumentNames: Record<string, string> = {
    ir_swap: 'swap rate',
    mm: 'money-market rate',
    zero: 'zero rate',
    discount: 'discount factor',
};
export function describeMarketSeries(uri: string): string {
    const datum = parseDatum(uri);
    if (!datum || datum.asset !== 'ir') return uri;
    return [
        instrumentNames[datum.params.get('instrument') ?? ''] ?? 'IR series',
        ...(
            [
                ['curve_id', 'curve'],
                ['index_name', 'index'],
                ['tenor', 'floating tenor'],
                ['fwd_start', 'forward start'],
            ] as const
        ).flatMap(([key, label]) =>
            datum.params.has(key) ? [label + ' ' + datum.params.get(key)] : [],
        ),
    ].join(', ');
}
function checked(raw: unknown) {
    const r = envelope.parse(raw);
    if (r.result.outcome !== 'ok')
        throw new AppError(
            'backend_read_failed',
            'Backend refused or could not complete the market read.',
            403,
        );
    return r;
}
export class MarketLookup {
    constructor(
        private backend: Backend,
        private now: () => number = Date.now,
    ) {}
    async lookup(
        kind: 'fx' | 'ir',
        input: Fx | Ir,
        session: Session,
        signal?: AbortSignal,
    ): Promise<unknown> {
        const started = this.now(),
            snapshot = new Date(started).toISOString();
        if (input.asOf && input.asOf > snapshot.slice(0, 10))
            throw new AppError('future_as_of', 'Future valuation dates are not supported.', 400);
        const cutoff = input.asOf
            ? Math.min(started, Date.parse(input.asOf + 'T23:59:59.999Z'))
            : started;
        const cutoffText =
            input.asOf && input.asOf < snapshot.slice(0, 10)
                ? input.asOf + 'T23:59:59.999999Z'
                : new Date(cutoff).toISOString();
        const cutoffMicros = observationMicros(cutoffText, cutoff);
        if ('baseCurrency' in input && input.baseCurrency === input.quoteCurrency)
            throw new AppError('invalid_pair', 'Use two different currencies.', 400);
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(20000)]);
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= this.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        const base = {
            tool: kind === 'fx' ? 'read_fx_spot' : 'read_ir_rate',
            request: input,
            partyId: session.partyId,
            source: 'OreStudio',
            retrievedAt: snapshot,
            asOf: cutoffText,
            historicalRequest: Boolean(input.asOf),
            maxAgeHours: input.maxAgeHours,
        };
        const candidates: Series[] = [],
            ids = new Set<string>();
        let total: number | undefined;
        for (let offset = 0; offset < MAX_ROWS; offset += PAGE) {
            check();
            const raw = await this.backend.request(
                'series',
                {
                    offset,
                    limit: PAGE,
                    order: { field: '', descending: false },
                    filter:
                        input.provider || input.seriesId
                            ? {
                                  provider_code: input.provider ?? null,
                                  id_one_of: input.seriesId ? [input.seriesId] : null,
                                  provider_code_one_of: null,
                              }
                            : null,
                    as_of: snapshot,
                },
                session,
                deadline,
            );
            check();
            const r = checked(raw);
            const rows = z
                .object({ market_series: z.array(marketSeries).max(PAGE) })
                .parse(raw).market_series;
            if (
                (total !== undefined && r.total !== total) ||
                rows.length !== Math.min(PAGE, Math.max(0, r.total - offset)) ||
                rows.some((row) => ids.has(row.id))
            )
                return {
                    ...base,
                    status: 'incomplete',
                    reason: 'Catalogue changed while reading; retry.',
                };
            total = r.total;
            for (const row of rows) {
                ids.add(row.id);
                // No implicit cross-party or system catalogue fallback; these are selected-party observations.
                if (
                    row.tenant_id.toLowerCase() === session.tenantId.toLowerCase() &&
                    row.party_id.toLowerCase() === session.partyId.toLowerCase() &&
                    seriesMatches(row, input)
                )
                    candidates.push(row);
            }
            if (offset + rows.length >= r.total) break;
            if (!rows.length || offset + PAGE >= MAX_ROWS)
                return {
                    ...base,
                    status: 'incomplete',
                    reason: 'Catalogue scan limit reached; specify a provider or series ID.',
                };
        }
        if (!candidates.length)
            return {
                ...base,
                status: 'not_found',
                reason: 'No matching series in the selected party. No inverse, cross rate or other-party fallback was used.',
            };
        const choice = (s: Series) => ({
            seriesId: s.id,
            provider: s.provider_code,
            instrument: s.oresmd_uri,
        });
        if (
            candidates.length !== 1 ||
            (kind === 'ir' && !('baseCurrency' in input) && !input.instrument && !input.seriesId)
        )
            return {
                ...base,
                status: 'ambiguous',
                reason:
                    kind === 'ir'
                        ? 'Choose the intended rate instrument or series. The list shows series metadata; the requested maturity is checked after selection.'
                        : 'Choose a provider or exact series ID; no provider was selected automatically.',
                candidates: candidates.slice(0, 20).map(choice),
                candidateCount: candidates.length,
            };
        const selected = candidates[0]!;
        const selectedInstrument = parseDatum(selected.oresmd_uri)?.params.get('instrument');
        const identity = {
            ...base,
            series: choice(selected),
            units:
                kind === 'fx'
                    ? `${(input as Fx).quoteCurrency} per 1 ${(input as Fx).baseCurrency}`
                    : selectedInstrument === 'discount'
                      ? 'discount factor'
                      : 'decimal rate',
        };
        const seen = new Set<string>();
        let observationTotal: number | undefined, previousInstant: bigint | undefined;
        let latest: z.infer<typeof observation> | undefined,
            latestTime = -Infinity;
        let latestInstant: bigint | undefined;
        for (let offset = 0; offset < MAX_ROWS; offset += PAGE) {
            check();
            const raw = await this.backend.request(
                'observations',
                {
                    series_id: selected.id,
                    scope: 'direct',
                    offset,
                    limit: PAGE,
                    // Empty field + false means backend default observation_datetime DESC, id ASC. Explicit fields are unsupported.
                    order: { field: '', descending: false },
                    filter: {
                        series_id: null,
                        observation_datetime: null,
                        id_one_of: null,
                        series_id_one_of: null,
                        observation_datetime_one_of: null,
                        observation_datetime_from: null,
                        observation_datetime_to: cutoffText,
                    },
                },
                session,
                deadline,
            );
            check();
            const r = checked(raw);
            const rows = z
                .object({ market_observations: z.array(observation).max(PAGE) })
                .parse(raw).market_observations;
            if (
                (observationTotal !== undefined && r.total !== observationTotal) ||
                rows.length !== Math.min(PAGE, Math.max(0, r.total - offset))
            )
                return {
                    ...identity,
                    status: 'incomplete',
                    reason: 'Observations changed while reading; retry.',
                };
            observationTotal = r.total;
            for (const row of rows) {
                const time = observationTime(row.observation_datetime);
                const instant = observationMicros(row.observation_datetime, time);
                const key = instant + row.oresmd_uri;
                if ((previousInstant !== undefined && instant > previousInstant) || seen.has(key))
                    return {
                        ...identity,
                        status: 'incomplete',
                        reason: 'Observation order or page consistency could not be verified.',
                    };
                previousInstant = instant;
                seen.add(key);
                if (
                    row.series_id.toLowerCase() !== selected.id.toLowerCase() ||
                    row.tenant_id.toLowerCase() !== session.tenantId.toLowerCase() ||
                    row.party_id.toLowerCase() !== session.partyId.toLowerCase() ||
                    instant > cutoffMicros
                )
                    throw new AppError(
                        'invalid_market_data',
                        'Backend observation did not match the requested identity or cutoff.',
                    );
                if (!pointMatches(row.oresmd_uri, selected, input)) continue;
                if (
                    !/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/.test(row.value) ||
                    !Number.isFinite(Number(row.value)) ||
                    ((kind === 'fx' || selectedInstrument === 'discount') &&
                        Number(row.value) <= 0) ||
                    !row.source
                )
                    throw new AppError(
                        'invalid_market_data',
                        'Observation has an invalid value or missing source.',
                    );
                if (latest && instant === latestInstant)
                    return {
                        ...identity,
                        status: 'ambiguous',
                        reason: 'Multiple values at the latest observation time; no value was selected.',
                    };
                if (!latest) {
                    latest = row;
                    latestTime = time;
                    latestInstant = instant;
                }
            }
            // Read through the complete timestamp group so a duplicate/conflicting latest point cannot be hidden on the next page.
            if (
                latestInstant !== undefined &&
                ((previousInstant !== undefined && previousInstant < latestInstant) ||
                    offset + rows.length >= r.total)
            )
                break;
            if (offset + rows.length >= r.total) break;
            if (!rows.length || offset + PAGE >= MAX_ROWS)
                return {
                    ...identity,
                    status: 'incomplete',
                    reason: 'Observation scan limit reached; no value was selected.',
                };
        }
        if (!latest)
            return {
                ...identity,
                status: 'no_observation',
                reason: 'No matching point observation exists on or before the requested time.',
            };
        const ageHours = (cutoff - latestTime) / 3600000,
            stale = ageHours > input.maxAgeHours;
        return {
            ...identity,
            status: stale ? 'stale' : 'ok',
            freshness: { ageHours, stale },
            observation: {
                value: latest.value,
                observedAt:
                    latestInstant! % 1000n === 0n
                        ? new Date(latestTime).toISOString()
                        : latest.observation_datetime.replace(' ', 'T'),
                source: latest.source,
                instrument: latest.oresmd_uri,
            },
            warning: stale
                ? 'Historical/stale observation. Do not present this as a current market quote.'
                : input.asOf
                  ? 'Value is for the requested historical valuation date, not a current quote.'
                  : null,
        };
    }
}

// Critical market facts are rendered from validated tool output, never invented by a second model turn.
export function formatMarketResult(raw: unknown): string | undefined {
    if (
        !raw ||
        typeof raw !== 'object' ||
        !['read_fx_spot', 'read_ir_rate'].includes((raw as { tool: string }).tool)
    )
        return;
    const r = z
        .object({
            tool: z.string(),
            status: z.string(),
            asOf: z.string(),
            historicalRequest: z.boolean(),
            request: z.record(z.string(), z.unknown()),
            reason: z.string().optional(),
            units: z.string().optional(),
            series: z
                .object({ provider: z.string(), seriesId: z.string(), instrument: z.string() })
                .optional(),
            candidates: z
                .array(
                    z.object({
                        provider: z.string(),
                        seriesId: z.string(),
                        instrument: z.string(),
                    }),
                )
                .optional(),
            observation: z
                .object({ value: z.string(), observedAt: z.string(), source: z.string() })
                .optional(),
        })
        .parse(raw);
    const label =
        r.tool === 'read_fx_spot'
            ? `${r.request.baseCurrency}/${r.request.quoteCurrency} spot`
            : [
                  r.request.currency,
                  r.request.term,
                  instrumentNames[
                      String(
                          r.request.instrument ??
                              (r.series
                                  ? parseDatum(r.series.instrument)?.params.get('instrument')
                                  : ''),
                      )
                  ] ?? 'rate',
              ].join(' ');
    if (r.status === 'ambiguous')
        return r.candidates?.length
            ? [
                  label + '. ' + r.reason,
                  r.candidates
                      .map(
                          (c, index) =>
                              `${index + 1}. ${markdownText(c.provider)} · ${markdownText(describeMarketSeries(c.instrument))} [\`${c.seriesId}\`]`,
                      )
                      .join('\n'),
                  'Please specify the provider, option number or series ID. For rates, you can also give the instrument, curve, index, floating tenor or forward start.',
              ].join('\n\n')
            : label + '. ' + r.reason;
    if (!['ok', 'stale'].includes(r.status) || !r.observation)
        return `${label}: ${r.reason ?? 'No usable observation.'}${r.series ? `\nProvider: ${r.series.provider}.` : ''}\nRequested cutoff: ${r.asOf}.`;
    return `${r.status === 'stale' ? 'Historical/stale observation — not a current market quote.\n' : r.historicalRequest ? 'Historical lookup.\n' : 'Latest recorded observation.\n'}${label}: ${r.observation.value} (${r.units}).\nObserved: ${r.observation.observedAt}.\nProvider: ${markdownText(r.series?.provider)}. Source: ${markdownText(r.observation.source)}.\n${r.tool === 'read_ir_rate' && r.series ? 'Series: ' + markdownText(describeMarketSeries(r.series.instrument)) + '\n' : ''}Requested cutoff: ${r.asOf}.`;
}

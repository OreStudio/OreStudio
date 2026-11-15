// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { marketSeries, marketId, observationTime, parseDatum } from './market.js';
import { readPages } from './read-pages.js';
import { maturity, type CurveInput } from './fx-curve.js';
import { FxAnalytics } from './fx-analytics.js';
import {
    marketCurveInput,
    testDatasetConventions,
    type MarketCurveRequest,
    type MarketCurveField,
} from './fx-market-request.js';

export const marketCurveTools = {
    build_fx_curve_from_market: {
        description:
            'Build a EUR/USD forward curve from stored market inputs. Give the provider, named USD discount curve and valuation date when known. The agent resolves spot and forward series and can ask for missing conventions; omit unknown values and IDs. Use build_fx_forward_curve for supplied quotes and discount factors.',
        schema: marketCurveInput,
    },
} as const;
const observation = z.object({
    series_id: marketId,
    tenant_id: marketId,
    party_id: marketId,
    observation_datetime: z.string().max(100),
    source: z.string().max(1000),
    oresmd_uri: z.string().max(2048),
    value: z.string().max(200),
});
const envelope = z.object({
    result: z.object({ outcome: z.string() }),
    total: z.number().int().nonnegative(),
});
type Series = z.infer<typeof marketSeries>;
type Observation = z.infer<typeof observation>;
const calendarOf = (request: MarketCurveRequest): CurveInput['calendar'] => ({
    businessDayConvention:
        request.calendar === 'weekends_following'
            ? 'following'
            : request.calendar === 'weekends_modified_following'
              ? 'modified_following'
              : 'unadjusted',
    endOfMonth: request.endOfMonth!,
    holidays: [],
});
const periodDate = (anchor: string, term: string, calendar: CurveInput['calendar']) => {
    if (z.iso.date().safeParse(term).success) return term;
    const m = /^([1-9]\d?)([MY])$/.exec(term);
    if (!m)
        throw new Error(
            `The stored maturity ${term} cannot be resolved with the selected monthly/yearly convention.`,
        );
    return maturity(anchor, Number(m[1]) * (m[2] === 'Y' ? 12 : 1), calendar);
};
const ref = (row: Observation) => ({
    seriesId: row.series_id,
    observedAt: row.observation_datetime.replace(' ', 'T'),
    source: row.source,
    uri: row.oresmd_uri,
});

export class FxMarketCurve {
    private analytics: FxAnalytics;
    constructor(private backend: Backend) {
        this.analytics = new FxAnalytics(backend);
    }

    async execute(raw: unknown, session: Session, signal?: AbortSignal) {
        const request = marketCurveInput.parse(raw);
        const tool = 'build_fx_curve_from_market';
        const base = { tool, request, source: 'OreStudio over NATS; selected-party data only' };
        const ask = (
            field: MarketCurveField,
            reason: string,
            candidates: { id: string; name: string; aliases?: string[] }[] = [],
        ) => ({
            ...base,
            status: 'selection_required',
            field,
            reason:
                reason +
                ([
                    'spotDate',
                    'forwardQuote',
                    'pointSize',
                    'calendar',
                    'endOfMonth',
                    'usdPillarAnchor',
                ].includes(field) && testDatasetConventions(request)
                    ? '\n\nFor these labelled synthetic test inputs, you can reply **use test dataset conventions**. This selects spot date 2016-12-30, outright forwards, unadjusted dates, no end-of-month rolling, and USD tenors measured from valuation date. You can also supply your own conventions.'
                    : ''),
            candidates,
        });
        if (!request.asOf)
            return ask(
                'asOf',
                (request.spotDate ||
                request.forwardQuote ||
                request.calendar ||
                request.endOfMonth !== undefined ||
                request.usdPillarAnchor
                    ? 'I received your conventions, but there is no active curve request with a valuation date. This can happen after signing in again or when the previous request expires. I have kept the conventions for your next reply.\n\n'
                    : 'I can build an EUR/USD forward curve from stored OreStudio data. ') +
                    'Which valuation date should I use to select the observations? Reply with the original request including provider, USD curve name and “as of YYYY-MM-DD”, or give those details one at a time. The spot date alone does not select the observation date.',
            );
        if (request.asOf > new Date().toISOString().slice(0, 10))
            return {
                ...base,
                status: 'invalid_input',
                reason: 'The valuation date is in the future. Select a stored-data date.',
            };
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]);
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= Date.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        const read = async (op: 'series' | 'observations', args: unknown) => {
            check();
            const raw = await this.backend.request(op, args, session, deadline);
            check();
            if (envelope.parse(raw).result.outcome !== 'ok')
                throw new AppError(
                    'backend_denied',
                    'OreStudio did not authorize or complete the market-data read.',
                    403,
                );
            return raw;
        };
        try {
            const all = await readPages(
                async (offset, limit) => {
                    const r = z
                        .object({
                            total: z.number().int().nonnegative(),
                            market_series: z.array(marketSeries).max(50),
                        })
                        .parse(
                            await read('series', {
                                offset,
                                limit,
                                order: { field: '', descending: false },
                                filter: null,
                                as_of: null,
                            }),
                        );
                    return { total: r.total, rows: r.market_series };
                },
                (s) => s.id,
                {
                    maxRows: 2000,
                    code: 'incomplete_analytics_inputs',
                    message:
                        'The market-series scan changed or exceeded its bound; no complete selection is claimed.',
                },
            );
            const own = all.filter(
                (s) =>
                    s.tenant_id.toLowerCase() === session.tenantId.toLowerCase() &&
                    s.party_id.toLowerCase() === session.partyId.toLowerCase(),
            );
            const fx = (s: Series, instrument: string) => {
                const d = parseDatum(s.oresmd_uri);
                return (
                    d?.asset === 'fx' &&
                    d.currency === 'EUR' &&
                    d.params.get('ccy') === 'USD' &&
                    d.params.get('instrument') === instrument &&
                    d.params.get('type') === 'series'
                );
            };
            const discount = own.filter((s) => {
                const d = parseDatum(s.oresmd_uri);
                return (
                    d?.asset === 'ir' &&
                    d.currency === 'USD' &&
                    d.params.get('instrument') === 'discount' &&
                    d.params.get('type') === 'series'
                );
            });
            const providers = [
                ...new Set(
                    own
                        .filter((s) => fx(s, 'fx_spot') || fx(s, 'fx_fwd'))
                        .map((s) => s.provider_code),
                ),
            ].sort();
            if (!request.provider && !providers.length)
                return {
                    ...base,
                    status: 'not_found',
                    reason: 'No EUR/USD spot or forward provider is available in the selected party’s market-series catalogue.',
                };
            if (!request.provider)
                return ask(
                    'provider',
                    'Which provider should supply EUR/USD spot and forward quotes?',
                    providers.map((p) => ({ id: p, name: p })),
                );
            const providerMatches = (s: Series) =>
                s.provider_code.toLowerCase() === request.provider!.toLowerCase();
            const spots = own.filter(
                (s) =>
                    fx(s, 'fx_spot') &&
                    providerMatches(s) &&
                    (!request.spotSeriesId ||
                        s.id.toLowerCase() === request.spotSeriesId.toLowerCase()),
            );
            const forwards = own.filter(
                (s) =>
                    fx(s, 'fx_fwd') &&
                    providerMatches(s) &&
                    (!request.forwardSeriesId ||
                        s.id.toLowerCase() === request.forwardSeriesId.toLowerCase()),
            );
            const curves = discount.filter(
                (s) =>
                    !request.usdCurve ||
                    s.id.toLowerCase() === request.usdCurve.toLowerCase() ||
                    parseDatum(s.oresmd_uri)?.params.get('curve_id')?.toLowerCase() ===
                        request.usdCurve.toLowerCase(),
            );
            const missing = [
                ...(!spots.length ? [`EUR/USD spot from ${request.provider}`] : []),
                ...(!forwards.length
                    ? [`EUR/USD market forward quotes from ${request.provider}`]
                    : []),
                ...(!curves.length
                    ? [`USD discount curve ${request.usdCurve ?? '(no available curve)'}`]
                    : []),
            ];
            if (missing.length)
                return {
                    ...base,
                    status: 'not_found',
                    missing,
                    coverage: {
                        catalogueRows: all.length,
                        selectedPartyRows: own.length,
                        complete: true,
                    },
                    reason: `No selected-party series was found for: ${missing.join('; ')}. No synthetic data or other party's data was substituted.`,
                };
            const choose = (field: 'usdCurve', rows: Series[]) =>
                ask(
                    field,
                    'Choose the exact USD discount-curve series; a curve name can exist under several providers.',
                    rows.map((s) => ({
                        id: s.id,
                        name: `${parseDatum(s.oresmd_uri)?.params.get('curve_id')} from ${s.provider_code} (${s.id})`,
                        aliases: [parseDatum(s.oresmd_uri)!.params.get('curve_id')!],
                    })),
                );
            if (!request.usdCurve || curves.length > 1) return choose('usdCurve', curves);
            if (spots.length > 1)
                return ask(
                    'spotSeriesId',
                    'Choose the exact EUR/USD spot series.',
                    spots.map((s) => ({ id: s.id, name: `${s.oresmd_uri} (${s.id})` })),
                );
            if (forwards.length > 1)
                return ask(
                    'forwardSeriesId',
                    'Choose the exact EUR/USD forward series.',
                    forwards.map((s) => ({ id: s.id, name: `${s.oresmd_uri} (${s.id})` })),
                );
            const selected = [spots[0]!, forwards[0]!, curves[0]!] as const;
            const cutoff = Math.min(Date.parse(`${request.asOf}T23:59:59.999Z`), Date.now());
            const readPoints = async (series: Series) => {
                const rows = await readPages(
                    async (offset, limit) => {
                        const r = z
                            .object({
                                total: z.number().int().nonnegative(),
                                market_observations: z.array(observation).max(50),
                            })
                            .parse(
                                await read('observations', {
                                    series_id: series.id,
                                    scope: 'direct',
                                    offset,
                                    limit,
                                    order: { field: '', descending: false },
                                    filter: null,
                                }),
                            );
                        if (
                            r.market_observations.some(
                                (o) =>
                                    o.tenant_id.toLowerCase() !== session.tenantId.toLowerCase() ||
                                    o.party_id.toLowerCase() !== session.partyId.toLowerCase() ||
                                    o.series_id.toLowerCase() !== series.id.toLowerCase(),
                            )
                        )
                            throw new AppError(
                                'party_mismatch',
                                'Market observations do not match the selected series and session.',
                                403,
                            );
                        return { total: r.total, rows: r.market_observations };
                    },
                    (o) => `${o.observation_datetime}|${o.source}|${o.oresmd_uri}`,
                    {
                        maxRows: 500,
                        code: 'incomplete_analytics_inputs',
                        message:
                            'Market observations changed, were duplicated/incomplete or exceeded the read bound.',
                    },
                );
                const latest = new Map<string, Observation>();
                for (const row of rows) {
                    const time = observationTime(row.observation_datetime);
                    if (row.observation_datetime.slice(0, 10) !== request.asOf || time > cutoff)
                        continue;
                    const s = parseDatum(series.oresmd_uri),
                        p = parseDatum(row.oresmd_uri);
                    if (
                        !s ||
                        !p ||
                        p.asset !== s.asset ||
                        p.currency !== s.currency ||
                        p.params.get('type') !== 'quote' ||
                        [...s.params].some(([k, v]) => k !== 'type' && p.params.get(k) !== v)
                    )
                        throw new Error(
                            'An observation disagrees with its selected series identity.',
                        );
                    const previous = latest.get(row.oresmd_uri);
                    if (previous && observationTime(previous.observation_datetime) === time)
                        throw new Error(
                            'Conflicting observations at the same timestamp require a different provider or date.',
                        );
                    if (!previous || observationTime(previous.observation_datetime) < time)
                        latest.set(row.oresmd_uri, row);
                }
                return [...latest.values()];
            };
            const spotRows = await readPoints(selected[0]);
            const forwardRows = await readPoints(selected[1]);
            const usdRows = await readPoints(selected[2]);
            if (spotRows.length !== 1 || forwardRows.length < 2 || !usdRows.length)
                return {
                    ...base,
                    status: 'no_observation',
                    reason: `Stored observations on ${request.asOf} are incomplete: ${spotRows.length} spot, ${forwardRows.length} forward pillars and ${usdRows.length} USD discount pillars. No stale observations, rates-as-discounts or synthetic inputs were substituted.`,
                };
            if (!request.spotDate)
                return ask(
                    'spotDate',
                    'I found the stored spot, forward quotes and USD discount inputs. The curve has not been built yet: I need their settlement and quote conventions.\n\nWhat is the spot settlement date (the date the spot currency exchange settles)? It can differ from the valuation date used to select prices. Reply “spot date YYYY-MM-DD”. You can give the remaining conventions in the same reply.',
                );
            if (!request.forwardQuote)
                return ask(
                    'forwardQuote',
                    'Are the stored forward quotes outrights or points? An outright is the full exchange rate, such as 1.06 USD per EUR. Points are an adjustment added to spot after applying a point size. The stored quote type does not establish this. Reply “outrights” or “points”.',
                    [
                        { id: 'outright', name: 'outrights' },
                        { id: 'points', name: 'points' },
                    ],
                );
            if (request.forwardQuote === 'points' && !request.pointSize)
                return ask(
                    'pointSize',
                    'What is one forward point in USD per EUR? I will calculate each outright as spot + points × point size. Reply, for example, “point size 0.0001”, using the scale of your stored quotes.',
                );
            if (!request.calendar)
                return ask(
                    'calendar',
                    'How should maturity dates be adjusted? “Unadjusted” keeps the calculated date. “Following, weekends only” moves a weekend date to Monday; “modified following, weekends only” instead moves backward if that crosses a month. These profiles do not include currency holidays. Reply with one of the choices below.',
                    [
                        { id: 'unadjusted', name: 'unadjusted' },
                        { id: 'weekends_following', name: 'following, weekends only' },
                        {
                            id: 'weekends_modified_following',
                            name: 'modified following, weekends only',
                        },
                    ],
                );
            if (request.endOfMonth === undefined)
                return ask(
                    'endOfMonth',
                    'Should a tenor starting on the last day of a month also end on the last day of its maturity month? Reply “use end of month” or “no end of month”. This affects the settlement dates used to build the curve.',
                );
            if (
                !request.usdPillarAnchor &&
                usdRows.some(
                    (o) =>
                        !z.iso.date().safeParse(parseDatum(o.oresmd_uri)?.params.get('term'))
                            .success,
                )
            )
                return ask(
                    'usdPillarAnchor',
                    'The USD discount inputs use tenor labels rather than explicit maturity dates. From which date should I measure those tenors? Reply “USD tenors use valuation date” or “USD tenors use spot date”. This determines which USD discount factor matches each forward settlement date.',
                    [
                        { id: 'valuation', name: 'valuation date' },
                        { id: 'spot', name: 'spot date' },
                    ],
                );
            const calendar = calendarOf(request);
            const forwardPoints = forwardRows.map((row) => {
                const term = parseDatum(row.oresmd_uri)?.params.get('term');
                if (!term || !/^[1-9]\d?[MY]$/.test(term))
                    throw new Error(
                        'Forward quotes need explicit monthly/yearly tenor labels; an unresolved date or convention was not guessed.',
                    );
                return {
                    tenor: term,
                    date: periodDate(request.spotDate!, term, calendar),
                    value: ref(row),
                };
            });
            const points: CurveInput['usdCurve']['points'] = usdRows
                .map((row) => {
                    const term = parseDatum(row.oresmd_uri)?.params.get('term');
                    if (!term) throw new Error('A USD discount point has no maturity coordinate.');
                    return {
                        date: periodDate(
                            request.usdPillarAnchor === 'spot' ? request.spotDate! : request.asOf!,
                            term,
                            calendar,
                        ),
                        discountFactor: ref(row),
                    };
                })
                .sort((a, b) => a.date.localeCompare(b.date));
            if (!points.some((p) => p.date === request.asOf))
                points.unshift({ date: request.asOf, discountFactor: 1 });
            const input = {
                valuationDate: request.asOf,
                spotDate: request.spotDate,
                inputReference: 'Selected OreStudio observations; agent-local construction',
                spot: ref(spotRows[0]!),
                quoteConvention: request.forwardQuote,
                pipSize: request.pointSize ?? 0.0001,
                calendar,
                usdCurve: {
                    reference: `${parseDatum(selected[2].oresmd_uri)?.params.get('curve_id')} / ${selected[2].id}`,
                    points,
                },
                forwards: forwardPoints.sort((a, b) => a.date.localeCompare(b.date)),
                tolerance: { absolute: 1e-10, relative: 1e-10 },
            };
            const result = await this.analytics.execute(
                'build_fx_forward_curve',
                input,
                session,
                deadline,
            );
            return {
                ...result,
                tool,
                request,
                selection: {
                    provider: request.provider,
                    asOf: request.asOf,
                    series: selected.map((s) => ({
                        id: s.id,
                        uri: s.oresmd_uri,
                        provider: s.provider_code,
                    })),
                    discountAnchor:
                        'Discount factor 1 at the USD valuation reference date is mathematical normalization, not a market quote.',
                    calendarProfile: request.calendar,
                },
                source: 'Selected OreStudio observations over NATS; agent-derived analysis',
            };
        } catch (error) {
            check();
            if (error instanceof AppError) throw error;
            return {
                ...base,
                status: 'invalid_input',
                reason:
                    error instanceof z.ZodError
                        ? 'OreStudio returned malformed market data.'
                        : (error as Error).message,
            };
        }
    }
}

export function formatMarketCurve(raw: unknown): string | undefined {
    const r = raw as {
        tool?: string;
        reason?: string;
        candidates?: { id: string; name: string }[];
    };
    if (r.tool !== 'build_fx_curve_from_market') return;
    return r.reason
        ? [r.reason, (r.candidates ?? []).map((c) => `- ${c.name}`).join('\n')]
              .filter(Boolean)
              .join('\n\n')
        : undefined;
}

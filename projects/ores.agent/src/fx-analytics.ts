// SPDX-License-Identifier: GPL-3.0-or-later
import { createHash } from 'node:crypto';
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { marketId, observationTime, parseDatum } from './market.js';
import { readPages } from './read-pages.js';
import { buildCurve, curveInput } from './fx-curve.js';
import { buildSurface, surfaceInput } from './fx-surface.js';
import { markdownTable, markdownText } from './markdown.js';

export const analyticsTools = {
    build_fx_forward_curve: {
        description:
            'Build a EUR/USD forward curve from supplied spot, forwards and USD discount factors. Requires dates, calendar, quote units and tolerances. An exact stored observation can replace a numeric input when seriesId, observedAt, source and URI are known. For lookup by provider and named USD curve use build_fx_curve_from_market. Returns interpolated forwards and repricing residuals.',
        schema: curveInput,
    },
    build_fx_volatility_surface: {
        description:
            'Build a EUR/USD Black volatility surface using validated curve inputs and supplied ATM, 10-delta and 25-delta risk reversals and butterflies. Requires explicit delta, ATM and butterfly conventions. Returns smile nodes, quote recovery, arbitrage checks and extrapolation warnings. Use a forward-curve tool first if only forwards were requested.',
        schema: surfaceInput,
    },
} as const;

const observation = z.object({
    series_id: marketId,
    tenant_id: marketId,
    party_id: marketId,
    observation_datetime: z.string().max(100),
    value: z.string().max(200),
    source: z.string().max(1000),
    oresmd_uri: z.string().max(2048),
});
const reply = z.object({
    result: z.object({ outcome: z.string() }),
    total: z.number().int().nonnegative(),
    market_observations: z.array(observation).max(50),
});
const hash = (value: unknown) => createHash('sha256').update(JSON.stringify(value)).digest('hex');
const instant = (text: string) => {
    observationTime(text);
    return text
        .replace(' ', 'T')
        .replace(
            /(?:\.(\d{1,6}))?Z$/,
            (_match, fraction: string | undefined) => `.${(fraction ?? '').padEnd(6, '0')}Z`,
        );
};

export class FxAnalytics {
    constructor(private backend: Backend) {}

    async execute(
        name: keyof typeof analyticsTools,
        raw: unknown,
        session: Session,
        signal?: AbortSignal,
    ) {
        const parsed = analyticsTools[name].schema.safeParse(raw);
        if (!parsed.success) {
            return {
                tool: name,
                status: 'selection_required',
                field: 'analyticsInputs',
                request:
                    raw && typeof raw === 'object' && !Array.isArray(raw)
                        ? (raw as Record<string, unknown>)
                        : {},
                issues: parsed.error.issues.map((i) => ({
                    path: i.path.join('.'),
                    message: i.message,
                })),
                reason: 'Supply the missing or corrected inputs and explicit conventions. No curve or surface has been validated.',
            };
        }
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]);
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= Date.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        const request = parsed.data,
            provenance: Record<string, unknown>[] = [];
        const cache = new Map<string, number>();
        let reads = 0;
        const valuation = 'curve' in request ? request.curve.valuationDate : request.valuationDate;
        const resolve = async (value: unknown, path: string): Promise<unknown> => {
            check();
            if (Array.isArray(value)) {
                const result: unknown[] = [];
                for (const [i, v] of value.entries()) result.push(await resolve(v, `${path}.${i}`));
                return result;
            }
            if (!value || typeof value !== 'object') return value;
            const obj = value as Record<string, unknown>;
            if (typeof obj.seriesId === 'string') {
                const key = JSON.stringify(obj);
                if (cache.has(key)) return cache.get(key)!;
                if (++reads > 64)
                    throw new Error(
                        'At most 64 distinct referenced observations are supported per construction.',
                    );
                const requestedInstant = instant(obj.observedAt as string);
                if (
                    Date.parse(obj.observedAt as string) >
                        Date.parse(`${valuation}T23:59:59.999Z`) ||
                    Date.parse(obj.observedAt as string) > Date.now()
                )
                    throw new Error('Referenced observation is later than valuation or now.');
                if ((obj.observedAt as string).slice(0, 10) !== valuation)
                    throw new Error(
                        'Referenced observations must be from the valuation date; stale or mixed-date inputs were not selected.',
                    );
                const rows = await readPages(
                    async (offset, limit) => {
                        check();
                        const r = reply.parse(
                            await this.backend.request(
                                'observations',
                                {
                                    series_id: obj.seriesId,
                                    scope: 'direct',
                                    offset,
                                    limit,
                                    order: { field: '', descending: false },
                                    filter: null,
                                },
                                session,
                                deadline,
                            ),
                        );
                        check();
                        if (r.result.outcome !== 'ok')
                            throw new AppError(
                                'backend_denied',
                                'Observation read was not authorized or completed.',
                                403,
                            );
                        if (
                            r.market_observations.some(
                                (row) =>
                                    row.series_id.toLowerCase() !==
                                        (obj.seriesId as string).toLowerCase() ||
                                    row.tenant_id.toLowerCase() !==
                                        session.tenantId.toLowerCase() ||
                                    row.party_id.toLowerCase() !== session.partyId.toLowerCase(),
                            )
                        )
                            throw new AppError(
                                'party_mismatch',
                                'Referenced observation identity does not match the selected session.',
                                403,
                            );
                        return { total: r.total, rows: r.market_observations };
                    },
                    (row) => `${row.observation_datetime}|${row.source}|${row.oresmd_uri}`,
                    {
                        maxRows: 500,
                        code: 'incomplete_analytics_inputs',
                        message:
                            'Referenced observation pages changed, were incomplete or exceeded the scan limit.',
                    },
                );
                const matches = rows.filter(
                    (row) =>
                        instant(row.observation_datetime) === requestedInstant &&
                        row.source === obj.source &&
                        row.oresmd_uri === obj.uri,
                );
                if (matches.length !== 1)
                    throw new Error(
                        `Exactly one observation is required at ${path}; found ${matches.length}.`,
                    );
                const row = matches[0]!;
                const datum = parseDatum(row.oresmd_uri);
                const isDiscount = /usdCurve\.points\.\d+\.discountFactor$/.test(path);
                const isSpot = /(?:^|\.)spot$/.test(path);
                const wing = /^quotes\.\d+\.(atm|rr10|bf10|rr25|bf25)$/.exec(path)?.[1];
                const label =
                    wing === 'atm'
                        ? 'ATM'
                        : wing
                          ? wing.slice(2) + wing.slice(0, 2).toUpperCase()
                          : undefined;
                if (
                    !datum ||
                    datum.params.get('type') !== 'quote' ||
                    (isDiscount
                        ? datum.asset !== 'ir' ||
                          datum.currency !== 'USD' ||
                          datum.params.get('instrument') !== 'discount' ||
                          datum.params.get('quote') !== 'rate'
                        : datum.asset !== 'fx' ||
                          datum.currency !== 'EUR' ||
                          datum.params.get('ccy') !== 'USD' ||
                          datum.params.get('instrument') !==
                              (isSpot ? 'fx_spot' : wing ? 'fx_option' : 'fx_fwd') ||
                          datum.params.get('quote') !== (wing ? 'rate_lnvol' : 'rate') ||
                          Boolean(wing && datum.params.get('strike_label') !== label))
                )
                    throw new Error(
                        `Referenced observation has incompatible units, instrument or currency at ${path}.`,
                    );
                if (!/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/.test(row.value))
                    throw new Error('Observation is not a numeric value.');
                const number = Number(row.value);
                if (!Number.isFinite(number)) throw new Error('Observation value is not finite.');
                provenance.push({ path, ...row });
                cache.set(key, number);
                return number;
            }
            const result: Record<string, unknown> = {};
            for (const [key, v] of Object.entries(obj))
                result[key] = await resolve(v, path ? `${path}.${key}` : key);
            return result;
        };
        try {
            const resolved = await resolve(request, '');
            check();
            const result =
                name === 'build_fx_forward_curve'
                    ? buildCurve(curveInput.parse(resolved)).result
                    : buildSurface(surfaceInput.parse(resolved));
            check();
            const complete =
                'gridComplete' in result ? result.gridComplete : result.targetsComplete;
            return {
                tool: name,
                status: !result.validated ? 'validation_failed' : complete ? 'ok' : 'incomplete',
                result,
                reproduction: {
                    implementation: 'agent-fx-analytics-1',
                    inputHash: hash(request),
                    resolvedInputHash: hash(resolved),
                    inputs: request,
                    resolvedInputs: resolved,
                    observations: provenance,
                },
                source: provenance.length
                    ? 'Supplied inputs and selected-party OreStudio observations; agent-derived analysis'
                    : 'Supplied inputs; agent-derived analysis',
                warning:
                    'Read-only analysis, not an executable quote or published curve/surface. Referenced reads are not an atomic server snapshot.',
            };
        } catch (error) {
            check();
            if (error instanceof AppError) throw error;
            return {
                tool: name,
                status: 'invalid_input',
                request,
                reason:
                    error instanceof z.ZodError
                        ? 'Resolved input does not match its declared type.'
                        : (error as Error).message,
            };
        }
    }
}

export function formatAnalytics(raw: unknown): string | undefined {
    const r = raw as {
        tool?: string;
        status?: string;
        reason?: string;
        issues?: { path: string; message: string }[];
        result?: ReturnType<typeof buildCurve>['result'] | ReturnType<typeof buildSurface>;
        reproduction?: {
            inputHash: string;
            resolvedInputHash: string;
            observations?: { source?: string }[];
        };
        source?: string;
    };
    if (!r.tool || !Object.hasOwn(analyticsTools, r.tool)) return;
    if (!r.result)
        return `${r.reason}\n${r.issues?.map((i) => `${i.path || 'inputs'}: ${i.message}`).join('\n') ?? ''}`.trim();
    const result = r.result;
    const display = (n: number | undefined) => (n === undefined ? 'Unavailable' : n.toFixed(6));
    const errorText = (n: number) => (n === 0 ? '0' : n.toExponential(2));
    const rows: string[] = [];
    if ('repricing' in result) {
        rows.push(
            result.validated
                ? `The EUR/USD forward curve was built. All ${result.repricing.length} input forwards passed the repricing checks.`
                : 'The EUR/USD curve did not pass validation. Review the failed repricing checks below.',
            'Each forward is USD per EUR: the number of dollars for €1 at that maturity. EUR discount factors are relative to spot settlement.',
            '### Quoted maturities',
            markdownTable(
                ['Tenor', 'Settlement date', 'Forward (USD/EUR)', 'Residual', 'Check'],
                result.repricing.map((q) => [
                    q.tenor,
                    q.date,
                    display(q.outright),
                    errorText(q.residual),
                    q.passed ? 'Passed' : 'Failed',
                ]),
            ),
            `Residual units: ${result.repricingUnits}. A residual is the difference between the reconstructed value and the input quote.`,
            '### Requested maturities',
            'An interpolated forward is calculated between input maturities; it is not a separate market quote.',
            markdownTable(
                ['Tenor', 'Settlement date', 'Forward (USD/EUR)', 'Type'],
                result.targets.map((q) => [q.tenor, q.date, display(q.outright), q.classification]),
            ),
        );
    } else {
        rows.push(
            result.validated
                ? 'The EUR/USD volatility surface passed quote recovery and the applicable finite-grid diagnostics.'
                : 'The EUR/USD volatility surface did not pass all validation checks. Review quote recovery and diagnostics below.',
            '### Strikes and volatilities',
            'Volatility is shown in decimal units: 0.120000 means 12%.',
            markdownTable(
                ['Expiry', 'Node', 'Strike', 'Volatility'],
                result.expiries.flatMap((s) =>
                    s.nodes.map((n) => [
                        s.expiryDate,
                        n.label,
                        display(n.strike),
                        display(n.volatility),
                    ]),
                ),
            ),
            '### Quote recovery',
            markdownTable(
                ['Expiry', 'Quote', 'Recovered', 'Error (volatility)', 'Check'],
                result.expiries.flatMap((s) =>
                    s.recovery.map((q) => [
                        s.expiryDate,
                        q.label,
                        display(q.recovered),
                        errorText(q.error),
                        q.passed ? 'Passed' : 'Failed',
                    ]),
                ),
            ),
        );
        if (result.grid.length)
            rows.push(
                '### Requested surface points',
                markdownTable(
                    ['Expiry', 'Strike', 'Volatility', 'Type'],
                    result.grid.map((g) => [
                        g.expiryDate,
                        display(g.strike),
                        'volatility' in g ? display(g.volatility) : 'Unavailable',
                        g.classification,
                    ]),
                ),
            );
        rows.push(
            '### Diagnostics',
            ...result.diagnostics.map(
                (d) =>
                    `${d.check}: ${d.passed ? 'pass' : 'fail'} (${d.violations} violations); ${d.domain}.`,
            ),
        );
    }
    rows.push(
        'Displayed rates and volatilities are rounded to six decimal places. Validation uses full precision; exact values, tolerances, input references and configuration are in OreStudio tool evidence.',
        `Source: ${markdownText(r.source ?? 'Agent-derived analysis')}.`,
        ...(r.reproduction?.observations?.some((o) => o.source?.startsWith('synthetic.'))
            ? ['These inputs are labelled synthetic test data, not live market prices.']
            : []),
        ...result.warnings.map(markdownText),
    );
    return rows.filter(Boolean).join('\n\n');
}

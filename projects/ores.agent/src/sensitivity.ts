// SPDX-License-Identifier: GPL-3.0-or-later
import { markdownTable, markdownText } from './markdown.js';
import { createHash } from 'node:crypto';
import { AppError } from './backend.js';
import { decimalSum, decimalParts, decimalText } from './decimal.js';
import { factorPillar, type RiskMeasure } from './sensitivity-selectors.js';

export interface SensitivityMetric {
    id: string;
    tenant_id: string;
    party_id: string;
    report_instance_id: string;
    report_definition_id: string;
    result_id: string;
    line: number;
    as_of: string;
    subject_type: string;
    subject_id: string;
    measure: string;
    value: number;
    source: string;
    family: string;
    currency: string;
    scenario: string;
    factor: string;
    qualifiers: string;
}
interface Request {
    measure: string;
    currency?: string;
    factor?: string;
    factor2?: string;
    scenario?: string;
    riskMeasures?: RiskMeasure[];
    fxPair?: string;
    maturity?: string;
    smile?: 'ATM';
    bucketId?: string;
    offset: number;
    limit: number;
    bucketOffset: number;
    bucketLimit: number;
}
// ORE's sensitivity.csv Delta/Gamma are monetary finite differences at the
// reported target shifts, not derivatives divided by the shifts. Keep every
// risk-factor, scenario, currency and shift dimension in the aggregation key.
export function summarizeSensitivities(
    metrics: SensitivityMetric[],
    expected: Set<string>,
    args: Request,
    identity: {
        tenantId: string;
        partyId: string;
        runId: string;
        definitionId: string;
        pricedAt: string;
    },
) {
    const groups = new Map<
        string,
        { dimensions: Record<string, string>; rows: SensitivityMetric[] }
    >();
    const refused: { metricId: string; reason: string }[] = [];
    const requested = [...new Set(args.riskMeasures ?? [args.measure])];
    const similar = new Set<string>();
    const natural = args.fxPair || args.maturity || args.smile;
    for (const row of metrics) {
        if (
            row.tenant_id !== identity.tenantId ||
            row.party_id !== identity.partyId ||
            row.report_instance_id !== identity.runId ||
            row.report_definition_id !== identity.definitionId ||
            row.subject_type !== 'trade' ||
            !expected.has(row.subject_id) ||
            row.as_of.slice(0, 10) !== identity.pricedAt ||
            row.family !== 'sensitivity_report' ||
            row.source !== 'sensitivity.csv' ||
            !['Delta', 'Gamma'].includes(row.measure)
        )
            throw new AppError(
                'scope_mismatch',
                'Sensitivity identity, date or canonical source disagrees with the run.',
                403,
            );
        const views = requested.filter(
            (view) =>
                view === 'greeks' ||
                (view === 'gamma' ? row.measure === 'Gamma' : row.measure === 'Delta'),
        );
        if (!views.length) continue;
        let q: Record<string, unknown>;
        try {
            q = JSON.parse(row.qualifiers);
            if (!q || Array.isArray(q) || typeof q !== 'object') throw Error();
        } catch {
            throw new AppError(
                'invalid_sensitivity_metadata',
                'Invalid sensitivity qualifiers; no total calculated.',
                422,
            );
        }
        const factor = q.Factor_1,
            factor2 = q.Factor_2 ?? '',
            shiftText = q.ShiftSize_1,
            shift2Text = q.ShiftSize_2 ?? '0';
        const validFactor =
            typeof factor === 'string' &&
            /^[A-Za-z][A-Za-z0-9_]*\/[^\r\n]+$/.test(factor) &&
            factor.length <= 256;
        const pillar = validFactor ? factorPillar(factor) : undefined;
        const rateFactor = ['DiscountCurve', 'YieldCurve', 'IndexCurve'].includes(
            pillar?.type ?? '',
        );
        const volatilityFactor = pillar?.type?.endsWith('Volatility');
        const relevant = views.filter(
            (view) =>
                !pillar ||
                (view === 'vega' ? volatilityFactor : view === 'dv01' ? rateFactor : true),
        );
        if (!relevant.length || (args.scenario !== undefined && args.scenario !== row.scenario))
            continue;
        if (validFactor && args.factor !== undefined && args.factor !== factor) continue;
        if (typeof factor2 === 'string' && args.factor2 !== undefined && args.factor2 !== factor2)
            continue;
        if (pillar && args.fxPair && pillar.pair !== args.fxPair) {
            if (!pillar.type?.startsWith('FX') || pillar.pair) continue;
        }
        if (
            validFactor &&
            pillar &&
            ((args.maturity && pillar.maturity && pillar.maturity !== args.maturity) ||
                (args.smile && pillar.smile && pillar.smile !== args.smile))
        ) {
            similar.add(factor);
            continue;
        }
        const numeric = (v: unknown) =>
            typeof v === 'string' &&
            v.length <= 128 &&
            /^[+-]?\d+(?:\.\d+)?(?:e[+-]?\d+)?$/i.test(v) &&
            Number.isFinite(Number(v));
        if (
            !validFactor ||
            (args.fxPair && !pillar?.pair) ||
            (args.maturity && !pillar?.maturity) ||
            (args.smile && !pillar?.smile) ||
            typeof factor2 !== 'string' ||
            factor2.length > 256 ||
            (factor2 && !/^[A-Za-z][A-Za-z0-9_]*\/[^\r\n]+$/.test(factor2)) ||
            !numeric(shiftText) ||
            Number(shiftText) <= 0 ||
            !numeric(shift2Text) ||
            (factor2 ? Number(shift2Text) <= 0 : Number(shift2Text) !== 0) ||
            q.IsPar !== 'false' ||
            !/^[A-Z]{3}$/.test(row.currency) ||
            q.Currency !== row.currency ||
            row.factor ||
            !Number.isFinite(row.value)
        ) {
            refused.push({
                metricId: row.id,
                reason: 'Unverified raw factor, positive target shift, currency or IsPar metadata.',
            });
            continue;
        }
        // Cross-gamma rows' Delta column is not a standalone factor Delta.
        if (factor2 && row.measure !== 'Gamma') continue;
        if (!factor2 && decimalText(String(shift2Text)) !== '0') {
            refused.push({ metricId: row.id, reason: 'Second shift has no factor.' });
            continue;
        }
        const dimensions = {
            measure: row.measure,
            factor,
            shift: decimalText(String(shiftText)),
            factor2,
            shift2: decimalText(String(shift2Text)),
            currency: row.currency,
            scenario: row.scenario,
            isPar: 'false',
            source: row.source,
            family: row.family,
        };
        for (const view of relevant) {
            if (view === 'dv01' && dimensions.shift !== '0.0001') continue;
            const labelled =
                requested.length > 1 || view === 'vega' || view === 'dv01'
                    ? { ...dimensions, requestedMeasure: view }
                    : dimensions;
            const key = JSON.stringify(labelled);
            const group: { dimensions: Record<string, string>; rows: SensitivityMetric[] } =
                groups.get(key) ?? { dimensions: labelled, rows: [] };
            group.rows.push(row);
            groups.set(key, group);
        }
    }
    // Unknown dimensions may conceal a duplicate or a member of a valid bucket.
    // Withhold all figures rather than silently dropping these rows.
    if (refused.length)
        return {
            status: 'unavailable',
            reason: 'Sensitivity metadata cannot establish compatible units/bump dimensions. No figures calculated.',
            refusedMetricCount: refused.length,
            refusedMetrics: refused.slice(0, 10),
        };
    const factors = [
        ...new Set([...groups.values()].map((group) => group.dimensions.factor!)),
    ].sort();
    if (
        natural &&
        !args.factor &&
        !args.bucketId &&
        (factors.length > 1 || (!factors.length && similar.size))
    ) {
        const options = factors.length ? factors : [...similar].sort();
        return {
            status: 'selection_required',
            field: 'factor',
            reason: factors.length
                ? 'Several stored factors match this request. Choose an exact factor by number or name.'
                : 'No stored factor matches that maturity or smile label. Choose a reported factor to change the selection.',
            candidates: options.slice(0, 20).map((factor) => ({ id: factor, name: factor })),
        };
    }
    if (!groups.size)
        return {
            status: 'unavailable',
            reason: 'No verified stored sensitivity bucket matches the requested measure/factor. Missing or suppressed sensitivities are not zero. Vega needs a volatility-factor Delta; DV01 needs a raw rate-factor Delta at exactly a 0.0001 target shift.',
        };
    const sorted = [...groups]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([key, group]) => ({
            ...group,
            bucketId: createHash('sha256').update(key).digest('hex').slice(0, 24),
        }))
        .filter((group) => !args.bucketId || group.bucketId === args.bucketId);
    if (!sorted.length)
        return {
            status: 'unavailable',
            reason: 'That exact sensitivity bucket is no longer available in this run. Request the bucket list again.',
        };
    const missingMeasures =
        requested.length > 1
            ? requested.filter(
                  (view) =>
                      ![...groups.values()].some(
                          (group) => group.dimensions.requestedMeasure === view,
                      ),
              )
            : [];
    const buckets = sorted
        .slice(args.bucketOffset, args.bucketOffset + args.bucketLimit)
        .map(({ bucketId, dimensions, rows }) => {
            const byTrade = new Map<string, SensitivityMetric[]>();
            for (const row of rows)
                byTrade.set(row.subject_id, [...(byTrade.get(row.subject_id) ?? []), row]);
            const missing = [...expected].filter((id) => !byTrade.has(id)),
                duplicates = [...byTrade].filter(([, r]) => r.length !== 1).map(([id]) => id);
            const complete =
                !missing.length &&
                !duplicates.length &&
                (!args.currency || args.currency === dimensions.currency);
            const unique = [...byTrade.values()].filter((r) => r.length === 1).map((r) => r[0]!);
            const parts = unique.map((row) => ({ row, ...decimalParts(row.value) })),
                scale = Math.max(0, ...parts.map((p) => p.scale));
            const magnitude = (p: (typeof parts)[number]) =>
                (p.coefficient < 0n ? -p.coefficient : p.coefficient) *
                10n ** BigInt(scale - p.scale);
            parts.sort((a, b) =>
                magnitude(a) > magnitude(b)
                    ? -1
                    : magnitude(a) < magnitude(b)
                      ? 1
                      : a.row.subject_id.localeCompare(b.row.subject_id),
            );
            return {
                bucketId,
                ...dimensions,
                status: complete ? 'ok' : 'incomplete',
                unit: 'cash_finite_difference_at_reported_target_shifts',
                normalization: 'none; not divided by shift sizes; engine stencil is not inferred',
                coverage: {
                    expectedTradeCount: expected.size,
                    valuedTradeCount: unique.length,
                    missingTradeCount: missing.length,
                    duplicateTradeCount: duplicates.length,
                    failedTradeCount: null,
                },
                total: complete
                    ? {
                          figure: decimalSum(rows.map((r) => r.value)),
                          currency: dimensions.currency,
                      }
                    : null,
                reason: complete
                    ? null
                    : 'Missing/suppressed, duplicate or differently denominated member figures prevent a full-scope total; missing is not zero.',
                missingTradeIds: missing.slice(0, 10),
                duplicateTradeIds: duplicates.slice(0, 10),
                contributors: parts.slice(args.offset, args.offset + args.limit).map(({ row }) => ({
                    tradeId: row.subject_id,
                    figure: decimalSum([row.value]),
                    currency: row.currency,
                    metricId: row.id,
                    resultId: row.result_id,
                    line: row.line,
                })),
                contributorCount: unique.length,
                offset: args.offset,
                nextOffset:
                    args.offset + args.limit < unique.length ? args.offset + args.limit : null,
            };
        });
    return {
        status:
            buckets.length && !missingMeasures.length && buckets.every((b) => b.status === 'ok')
                ? 'ok'
                : 'incomplete',
        reason: 'Each bucket is independent; never sum across factors, bumps, scenarios or currencies. DV01 is the signed ORE rate Delta for the stated +1bp target bump, not a sign-reversed loss measure. Vega is cash Delta for the stated volatility bump, not per-unit vega.',
        buckets,
        ...(missingMeasures.length ? { missingMeasures } : {}),
        bucketCount: sorted.length,
        bucketOffset: args.bucketOffset,
        nextBucketOffset:
            args.bucketOffset + args.bucketLimit < sorted.length
                ? args.bucketOffset + args.bucketLimit
                : null,
    };
}

export function formatSensitivities(r: any): string {
    const viewNames: Record<string, string> = {
        delta: 'Delta',
        gamma: 'Gamma',
        vega: 'Vega (stored Delta)',
        dv01: 'DV01 (stored Delta)',
    };
    return (
        `${r.warning ? r.warning + '\n' : ''}## Stored sensitivities\n\n${r.scope.type} ${markdownText(r.scope.name)} [${r.scope.id}]\nRun: ${r.run.id}; health: ${r.health}; actual pricing date: ${r.run.priced_at}\nMembership: bookings at ${r.membershipAt}.\n${r.reason}\n${r.missingMeasures?.length ? 'No verified stored bucket for ' + r.missingMeasures.map((view: string) => viewNames[view]).join(', ') + '.\n' : ''}` +
        r.buckets
            .map(
                (b: any, index: number) =>
                    `\n**Bucket ${r.bucketOffset + index + 1} · ${viewNames[b.requestedMeasure] ?? b.measure}**\n${markdownText(b.factor)} (target shift ${b.shift})${b.factor2 ? ` × ${markdownText(b.factor2)} (target shift ${b.shift2})` : ''}; ${b.currency}; scenario ${markdownText(b.scenario || 'base')}; raw (IsPar=false).\n` +
                    `${b.total ? 'Full-scope cash finite difference: ' + b.total.figure + ' ' + b.total.currency : 'No full-scope total. ' + b.reason}\n` +
                    `Coverage: ${b.coverage.valuedTradeCount}/${b.coverage.expectedTradeCount}; missing ${b.coverage.missingTradeCount}; duplicates ${b.coverage.duplicateTradeCount}. Source: ${b.source}.\n` +
                    '\n' +
                    markdownTable(
                        ['Trade ID', 'Value', 'Currency'],
                        b.contributors.map((c: any) => [c.tradeId, c.figure, c.currency]),
                    ) +
                    '\n' +
                    (b.nextOffset !== null
                        ? `\nSay “Next contributors for bucket ${r.bucketOffset + index + 1}” for more trades.`
                        : ''),
            )
            .join('\n\n') +
        (r.nextBucketOffset !== null ? '\nSay “Next buckets” to see more.' : '') +
        '\nStored raw sensitivities only; no repricing, FX conversion, zero fill or cross-bucket total.'
    );
}

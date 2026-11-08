// SPDX-License-Identifier: GPL-3.0-or-later
import { markdownTable } from './markdown.js';
import { createHash } from 'node:crypto';
import { AppError } from './backend.js';
import { decimalSum, decimalParts, decimalText } from './decimal.js';

export interface SensitivityMetric {
    id: string; tenant_id: string; party_id: string; report_instance_id: string;
    report_definition_id: string; result_id: string; line: number; as_of: string;
    subject_type: string; subject_id: string; measure: string; value: number;
    source: string; family: string; currency: string; scenario: string; factor: string; qualifiers: string;
}
interface Request {
    measure: string; currency?: string; factor?: string; factor2?: string; scenario?: string;
    offset: number; limit: number; bucketOffset: number; bucketLimit: number;
}
// ORE's sensitivity.csv Delta/Gamma are monetary finite differences at the
// reported target shifts, not derivatives divided by the shifts. Keep every
// risk-factor, scenario, currency and shift dimension in the aggregation key.
export function summarizeSensitivities(metrics: SensitivityMetric[], expected: Set<string>,
    args: Request, identity: { tenantId: string; partyId: string; runId: string; definitionId: string; pricedAt: string }) {
    const groups = new Map<string, { dimensions: Record<string, string>; rows: SensitivityMetric[] }>();
    const refused: { metricId: string; reason: string }[] = [];
    for (const row of metrics) {
        if (row.tenant_id !== identity.tenantId || row.party_id !== identity.partyId || row.report_instance_id !== identity.runId || row.report_definition_id !== identity.definitionId || row.subject_type !== 'trade' || !expected.has(row.subject_id) || row.as_of.slice(0, 10) !== identity.pricedAt || row.family !== 'sensitivity_report' || row.source !== 'sensitivity.csv' || !['Delta','Gamma'].includes(row.measure)) throw new AppError('scope_mismatch', 'Sensitivity identity, date or canonical source disagrees with the run.', 403);
        if (args.measure === 'delta' && row.measure !== 'Delta' || args.measure === 'gamma' && row.measure !== 'Gamma' || ['vega','dv01'].includes(args.measure) && row.measure !== 'Delta') continue;
        let q: Record<string, unknown>;
        try { q = JSON.parse(row.qualifiers); if (!q || Array.isArray(q) || typeof q !== 'object') throw Error(); }
        catch { throw new AppError('invalid_sensitivity_metadata', 'Invalid sensitivity qualifiers; no total calculated.', 422); }
        const factor = q.Factor_1, factor2 = q.Factor_2 ?? '', shiftText = q.ShiftSize_1, shift2Text = q.ShiftSize_2 ?? '0';
        const numeric = (v: unknown) => typeof v === 'string' && v.length <= 128 && /^[+-]?\d+(?:\.\d+)?(?:e[+-]?\d+)?$/i.test(v) && Number.isFinite(Number(v));
        if (typeof factor !== 'string' || !/^[A-Za-z][A-Za-z0-9_]*\/[^\r\n]+$/.test(factor) || factor.length > 256 || typeof factor2 !== 'string' || factor2.length > 256 || factor2 && !/^[A-Za-z][A-Za-z0-9_]*\/[^\r\n]+$/.test(factor2) || !numeric(shiftText) || Number(shiftText) <= 0 || !numeric(shift2Text) || (factor2 ? Number(shift2Text) <= 0 : Number(shift2Text) !== 0) || q.IsPar !== 'false' || !/^[A-Z]{3}$/.test(row.currency) || q.Currency !== row.currency || row.factor || !Number.isFinite(row.value)) {
            refused.push({ metricId: row.id, reason: 'Unverified raw factor, positive target shift, currency or IsPar metadata.' }); continue;
        }
        // Cross-gamma rows' Delta column is not a standalone factor Delta.
        if (factor2 && row.measure !== 'Gamma') continue;
        if (args.measure === 'vega' && !factor.split('/')[0]!.endsWith('Volatility')) continue;
        if (args.measure === 'dv01' && (!['DiscountCurve','YieldCurve','IndexCurve'].includes(factor.split('/')[0]!) || decimalText(String(shiftText)) !== '0.0001')) continue;
        if (args.factor !== undefined && args.factor !== factor || args.factor2 !== undefined && args.factor2 !== factor2 || args.scenario !== undefined && args.scenario !== row.scenario) continue;
        if (!factor2 && decimalText(String(shift2Text)) !== '0') { refused.push({ metricId: row.id, reason: 'Second shift has no factor.' }); continue; }
        const dimensions = { measure: row.measure, factor, shift: decimalText(String(shiftText)), factor2,
            shift2: decimalText(String(shift2Text)), currency: row.currency, scenario: row.scenario,
            isPar: 'false', source: row.source, family: row.family };
        const key = JSON.stringify(dimensions);
        const group: { dimensions: Record<string, string>; rows: SensitivityMetric[] } = groups.get(key) ?? { dimensions, rows: [] };
        group.rows.push(row); groups.set(key, group);
    }
    // Unknown dimensions may conceal a duplicate or a member of a valid bucket.
    // Withhold all figures rather than silently dropping these rows.
    if (refused.length) return { status: 'unavailable', reason: 'Sensitivity metadata cannot establish compatible units/bump dimensions. No figures calculated.', refusedMetricCount: refused.length, refusedMetrics: refused.slice(0, 10) };
    if (!groups.size) return { status: 'unavailable', reason: 'No verified stored sensitivity bucket matches the requested measure/factor. Missing or suppressed sensitivities are not zero. Vega needs a volatility-factor Delta; DV01 needs a raw rate-factor Delta at exactly a 0.0001 target shift.' };
    const sorted = [...groups].sort(([a], [b]) => a.localeCompare(b));
    const buckets = sorted.slice(args.bucketOffset, args.bucketOffset + args.bucketLimit).map(([key, { dimensions, rows }]) => {
        const byTrade = new Map<string, SensitivityMetric[]>();
        for (const row of rows) byTrade.set(row.subject_id, [...(byTrade.get(row.subject_id) ?? []), row]);
        const missing = [...expected].filter(id => !byTrade.has(id)), duplicates = [...byTrade].filter(([, r]) => r.length !== 1).map(([id]) => id);
        const complete = !missing.length && !duplicates.length && (!args.currency || args.currency === dimensions.currency);
        const unique = [...byTrade.values()].filter(r => r.length === 1).map(r => r[0]!);
        const parts = unique.map(row => ({ row, ...decimalParts(row.value) })), scale = Math.max(0, ...parts.map(p => p.scale));
        const magnitude = (p: typeof parts[number]) => (p.coefficient < 0n ? -p.coefficient : p.coefficient) * 10n ** BigInt(scale - p.scale);
        parts.sort((a,b) => magnitude(a) > magnitude(b) ? -1 : magnitude(a) < magnitude(b) ? 1 : a.row.subject_id.localeCompare(b.row.subject_id));
        return { bucketId: createHash('sha256').update(key).digest('hex').slice(0, 24), ...dimensions,
            status: complete ? 'ok' : 'incomplete', unit: 'cash_finite_difference_at_reported_target_shifts',
            normalization: 'none; not divided by shift sizes; engine stencil is not inferred',
            coverage: { expectedTradeCount: expected.size, valuedTradeCount: unique.length, missingTradeCount: missing.length, duplicateTradeCount: duplicates.length, failedTradeCount: null },
            total: complete ? { figure: decimalSum(rows.map(r => r.value)), currency: dimensions.currency } : null,
            reason: complete ? null : 'Missing/suppressed, duplicate or differently denominated member figures prevent a full-scope total; missing is not zero.',
            missingTradeIds: missing.slice(0, 10), duplicateTradeIds: duplicates.slice(0, 10),
            contributors: parts.slice(args.offset, args.offset + args.limit).map(({row}) => ({tradeId: row.subject_id, figure: decimalSum([row.value]), currency: row.currency, metricId: row.id, resultId: row.result_id, line: row.line})),
            contributorCount: unique.length, offset: args.offset, nextOffset: args.offset + args.limit < unique.length ? args.offset + args.limit : null };
    });
    return { status: buckets.length && buckets.every(b => b.status === 'ok') ? 'ok' : 'incomplete',
        reason: 'Each bucket is independent; never sum across factors, bumps, scenarios or currencies. DV01 is the signed ORE rate Delta for the stated +1bp target bump, not a sign-reversed loss measure. Vega is cash Delta for the stated volatility bump, not per-unit vega.',
        buckets, bucketCount: sorted.length, bucketOffset: args.bucketOffset,
        nextBucketOffset: args.bucketOffset + args.bucketLimit < sorted.length ? args.bucketOffset + args.bucketLimit : null };
}

export function formatSensitivities(r: any): string {
    return `${r.warning ? r.warning + '\n' : ''}## Stored sensitivities\n\n${r.scope.type} ${r.scope.name} [${r.scope.id}]\nRun: ${r.run.id}; health: ${r.health}; actual pricing date: ${r.run.priced_at}\nMembership: bookings at ${r.membershipAt}.\n${r.reason}\n` + r.buckets.map((b: any) =>
        `${b.measure}: ${b.factor} (target shift ${b.shift})${b.factor2 ? ` × ${b.factor2} (target shift ${b.shift2})` : ''}; ${b.currency}; scenario ${b.scenario || 'base'}; raw (IsPar=false).\n` +
        `${b.total ? 'Full-scope cash finite difference: ' + b.total.figure + ' ' + b.total.currency : 'No full-scope total. ' + b.reason}\n` +
        `Coverage: ${b.coverage.valuedTradeCount}/${b.coverage.expectedTradeCount}; missing ${b.coverage.missingTradeCount}; duplicates ${b.coverage.duplicateTradeCount}. Source: ${b.source}.\n` +
        '\n' + markdownTable(['Trade ID', 'Value', 'Currency'], b.contributors.map((c: any) => [c.tradeId, c.figure, c.currency])) + '\n' +
        (b.nextOffset !== null ? `\nNext contributor offset: ${b.nextOffset}` : '')).join('\n\n') +
        (r.nextBucketOffset !== null ? `\nNext bucket offset: ${r.nextBucketOffset}` : '') + '\nStored raw sensitivities only; no repricing, FX conversion, zero fill or cross-bucket total.';
}

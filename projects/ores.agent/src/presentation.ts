// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { formatScopeReporting } from './scope-reporting.js';
import { formatReporting, reportingTools } from './reporting.js';
import { formatDiscovery, discoveryTools } from './discovery.js';
import { marketCandidate } from './market-dialogue.js';
import { formatMarketResult } from './market.js';
import type { PendingChoice } from './conversation.js';
import { analyticsTools, formatAnalytics } from './fx-analytics.js';
import { formatMarketCurve } from './fx-market-curve.js';
import { marketCurveInput } from './fx-market-request.js';
import { formatCurrencies } from './currency-reference.js';

const metadata = z.object({
    tool: z.string(),
    status: z.string().optional(),
    request: z.record(z.string(), z.unknown()).optional(),
    field: z.string().optional(),
});
const candidates = z.array(
    z.object({ id: z.string(), name: z.string(), aliases: z.array(z.string()).optional() }),
);
export interface Presentation {
    answer: string | undefined;
    pending?: PendingChoice;
}

// Domain adapters validate financial output. This boundary types the small
// conversation envelope and selects one formatter, without probing all of them.
export function present(raw: unknown): Presentation {
    const parsed = metadata.safeParse(raw);
    if (!parsed.success) return { answer: undefined };
    const result = parsed.data;
    let answer: string | undefined;
    if (result.tool === 'build_fx_curve_from_market') {
        answer =
            formatMarketCurve(raw) ??
            formatAnalytics({ ...(raw as object), tool: 'build_fx_forward_curve' });
        if (
            result.status === 'selection_required' &&
            result.request &&
            result.field &&
            Object.hasOwn(marketCurveInput.shape, result.field)
        ) {
            return {
                answer,
                pending: {
                    kind: 'market_curve',
                    args: marketCurveInput.parse(result.request),
                    field: result.field as keyof typeof marketCurveInput.shape,
                    candidates: z.object({ candidates: candidates.default([]) }).parse(raw)
                        .candidates,
                },
            };
        }
    } else if (result.tool === 'list_currencies') answer = formatCurrencies(raw);
    else if (Object.hasOwn(analyticsTools, result.tool)) answer = formatAnalytics(raw);
    else if (result.tool === 'summarize_scope_results') answer = formatScopeReporting(raw);
    else if (Object.hasOwn(reportingTools, result.tool)) answer = formatReporting(raw);
    else if (Object.hasOwn(discoveryTools, result.tool)) answer = formatDiscovery(raw);
    else if (['read_fx_spot', 'read_ir_rate'].includes(result.tool))
        answer = formatMarketResult(raw);
    if (!result.request || !raw || typeof raw !== 'object') return { answer };
    if (result.tool === 'summarize_scope_results') {
        const page = z
            .object({
                bucketOffset: z.number().int(),
                nextBucketOffset: z.number().int().nullable(),
                scope: z.object({ id: z.string() }),
                run: z.object({ id: z.string() }),
                buckets: z.array(
                    z.object({ bucketId: z.string(), nextOffset: z.number().int().nullable() }),
                ),
            })
            .safeParse(raw);
        if (page.success) {
            const data = page.data;
            return {
                answer,
                pending: {
                    kind: 'sensitivity_page',
                    args: { ...result.request, scope: data.scope.id, runId: data.run.id },
                    nextBucketOffset: data.nextBucketOffset,
                    buckets: data.buckets.map((bucket, index) => ({
                        number: data.bucketOffset + index + 1,
                        id: bucket.bucketId,
                        nextOffset: bucket.nextOffset,
                    })),
                },
            };
        }
    }
    if (
        result.tool === 'summarize_scope_results' &&
        ['ok', 'stale'].includes(result.status ?? '')
    ) {
        const page = z
            .object({
                nextOffset: z.number().int().nullable().optional(),
                scope: z.object({ id: z.string() }),
                run: z.object({ id: z.string() }),
            })
            .safeParse(raw);
        if (page.success && page.data.nextOffset != null)
            return {
                answer,
                pending: {
                    kind: 'contributors',
                    args: {
                        ...result.request,
                        scope: page.data.scope.id,
                        runId: page.data.run.id,
                        offset: page.data.nextOffset,
                    },
                },
            };
    }
    if (
        (result.tool === 'build_fx_forward_curve' ||
            result.tool === 'build_fx_volatility_surface') &&
        ['selection_required', 'invalid_input'].includes(result.status ?? '')
    )
        return { answer, pending: { kind: 'analytics', name: result.tool, args: result.request } };
    if (
        (result.tool === 'read_fx_spot' || result.tool === 'read_ir_rate') &&
        result.status === 'ambiguous'
    ) {
        const choices = z.object({ candidates: z.array(marketCandidate) }).safeParse(raw)
            .data?.candidates;
        if (!choices?.length) return { answer };
        return {
            answer,
            pending: {
                kind: 'market_point',
                field: 'series',
                name: result.tool,
                args: result.request,
                candidates: choices,
            },
        };
    }
    const field = result.field;
    if (
        (result.status === 'ambiguous' || result.status === 'selection_required') &&
        (field === 'scope' ||
            field === 'runId' ||
            field === 'book' ||
            field === 'portfolio' ||
            field === 'currency' ||
            field === 'factor')
    ) {
        const choices = z.object({ candidates: candidates.default([]) }).parse(raw).candidates;
        const args = { ...result.request };
        if (field === 'factor') {
            delete args.fxPair;
            delete args.maturity;
            delete args.smile;
            args.bucketOffset = 0;
            args.offset = 0;
            const selected = z
                .object({ scope: z.object({ id: z.string() }), run: z.object({ id: z.string() }) })
                .safeParse(raw);
            if (selected.success) {
                args.scope = selected.data.scope.id;
                args.runId = selected.data.run.id;
            }
        }
        return {
            answer,
            pending: {
                kind: 'tool',
                name: result.tool,
                args,
                field,
                candidates: choices,
            },
        };
    }
    return { answer };
}

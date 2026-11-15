// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { marketId } from './market.js';

export const marketCurveInput = z
    .object({
        provider: z.string().trim().min(1).max(100).optional(),
        spotSeriesId: marketId
            .optional()
            .describe('Exact selected spot series UUID. Omit for discovery.'),
        forwardSeriesId: marketId
            .optional()
            .describe('Exact selected forward series UUID. Omit for discovery.'),
        usdCurve: z.string().trim().min(1).max(200).optional(),
        asOf: z.iso.date().optional().describe('UTC valuation date chosen by the user.'),
        spotDate: z.iso
            .date()
            .optional()
            .describe('Spot settlement date, separate from the valuation date.'),
        forwardQuote: z.enum(['outright', 'points']).optional(),
        pointSize: z
            .number()
            .finite()
            .positive()
            .max(1)
            .optional()
            .describe(
                'Rate change per quoted point. Supply for points when known; omit for outrights.',
            ),
        calendar: z
            .enum(['unadjusted', 'weekends_following', 'weekends_modified_following'])
            .optional(),
        endOfMonth: z.boolean().optional(),
        usdPillarAnchor: z.enum(['valuation', 'spot']).optional(),
    })
    .strict();
export type MarketCurveRequest = z.infer<typeof marketCurveInput>;
export type MarketCurveField = keyof MarketCurveRequest;

export function isTestDatasetConventionReply(message: string): boolean {
    return /^(?:please\s+)?(?:use|apply)\s+(?:the\s+)?test\s+data[\s-]*set\s+conventions(?:\s+please)?[.!]?$/i.test(
        message.trim(),
    );
}

// This is an explicit opt-in for the installed, labelled evaluation dataset only.
// Never apply its conventions to a production provider or overwrite a user choice.
export function testDatasetConventions(
    request: MarketCurveRequest,
): MarketCurveRequest | undefined {
    const conventions: MarketCurveRequest = {
        spotDate: '2016-12-30',
        forwardQuote: 'outright',
        calendar: 'unadjusted',
        endOfMonth: false,
        usdPillarAnchor: 'valuation',
    };
    if (
        request.provider?.toLowerCase() !== 'agent-e2e' ||
        request.usdCurve?.toLowerCase() !== 'usd-agent-e2e' ||
        request.asOf !== '2016-12-30' ||
        Object.entries(conventions).some(
            ([key, value]) =>
                request[key as MarketCurveField] !== undefined &&
                request[key as MarketCurveField] !== value,
        )
    )
        return;
    return conventions;
}

// Extract only explicitly labelled selectors; no model proposes market values.
export function marketCurveSelectors(text: string): MarketCurveRequest {
    const result: MarketCurveRequest = {};
    const spotSeries = /\bspot\s+series\s+([0-9a-f-]{36})\b/i.exec(text)?.[1];
    if (spotSeries) result.spotSeriesId = spotSeries;
    const forwardSeries = /\bforward\s+series\s+([0-9a-f-]{36})\b/i.exec(text)?.[1];
    if (forwardSeries) result.forwardSeriesId = forwardSeries;
    for (const [label, pattern] of [
        ['provider', /\b(?:from\s+)?provider\s+(?:"([^"]+)"|'([^']+)'|([\w.-]+))/gi],
        ['USD curve', /\bUSD\s+(?:discount\s+)?curve\s+(?:"([^"]+)"|'([^']+)'|([\w.-]+))/gi],
        ['valuation date', /\b(?:as\s+of|valuation\s+date)\s*[:=]?\s*(\d{4}-\d{2}-\d{2})\b/gi],
        ['spot date', /\bspot\s+(?:settlement\s+)?date\s*[:=]?\s*(\d{4}-\d{2}-\d{2})\b/gi],
        ['point size', /\b(?:point|pip)\s+size\s*[:=]?\s*(\d+(?:\.\d+)?(?:e[+-]?\d+)?)/gi],
    ] as const) {
        const values = [...text.matchAll(pattern)].map((m) =>
            (m[1] ?? m[2] ?? m[3])!.toLowerCase(),
        );
        if (new Set(values).size > 1)
            throw new Error(`Please select one ${label}; conflicting selectors were supplied.`);
    }
    if (
        /\bquotes?\s+(?:are\s+|in\s+)?outrights?\b/i.test(text) &&
        /\bquotes?\s+(?:are\s+|in\s+)?points\b/i.test(text)
    )
        throw new Error('Please select one forward quote unit: outrights or points.');
    if (
        /\b(?:no|without)\s+end[- ]of[- ]month\b/i.test(text) &&
        /\b(?:use|apply|with)\s+end[- ]of[- ]month\b/i.test(text)
    )
        throw new Error('Please select one end-of-month rolling policy.');
    if (/\bunadjusted\b/i.test(text) && /\bfollowing\b/i.test(text))
        throw new Error('Please select one calendar adjustment convention.');
    const provider = /\b(?:from\s+)?provider\s+(?:"([^"]+)"|'([^']+)'|([\w.-]+))/i.exec(text);
    if (provider)
        result.provider = (provider[1] ?? provider[2] ?? provider[3])!.replace(/[.!]+$/, '');
    const curve = /\bUSD\s+(?:discount\s+)?curve\s+(?:"([^"]+)"|'([^']+)'|([\w.-]+))/i.exec(text);
    if (curve) result.usdCurve = (curve[1] ?? curve[2] ?? curve[3])!.replace(/[.!]+$/, '');
    const asOf = /\b(?:as\s+of|valuation\s+date)\s*[:=]?\s*(\d{4}-\d{2}-\d{2})\b/i.exec(text)?.[1];
    if (asOf) result.asOf = asOf;
    const spot = /\bspot\s+(?:settlement\s+)?date\s*[:=]?\s*(\d{4}-\d{2}-\d{2})\b/i.exec(text)?.[1];
    if (spot) result.spotDate = spot;
    if (
        /\b(?:forward\s+)?quotes?\s+(?:are\s+|in\s+)?outrights?\b/i.test(text) ||
        /^outrights?[.!]?$/i.test(text.trim())
    )
        result.forwardQuote = 'outright';
    else if (
        /\b(?:forward\s+)?quotes?\s+(?:are\s+|in\s+)?points\b/i.test(text) ||
        /^points[.!]?$/i.test(text.trim())
    )
        result.forwardQuote = 'points';
    const size = /\b(?:point|pip)\s+size\s*[:=]?\s*(\d+(?:\.\d+)?(?:e[+-]?\d+)?)/i.exec(text)?.[1];
    if (size) result.pointSize = Number(size);
    if (/\bunadjusted\b/i.test(text)) result.calendar = 'unadjusted';
    else if (/\bmodified\s+following\b/i.test(text) && /\bweekends?\s+only\b/i.test(text))
        result.calendar = 'weekends_modified_following';
    else if (/\bfollowing\b/i.test(text) && /\bweekends?\s+only\b/i.test(text))
        result.calendar = 'weekends_following';
    if (/\b(?:no|without)\s+end[- ]of[- ]month\b/i.test(text)) result.endOfMonth = false;
    else if (/\b(?:use|apply|with)\s+end[- ]of[- ]month\b/i.test(text)) result.endOfMonth = true;
    if (/\bUSD\s+(?:pillar|tenor)s?[^.;]*\bvaluation(?:[- ]date)?\b/i.test(text))
        result.usdPillarAnchor = 'valuation';
    else if (/\bUSD\s+(?:pillar|tenor)s?[^.;]*\bspot(?:[- ]date)?\b/i.test(text))
        result.usdPillarAnchor = 'spot';
    return result;
}

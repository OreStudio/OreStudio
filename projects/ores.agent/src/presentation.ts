// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { formatScopeReporting } from './scope-reporting.js';
import { formatReporting, reportingTools } from './reporting.js';
import { formatDiscovery, discoveryTools } from './discovery.js';
import { formatMarketResult } from './market.js';
import type { PendingChoice } from './conversation.js';

const metadata = z.object({ tool: z.string(), status: z.string().optional(),
    request: z.record(z.string(), z.unknown()).optional(), field: z.string().optional() });
const candidates = z.array(z.object({ id: z.string(), name: z.string() }));
const providers = z.array(z.object({ provider: z.string(), seriesId: z.string() }));
export interface Presentation { answer: string | undefined; pending?: PendingChoice }

// Domain adapters validate financial output. This boundary types the small
// conversation envelope and selects one formatter, without probing all of them.
export function present(raw: unknown): Presentation {
    const parsed = metadata.safeParse(raw);
    if (!parsed.success) return { answer: undefined };
    const result = parsed.data;
    let answer: string | undefined;
    if (result.tool === 'summarize_scope_results') answer = formatScopeReporting(raw);
    else if (Object.hasOwn(reportingTools, result.tool)) answer = formatReporting(raw);
    else if (Object.hasOwn(discoveryTools, result.tool)) answer = formatDiscovery(raw);
    else if (['read_fx_spot', 'read_ir_rate'].includes(result.tool)) answer = formatMarketResult(raw);
    if (!result.request || !raw || typeof raw !== 'object') return { answer };
    if (['read_fx_spot', 'read_ir_rate'].includes(result.tool) && result.status === 'ambiguous') {
        const choices = z.object({ candidates: providers }).parse(raw).candidates;
        return { answer, pending: { kind: 'provider', name: result.tool, args: result.request, candidates: choices } };
    }
    const field = result.field;
    if ((result.status === 'ambiguous' || result.status === 'selection_required') &&
        (field === 'scope' || field === 'runId' || field === 'book' || field === 'portfolio')) {
        const choices = z.object({ candidates: candidates.default([]) }).parse(raw).candidates;
        return { answer, pending: { kind: 'tool', name: result.tool, args: result.request, field, candidates: choices } };
    }
    return { answer };
}

// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError } from './backend.js';

export interface ReadIntent {
    route: 'scope' | 'reporting' | 'market' | 'discovery';
    refusal?: string;
    clarification?: string;
    date?: string;
    factor?: string;
    factor2?: string;
    scenario?: string;
}
const months = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'];

// This only recognises explicit request syntax. Unknown phrasing remains a
// model proposal; identity, financial semantics and facts stay in the executor.
export function readIntent(message: string): ReadIntent {
    const business = /\b(trade|trades|book|books|portfolio|portfolios|counterparty|counterparties)\b/i.test(message);
    const scope = /\b(book|portfolio)\b/i.test(message) && /\b(npv|valuation|summary|summarize|contributors?|total|greeks?|delta|gamma|vega|dv01|(?:stored|base[- ]currency)\s+value)\b/i.test(message);
    const reporting = /\b(npv|valuations?|reports?|runs?|results|greeks?)\b/i.test(message);
    const market = !business && /\b(currency|currencies|spot|rates?|swap|market|series|observations?|[A-Z]{3}\s*\/\s*[A-Z]{3}|EURUSD|USDJPY|GBPUSD)\b/i.test(message);
    const intent: ReadIntent = { route: scope ? 'scope' : reporting ? 'reporting' : market ? 'market' : 'discovery' };
    // A request to execute must not be interpreted as a similarly named read.
    const action = /(?:^|[.;?!]\s*|\band\s+)(?:please\s+|(?:can|could|would)\s+you\s+)?(?:run|execute|trigger|submit|create|book|place|buy|sell|cancel|delete|amend)\b/i.test(message);
    if (action && /\b(reports?|trades?|orders?|forwards?|swaps?|bonds?|options?|EURUSD|[A-Z]{3}\/[A-Z]{3})\b/i.test(message)) {
        intent.refusal = 'This is a read-only assistant. Report execution, booking and other changes are not available. Nothing was submitted or changed.';
        return intent;
    }
    const dates = [...message.matchAll(/\b\d{4}-\d{2}-\d{2}\b/g)].map(m => m[0]);
    for (const m of message.matchAll(/\b(?:on|for|as of)\s+(\d{1,2})\s+([A-Za-z]+)\s+(\d{4})\b/gi)) {
        const month = months.indexOf(m[2]!.toLowerCase());
        if (month >= 0) dates.push(`${m[3]}-${String(month + 1).padStart(2, '0')}-${m[1]!.padStart(2, '0')}`);
    }
    if (dates.some(d => !z.iso.date().safeParse(d).success) || new Set(dates).size > 1) {
        intent.clarification = 'Please specify one valid valuation/observation date as YYYY-MM-DD. No date range or comparison was selected.';
    } else intent.date = dates[0];
    // Preserve labelled bucket selectors; do not interpret an unlabelled tenor
    // or currency inside a factor as an independent filter.
    for (const key of ['factor2', 'factor', 'scenario'] as const) {
        const pattern = new RegExp(`\\b${key}\\s*(?:[:=]\\s*|\\s+)(?:"([^"]+)"|'([^']+)'|([^\\s,;?!]+))`, 'gi');
        const values = [...message.matchAll(pattern)].map(m => m[1] ?? m[2] ?? m[3]!);
        if (new Set(values.map(v => v.toLowerCase())).size > 1) intent.clarification = `Please specify one ${key} selector for this request.`;
        else if (values[0]) intent[key] = values[0];
    }
    return intent;
}

export function preserveReadSelectors(name: string, raw: unknown, intent: ReadIntent): Record<string, any> {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) throw new AppError('invalid_arguments', 'Tool arguments must be an object.', 400);
    const args = { ...raw } as Record<string, any>;
    const requireLiteral = (key: string, value: string | undefined) => {
        if (value === undefined) return;
        if (args[key] !== undefined && args[key] !== value) throw new AppError('selection_not_requested', 'A proposed selector differs from the explicit user request.', 422);
        args[key] = value;
    };
    if (name === 'summarize_scope_results' || name === 'read_trade_results') requireLiteral('valuationDate', intent.date);
    if (name === 'read_fx_spot' || name === 'read_ir_rate') requireLiteral('asOf', intent.date);
    if (name === 'summarize_scope_results') for (const key of ['factor', 'factor2', 'scenario'] as const) requireLiteral(key, intent[key]);
    return args;
}

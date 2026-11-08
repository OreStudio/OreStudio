// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError } from './backend.js';

export type Measure = 'npv' | 'npv_base' | 'greeks' | 'delta' | 'gamma' | 'vega' | 'dv01';
export type ToolArguments = Record<string, unknown>;
export interface RequestPlan {
    message: string;
    route: 'scope' | 'reporting' | 'market' | 'discovery';
    refusal?: string;
    clarification?: string;
    date?: string;
    factor?: string;
    factor2?: string;
    scenario?: string;
    tradeId?: string;
    runId?: string;
    scopeType?: 'book' | 'portfolio';
    scope?: string;
    measure?: Measure;
    provider?: string;
    currency?: string;
    allowPartial: boolean;
}
export type ReadIntent = RequestPlan;
const uuid = '[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}';
const months = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'];

// This only recognises explicit request syntax. Unknown phrasing remains a
// model proposal; identity, financial semantics and facts stay in the executor.
export function readIntent(message: string): RequestPlan {
    const business = /\b(trade|trades|book|books|portfolio|portfolios|counterparty|counterparties)\b/i.test(message);
    const scope = /\b(book|portfolio)\b/i.test(message) && /\b(npv|npv_base|valuation|summary|summarize|contributors?|total|greeks?|delta|gamma|vega|dv01|(?:stored|base[- ]currency)\s+value)\b/i.test(message);
    const reporting = /\b(npv|npv_base|valuations?|reports?|runs?|results|greeks?)\b/i.test(message);
    const market = !business && /\b(currency|currencies|spot|rates?|swap|market|series|observations?|[A-Z]{3}\s*\/\s*[A-Z]{3}|EURUSD|USDJPY|GBPUSD)\b/i.test(message);
    const intent: RequestPlan = { message, route: scope ? 'scope' : reporting ? 'reporting' : market ? 'market' : 'discovery', allowPartial: false };
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
    for (const [key, label] of [['tradeId', 'trade'], ['runId', '(?:run|report)']] as const) {
        const values = [...message.matchAll(new RegExp(`\\b${label}\\s+(?:UUID\\s+)?(${uuid})\\b`, 'gi'))].map(m => m[1]!.toLowerCase());
        if (new Set(values).size > 1) intent.clarification = `Please select one ${key} for this request.`;
        else intent[key] = values[0];
    }
    const namedScope = /\b(book|portfolio)\s+(?:"([^"]+)"|'([^']+)'|(.+?)(?=\s+(?:from|in)\s+(?:run|report)\b|\s+on\s+\d{4}-\d{2}-\d{2}|[?.!]|$))/i.exec(message);
    if (namedScope) {
        intent.scopeType = namedScope[1]!.toLowerCase() as 'book' | 'portfolio';
        intent.scope = (namedScope[2] ?? namedScope[3] ?? namedScope[4])!.trim();
    }
    const measureText = message.replace(/"[^"]*"|'[^']*'/g, '');
    const greek = /\b(greeks?|delta|gamma|vega|dv01)\b/i.exec(measureText)?.[1]?.toLowerCase();
    if (greek) intent.measure = greek === 'greek' ? 'greeks' : greek as Measure;
    else if (scope || /\b(npv|npv_base|valuation)\b/i.test(measureText)) {
        intent.measure = /\b(?:base(?:[- ]currency)?|npv_base)\b/i.test(measureText) ? 'npv_base'
            : /\bnative(?:[- ]currency)?\b/i.test(measureText) ? 'npv' : scope ? 'npv_base' : 'npv';
    }
    const negatesPartial = /\b(?:do\s+not|don't|never|without|exclude|avoid|no)\b[^.;?!]{0,100}\b(?:partial|errored|errors)\b/i.test(message);
    intent.allowPartial = !negatesPartial && /\b(?:show|read|inspect|include|allow)\s+(?:the\s+)?(?:partial|errored)\b/i.test(message);
    const namedCurrency = /\b(?:[Ii]n|[Cc]urrency\s*[:=]?)\s+([A-Z]{3})\b/.exec(message)?.[1];
    if (namedCurrency && !['NPV', 'THE', 'FOR', 'AND', 'RAW'].includes(namedCurrency)) intent.currency = namedCurrency;
    if (market) {
        const providers = [...message.matchAll(/\b(?:from|using)\s+(?:the\s+)?(?:provider\s+)?["']?([a-z0-9][a-z0-9_.-]*)|\bprovider\s*(?:[:=]\s*|\s+)["']?([a-z0-9][a-z0-9_.-]*)/gi)]
            .map(m => (m[1] ?? m[2])!.replace(/[.!]+$/, '').toLowerCase())
            .filter(v => !['orestudio', 'the', 'my', 'a', 'an', 'today', 'yesterday', 'now'].includes(v) && !/^\d{4}-\d{2}-\d{2}$/.test(v));
        if (new Set(providers).size > 1) intent.clarification = 'Please select one provider for this request.';
        else intent.provider = providers[0];
    }
    return intent;
}

export function preserveReadSelectors(name: string, raw: unknown, intent: RequestPlan): ToolArguments {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) throw new AppError('invalid_arguments', 'Tool arguments must be an object.', 400);
    const args = { ...raw } as ToolArguments;
    const requireLiteral = (key: string, value: string | undefined) => {
        if (value === undefined) return;
        const proposed = args[key];
        if (proposed !== undefined && proposed !== value &&
            !(typeof proposed === 'string' && ['tradeId', 'runId', 'scope', 'scopeType', 'provider'].includes(key) && proposed.toLowerCase() === value.toLowerCase())) {
            throw new AppError('selection_not_requested', 'A proposed selector differs from the explicit user request.', 422);
        }
        args[key] = value;
    };
    if (name === 'summarize_scope_results' || name === 'read_trade_results') requireLiteral('valuationDate', intent.date);
    if (name === 'read_fx_spot' || name === 'read_ir_rate') requireLiteral('asOf', intent.date);
    if (name === 'summarize_scope_results') for (const key of ['factor', 'factor2', 'scenario'] as const) requireLiteral(key, intent[key]);
    if (['read_trade', 'read_trade_results', 'find_report_runs'].includes(name)) requireLiteral('tradeId', intent.tradeId);
    if (['read_run_status', 'read_trade_results', 'summarize_scope_results'].includes(name)) requireLiteral('runId', intent.runId);
    if (name === 'summarize_scope_results') {
        requireLiteral('scope', intent.scope);
        requireLiteral('scopeType', intent.scopeType);
        for (const key of ['factor', 'factor2', 'scenario']) if (args[key] === '') delete args[key];
        if (intent.measure) args.measure = intent.measure;
    }
    if (name === 'read_trade_results') {
        if (args.allowPartial && !intent.allowPartial) throw new AppError('partial_not_requested', 'Explicit partial inspection is required.', 422);
        args.allowPartial = intent.allowPartial;
        if (intent.measure) args.measure = intent.measure;
    }
    if (['read_fx_spot', 'read_ir_rate'].includes(name)) requireLiteral('provider', intent.provider);
    if (['read_trade_results', 'summarize_scope_results'].includes(name)) {
        if (intent.currency) requireLiteral('currency', intent.currency);
        else delete args.currency;
    }
    // A model may fill missing syntax, but cannot invent exact identities or
    // silently narrow a request using a selector absent from the user's text.
    for (const key of ['tradeId', 'runId', 'provider', 'seriesId', 'factor', 'factor2', 'scenario']) {
        const value = args[key];
        const selected = key === 'tradeId' ? intent.tradeId : key === 'runId' ? intent.runId : undefined;
        if (typeof value === 'string' && value && value !== selected && !intent.message.toLowerCase().includes(value.toLowerCase())) {
            throw new AppError('selection_not_requested', 'Use literal user selectors or an explicit clarification choice.', 422);
        }
    }
    return args;
}

export function scopeArguments(plan: RequestPlan): ToolArguments | undefined {
    if (plan.route !== 'scope' || !plan.scope || !plan.scopeType || !plan.runId) return;
    return preserveReadSelectors('summarize_scope_results', {}, plan);
}

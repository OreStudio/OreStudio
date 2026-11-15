// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError } from './backend.js';
import {
    marketCurveSelectors,
    marketCurveInput,
    isTestDatasetConventionReply,
} from './fx-market-request.js';
import { riskMeasure, riskSelectors, type RiskMeasure } from './sensitivity-selectors.js';
import { directedPair, marketTerms, type MarketTerms } from './market-request.js';

export type Measure = 'npv' | 'npv_base' | 'greeks' | 'delta' | 'gamma' | 'vega' | 'dv01';
export type ToolArguments = Record<string, unknown>;
export interface RequestPlan {
    message: string;
    route: 'scope' | 'reporting' | 'market' | 'discovery' | 'analytics';
    analyticsTool?:
        'build_fx_forward_curve' | 'build_fx_volatility_surface' | 'build_fx_curve_from_market';
    analyticsInput?: ToolArguments;
    refusal?: string;
    clarification?: string;
    date?: string;
    factor?: string;
    factor2?: string;
    scenario?: string;
    riskMeasures?: RiskMeasure[];
    fxPair?: string;
    maturity?: string;
    smile?: 'ATM';
    contributorLimit?: number;
    tradeId?: string;
    runId?: string;
    scopeType?: 'book' | 'portfolio';
    scope?: string;
    measure?: Measure;
    provider?: string;
    irPoint?: MarketTerms;
    spotPair?: ReturnType<typeof directedPair>;
    marketSeriesId?: string;
    currency?: string;
    allowPartial: boolean;
    runStatus?: 'completed' | 'clean';
    latest?: true;
}
export type ReadIntent = RequestPlan;
const uuid = '[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}';
const months = [
    'january',
    'february',
    'march',
    'april',
    'may',
    'june',
    'july',
    'august',
    'september',
    'october',
    'november',
    'december',
];

// This only recognises explicit request syntax. Unknown phrasing remains a
// model proposal; identity, financial semantics and facts stay in the executor.
export function readIntent(message: string): RequestPlan {
    const head = message.split('{', 1)[0]!;
    const fx = /\b(?:FX|EUR\s*\/?\s*USD|foreign exchange)\b/i.test(head);
    const surface =
        (fx && /\b(?:volatility|vol)\s+surface\b/i.test(head)) ||
        /build_fx_volatility_surface/.test(head);
    const curve =
        (fx && /\bcurve\b/i.test(head) && /\b(?:forwards?|discount|consistent)\b/i.test(head)) ||
        /build_fx_forward_curve/.test(head);
    if (surface || curve) {
        const plan: RequestPlan = {
            message,
            route: 'analytics',
            allowPartial: false,
            analyticsTool: surface ? 'build_fx_volatility_surface' : 'build_fx_forward_curve',
            analyticsInput: {},
        };
        // Supplied financial inputs bypass model proposals and generic date parsing:
        // a curve necessarily contains several valid dates, not a date selector.
        const start = message.indexOf('{');
        const pairs = [...head.matchAll(/\b([A-Z]{3})\s*\/\s*([A-Z]{3})\b/gi)].map((m) =>
            `${m[1]}/${m[2]}`.toUpperCase(),
        );
        if (pairs.some((pair) => pair !== 'EUR/USD')) {
            plan.clarification =
                'Only EUR/USD curve and surface construction is supported. No different currency pair was substituted.';
            return plan;
        }
        if (start < 0 && !surface && !/build_fx_forward_curve/.test(head)) {
            plan.analyticsTool = 'build_fx_curve_from_market';
            try {
                const selection = marketCurveInput.safeParse(marketCurveSelectors(message));
                if (selection.success) plan.analyticsInput = selection.data;
                else
                    plan.clarification =
                        'Please provide valid market selectors and dates as YYYY-MM-DD. No data selection was inferred.';
            } catch (error) {
                plan.clarification = (error as Error).message;
            }
        }
        if (start >= 0) {
            try {
                const text = message.slice(start).replace(/\s*```\s*$/, '');
                const parsed: unknown = JSON.parse(text);
                if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) throw Error();
                plan.analyticsInput = parsed as ToolArguments;
            } catch {
                plan.clarification =
                    'Supply one valid JSON input object for the curve or surface. No inputs were inferred.';
            }
        }
        if (
            /\b(?:book|submit|publish|execute|buy|sell)\b/i.test(
                message.slice(0, start < 0 ? message.length : start),
            )
        )
            plan.refusal =
                'This is read-only curve/surface analysis. Publishing, booking and execution are not available.';
        return plan;
    }
    // A clarification pasted after sign-in, restart or pending-choice expiry has
    // no original selectors. Keep explicit conventions and ask for the missing
    // market selection instead of sending an orphaned follow-up to inference.
    if (isTestDatasetConventionReply(message))
        return {
            message,
            route: 'analytics',
            allowPartial: false,
            clarification:
                'There is no active curve request to apply the test conventions to. Please resend your EUR/USD request with the provider, USD curve name and valuation date. When the inputs are found, reply “use test dataset conventions”.',
        };
    if (
        /^\s*(?:spot\s+(?:settlement\s+)?date\b|(?:forward\s+)?quotes?\s+(?:are|in)\s+(?:outrights?|points)\b|(?:no|use)\s+end[- ]of[- ]month\b|USD\s+tenors?\s+use\b)/i.test(
            message,
        )
    ) {
        const plan: RequestPlan = {
            message,
            route: 'analytics',
            allowPartial: false,
            analyticsTool: 'build_fx_curve_from_market',
        };
        try {
            const parsed = marketCurveInput.safeParse(marketCurveSelectors(message));
            if (parsed.success) plan.analyticsInput = parsed.data;
            else
                plan.clarification =
                    'Please check the supplied convention values and use dates in YYYY-MM-DD format. Resend the original EUR/USD request with its provider, USD curve name and valuation date.';
        } catch (error) {
            plan.clarification = (error as Error).message;
        }
        return plan;
    }
    const business =
        /\b(trade|trades|book|books|portfolio|portfolios|counterparty|counterparties)\b/i.test(
            message,
        );
    const scope =
        /\b(book|portfolio)\b/i.test(message) &&
        /\b(npv|npv_base|valuation|summary|summarize|contributors?|total|greeks?|sensitivit(?:y|ies)|delta|gamma|vega|dv01|(?:stored|base[- ]currency)\s+value)\b/i.test(
            message,
        );
    const reporting = /\b(npv|npv_base|valuations?|reports?|runs?|results|greeks?)\b/i.test(
        message,
    );
    const market =
        !business &&
        /\b(currency|currencies|spot|rates?|swap|ois|yields?|discount factors?|deposit|market|series|observations?|[A-Z]{3}\s*\/\s*[A-Z]{3}|EURUSD|USDJPY|GBPUSD)\b/i.test(
            message,
        );
    const intent: RequestPlan = {
        message,
        route: scope ? 'scope' : reporting ? 'reporting' : market ? 'market' : 'discovery',
        allowPartial: false,
    };
    if (
        reporting &&
        /\b(?:clean|successful|complete)\s+(?:report\s+)?runs?\b|\b(?:cleanly|successfully)\s+completed\b/i.test(
            message,
        )
    )
        intent.runStatus = 'clean';
    else if (reporting && /\b(?:completed|finished)\b/i.test(message))
        intent.runStatus = 'completed';
    // A request to execute must not be interpreted as a similarly named read.
    const action =
        /(?:^|[.;?!]\s*|\band\s+)(?:please\s+|(?:can|could|would)\s+you\s+)?(?:run|execute|trigger|submit|create|book|place|buy|sell|cancel|delete|amend)\b/i.test(
            message,
        );
    if (
        action &&
        /\b(reports?|trades?|orders?|forwards?|swaps?|bonds?|options?|EURUSD|[A-Z]{3}\/[A-Z]{3})\b/i.test(
            message,
        )
    ) {
        intent.refusal =
            'This is a read-only assistant. Report execution, booking and other changes are not available. Nothing was submitted or changed.';
        return intent;
    }
    const dates = [...message.matchAll(/\b\d{4}-\d{2}-\d{2}\b/g)].map((m) => m[0]);
    for (const m of message.matchAll(
        /\b(?:on|for|as of)\s+(\d{1,2})\s+([A-Za-z]+)\s+(\d{4})\b/gi,
    )) {
        const month = months.indexOf(m[2]!.toLowerCase());
        if (month >= 0)
            dates.push(`${m[3]}-${String(month + 1).padStart(2, '0')}-${m[1]!.padStart(2, '0')}`);
    }
    if (dates.some((d) => !z.iso.date().safeParse(d).success) || new Set(dates).size > 1) {
        intent.clarification =
            'Please specify one valid valuation/observation date as YYYY-MM-DD. No date range or comparison was selected.';
    } else intent.date = dates[0];
    // Preserve labelled bucket selectors; do not interpret an unlabelled tenor
    // or currency inside a factor as an independent filter.
    for (const key of ['factor2', 'factor', 'scenario'] as const) {
        const pattern = new RegExp(
            `\\b${key}\\s*(?:[:=]\\s*|\\s+)(?:"([^"]+)"|'([^']+)'|([^\\s,;?!]+))`,
            'gi',
        );
        const values = [...message.matchAll(pattern)].map(
            (m) => m[1] ?? m[2] ?? m[3]!.replace(/\.+$/, ''),
        );
        if (new Set(values.map((v) => v.toLowerCase())).size > 1)
            intent.clarification = `Please specify one ${key} selector for this request.`;
        else if (values[0]) intent[key] = values[0];
    }
    for (const [key, label] of [
        ['tradeId', 'trade'],
        ['runId', '(?:run|report)'],
    ] as const) {
        const values = [
            ...message.matchAll(new RegExp(`\\b${label}\\s+(?:UUID\\s+)?(${uuid})\\b`, 'gi')),
        ].map((m) => m[1]!.toLowerCase());
        if (new Set(values).size > 1)
            intent.clarification = `Please select one ${key} for this request.`;
        else intent[key] = values[0];
    }
    const scopeText = message.replace(/[“”]/g, '"');
    const quotedScope = /(?:"([^"]+)"|'([^']+)')\s+(book|portfolio)\b/i.exec(scopeText);
    const namedScope =
        /\b(book|portfolio)\s+(?:"([^"]+)"|'([^']+)'|(.+?)(?=\s+(?:from|in|using)\s+(?:run|report)\b|\s+(?:on|as of|at|for)\s+\d{4}-\d{2}-\d{2}|\s+for\s+(?:factor|scenario|[A-Z]{3}\s*\/)\b|\s+(?:and|with|including)\s+(?:(?:its|the|top)\s+)*(?:contributors?|breakdown)\b|[?.!]|$))/i.exec(
            scopeText,
        );
    if (quotedScope && scope) {
        intent.scopeType = quotedScope[3]!.toLowerCase() as 'book' | 'portfolio';
        intent.scope = (quotedScope[1] ?? quotedScope[2])!.trim();
    } else if (namedScope) {
        intent.scopeType = namedScope[1]!.toLowerCase() as 'book' | 'portfolio';
        intent.scope = (namedScope[2] ?? namedScope[3] ?? namedScope[4])!.trim();
        if (
            /^(?:(?:and|with|including)\s+(?:(?:its|the|top)\s+)*(?:contributors?|breakdown)\b|(?:from|in|using)\s+(?:run|report)\b)/i.test(
                intent.scope,
            )
        )
            delete intent.scope;
    } else if (scope) {
        const kind = /\b(book|portfolio)\b/i.exec(message)?.[1]?.toLowerCase();
        if (kind === 'book' || kind === 'portfolio') intent.scopeType = kind;
    }
    const measureText = scopeText.replace(/"[^"]*"|'[^']*'/g, '');
    const riskText = intent.scope ? measureText.replace(intent.scope, ' ') : measureText;
    const measureQuery = riskText.replace(/\b\d{1,3}[- ]delta\b/gi, '');
    const greeks = [
        ...new Set(
            [...measureQuery.matchAll(/\b(delta|gamma|vega|dv01)\b/gi)].map((match) =>
                riskMeasure.parse(match[1]!.toLowerCase()),
            ),
        ),
    ];
    const generalGreeks = /\b(?:greeks?|sensitivit(?:y|ies))\b/i.test(riskText);
    if (greeks.length || generalGreeks) {
        intent.measure = greeks.length === 1 && !generalGreeks ? greeks[0]! : 'greeks';
        if (greeks.length > 1) intent.riskMeasures = greeks;
        if (
            /\bpar\s+(?:greeks?|delta|gamma|vega|dv01|sensitivities)\b/i.test(riskText) &&
            !/\bnon[- ]par\s/i.test(riskText)
        )
            intent.clarification =
                'This query reads stored raw sensitivities (IsPar=false). Par sensitivities require a separate verified output contract.';
        if (intent.factor && /^[a-z]{3}\s*\/\s*[a-z]{3}$/i.test(intent.factor))
            delete intent.factor;
        if (!intent.factor && !intent.factor2) {
            const selectors = riskSelectors(riskText);
            if (selectors.clarification) intent.clarification = selectors.clarification;
            else Object.assign(intent, selectors);
            if (greeks.includes('vega') && /\b\d{1,3}[- ]delta\b/i.test(riskText))
                intent.clarification =
                    'Use the exact stored factor for a delta-labelled smile point. The agent does not convert delta labels to ORE smile coordinates.';
        }
        if (
            /\b(?:normali[sz]ed|per[- ]unit)\s+(?:greeks?|delta|gamma|vega|dv01)\b|\b(?:greeks?|delta|gamma|vega|dv01)\s+per[- ]unit\b/i.test(
                riskText,
            )
        )
            intent.clarification =
                'The verified output contains cash finite differences at the reported target shifts. Per-unit sensitivities are unavailable.';
    } else if (scope || /\b(npv|npv_base|valuation)\b/i.test(measureText)) {
        intent.measure = /\b(?:base(?:[- ]currency)?|npv_base)\b/i.test(measureText)
            ? 'npv_base'
            : /\bnative(?:[- ]currency)?\b/i.test(measureText)
              ? 'npv'
              : scope
                ? 'npv_base'
                : 'npv';
    }
    if (
        scope &&
        !intent.runId &&
        ['npv', 'npv_base'].includes(intent.measure ?? '') &&
        /\b(?:latest|most recent)\b/i.test(measureText)
    )
        intent.latest = true;
    const negatesPartial =
        /\b(?:do\s+not|don't|never|without|exclude|avoid|no)\b[^.;?!]{0,100}\b(?:partial|errored|errors)\b/i.test(
            message,
        );
    intent.allowPartial =
        !negatesPartial &&
        /\b(?:show|read|inspect|include|allow)\s+(?:the\s+)?(?:partial|errored)\b/i.test(message);
    const namedCurrency = /\b(?:[Ii]n|[Cc]urrency\s*[:=]?)\s+([A-Z]{3})\b(?!\s*\/)/.exec(
        message,
    )?.[1];
    if (namedCurrency && !['NPV', 'THE', 'FOR', 'AND', 'RAW'].includes(namedCurrency))
        intent.currency = namedCurrency;
    if (intent.route === 'market') {
        const parsed = marketTerms(message);
        if (parsed.conflicts.includes('provider'))
            intent.clarification = 'Please select one provider for this request.';
        else intent.provider = parsed.args.provider;
        if (parsed.conflicts.includes('seriesId'))
            intent.clarification = 'Please select one series ID for this request.';
        else intent.marketSeriesId = parsed.args.seriesId;
        intent.spotPair = directedPair(message);
        if (
            !intent.spotPair &&
            /\b(?:rates?|yields?|swaps?|ois|discount factors?|deposit|zero curve)\b/i.test(
                message,
            ) &&
            !/^(?:please\s+)?(?:list|find|discover)\b|\b(?:available|market series)\b/i.test(
                message,
            ) &&
            (parsed.args.currency ||
                parsed.args.instrument ||
                parsed.conflicts.includes('currency') ||
                parsed.conflicts.includes('instrument'))
        ) {
            if (parsed.conflicts.some((field) => field !== 'term'))
                intent.clarification =
                    'Please give one value for each rate selector. Specify the instrument, curve, index or currency you want.';
            else intent.irPoint = parsed.args;
        }
    }
    if (scope) {
        const numbers = 'one two three four five six seven eight nine ten'.split(' ');
        const limit =
            /\b(?:show|list|include|display|give|with)\s+(?:(?:me|its|the)\s+)*top\s+(\d+|one|two|three|four|five|six|seven|eight|nine|ten)\s+(?:trade\s+)?contributors?\b/i.exec(
                riskText,
            )?.[1] ??
            (/\b(?:share|concentration|proportion|percentage)\b/i.test(riskText)
                ? undefined
                : /\btop\s+(\d+|one|two|three|four|five|six|seven|eight|nine|ten)\s+(?:trade\s+)?contributors?\b/i.exec(
                      riskText,
                  )?.[1]);
        if (limit) {
            const value = /^\d+$/.test(limit)
                ? Number(limit)
                : numbers.indexOf(limit.toLowerCase()) + 1;
            if (value >= 1 && value <= 25) intent.contributorLimit = value;
            else intent.clarification = 'Choose 1 to 25 contributors per page.';
        }
    }
    return intent;
}

export function preserveReadSelectors(
    name: string,
    raw: unknown,
    intent: RequestPlan,
    references?: { has(key: string, value: string): boolean },
): ToolArguments {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw))
        throw new AppError('invalid_arguments', 'Tool arguments must be an object.', 400);
    const args = { ...raw } as ToolArguments;
    const requireLiteral = (key: string, value: string | undefined) => {
        if (value === undefined) return;
        const proposed = args[key];
        if (
            proposed !== undefined &&
            proposed !== value &&
            !(
                typeof proposed === 'string' &&
                ['tradeId', 'runId', 'scope', 'scopeType', 'provider'].includes(key) &&
                proposed.toLowerCase() === value.toLowerCase()
            )
        ) {
            throw new AppError(
                'selection_not_requested',
                'A proposed selector differs from the explicit user request.',
                422,
            );
        }
        args[key] = value;
    };
    if (name === 'summarize_scope_results' || name === 'read_trade_results')
        requireLiteral('valuationDate', intent.date);
    if (name === 'read_fx_spot' || name === 'read_ir_rate') requireLiteral('asOf', intent.date);
    if (name === 'find_report_runs') {
        requireLiteral('status', intent.runStatus);
        requireLiteral('valuationDate', intent.date);
        if (!intent.date) delete args.valuationDate;
        delete args.orderBy;
    }
    if (name === 'summarize_scope_results')
        for (const key of ['factor', 'factor2', 'scenario'] as const)
            requireLiteral(key, intent[key]);
    if (['read_trade', 'read_trade_results', 'find_report_runs'].includes(name))
        requireLiteral('tradeId', intent.tradeId);
    if (['read_run_status', 'read_trade_results', 'summarize_scope_results'].includes(name))
        requireLiteral('runId', intent.runId);
    if (name === 'summarize_scope_results') {
        requireLiteral('scope', intent.scope);
        requireLiteral('scopeType', intent.scopeType);
        for (const key of ['factor', 'factor2', 'scenario']) if (args[key] === '') delete args[key];
        if (intent.measure) args.measure = intent.measure;
        for (const key of ['riskMeasures', 'fxPair', 'maturity', 'smile', 'bucketId'])
            delete args[key];
        for (const key of ['riskMeasures', 'fxPair', 'maturity', 'smile'] as const)
            if (intent[key] !== undefined) args[key] = intent[key];
        if (intent.contributorLimit) args.limit = intent.contributorLimit;
        if (intent.latest) args.latest = true;
        else delete args.latest;
    }
    if (name === 'read_trade_results') {
        if (args.allowPartial && !intent.allowPartial)
            throw new AppError(
                'partial_not_requested',
                'Explicit partial inspection is required.',
                422,
            );
        args.allowPartial = intent.allowPartial;
        if (intent.measure) args.measure = intent.measure;
    }
    if (['read_fx_spot', 'read_ir_rate'].includes(name)) {
        requireLiteral('provider', intent.provider);
        requireLiteral('seriesId', intent.marketSeriesId);
    }
    if (name === 'read_fx_spot' && intent.spotPair) {
        requireLiteral('baseCurrency', intent.spotPair.baseCurrency);
        requireLiteral('quoteCurrency', intent.spotPair.quoteCurrency);
    }
    if (name === 'read_ir_rate' && intent.irPoint)
        for (const [field, value] of Object.entries(intent.irPoint)) requireLiteral(field, value);
    if (['read_trade_results', 'summarize_scope_results'].includes(name)) {
        if (intent.currency) requireLiteral('currency', intent.currency);
        else delete args.currency;
    }
    // A model may fill missing syntax, but cannot invent exact identities or
    // silently narrow a request using a selector absent from the user's text.
    for (const key of [
        'tradeId',
        'runId',
        'provider',
        'seriesId',
        'factor',
        'factor2',
        'scenario',
    ]) {
        const value = args[key];
        const selected =
            key === 'tradeId'
                ? intent.tradeId
                : key === 'runId'
                  ? intent.runId
                  : key === 'seriesId'
                    ? intent.marketSeriesId
                    : undefined;
        if (
            typeof value === 'string' &&
            value &&
            value !== selected &&
            !intent.message.toLowerCase().includes(value.toLowerCase()) &&
            !references?.has(key, value)
        ) {
            throw new AppError(
                'selection_not_requested',
                'Use literal user selectors or an explicit clarification choice.',
                422,
            );
        }
    }
    return args;
}

export function scopeArguments(plan: RequestPlan): ToolArguments | undefined {
    if (plan.route !== 'scope' || !plan.scope || !plan.scopeType || (!plan.runId && !plan.latest))
        return;
    return preserveReadSelectors('summarize_scope_results', {}, plan);
}

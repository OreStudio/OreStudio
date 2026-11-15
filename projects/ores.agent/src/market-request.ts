// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { marketId } from './market.js';

const numbers = [
    'one',
    'two',
    'three',
    'four',
    'five',
    'six',
    'seven',
    'eight',
    'nine',
    'ten',
    'eleven',
    'twelve',
];
const tenorText =
    '(?:overnight|(?:[0-9]{1,4}|' +
    numbers.join('|') +
    ')(?:\\s*-\\s*|\\s*)(?:days?|weeks?|months?|years?|[dwmy]))';
const literal = '"([^"]+)"|([A-Za-z0-9][A-Za-z0-9_.-]{0,99})';
const nonProviders = new Set([
    'orestudio',
    'the',
    'my',
    'a',
    'an',
    'today',
    'yesterday',
    'now',
    'curve',
    'index',
    'series',
    'spot',
    'forward',
    'provider',
    'on',
    'as',
    'for',
    'with',
    'at',
    'in',
    'and',
    'to',
    'rate',
    'floating',
    'maturity',
]);
const currencies = new Set([
    'USD',
    'EUR',
    'GBP',
    'JPY',
    'CHF',
    'CAD',
    'AUD',
    'NZD',
    'SEK',
    'NOK',
    'DKK',
    'ZAR',
    'MXN',
    'BRL',
    'HKD',
    'SGD',
    'CNY',
    'CNH',
    'INR',
    'KRW',
    'PLN',
    'CZK',
    'HUF',
    'TRY',
]);
const irKinds = [
    ['discount', /\bdiscount\s+factors?\b/i],
    ['zero', /\bzero[- ](?:coupon|rate|yield)\b/i],
    ['mm', /\b(?:deposit|money[- ]market|cash deposit)\b/i],
    ['ir_swap', /\b(?:swap|swaps|ois)\b/i],
] as const;
export type MarketField =
    | 'currency'
    | 'instrument'
    | 'term'
    | 'floatingTenor'
    | 'forwardStart'
    | 'curveId'
    | 'indexName'
    | 'provider'
    | 'seriesId';
export type MarketTerms = Partial<Record<MarketField, string>>;

export function periodValue(text: string, zero = false): string | undefined {
    const value = text.trim();
    if (/^overnight$/i.test(value)) return '1D';
    if (zero && /^(?:spot|spot[- ]starting|immediate)$/i.test(value)) return '0D';
    const match = new RegExp(
        '^(\\d{1,4}|' +
            numbers.join('|') +
            ')(?:\\s*-\\s*|\\s*)(days?|weeks?|months?|years?|[dwmy])$',
        'i',
    ).exec(value);
    if (!match) return;
    const n = /^\d/.test(match[1]!)
        ? Number(match[1])
        : numbers.indexOf(match[1]!.toLowerCase()) + 1;
    if (n === 0) return zero ? '0D' : undefined;
    return n + match[2]![0]!.toUpperCase();
}

export function instrumentValue(text: string) {
    if (['discount', 'zero', 'mm', 'ir_swap'].includes(text.toLowerCase()))
        return text.toLowerCase();
    const hits = irKinds.filter(([, pattern]) => pattern.test(text));
    return hits.length === 1 ? hits[0]![0] : undefined;
}

function currencyValue(text: string) {
    if (/^[A-Za-z]{3}$/.test(text)) return text.toUpperCase();
    if (/^(?:sterling|pounds?(?: sterling)?|british pounds?)$/i.test(text)) return 'GBP';
    if (/^(?:u\.?s\.? dollars?|us dollars?)$/i.test(text)) return 'USD';
    if (/^euros?$/i.test(text)) return 'EUR';
    if (/^(?:japanese )?yen$/i.test(text)) return 'JPY';
}

export function marketFieldValue(field: MarketField, text: string) {
    const value = text.trim();
    switch (field) {
        case 'term':
        case 'floatingTenor':
            return periodValue(value);
        case 'forwardStart':
            return periodValue(value, true);
        case 'currency':
            return currencyValue(value);
        case 'instrument':
            return instrumentValue(value);
        case 'seriesId':
            return marketId.safeParse(value).success ? value.toLowerCase() : undefined;
        case 'provider':
            return value.length <= 100 && /^[\w.-]+$/.test(value) ? value.toLowerCase() : undefined;
        default:
            return value.length <= 100 && /^[\w .-]+$/.test(value) ? value : undefined;
    }
}

export function marketTerms(message: string) {
    const values: Partial<Record<MarketField, Set<string>>> = {};
    const add = (field: MarketField, text: string) => {
        const value = marketFieldValue(field, text);
        if (value) (values[field] ??= new Set()).add(value);
    };
    let remaining = message;
    const consume = (field: MarketField, pattern: RegExp) => {
        remaining = remaining.replace(
            pattern,
            (_whole, a: string | undefined, b: string | undefined) => {
                if (
                    (field === 'curveId' || field === 'indexName') &&
                    !a &&
                    /^(?:on|as|for|from|using|rate|factor|is|of)$/i.test(b ?? '')
                )
                    return _whole;
                add(field, a ?? b!.replace(/\.+$/, ''));
                return ' ';
            },
        );
    };
    consume('provider', new RegExp('\\bprovider\\s*(?:[:=]\\s*|\\s+)(?:' + literal + ')', 'gi'));
    consume('curveId', /\b(?:using|on|from)\s+(?:the\s+)?([A-Z][A-Z\d_.-]*)\s+curve\b/g);
    consume(
        'curveId',
        new RegExp('\\bcurve(?:[- ]id)?\\s*(?:[:=]\\s*|\\s+)(?:' + literal + ')', 'gi'),
    );
    consume(
        'indexName',
        new RegExp('\\bindex(?:[- ]name)?\\s*(?:[:=]\\s*|\\s+)(?:' + literal + ')', 'gi'),
    );
    consume('seriesId', /\bseries(?:\s+id)?\s*[:=]?\s*([a-f\d-]{36})\b/gi);
    consume(
        'floatingTenor',
        new RegExp(
            '\\b(?:floating(?:[- ]leg)?(?:[- ]tenor)?|float(?:[- ]tenor)?)\\s*[:=]?\\s*(?:of\\s+)?(' +
                tenorText +
                ')\\b',
            'gi',
        ),
    );
    consume(
        'floatingTenor',
        new RegExp(
            '\\b(' + tenorText + ')\\s+(?:floating(?:[- ]leg)?|float)(?:[- ]tenor)?\\b',
            'gi',
        ),
    );
    consume(
        'forwardStart',
        new RegExp(
            '\\b(?:forward[- ]start(?:ing)?|start(?:s|ing)?\\s+in)\\s*[:=]?\\s*(' +
                tenorText +
                '|spot)\\b',
            'gi',
        ),
    );
    consume(
        'forwardStart',
        new RegExp('\\b(' + tenorText + ')\\s+forward[- ]start(?:ing)?\\b', 'gi'),
    );
    consume('forwardStart', /\b(spot[- ]starting)\b/gi);
    remaining = remaining.replace(
        new RegExp('\\b(?:from|using)\\s+(?:the\\s+)?(?:' + literal + ')', 'gi'),
        (whole, a, b) => {
            const candidate = (a ?? b.replace(/\.+$/, '')) as string;
            if (
                !nonProviders.has(candidate.toLowerCase()) &&
                !periodValue(candidate, true) &&
                !z.iso.date().safeParse(candidate).success
            ) {
                add('provider', candidate);
                return ' ';
            }
            return whole;
        },
    );
    const kindHits = irKinds.filter(([, pattern]) => pattern.test(remaining));
    // "zero curve" and "discount curve" can denote a named curve; an explicit discount
    // factor or zero rate determines the requested point type.
    for (const [kind] of kindHits) add('instrument', kind);
    if (!kindHits.length && /\bzero curve\b/i.test(message)) add('instrument', 'zero');
    if (kindHits.some(([kind]) => kind === 'ir_swap' || kind === 'mm') && !values.indexName)
        for (const token of remaining.matchAll(
            /\b(?:[A-Z]{3}-)?(?:SONIA|SOFR|EURIBOR|ESTR|TONA|SARON)(?:-\d+[DWMY])?\b/gi,
        ))
            add('indexName', token[0].toUpperCase());
    for (const token of remaining.matchAll(/\b[A-Z]{3}\b/g))
        if (currencies.has(token[0])) add('currency', token[0]);
    for (const name of remaining.matchAll(
        /\b(?:sterling|british pounds?|u\.?s\.? dollars?|euros?|(?:japanese )?yen)\b/gi,
    ))
        add('currency', name[0]);
    const terms = remaining.replace(/\b[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)+\b/g, ' ');
    for (const match of terms.matchAll(new RegExp('\\b(' + tenorText + ')\\b', 'gi')))
        add('term', match[1]!);
    const args: MarketTerms = {};
    const conflicts: MarketField[] = [];
    for (const field of Object.keys(values) as MarketField[]) {
        const entries = [...values[field]!];
        if (entries.length === 1) args[field] = entries[0];
        if (entries.length > 1) conflicts.push(field);
    }
    return { args, conflicts };
}

export function directedPair(message: string) {
    const pairs = [...message.matchAll(/\b([A-Z]{3})\s*\/\s*([A-Z]{3})\b/gi)].map((match) => [
        match[1]!.toUpperCase(),
        match[2]!.toUpperCase(),
    ]);
    for (const match of message.matchAll(/\b[A-Z]{6}\b/gi)) {
        const [base, quote] = [match[0].slice(0, 3).toUpperCase(), match[0].slice(3).toUpperCase()];
        if (currencies.has(base) && currencies.has(quote)) pairs.push([base, quote]);
    }
    const unique = [...new Set(pairs.map((pair) => pair.join('/')))];
    if (unique.length === 1) {
        const [baseCurrency, quoteCurrency] = unique[0]!.split('/');
        return { baseCurrency: baseCurrency!, quoteCurrency: quoteCurrency! };
    }
}

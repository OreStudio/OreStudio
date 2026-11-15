// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { decimalSum, decimalTotal } from './decimal.js';
import { marketId } from './market.js';
import { markdownTable, markdownText } from './markdown.js';
import type { ToolArguments } from './intent.js';

const uuid = '[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}';
const runPattern = new RegExp(
    '\\b(?:run|report)(?:\\s+(?:run|id))*\\s*[:=]?\\s*(' + uuid + ')\\b',
    'gi',
);
const idPattern = new RegExp('\\b' + uuid + '\\b', 'gi');
const maxTrades = 20;
const tradeFields = (message: string) => [
    ...new Set((message.match(/\b(?:bought|sold)\b/gi) ?? []).map((field) => field.toLowerCase())),
];
const exactTradeIds = (message: string) => [
    ...new Set([...message.matchAll(idPattern)].map((match) => match[0].toLowerCase())),
];
export interface TradeTotalRequest {
    tradeIds: string[];
    field?: 'bought' | 'sold';
    runId?: string;
    measure: 'npv' | 'npv_base';
    valuationDate?: string;
}

export function tradeTotalRequest(message: string) {
    const fields = tradeFields(message);
    if (
        !/\b(?:sum|total|add up|combined|aggregate)\b/i.test(message) ||
        (!fields.length && !/\b(?:value|values|valuations?|npvs?|worth)\b/i.test(message)) ||
        !/\btrades?\b/i.test(message)
    )
        return;
    const runIds = new Set<string>();
    const tradeText = message.replace(runPattern, (_, id: string) => {
        runIds.add(id.toLowerCase());
        return ' ';
    });
    const tradeIds = exactTradeIds(tradeText);
    if (!tradeIds.length) return;
    if (tradeIds.length > maxTrades)
        return { error: `Choose at most ${maxTrades} trade IDs per total.` };
    if (fields.length > 1) return { error: 'Choose one trade field to sum: Bought or Sold.' };
    const field = fields[0];
    if (field === 'bought' || field === 'sold')
        return { request: { tradeIds, field, measure: 'npv_base' } satisfies TradeTotalRequest };
    if (runIds.size > 1) return { error: 'Choose one report run for the total.' };
    const dates = [...new Set(message.match(/\b\d{4}-\d{2}-\d{2}\b/g))];
    if (dates.length > 1 || (dates.length === 1 && !z.iso.date().safeParse(dates[0]).success))
        return { error: 'Give one valid pricing date for the total.' };
    const request: TradeTotalRequest = {
        tradeIds,
        runId: [...runIds][0],
        valuationDate: dates[0],
        measure: /\bnative(?:[- ]currency)?\b/i.test(message) ? 'npv' : 'npv_base',
    };
    return { request };
}

const tradeDetail = z.object({
    tool: z.literal('read_trade'),
    status: z.literal('ok'),
    trade: z.object({ id: marketId }),
    economicsStatus: z.literal('available'),
    economics: z.record(z.string(), z.unknown()),
});
const tradeAmount = z.string().regex(/^\+?(?:\d+(?:\.\d*)?|\.\d+)$/);

export function tradeAnalysisInputs(message: string) {
    const tradeIds = exactTradeIds(message.replace(runPattern, ' '));
    const fields = tradeFields(message).filter(
        (field): field is 'bought' | 'sold' => field === 'bought' || field === 'sold',
    );
    if (!tradeIds.length || !fields.length) return;
    if (tradeIds.length > maxTrades)
        return { error: `Choose at most ${maxTrades} trade IDs per calculation.` };
    return { tradeIds, fields };
}

export function tradeTableRequest(message: string) {
    if (
        !/\btrades?\b/i.test(message) ||
        !/^(?:(?:please|can you|could you|would you)\s+)*(?:(?:create|make|build)\s+(?:(?:me|a|the|an|markdown)\s+)*table\b|(?:give|show|provide|format|put|present|list)\b[^.!?]*\btable\b|tabulate\b)/i.test(
            message,
        )
    )
        return;
    // A table with a calculation still belongs to the aggregation workflow.
    if (tradeTotalRequest(message)) return;
    const ids = exactTradeIds(message.replace(runPattern, ' '));
    if (!ids.length) return;
    if (ids.length > maxTrades)
        return { error: `Choose at most ${maxTrades} trade IDs per table.` };
    return { ids, fields: tradeFields(message) };
}

const tableTrade = z.object({
    tool: z.literal('read_trade'),
    status: z.literal('ok'),
    trade: z.object({
        id: marketId,
        trade_type: z.string(),
        origin_reference: z.string().optional(),
        version: z.number().optional(),
    }),
    book: z.object({ name: z.string() }).nullish(),
    counterparty: z.object({ full_name: z.string(), short_code: z.string() }).nullish(),
    booking: z.object({ trade_date: z.string().nullish() }).nullish(),
    economicsStatus: z.string(),
    economics: z.record(z.string(), z.unknown()).nullish(),
});
const currencyCode = z.string().regex(/^[A-Z]{3}$/);
function readTradeLeg(raw: unknown, tradeId: string, field: 'bought' | 'sold') {
    const detail = tradeDetail.safeParse(raw).data;
    if (detail?.trade.id.toLowerCase() !== tradeId) return;
    const amount = tradeAmount.safeParse(detail.economics[field + '_amount']).data;
    const currency = currencyCode.safeParse(detail.economics[field + '_currency']).data;
    if (amount && currency) return { amount, currency };
}

export async function readTradeAnalysisRows(
    tradeIds: string[],
    fields: ('bought' | 'sold')[],
    read: (args: ToolArguments) => Promise<{ result: unknown }>,
) {
    const rows = [];
    const missing = [];
    for (const tradeId of tradeIds) {
        const raw = (await read({ tradeId })).result;
        const legs: Partial<Record<'bought' | 'sold', { amount: string; currency: string }>> = {};
        for (const field of fields) {
            const leg = readTradeLeg(raw, tradeId, field);
            if (leg) legs[field] = leg;
            else missing.push(`${tradeId} (${field})`);
        }
        rows.push({ tradeId, ...legs });
    }
    return { rows, missing };
}

function leg(raw: Record<string, unknown> | null | undefined, field: string) {
    const amount = tradeAmount.safeParse(raw?.[field + '_amount']).data;
    const currency = currencyCode.safeParse(raw?.[field + '_currency']).data;
    return amount && currency ? amount + ' ' + currency : 'Unavailable';
}

export async function renderTradeTable(
    ids: string[],
    fields: string[],
    read: (args: ToolArguments) => Promise<{ result: unknown }>,
) {
    const compact = fields.length > 0;
    const headers = compact
        ? ['Trade ID', ...fields.map((field) => (field === 'bought' ? 'Bought' : 'Sold'))]
        : [
              'Trade ID',
              'Type',
              'Book',
              'Counterparty',
              'Trade date',
              'Bought',
              'Sold',
              'Value date',
              'Settlement',
              'Reference',
              'Version',
          ];
    const rows: unknown[][] = [];
    for (const tradeId of ids) {
        const row = tableTrade.safeParse((await read({ tradeId })).result).data;
        if (!row || row.trade.id.toLowerCase() !== tradeId) {
            rows.push([tradeId, 'Unavailable', ...Array(headers.length - 2).fill('—')]);
            continue;
        }
        const economics = row.economicsStatus === 'available' ? row.economics : undefined;
        if (compact) {
            rows.push([tradeId, ...fields.map((field) => leg(economics, field))]);
            continue;
        }
        rows.push([
            tradeId,
            row.trade.trade_type,
            row.book?.name ?? '—',
            row.counterparty
                ? row.counterparty.full_name + ' (' + row.counterparty.short_code + ')'
                : '—',
            row.booking?.trade_date ?? '—',
            leg(economics, 'bought'),
            leg(economics, 'sold'),
            economics?.value_date ?? '—',
            economics?.settlement ?? '—',
            row.trade.origin_reference || '—',
            row.trade.version ?? '—',
        ]);
    }
    return markdownTable(headers, rows);
}

export async function sumTradeAmounts(
    tradeIds: string[],
    field: 'bought' | 'sold',
    read: (args: ToolArguments) => Promise<{ result: unknown }>,
) {
    const amounts = new Map<string, string[]>();
    const rows: string[][] = [];
    const missing: string[][] = [];
    const label = field === 'bought' ? 'Bought' : 'Sold';
    for (const tradeId of tradeIds) {
        const raw = (await read({ tradeId })).result;
        const leg = readTradeLeg(raw, tradeId, field);
        if (!leg) {
            const type = z
                .object({ trade: z.object({ id: marketId, trade_type: z.string() }) })
                .safeParse(raw).data?.trade;
            missing.push([
                tradeId,
                type?.id.toLowerCase() === tradeId ? type.trade_type : 'Unverified',
            ]);
            continue;
        }
        const { amount, currency } = leg;
        const normalized = amount.replace(/^\+/, '').replace(/^\./, '0.').replace(/\.$/, '');
        const group = amounts.get(currency) ?? [];
        group.push(normalized);
        amounts.set(currency, group);
        rows.push([tradeId, normalized, currency]);
    }
    const totals = [...amounts]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([currency, group]) => ({
            currency,
            value: decimalTotal(group),
            count: group.length,
        }));
    return {
        evidence: {
            tool: 'process_read_analysis',
            status: missing.length ? 'incomplete' : 'ok',
            operation: 'sum_trade_amounts',
            field,
            tradeIds,
            totals,
            missingTradeIds: missing.map(([id]) => id),
        },
        answer: [
            ...(missing.length
                ? [
                      `A complete ${label} total is unavailable. ${missing.length} of ${tradeIds.length} trades have no verified ${label} field.`,
                      markdownTable(
                          ['Trade with no verified ' + label + ' field', 'Type'],
                          missing,
                      ),
                  ]
                : []),
            ...(totals.length
                ? [
                      [
                          `${label} ${missing.length ? 'subtotals for verified trades' : 'totals for ' + rows.length + ' trades'}, grouped by currency:`,
                          ...totals.map(
                              (total) =>
                                  `- **${total.currency} ${total.value}** (${total.count} trades)`,
                          ),
                      ].join('\n'),
                      markdownTable(['Trade ID', label + ' amount', 'Currency'], rows),
                  ]
                : []),
        ].join('\n\n'),
    };
}

const verified = z.object({
    status: z.enum(['ok', 'stale']),
    health: z.literal('completed clean'),
    run: z.object({ id: marketId, priced_at: z.string().min(1) }),
    result: z.object({
        subject_id: marketId,
        measure: z.enum(['npv', 'npv_base']),
        figure: z.number().finite(),
        currency: z.string().regex(/^[A-Z]{3}$/),
        source: z.string().min(1),
    }),
});

export async function sumStoredTradeValues(
    request: TradeTotalRequest,
    read: (args: ToolArguments) => Promise<{ result: unknown }>,
) {
    let runId = request.runId;
    let pricedAt: string | undefined;
    const rows: z.infer<typeof verified>[] = [];
    const missing: string[] = [];
    for (const tradeId of request.tradeIds) {
        const readback = await read({
            tradeId,
            measure: request.measure,
            ...(runId ? { runId } : {}),
            ...(request.valuationDate ? { valuationDate: request.valuationDate } : {}),
        });
        const row = verified.safeParse(readback.result).data;
        if (
            !row ||
            row.result.subject_id.toLowerCase() !== tradeId ||
            row.result.measure !== request.measure ||
            (runId && row.run.id.toLowerCase() !== runId) ||
            (pricedAt && row.run.priced_at !== pricedAt)
        ) {
            if (!runId)
                return {
                    selectionRequired: 'runId',
                    answer: 'Which completed report run should I use? Reply with its run UUID. I’ll use that same run for all the listed trades.',
                };
            missing.push(tradeId);
            continue;
        }
        runId = row.run.id.toLowerCase();
        pricedAt = row.run.priced_at;
        rows.push(row);
    }
    if (missing.length)
        return {
            selectionRequired: 'runId',
            answer:
                `Run ${runId} has no verified matching value for ${missing.length} of the requested trades. No total was calculated. Reply with another completed run UUID to retry all the trades.\n\n` +
                missing.map((id) => '- ' + id).join('\n'),
        };
    const currency = rows[0]?.result.currency;
    if (!currency || rows.some((row) => row.result.currency !== currency))
        return {
            selectionRequired: 'runId',
            answer: 'The stored values use different currencies. No total was calculated. Reply with a run UUID that has comparable values.',
        };
    const figures = rows.map((row) => decimalSum([row.result.figure]));
    const value = decimalTotal(figures);
    const stale = rows.some((row) => row.status === 'stale');
    return {
        evidence: {
            tool: 'process_read_analysis',
            status: 'ok',
            operation: 'sum_trade_values',
            tradeIds: request.tradeIds,
            value,
            currency,
            runId,
            pricedAt,
            measure: request.measure,
        },
        answer: [
            `Total ${request.measure === 'npv_base' ? 'base-currency NPV' : 'native-currency NPV'} for ${rows.length} trades: **${value} ${currency}**.`,
            `Run: ${runId}. Pricing date: ${markdownText(pricedAt)}.`,
            ...(stale ? ['These are stored historical valuations.'] : []),
            markdownTable(
                ['Trade ID', 'NPV (' + currency + ')', 'Source'],
                rows.map((row, i) => [row.result.subject_id, figures[i], row.result.source]),
            ),
        ].join('\n\n'),
    };
}

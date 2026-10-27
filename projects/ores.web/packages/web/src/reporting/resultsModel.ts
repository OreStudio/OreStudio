/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { z } from 'zod';
import type { AggregateRow, Money, RunResultRow } from '../api/client.js';

/**
 * The shapes a run's results arrive in, parsed once at the boundary.
 *
 * The server states a figure as text, in the currency the engine wrote, so a
 * number is parsed only where a number is needed and a screen never has to guess
 * what an untyped row held.
 */

const moneySchema = z.object({
    currency: z.string().default(''),
    total: z.string().default(''),
});

const tradeSchema = z.object({
    trade_id: z.string(),
    trade_type: z.string().default(''),
    npv: z.string().default(''),
    npv_currency: z.string().default(''),
    npv_base: z.string().default(''),
    notional: z.string().default(''),
    notional_currency: z.string().default(''),
    counterparty: z.string().default(''),
    counterparty_name: z.string().default(''),
    book_id: z.string().default(''),
    book_name: z.string().default(''),
});

const aggregateSchema = z.object({
    key: z.string(),
    name: z.string().default(''),
    trades: z.number().default(0),
    totals: z.array(moneySchema).default([]),
    notional: z.array(moneySchema).default([]),
    group: z.string().default(''),
    is_group: z.boolean().default(false),
    previous: z.array(moneySchema).default([]),
    change: z.array(moneySchema).default([]),
    change_pct: z.array(moneySchema).default([]),
    is_removed: z.boolean().default(false),
    depth: z.number().default(0),
});

const errorSchema = z.object({
    scope: z.string().default(''),
    severity: z.string().default(''),
    message: z.string().default(''),
    cause: z.string().default(''),
    subject_id: z.string().default(''),
});

/** One error the run recorded, as the screen reads it. */
export interface RunError {
    readonly scope: string;
    readonly severity: string;
    readonly message: string;
    readonly cause: string;
    readonly subjectId: string;
}

/** The trades a run priced, as the screen reads them. */
export function toTradeRows(rows: readonly unknown[]): readonly RunResultRow[] {
    return rows.flatMap((row) => {
        const parsed = tradeSchema.safeParse(row);
        return parsed.success ? [parsed.data] : [];
    });
}

/** The books or counterparties a run's figures were rolled up to. */
export function toAggregateRows(rows: readonly unknown[]): readonly AggregateRow[] {
    return rows.flatMap((row) => {
        const parsed = aggregateSchema.safeParse(row);
        return parsed.success ? [parsed.data as AggregateRow] : [];
    });
}

/** The errors a run recorded, as the screen reads them. */
export function toErrors(rows: readonly unknown[]): readonly RunError[] {
    return rows.flatMap((row) => {
        const parsed = errorSchema.safeParse(row);
        if (!parsed.success) {
            return [];
        }
        return [
            {
                scope: parsed.data.scope,
                severity: parsed.data.severity,
                message: parsed.data.message,
                cause: parsed.data.cause,
                subjectId: parsed.data.subject_id,
            },
        ];
    });
}

/** A figure with its thousands grouped and two decimals, or the text as written. */
export function asFigure(text: string): string {
    const figure = Number.parseFloat(text);
    if (!Number.isFinite(figure)) {
        return text;
    }
    return new Intl.NumberFormat('en-GB', {
        minimumFractionDigits: 2,
        maximumFractionDigits: 2,
    }).format(figure);
}

/** The totals of one currency a set of rows states. */
export function moneyOf(rows: readonly { readonly totals: readonly Money[] }[]): readonly Money[] {
    const totals = new Map<string, number>();
    for (const row of rows) {
        for (const money of row.totals) {
            const figure = Number.parseFloat(money.total);
            if (Number.isFinite(figure)) {
                totals.set(money.currency, (totals.get(money.currency) ?? 0) + figure);
            }
        }
    }
    return [...totals.entries()].map(([currency, total]) => ({
        currency,
        total: total.toFixed(2),
    }));
}

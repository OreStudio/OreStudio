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

/** A field the server may send as null, read as the empty string. */
const text = z
    .string()
    .nullish()
    .transform((value) => value ?? '');

const moneySchema = z.object({
    currency: text,
    total: text,
});

const tradeSchema = z.object({
    trade_id: text,
    trade_type: text,
    npv: text,
    npv_currency: text,
    npv_base: text,
    notional: text,
    notional_currency: text,
    counterparty: text,
    counterparty_name: text,
    book_id: text,
    book_name: text,
});

const aggregateSchema = z.object({
    key: text,
    name: text,
    trades: z
        .number()
        .nullish()
        .transform((value) => value ?? 0),
    totals: z.array(moneySchema).default([]),
    notional: z.array(moneySchema).default([]),
    group: text,
    is_group: z
        .boolean()
        .nullish()
        .transform((value) => value ?? false),
    previous: z.array(moneySchema).default([]),
    change: z.array(moneySchema).default([]),
    change_pct: z.array(moneySchema).default([]),
    is_removed: z
        .boolean()
        .nullish()
        .transform((value) => value ?? false),
    depth: z
        .number()
        .nullish()
        .transform((value) => Math.max(0, value ?? 0)),
});

const errorSchema = z.object({
    scope: text,
    severity: text,
    message: text,
    cause: text,
    subject_id: text,
});

/** One error the run recorded, as the screen reads it. */
export interface RunError {
    readonly scope: string;
    readonly severity: string;
    readonly message: string;
    readonly cause: string;
    readonly subjectId: string;
}

/** What a parse of the server's rows produced, and how many it could not read. */
export interface Parsed<Row> {
    readonly rows: readonly Row[];
    readonly unread: number;
}

/** The trades a run priced, as the screen reads them. */
export function toTradeRows(rows: readonly unknown[]): Parsed<RunResultRow> {
    const read: RunResultRow[] = [];
    let unread = 0;
    for (const row of rows) {
        const parsed = tradeSchema.safeParse(row);
        if (parsed.success && parsed.data.trade_id !== '') {
            read.push(parsed.data);
        } else {
            unread += 1;
        }
    }
    return { rows: read, unread };
}

/** The books or counterparties a run's figures were rolled up to. */
export function toAggregateRows(rows: readonly unknown[]): Parsed<AggregateRow> {
    const read: AggregateRow[] = [];
    let unread = 0;
    for (const row of rows) {
        const parsed = aggregateSchema.safeParse(row);
        if (parsed.success && parsed.data.key !== '') {
            read.push(parsed.data);
        } else {
            unread += 1;
        }
    }
    return { rows: read, unread };
}

/** The errors a run recorded, as the screen reads them. */
export function toErrors(rows: readonly unknown[]): Parsed<RunError> {
    const read: RunError[] = [];
    let unread = 0;
    for (const row of rows) {
        const parsed = errorSchema.safeParse(row);
        if (!parsed.success) {
            unread += 1;
            continue;
        }
        read.push({
            scope: parsed.data.scope,
            severity: parsed.data.severity,
            message: parsed.data.message,
            cause: parsed.data.cause,
            subjectId: parsed.data.subject_id,
        });
    }
    return { rows: read, unread };
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

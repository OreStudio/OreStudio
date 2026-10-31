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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The Operations queue: the authorisation requests a person may decide, each
 * with the deal it names.
 *
 * The request is the unit of work and the deal is what the person reads. The
 * server joins them, because the inbox states a request and never what it is
 * about; the screen only divides the rows it is given.
 */

/** The state a request is in, as the inbox spells it. */
export type QueueState = 'waiting' | 'held' | 'approved' | 'refused' | 'cancelled' | string;

/**
 * How much work a deal is.
 *
 * The division is the queue's own and not the deal's asset class: a plain swap
 * and a package can be the same class and are not the same work.
 */
export type QueueClass = 'flow' | 'non-flow';

export interface QueueRow {
    readonly requestId: string;
    readonly requestVersion: number;
    readonly stateCode: string;
    readonly requestedBy: string;
    readonly requestedAt: string;
    readonly reason: string;
    readonly tradeId: string;
    /** The trade's own reference, which is what a person recognises it by. */
    readonly reference: string;
    readonly classCode: QueueClass;
    /** The entities the row links to: the book and the counterparty it faces. */
    readonly bookId: string;
    readonly counterpartyId: string;
    readonly tradeType: string;
    readonly book: string;
    readonly counterparty: string;
    readonly tradeDate: string;
    readonly status: string;
    /** The version the request was raised against, and the version now. */
    readonly requestTradeVersion: number;
    readonly tradeVersion: number;
    /** The deal moved after its request was raised. The row is still open. */
    readonly changed: boolean;
}

const rowSchema = z.object({
    requestId: z.string().default(''),
    requestVersion: z.int().nonnegative().default(0),
    stateCode: z.string().default(''),
    requestedBy: z.string().default(''),
    requestedAt: z.string().default(''),
    reason: z.string().default(''),
    tradeId: z.string().default(''),
    reference: z.string().default(''),
    classCode: z.enum(['flow', 'non-flow']).default('non-flow'),
    bookId: z.string().default(''),
    counterpartyId: z.string().default(''),
    tradeType: z.string().default(''),
    book: z.string().default(''),
    counterparty: z.string().default(''),
    tradeDate: z.string().default(''),
    status: z.string().default(''),
    requestTradeVersion: z.int().nonnegative().default(0),
    tradeVersion: z.int().nonnegative().default(0),
    changed: z.boolean().default(false),
});

const pageSchema = z.object({
    items: z.array(rowSchema).default([]),
    answered: z.array(rowSchema).default([]),
    total: z.int().nonnegative().default(0),
});

/** The states the queue groups by, in the order a person works them. */
export const QUEUE_GROUPS = ['awaiting', 'held', 'changed', 'authorised', 'denied'] as const;
export type QueueGroup = (typeof QUEUE_GROUPS)[number] | 'all';

const GROUP_OF_STATE: Readonly<Record<string, QueueGroup>> = {
    waiting: 'awaiting',
    held: 'held',
    approved: 'authorised',
    refused: 'denied',
    cancelled: 'denied',
};

/**
 * Which group a row is drawn in.
 *
 * A deal that moved after its request was raised is drawn as a change, because
 * that is the work it is now: the person re-reads the deal rather than deciding
 * it for the first time. Everything else follows its request's state.
 */
export function groupOf(row: QueueRow): QueueGroup {
    if (row.changed) return 'changed';
    return GROUP_OF_STATE[row.stateCode] ?? 'awaiting';
}

/**
 * The rows of one class in the states a person asked for, newest request first.
 *
 * The states are a choice rather than a mode: a person working the queue wants
 * the held deals and the changes at once and not the waiting ones. An empty
 * choice draws nothing, which is what a person who unticked every box asked
 * for, and `all` is stated by the screen as every group.
 */
export function rowsOf(
    rows: readonly QueueRow[],
    klass: QueueClass | 'all',
    states: readonly QueueGroup[],
): readonly QueueRow[] {
    const wanted = new Set(states);
    return rows
        .filter((row) => (klass === 'all' || row.classCode === klass) && wanted.has(groupOf(row)))
        .slice()
        .sort((left, right) => right.requestedAt.localeCompare(left.requestedAt));
}

/** How many rows one class holds in each group, for the rail's figures. */
export function countsOf(
    rows: readonly QueueRow[],
    klass: QueueClass | 'all',
): Readonly<Record<QueueGroup, number>> {
    const counts = { awaiting: 0, held: 0, changed: 0, authorised: 0, denied: 0, all: 0 } as Record<
        QueueGroup,
        number
    >;
    for (const row of rows) {
        if (klass !== 'all' && row.classCode !== klass) continue;
        counts.all += 1;
        counts[groupOf(row)] += 1;
    }
    return counts;
}

export const authorisationQueue = {
    /**
     * The queue in one read.
     *
     * A tenant's queue is the deals awaiting one control, so it is read whole
     * rather than paged: a page would hide the count the rail states, and the
     * count is what tells a person whether the queue moved.
     */
    async read(): Promise<{ items: readonly QueueRow[]; answered: readonly QueueRow[] }> {
        const params = new URLSearchParams({ offset: '0', limit: '100' });
        return pageSchema.parse(
            await request(`/api/queue/authorisations?${params.toString()}`, { method: 'GET' }),
        ) as { items: readonly QueueRow[]; answered: readonly QueueRow[] };
    },
};

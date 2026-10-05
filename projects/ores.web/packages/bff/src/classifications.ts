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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    CLASSIFICATION_LISTS,
    classificationCatalogue,
    classificationList,
    countClassificationRows,
    listClassificationRows,
    readClassificationLabels,
    readLabelCatalogue,
    setClassificationLabel,
    readEntityHistory,
    removeClassificationRow,
    saveClassificationRow,
    saveClassificationRows,
    type ClassificationWrite,
} from '@ores/wire-protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

const codeSchema = z.string().trim().min(1).max(100);
const textSchema = z.string().max(2000).default('');
const orderSchema = z.int().min(0).max(1_000_000).nullable().default(null);
const reasonSchema = z.object({
    reasonCode: z.string().trim().min(1).max(200),
    commentary: textSchema,
});

const createBodySchema = reasonSchema.extend({
    code: codeSchema,
    name: textSchema,
    description: textSchema,
    displayOrder: orderSchema,
});

const updateBodySchema = reasonSchema.extend({
    name: textSchema,
    description: textSchema,
    displayOrder: orderSchema,
    version: z.int().nonnegative(),
});

const orderBodySchema = reasonSchema.extend({
    rows: z
        .array(
            z.object({
                code: codeSchema,
                name: textSchema,
                description: textSchema,
                displayOrder: z.int().min(0).max(1_000_000),
                version: z.int().nonnegative(),
            }),
        )
        .min(1)
        .max(1000)
        .refine((rows) => new Set(rows.map((row) => row.code)).size === rows.length, {
            message: 'Each row may appear once.',
        }),
});

const labelBodySchema = reasonSchema.extend({
    badgeCode: z.string().trim().min(1).max(200).nullable(),
});

const historyQuerySchema = z.object({
    entityType: z.string().min(1).max(200),
    entityId: z.string().min(1).max(200),
});

/** The entity types whose history the BFF serves: those of the lists it serves. */
const HISTORY_TYPES = new Set(CLASSIFICATION_LISTS.map((list) => list.entityType));

/**
 * The list the address names, or a 404.
 *
 * The browser names a list by its key, never by a subject: the catalogue maps
 * the key to the subjects, and a key the catalogue lacks reaches nothing.
 */
function listFor(request: FastifyRequest): NonNullable<ReturnType<typeof classificationList>> {
    const { list } = request.params as { list: string };
    const found = classificationList(list);
    if (found === undefined) {
        throw notFound(`There is no classification list ${list}.`);
    }
    return found;
}

/** The list the address names, refused when it holds spellings ORE needs exactly. */
function editableListFor(
    request: FastifyRequest,
): NonNullable<ReturnType<typeof classificationList>> {
    const list = listFor(request);
    if (!list.editable) {
        throw notPermitted('This list holds spellings ORE documents must write exactly.');
    }
    return list;
}

/**
 * Turns a refused write into the status that says why.
 *
 * The server sends no words with some refusals, so a conflict says what it
 * means rather than showing the person an empty error.
 */
function answer(outcome: ClassificationWrite): void {
    if (outcome.done) {
        return;
    }
    switch (outcome.outcome) {
        case 'conflict':
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    outcome.message === ''
                        ? 'This row changed since it was read, or its code is already in the list. Reload and try again.'
                        : outcome.message,
            });
        case 'invalid':
            throw invalidRequest(outcome.message);
        case 'denied':
            throw notPermitted(outcome.message);
        case 'missing':
            throw notFound(outcome.message);
        default:
            throw new HttpFailure(502, { code: 'upstream-unavailable', message: outcome.message });
    }
}

/**
 * The classification routes: the catalogue, a list's rows, the writes, and
 * the history of one row.
 */
export function registerClassificationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The lists the screen offers, by topic, with each list's columns and how
     * many rows it holds. A count that cannot be read is null rather than a
     * failed page: the list is still there to open.
     */
    server.get('/api/classifications', async (request) => {
        const session = requireSession(request);
        /*
         * One at a time: the session's broker client answers 28 concurrent
         * requests in about eight seconds and the same requests in sequence in
         * about 0.2 seconds.
         */
        const counts: PromiseSettledResult<number>[] = [];
        for (const list of CLASSIFICATION_LISTS) {
            counts.push(
                await countClassificationRows(session.client, list).then(
                    (value) => ({ status: 'fulfilled', value }) as const,
                    (reason: unknown) => ({ status: 'rejected', reason }) as const,
                ),
            );
        }
        return {
            lists: classificationCatalogue().map((list, index) => {
                const count = counts[index];
                return { ...list, count: count?.status === 'fulfilled' ? count.value : null };
            }),
        };
    });

    /**
     * Every row of one list, each with its label. Labels that cannot be read
     * leave the rows unlabelled rather than unread.
     */
    server.get('/api/classifications/:list', async (request) => {
        const session = requireSession(request);
        const list = listFor(request);
        const [rows, labels] = await Promise.all([
            listClassificationRows(session.client, list),
            readClassificationLabels(session.client, list).catch(
                (error: unknown): Readonly<Record<string, string>> => {
                    request.log.warn({ err: error, list: list.key }, 'labels could not be read');
                    return {};
                },
            ),
        ]);
        return { rows: rows.map((row) => ({ ...row, labelCode: labels[row.code] ?? null })) };
    });

    /** The shared label catalogue: every label, and the labels each code domain uses. */
    server.get('/api/labels', async (request) => {
        const session = requireSession(request);
        return readLabelCatalogue(session.client);
    });

    /**
     * Gives a row a label from the catalogue, or takes it away. A label is how
     * a code is drawn, not how ORE spells it, so a read-only list may be
     * labelled too.
     */
    server.put('/api/classifications/:list/rows/:code/label', async (request, reply) => {
        const session = requireSession(request);
        const list = listFor(request);
        const { code } = request.params as { code: string };
        const body = labelBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A label needs a badge, or null to take it away, and a reason.');
        }
        const { badgeCode, reasonCode, commentary } = body.data;
        /*
         * The label catalogue maps any code of any domain, so it cannot refuse
         * a code the list does not hold; the BFF does.
         */
        const rows = await listClassificationRows(session.client, list);
        if (!rows.some((row) => row.code === code)) {
            throw notFound(`${list.key} has no row ${code}.`);
        }
        answer(
            await setClassificationLabel(session.client, list, code, badgeCode, {
                reasonCode,
                commentary,
            }),
        );
        return reply.code(204).send();
    });

    /** Adds a row. A code already in the list is refused rather than replaced. */
    server.post('/api/classifications/:list/rows', async (request, reply) => {
        const session = requireSession(request);
        const list = editableListFor(request);
        const body = createBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A row needs a code and a change reason.');
        }
        const { reasonCode, commentary, ...row } = body.data;
        answer(
            await saveClassificationRow(
                session.client,
                list,
                { ...row, version: null },
                { reasonCode, commentary },
            ),
        );
        return reply.code(204).send();
    });

    /** Corrects a row, against the version the screen read. */
    server.put('/api/classifications/:list/rows/:code', async (request, reply) => {
        const session = requireSession(request);
        const list = editableListFor(request);
        const { code } = request.params as { code: string };
        const body = updateBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A correction needs the version it was read at and a reason.');
        }
        const { reasonCode, commentary, ...row } = body.data;
        answer(
            await saveClassificationRow(
                session.client,
                list,
                { ...row, code },
                { reasonCode, commentary },
            ),
        );
        return reply.code(204).send();
    });

    /** Writes the new display order of the rows that moved, in one call. */
    server.put('/api/classifications/:list/order', async (request, reply) => {
        const session = requireSession(request);
        const list = editableListFor(request);
        if (list.shape === 'plain') {
            throw invalidRequest('This list has no order.');
        }
        const body = orderBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('Send the rows that moved, each with its version, and a reason.');
        }
        const { reasonCode, commentary, rows } = body.data;
        answer(
            await saveClassificationRows(session.client, list, rows, { reasonCode, commentary }),
        );
        return reply.code(204).send();
    });

    /** Removes a row. Its versions stay in the history. */
    server.delete('/api/classifications/:list/rows/:code', async (request, reply) => {
        const session = requireSession(request);
        const list = editableListFor(request);
        const { code } = request.params as { code: string };
        const body = reasonSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A removal needs a change reason.');
        }
        answer(await removeClassificationRow(session.client, list, code, body.data));
        return reply.code(204).send();
    });

    /** Every version of one row, newest first, with what changed in each. */
    server.get('/api/history', async (request) => {
        const session = requireSession(request);
        const query = historyQuerySchema.safeParse(request.query);
        if (!query.success) {
            throw invalidRequest('Name the record by its entity type and its key.');
        }
        if (!HISTORY_TYPES.has(query.data.entityType)) {
            throw notFound(`No history is served for ${query.data.entityType}.`);
        }
        return {
            versions: await readEntityHistory(
                session.client,
                query.data.entityType,
                query.data.entityId,
            ),
        };
    });
}

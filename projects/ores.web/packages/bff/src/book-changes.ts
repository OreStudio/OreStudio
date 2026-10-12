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
import { previewBookChanges, raiseBookChanges } from '@ores/wire-protocol';
import { idSchema, input } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The routes a maker uses to propose changes to books instead of writing them.
 *
 * A preview runs the real write for each line and writes nothing, so every
 * refusal comes back before anyone waits, together with the parts the policy
 * needs. A raise holds the lines in one approval request. The server checks the
 * permission each line's write needs, so these routes check none of their own.
 */

/** The most lines one proposal may hold, as the server holds it. */
const MAX_LINES = 1000;

const textSchema = z.string().max(2000);

const lineSchema = z.object({
    operation: z.enum(['put', 'delete']),
    entityId: idSchema,
    baseVersion: z.int().nonnegative(),
    partyId: idSchema,
    name: z.string().trim().min(1).max(200),
    description: textSchema,
    parentPortfolioId: idSchema,
    ownerUnitId: idSchema.nullable(),
    functionalCurrency: z.string().trim().max(16),
    glAccountRef: textSchema,
    costCenter: textSchema,
    bookStatus: z.string().trim().max(100),
    regulatoryBookType: z.string().trim().max(100),
    bookPurposeType: z.string().trim().max(100),
    ledgerFeedType: z.string().trim().max(100),
    isSweepable: z.boolean(),
    ratesCentreCode: textSchema,
    sandboxId: idSchema.nullable(),
});

const previewBodySchema = z.object({
    lines: z.array(lineSchema).min(1).max(MAX_LINES),
});

const raiseBodySchema = previewBodySchema.extend({
    reason: z.string().trim().min(1).max(500),
});

export function registerBookChangeRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Runs the proposed writes through the real service and writes nothing. */
    server.post('/api/book-changes/preview', async (request) => {
        const session = requireSession(request);
        const body = input(previewBodySchema, request.body);
        return await previewBookChanges(session.client, session.tenantId, body.lines);
    });

    /**
     * Raises one request that holds the proposed writes.
     *
     * A refusal of the lines, or of a proposal that needs no request, is an
     * answer with the reason and no request id, because the maker has
     * something to correct.
     */
    server.post('/api/book-changes', async (request) => {
        const session = requireSession(request);
        const body = input(raiseBodySchema, request.body);
        return await raiseBookChanges(session.client, session.tenantId, body);
    });
}

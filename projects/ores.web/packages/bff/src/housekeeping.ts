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
    STORE_SAMPLES_LIMIT,
    readStoreBounds,
    readStoreSamples,
    saveStoreBound,
    storeBoundResultSchema,
    storeBoundWriteSchema,
    storeBoundsViewSchema,
    storeSamplesViewSchema,
    toWireTimestamp,
    type WriteOutcome,
} from '@ores/wire-protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The housekeeping routes: what the installation keeps, how large each store is,
 * and the one write a person makes to it.
 *
 * Two reads and one write, each one operation of the scheduler service. Nothing
 * here joins a store to its policy or decides whether a store is bounded: the
 * bounds view states that, and a screen that repeated the rule would be a second
 * answer to the same question.
 *
 * The write is the smallest one the domain allows. A person changes whether the
 * bound applies, its period and its cadence, and the route reads the store's own
 * row to compose the rest of the policy — its target, its action and its words.
 * A client that sent those back could retarget a bound, so it never sees them as
 * input.
 */

const codeParamsSchema = z.object({
    code: z.string().trim().min(1).max(120),
});

const samplesQuerySchema = z.object({
    limit: z
        .string()
        .regex(/^\d+$/)
        .transform(Number)
        .pipe(z.int().min(1).max(STORE_SAMPLES_LIMIT))
        .default(STORE_SAMPLES_LIMIT),
});

const LIMIT_REFUSED = `The samples read takes a limit of 1 to ${STORE_SAMPLES_LIMIT}.`;

const STORE_REFUSED = 'The address names no store.';

function storeCode(request: FastifyRequest): string {
    const parsed = codeParamsSchema.safeParse(request.params);
    if (!parsed.success) {
        throw invalidRequest(STORE_REFUSED);
    }
    return parsed.data.code;
}

/**
 * Turns a refused write into the status that says why.
 *
 * A conflict is the version the person read no longer holding, so the screen
 * reads again and the second attempt names the version it finds; the route does
 * not retry a write on the person's behalf.
 */
function answer(outcome: WriteOutcome): void {
    if (outcome.done) {
        return;
    }
    switch (outcome.outcome) {
        case 'conflict':
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    outcome.message === ''
                        ? 'This bound changed since it was read. Reload and try again.'
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
 * The store named in the address, read from the catalogue the route already has
 * to ask for, so the write composes from the platform's statement of the store
 * and not from the request.
 */
async function storeIn(session: LiveSession, code: string) {
    const stores = await readStoreBounds(session.client);
    const store = stores.find((candidate) => candidate.code === code);
    if (store === undefined) {
        throw notFound(`This installation keeps no store called ${code}.`);
    }
    return store;
}

export function registerHousekeepingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Every store the installation keeps, with its bound, its last run and its size. */
    server.get('/api/operations/housekeeping/stores', async (request) => {
        const session = requireSession(request);
        const rows = await readStoreBounds(session.client);
        return storeBoundsViewSchema.parse({
            read_at: toWireTimestamp(new Date()),
            rows,
        });
    });

    /** One store's measurements, oldest first, for the plot of its growth. */
    server.get('/api/operations/housekeeping/stores/:code/samples', async (request) => {
        const session = requireSession(request);
        const code = storeCode(request);
        const query = samplesQuerySchema.safeParse(request.query ?? {});
        if (!query.success) {
            throw invalidRequest(LIMIT_REFUSED);
        }
        const samples = await readStoreSamples(session.client, code, query.data.limit);
        return storeSamplesViewSchema.parse({ store_code: code, samples });
    });

    /**
     * Changes one store's bound: whether it applies, its period and its cadence.
     *
     * The store is read before the write, so the policy's target, its action and
     * its words come from the catalogue. A store the catalogue says no period can
     * bound is refused by the write itself, which has no action to send.
     */
    server.put('/api/operations/housekeeping/stores/:code/policy', async (request) => {
        const session = requireSession(request);
        const code = storeCode(request);
        const body = storeBoundWriteSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'A bound names whether it applies, its period in days, its cadence in ' +
                    'hours and the reason for the change.',
            );
        }
        const store = await storeIn(session, code);
        const outcome = await saveStoreBound(
            session.client,
            store,
            {
                enabled: body.data.enabled,
                retentionDays: body.data.retention_days,
                cadenceHours: body.data.cadence_hours,
            },
            { reasonCode: body.data.reason_code, commentary: body.data.commentary },
        );
        answer(outcome);
        return storeBoundResultSchema.parse({ success: true, message: '', outcome: 'ok' });
    });
}

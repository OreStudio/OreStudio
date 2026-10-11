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
import { isWireTimestamp } from '@ores/wire-protocol';
import { subjects } from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure } from './errors.js';
import { input } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The reads of a composite object: the series an identity names, the series a party
 * can see, the object at one instant, one slice of it over time, and its evolution over a range.
 *
 * Each route forwards the typed request to the server's subject and returns the
 * reply without its success and message fields. The server checks the
 * permission and the shape, so a route adds no rule of its own beyond the
 * structure of the request.
 *
 * Every read is a POST because the identity is a structure, not a path. None of
 * them writes.
 */

/**
 * The wire spelling for an end of a range the caller did not give.
 *
 * The server reads the range only when no instants are stated, and a stated set
 * replaces the range. So this value reaches the server only where the server
 * ignores it. The evolution tests pin that.
 */
const NO_INSTANT = '1970-01-01 00:00:00Z';

/** The most instants a caller may state, so a request cannot ask for an unbounded matrix. */
export const MAX_STATED_INSTANTS = 500;

const word = z.string().trim().min(1).max(100);
const instant = z.string().refine(isWireTimestamp, {
    message: 'An instant is written YYYY-MM-DD HH:MM:SSZ.',
});

const identity = z.strictObject({
    asset: word,
    scope: word,
    instrument_type: word,
    quote_type: word,
    party_id: z.union([z.uuid(), z.literal('')]).default(''),
    fields: z
        .array(z.strictObject({ name: word, text: word }))
        .max(20)
        .default([]),
});

const identityBody = z.strictObject({ identity });

const seriesListBody = z.strictObject({
    party_id: z.union([z.uuid(), z.literal('')]).default(''),
});

const snapshotBody = identityBody.extend({
    include_provenance: z.boolean().default(false),
});

/** Wire timestamps are fixed width, so the text order is the time order. */
const rangeOrder = {
    message: 'The range ends before it starts.',
};

const sliceBody = identityBody
    .extend({
        component_field: word,
        component_value: word,
        from_instant: instant,
        to_instant: instant,
    })
    .refine((body) => body.from_instant <= body.to_instant, rangeOrder);

const evolutionBody = identityBody
    .extend({
        from_instant: instant.optional(),
        to_instant: instant.optional(),
        instants: z.array(instant).max(MAX_STATED_INSTANTS).default([]),
    })
    .refine(
        (body) =>
            body.instants.length > 0 ||
            (body.from_instant !== undefined && body.to_instant !== undefined),
        { message: 'State the instants, or state both ends of the range.' },
    )
    .refine(
        (body) =>
            body.instants.length > 0 ||
            (body.from_instant as string) <= (body.to_instant as string),
        rangeOrder,
    );

const replySchema = z.looseObject({
    success: z.boolean(),
    message: z.string().default(''),
});

/**
 * A refused read is a request the server will not answer, such as an object
 * with no axis or a range that ends before it starts. The server's words are
 * the reason, so they travel as they are.
 */
function refused(message: string): HttpFailure {
    return new HttpFailure(422, { code: 'invalid-request', message });
}

async function call(
    session: LiveSession,
    subject: string,
    body: unknown,
): Promise<Record<string, unknown>> {
    const reply = await session.client.callAuthenticated(subject, body, replySchema);
    if (!reply.success) {
        throw refused(reply.message);
    }
    return Object.fromEntries(
        Object.entries(reply).filter(([key]) => key !== 'success' && key !== 'message'),
    );
}

export function registerCurveRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.post('/api/curves/series', async (request) => {
        const session = requireSession(request);
        const body = input(identityBody, request.body);
        return call(session, subjects.resolve_series_identity_request, body.identity);
    });

    // The series the session's party can see, each as the identity the other reads
    // take. The body may be empty, which asks across the parties the session sees.
    server.post('/api/curves/series-list', async (request) => {
        const session = requireSession(request);
        const body = input(seriesListBody, request.body ?? {});
        return call(session, subjects.list_series_identities_request, body);
    });

    server.post('/api/curves/snapshot', async (request) => {
        const session = requireSession(request);
        const body = input(snapshotBody, request.body);
        return call(session, subjects.get_curve_snapshot_request, body);
    });

    server.post('/api/curves/slice', async (request) => {
        const session = requireSession(request);
        const body = input(sliceBody, request.body);
        return call(session, subjects.get_series_slice_request, body);
    });

    server.post('/api/curves/evolution', async (request) => {
        const session = requireSession(request);
        const body = input(evolutionBody, request.body);
        // A stated set replaces the range, so the range is not sent with it.
        const stated = body.instants.length > 0;
        return call(session, subjects.get_series_evolution_request, {
            identity: body.identity,
            from_instant: stated ? NO_INSTANT : (body.from_instant ?? NO_INSTANT),
            to_instant: stated ? NO_INSTANT : (body.to_instant ?? NO_INSTANT),
            instants: body.instants,
        });
    });
}

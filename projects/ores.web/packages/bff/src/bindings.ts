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
import { feedBindingActivityListSchema } from '@ores/contracts';
import {
    subjects,
    type FeedBindingWrite,
    type FeedBindingsFilter,
} from '@ores/wire-protocol/generated/marketdata/protocol/feed_binding_protocol';
import {
    subjects as operationSubjects,
    type ListFeedBindingActivityResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { NO_ORDER, idSchema, refusal, resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';
import type { SyntheticResource } from './synthetic.js';

/**
 * The feed bindings of the party the session works in, served as a record kind of the
 * synthetic routes, and what each binding's source has written.
 *
 * The bindings and their observations are isolated by tenant alone, so the resource
 * adds the session's party to every read. A browser states the source and whether the
 * binding is enabled, and nothing else: the party is the session's, and the kind is
 * synthetic. A binding that a real feed made is read only and a write to it is refused,
 * because this product does not connect real feeds. It can still be deleted.
 */

/** The kind of a binding that names a generated producer. */
const SYNTHETIC = 'SYNTHETIC';

/** The party the session works in, or a refusal when it has not chosen one. */
function partyOf(session: LiveSession): string {
    const party = session.party?.id;
    if (party === undefined) {
        throw invalidRequest('Choose a party before reading or changing its bindings.');
    }
    return party;
}

/** What a browser may state about a binding. Everything else is the server's or the session's. */
const writes = z.strictObject({
    id: idSchema,
    source_name: z.string().trim().min(1).max(200),
    enabled: z.boolean(),
});

/**
 * The whole write the server takes: the browser's members, the session's party, and
 * the kind. A binding that exists keeps its party and kind, and a vendor binding is
 * refused.
 */
async function complete(
    session: LiveSession,
    write: Record<string, unknown>,
    read: (id: string) => Promise<Record<string, unknown> | undefined>,
): Promise<Record<string, unknown>> {
    const stated = writes.parse(write);
    // The read is scoped to the session's party, so an id of another party's binding reads
    // as no binding and the write goes out as a new one, which the server refuses as a
    // key that exists. Between this read and the save a row may change; the version of an
    // edit and the must-not-exist of a create are what the server decides on.
    const existing = await read(stated.id);
    if (existing !== undefined && existing['producer_kind'] !== SYNTHETIC) {
        throw notPermitted(
            'Real feeds are not supported, so a binding that a real feed made is read only. It can be deleted.',
        );
    }
    return {
        id: stated.id,
        party_id: existing === undefined ? partyOf(session) : String(existing['party_id']),
        source_name: stated.source_name,
        producer_kind: SYNTHETIC,
        enabled: stated.enabled,
    } satisfies FeedBindingWrite;
}

export const FEED_BINDINGS_RESOURCE: SyntheticResource = {
    key: 'feed-bindings',
    entityType: 'ores.marketdata.feed_binding',
    keyField: 'id',
    oneOf: 'id_one_of',
    removalKey: 'source_name',
    rows: 'feed_bindings',
    namespace: 'marketdata',
    filter: {
        party_id: null,
        id_one_of: null,
        party_id_one_of: null,
        search: null,
    } satisfies FeedBindingsFilter,
    scope: (session) => ({ party_id: partyOf(session) }),
    search: true,
    sortable: ['source_name', 'enabled'],
    subjects: {
        list: subjects.list_feed_bindings_request,
        putMany: subjects.put_many_feed_bindings_request,
        remove: subjects.delete_feed_binding_request,
        removeMany: subjects.delete_many_feed_bindings_request,
    },
    writes,
    complete,
};

/** The entity type of a binding's history, which only the party that owns the binding may read. */
export const FEED_BINDING_ENTITY = FEED_BINDINGS_RESOURCE.entityType;

/**
 * Refuses the history of a binding the session's party does not own.
 *
 * The history of a binding is read by its source name, which every party of a tenant may
 * hold, and the history store is isolated by tenant alone. So the binding is looked up
 * first, as the session's party and by that exact name, and a name the party does not
 * hold has no history to show.
 */
export async function assertBindingVisible(
    session: LiveSession,
    sourceName: string,
): Promise<void> {
    const reply = await session.client.callAuthenticated(
        subjects.list_feed_bindings_request,
        {
            offset: 0,
            limit: 100,
            order: NO_ORDER,
            filter: {
                ...FEED_BINDINGS_RESOURCE.filter,
                party_id: partyOf(session),
                search: sourceName,
            },
            as_of: null,
        },
        z.looseObject({
            result: resultSchema,
            feed_bindings: z.array(z.looseObject({ source_name: z.string() })).default([]),
        }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    if (!reply.feed_bindings.some((found) => found.source_name === sourceName)) {
        throw notFound('There is no such binding for this party.');
    }
}

const activityReply = feedBindingActivityListSchema.extend({
    success: z.boolean().default(false),
    message: z.string().default(''),
}) satisfies z.ZodType<Pick<ListFeedBindingActivityResponse, 'activities'>, unknown>;

/**
 * The route for what each binding has written: the series of its source's newest
 * observation and the time of that observation. The server reads for the session's
 * party, so the answer is the party's view.
 */
export function registerBindingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/bindings/activity', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            operationSubjects.list_feed_binding_activity_request,
            {},
            activityReply,
        );
        if (!reply.success) {
            throw new HttpFailure(502, {
                code: 'upstream-unavailable',
                message: reply.message === '' ? 'The bindings could not be read.' : reply.message,
            });
        }
        return { activities: reply.activities };
    });
}

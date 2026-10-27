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
    askForRoles,
    clearNotifications,
    decideRequest,
    markNotificationsRead,
    readHoldReasons,
    readKindHoldRules,
    readMyNotifications,
    readMyRequests,
    readRequest,
    readRequestChanges,
    readRequestChecks,
    readRequestQueue,
    readRequestStory,
    readUnreadNotificationCount,
    withdrawRequest,
} from '@ores/wire-protocol';
import type { RequestViewer } from '@ores/wire-protocol';
import { subjects as authorisationSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_authorisation_request_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { invalidRequest, notFound } from './errors.js';
import { readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The inbox routes: a member's own requests and notifications, and the queue a
 * tenant administrator answers from.
 *
 * Every route here is gated on a session and on nothing else. Which role may
 * decide which kind of request is data, not a mode: the server holds the
 * permission each kind names and refuses the call itself, so the BFF must not
 * guess at it and answer a refusal of its own. A route that answered 403 from
 * a mode check would refuse an administrator that the server would have
 * accepted.
 */

/**
 * The most rows one page may ask for.
 *
 * The cap is not only about payload size. A page of requests costs the join
 * inside the wire layer one read per request, because the store that holds
 * them serves no batched read, so a page is also a bound on round trips.
 */
const MAX_PAGE = 100;
const DEFAULT_PAGE = 50;

const pageQuerySchema = z.object({
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(MAX_PAGE)).default(DEFAULT_PAGE),
});

const unreadQuerySchema = pageQuerySchema.extend({
    unreadOnly: z
        .enum(['true', 'false'])
        .default('false')
        .transform((value) => value === 'true'),
});

const idSchema = z.uuid();

/** The four decisions a decider may reach, as the store spells them. */
const DECISION_CODES = ['approve', 'refuse', 'hold', 'resume'] as const;

const askBodySchema = z.object({
    roleIds: z.array(idSchema).min(1).max(MAX_PAGE),
    reason: z.string().trim().max(500),
});

const intentBodySchema = z.object({
    version: z.int().nonnegative(),
    comment: z.string().trim().max(500),
});

const decideBodySchema = intentBodySchema.extend({
    decisionCode: z.enum(DECISION_CODES),
    // The part the decider answers for. A request that names no parts takes none.
    partCode: z
        .string()
        .trim()
        .max(100)
        .regex(/^[a-z][a-z0-9_]*$/)
        .default(''),
    /*
     * Why the request is parked, when the decision is a hold: one of the codes
     * the hold reason lookup holds. The shape is checked here so a typo is a
     * 400; whether the lookup holds the code is the server's answer, and it
     * names the codes it does hold.
     */
    holdReasonCode: z
        .string()
        .trim()
        .max(100)
        .regex(/^[a-z][a-z0-9_.]*$/)
        .default(''),
});

/**
 * The notifications a write names.
 *
 * `ids` is required and has no default, deliberately. An absent field and an
 * empty list mean opposite things to the server: an empty list is "every one
 * that is unread", or "every one that is read" for a clear. A body that
 * forgot the field would therefore clear the whole list, so it is refused
 * rather than read as empty.
 */
const idsBodySchema = z.object({ ids: z.array(idSchema).max(MAX_PAGE) });

function pageOf(query: unknown) {
    const page = pageQuerySchema.safeParse(query);
    if (!page.success) {
        throw invalidRequest(`A page names an offset and a limit of 1 to ${MAX_PAGE}.`);
    }
    return page.data;
}

function requestIdOf(request: FastifyRequest): string {
    const id = idSchema.safeParse((request.params as { id: string }).id);
    if (!id.success) {
        throw invalidRequest('A request is named by its identifier.');
    }
    return id.data;
}

/**
 * Who the reads are for.
 *
 * The join cannot turn an account id into a username for a member, who may not
 * read the account list, so it is told who is asking. Their own name is the one
 * name it never has to be told twice.
 */
function viewerOf(session: LiveSession): RequestViewer {
    return { accountId: session.accountId, username: session.username };
}

/**
 * The trades a request is about.
 *
 * An operational authorisation is raised against a trade, and the trading
 * component keeps the pair: the request names no trade itself, so this is the
 * only way from one to the other. The identifier is read for each trade because
 * a uuid is not a name a person recognises, and a trade with no identifier is
 * still linked.
 */
async function subjectsOf(session: LiveSession, requestId: string): Promise<readonly unknown[]> {
    /*
     * A refused read costs the link and nothing else: the request page is worth
     * opening whether or not this reader may see what the request is about.
     */
    let joined: readonly Record<string, unknown>[];
    try {
        joined = await readAll(
            session,
            authorisationSubjects.list_trade_authorisation_requests_request,
            'trade_authorisation_requests',
            {
                filter: { trade_id: null, request_id_one_of: [requestId], trade_id_one_of: null },
                as_of: null,
            },
        );
    } catch {
        return [];
    }
    const subjects = [];
    for (const row of joined.slice(0, 10)) {
        const tradeId = String(row['trade_id'] ?? '');
        if (tradeId === '') continue;
        let label = tradeId;
        try {
            const identifiers = await readAll(
                session,
                identifierSubjects.list_trade_identifiers_request,
                'identifiers',
                { filter: { trade_id_one_of: [tradeId] }, as_of: null },
            );
            const value = identifiers
                .map((identifier) => String(identifier['id_value'] ?? ''))
                .find((candidate) => candidate !== '');
            if (value !== undefined) label = value;
        } catch {
            // The uuid is still a link, so a refused identifier read costs only the label.
        }
        subjects.push({ tradeId, tradeVersion: Number(row['trade_version'] ?? 0), label });
    }
    return subjects;
}

export function registerInboxRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** The signed-in person's own requests, newest first, with what was decided. */
    server.get('/api/me/requests', async (request) => {
        const session = requireSession(request);
        const page = pageOf(request.query);
        return await readMyRequests(session.client, page, viewerOf(session));
    });

    /** Asks for roles for the signed-in person, and answers the request raised. */
    server.post('/api/me/requests', async (request) => {
        const session = requireSession(request);
        const body = askBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('Asking for roles names at least one role, and why.');
        }
        return await askForRoles(session.client, body.data);
    });

    /**
     * Takes back a request the person raised, against the version they saw.
     *
     * The body is a body on a DELETE because it carries the claim: only the
     * version the person read may be withdrawn, so a queue that moved on is
     * refused rather than silently discarded.
     */
    server.delete('/api/me/requests/:id', async (request, reply) => {
        const session = requireSession(request);
        const requestId = requestIdOf(request);
        const body = intentBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('Withdrawing a request names the version read and why.');
        }
        await withdrawRequest(session.client, {
            requestId,
            version: body.data.version,
            comment: body.data.comment,
        });
        return reply.code(204).send();
    });

    /**
     * The open requests the signed-in person may decide, oldest first.
     *
     * The server picks the queue, so a member with no kind to decide gets an
     * empty page rather than a refusal.
     */
    server.get('/api/requests', async (request) => {
        const session = requireSession(request);
        const page = pageOf(request.query);
        return await readRequestQueue(session.client, page, viewerOf(session));
    });

    /**
     * The one request an identifier names, when the signed-in person may open
     * it.
     *
     * A notice carries the request it is about, and a notice is read after the
     * request stopped waiting, so a queue read cannot answer this. The server
     * decides who may open it; a request this person may not see is answered
     * as not found, which is also what a request that does not exist answers.
     */
    server.get('/api/requests/:id', async (request) => {
        const session = requireSession(request);
        const id = requestIdOf(request);
        const detail = await readRequest(session.client, id, viewerOf(session));
        return { ...detail, subjects: await subjectsOf(session, id) };
    });

    /**
     * The whole story of one request, newest first.
     *
     * Every row any component wrote for it, merged by the wire layer: the
     * request's versions, the answers given on it, the roles it asked for and
     * what became of them, and the notices this reader was given. A request the
     * person may not open answers empty, which is what opening it answers too.
     */
    server.get('/api/requests/:id/story', async (request) => {
        const session = requireSession(request);
        return await readRequestStory(session.client, requestIdOf(request));
    });

    /**
     * What the request proposes, for any kind: the operation, the thing it is
     * about, and each field beside the value it would replace.
     *
     * The request is read first, because the kind decides where its lines are
     * kept. A request the person may not open is not found, as it is when it is
     * opened.
     */
    server.get('/api/requests/:id/changes', async (request) => {
        const session = requireSession(request);
        const requestId = requestIdOf(request);
        const found = await readRequest(session.client, requestId, viewerOf(session));
        if (found === null) {
            throw notFound('No such request.');
        }
        return {
            requestId,
            kindCode: found.kindCode,
            changes: await readRequestChanges(session.client, found),
        };
    });

    /**
     * What running the request's lines through the real write now says.
     *
     * The live record may have moved since the request was raised, so a decider
     * reads the checks as they stand.
     */
    server.get('/api/requests/:id/checks', async (request) => {
        const session = requireSession(request);
        const requestId = requestIdOf(request);
        const found = await readRequest(session.client, requestId, viewerOf(session));
        if (found === null) {
            throw notFound('No such request.');
        }
        return { requestId, ...(await readRequestChecks(session.client, found)) };
    });

    /** Approves, refuses, holds or resumes a request, against the version read. */
    server.post('/api/requests/:id/decision', async (request, reply) => {
        const session = requireSession(request);
        const requestId = requestIdOf(request);
        const body = decideBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'A decision is approve, refuse, hold or resume, against the version read.',
            );
        }
        await decideRequest(session.client, {
            requestId,
            version: body.data.version,
            decisionCode: body.data.decisionCode,
            comment: body.data.comment,
            partCode: body.data.partCode,
            holdReasonCode: body.data.holdReasonCode,
        });
        return reply.code(204).send();
    });

    /**
     * The reasons a decider may park a request with.
     *
     * A hold names a code from this list rather than free text, so the decision
     * panel offers it. The list is a tenant-wide lookup and carries no personal
     * data, so the route is gated on a session and on nothing else.
     */
    server.get('/api/hold-reasons', async (request) => {
        const session = requireSession(request);
        const holdReasons = await readHoldReasons(session.client);
        /*
         * The kinds' rules are a second read, and a person who may hold a
         * request but not read the kind catalogue still gets the reasons: the
         * panel then offers no hold, which is the failure that asks for a
         * grant rather than one that reports a server fault.
         */
        let allowsHoldByKind: Readonly<Record<string, boolean>> = {};
        try {
            allowsHoldByKind = await readKindHoldRules(session.client);
        } catch {
            allowsHoldByKind = {};
        }
        return { holdReasons, allowsHoldByKind };
    });

    /** The signed-in person's notifications, newest first. */
    server.get('/api/me/notifications', async (request) => {
        const session = requireSession(request);
        const query = unreadQuerySchema.safeParse(request.query);
        if (!query.success) {
            throw invalidRequest(`A page names an offset and a limit of 1 to ${MAX_PAGE}.`);
        }
        return await readMyNotifications(session.client, {
            unreadOnly: query.data.unreadOnly,
            offset: query.data.offset,
            limit: query.data.limit,
        });
    });

    /** How many notifications are unread, for the bell on every screen. */
    server.get('/api/me/notifications/unread-count', async (request) => {
        const session = requireSession(request);
        return { unread: await readUnreadNotificationCount(session.client) };
    });

    /**
     * Marks notifications read.
     *
     * An empty `ids` marks every unread one, so the field is required: see
     * {@link idsBodySchema}.
     */
    server.post('/api/me/notifications/read', async (request) => {
        const session = requireSession(request);
        const body = idsBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'Marking notifications read names the ones to mark, or an empty list for all unread.',
            );
        }
        return { marked: await markNotificationsRead(session.client, { ids: body.data.ids }) };
    });

    /**
     * Removes notifications from the person's list.
     *
     * An empty `ids` clears every read one, so the field is required: see
     * {@link idsBodySchema}.
     */
    server.post('/api/me/notifications/clear', async (request) => {
        const session = requireSession(request);
        const body = idsBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'Clearing notifications names the ones to clear, or an empty list for all read.',
            );
        }
        return { cleared: await clearNotifications(session.client, { ids: body.data.ids }) };
    });
}

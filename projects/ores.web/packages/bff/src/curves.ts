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
import { subjects as publishedCurves } from '@ores/wire-protocol/generated/marketdata/protocol/published_curves_operations_protocol';
import { subjects as marketSeriesSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_series_protocol';
import { HttpFailure } from './errors.js';
import { NO_ORDER, input, resultSchema } from './refdata-calls.js';
import { anyUuid } from './records.js';
import type { LiveSession } from './sessions.js';

/**
 * The reads of a composite object: the series an identity names, the series a party
 * can see, the object at one instant, one slice of it over time, and its evolution
 * over a range. Beside them, the two writes a person makes by hand: keying a
 * manual point on one node, and clearing it.
 *
 * Each route forwards the typed request to the server's subject and returns the
 * reply without its success and message fields. The server checks the
 * permission and the shape, so a route adds no rule of its own beyond the
 * structure of the request.
 *
 * Every route is a POST because the identity is a structure, not a path. Only the
 * key and clear routes write. The server decides who may write, so the routes
 * add no permission rule of their own.
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
    provider_code: z.string().trim().max(100).default(''),
    fields: z
        .array(z.strictObject({ name: word, text: word }))
        .max(20)
        .default([]),
});

const identityBody = z.strictObject({ identity });

const seriesListBody = z.strictObject({
    party_id: z.union([z.uuid(), z.literal('')]).default(''),
});

/** A node of the object: one coordinate for each declared axis, in the shape's order. */
const coordinates = z.array(word).min(1).max(8);

const keyPointBody = identityBody.extend({
    coordinates,
    // The server reads the text as a number and refuses what is not one.
    value: z.string().trim().min(1).max(40),
    change_reason_code: word,
    change_commentary: z.string().trim().min(1).max(2000),
});

const clearPointBody = identityBody.extend({ coordinates });

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
 * What one published set's read states about the curve the screen is on.
 *
 * The filters are the list's own: a party, the text a curve's name must hold,
 * whether the candidates and the ended sets are drawn, and a page. The request
 * ids let a screen that holds a page of approval requests read their subjects in
 * one call, which is what the queue does.
 */
const publishedListBody = z.strictObject({
    party_id: z.union([z.uuid(), z.literal('')]).default(''),
    search: z.string().trim().max(200).default(''),
    include_candidates: z.boolean().default(false),
    include_ended: z.boolean().default(false),
    request_id_one_of: z.array(z.uuid()).max(100).default([]),
    offset: z.int().min(0).default(0),
    limit: z.int().min(1).max(200).default(50),
});

/**
 * The last sets of one curve, with the points each holds.
 *
 * The sets are read beside their points because the snapshot read serves the
 * market at the caller's clock and a set built at an earlier as-of is not that.
 */
const publishedSetsBody = z.strictObject({
    series_id: z.uuid(),
    limit: z.int().min(1).max(20).default(5),
    include_candidates: z.boolean().default(true),
});

/** The most series one read of a party's catalogue asks for, which is the server's page. */
const SERIES_PAGE = 200;

/**
 * The most pages of series one read asks for.
 *
 * The read walks the catalogue a page at a time, so it stops on a page that is
 * short or empty and, failing both, on this count: a server that ignored the
 * offset would otherwise answer the same full page for ever and the read would
 * never return. Fifty pages are ten thousand series, more than any one party's
 * catalogue holds, so the cap bounds the work and never truncates a real list.
 */
const SERIES_PAGES = 50;

/**
 * The series a blueprint names, by the URI each is published under.
 *
 * A curve blueprint holds its output series as a soft reference, so the screen
 * that reads blueprints has to name each one, and the series table is the only
 * place the URI lives. Stated ids reach the server as its own one-of filter, so
 * a page of rows is one read of the rows asked for; no ids asks for every
 * series the session's party holds, which is the list a person chooses a
 * blueprint's output series from.
 */
const seriesByBody = z.strictObject({
    ids: z.array(anyUuid).max(SERIES_PAGE).default([]),
});

/**
 * The recipe a person tests, and the instant to test it at.
 *
 * The test writes nothing: it builds the curve in the market data service and
 * echoes the points back, so a refusal is the recipe's or the market's and not a
 * state the screen has to undo.
 */
const computeBody = z.strictObject({
    bootstrap_config_id: z.uuid(),
    as_of: instant,
});

/**
 * The act that takes a published set out of use.
 *
 * The reason is a change reason code the server validates, and the request is
 * the one that authorised the act when a decision stands behind it.
 */
const withdrawBody = z.strictObject({
    publish_id: z.uuid(),
    reason_code: z.string().trim().min(1).max(100),
    comment: z.string().trim().max(500).default(''),
    approval_request_id: z.union([z.uuid(), z.literal('')]).default(''),
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

    // The oresmd URI of each series a set of ids names, or of every series the
    // session's party holds when no id is stated. A blueprint names its output
    // series by id, and the screen that lists blueprints shows the URI the curve
    // is published under.
    server.post('/api/curves/series-by-id', async (request) => {
        const session = requireSession(request);
        const body = input(seriesByBody, request.body);
        const stated = body.ids.length > 0;
        const series: { id: string; uri: string }[] = [];
        for (let page = 0; page < SERIES_PAGES; page += 1) {
            const reply = await session.client.callAuthenticated(
                marketSeriesSubjects.list_market_series_request,
                {
                    offset: page * SERIES_PAGE,
                    limit: SERIES_PAGE,
                    order: NO_ORDER,
                    filter: {
                        id_one_of: stated ? body.ids : null,
                        provider_code: null,
                        provider_code_one_of: null,
                    },
                    as_of: null,
                },
                z.looseObject({
                    result: resultSchema,
                    market_series: z
                        .array(z.looseObject({ id: anyUuid, oresmd_uri: z.string() }))
                        .default([]),
                }),
            );
            if (reply.result.outcome !== 'ok') {
                throw refused(reply.result.message);
            }
            const rows = reply.market_series;
            series.push(...rows.map((row) => ({ id: row.id, uri: row.oresmd_uri })));
            // A stated set is one page. The party's catalogue is walked until a
            // page is empty, which is the only end a short page is not, and the
            // loop's own count is the third end: a server that answered a full
            // page every time would otherwise keep this answer from returning.
            if (stated || rows.length === 0 || rows.length < SERIES_PAGE) {
                break;
            }
        }
        return { series };
    });

    // Builds the recipe's curve at an instant and echoes its points. Nothing is
    // written and nothing is published; the check tab reads the answer.
    server.post('/api/curves/compute', async (request) => {
        const session = requireSession(request);
        const body = input(computeBody, request.body);
        const reply = await session.client.callAuthenticated(
            subjects.compute_curve_request,
            { bootstrap_config_id: body.bootstrap_config_id, as_of: body.as_of },
            z.looseObject({
                success: z.boolean(),
                message: z.string().default(''),
                points: z
                    .array(
                        z.looseObject({
                            point_id: z.string(),
                            date: z.string(),
                            discount_factor: z.number(),
                        }),
                    )
                    .default([]),
            }),
        );
        if (!reply.success) {
            throw refused(reply.message);
        }
        return {
            points: reply.points.map((point) => ({
                point_id: point.point_id,
                date: point.date,
                discount_factor: point.discount_factor,
            })),
        };
    });

    // The series the session's party can see, each as the identity the other reads
    // take. The body may be empty, which asks across the parties the session sees.
    server.post('/api/curves/series-list', async (request) => {
        const session = requireSession(request);
        const body = input(seriesListBody, request.body ?? {});
        return call(session, subjects.list_series_identities_request, body);
    });

    // Keys a manual point on one node. The point owns the node until it is cleared.
    server.post('/api/curves/manual-point', async (request) => {
        const session = requireSession(request);
        const body = input(keyPointBody, request.body);
        return call(session, subjects.key_manual_point_request, body);
    });

    // Clears the manual point on one node, so the feed value shows again.
    server.post('/api/curves/manual-point/clear', async (request) => {
        const session = requireSession(request);
        const body = input(clearPointBody, request.body);
        return call(session, subjects.clear_manual_point_request, body);
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

    // The tenant's published sets, with the state each is in. The read is the
    // list screen's whole page: the filters and the pager are the server's, so a
    // screen never filters a page it was given.
    server.post('/api/curves/published', async (request) => {
        const session = requireSession(request);
        const body = input(publishedListBody, request.body ?? {});
        return call(session, publishedCurves.list_published_curves_request, body);
    });

    // One curve's last published sets, with the pillars each holds. The overlay
    // and the comparison read this once.
    server.post('/api/curves/published-sets', async (request) => {
        const session = requireSession(request);
        const body = input(publishedSetsBody, request.body);
        return call(session, publishedCurves.get_published_curve_sets_request, body);
    });

    // Takes a published set out of use. The server decides who may, so the route
    // adds no rule of its own.
    server.post('/api/curves/published/withdraw', async (request) => {
        const session = requireSession(request);
        const body = input(withdrawBody, request.body);
        return call(session, publishedCurves.withdraw_curve_request, body);
    });
}

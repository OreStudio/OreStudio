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

import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_protocol';
import { subjects as subscriptionSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/subscription_protocol';
import { subjects as routeSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/route_protocol';
import { invalidRequest } from './errors.js';
import { input, readAll, refusal, write } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * What a party takes from the market data service: its subscriptions to the
 * providers, and the one source that serves each of them.
 *
 * The service owns the party of a subscription and of a route, so the routes
 * here name the provider and never the party. The catalogue read exists because
 * a route names a source by its id, and nothing else the browser can read
 * carries that id.
 */

/**
 * A subscription: one party's entitlement to one provider's feeds, with the
 * three rights the vendor licences name.
 */
const subscriptionSchema = z.looseObject({
    version: z.number(),
    id: z.string(),
    party_id: z.string(),
    provider_code: z.string(),
    display: z.boolean().default(false),
    non_display: z.boolean().default(false),
    derived: z.boolean().default(false),
    modified_by: z.string().default(''),
    performed_by: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
    recorded_at: z.string().default(''),
});

/**
 * A route: the one source that serves a party for a provider. The source is
 * named by its id, and its name and kind are read from the catalogue.
 */
const routeSchema = z.looseObject({
    version: z.number(),
    id: z.string(),
    party_id: z.string(),
    provider_code: z.string(),
    source_id: z.string(),
    modified_by: z.string().default(''),
    performed_by: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
    recorded_at: z.string().default(''),
});

/** One provider of the catalogue, named for the screen's selectors. */
const providerSchema = z.looseObject({
    code: z.string(),
    name: z.string().default(''),
    kind: z.string().default(''),
    description: z.string().default(''),
    display_order: z.number().default(0),
});

/** One source of the catalogue, with the provider it belongs to and its kind. */
const sourceSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    provider_code: z.string(),
    kind: z.string().default(''),
    is_default: z.boolean().default(false),
    description: z.string().default(''),
});

/** The rights a new subscription may carry. A subscription holds at least one. */
const rightsSchema = z.object({
    display: z.boolean().default(false),
    non_display: z.boolean().default(false),
    derived: z.boolean().default(false),
});

const newSubscriptionSchema = rightsSchema.extend({
    provider_code: z.string().trim().min(1).max(100),
});

const newRouteSchema = z.object({
    provider_code: z.string().trim().min(1).max(100),
    source_id: z.uuid(),
});

/** A removal names the row it removes and the reason it goes. */
const removalSchema = z.object({
    version: z.int().nonnegative(),
    reason_code: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000).default(''),
});

/**
 * The party the write belongs to: the one the session acts in. The service
 * states the party of a subscription from the session and never from the
 * browser, so this is only ever the session's own party.
 */
function sessionParty(session: LiveSession): string {
    const party = session.party?.id;
    if (party === undefined) {
        throw invalidRequest('Choose the party you act in before changing market data.');
    }
    return party;
}

/** The reason every new row states, which is the only one that applies to a create. */
const NEW_RECORD = 'system.new_record';

export function registerSubscriptionRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * Every subscription the session may read, with the party that holds it and
     * the rights it carries.
     */
    server.get('/api/marketdata/subscriptions', async (request) => {
        const session = requireSession(request);
        const rows = await readAll(
            session,
            subscriptionSubjects.list_subscriptions_request,
            'subscriptions',
            { filter: null, as_of: null },
        );
        return { subscriptions: rows.map((row) => subscriptionSchema.parse(row)) };
    });

    /** Every route the session may read: the source that serves each party and provider. */
    server.get('/api/marketdata/routes', async (request) => {
        const session = requireSession(request);
        const rows = await readAll(session, routeSubjects.list_routes_request, 'routes', {
            filter: null,
            as_of: null,
        });
        return { routes: rows.map((row) => routeSchema.parse(row)) };
    });

    /**
     * The providers and the sources, which are what a subscription and a route
     * choose from. A route names its source by id, so the screen cannot write one
     * without this read.
     */
    server.get('/api/marketdata/catalogue', async (request) => {
        const session = requireSession(request);
        const [providers, sources] = await Promise.all([
            readAll(session, providerSubjects.list_providers_request, 'providers', {
                filter: null,
                as_of: null,
            }),
            readAll(session, feedSubjects.list_feeds_request, 'feeds', {
                filter: null,
                as_of: null,
            }),
        ]);
        return {
            providers: providers
                .map((row) => providerSchema.parse(row))
                .sort((left, right) => left.code.localeCompare(right.code)),
            sources: sources.map((row) => {
                const feed = z
                    .looseObject({ id: z.string(), name: z.string(), publisher_code: z.string() })
                    .parse(row);
                return {
                    id: feed.id,
                    name: feed.name,
                    provider_code: feed.publisher_code,
                    kind: 'feed',
                };
            }),
        };
    });

    /** Adds a subscription for the party the session acts in. */
    server.post('/api/marketdata/subscriptions', async (request) => {
        const session = requireSession(request);
        const party = sessionParty(session);
        const parsed = input(newSubscriptionSchema, request.body);
        if (!parsed.display && !parsed.non_display && !parsed.derived) {
            throw invalidRequest('A subscription needs at least one right.');
        }
        const result = await write(session, subscriptionSubjects.put_subscription_request, {
            change: {
                write: {
                    id: randomUUID(),
                    party_id: party,
                    provider_code: parsed.provider_code,
                    display: parsed.display,
                    non_display: parsed.non_display,
                    derived: parsed.derived,
                },
                precondition: { kind: 'must_not_exist', version: null },
            },
            intent: { reason_code: NEW_RECORD, commentary: '' },
        });
        if (result.outcome !== 'ok') {
            throw refusal(result);
        }
        return { provider_code: parsed.provider_code };
    });

    /** Removes the session party's subscription to one provider, at the version it was read at. */
    server.delete('/api/marketdata/subscriptions/:provider', async (request) => {
        const session = requireSession(request);
        sessionParty(session);
        const provider = input(
            z.string().trim().min(1).max(100),
            (request.params as { provider: string }).provider,
        );
        const parsed = input(removalSchema, request.body);
        const result = await write(session, subscriptionSubjects.delete_subscription_request, {
            removal: {
                key: { provider_code: provider },
                precondition: { kind: 'must_match_version', version: parsed.version },
            },
            intent: { reason_code: parsed.reason_code, commentary: parsed.commentary },
        });
        if (result.outcome !== 'ok') {
            throw refusal(result);
        }
        return { provider_code: provider };
    });

    /** Adds a route for the session party, choosing the source that serves a provider. */
    server.post('/api/marketdata/routes', async (request) => {
        const session = requireSession(request);
        const party = sessionParty(session);
        const parsed = input(newRouteSchema, request.body);
        const result = await write(session, routeSubjects.put_route_request, {
            change: {
                write: {
                    id: randomUUID(),
                    party_id: party,
                    provider_code: parsed.provider_code,
                    source_id: parsed.source_id,
                },
                precondition: { kind: 'must_not_exist', version: null },
            },
            intent: { reason_code: NEW_RECORD, commentary: '' },
        });
        if (result.outcome !== 'ok') {
            throw refusal(result);
        }
        return { provider_code: parsed.provider_code };
    });

    /** Removes the session party's route for one provider, at the version it was read at. */
    server.delete('/api/marketdata/routes/:provider', async (request) => {
        const session = requireSession(request);
        sessionParty(session);
        const provider = input(
            z.string().trim().min(1).max(100),
            (request.params as { provider: string }).provider,
        );
        const parsed = input(removalSchema, request.body);
        const result = await write(session, routeSubjects.delete_route_request, {
            removal: {
                key: { provider_code: provider },
                precondition: { kind: 'must_match_version', version: parsed.version },
            },
            intent: { reason_code: parsed.reason_code, commentary: parsed.commentary },
        });
        if (result.outcome !== 'ok') {
            throw refusal(result);
        }
        return { provider_code: provider };
    });
}

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
    DEFAULT_OBSERVATIONS,
    DEFAULT_SERIES_WINDOW,
    MAX_OBSERVATIONS,
    seriesObservationsSchema,
    seriesWindowSchema,
    type SeriesObservations,
} from '@ores/contracts';
import {
    subjects,
    type MarketObservationsFilter,
} from '@ores/wire-protocol/generated/marketdata/protocol/market_observation_protocol';
import { NO_ORDER, idSchema, input, refusal, resultSchema } from './refdata-calls.js';
import { observedSince } from './series-window.js';
import type { LiveSession } from './sessions.js';

/**
 * The values of one series over time, for the series page: the observations of
 * the window asked for, oldest first.
 *
 * The route forwards to the server's list of a series' observations, which
 * orders them newest first and checks the permission and the party. The window
 * is turned into the earliest observation the read takes here rather than in
 * the browser, because the deployment's clock is the one the observations were
 * stamped against. The count stays the cap on the answer, so a window wider
 * than the cap returns its newest observations. A request for more than the cap
 * is clamped, not refused, because the page asks for what it can draw and the
 * answer says how many the window holds.
 */

const querySchema = z.object({
    limit: z.coerce.number().pipe(z.int().min(1)).default(DEFAULT_OBSERVATIONS),
    window: seriesWindowSchema.default(DEFAULT_SERIES_WINDOW),
});

const replySchema = z.looseObject({
    result: resultSchema,
    market_observations: z
        .array(
            z.looseObject({
                observation_datetime: z.string(),
                value: z.string(),
                source: z.string().default(''),
            }),
        )
        .default([]),
    total: z.number().optional(),
});

export function registerSeriesRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/series/:id/observations', async (request): Promise<SeriesObservations> => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        const { limit, window } = input(querySchema, request.query);
        const since = observedSince(window, Date.now());
        const reply = await session.client.callAuthenticated(
            subjects.list_by_series_id_market_observations_request,
            {
                series_id: id,
                scope: 'direct',
                offset: 0,
                limit: Math.min(limit, MAX_OBSERVATIONS),
                order: NO_ORDER,
                filter: {
                    series_id: null,
                    observation_datetime: null,
                    id_one_of: null,
                    series_id_one_of: null,
                    observation_datetime_one_of: null,
                    observation_datetime_from: since,
                    observation_datetime_to: null,
                } satisfies MarketObservationsFilter,
            },
            replySchema,
        );
        if (reply.result.outcome !== 'ok') throw refusal(reply.result);
        // The server answers newest first. The chart reads left to right in time.
        const observations = reply.market_observations
            .map((row) => ({
                at: row.observation_datetime,
                value: row.value,
                source: row.source,
            }))
            .reverse();
        return seriesObservationsSchema.parse({
            observations,
            total: reply.total ?? observations.length,
        });
    });
}

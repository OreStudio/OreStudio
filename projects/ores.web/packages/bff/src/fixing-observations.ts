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
 */
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects } from '@ores/wire-protocol/generated/marketdata/protocol/fixing_read_operations_protocol';
import { input, refusal, resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * One fixing series as the system view lists it.
 *
 * The service answers the whole list in one read: the counts and the latest day
 * are the database's aggregate, so a screen that shows a line per series never
 * carries the values of all of them. A series that holds no day is one of the
 * rows, with a count of zero and no latest day.
 */
const summarySchema = z.looseObject({
    series_id: z.string(),
    oresmd_uri: z.string(),
    asset_class: z.string(),
    index_label: z.string(),
    source: z.string(),
    provider_code: z.string(),
    latest_value: z.string(),
    latest_date: z.string().nullable().default(null),
    days: z.number().int().default(0),
});

/**
 * Every FX and IR fixing series the platform states, as one list.
 *
 * This is the system view of the fixing observations: what the sources
 * published, read only. It is not the party's fixings manager, which decides
 * what a party's trades fix on.
 */
export function registerFixingObservationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/marketdata/fixing-observations', async (request) => {
        const session = requireSession(request);
        const filters = input(
            z.object({
                asset_class: z.string().default(''),
                currency: z.string().default(''),
                source: z.string().default(''),
            }),
            request.query,
        );
        const reply = await session.client.callAuthenticated(
            subjects.list_fixing_series_summaries_request,
            {
                asset_class: filters.asset_class,
                currency: filters.currency,
                source: filters.source,
            },
            z.looseObject({ result: resultSchema, series: z.array(summarySchema).default([]) }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        return { series: reply.series };
    });
}

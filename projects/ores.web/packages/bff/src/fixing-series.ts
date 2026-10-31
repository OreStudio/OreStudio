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
import { subjects as seriesSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_series_protocol';
import { subjects as fixingSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_fixing_protocol';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_protocol';
import { subjects as scopeSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_scope_protocol';
import { subjects as sessionSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/session_protocol';
import { subjects as supplierSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/supplier_protocol';
import { input, readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * One item in the store, named by its oresmd URI.
 *
 * The URI is the item's identity, so it names the series the store holds and
 * the fixings written against it. The feeds and their sessions are provenance:
 * they say how the values reached the store, not what the item is.
 */
export function registerFixingSeriesRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/marketdata/fixing-series', async (request) => {
        const session = requireSession(request);
        const { uri } = input(z.object({ uri: z.string().min(1) }), request.query);
        const empty = { filter: null, as_of: null };
        const [seriesRows, providers, feeds, scopes, sessions, suppliers] = await Promise.all([
            readAll(session, seriesSubjects.list_market_series_request, 'market_series', empty),
            readAll(session, providerSubjects.list_providers_request, 'providers', empty),
            readAll(session, feedSubjects.list_feeds_request, 'feeds', empty),
            readAll(session, scopeSubjects.list_feed_scopes_request, 'scopes', empty),
            readAll(session, sessionSubjects.list_sessions_request, 'sessions', empty),
            readAll(session, supplierSubjects.list_suppliers_request, 'suppliers', empty),
        ]);
        const series = seriesRows
            .map((row) =>
                z
                    .looseObject({
                        id: z.string(),
                        oresmd_uri: z.string(),
                        provider_code: z.string(),
                        version: z.number().int().default(0),
                    })
                    .parse(row),
            )
            .find((row) => row.oresmd_uri === uri);
        if (series === undefined) {
            return { series: null };
        }
        const fixingRows = await readAll(
            session,
            fixingSubjects.list_market_fixings_request,
            'market_fixings',
            {
                filter: { series_id: null, id_one_of: null, series_id_one_of: [series.id] },
                as_of: null,
            },
        );
        const values = fixingRows
            .map((row) =>
                z
                    .looseObject({
                        fixing_date: z.string(),
                        value: z.string(),
                    })
                    .parse(row),
            )
            .map((row) => ({ date: row.fixing_date, value: row.value }))
            .sort((left, right) => left.date.localeCompare(right.date));
        const publisher = providers
            .map((row) => z.looseObject({ code: z.string(), name: z.string() }).parse(row))
            .find((row) => row.code === series.provider_code);
        const sessionByCode = new Map(
            sessions
                .map((row) =>
                    z
                        .looseObject({
                            code: z.string(),
                            supplier_code: z.string().default(''),
                            environment: z.string().default(''),
                        })
                        .parse(row),
                )
                .map((row) => [row.code, row]),
        );
        const supplierByCode = new Map(
            suppliers
                .map((row) => z.looseObject({ code: z.string(), name: z.string() }).parse(row))
                .map((row) => [row.code, row]),
        );
        const scopedFeeds = new Set(
            scopes
                .map((row) => z.looseObject({ feed_id: z.string(), item: z.string() }).parse(row))
                .filter((row) => row.item === uri)
                .map((row) => row.feed_id),
        );
        const provenance = feeds
            .map((row) =>
                z
                    .looseObject({
                        id: z.string(),
                        name: z.string(),
                        publisher_code: z.string(),
                        session_code: z.string(),
                        service: z.string().default(''),
                    })
                    .parse(row),
            )
            .filter((row) => scopedFeeds.has(row.id))
            .map((row) => {
                const sessionRow = sessionByCode.get(row.session_code);
                return {
                    id: row.id,
                    name: row.name,
                    service: row.service,
                    session_code: row.session_code,
                    environment: sessionRow?.environment ?? '',
                    supplier_name: supplierByCode.get(sessionRow?.supplier_code ?? '')?.name ?? '',
                };
            });
        return {
            series: {
                id: series.id,
                oresmd_uri: series.oresmd_uri,
                provider_code: series.provider_code,
                version: series.version,
            },
            publisher:
                publisher === undefined ? null : { code: publisher.code, name: publisher.name },
            values: {
                first: values[0]?.date ?? '',
                last: values.at(-1)?.date ?? '',
                count: values.length,
                rows: values,
            },
            feeds: provenance,
        };
    });
}

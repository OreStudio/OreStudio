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
import { subjects as fixingSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_fixing_protocol';
import { subjects as seriesSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_series_protocol';
import { subjects as sourceSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/market_data_source_protocol';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as termsSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_terms_operations_protocol';
import { invalidRequest } from './errors.js';
import { input, readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The official rates: the providers that publish them, their terms, their sources and the
 * fixings the official rates adapter stores. A provider is official when it states a terms
 * page, since only such a provider is read through the adapter.
 */

const officialProviderSchema = z.looseObject({
    code: z.string(),
    name: z.string(),
    kind: z.string().default(''),
    description: z.string().default(''),
    terms_url: z.string().default(''),
    terms_credit: z.string().default(''),
    allows_storage: z.boolean().default(false),
    terms_accepted_digest: z.string().default(''),
    terms_accepted_by: z.string().default(''),
    terms_accepted_at: z.string().nullable().default(null),
});

const sourceSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    provider_code: z.string(),
    kind: z.string().default(''),
});

const seriesSchema = z.looseObject({
    id: z.string(),
    oresmd_uri: z.string(),
    provider_code: z.string(),
});

const fixingSchema = z.looseObject({
    series_id: z.string(),
    fixing_date: z.string(),
    value: z.string(),
});

/** The most series one read of fixings may name, which bounds a reply to a page of a screen. */
const MOST_SERIES = 12;

/**
 * The pair an FX fixing item names: its subject and its currency. The item is
 * =oresmd://fx/<BASE>?type=fixing&index=fx&source=<KEY>&ccy=<QUOTE>=.
 */
export function fixingPair(
    uri: string,
): { readonly base: string; readonly quote: string; readonly source: string } | null {
    const match = /^oresmd:\/\/fx\/([A-Z]{3})\?type=fixing&index=fx&source=([A-Z0-9]+)&ccy=([A-Z]{3})$/.exec(
        uri,
    );
    if (match === null) {
        return null;
    }
    return { base: match[1] ?? '', source: match[2] ?? '', quote: match[3] ?? '' };
}

export function registerOfficialRatesRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Every official provider with its terms, its sources and the fixing series it fills. */
    server.get('/api/marketdata/official', async (request) => {
        const session = requireSession(request);
        const providers = (
            await readAll(session, providerSubjects.list_providers_request, 'providers', {
                filter: null,
                as_of: null,
            })
        )
            .map((row) => officialProviderSchema.parse(row))
            .filter((provider) => provider.terms_url !== '');
        const codes = providers.map((provider) => provider.code);
        if (codes.length === 0) {
            return { providers: [] };
        }
        const [sources, series] = await Promise.all([
            readAll(session, sourceSubjects.list_market_data_sources_request, 'sources', {
                filter: null,
                as_of: null,
            }),
            readAll(session, seriesSubjects.list_market_series_request, 'market_series', {
                filter: { provider_code: null, id_one_of: null, provider_code_one_of: codes },
                as_of: null,
            }),
        ]);
        const parsedSources = sources.map((row) => sourceSchema.parse(row));
        const parsedSeries = series
            .map((row) => seriesSchema.parse(row))
            .flatMap((row) => {
                const pair = fixingPair(row.oresmd_uri);
                return pair === null ? [] : [{ ...row, ...pair }];
            });
        return {
            providers: providers
                .map((provider) => ({
                    code: provider.code,
                    name: provider.name,
                    description: provider.description,
                    terms_url: provider.terms_url,
                    terms_credit: provider.terms_credit,
                    allows_storage: provider.allows_storage,
                    terms_accepted: provider.terms_accepted_digest !== '',
                    terms_accepted_by: provider.terms_accepted_by,
                    terms_accepted_at: provider.terms_accepted_at,
                    sources: parsedSources
                        .filter((source) => source.provider_code === provider.code)
                        .map((source) => ({ id: source.id, name: source.name, kind: source.kind })),
                    series: parsedSeries
                        .filter((row) => row.provider_code === provider.code)
                        .map((row) => ({
                            id: row.id,
                            oresmd_uri: row.oresmd_uri,
                            base: row.base,
                            quote: row.quote,
                        }))
                        .sort((left, right) => left.quote.localeCompare(right.quote)),
                }))
                .sort((left, right) => left.name.localeCompare(right.name)),
        };
    });

    /** The fixings of up to a dozen series, oldest first, by series. */
    server.get('/api/marketdata/official/fixings', async (request) => {
        const session = requireSession(request);
        const query = input(z.object({ series: z.string().min(1) }), request.query);
        const ids = [...new Set(query.series.split(',').filter((id) => id !== ''))];
        if (ids.length > MOST_SERIES) {
            throw invalidRequest(`A read names at most ${String(MOST_SERIES)} series.`);
        }
        const rows = await readAll(session, fixingSubjects.list_market_fixings_request, 'market_fixings', {
            filter: { series_id: null, id_one_of: null, series_id_one_of: ids },
            as_of: null,
        });
        const bySeries: Record<string, { date: string; value: string }[]> = {};
        for (const id of ids) {
            bySeries[id] = [];
        }
        for (const row of rows.map((r) => fixingSchema.parse(r))) {
            bySeries[row.series_id]?.push({ date: row.fixing_date, value: row.value });
        }
        for (const list of Object.values(bySeries)) {
            list.sort((left, right) => left.date.localeCompare(right.date));
        }
        return { fixings: bySeries };
    });

    /** Accepts a provider's terms as they stand; the service records who and when. */
    server.post('/api/marketdata/official/:code/accept-terms', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: z.string().min(1) }), request.params);
        const reply = await session.client.callAuthenticated(
            termsSubjects.accept_provider_terms_request,
            { provider_code: code },
            z.looseObject({
                success: z.boolean().default(false),
                message: z.string().default(''),
                accepted_by: z.string().default(''),
                accepted_at: z.string().nullable().default(null),
            }),
        );
        if (!reply.success) {
            throw invalidRequest(reply.message);
        }
        return {
            accepted_by: reply.accepted_by,
            accepted_at: reply.accepted_at,
            message: reply.message,
        };
    });
}

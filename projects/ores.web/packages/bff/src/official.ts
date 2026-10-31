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
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_protocol';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as termsSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_terms_operations_protocol';
import { invalidRequest } from './errors.js';
import { input, readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The official rates: the publishers that publish them, their terms, their feeds and the
 * fixings the official rates adapter stores. A publisher is official when it states a terms
 * page, since only such a publisher is read through the adapter.
 *
 * A feed is one publisher's data, on one service, through one session, with the publisher's
 * own key. Each official provider is grouped from its feeds, and each fixing series is
 * matched to the feed whose =provider_key= its URI names: the URI carries
 * =source=<KEY>=, and the key is the publisher's own name for the data it serves.
 */

const officialProviderSchema = z.looseObject({
    version: z.number().int().default(0),
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

/** One feed of the catalogue: the publisher's data, its session, its service and its own key. */
const feedSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    publisher_code: z.string(),
    session_code: z.string().default(''),
    service: z.string().default(''),
    provider_key: z.string().default(''),
    is_default: z.boolean().default(false),
    description: z.string().default(''),
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
    const match =
        /^oresmd:\/\/fx\/([A-Z]{3})\?type=fixing&index=fx&source=([A-Z0-9]+)&ccy=([A-Z]{3})$/.exec(
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
    /** Every official provider with its terms, its feeds and the fixing series it fills. */
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
        const [feeds, series] = await Promise.all([
            readAll(session, feedSubjects.list_feeds_request, 'feeds', {
                filter: null,
                as_of: null,
            }),
            readAll(session, seriesSubjects.list_market_series_request, 'market_series', {
                filter: { provider_code: null, id_one_of: null, provider_code_one_of: codes },
                as_of: null,
            }),
        ]);
        const parsedFeeds = feeds.map((row) => feedSchema.parse(row));
        const parsedSeries = series
            .map((row) => seriesSchema.parse(row))
            .flatMap((row) => {
                const pair = fixingPair(row.oresmd_uri);
                return pair === null ? [] : [{ ...row, ...pair }];
            });
        // The screen takes a provider's first series as its base; the catalogue gives every
        // series of a provider one base, and a catalogue that does not is said here.
        for (const code of codes) {
            const bases = new Set(
                parsedSeries.filter((s) => s.provider_code === code).map((s) => s.base),
            );
            if (bases.size > 1) {
                request.log.warn(
                    { provider: code, bases: [...bases] },
                    'an official provider has more than one base',
                );
            }
        }
        // A series is read by the feed whose own key its URI names, so a series no feed of its
        // publisher names cannot arrive; the catalogue that holds one is said here.
        for (const row of parsedSeries) {
            const read = parsedFeeds.some(
                (feed) => feed.publisher_code === row.provider_code && feed.provider_key === row.source,
            );
            if (!read) {
                request.log.warn(
                    { provider: row.provider_code, provider_key: row.source },
                    'no feed of the official provider names the key its fixing URI states',
                );
            }
        }
        return {
            providers: providers
                .map((provider) => ({
                    code: provider.code,
                    version: provider.version,
                    name: provider.name,
                    description: provider.description,
                    terms_url: provider.terms_url,
                    terms_credit: provider.terms_credit,
                    allows_storage: provider.allows_storage,
                    terms_accepted: provider.terms_accepted_digest !== '',
                    terms_accepted_by: provider.terms_accepted_by,
                    terms_accepted_at: provider.terms_accepted_at,
                    feeds: parsedFeeds
                        .filter((feed) => feed.publisher_code === provider.code)
                        .map((feed) => ({
                            id: feed.id,
                            name: feed.name,
                            publisher_code: feed.publisher_code,
                            session_code: feed.session_code,
                            service: feed.service,
                            provider_key: feed.provider_key,
                            is_default: feed.is_default,
                            description: feed.description,
                        }))
                        .sort(
                            (left, right) =>
                                Number(right.is_default) - Number(left.is_default) ||
                                left.name.localeCompare(right.name),
                        ),
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
        if (!ids.every((id) => z.uuid().safeParse(id).success)) {
            throw invalidRequest('A series is named by its id.');
        }
        const rows = await readAll(
            session,
            fixingSubjects.list_market_fixings_request,
            'market_fixings',
            {
                filter: { series_id: null, id_one_of: null, series_id_one_of: ids },
                as_of: null,
            },
        );
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

    /**
     * Accepts a provider's terms at the version the person read; the service refuses when
     * the provider changed since, and records who and when.
     */
    server.post('/api/marketdata/official/:code/accept-terms', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: z.string().min(1) }), request.params);
        const { version } = input(z.object({ version: z.number().int().min(1) }), request.body);
        const reply = await session.client.callAuthenticated(
            termsSubjects.accept_provider_terms_request,
            { provider_code: code, provider_version: version },
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

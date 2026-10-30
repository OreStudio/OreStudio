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
import { subjects as scopeSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_scope_protocol';
import { subjects as sessionSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/session_protocol';
import { subjects as supplierSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/supplier_protocol';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as termsSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_terms_operations_protocol';
import { invalidRequest } from './errors.js';
import { input, readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The public rates: the publishers that release their rates to the public,
 * their terms, the feeds that carry them, and the fixings in the store.
 *
 * A publisher is public when it states a terms page, since only such a
 * publisher is read through the official adapter. A feed is one publisher on
 * one service through one session; the route names the feed a party reads.
 */

const publicProviderSchema = z.looseObject({
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

const feedSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    publisher_code: z.string(),
    session_code: z.string(),
    service: z.string().default(''),
    provider_key: z.string().default(''),
    is_default: z.boolean().default(false),
});

const scopeSchema = z.looseObject({
    feed_id: z.string(),
    asset_class: z.string(),
});

const sessionSchema = z.looseObject({
    code: z.string(),
    supplier_code: z.string().default(''),
});

const supplierSchema = z.looseObject({
    code: z.string(),
    name: z.string().default(''),
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

/** One series' coverage: the first and last date it holds and how many values it holds. */
interface SeriesCoverage {
    readonly first: string;
    readonly last: string;
    readonly count: number;
}

function coverageOf(rows: readonly { date: string }[]): SeriesCoverage | undefined {
    if (rows.length === 0) {
        return undefined;
    }
    const dates = rows.map((row) => row.date).sort();
    return { first: dates[0] ?? '', last: dates[dates.length - 1] ?? '', count: dates.length };
}

/** Every fixing of @p ids, read a batch at a time. */
async function readFixings(
    session: LiveSession,
    ids: readonly string[],
): Promise<readonly z.infer<typeof fixingSchema>[]> {
    const out: z.infer<typeof fixingSchema>[] = [];
    for (let at = 0; at < ids.length; at += MOST_SERIES) {
        const batch = ids.slice(at, at + MOST_SERIES);
        const rows = await readAll(
            session,
            fixingSubjects.list_market_fixings_request,
            'market_fixings',
            {
                filter: { series_id: null, id_one_of: null, series_id_one_of: batch },
                as_of: null,
            },
        );
        out.push(...rows.map((row) => fixingSchema.parse(row)));
    }
    return out;
}

/** The public publishers, their feeds and their series, as the screens read them. */
async function publicCatalogue(session: LiveSession): Promise<{
    readonly providers: readonly z.infer<typeof publicProviderSchema>[];
    readonly feeds: readonly z.infer<typeof feedSchema>[];
    readonly scopes: readonly z.infer<typeof scopeSchema>[];
    readonly sessions: readonly z.infer<typeof sessionSchema>[];
    readonly suppliers: readonly z.infer<typeof supplierSchema>[];
    readonly series: readonly z.infer<typeof seriesSchema>[];
    readonly fixings: ReadonlyMap<string, { readonly date: string; readonly value: string }[]>;
}> {
    const empty = { filter: null, as_of: null };
    const providers = (
        await readAll(session, providerSubjects.list_providers_request, 'providers', empty)
    )
        .map((row) => publicProviderSchema.parse(row))
        .filter((provider) => provider.terms_url !== '');
    const codes = providers.map((provider) => provider.code);
    if (codes.length === 0) {
        return {
            providers: [],
            feeds: [],
            scopes: [],
            sessions: [],
            suppliers: [],
            series: [],
            fixings: new Map(),
        };
    }
    const [feeds, scopes, sessions, suppliers, series] = await Promise.all([
        readAll(session, feedSubjects.list_feeds_request, 'feeds', empty),
        readAll(session, scopeSubjects.list_feed_scopes_request, 'scopes', empty),
        readAll(session, sessionSubjects.list_sessions_request, 'sessions', empty),
        readAll(session, supplierSubjects.list_suppliers_request, 'suppliers', empty),
        readAll(session, seriesSubjects.list_market_series_request, 'market_series', {
            filter: { provider_code: null, id_one_of: null, provider_code_one_of: codes },
            as_of: null,
        }),
    ]);
    const parsedSeries = series.map((row) => seriesSchema.parse(row));
    const rows = await readFixings(
        session,
        parsedSeries.map((row) => row.id),
    );
    const fixings = new Map<string, { date: string; value: string }[]>();
    for (const row of rows) {
        const list = fixings.get(row.series_id) ?? [];
        list.push({ date: row.fixing_date, value: row.value });
        fixings.set(row.series_id, list);
    }
    for (const list of fixings.values()) {
        list.sort((left, right) => left.date.localeCompare(right.date));
    }
    return {
        providers,
        feeds: feeds.map((row) => feedSchema.parse(row)),
        scopes: scopes.map((row) => scopeSchema.parse(row)),
        sessions: sessions.map((row) => sessionSchema.parse(row)),
        suppliers: suppliers.map((row) => supplierSchema.parse(row)),
        series: parsedSeries,
        fixings,
    };
}

/** A CSV cell, quoted when the value holds a comma or a quote. */
function cell(value: string): string {
    return /[",\n]/.test(value) ? `"${value.replaceAll('"', '""')}"` : value;
}

export function registerPublicRatesRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Every public publisher with its terms, its feeds and the fixing series it fills. */
    server.get('/api/marketdata/public-rates', async (request) => {
        const session = requireSession(request);
        const catalogue = await publicCatalogue(session);
        const sessionById = new Map(catalogue.sessions.map((row) => [row.code, row]));
        const supplierByCode = new Map(catalogue.suppliers.map((row) => [row.code, row]));
        const baseByProvider = new Map<string, Set<string>>();
        for (const row of catalogue.series) {
            const pair = fixingPair(row.oresmd_uri);
            if (pair !== null) {
                const bases = baseByProvider.get(row.provider_code) ?? new Set<string>();
                bases.add(pair.base);
                baseByProvider.set(row.provider_code, bases);
            }
        }
        for (const [provider, bases] of baseByProvider) {
            if (bases.size > 1) {
                request.log.warn(
                    { provider, bases: [...bases] },
                    'a public publisher has more than one base',
                );
            }
        }
        return {
            providers: catalogue.providers
                .map((provider) => {
                    const feeds = catalogue.feeds.filter(
                        (feed) => feed.publisher_code === provider.code,
                    );
                    return {
                        code: provider.code,
                        version: provider.version,
                        name: provider.name,
                        description: provider.description,
                        kind: provider.kind,
                        terms_url: provider.terms_url,
                        terms_credit: provider.terms_credit,
                        allows_storage: provider.allows_storage,
                        terms_accepted: provider.terms_accepted_digest !== '',
                        terms_accepted_by: provider.terms_accepted_by,
                        terms_accepted_at: provider.terms_accepted_at,
                        feeds: feeds.map((feed) => {
                            const session = sessionById.get(feed.session_code);
                            return {
                                id: feed.id,
                                name: feed.name,
                                service: feed.service,
                                provider_key: feed.provider_key,
                                is_default: feed.is_default,
                                session_code: feed.session_code,
                                supplier_code: session?.supplier_code ?? '',
                                supplier_name:
                                    supplierByCode.get(session?.supplier_code ?? '')?.name ?? '',
                                asset_classes: [
                                    ...new Set(
                                        catalogue.scopes
                                            .filter((scope) => scope.feed_id === feed.id)
                                            .map((scope) => scope.asset_class),
                                    ),
                                ].sort(),
                            };
                        }),
                        series: catalogue.series
                            .filter((row) => row.provider_code === provider.code)
                            .flatMap((row) => {
                                const pair = fixingPair(row.oresmd_uri);
                                return pair === null
                                    ? []
                                    : [
                                          {
                                              id: row.id,
                                              oresmd_uri: row.oresmd_uri,
                                              base: pair.base,
                                              quote: pair.quote,
                                          },
                                      ];
                            })
                            .sort((left, right) => left.quote.localeCompare(right.quote)),
                    };
                })
                .sort((left, right) => left.name.localeCompare(right.name)),
        };
    });

    /**
     * The coverage of every series a public publisher fills: its first and last
     * date and how many values it holds. The list reads this to say whether a
     * publisher is arriving, and the availability tab to say how far back.
     */
    server.get('/api/marketdata/public-rates/coverage', async (request) => {
        const session = requireSession(request);
        const catalogue = await publicCatalogue(session);
        const series: Record<string, SeriesCoverage> = {};
        for (const [id, rows] of catalogue.fixings) {
            const coverage = coverageOf(rows);
            if (coverage !== undefined) {
                series[id] = coverage;
            }
        }
        return { series };
    });

    /** The fixings of up to a dozen series, oldest first, by series. */
    server.get('/api/marketdata/public-rates/fixings', async (request) => {
        const session = requireSession(request);
        const query = input(z.object({ series: z.string().min(1) }), request.query);
        const ids = [...new Set(query.series.split(',').filter((id) => id !== ''))];
        if (ids.length > MOST_SERIES) {
            throw invalidRequest(`A read names at most ${String(MOST_SERIES)} series.`);
        }
        if (!ids.every((id) => z.uuid().safeParse(id).success)) {
            throw invalidRequest('A series is named by its id.');
        }
        const rows = await readFixings(session, ids);
        const bySeries: Record<string, { date: string; value: string }[]> = {};
        for (const id of ids) {
            bySeries[id] = [];
        }
        for (const row of rows) {
            bySeries[row.series_id]?.push({ date: row.fixing_date, value: row.value });
        }
        for (const list of Object.values(bySeries)) {
            list.sort((left, right) => left.date.localeCompare(right.date));
        }
        return { fixings: bySeries };
    });

    /**
     * The historical fixings of one public publisher as a file, for a date
     * range and, when given, one item. The publisher's series are read, the
     * rows outside the range are dropped, and the file keeps the value as the
     * store wrote it.
     */
    server.get('/api/marketdata/public-rates/fixings.csv', async (request, reply) => {
        const session = requireSession(request);
        const query = input(
            z.object({
                publisher: z.string().min(1),
                from: z.iso.date().optional(),
                to: z.iso.date().optional(),
                item: z.string().optional(),
            }),
            request.query,
        );
        const catalogue = await publicCatalogue(session);
        const provider = catalogue.providers.find((row) => row.code === query.publisher);
        if (provider === undefined) {
            throw invalidRequest(`No public publisher is named ${query.publisher}.`);
        }
        const rows: string[] = ['publisher,date,pair,item,value'];
        let written = 0;
        for (const series of catalogue.series.filter(
            (row) => row.provider_code === provider.code,
        )) {
            if (query.item !== undefined && query.item !== series.oresmd_uri) {
                continue;
            }
            const pair = fixingPair(series.oresmd_uri);
            const name = pair === null ? '' : `${pair.base}/${pair.quote}`;
            for (const fixing of catalogue.fixings.get(series.id) ?? []) {
                if (query.from !== undefined && fixing.date < query.from) continue;
                if (query.to !== undefined && fixing.date > query.to) continue;
                rows.push(
                    [
                        cell(provider.code),
                        cell(fixing.date),
                        cell(name),
                        cell(series.oresmd_uri),
                        cell(fixing.value),
                    ].join(','),
                );
                written += 1;
            }
        }
        const suffix = [query.from ?? '', query.to ?? ''].filter((part) => part !== '').join('_');
        const filename = `${provider.code}-fixings${suffix === '' ? '' : `-${suffix}`}.csv`;
        return reply
            .header('content-type', 'text/csv; charset=utf-8')
            .header('content-disposition', `attachment; filename="${filename}"`)
            .header('x-fixing-rows', String(written))
            .send(`${rows.join('\n')}\n`);
    });

    /**
     * Accepts a provider's terms at the version the person read; the service refuses when
     * the provider changed since, and records who and when.
     */
    server.post('/api/marketdata/public-rates/:code/accept-terms', async (request) => {
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

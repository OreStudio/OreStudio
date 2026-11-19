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
import { subjects } from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure } from './errors.js';
import { resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The quotes view's routes, as the C++ server answers them, kept here until the BFF
 * goes: the feeds the caller may watch, the board's quotes of those feeds, and one
 * point of a provider's series over a span.
 *
 * The board holds the latest quote of each datum on every feed, from one
 * subscription to every feed subject. It holds no entitlement and no tenant: a feed
 * id is unique in the installation and its ticks are the same for every party that
 * may watch it, so every read asks the market data service which feeds the caller
 * may watch for display, and the board answers those alone.
 */

/** The subject every feed publishes its ticks under. */
export const MARKET_TICK_BASE = 'marketdata.v1.ops.market_tick';

/** How many quotes the board keeps; a datum first heard beyond it is not kept. */
export const BOARD_CAPACITY = 200_000;

/** The most quotes one read of the board returns. */
export const MOST_QUOTES = 20_000;

export const quoteTickSchema = z.object({
    value: z.string(),
    observation_time: z.string(),
    oresmd_uri: z.string().default(''),
    source: z.string().default(''),
});
export type QuoteTick = z.infer<typeof quoteTickSchema>;

/** Listens on a subject for ticks, each with the subject it came on, and returns the way to stop. */
export type QuoteSubscriber = (
    subject: string,
    onTick: (tick: QuoteTick, subject: string) => void,
) => () => void;

/** One quote as the board holds it, before the labels its URI gives. */
export interface BoardQuote {
    readonly provider: string;
    readonly feed: string;
    readonly source: string;
    readonly uri: string;
    readonly value: string;
    readonly previous: string;
    readonly at: string;
    readonly sequence: number;
}

/** A subject token as the server writes it: every byte that is not plain is %XX. */
function tokenText(token: string): string | undefined {
    try {
        return decodeURIComponent(token);
    } catch {
        return undefined;
    }
}

/** The provider and the feed a feed subject names, or nothing for a subject that names none. */
export function feedOf(subject: string): { provider: string; feed: string } | undefined {
    const base = `${MARKET_TICK_BASE}.`;
    const at = subject.indexOf(base);
    if (at < 0) return undefined;
    const [providerToken, feed] = subject.slice(at + base.length).split('.');
    if (providerToken === undefined || feed === undefined || feed === '') return undefined;
    const provider = tokenText(providerToken);
    return provider === undefined || provider === '' ? undefined : { provider, feed };
}

export class QuoteBoard {
    readonly #subscribe: QuoteSubscriber;
    readonly #quotes = new Map<string, BoardQuote>();
    #sequence = 0;
    #refused = 0;
    #stop: (() => void) | undefined;

    constructor(subscribe: QuoteSubscriber) {
        this.#subscribe = subscribe;
    }

    /**
     * Starts listening, the first time it is called. A transport that is not connected
     * yet throws; the board then stays unstarted and the next read tries again, and the
     * read in hand answers from the empty board rather than failing.
     */
    start(): void {
        if (this.#stop !== undefined) return;
        try {
            this.#stop = this.#subscribe(`${MARKET_TICK_BASE}.>`, (tick, subject) => {
                this.take(subject, tick);
            });
        } catch {
            this.#stop = undefined;
        }
    }

    /** Takes one tick heard on @p subject; a subject that names no feed is ignored. */
    take(subject: string, tick: QuoteTick): void {
        const feed = feedOf(subject);
        if (feed === undefined || tick.oresmd_uri === '') return;
        const key = `${feed.provider}|${feed.feed}|${tick.oresmd_uri}`;
        const held = this.#quotes.get(key);
        if (held === undefined && this.#quotes.size >= BOARD_CAPACITY) {
            this.#refused += 1;
            return;
        }
        this.#sequence += 1;
        this.#quotes.set(key, {
            provider: feed.provider,
            feed: feed.feed,
            source: tick.source,
            uri: tick.oresmd_uri,
            value: tick.value,
            previous: held?.value ?? tick.value,
            at: tick.observation_time,
            sequence: this.#sequence,
        });
    }

    /** The quotes of @p feeds that arrived after @p sequence, oldest first. */
    after(sequence: number, feeds: ReadonlySet<string>): BoardQuote[] {
        return [...this.#quotes.values()]
            .filter((q) => q.sequence > sequence && feeds.has(`${q.provider}|${q.feed}`))
            .sort((a, b) => a.sequence - b.sequence);
    }

    /** The board's count now: the sequence of the newest quote. */
    get sequence(): number {
        return this.#sequence;
    }

    /** How many datums the board did not keep because it was full. */
    get refused(): number {
        return this.#refused;
    }

    /** The source and the time of the newest quote of each feed it has heard. */
    feeds(): Map<string, { readonly source: string; readonly at: string }> {
        const out = new Map<string, { source: string; at: string }>();
        for (const q of this.#quotes.values()) {
            const key = `${q.provider}|${q.feed}`;
            const seen = out.get(key);
            if (seen === undefined || q.at >= seen.at) out.set(key, { source: q.source, at: q.at });
        }
        return out;
    }

    /** Stops listening. */
    stop(): void {
        this.#stop?.();
        this.#stop = undefined;
    }
}

/**
 * One read's quotes and the sequence it states. A read over the ceiling sends the
 * oldest and states the sequence of the last it sent, so the next read carries on
 * from there and loses none.
 */
export function pageOf(
    rows: readonly BoardQuote[],
    now: number,
    most = MOST_QUOTES,
): { readonly rows: readonly BoardQuote[]; readonly sequence: number } {
    if (rows.length <= most) return { rows, sequence: now };
    const sent = rows.slice(0, most);
    return { rows: sent, sequence: sent[sent.length - 1]?.sequence ?? now };
}

/** The roles of an instrument type's fields and its asset class, as the server's schema states. */
export interface InstrumentShape {
    readonly assetClass: string;
    readonly roles: ReadonlyMap<string, string>;
}

/** The labels a quote's URI gives. */
export interface QuoteLabels {
    readonly instrument_type: string;
    readonly asset_class: string;
    readonly series_uri: string;
    readonly series: string;
    readonly point: string;
}

/**
 * The labels of a canonical oresmd URI: its instrument type and asset class, its series
 * URI, and the words of its series and of its point. The URI is the one the server's
 * codec wrote, so its series is the same text with type=series and no coordinates,
 * which is what the codec writes for the series.
 */
export function labelsOf(
    uri: string,
    shapes: ReadonlyMap<string, InstrumentShape>,
): QuoteLabels | undefined {
    const question = uri.indexOf('?');
    const scheme = 'oresmd://';
    if (question < 0 || !uri.startsWith(scheme)) return undefined;
    const head = uri.slice(0, question);
    const slash = head.indexOf('/', scheme.length);
    const subject = slash < 0 ? '' : (tokenText(head.slice(slash + 1)) ?? '');
    const pairs = uri
        .slice(question + 1)
        .split('&')
        .map((pair) => {
            const equals = pair.indexOf('=');
            return equals < 0
                ? { raw: pair, name: pair, value: '' }
                : { raw: pair, name: pair.slice(0, equals), value: pair.slice(equals + 1) };
        });
    const instrument = pairs.find((p) => p.name === 'instrument')?.value ?? '';
    const shape = shapes.get(instrument);
    if (shape === undefined) return undefined;
    const kept: string[] = [];
    const identity = subject === '' ? [] : [subject];
    const point: string[] = [];
    for (const p of pairs) {
        const role = shape.roles.get(p.name);
        if (p.name === 'type') kept.push('type=series');
        else if (role === 'coordinate') point.push(tokenText(p.value) ?? p.value);
        else {
            kept.push(p.raw);
            if (role === 'identity') identity.push(tokenText(p.value) ?? p.value);
        }
    }
    return {
        instrument_type: instrument,
        asset_class: shape.assetClass,
        series_uri: `${head}?${kept.join('&')}`,
        series: identity.join(' '),
        point: point.join(' '),
    };
}

const watchableSchema = z.looseObject({
    result: resultSchema,
    feeds: z.array(z.object({ provider_code: z.string(), feed_id: z.string() })),
});

const schemaReplySchema = z.looseObject({
    instruments: z.array(
        z.looseObject({
            code: z.string(),
            asset_class: z.string(),
            fields: z.array(z.looseObject({ name: z.string(), role: z.string() })),
        }),
    ),
});

const ticksReplySchema = z.looseObject({
    result: resultSchema,
    coordinates: z.array(z.string()),
    first_instant: z.string().nullable().optional(),
    gaps_us: z.array(z.number()),
    coordinate_indexes: z.array(z.number()),
    values: z.array(z.number()),
});

function refused(result: z.infer<typeof resultSchema>): HttpFailure {
    if (result.outcome === 'denied')
        return new HttpFailure(403, { code: 'forbidden', message: result.message });
    return new HttpFailure(400, { code: 'invalid-request', message: result.message });
}

/** The feeds the session may watch for display, as provider|feed keys and as rows. */
async function watchable(
    session: LiveSession,
): Promise<{ readonly provider_code: string; readonly feed_id: string }[]> {
    const answer = await session.client.callAuthenticated(
        subjects.list_watchable_feeds_request,
        { for_display: true },
        watchableSchema,
    );
    if (answer.result.outcome !== 'ok') throw refused(answer.result);
    return answer.feeds;
}

/** An instant as the wire writes it, with a space between the date and the time. */
function wireInstant(text: string): string {
    return text.replace('T', ' ');
}

/** The instant a server writes as text, as the microseconds since the epoch. */
function microsOf(text: string): number {
    const iso = text.replace(' ', 'T');
    const fraction = /\.(\d+)Z$/.exec(iso)?.[1] ?? '';
    const millis = Date.parse(iso.replace(/\.\d+Z$/, 'Z'));
    return millis * 1000 + Number((fraction + '000000').slice(0, 6));
}

export function registerQuoteRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
    board: QuoteBoard,
): void {
    /**
     * The schema's shapes, read once with the first reader's session and kept for the
     * life of the process: the schema is the codec's own table and changes with a
     * release, which restarts the BFF.
     */
    let shapes: Map<string, InstrumentShape> | undefined;
    const labels = new Map<string, QuoteLabels | undefined>();
    const shapesFor = async (session: LiveSession): Promise<Map<string, InstrumentShape>> => {
        if (shapes !== undefined) return shapes;
        const answer = await session.client.callAuthenticated(
            subjects.describe_oresmd_schema_request,
            {},
            schemaReplySchema,
        );
        shapes = new Map(
            answer.instruments.map((i) => [
                i.code,
                {
                    assetClass: i.asset_class,
                    roles: new Map(i.fields.map((f) => [f.name, f.role])),
                },
            ]),
        );
        return shapes;
    };

    server.get('/api/marketdata/quotes/feeds', async (request) => {
        const session = requireSession(request);
        board.start();
        const heard = board.feeds();
        return {
            feeds: (await watchable(session)).map((f) => {
                const seen = heard.get(`${f.provider_code}|${f.feed_id}`);
                return {
                    provider: f.provider_code,
                    feed: f.feed_id,
                    source: seen?.source ?? '',
                    last_at: seen?.at ?? null,
                };
            }),
        };
    });

    server.get('/api/marketdata/quotes', async (request) => {
        const session = requireSession(request);
        const query = z.object({ after: z.string().optional() }).strict().safeParse(request.query);
        if (!query.success)
            throw new HttpFailure(400, { code: 'invalid-request', message: 'Unknown parameter.' });
        const after = query.data.after === undefined ? 0 : Number(query.data.after);
        if (!Number.isInteger(after) || after < 0)
            throw new HttpFailure(400, {
                code: 'invalid-request',
                message: `The after parameter is not a board sequence: ${query.data.after ?? ''}`,
            });
        board.start();
        const feeds = new Set(
            (await watchable(session)).map((f) => `${f.provider_code}|${f.feed_id}`),
        );
        // The sequence is read with the rows, after the entitlement's answer, so a quote that
        // arrives while it is asked is sent once and not again.
        const { rows, sequence } = pageOf(board.after(after, feeds), board.sequence);
        const known = await shapesFor(session);
        const quotes = [];
        for (const q of rows) {
            if (!labels.has(q.uri)) labels.set(q.uri, labelsOf(q.uri, known));
            const named = labels.get(q.uri);
            if (named === undefined) continue;
            quotes.push({
                provider: q.provider,
                feed: q.feed,
                source: q.source,
                uri: q.uri,
                value: q.value,
                previous: q.previous,
                at: q.at,
                ...named,
            });
        }
        return { sequence, quotes };
    });

    server.get('/api/marketdata/quotes/history', async (request) => {
        const session = requireSession(request);
        const query = z
            .object({
                provider: z.string().min(1),
                series: z.string().min(1),
                point: z.string().optional(),
                from: z.string().min(1),
                to: z.string().optional(),
            })
            .strict()
            .safeParse(request.query);
        if (!query.success)
            throw new HttpFailure(400, {
                code: 'invalid-request',
                message: 'A history read names the provider, the series and where it starts.',
            });
        const { provider, series, point, from, to } = query.data;
        const answer = await session.client.callAuthenticated(
            subjects.get_provider_ticks_request,
            {
                provider_code: provider,
                series_uri: series,
                from_instant: wireInstant(from),
                to_instant: to === undefined ? null : wireInstant(to),
                coordinates: point === undefined || point === '' ? [] : [point],
                for_display: true,
            },
            ticksReplySchema,
        );
        if (answer.result.outcome !== 'ok') throw refused(answer.result);
        if (
            answer.gaps_us.length !== answer.values.length ||
            answer.coordinate_indexes.length !== answer.values.length
        )
            throw new HttpFailure(502, {
                code: 'upstream-unavailable',
                message: "The history reply's arrays differ in length.",
            });
        let at = answer.first_instant ? microsOf(answer.first_instant) : 0;
        return {
            ticks: answer.values.map((value, i) => {
                at += answer.gaps_us[i] ?? 0;
                return {
                    at: new Date(Math.floor(at / 1000)).toISOString(),
                    uri: answer.coordinates[answer.coordinate_indexes[i] ?? 0] ?? '',
                    value,
                };
            }),
        };
    });
}

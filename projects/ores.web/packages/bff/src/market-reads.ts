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
import { isWireTimestamp } from '@ores/wire-protocol';
import { subjects } from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure } from './errors.js';
import { input } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The four market-data reads a desk screen asks: the instrument catalogue, one
 * currency's rate ladder, one base currency's FX montage and one curve's
 * analytics.
 *
 * Each route forwards the typed request to the server's subject and returns the
 * answer as the server wrote it. No route derives an instrument's identity, an
 * axis order, a maturity date or a staleness verdict: those are the server's
 * facts, and a screen that recomputed one would disagree with the screen beside
 * it.
 */

const instant = z.string().refine(isWireTimestamp, {
    message: 'An instant is written YYYY-MM-DD HH:MM:SSZ.',
});

const providers = z.array(z.string().trim().min(1).max(100)).max(50).default([]);

const instrumentsBody = z.strictObject({
    query: z.string().trim().max(200).default(''),
    kinds: z.array(z.string().trim().min(1).max(100)).max(20).default([]),
    provider_code: z.string().trim().max(100).default(''),
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(500)).default(200),
});

const rateLadderBody = z.strictObject({
    currency: z.string().trim().length(3),
    instrument_type: z.string().trim().min(1).max(100).default('discount'),
    forward_start: z.string().trim().max(20).default(''),
    as_of: instant,
    providers,
    limit: z.coerce.number().pipe(z.int().min(1).max(200)).default(100),
});

const fxMontageBody = z.strictObject({
    base_currency: z.string().trim().length(3),
    as_of: instant,
    providers,
});

const uuid = z
    .string()
    .regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i, 'An id is a UUID.');

const curveAnalyticsBody = z.strictObject({
    instrument_id: uuid,
    as_of: instant,
});

const sourceSchema = z.looseObject({
    provider_code: z.string().default(''),
    provider_kind: z.string().default(''),
    series_id: z.string().default(''),
    as_of: z.string().default(''),
    age_seconds: z.number().default(0),
    stale: z.boolean().default(false),
});

const axisSchema = z.looseObject({
    field: z.string(),
    values: z.array(z.string()).default([]),
});

const instrumentSchema = z.looseObject({
    instrument_id: z.string(),
    kind: z.string().default(''),
    name: z.string().default(''),
    currency: z.string().default(''),
    base_currency: z.string().default(''),
    quote_currency: z.string().default(''),
    instrument_type: z.string().default(''),
    quote_type: z.string().default(''),
    forward_start: z.string().default(''),
    axes: z.array(axisSchema).default([]),
    sources: z.array(sourceSchema).default([]),
    party_id: z.string().default(''),
    stale: z.boolean().default(false),
    as_of: z.string().default(''),
});

const instrumentsReply = z.looseObject({
    success: z.boolean(),
    message: z.string().default(''),
    instruments: z.array(instrumentSchema).default([]),
    total: z.number().default(0),
    truncated: z.boolean().default(false),
});

const rungQuoteSchema = z.looseObject({
    provider_code: z.string().default(''),
    value: z.number(),
    as_of: z.string().default(''),
    age_seconds: z.number().default(0),
    stale: z.boolean().default(false),
    series_id: z.string().default(''),
});

const consensusSchema = z.looseObject({
    value: z.number(),
    method: z.string().default(''),
    providers_used: z.number().default(0),
});

const rungSchema = z.looseObject({
    tenor: z.string().default(''),
    maturity_date: z.string().default(''),
    year_fraction: z.number().default(0),
    quotes: z.array(rungQuoteSchema).default([]),
    consensus: consensusSchema,
});

const rateLadderReply = z.looseObject({
    success: z.boolean(),
    message: z.string().default(''),
    currency: z.string().default(''),
    instrument_type: z.string().default(''),
    forward_start: z.string().default(''),
    as_of: z.string().default(''),
    provider_count: z.number().default(0),
    tenor_convention: z.string().default(''),
    day_count_convention: z.string().default(''),
    rungs: z.array(rungSchema).default([]),
});

const montageSpotSchema = z.looseObject({
    provider_code: z.string().default(''),
    value: z.number(),
    as_of: z.string().default(''),
    age_seconds: z.number().default(0),
    stale: z.boolean().default(false),
    series_id: z.string().default(''),
});

const montagePairSchema = z.looseObject({
    pair_name: z.string().default(''),
    base_currency: z.string().default(''),
    quote_currency: z.string().default(''),
    spot_by_provider: z.array(montageSpotSchema).default([]),
    consensus: z.number(),
    change_pct: z.number().nullable().default(null),
    day_high: z.number().nullable().default(null),
    day_low: z.number().nullable().default(null),
});

const fxMontageReply = z.looseObject({
    success: z.boolean(),
    message: z.string().default(''),
    base_currency: z.string().default(''),
    as_of: z.string().default(''),
    change_since: z.string().default(''),
    pairs: z.array(montagePairSchema).default([]),
});

const conventionsSchema = z.looseObject({
    index_name: z.string().default(''),
    day_count: z.string().default(''),
    compounding: z.string().default(''),
    interpolation: z.string().default(''),
    calendar: z.string().default(''),
    spot_lag_days: z.number().default(0),
    pillar_count: z.number().default(0),
});

const marketPillarSchema = z.looseObject({
    tenor: z.string().default(''),
    maturity_date: z.string().default(''),
    year_fraction: z.number().default(0),
    rate: z.number(),
    provider_code: z.string().default(''),
    series_id: z.string().default(''),
    as_of: z.string().default(''),
});

const bootstrappedSchema = z.looseObject({
    tenor: z.string().default(''),
    maturity_date: z.string().default(''),
    year_fraction: z.number().default(0),
    discount_factor: z.number(),
    zero_rate: z.number(),
    implied_forward: z.number(),
    shift_bps: z.number(),
});

const curveAnalyticsReply = z.looseObject({
    success: z.boolean(),
    message: z.string().default(''),
    instrument_id: z.string().default(''),
    name: z.string().default(''),
    as_of: z.string().default(''),
    stale: z.boolean().default(false),
    conventions: conventionsSchema,
    market_pillars: z.array(marketPillarSchema).default([]),
    bootstrapped: z.array(bootstrappedSchema).default([]),
});

/**
 * A refused read is a question the server will not answer, such as a currency it
 * holds no ladder for. The server's words are the reason, so they travel as they
 * are and the screen shows them.
 */
function refused(message: string): HttpFailure {
    return new HttpFailure(422, { code: 'invalid-request', message: message });
}

/**
 * The reply, once the server has agreed to answer it.
 *
 * A refusal is the server's, so its words travel as the reason. The schema is
 * what makes the rest of the answer safe to read: a field the server did not
 * send is its default here and nowhere else.
 */
async function ask<Schema extends z.ZodType>(
    session: LiveSession,
    subject: string,
    body: Record<string, unknown>,
    schema: Schema,
): Promise<z.infer<Schema>> {
    const reply = (await session.client.callAuthenticated(
        subject,
        body,
        schema,
    )) as z.infer<Schema> & {
        success?: boolean;
        message?: string;
    };
    if (reply.success === false) {
        throw refused(reply.message ?? '');
    }
    return reply;
}

export function registerMarketReadRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    // The catalogue of the session party: one entry per instrument, with its
    // axes in draw order and the verdict on each source.
    server.post('/api/market/instruments', async (request) => {
        const session = requireSession(request);
        const body = input(instrumentsBody, request.body);
        const reply = await ask(
            session,
            subjects.list_instruments_request,
            {
                party_id: '',
                query: body.query,
                kinds: body.kinds,
                provider_code: body.provider_code,
                offset: body.offset,
                limit: body.limit,
            },
            instrumentsReply,
        );
        return {
            instruments: reply.instruments,
            total: reply.total,
            truncated: reply.truncated,
        };
    });

    // One currency's ladder at an instant. The maturity dates, the year
    // fractions and the consensus are the server's.
    server.post('/api/market/rate-ladder', async (request) => {
        const session = requireSession(request);
        const body = input(rateLadderBody, request.body);
        const reply = await ask(
            session,
            subjects.get_rate_ladder_request,
            {
                party_id: '',
                currency: body.currency,
                instrument_type: body.instrument_type,
                forward_start: body.forward_start,
                as_of: body.as_of,
                providers: body.providers,
                limit: body.limit,
            },
            rateLadderReply,
        );
        const { success: _success, message: _message, ...facts } = reply;
        return facts;
    });

    // One base currency's crosses at an instant, measured from the instant the
    // server states rather than one the browser chose.
    server.post('/api/market/fx-montage', async (request) => {
        const session = requireSession(request);
        const body = input(fxMontageBody, request.body);
        const reply = await ask(
            session,
            subjects.get_fx_montage_request,
            {
                party_id: '',
                base_currency: body.base_currency,
                providers: body.providers,
                as_of: body.as_of,
            },
            fxMontageReply,
        );
        const { success: _success, message: _message, ...facts } = reply;
        return facts;
    });

    // One curve's conventions, its market pillars and what the bootstrap makes
    // of them.
    server.post('/api/market/curve-analytics', async (request) => {
        const session = requireSession(request);
        const body = input(curveAnalyticsBody, request.body);
        const reply = await ask(
            session,
            subjects.get_curve_analytics_request,
            {
                party_id: '',
                instrument_id: body.instrument_id,
                as_of: body.as_of,
            },
            curveAnalyticsReply,
        );
        const { success: _success, message: _message, ...facts } = reply;
        return facts;
    });
}

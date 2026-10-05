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
    REFDATA_RECORDS,
    listRecords,
    recordResource,
    resourceName,
    removeRecord,
    saveRecord,
    type RecordResource,
    type WriteOutcome,
} from '@ores/wire-protocol';
import type { CurrencyWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_protocol';
import type { CurrencyCalendarWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_calendar_protocol';
import type { CurrencyCountryWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_country_protocol';
import type { CurrencyCurrencyGroupWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_currency_group_protocol';
import type { CurrencyGroupWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_group_protocol';
import type { CurrencyPairWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_protocol';
import type { CurrencyPairConventionWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_convention_protocol';
import type { CurrencyPairConventionCalendarWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_convention_calendar_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

const code = z.string().trim().min(1).max(100);
const text = z.string().max(2000);
const count = z.int().min(0).max(1_000_000);

/**
 * What each writable resource accepts, in the server's own field names.
 *
 * Each schema satisfies the resource's generated write type, so a model change
 * the schema misses fails the typecheck rather than the request.
 */
const WRITES: Readonly<Record<string, z.ZodType<Record<string, unknown>>>> = {
    currencies: z.object({
        iso_code: z.string().trim().length(3),
        name: text.min(1),
        numeric_code: z.string().max(3),
        symbol: z.string().max(20),
        fraction_symbol: z.string().max(20),
        fractions_per_unit: count,
        rounding_type: code,
        rounding_precision: z.int().min(0).max(20),
        format: z.string().max(100),
        monetary_nature: code,
        market_tier: code,
        ore_currency_type: z.string().max(100).nullable(),
        image_id: z.uuid().nullable(),
        spot_days: z.int().min(0).max(10),
        day_basis: z.string().max(20),
        base_precedence: count,
    }) satisfies z.ZodType<CurrencyWrite>,
    'currency-groups': z.object({
        code,
        name: text.min(1),
        description: text,
        display_order: count,
    }) satisfies z.ZodType<CurrencyGroupWrite>,
    'currency-countries': z.object({
        currency_iso_code: code,
        country_alpha2_code: code,
    }) satisfies z.ZodType<CurrencyCountryWrite>,
    'currency-calendars': z.object({
        currency_iso_code: code,
        calendar_code: code,
    }) satisfies z.ZodType<CurrencyCalendarWrite>,
    'currency-memberships': z.object({
        currency_iso_code: code,
        currency_group_code: code,
    }) satisfies z.ZodType<CurrencyCurrencyGroupWrite>,
    'currency-pairs': z.object({
        pair_code: z.string().trim().min(7).max(15),
        base_currency: code,
        quote_currency: code,
        classification: code,
    }) satisfies z.ZodType<CurrencyPairWrite>,
    'currency-pair-conventions': z.object({
        pair_code: z.string().trim().min(7).max(15),
        pip_factor: z.number().positive(),
        tick_size: z.number().positive(),
        decimal_places: z.int().min(0).max(12),
        business_day_convention: code.nullable(),
        spot_relative: z.boolean().nullable(),
        end_of_month: z.boolean().nullable(),
    }) satisfies z.ZodType<CurrencyPairConventionWrite>,
    'pair-calendars': z.object({
        pair_code: z.string().trim().min(7).max(15),
        calendar_code: code,
    }) satisfies z.ZodType<CurrencyPairConventionCalendarWrite>,
};

const intentSchema = z.object({
    reasonCode: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000).default(''),
});

const saveBodySchema = intentSchema.extend({
    write: z.record(z.string(), z.unknown()),
    version: z.int().nonnegative().nullable(),
});

const removeBodySchema = intentSchema.extend({
    key: z.record(z.string(), z.string().min(1).max(100)),
});

/**
 * The resource the address names, or a 404.
 *
 * The browser names a resource by its key, never by a subject: the registry
 * maps the key to the subjects, and a key the registry lacks reaches nothing.
 */
function resourceFor(request: FastifyRequest): RecordResource {
    const { resource } = request.params as { resource: string };
    const found = recordResource(resource);
    if (found === undefined) {
        throw notFound(`There is no reference data resource ${resource}.`);
    }
    return found;
}

/** The resource the address names, refused when it is read only. */
function writableFor(request: FastifyRequest): RecordResource {
    const resource = resourceFor(request);
    if (!resource.writable) {
        throw notPermitted(`${resource.key} is read only here.`);
    }
    return resource;
}

/**
 * Turns a refused write into the status that says why. The server sends no
 * words with some refusals, so a conflict says what it means.
 */
function answer(outcome: WriteOutcome): void {
    if (outcome.done) {
        return;
    }
    switch (outcome.outcome) {
        case 'conflict':
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    outcome.message === ''
                        ? 'This record changed since it was read, or it already exists. Reload and try again.'
                        : outcome.message,
            });
        case 'invalid':
            throw invalidRequest(outcome.message);
        case 'denied':
            throw notPermitted(outcome.message);
        case 'missing':
            throw notFound(outcome.message);
        default:
            throw new HttpFailure(502, { code: 'upstream-unavailable', message: outcome.message });
    }
}

/**
 * The reference data record routes: a resource's rows, one parent's rows of a
 * junction, and the writes. The registry decides which resources exist.
 */
export function registerRecordRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The resources a screen may read, each with its key fields and the
     * permissions the server checks to write and to remove a row.
     */
    server.get('/api/refdata', async (request) => {
        requireSession(request);
        return {
            resources: REFDATA_RECORDS.map((resource) => ({
                key: resource.key,
                entityType: resource.entityType,
                keyFields: resource.keyFields,
                versioned: resource.versioned,
                writable: resource.writable,
                writePermission: `refdata::${resourceName(resource)}:write`,
                deletePermission: `refdata::${resourceName(resource)}:delete`,
            })),
        };
    });

    server.get('/api/refdata/:resource', async (request) => {
        const session = requireSession(request);
        return { rows: await listRecords(session.client, resourceFor(request)) };
    });

    /** The rows of a junction that belong to one parent, such as one currency's countries. */
    server.get('/api/refdata/:resource/by/:parent', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        if (resource.listBy === undefined) {
            throw notFound(`${resource.key} is not read by a parent.`);
        }
        const { parent } = request.params as { parent: string };
        return { rows: await listRecords(session.client, resource, parent) };
    });

    /** Writes one row: a new row with no version, else a correction of the version read. */
    server.put('/api/refdata/:resource', async (request, reply) => {
        const session = requireSession(request);
        const resource = writableFor(request);
        const body = saveBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'A write needs the row, the version it was read at, and a reason.',
            );
        }
        const write = WRITES[resource.key]?.safeParse(body.data.write);
        if (write === undefined || !write.success) {
            throw invalidRequest(`The row is not a valid ${resource.key} row.`);
        }
        answer(
            await saveRecord(session.client, resource, write.data, body.data.version, {
                reasonCode: body.data.reasonCode,
                commentary: body.data.commentary,
            }),
        );
        return reply.code(204).send();
    });

    /** Removes one row, named by exactly its key fields. */
    server.delete('/api/refdata/:resource', async (request, reply) => {
        const session = requireSession(request);
        const resource = writableFor(request);
        const body = removeBodySchema.safeParse(request.body);
        const named = body.success ? Object.keys(body.data.key).sort().join(',') : '';
        if (!body.success || named !== [...resource.keyFields].sort().join(',')) {
            throw invalidRequest(
                `A removal names the row by ${resource.keyFields.join(' and ')}, and a reason.`,
            );
        }
        answer(
            await removeRecord(session.client, resource, body.data.key, {
                reasonCode: body.data.reasonCode,
                commentary: body.data.commentary,
            }),
        );
        return reply.code(204).send();
    });
}

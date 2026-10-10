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
import { recordResource } from '@ores/wire-protocol';
import { subjects as businessDayConventionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_day_convention_type_protocol';
import { subjects as calendarNameSubjects } from '@ores/wire-protocol/generated/refdata/protocol/calendar_name_protocol';
import { subjects as dayCountSubjects } from '@ores/wire-protocol/generated/refdata/protocol/day_count_fraction_type_protocol';
import { subjects as floatingIndexSubjects } from '@ores/wire-protocol/generated/refdata/protocol/floating_index_type_protocol';
import { subjects as paymentFrequencySubjects } from '@ores/wire-protocol/generated/refdata/protocol/payment_frequency_protocol';
import { subjects as subPeriodSubjects } from '@ores/wire-protocol/generated/refdata/protocol/sub_periods_coupon_type_protocol';
import { invalidRequest, notFound } from './errors.js';
import { NO_ORDER, idSchema, input, intentSchema, readAll, resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * One instrument family and the convention entity that serves it.
 *
 * The server carries one list per convention entity and nothing that maps an
 * instrument family to its entity, so the mapping is stated here once, from the
 * journey document's table. `writable` marks the two families whose terms the
 * screen draws in full; the rest are listed and read, and are not written until
 * their terms are drawn.
 */
export interface ConventionFamily {
    readonly key: string;
    readonly entity: string;
    readonly prefix: string;
    readonly writable: boolean;
}

export const CONVENTION_FAMILIES: readonly ConventionFamily[] = [
    ['deposit', 'deposit_convention', true],
    ['fra', 'fra_convention', false],
    ['future', 'future_convention', false],
    ['ois', 'ois_convention', false],
    ['average-ois', 'average_ois_convention', false],
    ['swap', 'swap_convention', true],
    ['tenor-basis-swap', 'tenor_basis_swap_convention', false],
    ['tenor-basis-two-swap', 'tenor_basis_two_swap_convention', false],
    ['cross-currency-basis', 'cross_currency_basis_convention', false],
    ['cross-currency-fix-float', 'cross_currency_fix_float_convention', false],
    ['bond-yield', 'bond_yield_convention', false],
    ['cds', 'cds_convention', false],
    ['cms-spread-option', 'cms_spread_option_convention', false],
    ['commodity-forward', 'commodity_forward_convention', false],
    ['commodity-future', 'commodity_future_convention', false],
    ['fx-option', 'fx_option_convention', false],
    ['bma-basis-swap', 'bma_basis_swap_convention', false],
    ['zero', 'zero_convention', false],
    ['inflation-swap', 'inflation_swap_convention', false],
    ['intraday-power-load', 'intraday_power_load_convention', false],
    ['ibor-index', 'ibor_index_convention', false],
    ['overnight-index', 'overnight_index_convention', false],
    ['swap-index', 'swap_index_convention', false],
    ['tenor', 'tenor_convention', false],
    ['currency-pair', 'currency_pair_convention', false],
].map(([key, entity, writable]) => ({
    key: String(key),
    entity: String(entity),
    prefix: `${String(entity)}s`,
    writable: writable === true,
}));

const MAX_ROWS = 500;

const pageQuerySchema = z.object({
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(MAX_ROWS).default(MAX_ROWS),
});

const writeBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative().nullable(),
    write: z.looseObject({ id: idSchema }),
});

const PICK_LISTS = [
    {
        key: 'calendars',
        subject: calendarNameSubjects.list_calendar_names_request,
        rows: 'calendar_names',
    },
    {
        key: 'businessDayConventions',
        subject: businessDayConventionSubjects.list_business_day_convention_types_request,
        rows: 'types',
    },
    {
        key: 'dayCountFractions',
        subject: dayCountSubjects.list_day_count_fraction_types_request,
        rows: 'types',
    },
    {
        key: 'floatingIndices',
        subject: floatingIndexSubjects.list_floating_index_types_request,
        rows: 'types',
    },
    {
        key: 'paymentFrequencies',
        subject: paymentFrequencySubjects.list_payment_frequencies_request,
        rows: 'payment_frequencies',
    },
    {
        key: 'subPeriodsCouponTypes',
        subject: subPeriodSubjects.list_sub_periods_coupon_types_request,
        rows: 'types',
    },
] as const;

function familyOf(request: FastifyRequest): ConventionFamily {
    const { family } = request.params as { family: string };
    const found = CONVENTION_FAMILIES.find((candidate) => candidate.key === family);
    if (found === undefined) {
        throw notFound(`There is no convention family ${family}.`);
    }
    return found;
}

/**
 * The convention routes: the families with their counts, one family's rows, the
 * pick lists, and the write of a family whose terms the screen draws.
 *
 * The server's convention lists filter by id only, so a search is the client's,
 * over the rows this returns.
 */
export function registerConventionRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/conventions/families', async (request) => {
        const session = requireSession(request);
        const families = [];
        for (const family of CONVENTION_FAMILIES) {
            const reply = await session.client.callAuthenticated(
                `refdata.v1.${family.prefix}.list`,
                { offset: 0, limit: 1, order: NO_ORDER, filter: { id_one_of: null }, as_of: null },
                z.looseObject({ result: resultSchema, total: z.int().nonnegative().default(0) }),
            );
            // A count that cannot be read is null rather than a failed page: the family is still there to open.
            families.push({
                key: family.key,
                entity: family.entity,
                writable: family.writable,
                count: reply.result.outcome === 'ok' ? reply.total : null,
            });
        }
        return { families };
    });

    server.get('/api/conventions/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(session, list.subject, list.rows, { filter: null });
        }
        const currencies = recordResource('currencies');
        if (currencies === undefined) {
            throw invalidRequest('The currency list is not registered.');
        }
        lists['currencies'] = await readAll(session, currencies.subjects.list, currencies.rows, {
            filter: null,
            as_of: null,
        });
        return lists;
    });

    server.get('/api/conventions/:family', async (request) => {
        const session = requireSession(request);
        const family = familyOf(request);
        const query = input(pageQuerySchema, request.query);
        const reply = await session.client.callAuthenticated(
            `refdata.v1.${family.prefix}.list`,
            {
                offset: query.offset,
                limit: query.limit,
                order: NO_ORDER,
                filter: { id_one_of: null },
                as_of: null,
            },
            z.looseObject({ result: resultSchema, total: z.int().nonnegative().default(0) }),
        );
        if (reply.result.outcome !== 'ok') {
            throw invalidRequest(reply.result.message);
        }
        return {
            rows: z.array(z.looseObject({})).default([]).parse(reply[family.prefix]),
            total: reply.total,
        };
    });

    server.put('/api/conventions/:family', async (request) => {
        const session = requireSession(request);
        const family = familyOf(request);
        if (!family.writable) {
            throw invalidRequest(`The terms of ${family.key} conventions are not drawn yet.`);
        }
        const body = input(writeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            `refdata.v1.${family.prefix}.put`,
            {
                change: {
                    write: body.write,
                    precondition:
                        body.version === null
                            ? { kind: 'must_not_exist', version: null }
                            : { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        return { result: reply.result, convention: reply[family.entity] ?? null };
    });
}

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
import { subjects as conventionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/convention_protocol';
import { subjects as tenorBasisSwapConventionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_basis_swap_convention_protocol';
import { subjects as dayCountSubjects } from '@ores/wire-protocol/generated/refdata/protocol/day_count_fraction_type_protocol';
import { subjects as floatingIndexSubjects } from '@ores/wire-protocol/generated/refdata/protocol/floating_index_type_protocol';
import { CONVENTION_FAMILY_ENTRIES } from '@ores/wire-protocol/generated/refdata/domain/convention_family_registry';
import { subjects as paymentFrequencySubjects } from '@ores/wire-protocol/generated/refdata/protocol/payment_frequency_protocol';
import { subjects as subPeriodSubjects } from '@ores/wire-protocol/generated/refdata/protocol/sub_periods_coupon_type_protocol';
import { invalidRequest, notFound } from './errors.js';
import { NO_ORDER, idSchema, input, intentSchema, readAll, resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * One instrument family and the convention entity that serves it.
 *
 * The plain document families come from the generated registry, so a family
 * added to the modelling tree reaches the screen with no edit here. What the
 * registry cannot state is stated below: the composite family, the kinds the
 * screen lists that the conventions document does not carry, and which
 * families the screen writes. `writable` marks the two families whose terms the
 * screen draws in full; the rest are listed and read, and are not written until
 * their terms are drawn.
 */
export interface ConventionFamily {
    readonly key: string;
    readonly entity: string;
    readonly prefix: string;
    readonly writable: boolean;
}

/**
 * A family whose entity is a composite: one central convention row plus the
 * mutually exclusive part entities it carries. The family protocol has no
 * list, so the central row is listed through the convention entity filtered by
 * its kind. The server therefore keeps the filter, the order, the page and the
 * total, and the family's get_many answers the parts for exactly that page's
 * keys, in the page's own order. The two are aligned, so the parts are zipped
 * onto the central rows here rather than joining two unrelated result sets.
 *
 * A family absent from this table is a plain entity whose own list is the page.
 */
interface CompositeFamily {
    /** The entity the screen shows the family as. */
    readonly entity: string;
    /** The registry families the composite stands for, so the screen lists none of them alone. */
    readonly registryKeys: readonly string[];
    /** The kind_code every central row of the family carries. */
    readonly kindCode: string;
    /** The family subject that answers a set of central keys with their parts. */
    readonly getManySubject: string;
}

const COMPOSITE_FAMILIES: Readonly<Record<string, CompositeFamily>> = {
    'tenor-basis-swap': {
        entity: 'tenor_basis_swap_convention',
        registryKeys: ['convention', 'tenor_basis_swap_pay_receive', 'tenor_basis_swap_long_short'],
        kindCode: 'TENOR_BASIS_SWAP',
        getManySubject:
            tenorBasisSwapConventionSubjects.get_many_tenor_basis_swap_conventions_request,
    },
};

/** Kinds the screen lists that the conventions document does not carry. */
const SCREEN_ONLY_ENTITIES: readonly (readonly [string, string])[] = [
    ['tenor', 'tenor_convention'],
    ['currency-pair', 'currency_pair_convention'],
];

const WRITABLE_FAMILIES: ReadonlySet<string> = new Set(['deposit', 'swap']);

const ABSORBED_REGISTRY_KEYS: ReadonlySet<string> = new Set(
    Object.values(COMPOSITE_FAMILIES).flatMap((composite) => composite.registryKeys),
);

function screenFamily(key: string, entity: string): ConventionFamily {
    return { key, entity, prefix: `${entity}s`, writable: WRITABLE_FAMILIES.has(key) };
}

export const CONVENTION_FAMILIES: readonly ConventionFamily[] = [
    ...CONVENTION_FAMILY_ENTRIES.filter((entry) => !ABSORBED_REGISTRY_KEYS.has(entry.key)).map(
        (entry) => screenFamily(entry.key.replaceAll('_', '-'), entry.entity),
    ),
    ...Object.entries(COMPOSITE_FAMILIES).map(([key, composite]) =>
        screenFamily(key, composite.entity),
    ),
    ...SCREEN_ONLY_ENTITIES.map(([key, entity]) => screenFamily(key, entity)),
].sort((left, right) => (left.key < right.key ? -1 : left.key > right.key ? 1 : 0));

/**
 * The columns of a part that the central row owns: its own identity, its link
 * to the centre, and its audit provenance. A zip takes only the terms and
 * leaves these to the central row, so the row keeps the version the list's
 * page was read at.
 */
const PART_LINK_FIELDS = new Set([
    'id',
    'version',
    'tenant_id',
    'convention_id',
    'party_id',
    'modified_by',
    'performed_by',
    'change_reason_code',
    'change_commentary',
    'recorded_at',
]);

const centralRow = z.looseObject({ id: z.string(), version: z.int() });
const compositeItem = z.looseObject({ key: z.looseObject({ id: z.string() }) });

/** One central row with whichever part is populated laid over its terms. */
function zipParts(
    central: Readonly<Record<string, unknown>>,
    item: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    let merged = { ...central };
    for (const value of Object.values(item)) {
        if (!Array.isArray(value)) {
            continue;
        }
        const part = value.find((member) => typeof member === 'object' && member !== null) as
            Record<string, unknown> | undefined;
        if (part === undefined) {
            continue;
        }
        const terms = Object.fromEntries(
            Object.entries(part).filter(([field]) => !PART_LINK_FIELDS.has(field)),
        );
        merged = { ...merged, ...terms };
    }
    return merged;
}

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
 * A plain family's own list is the page. A composite family is listed through
 * the convention entity filtered by kind, and its parts are read for that same
 * page's keys. The server keeps the filter, the order, the page and the total
 * in both cases, so a search is the client's, over the rows this returns.
 */
export function registerConventionRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/conventions/families', async (request) => {
        const session = requireSession(request);
        const families = [];
        for (const family of CONVENTION_FAMILIES) {
            const composite = COMPOSITE_FAMILIES[family.key];
            // A count that cannot be read is null rather than a failed page: the family is still there to open.
            let count: number | null = null;
            try {
                const reply = await session.client.callAuthenticated(
                    composite === undefined
                        ? `refdata.v1.${family.prefix}.list`
                        : conventionSubjects.list_conventions_request,
                    {
                        offset: 0,
                        limit: 1,
                        order: NO_ORDER,
                        filter:
                            composite === undefined
                                ? { id_one_of: null }
                                : {
                                      kind_code: composite.kindCode,
                                      kind_code_one_of: null,
                                      id_one_of: null,
                                  },
                        as_of: null,
                    },
                    z.looseObject({
                        result: resultSchema,
                        total: z.int().nonnegative().default(0),
                    }),
                );
                count = reply.result.outcome === 'ok' ? reply.total : null;
            } catch {
                count = null;
            }
            families.push({
                key: family.key,
                entity: family.entity,
                writable: family.writable,
                count,
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
        const composite = COMPOSITE_FAMILIES[family.key];
        if (composite === undefined) {
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
        }
        // The family protocol has no list, so the central rows are listed
        // through the convention entity filtered by kind. The server keeps the
        // filter, the order, the page and the total.
        const reply = await session.client.callAuthenticated(
            conventionSubjects.list_conventions_request,
            {
                offset: query.offset,
                limit: query.limit,
                order: NO_ORDER,
                filter: {
                    kind_code: composite.kindCode,
                    kind_code_one_of: null,
                    id_one_of: null,
                },
                as_of: null,
            },
            z.looseObject({
                result: resultSchema,
                conventions: z.array(centralRow).default([]),
                total: z.int().nonnegative().default(0),
            }),
        );
        if (reply.result.outcome !== 'ok') {
            throw invalidRequest(reply.result.message);
        }
        if (reply.conventions.length === 0) {
            return { rows: [], total: reply.total };
        }
        // The parts are read for exactly the page's keys and answer in the
        // page's order, so only the terms are zipped onto the central rows.
        const parts = await session.client.callAuthenticated(
            composite.getManySubject,
            { keys: reply.conventions.map((row) => ({ id: row.id })) },
            z.looseObject({
                result: resultSchema,
                items: z.array(compositeItem).default([]),
            }),
        );
        if (parts.result.outcome !== 'ok') {
            throw invalidRequest(parts.result.message);
        }
        const items = new Map(
            parts.items.map((item) => [item.key.id, item as Readonly<Record<string, unknown>>]),
        );
        return {
            rows: reply.conventions.map((row) => {
                const item = items.get(row.id);
                return item === undefined
                    ? (row as Readonly<Record<string, unknown>>)
                    : zipParts(row, item);
            }),
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

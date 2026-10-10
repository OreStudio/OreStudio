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
import { recordResource, type RecordResource } from '@ores/wire-protocol';
import { subjects as businessCentreSubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_business_centre_protocol';
import { subjects as contactSubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_contact_information_protocol';
import { subjects as contactTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/contact_type_protocol';
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_identifier_protocol';
import { subjects as partyCounterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_counterparty_protocol';
import { subjects as partyIdSchemeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_id_scheme_protocol';
import { subjects as partyStatusSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_status_protocol';
import { subjects as partyTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_type_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

const PAGE = 1000;

/** The most counterparties one page, or one children request, may name. */
const MAX_PAGE = 200;

const NO_ORDER = { field: '', descending: false } as const;

const resultSchema = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string().default(''),
    message: z.string().default(''),
    fields: z
        .array(
            z.object({
                field: z.string().default(''),
                code: z.string().default(''),
                message: z.string().default(''),
            }),
        )
        .default([]),
});

const row = z.looseObject({});

const intentSchema = z.object({
    reason_code: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000).default(''),
});

const idSchema = z.uuid();

const pageQuerySchema = z.object({
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(MAX_PAGE).default(25),
    search: z.string().max(200).default(''),
    status: z.enum(['active', 'closed', 'all']).default('all'),
});

const childrenQuerySchema = z.object({
    ids: z
        .string()
        .min(1)
        .transform((value) => value.split(',').filter((id) => id !== ''))
        .pipe(z.array(idSchema).min(1).max(MAX_PAGE)),
});

const centresBodySchema = z.object({
    codes: z.array(z.string().trim().min(1).max(100)).max(1000),
    intent: intentSchema,
});

const visibilityBodySchema = z.object({
    partyId: idSchema,
    intent: intentSchema,
});

/*
 * The BFF checks the counterparty's id and the intent, and passes the child rows
 * through untouched: the refdata service validates them and names the field it
 * refuses.
 */
const compositeBodySchema = z.looseObject({
    intent: intentSchema,
    counterparty: z.looseObject({ id: idSchema }),
    identifiers: z.array(row).default([]),
    contacts: z.array(row).default([]),
    agreements: z.array(row).default([]),
    netting_sets: z.array(row).default([]),
    netting_set_identifiers: z.array(row).default([]),
    csas: z.array(row).default([]),
    eligible_currencies: z.array(row).default([]),
});

/** A request's input read against its schema, or a 400 that says what was wrong. */
function input<Schema extends z.ZodType>(schema: Schema, value: unknown): z.infer<Schema> {
    const parsed = schema.safeParse(value);
    if (!parsed.success) {
        throw invalidRequest(parsed.error.issues.map((issue) => issue.message).join(' '));
    }
    return parsed.data;
}

/** A pick list: the read-only reference list a counterparty form fills a select from. */
interface PickList {
    readonly key: string;
    readonly resource: RecordResource;
}

/**
 * The reference lists the form fills its selects from.
 *
 * The party types, statuses, identifier schemes and contact types are
 * classification lists the generic record table does not carry, so each is
 * stated here from its generated subjects. Business centres and currencies
 * come from the table.
 */
function listResource(key: string, entityType: string, rows: string, list: string): RecordResource {
    return {
        key,
        entityType,
        keyFields: ['code'],
        rows,
        versioned: true,
        writable: false,
        asOf: false,
        search: false,
        sortable: [],
        subjects: { list, put: '', remove: '' },
    };
}

const PICK_LISTS: readonly PickList[] = [
    {
        key: 'partyTypes',
        resource: listResource(
            'party-types',
            'ores.refdata.party_type',
            'types',
            partyTypeSubjects.list_party_types_request,
        ),
    },
    {
        key: 'partyStatuses',
        resource: listResource(
            'party-statuses',
            'ores.refdata.party_status',
            'statuses',
            partyStatusSubjects.list_party_statuses_request,
        ),
    },
    {
        key: 'identifierSchemes',
        resource: listResource(
            'party-id-schemes',
            'ores.refdata.party_id_scheme',
            'schemes',
            partyIdSchemeSubjects.list_party_id_schemes_request,
        ),
    },
    {
        key: 'contactTypes',
        resource: listResource(
            'contact-types',
            'ores.refdata.contact_type',
            'types',
            contactTypeSubjects.list_contact_types_request,
        ),
    },
];

/**
 * Turns a refused read into the status that says why, so the browser can tell a
 * missing permission from a bad request.
 */
function refusal(result: z.infer<typeof resultSchema>): HttpFailure {
    switch (result.outcome) {
        case 'denied':
            return notPermitted(result.message);
        case 'missing':
            return notFound(result.message);
        case 'unavailable':
        case 'failed':
            return new HttpFailure(502, { code: 'upstream-unavailable', message: result.message });
        default:
            return invalidRequest(result.message);
    }
}

/** Every row of a list, read a page at a time until a page comes back short. */
async function readAll(
    session: LiveSession,
    subject: string,
    rows: string,
    request: Readonly<Record<string, unknown>>,
): Promise<readonly Record<string, unknown>[]> {
    const all: Record<string, unknown>[] = [];
    for (let offset = 0; ; offset += PAGE) {
        const reply = await session.client.callAuthenticated(
            subject,
            { ...request, offset, limit: PAGE, order: NO_ORDER },
            z.looseObject({ result: resultSchema }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        const page = z.array(row).default([]).parse(reply[rows]);
        all.push(...page);
        if (page.length < PAGE) {
            return all;
        }
    }
}

/** A write's result, with the row it wrote when the server returns one. */
async function write(
    session: LiveSession,
    subject: string,
    body: unknown,
): Promise<z.infer<typeof resultSchema>> {
    const reply = await session.client.callAuthenticated(
        subject,
        body,
        z.looseObject({ result: resultSchema }),
    );
    return reply.result;
}

function text(value: unknown): string {
    return typeof value === 'string' ? value : '';
}

/** Whether a counterparty matches the search: its short code, full name or id. */
function matches(counterparty: Record<string, unknown>, search: string): boolean {
    if (search === '') {
        return true;
    }
    const needle = search.toLowerCase();
    return [counterparty['short_code'], counterparty['full_name'], counterparty['id']].some(
        (value) => text(value).toLowerCase().includes(needle),
    );
}

/** The counterparty id the address names, or a 400. */
function paramId(request: FastifyRequest): string {
    return input(idSchema, (request.params as { id: string }).id);
}

function isActive(counterparty: Record<string, unknown>): boolean {
    return text(counterparty['status']).toLowerCase() === 'active';
}

/** The rows of a children read that belong to one counterparty. */
function grouped(
    rows: readonly Record<string, unknown>[],
    id: string,
): readonly Record<string, unknown>[] {
    return rows.filter((entry) => entry['counterparty_id'] === id);
}

/**
 * The counterparty routes the onboarding screen calls: a page of the
 * landing list, the children of a page, the pick lists, the composite write,
 * and the two junction writes.
 *
 * The server's counterparty list filters by id only, so a search and a status
 * are applied here, over the tenant's whole list, before the page is cut.
 */
export function registerCounterpartyRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/counterparties', async (request) => {
        const session = requireSession(request);
        const query = input(pageQuerySchema, request.query);
        const all = await readAll(
            session,
            counterpartySubjects.list_counterparties_request,
            'counterparties',
            { filter: null, as_of: null },
        );
        const found = all
            .filter((entry) => matches(entry, query.search))
            .filter(
                (entry) =>
                    query.status === 'all' || isActive(entry) === (query.status === 'active'),
            )
            .sort((left, right) =>
                text(left['short_code']).localeCompare(text(right['short_code'])),
            );
        return {
            rows: found.slice(query.offset, query.offset + query.limit),
            total: found.length,
        };
    });

    server.get('/api/counterparties/children', async (request) => {
        const session = requireSession(request);
        const { ids } = input(childrenQuerySchema, request.query);
        const filter = { counterparty_id: null, id_one_of: null, counterparty_id_one_of: ids };
        const identifiers = await readAll(
            session,
            identifierSubjects.list_counterparty_identifiers_request,
            'counterparty_identifiers',
            { filter },
        );
        const contacts = await readAll(
            session,
            contactSubjects.list_counterparty_contact_informations_request,
            'counterparty_contact_informations',
            { filter },
        );
        const centres = await readAll(
            session,
            businessCentreSubjects.list_counterparty_business_centres_request,
            'counterparty_business_centres',
            { filter: { counterparty_id: null, counterparty_id_one_of: ids } },
        );
        return {
            children: ids.map((id) => ({
                counterpartyId: id,
                identifiers: grouped(identifiers, id),
                contacts: grouped(contacts, id),
                centres: grouped(centres, id),
            })),
        };
    });

    server.get('/api/counterparties/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(
                session,
                list.resource.subjects.list,
                list.resource.rows,
                {
                    filter: null,
                },
            );
        }
        const centres = recordResource('business-centres');
        const currencies = recordResource('currencies');
        if (centres === undefined || currencies === undefined) {
            throw invalidRequest('The business centre and currency lists are not registered.');
        }
        lists['businessCentres'] = await readAll(session, centres.subjects.list, centres.rows, {
            filter: null,
        });
        lists['currencies'] = await readAll(session, currencies.subjects.list, currencies.rows, {
            filter: null,
            as_of: null,
        });
        return lists;
    });

    server.put('/api/counterparties/composite', async (request) => {
        const session = requireSession(request);
        const body = input(compositeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            counterpartySubjects.put_counterparty_composite_request,
            body,
            z.looseObject({ result: resultSchema, counterparty: row.nullable().default(null) }),
        );
        return reply;
    });

    server.get('/api/counterparties/:id/visibility', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        return {
            partyCounterparties: await readAll(
                session,
                partyCounterpartySubjects.list_by_counterparty_id_party_counterparties_request,
                'party_counterparties',
                { counterparty_id: id, scope: 'direct', filter: null },
            ),
        };
    });

    server.put('/api/counterparties/:id/visibility', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const body = input(visibilityBodySchema, request.body);
        const result = await write(
            session,
            partyCounterpartySubjects.put_party_counterparty_request,
            {
                change: {
                    write: { party_id: body.partyId, counterparty_id: id },
                    precondition: { kind: 'any', version: null },
                },
                intent: body.intent,
            },
        );
        return { result };
    });

    server.put('/api/counterparties/:id/business-centres', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const body = input(centresBodySchema, request.body);
        const wanted = new Set(body.codes);
        const held = await readAll(
            session,
            businessCentreSubjects.list_counterparty_business_centres_request,
            'counterparty_business_centres',
            { filter: { counterparty_id: id, counterparty_id_one_of: null } },
        );
        const current = new Set(held.map((entry) => text(entry['business_centre_code'])));
        // Link before closing, so a failure part way never leaves the counterparty with none.
        for (const code of wanted) {
            if (current.has(code)) {
                continue;
            }
            const result = await write(
                session,
                businessCentreSubjects.put_counterparty_business_centre_request,
                {
                    change: {
                        write: { counterparty_id: id, business_centre_code: code },
                        precondition: { kind: 'must_not_exist', version: null },
                    },
                    intent: body.intent,
                },
            );
            if (result.outcome !== 'ok') {
                return { result };
            }
        }
        for (const code of current) {
            if (wanted.has(code)) {
                continue;
            }
            const result = await write(
                session,
                businessCentreSubjects.delete_counterparty_business_centre_request,
                {
                    removal: {
                        key: { counterparty_id: id, business_centre_code: code },
                        precondition: { kind: 'any', version: null },
                    },
                    intent: body.intent,
                },
            );
            if (result.outcome !== 'ok') {
                return { result };
            }
        }
        return { result: { outcome: 'ok', code: '', message: '', fields: [] } };
    });
}

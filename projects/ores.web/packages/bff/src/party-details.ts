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
import { subjects as businessUnitSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_unit_protocol';
import { subjects as businessUnitTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_unit_type_protocol';
import { subjects as contactTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/contact_type_protocol';
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as partyContactSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_contact_information_protocol';
import { subjects as partyCounterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_counterparty_protocol';
import { subjects as partyCountrySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_country_protocol';
import { subjects as partyCurrencySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_currency_protocol';
import { subjects as partyIdSchemeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_id_scheme_protocol';
import { subjects as partyIdentifierSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_identifier_protocol';
import { subjects as partySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_protocol';
import { subjects as partyStatusSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_status_protocol';
import { subjects as partyTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_type_protocol';
import { listResource } from './counterparties.js';
import { invalidRequest } from './errors.js';
import {
    idSchema,
    input,
    intentSchema,
    paramId,
    readAll,
    resultSchema,
    row,
    text,
    write,
} from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

const MAX_PAGE = 200;

const pageQuerySchema = z.object({
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(MAX_PAGE).default(25),
    search: z.string().max(200).default(''),
});

/** The sets a party carries, each read on its own so a slow one does not hold the others. */
const PANELS = {
    identifiers: {
        subject: partyIdentifierSubjects.list_by_party_id_party_identifiers_request,
        rows: 'party_identifiers',
    },
    contacts: {
        subject: partyContactSubjects.list_by_party_id_party_contact_informations_request,
        rows: 'party_contact_informations',
    },
    countries: {
        subject: partyCountrySubjects.list_by_party_id_party_countries_request,
        rows: 'party_countries',
    },
    currencies: {
        subject: partyCurrencySubjects.list_by_party_id_party_currencies_request,
        rows: 'party_currencies',
    },
    counterparties: {
        subject: partyCounterpartySubjects.list_by_party_id_party_counterparties_request,
        rows: 'party_counterparties',
    },
    units: {
        subject: businessUnitSubjects.list_by_party_id_business_units_request,
        rows: 'business_units',
    },
} as const;

type Panel = keyof typeof PANELS;

const panelSchema = z.enum(Object.keys(PANELS) as [Panel, ...Panel[]]);

/** The junctions the person closes or links, with the field that names the far side. */
const JUNCTIONS = {
    countries: {
        put: partyCountrySubjects.put_party_country_request,
        remove: partyCountrySubjects.delete_party_country_request,
        far: 'country_alpha2_code',
    },
    currencies: {
        put: partyCurrencySubjects.put_party_currency_request,
        remove: partyCurrencySubjects.delete_party_currency_request,
        far: 'currency_iso_code',
    },
    counterparties: {
        put: partyCounterpartySubjects.put_party_counterparty_request,
        remove: partyCounterpartySubjects.delete_party_counterparty_request,
        far: 'counterparty_id',
    },
} as const;

type Junction = keyof typeof JUNCTIONS;

const junctionSchema = z.enum(Object.keys(JUNCTIONS) as [Junction, ...Junction[]]);

const linkBodySchema = z.object({ intent: intentSchema });

const unitBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative(),
    write: z.looseObject({ id: idSchema, party_id: idSchema, unit_type_id: z.uuid().nullable() }),
});

const compositeBodySchema = z.looseObject({
    intent: intentSchema,
    party: z.looseObject({ id: idSchema }),
    identifiers: z.array(row).default([]),
    contacts: z.array(row).default([]),
});

const retireBodySchema = z.object({
    intent: intentSchema,
    idValue: z.string().trim().min(1).max(200),
    version: z.int().nonnegative(),
});

const asOfQuerySchema = z.object({ version: z.coerce.number().int().positive() });

const PICK_LISTS = [
    {
        key: 'partyTypes',
        subject: partyTypeSubjects.list_party_types_request,
        rows: 'types',
    },
    {
        key: 'partyStatuses',
        subject: partyStatusSubjects.list_party_statuses_request,
        rows: 'statuses',
    },
    {
        key: 'identifierSchemes',
        subject: partyIdSchemeSubjects.list_party_id_schemes_request,
        rows: 'schemes',
    },
    {
        key: 'contactTypes',
        subject: contactTypeSubjects.list_contact_types_request,
        rows: 'types',
    },
    {
        key: 'businessUnitTypes',
        subject: businessUnitTypeSubjects.list_business_unit_types_request,
        rows: 'types',
    },
] as const;

/** Whether a party is one the person may edit: the tenant's System party is not offered. */
function isEditable(party: Record<string, unknown>): boolean {
    return text(party['party_category']).toLowerCase() !== 'system';
}

/**
 * The party details routes: the landing list, each set a party carries, the
 * pick lists, the composite write and read, and the writes on the junctions and
 * the business units.
 *
 * The server searches the party list, so a search is sent to it. The System
 * party is dropped here, because its category is not user-editable.
 */
export function registerPartyDetailsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/party-details', async (request) => {
        const session = requireSession(request);
        const query = input(pageQuerySchema, request.query);
        const all = await readAll(session, partySubjects.list_parties_request, 'parties', {
            filter: { id_one_of: null, search: query.search === '' ? null : query.search },
            as_of: null,
        });
        const found = all
            .filter(isEditable)
            .sort((left, right) =>
                text(left['short_code']).localeCompare(text(right['short_code'])),
            );
        return {
            rows: found.slice(query.offset, query.offset + query.limit),
            total: found.length,
        };
    });

    server.get('/api/party-details/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(session, list.subject, list.rows, { filter: null });
        }
        for (const [key, resource] of [
            ['countries', recordResource('countries')],
            ['currencies', recordResource('currencies')],
            ['businessCentres', recordResource('business-centres')],
        ] as const) {
            if (resource === undefined) {
                throw invalidRequest(`The ${key} list is not registered.`);
            }
            lists[key] = await readAll(session, resource.subjects.list, resource.rows, {
                filter: null,
                ...(resource.asOf ? { as_of: null } : {}),
            });
        }
        lists['counterparties'] = await readAll(
            session,
            counterpartySubjects.list_counterparties_request,
            'counterparties',
            { filter: null, as_of: null },
        );
        return lists;
    });

    server.get('/api/party-details/:id/composite', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const { version } = input(asOfQuerySchema, request.query);
        return session.client.callAuthenticated(
            partySubjects.get_party_composite_as_of_request,
            { id, version },
            z.looseObject({ success: z.boolean(), message: z.string().default('') }),
        );
    });

    server.put('/api/party-details/composite', async (request) => {
        const session = requireSession(request);
        const body = input(compositeBodySchema, request.body);
        return session.client.callAuthenticated(
            partySubjects.put_party_composite_request,
            body,
            z.looseObject({ result: resultSchema, party: row.nullable().default(null) }),
        );
    });

    server.get('/api/party-details/:id/:panel', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const panel = PANELS[input(panelSchema, (request.params as { panel: string }).panel)];
        return {
            rows: await readAll(session, panel.subject, panel.rows, {
                party_id: id,
                scope: 'direct',
                filter: null,
            }),
        };
    });

    /*
     * An identifier's value is part of its key, so a value that changed is a
     * retire of the old row and a write of the new one. The composite writes
     * the new row; this retires the old.
     */
    server.delete('/api/party-details/:id/identifiers', async (request) => {
        const session = requireSession(request);
        paramId(request);
        const body = input(retireBodySchema, request.body);
        const result = await write(
            session,
            partyIdentifierSubjects.delete_party_identifier_request,
            {
                removal: {
                    key: { id_value: body.idValue },
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
        );
        return { result };
    });

    server.put('/api/party-details/:id/business-units', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const body = input(unitBodySchema, request.body);
        if (body.write.party_id !== id) {
            throw invalidRequest('The business unit belongs to another party.');
        }
        const result = await write(session, businessUnitSubjects.put_business_unit_request, {
            change: {
                write: body.write,
                precondition: { kind: 'must_match_version', version: body.version },
            },
            intent: body.intent,
        });
        return { result };
    });

    server.put('/api/party-details/:id/:junction/:far', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const params = request.params as { junction: string; far: string };
        const junction = JUNCTIONS[input(junctionSchema, params.junction)];
        const body = input(linkBodySchema, request.body);
        const result = await write(session, junction.put, {
            change: {
                write: { party_id: id, [junction.far]: params.far },
                precondition: { kind: 'any', version: null },
            },
            intent: body.intent,
        });
        return { result };
    });

    server.delete('/api/party-details/:id/:junction/:far', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const params = request.params as { junction: string; far: string };
        const junction = JUNCTIONS[input(junctionSchema, params.junction)];
        const body = input(linkBodySchema, request.body);
        const result = await write(session, junction.remove, {
            removal: {
                key: { party_id: id, [junction.far]: params.far },
                precondition: { kind: 'any', version: null },
            },
            intent: body.intent,
        });
        return { result };
    });
}

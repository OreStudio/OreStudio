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

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import type { PartyPage, TenantParty } from './domain.js';
import { OperationFailedError } from './errors.js';
import type { ListBusinessCentresRequest } from './generated/refdata/protocol/business_centre_protocol.js';
import type { ListCountriesRequest } from './generated/refdata/protocol/country_protocol.js';
import type { ListPartiesRequest } from './generated/refdata/protocol/party_protocol.js';
import { SUBJECTS, listPartiesReplySchema, resultEnvelopeSchema } from './operations.js';

const centresReplySchema = z.object({
    result: resultEnvelopeSchema,
    centres: z
        .array(z.object({ code: z.string(), country_alpha2_code: z.string().default('') }))
        .default([]),
});

const countriesReplySchema = z.object({
    result: resultEnvelopeSchema,
    countries: z
        .array(
            z.object({
                alpha2_code: z.string(),
                image_id: z
                    .string()
                    .nullish()
                    .transform((value) => (value === undefined || value === '' ? null : value)),
            }),
        )
        .default([]),
});

/**
 * The flag of each business centre named, by the centre's code.
 *
 * A centre has no flag of its own: it sits in a country, and the country
 * carries the flag. A centre or a country that has none is left out, so the
 * caller reads a missing entry as no flag.
 */
async function readCentreFlags(
    caller: AuthenticatedCaller,
    codes: readonly string[],
): Promise<Map<string, string>> {
    const flags = new Map<string, string>();
    if (codes.length === 0) {
        return flags;
    }
    const centresRequest: ListBusinessCentresRequest = {
        offset: 0,
        limit: codes.length,
        order: { field: '', descending: false },
        filter: { code_one_of: [...codes] },
    };
    const centres = await caller.callAuthenticated(
        SUBJECTS.listBusinessCentres,
        centresRequest,
        centresReplySchema,
    );
    if (centres.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listBusinessCentres, centres.result.message);
    }
    const countryCodes = [
        ...new Set(
            centres.centres
                .map((centre) => centre.country_alpha2_code)
                .filter((code) => code !== ''),
        ),
    ];
    if (countryCodes.length === 0) {
        return flags;
    }
    const countriesRequest: ListCountriesRequest = {
        offset: 0,
        limit: countryCodes.length,
        order: { field: '', descending: false },
        filter: { alpha2_code_one_of: countryCodes },
        as_of: null,
    };
    const countries = await caller.callAuthenticated(
        SUBJECTS.listCountries,
        countriesRequest,
        countriesReplySchema,
    );
    if (countries.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listCountries, countries.result.message);
    }
    const countryFlags = new Map<string, string>();
    for (const country of countries.countries) {
        if (country.image_id !== null) {
            countryFlags.set(country.alpha2_code, country.image_id);
        }
    }
    for (const centre of centres.centres) {
        const flag = countryFlags.get(centre.country_alpha2_code);
        if (flag !== undefined) {
            flags.set(centre.code, flag);
        }
    }
    return flags;
}

/** What one page of parties asks for. */
export interface PartyPageQuery {
    readonly offset: number;
    readonly limit: number;
}

/**
 * One page of the parties of the session's own tenant, and how many it holds.
 *
 * The tenant is the session's, so row-level security scopes the read from the
 * token and the request names no tenant. The server pages in key order. A
 * parent on another page is read by its id in one more read that names every
 * such parent. The flags of the page's business centres take two more reads,
 * one for the centres and one for their countries. A parent the session
 * cannot see is not answered, and the row says its parent is elsewhere.
 */
export async function readPartiesPage(
    caller: AuthenticatedCaller,
    query: PartyPageQuery,
): Promise<PartyPage> {
    const pageRequest: ListPartiesRequest = {
        offset: query.offset,
        limit: query.limit,
        order: { field: '', descending: false },
        filter: null,
    };
    const reply = await caller.callAuthenticated(
        SUBJECTS.listParties,
        pageRequest,
        listPartiesReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listParties, reply.result.message);
    }
    const names = new Map<string, string>(
        reply.parties.map((party) => [party.id, party.full_name]),
    );
    const elsewhere = [
        ...new Set(
            reply.parties
                .map((party) => party.parent_party_id)
                .filter((id): id is string => id !== null && !names.has(id)),
        ),
    ];
    if (elsewhere.length > 0) {
        const parentsRequest: ListPartiesRequest = {
            offset: 0,
            limit: elsewhere.length,
            order: { field: '', descending: false },
            filter: { id_one_of: elsewhere },
        };
        const parents = await caller.callAuthenticated(
            SUBJECTS.listParties,
            parentsRequest,
            listPartiesReplySchema,
        );
        if (parents.result.outcome !== 'ok') {
            throw new OperationFailedError(SUBJECTS.listParties, parents.result.message);
        }
        for (const parent of parents.parties) {
            names.set(parent.id, parent.full_name);
        }
    }
    const flags = await readCentreFlags(caller, [
        ...new Set(
            reply.parties.map((party) => party.business_center_code).filter((code) => code !== ''),
        ),
    ]);
    const parties = reply.parties.map((party): TenantParty => ({
        id: party.id,
        code: party.short_code,
        name: party.full_name,
        category: party.party_category,
        type: party.party_type,
        status: party.status,
        parentId: party.parent_party_id,
        parentName:
            party.parent_party_id === null ? null : (names.get(party.parent_party_id) ?? null),
        businessCentreCode: party.business_center_code,
        flagImageId: flags.get(party.business_center_code) ?? null,
    }));
    return { parties, totalCount: reply.total };
}

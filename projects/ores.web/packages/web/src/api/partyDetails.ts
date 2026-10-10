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
import type { BusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/business_centre';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';
import type { BusinessUnitType } from '@ores/wire-protocol/generated/refdata/domain/business_unit_type';
import type { ContactType } from '@ores/wire-protocol/generated/refdata/domain/contact_type';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { Country } from '@ores/wire-protocol/generated/refdata/domain/country';
import type { Currency } from '@ores/wire-protocol/generated/refdata/domain/currency';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import type { PartyContactInformation } from '@ores/wire-protocol/generated/refdata/domain/party_contact_information';
import type { PartyCounterparty } from '@ores/wire-protocol/generated/refdata/domain/party_counterparty';
import type { PartyCountry } from '@ores/wire-protocol/generated/refdata/domain/party_country';
import type { PartyCurrency } from '@ores/wire-protocol/generated/refdata/domain/party_currency';
import type { PartyIdScheme } from '@ores/wire-protocol/generated/refdata/domain/party_id_scheme';
import type { PartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/party_identifier';
import type { PartyStatus } from '@ores/wire-protocol/generated/refdata/domain/party_status';
import type { PartyType } from '@ores/wire-protocol/generated/refdata/domain/party_type';
import { request } from './transport.js';

/**
 * The party details screen's reads and writes.
 *
 * The BFF passes the refdata service's rows through under the service's own
 * field names, so a row here is typed from the generated domain type and the
 * parse checks only that it is an object with a version. The generated type is
 * the contract; a model change that renames a field fails the typecheck where a
 * screen reads it, not at this parse.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/** Why a write is made. */
export interface PartyIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The result a write carries. */
export interface PartyResult {
    readonly outcome:
        'ok' | 'invalid' | 'denied' | 'missing' | 'conflict' | 'unavailable' | 'failed';
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/** The outcome of a write as the screen renders it. */
export interface PartyWriteOutcome {
    readonly success: boolean;
    readonly code: string;
    readonly message: string;
    readonly fields: PartyResult['fields'];
    readonly party: Party | undefined;
}

/** One page of the tenant's parties, and the total the search matches. */
export interface PartyPage {
    readonly rows: readonly Party[];
    readonly total: number;
}

/** A set a party carries, read on its own. */
export interface PartySets {
    readonly identifiers: readonly PartyIdentifier[];
    readonly contacts: readonly PartyContactInformation[];
    readonly countries: readonly PartyCountry[];
    readonly currencies: readonly PartyCurrency[];
    readonly counterparties: readonly PartyCounterparty[];
    readonly units: readonly BusinessUnit[];
}

/** The reference data the screen's pickers draw from. */
export interface PartyPickLists {
    readonly partyTypes: readonly PartyType[];
    readonly partyStatuses: readonly PartyStatus[];
    readonly identifierSchemes: readonly PartyIdScheme[];
    readonly contactTypes: readonly ContactType[];
    readonly businessUnitTypes: readonly BusinessUnitType[];
    readonly countries: readonly Country[];
    readonly currencies: readonly Currency[];
    readonly businessCentres: readonly BusinessCentre[];
    readonly counterparties: readonly Counterparty[];
}

/** A party with its identifiers and contacts as they stood in one version's window. */
export interface PartyComposite {
    readonly party: Party;
    readonly identifiers: readonly PartyIdentifier[];
    readonly contacts: readonly PartyContactInformation[];
}

/** The composite write the confirm sends. */
export interface PartyCompositeRequest {
    readonly intent: PartyIntent;
    readonly party: Party;
    readonly identifiers: readonly PartyIdentifier[];
    readonly contacts: readonly PartyContactInformation[];
}

const resultSchema: z.ZodType<PartyResult> = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
    fields: z.array(z.object({ field: z.string(), code: z.string(), message: z.string() })),
});

/** A row the BFF passes through: an object with a version, typed by the caller. */
function rowsOf<Row>(): z.ZodType<readonly Row[]> {
    return z.array(z.looseObject({ version: z.int() })) as unknown as z.ZodType<readonly Row[]>;
}

const partySchema = z.looseObject({
    version: z.int(),
    id: z.string(),
}) as unknown as z.ZodType<Party>;

function writeOutcome(result: PartyResult, party: Party | undefined): PartyWriteOutcome {
    return {
        success: result.outcome === 'ok',
        code: result.code,
        message: result.message,
        fields: result.fields,
        party: result.outcome === 'ok' ? party : undefined,
    };
}

function base(partyId: string): string {
    return `/api/party-details/${encodeURIComponent(partyId)}`;
}

async function link(
    method: 'PUT' | 'DELETE',
    partyId: string,
    junction: 'countries' | 'currencies' | 'counterparties',
    far: string,
    intent: PartyIntent,
): Promise<PartyWriteOutcome> {
    const answer = z.object({ result: resultSchema }).parse(
        await request(`${base(partyId)}/${junction}/${encodeURIComponent(far)}`, {
            method,
            headers: JSON_HEADERS,
            body: JSON.stringify({ intent }),
        }),
    );
    return writeOutcome(answer.result, undefined);
}

export const partyDetails = {
    /** One page of the tenant's parties, searched on the server. */
    async page(query: {
        readonly offset: number;
        readonly limit: number;
        readonly search: string;
    }): Promise<PartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
            search: query.search,
        });
        return z
            .object({ rows: rowsOf<Party>(), total: z.int().nonnegative() })
            .parse(await request(`/api/party-details?${params.toString()}`, { method: 'GET' }));
    },

    /** Every set one party carries, each read on its own. */
    async sets(partyId: string): Promise<PartySets> {
        const read = async <Row>(panel: string): Promise<readonly Row[]> =>
            z
                .object({ rows: rowsOf<Row>() })
                .parse(await request(`${base(partyId)}/${panel}`, { method: 'GET' })).rows;
        return {
            identifiers: await read<PartyIdentifier>('identifiers'),
            contacts: await read<PartyContactInformation>('contacts'),
            countries: await read<PartyCountry>('countries'),
            currencies: await read<PartyCurrency>('currencies'),
            counterparties: await read<PartyCounterparty>('counterparties'),
            units: await read<BusinessUnit>('units'),
        };
    },

    /** Every list a picker draws from. */
    async pickLists(): Promise<PartyPickLists> {
        return z
            .object({
                partyTypes: rowsOf<PartyType>(),
                partyStatuses: rowsOf<PartyStatus>(),
                identifierSchemes: rowsOf<PartyIdScheme>(),
                contactTypes: rowsOf<ContactType>(),
                businessUnitTypes: rowsOf<BusinessUnitType>(),
                countries: rowsOf<Country>(),
                currencies: rowsOf<Currency>(),
                businessCentres: rowsOf<BusinessCentre>(),
                counterparties: rowsOf<Counterparty>(),
            })
            .parse(await request('/api/party-details/pick-lists', { method: 'GET' }));
    },

    /** The party with its identifiers and contacts as they stood in one version's window. */
    async compositeAsOf(partyId: string, version: number): Promise<PartyComposite> {
        const answer = z
            .looseObject({
                success: z.boolean(),
                message: z.string(),
                party: partySchema,
                identifiers: rowsOf<PartyIdentifier>(),
                contacts: rowsOf<PartyContactInformation>(),
            })
            .parse(
                await request(`${base(partyId)}/composite?version=${String(version)}`, {
                    method: 'GET',
                }),
            );
        if (!answer.success) {
            throw new Error(answer.message);
        }
        return { party: answer.party, identifiers: answer.identifiers, contacts: answer.contacts };
    },

    /** Writes the party and the identifiers and contacts the review staged, as one act. */
    async writeComposite(body: PartyCompositeRequest): Promise<PartyWriteOutcome> {
        const answer = z.object({ result: resultSchema, party: partySchema.nullable() }).parse(
            await request('/api/party-details/composite', {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(body),
            }),
        );
        return writeOutcome(answer.result, answer.party ?? undefined);
    },

    /** Retires an identifier whose value changed. */
    async retireIdentifier(
        partyId: string,
        idValue: string,
        version: number,
        intent: PartyIntent,
    ): Promise<PartyWriteOutcome> {
        const answer = z.object({ result: resultSchema }).parse(
            await request(`${base(partyId)}/identifiers`, {
                method: 'DELETE',
                headers: JSON_HEADERS,
                body: JSON.stringify({ idValue, version, intent }),
            }),
        );
        return writeOutcome(answer.result, undefined);
    },

    /** Links a country, a currency or a counterparty to the party. */
    linkMembership(
        partyId: string,
        junction: 'countries' | 'currencies' | 'counterparties',
        far: string,
        intent: PartyIntent,
    ): Promise<PartyWriteOutcome> {
        return link('PUT', partyId, junction, far, intent);
    },

    /** Closes a country or currency membership, or unlinks a counterparty. */
    closeMembership(
        partyId: string,
        junction: 'countries' | 'currencies' | 'counterparties',
        far: string,
        intent: PartyIntent,
    ): Promise<PartyWriteOutcome> {
        return link('DELETE', partyId, junction, far, intent);
    },

    /** Reclassifies a business unit against the version the person read. */
    async reclassifyUnit(
        unit: BusinessUnit,
        unitTypeId: string | null,
        intent: PartyIntent,
    ): Promise<PartyWriteOutcome> {
        const { version, ...fields } = unit;
        const written = {
            id: fields.id,
            party_id: fields.party_id,
            unit_name: fields.unit_name,
            parent_business_unit_id: fields.parent_business_unit_id,
            unit_code: fields.unit_code,
            business_centre_code: fields.business_centre_code,
            unit_type_id: unitTypeId,
            status: fields.status,
        };
        const answer = z.object({ result: resultSchema }).parse(
            await request(`${base(unit.party_id)}/business-units`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ intent, version, write: written }),
            }),
        );
        return writeOutcome(answer.result, undefined);
    },
};

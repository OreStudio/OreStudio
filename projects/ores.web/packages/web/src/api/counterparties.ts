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
import type { ContactType } from '@ores/wire-protocol/generated/refdata/domain/contact_type';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { CounterpartyBusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/counterparty_business_centre';
import type { CounterpartyContactInformation } from '@ores/wire-protocol/generated/refdata/domain/counterparty_contact_information';
import type { CounterpartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/counterparty_identifier';
import type { Currency } from '@ores/wire-protocol/generated/refdata/domain/currency';
import type { PartyCounterparty } from '@ores/wire-protocol/generated/refdata/domain/party_counterparty';
import type { PartyIdScheme } from '@ores/wire-protocol/generated/refdata/domain/party_id_scheme';
import type { PartyStatus } from '@ores/wire-protocol/generated/refdata/domain/party_status';
import type { PartyType } from '@ores/wire-protocol/generated/refdata/domain/party_type';
import type { PutCounterpartyCompositeRequest } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { request } from './transport.js';

/**
 * The counterparty onboarding screen's reads and writes.
 *
 * The BFF owns the session and the reference data service, so this module only
 * states the HTTP paths the screen calls and parses every answer with a schema
 * that mirrors the generated wire shapes. A field name here is the C++ member
 * name, because that is the key the server serialises.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/**
 * Why a write is made.
 *
 * The shape is the generated one, restated here because the utility protocol
 * that declares it is not on the package's browser-safe surface.
 */
export interface CounterpartyIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The result a write carries, restated for the same reason as the intent. */
export interface CounterpartyResult {
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

/** The filters one page of counterparties is read with. */
export interface CounterpartyPageQuery {
    readonly offset: number;
    readonly limit: number;
    readonly search: string;
    readonly status: 'active' | 'closed' | 'all';
}

/** One page of counterparties and the total the filter matches. */
export interface CounterpartyPage {
    readonly rows: readonly Counterparty[];
    readonly total: number;
}

/** The children of one counterparty, read together for the review screen. */
export interface CounterpartyChildren {
    readonly counterpartyId: string;
    readonly identifiers: readonly CounterpartyIdentifier[];
    readonly contacts: readonly CounterpartyContactInformation[];
    readonly centres: readonly CounterpartyBusinessCentre[];
}

/** The result of a write as the screen renders it: the code, the field failures and the row. */
export interface CounterpartyWriteOutcome {
    readonly success: boolean;
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
    readonly counterparty: Counterparty | undefined;
}

/** The reference data a counterparty form is filled from. */
export interface CounterpartyPickLists {
    readonly partyTypes: readonly PartyType[];
    readonly partyStatuses: readonly PartyStatus[];
    readonly businessCentres: readonly BusinessCentre[];
    readonly identifierSchemes: readonly PartyIdScheme[];
    readonly contactTypes: readonly ContactType[];
    readonly currencies: readonly Currency[];
}

const counterpartySchema: z.ZodType<Counterparty> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    id: z.string(),
    short_code: z.string(),
    full_name: z.string(),
    transliterated_name: z.string().nullable(),
    party_type: z.string(),
    parent_counterparty_id: z.string().nullable(),
    status: z.string(),
    image_id: z.string().nullable(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const counterpartyIdentifierSchema: z.ZodType<CounterpartyIdentifier> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    id: z.string(),
    counterparty_id: z.string(),
    id_scheme: z.string(),
    id_value: z.string(),
    description: z.string(),
    is_authoritative: z.boolean(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const counterpartyContactInformationSchema: z.ZodType<CounterpartyContactInformation> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    id: z.string(),
    counterparty_id: z.string(),
    contact_type: z.string(),
    street_line_1: z.string(),
    street_line_2: z.string(),
    city: z.string(),
    state: z.string(),
    country_code: z.string(),
    postal_code: z.string(),
    phone: z.string(),
    email: z.string(),
    web_page: z.string(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const counterpartyBusinessCentreSchema: z.ZodType<CounterpartyBusinessCentre> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    counterparty_id: z.string(),
    business_centre_code: z.string(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const partyCounterpartySchema: z.ZodType<PartyCounterparty> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    party_id: z.string(),
    counterparty_id: z.string(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const partyTypeSchema: z.ZodType<PartyType> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    code: z.string(),
    name: z.string(),
    description: z.string(),
    display_order: z.int(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const partyStatusSchema: z.ZodType<PartyStatus> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    code: z.string(),
    name: z.string(),
    description: z.string(),
    display_order: z.int(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const businessCentreSchema: z.ZodType<BusinessCentre> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    code: z.string(),
    source: z.string(),
    description: z.string(),
    city_name: z.string(),
    country_alpha2_code: z.string(),
    coding_scheme_code: z.string(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const partyIdSchemeSchema: z.ZodType<PartyIdScheme> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    code: z.string(),
    name: z.string(),
    description: z.string(),
    coding_scheme_code: z.string(),
    display_order: z.int(),
    max_cardinality: z.int().nullable(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const contactTypeSchema: z.ZodType<ContactType> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    code: z.string(),
    name: z.string(),
    description: z.string(),
    display_order: z.int(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const currencySchema: z.ZodType<Currency> = z.object({
    version: z.int(),
    tenant_id: z.string(),
    iso_code: z.string(),
    name: z.string(),
    numeric_code: z.string(),
    symbol: z.string(),
    fraction_symbol: z.string(),
    fractions_per_unit: z.int(),
    rounding_type: z.string(),
    rounding_precision: z.int(),
    format: z.string(),
    monetary_nature: z.string(),
    market_tier: z.string(),
    ore_currency_type: z.string().nullable(),
    image_id: z.string().nullable(),
    spot_days: z.int(),
    day_basis: z.string(),
    base_precedence: z.int(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

const counterpartyChildrenSchema: z.ZodType<CounterpartyChildren> = z.object({
    counterpartyId: z.string(),
    identifiers: z.array(counterpartyIdentifierSchema),
    contacts: z.array(counterpartyContactInformationSchema),
    centres: z.array(counterpartyBusinessCentreSchema),
});

const resultSchema: z.ZodType<CounterpartyResult> = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
    fields: z.array(
        z.object({
            field: z.string(),
            code: z.string(),
            message: z.string(),
        }),
    ),
});

/** The outcome a write reports, shared by the composite and the child writes. */
function writeOutcome(
    result: CounterpartyResult,
    counterparty: Counterparty | undefined,
): CounterpartyWriteOutcome {
    return {
        success: result.outcome === 'ok',
        code: result.code,
        message: result.message,
        fields: result.fields,
        counterparty: result.outcome === 'ok' ? counterparty : undefined,
    };
}

export const counterparties = {
    /** One page of counterparties, searched and filtered on the server. */
    async page(query: CounterpartyPageQuery): Promise<CounterpartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
            search: query.search,
            status: query.status,
        });
        return z
            .object({ rows: z.array(counterpartySchema), total: z.int().nonnegative() })
            .parse(await request(`/api/counterparties?${params.toString()}`, { method: 'GET' }));
    },

    /** The identifiers, contacts and centres of the named counterparties. */
    async children(ids: readonly string[]): Promise<readonly CounterpartyChildren[]> {
        if (ids.length === 0) {
            return [];
        }
        const query = new URLSearchParams({ ids: ids.join(',') });
        return z.object({ children: z.array(counterpartyChildrenSchema) }).parse(
            await request(`/api/counterparties/children?${query.toString()}`, {
                method: 'GET',
            }),
        ).children;
    },

    /** Which parties may see one counterparty. */
    async visibility(counterpartyId: string): Promise<readonly PartyCounterparty[]> {
        return z
            .object({ partyCounterparties: z.array(partyCounterpartySchema) })
            .parse(
                await request(
                    `/api/counterparties/${encodeURIComponent(counterpartyId)}/visibility`,
                    { method: 'GET' },
                ),
            ).partyCounterparties;
    },

    /** The reference data the onboarding form's pickers are filled from. */
    async pickLists(): Promise<CounterpartyPickLists> {
        return z
            .object({
                partyTypes: z.array(partyTypeSchema),
                partyStatuses: z.array(partyStatusSchema),
                businessCentres: z.array(businessCentreSchema),
                identifierSchemes: z.array(partyIdSchemeSchema),
                contactTypes: z.array(contactTypeSchema),
                currencies: z.array(currencySchema),
            })
            .parse(await request('/api/counterparties/pick-lists', { method: 'GET' }));
    },

    /** Writes a counterparty and its staged children in one transaction. */
    async writeComposite(
        request_: PutCounterpartyCompositeRequest,
    ): Promise<CounterpartyWriteOutcome> {
        const answer = z.object({ result: resultSchema, counterparty: counterpartySchema }).parse(
            await request('/api/counterparties/composite', {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(request_),
            }),
        );
        return writeOutcome(answer.result, answer.counterparty);
    },

    /** Replaces which business centres one counterparty trades in. */
    async writeBusinessCentres(
        counterpartyId: string,
        codes: readonly string[],
        intent: CounterpartyIntent,
    ): Promise<CounterpartyWriteOutcome> {
        const answer = z.object({ result: resultSchema }).parse(
            await request(
                `/api/counterparties/${encodeURIComponent(counterpartyId)}/business-centres`,
                {
                    method: 'PUT',
                    headers: JSON_HEADERS,
                    body: JSON.stringify({ codes, intent }),
                },
            ),
        );
        return writeOutcome(answer.result, undefined);
    },

    /** Replaces which parties may see one counterparty. */
    async writeVisibility(
        partyId: string,
        counterpartyId: string,
        intent: CounterpartyIntent,
    ): Promise<CounterpartyWriteOutcome> {
        const answer = z.object({ result: resultSchema }).parse(
            await request(`/api/counterparties/${encodeURIComponent(counterpartyId)}/visibility`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ partyId, intent }),
            }),
        );
        return writeOutcome(answer.result, undefined);
    },
};

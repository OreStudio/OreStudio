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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { Party } from '../domain/party.js';
import type { PartyContactInformation } from '../domain/party_contact_information.js';
import type { PartyIdentifier } from '../domain/party_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyKey {
    id: string;
}

export interface PartyWrite {
    id: string;
    short_code: string;
    full_name: string;
    codename: string;
    transliterated_name: string | null;
    party_category: string;
    party_type: string;
    parent_party_id: string | null;
    business_center_code: string;
    status: string;
    image_id: string | null;
}

export interface PartyChange {
    write: PartyWrite;
    precondition: Precondition;
}

export interface PartyRemoval {
    key: PartyKey;
    precondition: Precondition;
}

export interface PartyLookup {
    key: PartyKey;
    party: Party | null;
}

export interface PartyEvent {
    event_id: string;
    key: PartyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyVersionKey {
    party: PartyKey;
    version: number;
}

export interface PartyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPartiesResponse {
    result: Result;
    parties: Party[];
    total: number;
}

export interface GetPartyRequest {
    key: PartyKey;
}

export interface GetPartyResponse {
    result: Result;
    party: Party | null;
}

export interface GetManyPartiesRequest {
    keys: PartyKey[];
}

export interface GetManyPartiesResponse {
    result: Result;
    entries: PartyLookup[];
}

export interface PutPartyRequest {
    change: PartyChange;
    intent: ChangeIntent;
}

export interface PutPartyResponse {
    result: Result;
    party: Party;
}

export interface PutManyPartiesRequest {
    changes: PartyChange[];
    intent: ChangeIntent;
}

export interface PutManyPartiesResponse {
    result: Result;
    parties: Party[];
}

export interface DeletePartyRequest {
    removal: PartyRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyResponse {
    result: Result;
}

export interface DeleteManyPartiesRequest {
    removals: PartyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartiesResponse {
    result: Result;
}

export interface ListPartyVersionsRequest {
    key: PartyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyVersionsFilter | null;
}

export interface ListPartyVersionsResponse {
    result: Result;
    versions: Party[];
    total: number;
}

export interface GetPartyVersionRequest {
    key: PartyVersionKey;
}

export interface GetPartyVersionResponse {
    result: Result;
    version: Party;
}

/**
 * @brief Reads a party as it stood at a specific version, together with its
 * identifiers and contact information as they stood during that same
 * version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
export interface GetPartyCompositeAsOfRequest {
    id: string;
    version: number;
}

export interface GetPartyCompositeAsOfResponse {
    success: boolean;
    message: string;
    party: Party;
    identifiers: PartyIdentifier[];
    contacts: PartyContactInformation[];
}

export const subjects = {
    list_parties_request: "refdata.v1.parties.list",
    get_party_request: "refdata.v1.parties.get",
    get_many_parties_request: "refdata.v1.parties.get_many",
    put_party_request: "refdata.v1.parties.put",
    put_many_parties_request: "refdata.v1.parties.put_many",
    delete_party_request: "refdata.v1.parties.delete",
    delete_many_parties_request: "refdata.v1.parties.delete_many",
    list_party_versions_request: "refdata.v1.parties_versions.list",
    get_party_version_request: "refdata.v1.parties_versions.get",
    get_party_composite_as_of_request: "refdata.v1.parties.composite_as_of",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_parties_request: true,
    get_party_request: true,
    get_many_parties_request: true,
    put_party_request: true,
    put_many_parties_request: true,
    delete_party_request: true,
    delete_many_parties_request: true,
    list_party_versions_request: true,
    get_party_version_request: true,
    get_party_composite_as_of_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.parties_events.created",
    updated: "refdata.v1.parties_events.updated",
    deleted: "refdata.v1.parties_events.deleted",
} as const;

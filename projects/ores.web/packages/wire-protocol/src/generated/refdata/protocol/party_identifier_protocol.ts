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
import type { PartyIdentifier } from '../domain/party_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyIdentifierKey {
    id: string;
}

export interface PartyIdentifierWrite {
    id: string;
    id_scheme: string;
    id_value: string;
    description: string;
}

export interface PartyIdentifierChange {
    write: PartyIdentifierWrite;
    precondition: Precondition;
}

export interface PartyIdentifierRemoval {
    key: PartyIdentifierKey;
    precondition: Precondition;
}

export interface PartyIdentifierLookup {
    key: PartyIdentifierKey;
    party_identifier: PartyIdentifier | null;
}

export interface PartyIdentifiersFilter {
    party_id: string | null;
}

export interface PartyIdentifierEvent {
    event_id: string;
    key: PartyIdentifierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyIdentifierVersionKey {
    party_identifier: PartyIdentifierKey;
    version: number;
}

export interface PartyIdentifierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyIdentifiersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyIdentifiersFilter | null;
}

export interface ListPartyIdentifiersResponse {
    result: Result;
    party_identifiers: PartyIdentifier[];
    total: number;
}

export interface GetPartyIdentifierRequest {
    key: PartyIdentifierKey;
}

export interface GetPartyIdentifierResponse {
    result: Result;
    party_identifier: PartyIdentifier | null;
}

export interface GetManyPartyIdentifiersRequest {
    keys: PartyIdentifierKey[];
}

export interface GetManyPartyIdentifiersResponse {
    result: Result;
    entries: PartyIdentifierLookup[];
}

export interface PutPartyIdentifierRequest {
    change: PartyIdentifierChange;
    intent: ChangeIntent;
}

export interface PutPartyIdentifierResponse {
    result: Result;
    party_identifier: PartyIdentifier;
}

export interface PutManyPartyIdentifiersRequest {
    changes: PartyIdentifierChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyIdentifiersResponse {
    result: Result;
    party_identifiers: PartyIdentifier[];
}

export interface DeletePartyIdentifierRequest {
    removal: PartyIdentifierRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyIdentifierResponse {
    result: Result;
}

export interface DeleteManyPartyIdentifiersRequest {
    removals: PartyIdentifierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyIdentifiersResponse {
    result: Result;
}

export interface ListByPartyIdPartyIdentifiersRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyIdentifiersFilter | null;
}

export interface ListByPartyIdPartyIdentifiersResponse {
    result: Result;
    party_identifiers: PartyIdentifier[];
    total: number;
}

export interface ListPartyIdentifierVersionsRequest {
    key: PartyIdentifierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyIdentifierVersionsFilter | null;
}

export interface ListPartyIdentifierVersionsResponse {
    result: Result;
    versions: PartyIdentifier[];
    total: number;
}

export interface GetPartyIdentifierVersionRequest {
    key: PartyIdentifierVersionKey;
}

export interface GetPartyIdentifierVersionResponse {
    result: Result;
    version: PartyIdentifier;
}

export const subjects = {
    list_party_identifiers_request: "refdata.v1.party_identifiers.list",
    get_party_identifier_request: "refdata.v1.party_identifiers.get",
    get_many_party_identifiers_request: "refdata.v1.party_identifiers.get_many",
    put_party_identifier_request: "refdata.v1.party_identifiers.put",
    put_many_party_identifiers_request: "refdata.v1.party_identifiers.put_many",
    delete_party_identifier_request: "refdata.v1.party_identifiers.delete",
    delete_many_party_identifiers_request: "refdata.v1.party_identifiers.delete_many",
    list_by_party_id_party_identifiers_request: "refdata.v1.party_identifiers.list_by_party_id",
    list_party_identifier_versions_request: "refdata.v1.party_identifiers_versions.list",
    get_party_identifier_version_request: "refdata.v1.party_identifiers_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_identifiers_request: true,
    get_party_identifier_request: true,
    get_many_party_identifiers_request: true,
    put_party_identifier_request: true,
    put_many_party_identifiers_request: true,
    delete_party_identifier_request: true,
    delete_many_party_identifiers_request: true,
    list_by_party_id_party_identifiers_request: true,
    list_party_identifier_versions_request: true,
    get_party_identifier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.party_identifiers_events.created",
    updated: "refdata.v1.party_identifiers_events.updated",
    deleted: "refdata.v1.party_identifiers_events.deleted",
} as const;

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
import type { PartyType } from '../domain/party_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyTypeKey {
    code: string;
}

export interface PartyTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface PartyTypeChange {
    write: PartyTypeWrite;
    precondition: Precondition;
}

export interface PartyTypeRemoval {
    key: PartyTypeKey;
    precondition: Precondition;
}

export interface PartyTypeLookup {
    key: PartyTypeKey;
    party_type: PartyType | null;
}

export interface PartyTypeEvent {
    event_id: string;
    key: PartyTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyTypeVersionKey {
    party_type: PartyTypeKey;
    version: number;
}

export interface PartyTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPartyTypesResponse {
    result: Result;
    types: PartyType[];
    total: number;
}

export interface GetPartyTypeRequest {
    key: PartyTypeKey;
}

export interface GetPartyTypeResponse {
    result: Result;
    party_type: PartyType | null;
}

export interface GetManyPartyTypesRequest {
    keys: PartyTypeKey[];
}

export interface GetManyPartyTypesResponse {
    result: Result;
    entries: PartyTypeLookup[];
}

export interface PutPartyTypeRequest {
    change: PartyTypeChange;
    intent: ChangeIntent;
}

export interface PutPartyTypeResponse {
    result: Result;
    party_type: PartyType;
}

export interface PutManyPartyTypesRequest {
    changes: PartyTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyTypesResponse {
    result: Result;
    types: PartyType[];
}

export interface DeletePartyTypeRequest {
    removal: PartyTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyTypeResponse {
    result: Result;
}

export interface DeleteManyPartyTypesRequest {
    removals: PartyTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyTypesResponse {
    result: Result;
}

export interface ListPartyTypeVersionsRequest {
    key: PartyTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyTypeVersionsFilter | null;
}

export interface ListPartyTypeVersionsResponse {
    result: Result;
    versions: PartyType[];
    total: number;
}

export interface GetPartyTypeVersionRequest {
    key: PartyTypeVersionKey;
}

export interface GetPartyTypeVersionResponse {
    result: Result;
    version: PartyType;
}

export const subjects = {
    list_party_types_request: "refdata.v1.party_types.list",
    get_party_type_request: "refdata.v1.party_types.get",
    get_many_party_types_request: "refdata.v1.party_types.get_many",
    put_party_type_request: "refdata.v1.party_types.put",
    put_many_party_types_request: "refdata.v1.party_types.put_many",
    delete_party_type_request: "refdata.v1.party_types.delete",
    delete_many_party_types_request: "refdata.v1.party_types.delete_many",
    list_party_type_versions_request: "refdata.v1.party_types_versions.list",
    get_party_type_version_request: "refdata.v1.party_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_types_request: true,
    get_party_type_request: true,
    get_many_party_types_request: true,
    put_party_type_request: true,
    put_many_party_types_request: true,
    delete_party_type_request: true,
    delete_many_party_types_request: true,
    list_party_type_versions_request: true,
    get_party_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.party_types_events.created",
    updated: "refdata.v1.party_types_events.updated",
    deleted: "refdata.v1.party_types_events.deleted",
} as const;

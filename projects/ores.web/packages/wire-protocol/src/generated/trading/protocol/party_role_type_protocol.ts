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
import type { PartyRoleType } from '../domain/party_role_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyRoleTypeKey {
    code: string;
}

export interface PartyRoleTypeWrite {
    code: string;
    description: string;
}

export interface PartyRoleTypeChange {
    write: PartyRoleTypeWrite;
    precondition: Precondition;
}

export interface PartyRoleTypeRemoval {
    key: PartyRoleTypeKey;
    precondition: Precondition;
}

export interface PartyRoleTypeLookup {
    key: PartyRoleTypeKey;
    party_role_type: PartyRoleType | null;
}

export interface PartyRoleTypesFilter {
    code_one_of: string[] | null;
}

export interface PartyRoleTypeEvent {
    event_id: string;
    key: PartyRoleTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyRoleTypeVersionKey {
    party_role_type: PartyRoleTypeKey;
    version: number;
}

export interface PartyRoleTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyRoleTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyRoleTypesFilter | null;
    as_of: string | null;
}

export interface ListPartyRoleTypesResponse {
    result: Result;
    role_types: PartyRoleType[];
    total: number;
}

export interface GetPartyRoleTypeRequest {
    key: PartyRoleTypeKey;
}

export interface GetPartyRoleTypeResponse {
    result: Result;
    party_role_type: PartyRoleType | null;
}

export interface GetManyPartyRoleTypesRequest {
    keys: PartyRoleTypeKey[];
}

export interface GetManyPartyRoleTypesResponse {
    result: Result;
    entries: PartyRoleTypeLookup[];
}

export interface PutPartyRoleTypeRequest {
    change: PartyRoleTypeChange;
    intent: ChangeIntent;
}

export interface PutPartyRoleTypeResponse {
    result: Result;
    party_role_type: PartyRoleType | null;
}

export interface PutManyPartyRoleTypesRequest {
    changes: PartyRoleTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyRoleTypesResponse {
    result: Result;
    role_types: PartyRoleType[];
}

export interface DeletePartyRoleTypeRequest {
    removal: PartyRoleTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyRoleTypeResponse {
    result: Result;
}

export interface DeleteManyPartyRoleTypesRequest {
    removals: PartyRoleTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyRoleTypesResponse {
    result: Result;
}

export interface ListPartyRoleTypeVersionsRequest {
    key: PartyRoleTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyRoleTypeVersionsFilter | null;
}

export interface ListPartyRoleTypeVersionsResponse {
    result: Result;
    versions: PartyRoleType[];
    total: number;
}

export interface GetPartyRoleTypeVersionRequest {
    key: PartyRoleTypeVersionKey;
}

export interface GetPartyRoleTypeVersionResponse {
    result: Result;
    version: PartyRoleType | null;
}

export const subjects = {
    list_party_role_types_request: 'trading.v1.party_role_types.list',
    get_party_role_type_request: 'trading.v1.party_role_types.get',
    get_many_party_role_types_request: 'trading.v1.party_role_types.get_many',
    put_party_role_type_request: 'trading.v1.party_role_types.put',
    put_many_party_role_types_request: 'trading.v1.party_role_types.put_many',
    delete_party_role_type_request: 'trading.v1.party_role_types.delete',
    delete_many_party_role_types_request: 'trading.v1.party_role_types.delete_many',
    list_party_role_type_versions_request: 'trading.v1.party_role_types_versions.list',
    get_party_role_type_version_request: 'trading.v1.party_role_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_role_types_request: true,
    get_party_role_type_request: true,
    get_many_party_role_types_request: true,
    put_party_role_type_request: true,
    put_many_party_role_types_request: true,
    delete_party_role_type_request: true,
    delete_many_party_role_types_request: true,
    list_party_role_type_versions_request: true,
    get_party_role_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.party_role_types_events.created',
    updated: 'trading.v1.party_role_types_events.updated',
    deleted: 'trading.v1.party_role_types_events.deleted',
} as const;

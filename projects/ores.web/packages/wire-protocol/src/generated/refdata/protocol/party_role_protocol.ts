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
import type { PartyRole } from '../domain/party_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyRoleKey {
    code: string;
    coding_scheme_code: string;
}

export interface PartyRoleWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface PartyRoleChange {
    write: PartyRoleWrite;
    precondition: Precondition;
}

export interface PartyRoleRemoval {
    key: PartyRoleKey;
    precondition: Precondition;
}

export interface PartyRoleLookup {
    key: PartyRoleKey;
    party_role: PartyRole | null;
}

export interface PartyRoleEvent {
    event_id: string;
    key: PartyRoleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyRoleVersionKey {
    party_role: PartyRoleKey;
    version: number;
}

export interface PartyRoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyRolesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListPartyRolesResponse {
    result: Result;
    roles: PartyRole[];
    total: number;
}

export interface GetPartyRoleRequest {
    key: PartyRoleKey;
}

export interface GetPartyRoleResponse {
    result: Result;
    party_role: PartyRole | null;
}

export interface GetManyPartyRolesRequest {
    keys: PartyRoleKey[];
}

export interface GetManyPartyRolesResponse {
    result: Result;
    entries: PartyRoleLookup[];
}

export interface PutPartyRoleRequest {
    change: PartyRoleChange;
    intent: ChangeIntent;
}

export interface PutPartyRoleResponse {
    result: Result;
    party_role: PartyRole | null;
}

export interface PutManyPartyRolesRequest {
    changes: PartyRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyRolesResponse {
    result: Result;
    roles: PartyRole[];
}

export interface DeletePartyRoleRequest {
    removal: PartyRoleRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyRoleResponse {
    result: Result;
}

export interface DeleteManyPartyRolesRequest {
    removals: PartyRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyRolesResponse {
    result: Result;
}

export interface ListPartyRoleVersionsRequest {
    key: PartyRoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyRoleVersionsFilter | null;
}

export interface ListPartyRoleVersionsResponse {
    result: Result;
    versions: PartyRole[];
    total: number;
}

export interface GetPartyRoleVersionRequest {
    key: PartyRoleVersionKey;
}

export interface GetPartyRoleVersionResponse {
    result: Result;
    version: PartyRole | null;
}

export const subjects = {
    list_party_roles_request: 'refdata.v1.party_roles.list',
    get_party_role_request: 'refdata.v1.party_roles.get',
    get_many_party_roles_request: 'refdata.v1.party_roles.get_many',
    put_party_role_request: 'refdata.v1.party_roles.put',
    put_many_party_roles_request: 'refdata.v1.party_roles.put_many',
    delete_party_role_request: 'refdata.v1.party_roles.delete',
    delete_many_party_roles_request: 'refdata.v1.party_roles.delete_many',
    list_party_role_versions_request: 'refdata.v1.party_roles_versions.list',
    get_party_role_version_request: 'refdata.v1.party_roles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_roles_request: true,
    get_party_role_request: true,
    get_many_party_roles_request: true,
    put_party_role_request: true,
    put_many_party_roles_request: true,
    delete_party_role_request: true,
    delete_many_party_roles_request: true,
    list_party_role_versions_request: true,
    get_party_role_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.party_roles_events.created',
    updated: 'refdata.v1.party_roles_events.updated',
    deleted: 'refdata.v1.party_roles_events.deleted',
} as const;

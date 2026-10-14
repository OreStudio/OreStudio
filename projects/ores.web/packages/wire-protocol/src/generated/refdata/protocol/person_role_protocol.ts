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
import type { PersonRole } from '../domain/person_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PersonRoleKey {
    code: string;
    coding_scheme_code: string;
}

export interface PersonRoleWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface PersonRoleChange {
    write: PersonRoleWrite;
    precondition: Precondition;
}

export interface PersonRoleRemoval {
    key: PersonRoleKey;
    precondition: Precondition;
}

export interface PersonRoleLookup {
    key: PersonRoleKey;
    person_role: PersonRole | null;
}

export interface PersonRoleEvent {
    event_id: string;
    key: PersonRoleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PersonRoleVersionKey {
    person_role: PersonRoleKey;
    version: number;
}

export interface PersonRoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPersonRolesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListPersonRolesResponse {
    result: Result;
    roles: PersonRole[];
    total: number;
}

export interface GetPersonRoleRequest {
    key: PersonRoleKey;
}

export interface GetPersonRoleResponse {
    result: Result;
    person_role: PersonRole | null;
}

export interface GetManyPersonRolesRequest {
    keys: PersonRoleKey[];
}

export interface GetManyPersonRolesResponse {
    result: Result;
    entries: PersonRoleLookup[];
}

export interface PutPersonRoleRequest {
    change: PersonRoleChange;
    intent: ChangeIntent;
}

export interface PutPersonRoleResponse {
    result: Result;
    person_role: PersonRole | null;
}

export interface PutManyPersonRolesRequest {
    changes: PersonRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyPersonRolesResponse {
    result: Result;
    roles: PersonRole[];
}

export interface DeletePersonRoleRequest {
    removal: PersonRoleRemoval;
    intent: ChangeIntent;
}

export interface DeletePersonRoleResponse {
    result: Result;
}

export interface DeleteManyPersonRolesRequest {
    removals: PersonRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPersonRolesResponse {
    result: Result;
}

export interface ListPersonRoleVersionsRequest {
    key: PersonRoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PersonRoleVersionsFilter | null;
}

export interface ListPersonRoleVersionsResponse {
    result: Result;
    versions: PersonRole[];
    total: number;
}

export interface GetPersonRoleVersionRequest {
    key: PersonRoleVersionKey;
}

export interface GetPersonRoleVersionResponse {
    result: Result;
    version: PersonRole | null;
}

export const subjects = {
    list_person_roles_request: 'refdata.v1.person_roles.list',
    get_person_role_request: 'refdata.v1.person_roles.get',
    get_many_person_roles_request: 'refdata.v1.person_roles.get_many',
    put_person_role_request: 'refdata.v1.person_roles.put',
    put_many_person_roles_request: 'refdata.v1.person_roles.put_many',
    delete_person_role_request: 'refdata.v1.person_roles.delete',
    delete_many_person_roles_request: 'refdata.v1.person_roles.delete_many',
    list_person_role_versions_request: 'refdata.v1.person_roles_versions.list',
    get_person_role_version_request: 'refdata.v1.person_roles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_person_roles_request: true,
    get_person_role_request: true,
    get_many_person_roles_request: true,
    put_person_role_request: true,
    put_many_person_roles_request: true,
    delete_person_role_request: true,
    delete_many_person_roles_request: true,
    list_person_role_versions_request: true,
    get_person_role_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.person_roles_events.created',
    updated: 'refdata.v1.person_roles_events.updated',
    deleted: 'refdata.v1.person_roles_events.deleted',
} as const;

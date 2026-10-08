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
import type { StructureTemplateRole } from '../domain/structure_template_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StructureTemplateRoleKey {
    template_code: string;
    role: string;
}

export interface StructureTemplateRoleWrite {
    template_code: string;
    role: string;
    min_legs: number;
    max_legs: number;
    description: string;
}

export interface StructureTemplateRoleChange {
    write: StructureTemplateRoleWrite;
    precondition: Precondition;
}

export interface StructureTemplateRoleRemoval {
    key: StructureTemplateRoleKey;
    precondition: Precondition;
}

export interface StructureTemplateRoleLookup {
    key: StructureTemplateRoleKey;
    structure_template_role: StructureTemplateRole | null;
}

export interface StructureTemplateRoleEvent {
    event_id: string;
    key: StructureTemplateRoleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureTemplateRoleVersionKey {
    structure_template_role: StructureTemplateRoleKey;
    version: number;
}

export interface StructureTemplateRoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructureTemplateRolesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListStructureTemplateRolesResponse {
    result: Result;
    template_roles: StructureTemplateRole[];
    total: number;
}

export interface GetStructureTemplateRoleRequest {
    key: StructureTemplateRoleKey;
}

export interface GetStructureTemplateRoleResponse {
    result: Result;
    structure_template_role: StructureTemplateRole | null;
}

export interface GetManyStructureTemplateRolesRequest {
    keys: StructureTemplateRoleKey[];
}

export interface GetManyStructureTemplateRolesResponse {
    result: Result;
    entries: StructureTemplateRoleLookup[];
}

export interface PutStructureTemplateRoleRequest {
    change: StructureTemplateRoleChange;
    intent: ChangeIntent;
}

export interface PutStructureTemplateRoleResponse {
    result: Result;
    structure_template_role: StructureTemplateRole | null;
}

export interface PutManyStructureTemplateRolesRequest {
    changes: StructureTemplateRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyStructureTemplateRolesResponse {
    result: Result;
    template_roles: StructureTemplateRole[];
}

export interface DeleteStructureTemplateRoleRequest {
    removal: StructureTemplateRoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureTemplateRoleResponse {
    result: Result;
}

export interface DeleteManyStructureTemplateRolesRequest {
    removals: StructureTemplateRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructureTemplateRolesResponse {
    result: Result;
}

export interface ListStructureTemplateRoleVersionsRequest {
    key: StructureTemplateRoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureTemplateRoleVersionsFilter | null;
}

export interface ListStructureTemplateRoleVersionsResponse {
    result: Result;
    versions: StructureTemplateRole[];
    total: number;
}

export interface GetStructureTemplateRoleVersionRequest {
    key: StructureTemplateRoleVersionKey;
}

export interface GetStructureTemplateRoleVersionResponse {
    result: Result;
    version: StructureTemplateRole | null;
}

export const subjects = {
    list_structure_template_roles_request: 'trading.v1.structure_template_roles.list',
    get_structure_template_role_request: 'trading.v1.structure_template_roles.get',
    get_many_structure_template_roles_request: 'trading.v1.structure_template_roles.get_many',
    put_structure_template_role_request: 'trading.v1.structure_template_roles.put',
    put_many_structure_template_roles_request: 'trading.v1.structure_template_roles.put_many',
    delete_structure_template_role_request: 'trading.v1.structure_template_roles.delete',
    delete_many_structure_template_roles_request: 'trading.v1.structure_template_roles.delete_many',
    list_structure_template_role_versions_request:
        'trading.v1.structure_template_roles_versions.list',
    get_structure_template_role_version_request: 'trading.v1.structure_template_roles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structure_template_roles_request: true,
    get_structure_template_role_request: true,
    get_many_structure_template_roles_request: true,
    put_structure_template_role_request: true,
    put_many_structure_template_roles_request: true,
    delete_structure_template_role_request: true,
    delete_many_structure_template_roles_request: true,
    list_structure_template_role_versions_request: true,
    get_structure_template_role_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structure_template_roles_events.created',
    updated: 'trading.v1.structure_template_roles_events.updated',
    deleted: 'trading.v1.structure_template_roles_events.deleted',
} as const;

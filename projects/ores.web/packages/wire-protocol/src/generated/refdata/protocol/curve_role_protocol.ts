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
import type { CurveRole } from '../domain/curve_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveRoleKey {
    code: string;
}

export interface CurveRoleWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CurveRoleChange {
    write: CurveRoleWrite;
    precondition: Precondition;
}

export interface CurveRoleRemoval {
    key: CurveRoleKey;
    precondition: Precondition;
}

export interface CurveRoleLookup {
    key: CurveRoleKey;
    curve_role: CurveRole | null;
}

export interface CurveRoleEvent {
    event_id: string;
    key: CurveRoleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveRoleVersionKey {
    curve_role: CurveRoleKey;
    version: number;
}

export interface CurveRoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveRolesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveRolesResponse {
    result: Result;
    roles: CurveRole[];
    total: number;
}

export interface GetCurveRoleRequest {
    key: CurveRoleKey;
}

export interface GetCurveRoleResponse {
    result: Result;
    curve_role: CurveRole | null;
}

export interface GetManyCurveRolesRequest {
    keys: CurveRoleKey[];
}

export interface GetManyCurveRolesResponse {
    result: Result;
    entries: CurveRoleLookup[];
}

export interface PutCurveRoleRequest {
    change: CurveRoleChange;
    intent: ChangeIntent;
}

export interface PutCurveRoleResponse {
    result: Result;
    curve_role: CurveRole;
}

export interface PutManyCurveRolesRequest {
    changes: CurveRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveRolesResponse {
    result: Result;
    roles: CurveRole[];
}

export interface DeleteCurveRoleRequest {
    removal: CurveRoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveRoleResponse {
    result: Result;
}

export interface DeleteManyCurveRolesRequest {
    removals: CurveRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveRolesResponse {
    result: Result;
}

export interface ListCurveRoleVersionsRequest {
    key: CurveRoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveRoleVersionsFilter | null;
}

export interface ListCurveRoleVersionsResponse {
    result: Result;
    versions: CurveRole[];
    total: number;
}

export interface GetCurveRoleVersionRequest {
    key: CurveRoleVersionKey;
}

export interface GetCurveRoleVersionResponse {
    result: Result;
    version: CurveRole;
}

export const subjects = {
    list_curve_roles_request: "refdata.v1.curve_roles.list",
    get_curve_role_request: "refdata.v1.curve_roles.get",
    get_many_curve_roles_request: "refdata.v1.curve_roles.get_many",
    put_curve_role_request: "refdata.v1.curve_roles.put",
    put_many_curve_roles_request: "refdata.v1.curve_roles.put_many",
    delete_curve_role_request: "refdata.v1.curve_roles.delete",
    delete_many_curve_roles_request: "refdata.v1.curve_roles.delete_many",
    list_curve_role_versions_request: "refdata.v1.curve_roles_versions.list",
    get_curve_role_version_request: "refdata.v1.curve_roles_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_roles_request: true,
    get_curve_role_request: true,
    get_many_curve_roles_request: true,
    put_curve_role_request: true,
    put_many_curve_roles_request: true,
    delete_curve_role_request: true,
    delete_many_curve_roles_request: true,
    list_curve_role_versions_request: true,
    get_curve_role_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.curve_roles_events.created",
    updated: "refdata.v1.curve_roles_events.updated",
    deleted: "refdata.v1.curve_roles_events.deleted",
} as const;

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
import type { RoleProductType } from '../domain/role_product_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RoleProductTypeKey {
    role_id: string;
    product_type_code: string;
}

export interface RoleProductTypeWrite {
    role_id: string;
    product_type_code: string;
}

export interface RoleProductTypeChange {
    write: RoleProductTypeWrite;
    precondition: Precondition;
}

export interface RoleProductTypeRemoval {
    key: RoleProductTypeKey;
    precondition: Precondition;
}

export interface RoleProductTypeLookup {
    key: RoleProductTypeKey;
    role_product_type: RoleProductType | null;
}

export interface RoleProductTypesFilter {
    role_id: string | null;
    product_type_code: string | null;
    role_id_one_of: string[] | null;
    product_type_code_one_of: string[] | null;
}

export interface RoleProductTypeEvent {
    event_id: string;
    key: RoleProductTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListRoleProductTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RoleProductTypesFilter | null;
}

export interface ListRoleProductTypesResponse {
    result: Result;
    role_product_types: RoleProductType[];
    total: number;
}

export interface GetRoleProductTypeRequest {
    key: RoleProductTypeKey;
}

export interface GetRoleProductTypeResponse {
    result: Result;
    role_product_type: RoleProductType | null;
}

export interface GetManyRoleProductTypesRequest {
    keys: RoleProductTypeKey[];
}

export interface GetManyRoleProductTypesResponse {
    result: Result;
    entries: RoleProductTypeLookup[];
}

export interface PutRoleProductTypeRequest {
    change: RoleProductTypeChange;
    intent: ChangeIntent;
}

export interface PutRoleProductTypeResponse {
    result: Result;
    role_product_type: RoleProductType | null;
}

export interface PutManyRoleProductTypesRequest {
    changes: RoleProductTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyRoleProductTypesResponse {
    result: Result;
    role_product_types: RoleProductType[];
}

export interface DeleteRoleProductTypeRequest {
    removal: RoleProductTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteRoleProductTypeResponse {
    result: Result;
}

export interface DeleteManyRoleProductTypesRequest {
    removals: RoleProductTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRoleProductTypesResponse {
    result: Result;
}

export interface ListByRoleIdRoleProductTypesRequest {
    role_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleProductTypesFilter | null;
}

export interface ListByRoleIdRoleProductTypesResponse {
    result: Result;
    role_product_types: RoleProductType[];
    total: number;
}

export interface ListByProductTypeCodeRoleProductTypesRequest {
    product_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleProductTypesFilter | null;
}

export interface ListByProductTypeCodeRoleProductTypesResponse {
    result: Result;
    role_product_types: RoleProductType[];
    total: number;
}

export const subjects = {
    list_role_product_types_request: 'iam.v1.role_product_types.list',
    get_role_product_type_request: 'iam.v1.role_product_types.get',
    get_many_role_product_types_request: 'iam.v1.role_product_types.get_many',
    put_role_product_type_request: 'iam.v1.role_product_types.put',
    put_many_role_product_types_request: 'iam.v1.role_product_types.put_many',
    delete_role_product_type_request: 'iam.v1.role_product_types.delete',
    delete_many_role_product_types_request: 'iam.v1.role_product_types.delete_many',
    list_by_role_id_role_product_types_request: 'iam.v1.role_product_types.list_by_role_id',
    list_by_product_type_code_role_product_types_request:
        'iam.v1.role_product_types.list_by_product_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_role_product_types_request: true,
    get_role_product_type_request: true,
    get_many_role_product_types_request: true,
    put_role_product_type_request: true,
    put_many_role_product_types_request: true,
    delete_role_product_type_request: true,
    delete_many_role_product_types_request: true,
    list_by_role_id_role_product_types_request: true,
    list_by_product_type_code_role_product_types_request: true,
} as const;

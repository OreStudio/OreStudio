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
import type { ProductFamilyCode } from '../domain/product_family_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ProductFamilyCodeKey {
    code: string;
}

export interface ProductFamilyCodeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ProductFamilyCodeChange {
    write: ProductFamilyCodeWrite;
    precondition: Precondition;
}

export interface ProductFamilyCodeRemoval {
    key: ProductFamilyCodeKey;
    precondition: Precondition;
}

export interface ProductFamilyCodeLookup {
    key: ProductFamilyCodeKey;
    product_family_code: ProductFamilyCode | null;
}

export interface ProductFamilyCodesFilter {
    code_one_of: string[] | null;
}

export interface ProductFamilyCodeEvent {
    event_id: string;
    key: ProductFamilyCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ProductFamilyCodeVersionKey {
    product_family_code: ProductFamilyCodeKey;
    version: number;
}

export interface ProductFamilyCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListProductFamilyCodesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ProductFamilyCodesFilter | null;
    as_of: string | null;
}

export interface ListProductFamilyCodesResponse {
    result: Result;
    families: ProductFamilyCode[];
    total: number;
}

export interface GetProductFamilyCodeRequest {
    key: ProductFamilyCodeKey;
}

export interface GetProductFamilyCodeResponse {
    result: Result;
    product_family_code: ProductFamilyCode | null;
}

export interface GetManyProductFamilyCodesRequest {
    keys: ProductFamilyCodeKey[];
}

export interface GetManyProductFamilyCodesResponse {
    result: Result;
    entries: ProductFamilyCodeLookup[];
}

export interface PutProductFamilyCodeRequest {
    change: ProductFamilyCodeChange;
    intent: ChangeIntent;
}

export interface PutProductFamilyCodeResponse {
    result: Result;
    product_family_code: ProductFamilyCode | null;
}

export interface PutManyProductFamilyCodesRequest {
    changes: ProductFamilyCodeChange[];
    intent: ChangeIntent;
}

export interface PutManyProductFamilyCodesResponse {
    result: Result;
    families: ProductFamilyCode[];
}

export interface DeleteProductFamilyCodeRequest {
    removal: ProductFamilyCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteProductFamilyCodeResponse {
    result: Result;
}

export interface DeleteManyProductFamilyCodesRequest {
    removals: ProductFamilyCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyProductFamilyCodesResponse {
    result: Result;
}

export interface ListProductFamilyCodeVersionsRequest {
    key: ProductFamilyCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ProductFamilyCodeVersionsFilter | null;
}

export interface ListProductFamilyCodeVersionsResponse {
    result: Result;
    versions: ProductFamilyCode[];
    total: number;
}

export interface GetProductFamilyCodeVersionRequest {
    key: ProductFamilyCodeVersionKey;
}

export interface GetProductFamilyCodeVersionResponse {
    result: Result;
    version: ProductFamilyCode | null;
}

export const subjects = {
    list_product_family_codes_request: 'refdata.v1.product_family_codes.list',
    get_product_family_code_request: 'refdata.v1.product_family_codes.get',
    get_many_product_family_codes_request: 'refdata.v1.product_family_codes.get_many',
    put_product_family_code_request: 'refdata.v1.product_family_codes.put',
    put_many_product_family_codes_request: 'refdata.v1.product_family_codes.put_many',
    delete_product_family_code_request: 'refdata.v1.product_family_codes.delete',
    delete_many_product_family_codes_request: 'refdata.v1.product_family_codes.delete_many',
    list_product_family_code_versions_request: 'refdata.v1.product_family_codes_versions.list',
    get_product_family_code_version_request: 'refdata.v1.product_family_codes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_product_family_codes_request: true,
    get_product_family_code_request: true,
    get_many_product_family_codes_request: true,
    put_product_family_code_request: true,
    put_many_product_family_codes_request: true,
    delete_product_family_code_request: true,
    delete_many_product_family_codes_request: true,
    list_product_family_code_versions_request: true,
    get_product_family_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.product_family_codes_events.created',
    updated: 'refdata.v1.product_family_codes_events.updated',
    deleted: 'refdata.v1.product_family_codes_events.deleted',
} as const;

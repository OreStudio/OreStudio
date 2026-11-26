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
import type { BusinessUnitProductType } from '../domain/business_unit_product_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface BusinessUnitProductTypeKey {
    business_unit_id: string;
    product_type_code: string;
}

export interface BusinessUnitProductTypeWrite {
    business_unit_id: string;
    product_type_code: string;
}

export interface BusinessUnitProductTypeChange {
    write: BusinessUnitProductTypeWrite;
    precondition: Precondition;
}

export interface BusinessUnitProductTypeRemoval {
    key: BusinessUnitProductTypeKey;
    precondition: Precondition;
}

export interface BusinessUnitProductTypeLookup {
    key: BusinessUnitProductTypeKey;
    business_unit_product_type: BusinessUnitProductType | null;
}

export interface BusinessUnitProductTypesFilter {
    business_unit_id: string | null;
    product_type_code: string | null;
    business_unit_id_one_of: string[] | null;
    product_type_code_one_of: string[] | null;
}

export interface BusinessUnitProductTypeEvent {
    event_id: string;
    key: BusinessUnitProductTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListBusinessUnitProductTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessUnitProductTypesFilter | null;
}

export interface ListBusinessUnitProductTypesResponse {
    result: Result;
    business_unit_product_types: BusinessUnitProductType[];
    total: number;
}

export interface GetBusinessUnitProductTypeRequest {
    key: BusinessUnitProductTypeKey;
}

export interface GetBusinessUnitProductTypeResponse {
    result: Result;
    business_unit_product_type: BusinessUnitProductType | null;
}

export interface GetManyBusinessUnitProductTypesRequest {
    keys: BusinessUnitProductTypeKey[];
}

export interface GetManyBusinessUnitProductTypesResponse {
    result: Result;
    entries: BusinessUnitProductTypeLookup[];
}

export interface PutBusinessUnitProductTypeRequest {
    change: BusinessUnitProductTypeChange;
    intent: ChangeIntent;
}

export interface PutBusinessUnitProductTypeResponse {
    result: Result;
    business_unit_product_type: BusinessUnitProductType | null;
}

export interface PutManyBusinessUnitProductTypesRequest {
    changes: BusinessUnitProductTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessUnitProductTypesResponse {
    result: Result;
    business_unit_product_types: BusinessUnitProductType[];
}

export interface DeleteBusinessUnitProductTypeRequest {
    removal: BusinessUnitProductTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessUnitProductTypeResponse {
    result: Result;
}

export interface DeleteManyBusinessUnitProductTypesRequest {
    removals: BusinessUnitProductTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessUnitProductTypesResponse {
    result: Result;
}

export interface ListByBusinessUnitIdBusinessUnitProductTypesRequest {
    business_unit_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessUnitProductTypesFilter | null;
}

export interface ListByBusinessUnitIdBusinessUnitProductTypesResponse {
    result: Result;
    business_unit_product_types: BusinessUnitProductType[];
    total: number;
}

export interface ListByProductTypeCodeBusinessUnitProductTypesRequest {
    product_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessUnitProductTypesFilter | null;
}

export interface ListByProductTypeCodeBusinessUnitProductTypesResponse {
    result: Result;
    business_unit_product_types: BusinessUnitProductType[];
    total: number;
}

export const subjects = {
    list_business_unit_product_types_request: 'refdata.v1.business_unit_product_types.list',
    get_business_unit_product_type_request: 'refdata.v1.business_unit_product_types.get',
    get_many_business_unit_product_types_request: 'refdata.v1.business_unit_product_types.get_many',
    put_business_unit_product_type_request: 'refdata.v1.business_unit_product_types.put',
    put_many_business_unit_product_types_request: 'refdata.v1.business_unit_product_types.put_many',
    delete_business_unit_product_type_request: 'refdata.v1.business_unit_product_types.delete',
    delete_many_business_unit_product_types_request:
        'refdata.v1.business_unit_product_types.delete_many',
    list_by_business_unit_id_business_unit_product_types_request:
        'refdata.v1.business_unit_product_types.list_by_business_unit_id',
    list_by_product_type_code_business_unit_product_types_request:
        'refdata.v1.business_unit_product_types.list_by_product_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_unit_product_types_request: true,
    get_business_unit_product_type_request: true,
    get_many_business_unit_product_types_request: true,
    put_business_unit_product_type_request: true,
    put_many_business_unit_product_types_request: true,
    delete_business_unit_product_type_request: true,
    delete_many_business_unit_product_types_request: true,
    list_by_business_unit_id_business_unit_product_types_request: true,
    list_by_product_type_code_business_unit_product_types_request: true,
} as const;

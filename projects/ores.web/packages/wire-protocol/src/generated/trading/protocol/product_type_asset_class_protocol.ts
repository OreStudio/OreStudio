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
import type { ProductTypeAssetClass } from '../domain/product_type_asset_class.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ProductTypeAssetClassKey {
    product_type_code: string;
    asset_class_code: string;
}

export interface ProductTypeAssetClassWrite {
    product_type_code: string;
    asset_class_code: string;
    is_home: boolean;
}

export interface ProductTypeAssetClassChange {
    write: ProductTypeAssetClassWrite;
    precondition: Precondition;
}

export interface ProductTypeAssetClassRemoval {
    key: ProductTypeAssetClassKey;
    precondition: Precondition;
}

export interface ProductTypeAssetClassLookup {
    key: ProductTypeAssetClassKey;
    product_type_asset_class: ProductTypeAssetClass | null;
}

export interface ProductTypeAssetClassesFilter {
    product_type_code: string | null;
    product_type_code_one_of: string[] | null;
}

export interface ProductTypeAssetClassEvent {
    event_id: string;
    key: ProductTypeAssetClassKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListProductTypeAssetClassesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ProductTypeAssetClassesFilter | null;
}

export interface ListProductTypeAssetClassesResponse {
    result: Result;
    product_type_asset_classes: ProductTypeAssetClass[];
    total: number;
}

export interface GetProductTypeAssetClassRequest {
    key: ProductTypeAssetClassKey;
}

export interface GetProductTypeAssetClassResponse {
    result: Result;
    product_type_asset_class: ProductTypeAssetClass | null;
}

export interface GetManyProductTypeAssetClassesRequest {
    keys: ProductTypeAssetClassKey[];
}

export interface GetManyProductTypeAssetClassesResponse {
    result: Result;
    entries: ProductTypeAssetClassLookup[];
}

export interface PutProductTypeAssetClassRequest {
    change: ProductTypeAssetClassChange;
    intent: ChangeIntent;
}

export interface PutProductTypeAssetClassResponse {
    result: Result;
    product_type_asset_class: ProductTypeAssetClass | null;
}

export interface PutManyProductTypeAssetClassesRequest {
    changes: ProductTypeAssetClassChange[];
    intent: ChangeIntent;
}

export interface PutManyProductTypeAssetClassesResponse {
    result: Result;
    product_type_asset_classes: ProductTypeAssetClass[];
}

export interface DeleteProductTypeAssetClassRequest {
    removal: ProductTypeAssetClassRemoval;
    intent: ChangeIntent;
}

export interface DeleteProductTypeAssetClassResponse {
    result: Result;
}

export interface DeleteManyProductTypeAssetClassesRequest {
    removals: ProductTypeAssetClassRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyProductTypeAssetClassesResponse {
    result: Result;
}

export interface ListByProductTypeCodeProductTypeAssetClassesRequest {
    product_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ProductTypeAssetClassesFilter | null;
}

export interface ListByProductTypeCodeProductTypeAssetClassesResponse {
    result: Result;
    product_type_asset_classes: ProductTypeAssetClass[];
    total: number;
}

export const subjects = {
    list_product_type_asset_classes_request: 'trading.v1.product_type_asset_classes.list',
    get_product_type_asset_class_request: 'trading.v1.product_type_asset_classes.get',
    get_many_product_type_asset_classes_request: 'trading.v1.product_type_asset_classes.get_many',
    put_product_type_asset_class_request: 'trading.v1.product_type_asset_classes.put',
    put_many_product_type_asset_classes_request: 'trading.v1.product_type_asset_classes.put_many',
    delete_product_type_asset_class_request: 'trading.v1.product_type_asset_classes.delete',
    delete_many_product_type_asset_classes_request:
        'trading.v1.product_type_asset_classes.delete_many',
    list_by_product_type_code_product_type_asset_classes_request:
        'trading.v1.product_type_asset_classes.list_by_product_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_product_type_asset_classes_request: true,
    get_product_type_asset_class_request: true,
    get_many_product_type_asset_classes_request: true,
    put_product_type_asset_class_request: true,
    put_many_product_type_asset_classes_request: true,
    delete_product_type_asset_class_request: true,
    delete_many_product_type_asset_classes_request: true,
    list_by_product_type_code_product_type_asset_classes_request: true,
} as const;

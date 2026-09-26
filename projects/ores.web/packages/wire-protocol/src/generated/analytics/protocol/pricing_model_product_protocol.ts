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
import type { PricingModelProduct } from '../domain/pricing_model_product.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingModelProductKey {
    pricing_engine_type_code: string;
}

export interface PricingModelProductWrite {
    id: string;
    pricing_model_config_id: string;
    pricing_engine_type_code: string;
    model: string;
    engine: string;
}

export interface PricingModelProductChange {
    write: PricingModelProductWrite;
    precondition: Precondition;
}

export interface PricingModelProductRemoval {
    key: PricingModelProductKey;
    precondition: Precondition;
}

export interface PricingModelProductLookup {
    key: PricingModelProductKey;
    pricing_model_product: PricingModelProduct | null;
}

export interface PricingModelProductEvent {
    event_id: string;
    key: PricingModelProductKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingModelProductVersionKey {
    pricing_model_product: PricingModelProductKey;
    version: number;
}

export interface PricingModelProductVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingModelProductsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPricingModelProductsResponse {
    result: Result;
    products: PricingModelProduct[];
    total: number;
}

export interface GetPricingModelProductRequest {
    key: PricingModelProductKey;
}

export interface GetPricingModelProductResponse {
    result: Result;
    pricing_model_product: PricingModelProduct | null;
}

export interface GetManyPricingModelProductsRequest {
    keys: PricingModelProductKey[];
}

export interface GetManyPricingModelProductsResponse {
    result: Result;
    entries: PricingModelProductLookup[];
}

export interface PutPricingModelProductRequest {
    change: PricingModelProductChange;
    intent: ChangeIntent;
}

export interface PutPricingModelProductResponse {
    result: Result;
    pricing_model_product: PricingModelProduct;
}

export interface PutManyPricingModelProductsRequest {
    changes: PricingModelProductChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingModelProductsResponse {
    result: Result;
    products: PricingModelProduct[];
}

export interface DeletePricingModelProductRequest {
    removal: PricingModelProductRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingModelProductResponse {
    result: Result;
}

export interface DeleteManyPricingModelProductsRequest {
    removals: PricingModelProductRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingModelProductsResponse {
    result: Result;
}

export interface ListPricingModelProductVersionsRequest {
    key: PricingModelProductKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelProductVersionsFilter | null;
}

export interface ListPricingModelProductVersionsResponse {
    result: Result;
    versions: PricingModelProduct[];
    total: number;
}

export interface GetPricingModelProductVersionRequest {
    key: PricingModelProductVersionKey;
}

export interface GetPricingModelProductVersionResponse {
    result: Result;
    version: PricingModelProduct;
}

export const subjects = {
    list_pricing_model_products_request: "analytics.v1.pricing_model_products.list",
    get_pricing_model_product_request: "analytics.v1.pricing_model_products.get",
    get_many_pricing_model_products_request: "analytics.v1.pricing_model_products.get_many",
    put_pricing_model_product_request: "analytics.v1.pricing_model_products.put",
    put_many_pricing_model_products_request: "analytics.v1.pricing_model_products.put_many",
    delete_pricing_model_product_request: "analytics.v1.pricing_model_products.delete",
    delete_many_pricing_model_products_request: "analytics.v1.pricing_model_products.delete_many",
    list_pricing_model_product_versions_request: "analytics.v1.pricing_model_products_versions.list",
    get_pricing_model_product_version_request: "analytics.v1.pricing_model_products_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_model_products_request: true,
    get_pricing_model_product_request: true,
    get_many_pricing_model_products_request: true,
    put_pricing_model_product_request: true,
    put_many_pricing_model_products_request: true,
    delete_pricing_model_product_request: true,
    delete_many_pricing_model_products_request: true,
    list_pricing_model_product_versions_request: true,
    get_pricing_model_product_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.pricing_model_products_events.created",
    updated: "analytics.v1.pricing_model_products_events.updated",
    deleted: "analytics.v1.pricing_model_products_events.deleted",
} as const;

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
import type { PricingMethod } from '../domain/pricing_method.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingMethodKey {
    code: string;
}

export interface PricingMethodWrite {
    code: string;
    description: string;
    detail: string;
    source: string;
}

export interface PricingMethodChange {
    write: PricingMethodWrite;
    precondition: Precondition;
}

export interface PricingMethodRemoval {
    key: PricingMethodKey;
    precondition: Precondition;
}

export interface PricingMethodLookup {
    key: PricingMethodKey;
    pricing_method: PricingMethod | null;
}

export interface PricingMethodsFilter {
    code_one_of: string[] | null;
}

export interface PricingMethodEvent {
    event_id: string;
    key: PricingMethodKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingMethodVersionKey {
    pricing_method: PricingMethodKey;
    version: number;
}

export interface PricingMethodVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingMethodsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PricingMethodsFilter | null;
    as_of: string | null;
}

export interface ListPricingMethodsResponse {
    result: Result;
    methods: PricingMethod[];
    total: number;
}

export interface GetPricingMethodRequest {
    key: PricingMethodKey;
}

export interface GetPricingMethodResponse {
    result: Result;
    pricing_method: PricingMethod | null;
}

export interface GetManyPricingMethodsRequest {
    keys: PricingMethodKey[];
}

export interface GetManyPricingMethodsResponse {
    result: Result;
    entries: PricingMethodLookup[];
}

export interface PutPricingMethodRequest {
    change: PricingMethodChange;
    intent: ChangeIntent;
}

export interface PutPricingMethodResponse {
    result: Result;
    pricing_method: PricingMethod | null;
}

export interface PutManyPricingMethodsRequest {
    changes: PricingMethodChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingMethodsResponse {
    result: Result;
    methods: PricingMethod[];
}

export interface DeletePricingMethodRequest {
    removal: PricingMethodRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingMethodResponse {
    result: Result;
}

export interface DeleteManyPricingMethodsRequest {
    removals: PricingMethodRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingMethodsResponse {
    result: Result;
}

export interface ListPricingMethodVersionsRequest {
    key: PricingMethodKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingMethodVersionsFilter | null;
}

export interface ListPricingMethodVersionsResponse {
    result: Result;
    versions: PricingMethod[];
    total: number;
}

export interface GetPricingMethodVersionRequest {
    key: PricingMethodVersionKey;
}

export interface GetPricingMethodVersionResponse {
    result: Result;
    version: PricingMethod | null;
}

export const subjects = {
    list_pricing_methods_request: 'analytics.v1.pricing_methods.list',
    get_pricing_method_request: 'analytics.v1.pricing_methods.get',
    get_many_pricing_methods_request: 'analytics.v1.pricing_methods.get_many',
    put_pricing_method_request: 'analytics.v1.pricing_methods.put',
    put_many_pricing_methods_request: 'analytics.v1.pricing_methods.put_many',
    delete_pricing_method_request: 'analytics.v1.pricing_methods.delete',
    delete_many_pricing_methods_request: 'analytics.v1.pricing_methods.delete_many',
    list_pricing_method_versions_request: 'analytics.v1.pricing_methods_versions.list',
    get_pricing_method_version_request: 'analytics.v1.pricing_methods_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_methods_request: true,
    get_pricing_method_request: true,
    get_many_pricing_methods_request: true,
    put_pricing_method_request: true,
    put_many_pricing_methods_request: true,
    delete_pricing_method_request: true,
    delete_many_pricing_methods_request: true,
    list_pricing_method_versions_request: true,
    get_pricing_method_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.pricing_methods_events.created',
    updated: 'analytics.v1.pricing_methods_events.updated',
    deleted: 'analytics.v1.pricing_methods_events.deleted',
} as const;

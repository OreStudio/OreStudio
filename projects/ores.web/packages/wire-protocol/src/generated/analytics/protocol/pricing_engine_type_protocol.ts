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
import type { PricingEngineType } from '../domain/pricing_engine_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingEngineTypeKey {
    code: string;
}

export interface PricingEngineTypeWrite {
    code: string;
    description: string;
    instrument_type_code: string;
}

export interface PricingEngineTypeChange {
    write: PricingEngineTypeWrite;
    precondition: Precondition;
}

export interface PricingEngineTypeRemoval {
    key: PricingEngineTypeKey;
    precondition: Precondition;
}

export interface PricingEngineTypeLookup {
    key: PricingEngineTypeKey;
    pricing_engine_type: PricingEngineType | null;
}

export interface PricingEngineTypeEvent {
    event_id: string;
    key: PricingEngineTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingEngineTypeVersionKey {
    pricing_engine_type: PricingEngineTypeKey;
    version: number;
}

export interface PricingEngineTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingEngineTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPricingEngineTypesResponse {
    result: Result;
    types: PricingEngineType[];
    total: number;
}

export interface GetPricingEngineTypeRequest {
    key: PricingEngineTypeKey;
}

export interface GetPricingEngineTypeResponse {
    result: Result;
    pricing_engine_type: PricingEngineType | null;
}

export interface GetManyPricingEngineTypesRequest {
    keys: PricingEngineTypeKey[];
}

export interface GetManyPricingEngineTypesResponse {
    result: Result;
    entries: PricingEngineTypeLookup[];
}

export interface PutPricingEngineTypeRequest {
    change: PricingEngineTypeChange;
    intent: ChangeIntent;
}

export interface PutPricingEngineTypeResponse {
    result: Result;
    pricing_engine_type: PricingEngineType;
}

export interface PutManyPricingEngineTypesRequest {
    changes: PricingEngineTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingEngineTypesResponse {
    result: Result;
    types: PricingEngineType[];
}

export interface DeletePricingEngineTypeRequest {
    removal: PricingEngineTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingEngineTypeResponse {
    result: Result;
}

export interface DeleteManyPricingEngineTypesRequest {
    removals: PricingEngineTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingEngineTypesResponse {
    result: Result;
}

export interface ListPricingEngineTypeVersionsRequest {
    key: PricingEngineTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingEngineTypeVersionsFilter | null;
}

export interface ListPricingEngineTypeVersionsResponse {
    result: Result;
    versions: PricingEngineType[];
    total: number;
}

export interface GetPricingEngineTypeVersionRequest {
    key: PricingEngineTypeVersionKey;
}

export interface GetPricingEngineTypeVersionResponse {
    result: Result;
    version: PricingEngineType;
}

export const subjects = {
    list_pricing_engine_types_request: "analytics.v1.pricing_engine_types.list",
    get_pricing_engine_type_request: "analytics.v1.pricing_engine_types.get",
    get_many_pricing_engine_types_request: "analytics.v1.pricing_engine_types.get_many",
    put_pricing_engine_type_request: "analytics.v1.pricing_engine_types.put",
    put_many_pricing_engine_types_request: "analytics.v1.pricing_engine_types.put_many",
    delete_pricing_engine_type_request: "analytics.v1.pricing_engine_types.delete",
    delete_many_pricing_engine_types_request: "analytics.v1.pricing_engine_types.delete_many",
    list_pricing_engine_type_versions_request: "analytics.v1.pricing_engine_types_versions.list",
    get_pricing_engine_type_version_request: "analytics.v1.pricing_engine_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_engine_types_request: true,
    get_pricing_engine_type_request: true,
    get_many_pricing_engine_types_request: true,
    put_pricing_engine_type_request: true,
    put_many_pricing_engine_types_request: true,
    delete_pricing_engine_type_request: true,
    delete_many_pricing_engine_types_request: true,
    list_pricing_engine_type_versions_request: true,
    get_pricing_engine_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.pricing_engine_types_events.created",
    updated: "analytics.v1.pricing_engine_types_events.updated",
    deleted: "analytics.v1.pricing_engine_types_events.deleted",
} as const;

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
import type { PricingModelEnginePair } from '../domain/pricing_model_engine_pair.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingModelEnginePairKey {
    code: string;
}

export interface PricingModelEnginePairWrite {
    code: string;
    model: string;
    engine: string;
    pricing_engine_type_code: string;
    is_usual: boolean;
    note: string;
}

export interface PricingModelEnginePairChange {
    write: PricingModelEnginePairWrite;
    precondition: Precondition;
}

export interface PricingModelEnginePairRemoval {
    key: PricingModelEnginePairKey;
    precondition: Precondition;
}

export interface PricingModelEnginePairLookup {
    key: PricingModelEnginePairKey;
    pricing_model_engine_pair: PricingModelEnginePair | null;
}

export interface PricingModelEnginePairsFilter {
    code_one_of: string[] | null;
}

export interface PricingModelEnginePairEvent {
    event_id: string;
    key: PricingModelEnginePairKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingModelEnginePairVersionKey {
    pricing_model_engine_pair: PricingModelEnginePairKey;
    version: number;
}

export interface PricingModelEnginePairVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingModelEnginePairsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelEnginePairsFilter | null;
    as_of: string | null;
}

export interface ListPricingModelEnginePairsResponse {
    result: Result;
    pairs: PricingModelEnginePair[];
    total: number;
}

export interface GetPricingModelEnginePairRequest {
    key: PricingModelEnginePairKey;
}

export interface GetPricingModelEnginePairResponse {
    result: Result;
    pricing_model_engine_pair: PricingModelEnginePair | null;
}

export interface GetManyPricingModelEnginePairsRequest {
    keys: PricingModelEnginePairKey[];
}

export interface GetManyPricingModelEnginePairsResponse {
    result: Result;
    entries: PricingModelEnginePairLookup[];
}

export interface PutPricingModelEnginePairRequest {
    change: PricingModelEnginePairChange;
    intent: ChangeIntent;
}

export interface PutPricingModelEnginePairResponse {
    result: Result;
    pricing_model_engine_pair: PricingModelEnginePair | null;
}

export interface PutManyPricingModelEnginePairsRequest {
    changes: PricingModelEnginePairChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingModelEnginePairsResponse {
    result: Result;
    pairs: PricingModelEnginePair[];
}

export interface DeletePricingModelEnginePairRequest {
    removal: PricingModelEnginePairRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingModelEnginePairResponse {
    result: Result;
}

export interface DeleteManyPricingModelEnginePairsRequest {
    removals: PricingModelEnginePairRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingModelEnginePairsResponse {
    result: Result;
}

export interface ListPricingModelEnginePairVersionsRequest {
    key: PricingModelEnginePairKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelEnginePairVersionsFilter | null;
}

export interface ListPricingModelEnginePairVersionsResponse {
    result: Result;
    versions: PricingModelEnginePair[];
    total: number;
}

export interface GetPricingModelEnginePairVersionRequest {
    key: PricingModelEnginePairVersionKey;
}

export interface GetPricingModelEnginePairVersionResponse {
    result: Result;
    version: PricingModelEnginePair | null;
}

export const subjects = {
    list_pricing_model_engine_pairs_request: 'analytics.v1.pricing_model_engine_pairs.list',
    get_pricing_model_engine_pair_request: 'analytics.v1.pricing_model_engine_pairs.get',
    get_many_pricing_model_engine_pairs_request: 'analytics.v1.pricing_model_engine_pairs.get_many',
    put_pricing_model_engine_pair_request: 'analytics.v1.pricing_model_engine_pairs.put',
    put_many_pricing_model_engine_pairs_request: 'analytics.v1.pricing_model_engine_pairs.put_many',
    delete_pricing_model_engine_pair_request: 'analytics.v1.pricing_model_engine_pairs.delete',
    delete_many_pricing_model_engine_pairs_request:
        'analytics.v1.pricing_model_engine_pairs.delete_many',
    list_pricing_model_engine_pair_versions_request:
        'analytics.v1.pricing_model_engine_pairs_versions.list',
    get_pricing_model_engine_pair_version_request:
        'analytics.v1.pricing_model_engine_pairs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_model_engine_pairs_request: true,
    get_pricing_model_engine_pair_request: true,
    get_many_pricing_model_engine_pairs_request: true,
    put_pricing_model_engine_pair_request: true,
    put_many_pricing_model_engine_pairs_request: true,
    delete_pricing_model_engine_pair_request: true,
    delete_many_pricing_model_engine_pairs_request: true,
    list_pricing_model_engine_pair_versions_request: true,
    get_pricing_model_engine_pair_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.pricing_model_engine_pairs_events.created',
    updated: 'analytics.v1.pricing_model_engine_pairs_events.updated',
    deleted: 'analytics.v1.pricing_model_engine_pairs_events.deleted',
} as const;

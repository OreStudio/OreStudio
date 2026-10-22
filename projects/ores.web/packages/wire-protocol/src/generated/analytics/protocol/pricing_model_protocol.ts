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
import type { PricingModel } from '../domain/pricing_model.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingModelKey {
    code: string;
}

export interface PricingModelWrite {
    code: string;
    description: string;
    detail: string;
    source: string;
}

export interface PricingModelChange {
    write: PricingModelWrite;
    precondition: Precondition;
}

export interface PricingModelRemoval {
    key: PricingModelKey;
    precondition: Precondition;
}

export interface PricingModelLookup {
    key: PricingModelKey;
    pricing_model: PricingModel | null;
}

export interface PricingModelsFilter {
    code_one_of: string[] | null;
}

export interface PricingModelEvent {
    event_id: string;
    key: PricingModelKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingModelVersionKey {
    pricing_model: PricingModelKey;
    version: number;
}

export interface PricingModelVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingModelsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelsFilter | null;
    as_of: string | null;
}

export interface ListPricingModelsResponse {
    result: Result;
    models: PricingModel[];
    total: number;
}

export interface GetPricingModelRequest {
    key: PricingModelKey;
}

export interface GetPricingModelResponse {
    result: Result;
    pricing_model: PricingModel | null;
}

export interface GetManyPricingModelsRequest {
    keys: PricingModelKey[];
}

export interface GetManyPricingModelsResponse {
    result: Result;
    entries: PricingModelLookup[];
}

export interface PutPricingModelRequest {
    change: PricingModelChange;
    intent: ChangeIntent;
}

export interface PutPricingModelResponse {
    result: Result;
    pricing_model: PricingModel | null;
}

export interface PutManyPricingModelsRequest {
    changes: PricingModelChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingModelsResponse {
    result: Result;
    models: PricingModel[];
}

export interface DeletePricingModelRequest {
    removal: PricingModelRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingModelResponse {
    result: Result;
}

export interface DeleteManyPricingModelsRequest {
    removals: PricingModelRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingModelsResponse {
    result: Result;
}

export interface ListPricingModelVersionsRequest {
    key: PricingModelKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelVersionsFilter | null;
}

export interface ListPricingModelVersionsResponse {
    result: Result;
    versions: PricingModel[];
    total: number;
}

export interface GetPricingModelVersionRequest {
    key: PricingModelVersionKey;
}

export interface GetPricingModelVersionResponse {
    result: Result;
    version: PricingModel | null;
}

export const subjects = {
    list_pricing_models_request: 'analytics.v1.pricing_models.list',
    get_pricing_model_request: 'analytics.v1.pricing_models.get',
    get_many_pricing_models_request: 'analytics.v1.pricing_models.get_many',
    put_pricing_model_request: 'analytics.v1.pricing_models.put',
    put_many_pricing_models_request: 'analytics.v1.pricing_models.put_many',
    delete_pricing_model_request: 'analytics.v1.pricing_models.delete',
    delete_many_pricing_models_request: 'analytics.v1.pricing_models.delete_many',
    list_pricing_model_versions_request: 'analytics.v1.pricing_models_versions.list',
    get_pricing_model_version_request: 'analytics.v1.pricing_models_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_models_request: true,
    get_pricing_model_request: true,
    get_many_pricing_models_request: true,
    put_pricing_model_request: true,
    put_many_pricing_models_request: true,
    delete_pricing_model_request: true,
    delete_many_pricing_models_request: true,
    list_pricing_model_versions_request: true,
    get_pricing_model_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.pricing_models_events.created',
    updated: 'analytics.v1.pricing_models_events.updated',
    deleted: 'analytics.v1.pricing_models_events.deleted',
} as const;

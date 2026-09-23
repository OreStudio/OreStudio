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
import type { PricingModelProductParameter } from '../domain/pricing_model_product_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingModelProductParameterKey {
    parameter_name: string;
}

export interface PricingModelProductParameterWrite {
    id: string;
    pricing_model_config_id: string;
    pricing_model_product_id: string | null;
    parameter_scope: string;
    parameter_name: string;
    parameter_value: string;
}

export interface PricingModelProductParameterChange {
    write: PricingModelProductParameterWrite;
    precondition: Precondition;
}

export interface PricingModelProductParameterRemoval {
    key: PricingModelProductParameterKey;
    precondition: Precondition;
}

export interface PricingModelProductParameterLookup {
    key: PricingModelProductParameterKey;
    pricing_model_product_parameter: PricingModelProductParameter | null;
}

export interface PricingModelProductParameterEvent {
    event_id: string;
    key: PricingModelProductParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingModelProductParameterVersionKey {
    pricing_model_product_parameter: PricingModelProductParameterKey;
    version: number;
}

export interface PricingModelProductParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingModelProductParametersRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPricingModelProductParametersResponse {
    result: Result;
    parameters: PricingModelProductParameter[];
    total: number;
}

export interface GetPricingModelProductParameterRequest {
    key: PricingModelProductParameterKey;
}

export interface GetPricingModelProductParameterResponse {
    result: Result;
    pricing_model_product_parameter: PricingModelProductParameter | null;
}

export interface GetManyPricingModelProductParametersRequest {
    keys: PricingModelProductParameterKey[];
}

export interface GetManyPricingModelProductParametersResponse {
    result: Result;
    entries: PricingModelProductParameterLookup[];
}

export interface PutPricingModelProductParameterRequest {
    change: PricingModelProductParameterChange;
    intent: ChangeIntent;
}

export interface PutPricingModelProductParameterResponse {
    result: Result;
    pricing_model_product_parameter: PricingModelProductParameter;
}

export interface PutManyPricingModelProductParametersRequest {
    changes: PricingModelProductParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingModelProductParametersResponse {
    result: Result;
    parameters: PricingModelProductParameter[];
}

export interface DeletePricingModelProductParameterRequest {
    removal: PricingModelProductParameterRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingModelProductParameterResponse {
    result: Result;
}

export interface DeleteManyPricingModelProductParametersRequest {
    removals: PricingModelProductParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingModelProductParametersResponse {
    result: Result;
}

export interface ListPricingModelProductParameterVersionsRequest {
    key: PricingModelProductParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelProductParameterVersionsFilter | null;
}

export interface ListPricingModelProductParameterVersionsResponse {
    result: Result;
    versions: PricingModelProductParameter[];
    total: number;
}

export interface GetPricingModelProductParameterVersionRequest {
    key: PricingModelProductParameterVersionKey;
}

export interface GetPricingModelProductParameterVersionResponse {
    result: Result;
    version: PricingModelProductParameter;
}

export const subjects = {
    list_pricing_model_product_parameters_request: "analytics.v1.pricing_model_product_parameters.list",
    get_pricing_model_product_parameter_request: "analytics.v1.pricing_model_product_parameters.get",
    get_many_pricing_model_product_parameters_request: "analytics.v1.pricing_model_product_parameters.get_many",
    put_pricing_model_product_parameter_request: "analytics.v1.pricing_model_product_parameters.put",
    put_many_pricing_model_product_parameters_request: "analytics.v1.pricing_model_product_parameters.put_many",
    delete_pricing_model_product_parameter_request: "analytics.v1.pricing_model_product_parameters.delete",
    delete_many_pricing_model_product_parameters_request: "analytics.v1.pricing_model_product_parameters.delete_many",
    list_pricing_model_product_parameter_versions_request: "analytics.v1.pricing_model_product_parameters_versions.list",
    get_pricing_model_product_parameter_version_request: "analytics.v1.pricing_model_product_parameters_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_model_product_parameters_request: true,
    get_pricing_model_product_parameter_request: true,
    get_many_pricing_model_product_parameters_request: true,
    put_pricing_model_product_parameter_request: true,
    put_many_pricing_model_product_parameters_request: true,
    delete_pricing_model_product_parameter_request: true,
    delete_many_pricing_model_product_parameters_request: true,
    list_pricing_model_product_parameter_versions_request: true,
    get_pricing_model_product_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.pricing_model_product_parameters_events.created",
    updated: "analytics.v1.pricing_model_product_parameters_events.updated",
    deleted: "analytics.v1.pricing_model_product_parameters_events.deleted",
} as const;

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
import type { PricingModelConfig } from '../domain/pricing_model_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingModelConfigKey {
    name: string;
}

export interface PricingModelConfigWrite {
    id: string;
    name: string;
    description: string;
    config_variant: string;
}

export interface PricingModelConfigChange {
    write: PricingModelConfigWrite;
    precondition: Precondition;
}

export interface PricingModelConfigRemoval {
    key: PricingModelConfigKey;
    precondition: Precondition;
}

export interface PricingModelConfigLookup {
    key: PricingModelConfigKey;
    pricing_model_config: PricingModelConfig | null;
}

export interface PricingModelConfigEvent {
    event_id: string;
    key: PricingModelConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingModelConfigVersionKey {
    pricing_model_config: PricingModelConfigKey;
    version: number;
}

export interface PricingModelConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingModelConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPricingModelConfigsResponse {
    result: Result;
    configs: PricingModelConfig[];
    total: number;
}

export interface GetPricingModelConfigRequest {
    key: PricingModelConfigKey;
}

export interface GetPricingModelConfigResponse {
    result: Result;
    pricing_model_config: PricingModelConfig | null;
}

export interface GetManyPricingModelConfigsRequest {
    keys: PricingModelConfigKey[];
}

export interface GetManyPricingModelConfigsResponse {
    result: Result;
    entries: PricingModelConfigLookup[];
}

export interface PutPricingModelConfigRequest {
    change: PricingModelConfigChange;
    intent: ChangeIntent;
}

export interface PutPricingModelConfigResponse {
    result: Result;
    pricing_model_config: PricingModelConfig;
}

export interface PutManyPricingModelConfigsRequest {
    changes: PricingModelConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingModelConfigsResponse {
    result: Result;
    configs: PricingModelConfig[];
}

export interface DeletePricingModelConfigRequest {
    removal: PricingModelConfigRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingModelConfigResponse {
    result: Result;
}

export interface DeleteManyPricingModelConfigsRequest {
    removals: PricingModelConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingModelConfigsResponse {
    result: Result;
}

export interface ListPricingModelConfigVersionsRequest {
    key: PricingModelConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingModelConfigVersionsFilter | null;
}

export interface ListPricingModelConfigVersionsResponse {
    result: Result;
    versions: PricingModelConfig[];
    total: number;
}

export interface GetPricingModelConfigVersionRequest {
    key: PricingModelConfigVersionKey;
}

export interface GetPricingModelConfigVersionResponse {
    result: Result;
    version: PricingModelConfig;
}

export const subjects = {
    list_pricing_model_configs_request: "analytics.v1.pricing_model_configs.list",
    get_pricing_model_config_request: "analytics.v1.pricing_model_configs.get",
    get_many_pricing_model_configs_request: "analytics.v1.pricing_model_configs.get_many",
    put_pricing_model_config_request: "analytics.v1.pricing_model_configs.put",
    put_many_pricing_model_configs_request: "analytics.v1.pricing_model_configs.put_many",
    delete_pricing_model_config_request: "analytics.v1.pricing_model_configs.delete",
    delete_many_pricing_model_configs_request: "analytics.v1.pricing_model_configs.delete_many",
    list_pricing_model_config_versions_request: "analytics.v1.pricing_model_configs_versions.list",
    get_pricing_model_config_version_request: "analytics.v1.pricing_model_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_model_configs_request: true,
    get_pricing_model_config_request: true,
    get_many_pricing_model_configs_request: true,
    put_pricing_model_config_request: true,
    put_many_pricing_model_configs_request: true,
    delete_pricing_model_config_request: true,
    delete_many_pricing_model_configs_request: true,
    list_pricing_model_config_versions_request: true,
    get_pricing_model_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.pricing_model_configs_events.created",
    updated: "analytics.v1.pricing_model_configs_events.updated",
    deleted: "analytics.v1.pricing_model_configs_events.deleted",
} as const;

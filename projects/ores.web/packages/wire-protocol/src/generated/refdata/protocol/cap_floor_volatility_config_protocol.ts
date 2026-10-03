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
import type { CapFloorVolatilityConfig } from '../domain/cap_floor_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CapFloorVolatilityConfigKey {
    id: string;
}

export interface CapFloorVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    volatility_type: string | null;
    output_volatility_type: string | null;
    model_shift: number | null;
    output_shift: number | null;
    extrapolation: string | null;
    interpolation_method: string | null;
    include_atm: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    tenors: string | null;
    strikes: string | null;
    optional_quotes: string | null;
    ibor_index: string | null;
    index: string | null;
    rate_computation_period: string | null;
    on_cap_settlement_days: number | null;
    discount_curve: string | null;
    atm_tenors: string | null;
    settlement_days: number | null;
    interpolate_on: string | null;
    time_interpolation: string | null;
    strike_interpolation: string | null;
    input_type: string | null;
    quote_includes_index_name: string | null;
    flat_first_period: string | null;
    use_effecive_volatility: string | null;
    use_effective_volatility: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_index: string | null;
    proxy_source_rate_computation_period: string | null;
    proxy_target_index: string | null;
    proxy_target_rate_computation_period: string | null;
    proxy_target_on_cap_settlement_days: number | null;
    proxy_scaling_factor: number | null;
}

export interface CapFloorVolatilityConfigChange {
    write: CapFloorVolatilityConfigWrite;
    precondition: Precondition;
}

export interface CapFloorVolatilityConfigRemoval {
    key: CapFloorVolatilityConfigKey;
    precondition: Precondition;
}

export interface CapFloorVolatilityConfigLookup {
    key: CapFloorVolatilityConfigKey;
    cap_floor_volatility_config: CapFloorVolatilityConfig | null;
}

export interface CapFloorVolatilityConfigEvent {
    event_id: string;
    key: CapFloorVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CapFloorVolatilityConfigVersionKey {
    cap_floor_volatility_config: CapFloorVolatilityConfigKey;
    version: number;
}

export interface CapFloorVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCapFloorVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCapFloorVolatilityConfigsResponse {
    result: Result;
    cap_floor_volatility_configs: CapFloorVolatilityConfig[];
    total: number;
}

export interface GetCapFloorVolatilityConfigRequest {
    key: CapFloorVolatilityConfigKey;
}

export interface GetCapFloorVolatilityConfigResponse {
    result: Result;
    cap_floor_volatility_config: CapFloorVolatilityConfig | null;
}

export interface GetManyCapFloorVolatilityConfigsRequest {
    keys: CapFloorVolatilityConfigKey[];
}

export interface GetManyCapFloorVolatilityConfigsResponse {
    result: Result;
    entries: CapFloorVolatilityConfigLookup[];
}

export interface PutCapFloorVolatilityConfigRequest {
    change: CapFloorVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutCapFloorVolatilityConfigResponse {
    result: Result;
    cap_floor_volatility_config: CapFloorVolatilityConfig | null;
}

export interface PutManyCapFloorVolatilityConfigsRequest {
    changes: CapFloorVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCapFloorVolatilityConfigsResponse {
    result: Result;
    cap_floor_volatility_configs: CapFloorVolatilityConfig[];
}

export interface DeleteCapFloorVolatilityConfigRequest {
    removal: CapFloorVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCapFloorVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyCapFloorVolatilityConfigsRequest {
    removals: CapFloorVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCapFloorVolatilityConfigsResponse {
    result: Result;
}

export interface ListCapFloorVolatilityConfigVersionsRequest {
    key: CapFloorVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CapFloorVolatilityConfigVersionsFilter | null;
}

export interface ListCapFloorVolatilityConfigVersionsResponse {
    result: Result;
    versions: CapFloorVolatilityConfig[];
    total: number;
}

export interface GetCapFloorVolatilityConfigVersionRequest {
    key: CapFloorVolatilityConfigVersionKey;
}

export interface GetCapFloorVolatilityConfigVersionResponse {
    result: Result;
    version: CapFloorVolatilityConfig | null;
}

export const subjects = {
    list_cap_floor_volatility_configs_request: 'refdata.v1.cap_floor_volatility_configs.list',
    get_cap_floor_volatility_config_request: 'refdata.v1.cap_floor_volatility_configs.get',
    get_many_cap_floor_volatility_configs_request:
        'refdata.v1.cap_floor_volatility_configs.get_many',
    put_cap_floor_volatility_config_request: 'refdata.v1.cap_floor_volatility_configs.put',
    put_many_cap_floor_volatility_configs_request:
        'refdata.v1.cap_floor_volatility_configs.put_many',
    delete_cap_floor_volatility_config_request: 'refdata.v1.cap_floor_volatility_configs.delete',
    delete_many_cap_floor_volatility_configs_request:
        'refdata.v1.cap_floor_volatility_configs.delete_many',
    list_cap_floor_volatility_config_versions_request:
        'refdata.v1.cap_floor_volatility_configs_versions.list',
    get_cap_floor_volatility_config_version_request:
        'refdata.v1.cap_floor_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cap_floor_volatility_configs_request: true,
    get_cap_floor_volatility_config_request: true,
    get_many_cap_floor_volatility_configs_request: true,
    put_cap_floor_volatility_config_request: true,
    put_many_cap_floor_volatility_configs_request: true,
    delete_cap_floor_volatility_config_request: true,
    delete_many_cap_floor_volatility_configs_request: true,
    list_cap_floor_volatility_config_versions_request: true,
    get_cap_floor_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cap_floor_volatility_configs_events.created',
    updated: 'refdata.v1.cap_floor_volatility_configs_events.updated',
    deleted: 'refdata.v1.cap_floor_volatility_configs_events.deleted',
} as const;

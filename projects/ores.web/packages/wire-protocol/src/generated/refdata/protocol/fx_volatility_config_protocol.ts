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
import type { FxVolatilityConfig } from '../domain/fx_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolatilityConfigKey {
    id: string;
}

export interface FxVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    dimension: string;
    smile_type: string | null;
    smile_interpolation: string | null;
    deltas: string | null;
    smile_delta: string | null;
    conventions: string | null;
    expiries: string | null;
    fx_spot_id: string | null;
    fx_foreign_curve_id: string | null;
    fx_domestic_curve_id: string | null;
    calendar: string | null;
    day_counter: string | null;
    fx_index_tag: string | null;
    base_volatility_1: string | null;
    base_volatility_2: string | null;
    smile_extrapolation: string | null;
    time_interpolation: string | null;
    time_weighting: string | null;
    butterfly_error_tolerance: number | null;
}

export interface FxVolatilityConfigChange {
    write: FxVolatilityConfigWrite;
    precondition: Precondition;
}

export interface FxVolatilityConfigRemoval {
    key: FxVolatilityConfigKey;
    precondition: Precondition;
}

export interface FxVolatilityConfigLookup {
    key: FxVolatilityConfigKey;
    fx_volatility_config: FxVolatilityConfig | null;
}

export interface FxVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface FxVolatilityConfigEvent {
    event_id: string;
    key: FxVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolatilityConfigVersionKey {
    fx_volatility_config: FxVolatilityConfigKey;
    version: number;
}

export interface FxVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolatilityConfigsFilter | null;
}

export interface ListFxVolatilityConfigsResponse {
    result: Result;
    fx_volatility_configs: FxVolatilityConfig[];
    total: number;
}

export interface GetFxVolatilityConfigRequest {
    key: FxVolatilityConfigKey;
}

export interface GetFxVolatilityConfigResponse {
    result: Result;
    fx_volatility_config: FxVolatilityConfig | null;
}

export interface GetManyFxVolatilityConfigsRequest {
    keys: FxVolatilityConfigKey[];
}

export interface GetManyFxVolatilityConfigsResponse {
    result: Result;
    entries: FxVolatilityConfigLookup[];
}

export interface PutFxVolatilityConfigRequest {
    change: FxVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutFxVolatilityConfigResponse {
    result: Result;
    fx_volatility_config: FxVolatilityConfig | null;
}

export interface PutManyFxVolatilityConfigsRequest {
    changes: FxVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolatilityConfigsResponse {
    result: Result;
    fx_volatility_configs: FxVolatilityConfig[];
}

export interface DeleteFxVolatilityConfigRequest {
    removal: FxVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyFxVolatilityConfigsRequest {
    removals: FxVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolatilityConfigsResponse {
    result: Result;
}

export interface ListFxVolatilityConfigVersionsRequest {
    key: FxVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolatilityConfigVersionsFilter | null;
}

export interface ListFxVolatilityConfigVersionsResponse {
    result: Result;
    versions: FxVolatilityConfig[];
    total: number;
}

export interface GetFxVolatilityConfigVersionRequest {
    key: FxVolatilityConfigVersionKey;
}

export interface GetFxVolatilityConfigVersionResponse {
    result: Result;
    version: FxVolatilityConfig | null;
}

export const subjects = {
    list_fx_volatility_configs_request: 'refdata.v1.fx_volatility_configs.list',
    get_fx_volatility_config_request: 'refdata.v1.fx_volatility_configs.get',
    get_many_fx_volatility_configs_request: 'refdata.v1.fx_volatility_configs.get_many',
    put_fx_volatility_config_request: 'refdata.v1.fx_volatility_configs.put',
    put_many_fx_volatility_configs_request: 'refdata.v1.fx_volatility_configs.put_many',
    delete_fx_volatility_config_request: 'refdata.v1.fx_volatility_configs.delete',
    delete_many_fx_volatility_configs_request: 'refdata.v1.fx_volatility_configs.delete_many',
    list_fx_volatility_config_versions_request: 'refdata.v1.fx_volatility_configs_versions.list',
    get_fx_volatility_config_version_request: 'refdata.v1.fx_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_volatility_configs_request: true,
    get_fx_volatility_config_request: true,
    get_many_fx_volatility_configs_request: true,
    put_fx_volatility_config_request: true,
    put_many_fx_volatility_configs_request: true,
    delete_fx_volatility_config_request: true,
    delete_many_fx_volatility_configs_request: true,
    list_fx_volatility_config_versions_request: true,
    get_fx_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.fx_volatility_configs_events.created',
    updated: 'refdata.v1.fx_volatility_configs_events.updated',
    deleted: 'refdata.v1.fx_volatility_configs_events.deleted',
} as const;

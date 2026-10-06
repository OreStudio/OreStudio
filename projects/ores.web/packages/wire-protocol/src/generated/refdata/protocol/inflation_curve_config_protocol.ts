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
import type { InflationCurveConfig } from '../domain/inflation_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationCurveConfigKey {
    id: string;
}

export interface InflationCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    nominal_term_structure: string;
    inflation_type: string;
    conventions: string | null;
    has_quotes: boolean;
    extrapolation: string | null;
    calendar: string;
    day_counter: string | null;
    lag: string;
    frequency: string;
    base_rate: string | null;
    tolerance: number | null;
    has_seasonality: boolean;
    seasonality_base_date: string | null;
    seasonality_frequency: string | null;
    use_last_fixing_date: string | null;
    interpolation_variable: string | null;
    interpolation_method: string | null;
}

export interface InflationCurveConfigChange {
    write: InflationCurveConfigWrite;
    precondition: Precondition;
}

export interface InflationCurveConfigRemoval {
    key: InflationCurveConfigKey;
    precondition: Precondition;
}

export interface InflationCurveConfigLookup {
    key: InflationCurveConfigKey;
    inflation_curve_config: InflationCurveConfig | null;
}

export interface InflationCurveConfigsFilter {
    id_one_of: string[] | null;
}

export interface InflationCurveConfigEvent {
    event_id: string;
    key: InflationCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationCurveConfigVersionKey {
    inflation_curve_config: InflationCurveConfigKey;
    version: number;
}

export interface InflationCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCurveConfigsFilter | null;
    as_of: string | null;
}

export interface ListInflationCurveConfigsResponse {
    result: Result;
    inflation_curve_configs: InflationCurveConfig[];
    total: number;
}

export interface GetInflationCurveConfigRequest {
    key: InflationCurveConfigKey;
}

export interface GetInflationCurveConfigResponse {
    result: Result;
    inflation_curve_config: InflationCurveConfig | null;
}

export interface GetManyInflationCurveConfigsRequest {
    keys: InflationCurveConfigKey[];
}

export interface GetManyInflationCurveConfigsResponse {
    result: Result;
    entries: InflationCurveConfigLookup[];
}

export interface PutInflationCurveConfigRequest {
    change: InflationCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutInflationCurveConfigResponse {
    result: Result;
    inflation_curve_config: InflationCurveConfig | null;
}

export interface PutManyInflationCurveConfigsRequest {
    changes: InflationCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationCurveConfigsResponse {
    result: Result;
    inflation_curve_configs: InflationCurveConfig[];
}

export interface DeleteInflationCurveConfigRequest {
    removal: InflationCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationCurveConfigResponse {
    result: Result;
}

export interface DeleteManyInflationCurveConfigsRequest {
    removals: InflationCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationCurveConfigsResponse {
    result: Result;
}

export interface ListInflationCurveConfigVersionsRequest {
    key: InflationCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCurveConfigVersionsFilter | null;
}

export interface ListInflationCurveConfigVersionsResponse {
    result: Result;
    versions: InflationCurveConfig[];
    total: number;
}

export interface GetInflationCurveConfigVersionRequest {
    key: InflationCurveConfigVersionKey;
}

export interface GetInflationCurveConfigVersionResponse {
    result: Result;
    version: InflationCurveConfig | null;
}

export const subjects = {
    list_inflation_curve_configs_request: 'refdata.v1.inflation_curve_configs.list',
    get_inflation_curve_config_request: 'refdata.v1.inflation_curve_configs.get',
    get_many_inflation_curve_configs_request: 'refdata.v1.inflation_curve_configs.get_many',
    put_inflation_curve_config_request: 'refdata.v1.inflation_curve_configs.put',
    put_many_inflation_curve_configs_request: 'refdata.v1.inflation_curve_configs.put_many',
    delete_inflation_curve_config_request: 'refdata.v1.inflation_curve_configs.delete',
    delete_many_inflation_curve_configs_request: 'refdata.v1.inflation_curve_configs.delete_many',
    list_inflation_curve_config_versions_request:
        'refdata.v1.inflation_curve_configs_versions.list',
    get_inflation_curve_config_version_request: 'refdata.v1.inflation_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_curve_configs_request: true,
    get_inflation_curve_config_request: true,
    get_many_inflation_curve_configs_request: true,
    put_inflation_curve_config_request: true,
    put_many_inflation_curve_configs_request: true,
    delete_inflation_curve_config_request: true,
    delete_many_inflation_curve_configs_request: true,
    list_inflation_curve_config_versions_request: true,
    get_inflation_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_curve_configs_events.created',
    updated: 'refdata.v1.inflation_curve_configs_events.updated',
    deleted: 'refdata.v1.inflation_curve_configs_events.deleted',
} as const;

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
import type { CurveVolatilityConfig } from '../domain/curve_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityConfigKey {
    id: string;
}

export interface CurveVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    kind: string;
    is_wrapped: boolean;
    priority: number | null;
    quote_type: string | null;
    volatility_type: string | null;
    exercise_type: string | null;
    strikes: string | null;
    expiries: string | null;
    time_interpolation: string | null;
    strike_interpolation: string | null;
    extrapolation: string | null;
    time_extrapolation: string | null;
    time_extrapolation_variance: string | null;
    strike_extrapolation: string | null;
    calendar: string | null;
    quote: string | null;
    interpolation: string | null;
    enforce_monotone_variance: boolean | null;
    delta_type: string | null;
    atm_type: string | null;
    atm_delta_type: string | null;
    put_deltas: string | null;
    call_deltas: string | null;
    future_price_correction: string | null;
    proxy_volatility_curve: string | null;
    fx_volatility_curve: string | null;
    correlation_curve: string | null;
    cds_volatility_curve: string | null;
    position: number;
}

export interface CurveVolatilityConfigChange {
    write: CurveVolatilityConfigWrite;
    precondition: Precondition;
}

export interface CurveVolatilityConfigRemoval {
    key: CurveVolatilityConfigKey;
    precondition: Precondition;
}

export interface CurveVolatilityConfigLookup {
    key: CurveVolatilityConfigKey;
    curve_volatility_config: CurveVolatilityConfig | null;
}

export interface CurveVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityConfigEvent {
    event_id: string;
    key: CurveVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityConfigVersionKey {
    curve_volatility_config: CurveVolatilityConfigKey;
    version: number;
}

export interface CurveVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityConfigsFilter | null;
}

export interface ListCurveVolatilityConfigsResponse {
    result: Result;
    volatility_configs: CurveVolatilityConfig[];
    total: number;
}

export interface GetCurveVolatilityConfigRequest {
    key: CurveVolatilityConfigKey;
}

export interface GetCurveVolatilityConfigResponse {
    result: Result;
    curve_volatility_config: CurveVolatilityConfig | null;
}

export interface GetManyCurveVolatilityConfigsRequest {
    keys: CurveVolatilityConfigKey[];
}

export interface GetManyCurveVolatilityConfigsResponse {
    result: Result;
    entries: CurveVolatilityConfigLookup[];
}

export interface PutCurveVolatilityConfigRequest {
    change: CurveVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityConfigResponse {
    result: Result;
    curve_volatility_config: CurveVolatilityConfig | null;
}

export interface PutManyCurveVolatilityConfigsRequest {
    changes: CurveVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityConfigsResponse {
    result: Result;
    volatility_configs: CurveVolatilityConfig[];
}

export interface DeleteCurveVolatilityConfigRequest {
    removal: CurveVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityConfigsRequest {
    removals: CurveVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityConfigsResponse {
    result: Result;
}

export interface ListCurveVolatilityConfigVersionsRequest {
    key: CurveVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityConfigVersionsFilter | null;
}

export interface ListCurveVolatilityConfigVersionsResponse {
    result: Result;
    versions: CurveVolatilityConfig[];
    total: number;
}

export interface GetCurveVolatilityConfigVersionRequest {
    key: CurveVolatilityConfigVersionKey;
}

export interface GetCurveVolatilityConfigVersionResponse {
    result: Result;
    version: CurveVolatilityConfig | null;
}

export const subjects = {
    list_curve_volatility_configs_request: 'refdata.v1.curve_volatility_configs.list',
    get_curve_volatility_config_request: 'refdata.v1.curve_volatility_configs.get',
    get_many_curve_volatility_configs_request: 'refdata.v1.curve_volatility_configs.get_many',
    put_curve_volatility_config_request: 'refdata.v1.curve_volatility_configs.put',
    put_many_curve_volatility_configs_request: 'refdata.v1.curve_volatility_configs.put_many',
    delete_curve_volatility_config_request: 'refdata.v1.curve_volatility_configs.delete',
    delete_many_curve_volatility_configs_request: 'refdata.v1.curve_volatility_configs.delete_many',
    list_curve_volatility_config_versions_request:
        'refdata.v1.curve_volatility_configs_versions.list',
    get_curve_volatility_config_version_request: 'refdata.v1.curve_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_configs_request: true,
    get_curve_volatility_config_request: true,
    get_many_curve_volatility_configs_request: true,
    put_curve_volatility_config_request: true,
    put_many_curve_volatility_configs_request: true,
    delete_curve_volatility_config_request: true,
    delete_many_curve_volatility_configs_request: true,
    list_curve_volatility_config_versions_request: true,
    get_curve_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_configs_events.created',
    updated: 'refdata.v1.curve_volatility_configs_events.updated',
    deleted: 'refdata.v1.curve_volatility_configs_events.deleted',
} as const;

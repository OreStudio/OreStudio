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
import type { EquityCurveConfig } from '../domain/equity_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityCurveConfigKey {
    id: string;
}

export interface EquityCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    calendar: string | null;
    forecasting_curve: string;
    equity_type: string;
    exercise_style: string | null;
    spot_quote: string;
    has_quotes: boolean;
    day_counter: string | null;
    has_dividend_interpolation: boolean;
    dividend_interpolation_variable: string | null;
    dividend_interpolation_method: string | null;
    dividend_extrapolation: string | null;
    extrapolation: string | null;
}

export interface EquityCurveConfigChange {
    write: EquityCurveConfigWrite;
    precondition: Precondition;
}

export interface EquityCurveConfigRemoval {
    key: EquityCurveConfigKey;
    precondition: Precondition;
}

export interface EquityCurveConfigLookup {
    key: EquityCurveConfigKey;
    equity_curve_config: EquityCurveConfig | null;
}

export interface EquityCurveConfigEvent {
    event_id: string;
    key: EquityCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityCurveConfigVersionKey {
    equity_curve_config: EquityCurveConfigKey;
    version: number;
}

export interface EquityCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListEquityCurveConfigsResponse {
    result: Result;
    equity_curve_configs: EquityCurveConfig[];
    total: number;
}

export interface GetEquityCurveConfigRequest {
    key: EquityCurveConfigKey;
}

export interface GetEquityCurveConfigResponse {
    result: Result;
    equity_curve_config: EquityCurveConfig | null;
}

export interface GetManyEquityCurveConfigsRequest {
    keys: EquityCurveConfigKey[];
}

export interface GetManyEquityCurveConfigsResponse {
    result: Result;
    entries: EquityCurveConfigLookup[];
}

export interface PutEquityCurveConfigRequest {
    change: EquityCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutEquityCurveConfigResponse {
    result: Result;
    equity_curve_config: EquityCurveConfig | null;
}

export interface PutManyEquityCurveConfigsRequest {
    changes: EquityCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityCurveConfigsResponse {
    result: Result;
    equity_curve_configs: EquityCurveConfig[];
}

export interface DeleteEquityCurveConfigRequest {
    removal: EquityCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityCurveConfigResponse {
    result: Result;
}

export interface DeleteManyEquityCurveConfigsRequest {
    removals: EquityCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityCurveConfigsResponse {
    result: Result;
}

export interface ListEquityCurveConfigVersionsRequest {
    key: EquityCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityCurveConfigVersionsFilter | null;
}

export interface ListEquityCurveConfigVersionsResponse {
    result: Result;
    versions: EquityCurveConfig[];
    total: number;
}

export interface GetEquityCurveConfigVersionRequest {
    key: EquityCurveConfigVersionKey;
}

export interface GetEquityCurveConfigVersionResponse {
    result: Result;
    version: EquityCurveConfig | null;
}

export const subjects = {
    list_equity_curve_configs_request: 'refdata.v1.equity_curve_configs.list',
    get_equity_curve_config_request: 'refdata.v1.equity_curve_configs.get',
    get_many_equity_curve_configs_request: 'refdata.v1.equity_curve_configs.get_many',
    put_equity_curve_config_request: 'refdata.v1.equity_curve_configs.put',
    put_many_equity_curve_configs_request: 'refdata.v1.equity_curve_configs.put_many',
    delete_equity_curve_config_request: 'refdata.v1.equity_curve_configs.delete',
    delete_many_equity_curve_configs_request: 'refdata.v1.equity_curve_configs.delete_many',
    list_equity_curve_config_versions_request: 'refdata.v1.equity_curve_configs_versions.list',
    get_equity_curve_config_version_request: 'refdata.v1.equity_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_curve_configs_request: true,
    get_equity_curve_config_request: true,
    get_many_equity_curve_configs_request: true,
    put_equity_curve_config_request: true,
    put_many_equity_curve_configs_request: true,
    delete_equity_curve_config_request: true,
    delete_many_equity_curve_configs_request: true,
    list_equity_curve_config_versions_request: true,
    get_equity_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.equity_curve_configs_events.created',
    updated: 'refdata.v1.equity_curve_configs_events.updated',
    deleted: 'refdata.v1.equity_curve_configs_events.deleted',
} as const;

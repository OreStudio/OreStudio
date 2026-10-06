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
import type { CurveCorrelationConfig } from '../domain/curve_correlation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveCorrelationConfigKey {
    id: string;
}

export interface CurveCorrelationConfigWrite {
    id: string;
    curve_definition_id: string;
    correlation_type: string;
    index_1: string | null;
    index_2: string | null;
    conventions: string | null;
    swaption_volatility: string | null;
    discount_curve: string | null;
    currency: string | null;
    dimension: string | null;
    quote_type: string | null;
    extrapolation: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    option_tenors: string | null;
}

export interface CurveCorrelationConfigChange {
    write: CurveCorrelationConfigWrite;
    precondition: Precondition;
}

export interface CurveCorrelationConfigRemoval {
    key: CurveCorrelationConfigKey;
    precondition: Precondition;
}

export interface CurveCorrelationConfigLookup {
    key: CurveCorrelationConfigKey;
    curve_correlation_config: CurveCorrelationConfig | null;
}

export interface CurveCorrelationConfigsFilter {
    id_one_of: string[] | null;
}

export interface CurveCorrelationConfigEvent {
    event_id: string;
    key: CurveCorrelationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveCorrelationConfigVersionKey {
    curve_correlation_config: CurveCorrelationConfigKey;
    version: number;
}

export interface CurveCorrelationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveCorrelationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveCorrelationConfigsFilter | null;
    as_of: string | null;
}

export interface ListCurveCorrelationConfigsResponse {
    result: Result;
    correlation_configs: CurveCorrelationConfig[];
    total: number;
}

export interface GetCurveCorrelationConfigRequest {
    key: CurveCorrelationConfigKey;
}

export interface GetCurveCorrelationConfigResponse {
    result: Result;
    curve_correlation_config: CurveCorrelationConfig | null;
}

export interface GetManyCurveCorrelationConfigsRequest {
    keys: CurveCorrelationConfigKey[];
}

export interface GetManyCurveCorrelationConfigsResponse {
    result: Result;
    entries: CurveCorrelationConfigLookup[];
}

export interface PutCurveCorrelationConfigRequest {
    change: CurveCorrelationConfigChange;
    intent: ChangeIntent;
}

export interface PutCurveCorrelationConfigResponse {
    result: Result;
    curve_correlation_config: CurveCorrelationConfig | null;
}

export interface PutManyCurveCorrelationConfigsRequest {
    changes: CurveCorrelationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveCorrelationConfigsResponse {
    result: Result;
    correlation_configs: CurveCorrelationConfig[];
}

export interface DeleteCurveCorrelationConfigRequest {
    removal: CurveCorrelationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveCorrelationConfigResponse {
    result: Result;
}

export interface DeleteManyCurveCorrelationConfigsRequest {
    removals: CurveCorrelationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveCorrelationConfigsResponse {
    result: Result;
}

export interface ListCurveCorrelationConfigVersionsRequest {
    key: CurveCorrelationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveCorrelationConfigVersionsFilter | null;
}

export interface ListCurveCorrelationConfigVersionsResponse {
    result: Result;
    versions: CurveCorrelationConfig[];
    total: number;
}

export interface GetCurveCorrelationConfigVersionRequest {
    key: CurveCorrelationConfigVersionKey;
}

export interface GetCurveCorrelationConfigVersionResponse {
    result: Result;
    version: CurveCorrelationConfig | null;
}

export const subjects = {
    list_curve_correlation_configs_request: 'refdata.v1.curve_correlation_configs.list',
    get_curve_correlation_config_request: 'refdata.v1.curve_correlation_configs.get',
    get_many_curve_correlation_configs_request: 'refdata.v1.curve_correlation_configs.get_many',
    put_curve_correlation_config_request: 'refdata.v1.curve_correlation_configs.put',
    put_many_curve_correlation_configs_request: 'refdata.v1.curve_correlation_configs.put_many',
    delete_curve_correlation_config_request: 'refdata.v1.curve_correlation_configs.delete',
    delete_many_curve_correlation_configs_request:
        'refdata.v1.curve_correlation_configs.delete_many',
    list_curve_correlation_config_versions_request:
        'refdata.v1.curve_correlation_configs_versions.list',
    get_curve_correlation_config_version_request:
        'refdata.v1.curve_correlation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_correlation_configs_request: true,
    get_curve_correlation_config_request: true,
    get_many_curve_correlation_configs_request: true,
    put_curve_correlation_config_request: true,
    put_many_curve_correlation_configs_request: true,
    delete_curve_correlation_config_request: true,
    delete_many_curve_correlation_configs_request: true,
    list_curve_correlation_config_versions_request: true,
    get_curve_correlation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_correlation_configs_events.created',
    updated: 'refdata.v1.curve_correlation_configs_events.updated',
    deleted: 'refdata.v1.curve_correlation_configs_events.deleted',
} as const;

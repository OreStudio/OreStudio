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
import type { YieldCurveConfig } from '../domain/yield_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldCurveConfigKey {
    id: string;
}

export interface YieldCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    discount_curve: string;
    interpolation_variable: string | null;
    interpolation_method: string | null;
    mixed_interpolation_cutoff: number | null;
    day_counter: string | null;
    tolerance: number | null;
    extrapolation: string | null;
    extrapolation_method: string | null;
    exclude_t0_from_interpolation: string | null;
    has_report: boolean;
    report_pillar_dates: string | null;
}

export interface YieldCurveConfigChange {
    write: YieldCurveConfigWrite;
    precondition: Precondition;
}

export interface YieldCurveConfigRemoval {
    key: YieldCurveConfigKey;
    precondition: Precondition;
}

export interface YieldCurveConfigLookup {
    key: YieldCurveConfigKey;
    yield_curve_config: YieldCurveConfig | null;
}

export interface YieldCurveConfigsFilter {
    id_one_of: string[] | null;
}

export interface YieldCurveConfigEvent {
    event_id: string;
    key: YieldCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldCurveConfigVersionKey {
    yield_curve_config: YieldCurveConfigKey;
    version: number;
}

export interface YieldCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveConfigsFilter | null;
    as_of: string | null;
}

export interface ListYieldCurveConfigsResponse {
    result: Result;
    yield_curve_configs: YieldCurveConfig[];
    total: number;
}

export interface GetYieldCurveConfigRequest {
    key: YieldCurveConfigKey;
}

export interface GetYieldCurveConfigResponse {
    result: Result;
    yield_curve_config: YieldCurveConfig | null;
}

export interface GetManyYieldCurveConfigsRequest {
    keys: YieldCurveConfigKey[];
}

export interface GetManyYieldCurveConfigsResponse {
    result: Result;
    entries: YieldCurveConfigLookup[];
}

export interface PutYieldCurveConfigRequest {
    change: YieldCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutYieldCurveConfigResponse {
    result: Result;
    yield_curve_config: YieldCurveConfig | null;
}

export interface PutManyYieldCurveConfigsRequest {
    changes: YieldCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldCurveConfigsResponse {
    result: Result;
    yield_curve_configs: YieldCurveConfig[];
}

export interface DeleteYieldCurveConfigRequest {
    removal: YieldCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldCurveConfigResponse {
    result: Result;
}

export interface DeleteManyYieldCurveConfigsRequest {
    removals: YieldCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldCurveConfigsResponse {
    result: Result;
}

export interface ListYieldCurveConfigVersionsRequest {
    key: YieldCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveConfigVersionsFilter | null;
}

export interface ListYieldCurveConfigVersionsResponse {
    result: Result;
    versions: YieldCurveConfig[];
    total: number;
}

export interface GetYieldCurveConfigVersionRequest {
    key: YieldCurveConfigVersionKey;
}

export interface GetYieldCurveConfigVersionResponse {
    result: Result;
    version: YieldCurveConfig | null;
}

export const subjects = {
    list_yield_curve_configs_request: 'refdata.v1.yield_curve_configs.list',
    get_yield_curve_config_request: 'refdata.v1.yield_curve_configs.get',
    get_many_yield_curve_configs_request: 'refdata.v1.yield_curve_configs.get_many',
    put_yield_curve_config_request: 'refdata.v1.yield_curve_configs.put',
    put_many_yield_curve_configs_request: 'refdata.v1.yield_curve_configs.put_many',
    delete_yield_curve_config_request: 'refdata.v1.yield_curve_configs.delete',
    delete_many_yield_curve_configs_request: 'refdata.v1.yield_curve_configs.delete_many',
    list_yield_curve_config_versions_request: 'refdata.v1.yield_curve_configs_versions.list',
    get_yield_curve_config_version_request: 'refdata.v1.yield_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_curve_configs_request: true,
    get_yield_curve_config_request: true,
    get_many_yield_curve_configs_request: true,
    put_yield_curve_config_request: true,
    put_many_yield_curve_configs_request: true,
    delete_yield_curve_config_request: true,
    delete_many_yield_curve_configs_request: true,
    list_yield_curve_config_versions_request: true,
    get_yield_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.yield_curve_configs_events.created',
    updated: 'refdata.v1.yield_curve_configs_events.updated',
    deleted: 'refdata.v1.yield_curve_configs_events.deleted',
} as const;

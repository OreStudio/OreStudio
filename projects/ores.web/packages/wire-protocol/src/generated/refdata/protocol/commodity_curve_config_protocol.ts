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
import type { CommodityCurveConfig } from '../domain/commodity_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveConfigKey {
    id: string;
}

export interface CommodityCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    base_price_curve: string | null;
    base_yield_curve: string | null;
    yield_curve: string | null;
    spot_quote: string | null;
    has_quotes: boolean;
    day_counter: string | null;
    interpolation_method: string | null;
    conventions: string | null;
    extrapolation: string | null;
    has_basis_configuration: boolean;
    basis_base_price_curve: string | null;
    basis_base_price_conventions: string | null;
    basis_conventions: string | null;
    basis_day_counter: string | null;
    basis_interpolation_method: string | null;
    basis_add_basis: string | null;
    basis_month_offset: number | null;
    basis_average_base: string | null;
    basis_price_as_historical_fixing: string | null;
    has_price_segments: boolean;
}

export interface CommodityCurveConfigChange {
    write: CommodityCurveConfigWrite;
    precondition: Precondition;
}

export interface CommodityCurveConfigRemoval {
    key: CommodityCurveConfigKey;
    precondition: Precondition;
}

export interface CommodityCurveConfigLookup {
    key: CommodityCurveConfigKey;
    commodity_curve_config: CommodityCurveConfig | null;
}

export interface CommodityCurveConfigsFilter {
    id_one_of: string[] | null;
}

export interface CommodityCurveConfigEvent {
    event_id: string;
    key: CommodityCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveConfigVersionKey {
    commodity_curve_config: CommodityCurveConfigKey;
    version: number;
}

export interface CommodityCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveConfigsFilter | null;
    as_of: string | null;
}

export interface ListCommodityCurveConfigsResponse {
    result: Result;
    commodity_curve_configs: CommodityCurveConfig[];
    total: number;
}

export interface GetCommodityCurveConfigRequest {
    key: CommodityCurveConfigKey;
}

export interface GetCommodityCurveConfigResponse {
    result: Result;
    commodity_curve_config: CommodityCurveConfig | null;
}

export interface GetManyCommodityCurveConfigsRequest {
    keys: CommodityCurveConfigKey[];
}

export interface GetManyCommodityCurveConfigsResponse {
    result: Result;
    entries: CommodityCurveConfigLookup[];
}

export interface PutCommodityCurveConfigRequest {
    change: CommodityCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveConfigResponse {
    result: Result;
    commodity_curve_config: CommodityCurveConfig | null;
}

export interface PutManyCommodityCurveConfigsRequest {
    changes: CommodityCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurveConfigsResponse {
    result: Result;
    commodity_curve_configs: CommodityCurveConfig[];
}

export interface DeleteCommodityCurveConfigRequest {
    removal: CommodityCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveConfigResponse {
    result: Result;
}

export interface DeleteManyCommodityCurveConfigsRequest {
    removals: CommodityCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurveConfigsResponse {
    result: Result;
}

export interface ListCommodityCurveConfigVersionsRequest {
    key: CommodityCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveConfigVersionsFilter | null;
}

export interface ListCommodityCurveConfigVersionsResponse {
    result: Result;
    versions: CommodityCurveConfig[];
    total: number;
}

export interface GetCommodityCurveConfigVersionRequest {
    key: CommodityCurveConfigVersionKey;
}

export interface GetCommodityCurveConfigVersionResponse {
    result: Result;
    version: CommodityCurveConfig | null;
}

export const subjects = {
    list_commodity_curve_configs_request: 'refdata.v1.commodity_curve_configs.list',
    get_commodity_curve_config_request: 'refdata.v1.commodity_curve_configs.get',
    get_many_commodity_curve_configs_request: 'refdata.v1.commodity_curve_configs.get_many',
    put_commodity_curve_config_request: 'refdata.v1.commodity_curve_configs.put',
    put_many_commodity_curve_configs_request: 'refdata.v1.commodity_curve_configs.put_many',
    delete_commodity_curve_config_request: 'refdata.v1.commodity_curve_configs.delete',
    delete_many_commodity_curve_configs_request: 'refdata.v1.commodity_curve_configs.delete_many',
    list_commodity_curve_config_versions_request:
        'refdata.v1.commodity_curve_configs_versions.list',
    get_commodity_curve_config_version_request: 'refdata.v1.commodity_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curve_configs_request: true,
    get_commodity_curve_config_request: true,
    get_many_commodity_curve_configs_request: true,
    put_commodity_curve_config_request: true,
    put_many_commodity_curve_configs_request: true,
    delete_commodity_curve_config_request: true,
    delete_many_commodity_curve_configs_request: true,
    list_commodity_curve_config_versions_request: true,
    get_commodity_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_curve_configs_events.created',
    updated: 'refdata.v1.commodity_curve_configs_events.updated',
    deleted: 'refdata.v1.commodity_curve_configs_events.deleted',
} as const;

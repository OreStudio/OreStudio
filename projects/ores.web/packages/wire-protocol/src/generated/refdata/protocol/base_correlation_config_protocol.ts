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
import type { BaseCorrelationConfig } from '../domain/base_correlation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BaseCorrelationConfigKey {
    id: string;
}

export interface BaseCorrelationConfigWrite {
    id: string;
    curve_definition_id: string;
    terms: string;
    detachment_points: string;
    settlement_days: number;
    calendar: string;
    business_day_convention: string;
    day_counter: string;
    extrapolate: string | null;
    quote_name: string | null;
    start_date: string | null;
    rule: string | null;
    adjust_for_losses: string | null;
    index_term: string | null;
    index_spread: string | null;
    currency: string | null;
    calibrate_constituents_to_index_spread: string | null;
    use_assumed_recovery: string | null;
}

export interface BaseCorrelationConfigChange {
    write: BaseCorrelationConfigWrite;
    precondition: Precondition;
}

export interface BaseCorrelationConfigRemoval {
    key: BaseCorrelationConfigKey;
    precondition: Precondition;
}

export interface BaseCorrelationConfigLookup {
    key: BaseCorrelationConfigKey;
    base_correlation_config: BaseCorrelationConfig | null;
}

export interface BaseCorrelationConfigsFilter {
    id_one_of: string[] | null;
}

export interface BaseCorrelationConfigEvent {
    event_id: string;
    key: BaseCorrelationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BaseCorrelationConfigVersionKey {
    base_correlation_config: BaseCorrelationConfigKey;
    version: number;
}

export interface BaseCorrelationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBaseCorrelationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BaseCorrelationConfigsFilter | null;
}

export interface ListBaseCorrelationConfigsResponse {
    result: Result;
    base_correlation_configs: BaseCorrelationConfig[];
    total: number;
}

export interface GetBaseCorrelationConfigRequest {
    key: BaseCorrelationConfigKey;
}

export interface GetBaseCorrelationConfigResponse {
    result: Result;
    base_correlation_config: BaseCorrelationConfig | null;
}

export interface GetManyBaseCorrelationConfigsRequest {
    keys: BaseCorrelationConfigKey[];
}

export interface GetManyBaseCorrelationConfigsResponse {
    result: Result;
    entries: BaseCorrelationConfigLookup[];
}

export interface PutBaseCorrelationConfigRequest {
    change: BaseCorrelationConfigChange;
    intent: ChangeIntent;
}

export interface PutBaseCorrelationConfigResponse {
    result: Result;
    base_correlation_config: BaseCorrelationConfig | null;
}

export interface PutManyBaseCorrelationConfigsRequest {
    changes: BaseCorrelationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyBaseCorrelationConfigsResponse {
    result: Result;
    base_correlation_configs: BaseCorrelationConfig[];
}

export interface DeleteBaseCorrelationConfigRequest {
    removal: BaseCorrelationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteBaseCorrelationConfigResponse {
    result: Result;
}

export interface DeleteManyBaseCorrelationConfigsRequest {
    removals: BaseCorrelationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBaseCorrelationConfigsResponse {
    result: Result;
}

export interface ListBaseCorrelationConfigVersionsRequest {
    key: BaseCorrelationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BaseCorrelationConfigVersionsFilter | null;
}

export interface ListBaseCorrelationConfigVersionsResponse {
    result: Result;
    versions: BaseCorrelationConfig[];
    total: number;
}

export interface GetBaseCorrelationConfigVersionRequest {
    key: BaseCorrelationConfigVersionKey;
}

export interface GetBaseCorrelationConfigVersionResponse {
    result: Result;
    version: BaseCorrelationConfig | null;
}

export const subjects = {
    list_base_correlation_configs_request: 'refdata.v1.base_correlation_configs.list',
    get_base_correlation_config_request: 'refdata.v1.base_correlation_configs.get',
    get_many_base_correlation_configs_request: 'refdata.v1.base_correlation_configs.get_many',
    put_base_correlation_config_request: 'refdata.v1.base_correlation_configs.put',
    put_many_base_correlation_configs_request: 'refdata.v1.base_correlation_configs.put_many',
    delete_base_correlation_config_request: 'refdata.v1.base_correlation_configs.delete',
    delete_many_base_correlation_configs_request: 'refdata.v1.base_correlation_configs.delete_many',
    list_base_correlation_config_versions_request:
        'refdata.v1.base_correlation_configs_versions.list',
    get_base_correlation_config_version_request: 'refdata.v1.base_correlation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_base_correlation_configs_request: true,
    get_base_correlation_config_request: true,
    get_many_base_correlation_configs_request: true,
    put_base_correlation_config_request: true,
    put_many_base_correlation_configs_request: true,
    delete_base_correlation_config_request: true,
    delete_many_base_correlation_configs_request: true,
    list_base_correlation_config_versions_request: true,
    get_base_correlation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.base_correlation_configs_events.created',
    updated: 'refdata.v1.base_correlation_configs_events.updated',
    deleted: 'refdata.v1.base_correlation_configs_events.deleted',
} as const;

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
import type { CapFloorProxyConfig } from '../domain/cap_floor_proxy_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CapFloorProxyConfigKey {
    id: string;
}

export interface CapFloorProxyConfigWrite {
    id: string;
    cap_floor_volatility_config_id: string;
    source_curve_id: string | null;
    source_index: string | null;
    source_rate_computation_period: string | null;
    target_index: string | null;
    target_rate_computation_period: string | null;
    target_on_cap_settlement_days: number | null;
    scaling_factor: number | null;
}

export interface CapFloorProxyConfigChange {
    write: CapFloorProxyConfigWrite;
    precondition: Precondition;
}

export interface CapFloorProxyConfigRemoval {
    key: CapFloorProxyConfigKey;
    precondition: Precondition;
}

export interface CapFloorProxyConfigLookup {
    key: CapFloorProxyConfigKey;
    cap_floor_proxy_config: CapFloorProxyConfig | null;
}

export interface CapFloorProxyConfigsFilter {
    id_one_of: string[] | null;
}

export interface CapFloorProxyConfigEvent {
    event_id: string;
    key: CapFloorProxyConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CapFloorProxyConfigVersionKey {
    cap_floor_proxy_config: CapFloorProxyConfigKey;
    version: number;
}

export interface CapFloorProxyConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCapFloorProxyConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CapFloorProxyConfigsFilter | null;
    as_of: string | null;
}

export interface ListCapFloorProxyConfigsResponse {
    result: Result;
    cap_floor_proxy_configs: CapFloorProxyConfig[];
    total: number;
}

export interface GetCapFloorProxyConfigRequest {
    key: CapFloorProxyConfigKey;
}

export interface GetCapFloorProxyConfigResponse {
    result: Result;
    cap_floor_proxy_config: CapFloorProxyConfig | null;
}

export interface GetManyCapFloorProxyConfigsRequest {
    keys: CapFloorProxyConfigKey[];
}

export interface GetManyCapFloorProxyConfigsResponse {
    result: Result;
    entries: CapFloorProxyConfigLookup[];
}

export interface PutCapFloorProxyConfigRequest {
    change: CapFloorProxyConfigChange;
    intent: ChangeIntent;
}

export interface PutCapFloorProxyConfigResponse {
    result: Result;
    cap_floor_proxy_config: CapFloorProxyConfig | null;
}

export interface PutManyCapFloorProxyConfigsRequest {
    changes: CapFloorProxyConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCapFloorProxyConfigsResponse {
    result: Result;
    cap_floor_proxy_configs: CapFloorProxyConfig[];
}

export interface DeleteCapFloorProxyConfigRequest {
    removal: CapFloorProxyConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCapFloorProxyConfigResponse {
    result: Result;
}

export interface DeleteManyCapFloorProxyConfigsRequest {
    removals: CapFloorProxyConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCapFloorProxyConfigsResponse {
    result: Result;
}

export interface ListCapFloorProxyConfigVersionsRequest {
    key: CapFloorProxyConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CapFloorProxyConfigVersionsFilter | null;
}

export interface ListCapFloorProxyConfigVersionsResponse {
    result: Result;
    versions: CapFloorProxyConfig[];
    total: number;
}

export interface GetCapFloorProxyConfigVersionRequest {
    key: CapFloorProxyConfigVersionKey;
}

export interface GetCapFloorProxyConfigVersionResponse {
    result: Result;
    version: CapFloorProxyConfig | null;
}

export const subjects = {
    list_cap_floor_proxy_configs_request: 'refdata.v1.cap_floor_proxy_configs.list',
    get_cap_floor_proxy_config_request: 'refdata.v1.cap_floor_proxy_configs.get',
    get_many_cap_floor_proxy_configs_request: 'refdata.v1.cap_floor_proxy_configs.get_many',
    put_cap_floor_proxy_config_request: 'refdata.v1.cap_floor_proxy_configs.put',
    put_many_cap_floor_proxy_configs_request: 'refdata.v1.cap_floor_proxy_configs.put_many',
    delete_cap_floor_proxy_config_request: 'refdata.v1.cap_floor_proxy_configs.delete',
    delete_many_cap_floor_proxy_configs_request: 'refdata.v1.cap_floor_proxy_configs.delete_many',
    list_cap_floor_proxy_config_versions_request:
        'refdata.v1.cap_floor_proxy_configs_versions.list',
    get_cap_floor_proxy_config_version_request: 'refdata.v1.cap_floor_proxy_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cap_floor_proxy_configs_request: true,
    get_cap_floor_proxy_config_request: true,
    get_many_cap_floor_proxy_configs_request: true,
    put_cap_floor_proxy_config_request: true,
    put_many_cap_floor_proxy_configs_request: true,
    delete_cap_floor_proxy_config_request: true,
    delete_many_cap_floor_proxy_configs_request: true,
    list_cap_floor_proxy_config_versions_request: true,
    get_cap_floor_proxy_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cap_floor_proxy_configs_events.created',
    updated: 'refdata.v1.cap_floor_proxy_configs_events.updated',
    deleted: 'refdata.v1.cap_floor_proxy_configs_events.deleted',
} as const;

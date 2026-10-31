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
import type { SwaptionProxyConfig } from '../domain/swaption_proxy_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwaptionProxyConfigKey {
    id: string;
}

export interface SwaptionProxyConfigWrite {
    id: string;
    swaption_volatility_config_id: string;
    source_curve_id: string | null;
    source_short_swap_index_base: string | null;
    source_swap_index_base: string | null;
    target_short_swap_index_base: string | null;
    target_swap_index_base: string | null;
}

export interface SwaptionProxyConfigChange {
    write: SwaptionProxyConfigWrite;
    precondition: Precondition;
}

export interface SwaptionProxyConfigRemoval {
    key: SwaptionProxyConfigKey;
    precondition: Precondition;
}

export interface SwaptionProxyConfigLookup {
    key: SwaptionProxyConfigKey;
    swaption_proxy_config: SwaptionProxyConfig | null;
}

export interface SwaptionProxyConfigsFilter {
    id_one_of: string[] | null;
}

export interface SwaptionProxyConfigEvent {
    event_id: string;
    key: SwaptionProxyConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwaptionProxyConfigVersionKey {
    swaption_proxy_config: SwaptionProxyConfigKey;
    version: number;
}

export interface SwaptionProxyConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwaptionProxyConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionProxyConfigsFilter | null;
    as_of: string | null;
}

export interface ListSwaptionProxyConfigsResponse {
    result: Result;
    swaption_proxy_configs: SwaptionProxyConfig[];
    total: number;
}

export interface GetSwaptionProxyConfigRequest {
    key: SwaptionProxyConfigKey;
}

export interface GetSwaptionProxyConfigResponse {
    result: Result;
    swaption_proxy_config: SwaptionProxyConfig | null;
}

export interface GetManySwaptionProxyConfigsRequest {
    keys: SwaptionProxyConfigKey[];
}

export interface GetManySwaptionProxyConfigsResponse {
    result: Result;
    entries: SwaptionProxyConfigLookup[];
}

export interface PutSwaptionProxyConfigRequest {
    change: SwaptionProxyConfigChange;
    intent: ChangeIntent;
}

export interface PutSwaptionProxyConfigResponse {
    result: Result;
    swaption_proxy_config: SwaptionProxyConfig | null;
}

export interface PutManySwaptionProxyConfigsRequest {
    changes: SwaptionProxyConfigChange[];
    intent: ChangeIntent;
}

export interface PutManySwaptionProxyConfigsResponse {
    result: Result;
    swaption_proxy_configs: SwaptionProxyConfig[];
}

export interface DeleteSwaptionProxyConfigRequest {
    removal: SwaptionProxyConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwaptionProxyConfigResponse {
    result: Result;
}

export interface DeleteManySwaptionProxyConfigsRequest {
    removals: SwaptionProxyConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwaptionProxyConfigsResponse {
    result: Result;
}

export interface ListSwaptionProxyConfigVersionsRequest {
    key: SwaptionProxyConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionProxyConfigVersionsFilter | null;
}

export interface ListSwaptionProxyConfigVersionsResponse {
    result: Result;
    versions: SwaptionProxyConfig[];
    total: number;
}

export interface GetSwaptionProxyConfigVersionRequest {
    key: SwaptionProxyConfigVersionKey;
}

export interface GetSwaptionProxyConfigVersionResponse {
    result: Result;
    version: SwaptionProxyConfig | null;
}

export const subjects = {
    list_swaption_proxy_configs_request: 'refdata.v1.swaption_proxy_configs.list',
    get_swaption_proxy_config_request: 'refdata.v1.swaption_proxy_configs.get',
    get_many_swaption_proxy_configs_request: 'refdata.v1.swaption_proxy_configs.get_many',
    put_swaption_proxy_config_request: 'refdata.v1.swaption_proxy_configs.put',
    put_many_swaption_proxy_configs_request: 'refdata.v1.swaption_proxy_configs.put_many',
    delete_swaption_proxy_config_request: 'refdata.v1.swaption_proxy_configs.delete',
    delete_many_swaption_proxy_configs_request: 'refdata.v1.swaption_proxy_configs.delete_many',
    list_swaption_proxy_config_versions_request: 'refdata.v1.swaption_proxy_configs_versions.list',
    get_swaption_proxy_config_version_request: 'refdata.v1.swaption_proxy_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swaption_proxy_configs_request: true,
    get_swaption_proxy_config_request: true,
    get_many_swaption_proxy_configs_request: true,
    put_swaption_proxy_config_request: true,
    put_many_swaption_proxy_configs_request: true,
    delete_swaption_proxy_config_request: true,
    delete_many_swaption_proxy_configs_request: true,
    list_swaption_proxy_config_versions_request: true,
    get_swaption_proxy_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.swaption_proxy_configs_events.created',
    updated: 'refdata.v1.swaption_proxy_configs_events.updated',
    deleted: 'refdata.v1.swaption_proxy_configs_events.deleted',
} as const;

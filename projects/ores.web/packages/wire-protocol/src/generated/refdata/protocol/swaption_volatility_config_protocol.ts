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
import type { SwaptionVolatilityConfig } from '../domain/swaption_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwaptionVolatilityConfigKey {
    id: string;
}

export interface SwaptionVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    dimension: string | null;
    volatility_type: string | null;
    interpolation: string | null;
    extrapolation: string | null;
    output_volatility_type: string | null;
    model_shift: string | null;
    output_shift: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    option_tenors: string | null;
    swap_tenors: string | null;
    short_swap_index_base: string | null;
    swap_index_base: string | null;
    smile_option_tenors: string | null;
    smile_swap_tenors: string | null;
    smile_spreads: string | null;
    quote_tag: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_short_swap_index_base: string | null;
    proxy_source_swap_index_base: string | null;
    proxy_target_short_swap_index_base: string | null;
    proxy_target_swap_index_base: string | null;
}

export interface SwaptionVolatilityConfigChange {
    write: SwaptionVolatilityConfigWrite;
    precondition: Precondition;
}

export interface SwaptionVolatilityConfigRemoval {
    key: SwaptionVolatilityConfigKey;
    precondition: Precondition;
}

export interface SwaptionVolatilityConfigLookup {
    key: SwaptionVolatilityConfigKey;
    swaption_volatility_config: SwaptionVolatilityConfig | null;
}

export interface SwaptionVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface SwaptionVolatilityConfigEvent {
    event_id: string;
    key: SwaptionVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwaptionVolatilityConfigVersionKey {
    swaption_volatility_config: SwaptionVolatilityConfigKey;
    version: number;
}

export interface SwaptionVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwaptionVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionVolatilityConfigsFilter | null;
    as_of: string | null;
}

export interface ListSwaptionVolatilityConfigsResponse {
    result: Result;
    swaption_volatility_configs: SwaptionVolatilityConfig[];
    total: number;
}

export interface GetSwaptionVolatilityConfigRequest {
    key: SwaptionVolatilityConfigKey;
}

export interface GetSwaptionVolatilityConfigResponse {
    result: Result;
    swaption_volatility_config: SwaptionVolatilityConfig | null;
}

export interface GetManySwaptionVolatilityConfigsRequest {
    keys: SwaptionVolatilityConfigKey[];
}

export interface GetManySwaptionVolatilityConfigsResponse {
    result: Result;
    entries: SwaptionVolatilityConfigLookup[];
}

export interface PutSwaptionVolatilityConfigRequest {
    change: SwaptionVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutSwaptionVolatilityConfigResponse {
    result: Result;
    swaption_volatility_config: SwaptionVolatilityConfig | null;
}

export interface PutManySwaptionVolatilityConfigsRequest {
    changes: SwaptionVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManySwaptionVolatilityConfigsResponse {
    result: Result;
    swaption_volatility_configs: SwaptionVolatilityConfig[];
}

export interface DeleteSwaptionVolatilityConfigRequest {
    removal: SwaptionVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwaptionVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManySwaptionVolatilityConfigsRequest {
    removals: SwaptionVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwaptionVolatilityConfigsResponse {
    result: Result;
}

export interface ListSwaptionVolatilityConfigVersionsRequest {
    key: SwaptionVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionVolatilityConfigVersionsFilter | null;
}

export interface ListSwaptionVolatilityConfigVersionsResponse {
    result: Result;
    versions: SwaptionVolatilityConfig[];
    total: number;
}

export interface GetSwaptionVolatilityConfigVersionRequest {
    key: SwaptionVolatilityConfigVersionKey;
}

export interface GetSwaptionVolatilityConfigVersionResponse {
    result: Result;
    version: SwaptionVolatilityConfig | null;
}

export const subjects = {
    list_swaption_volatility_configs_request: 'refdata.v1.swaption_volatility_configs.list',
    get_swaption_volatility_config_request: 'refdata.v1.swaption_volatility_configs.get',
    get_many_swaption_volatility_configs_request: 'refdata.v1.swaption_volatility_configs.get_many',
    put_swaption_volatility_config_request: 'refdata.v1.swaption_volatility_configs.put',
    put_many_swaption_volatility_configs_request: 'refdata.v1.swaption_volatility_configs.put_many',
    delete_swaption_volatility_config_request: 'refdata.v1.swaption_volatility_configs.delete',
    delete_many_swaption_volatility_configs_request:
        'refdata.v1.swaption_volatility_configs.delete_many',
    list_swaption_volatility_config_versions_request:
        'refdata.v1.swaption_volatility_configs_versions.list',
    get_swaption_volatility_config_version_request:
        'refdata.v1.swaption_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swaption_volatility_configs_request: true,
    get_swaption_volatility_config_request: true,
    get_many_swaption_volatility_configs_request: true,
    put_swaption_volatility_config_request: true,
    put_many_swaption_volatility_configs_request: true,
    delete_swaption_volatility_config_request: true,
    delete_many_swaption_volatility_configs_request: true,
    list_swaption_volatility_config_versions_request: true,
    get_swaption_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.swaption_volatility_configs_events.created',
    updated: 'refdata.v1.swaption_volatility_configs_events.updated',
    deleted: 'refdata.v1.swaption_volatility_configs_events.deleted',
} as const;

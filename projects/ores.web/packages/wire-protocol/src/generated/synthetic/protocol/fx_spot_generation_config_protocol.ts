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
import type { FxSpotGenerationConfig } from '../domain/fx_spot_generation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxSpotGenerationConfigKey {
    id: string;
}

export interface FxSpotGenerationConfigWrite {
    id: string;
    party_id: string;
    config_id: string;
    base_currency_code: string;
    quote_currency_code: string;
    source_name: string;
    ore_key: string;
    price_source: string;
    gmm_initial_price: number;
    ticks_per_hour: number;
    process_type: string;
    enabled: boolean;
    auto_start: boolean;
    vintage_source: string;
    vintage_date: string;
    folder_id: string | null;
}

export interface FxSpotGenerationConfigChange {
    write: FxSpotGenerationConfigWrite;
    precondition: Precondition;
}

export interface FxSpotGenerationConfigRemoval {
    key: FxSpotGenerationConfigKey;
    precondition: Precondition;
}

export interface FxSpotGenerationConfigLookup {
    key: FxSpotGenerationConfigKey;
    fx_spot_generation_config: FxSpotGenerationConfig | null;
}

export interface FxSpotGenerationConfigsFilter {
    id_one_of: string[] | null;
}

export interface FxSpotGenerationConfigEvent {
    event_id: string;
    key: FxSpotGenerationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxSpotGenerationConfigVersionKey {
    fx_spot_generation_config: FxSpotGenerationConfigKey;
    version: number;
}

export interface FxSpotGenerationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxSpotGenerationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxSpotGenerationConfigsFilter | null;
    as_of: string | null;
}

export interface ListFxSpotGenerationConfigsResponse {
    result: Result;
    fx_spot_generation_configs: FxSpotGenerationConfig[];
    total: number;
}

export interface GetFxSpotGenerationConfigRequest {
    key: FxSpotGenerationConfigKey;
}

export interface GetFxSpotGenerationConfigResponse {
    result: Result;
    fx_spot_generation_config: FxSpotGenerationConfig | null;
}

export interface GetManyFxSpotGenerationConfigsRequest {
    keys: FxSpotGenerationConfigKey[];
}

export interface GetManyFxSpotGenerationConfigsResponse {
    result: Result;
    entries: FxSpotGenerationConfigLookup[];
}

export interface PutFxSpotGenerationConfigRequest {
    change: FxSpotGenerationConfigChange;
    intent: ChangeIntent;
}

export interface PutFxSpotGenerationConfigResponse {
    result: Result;
    fx_spot_generation_config: FxSpotGenerationConfig | null;
}

export interface PutManyFxSpotGenerationConfigsRequest {
    changes: FxSpotGenerationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyFxSpotGenerationConfigsResponse {
    result: Result;
    fx_spot_generation_configs: FxSpotGenerationConfig[];
}

export interface DeleteFxSpotGenerationConfigRequest {
    removal: FxSpotGenerationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxSpotGenerationConfigResponse {
    result: Result;
}

export interface DeleteManyFxSpotGenerationConfigsRequest {
    removals: FxSpotGenerationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxSpotGenerationConfigsResponse {
    result: Result;
}

export interface ListFxSpotGenerationConfigVersionsRequest {
    key: FxSpotGenerationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxSpotGenerationConfigVersionsFilter | null;
}

export interface ListFxSpotGenerationConfigVersionsResponse {
    result: Result;
    versions: FxSpotGenerationConfig[];
    total: number;
}

export interface GetFxSpotGenerationConfigVersionRequest {
    key: FxSpotGenerationConfigVersionKey;
}

export interface GetFxSpotGenerationConfigVersionResponse {
    result: Result;
    version: FxSpotGenerationConfig | null;
}

export const subjects = {
    list_fx_spot_generation_configs_request: 'synthetic.v1.fx_spot_generation_configs.list',
    get_fx_spot_generation_config_request: 'synthetic.v1.fx_spot_generation_configs.get',
    get_many_fx_spot_generation_configs_request: 'synthetic.v1.fx_spot_generation_configs.get_many',
    put_fx_spot_generation_config_request: 'synthetic.v1.fx_spot_generation_configs.put',
    put_many_fx_spot_generation_configs_request: 'synthetic.v1.fx_spot_generation_configs.put_many',
    delete_fx_spot_generation_config_request: 'synthetic.v1.fx_spot_generation_configs.delete',
    delete_many_fx_spot_generation_configs_request:
        'synthetic.v1.fx_spot_generation_configs.delete_many',
    list_fx_spot_generation_config_versions_request:
        'synthetic.v1.fx_spot_generation_configs_versions.list',
    get_fx_spot_generation_config_version_request:
        'synthetic.v1.fx_spot_generation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_spot_generation_configs_request: true,
    get_fx_spot_generation_config_request: true,
    get_many_fx_spot_generation_configs_request: true,
    put_fx_spot_generation_config_request: true,
    put_many_fx_spot_generation_configs_request: true,
    delete_fx_spot_generation_config_request: true,
    delete_many_fx_spot_generation_configs_request: true,
    list_fx_spot_generation_config_versions_request: true,
    get_fx_spot_generation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.fx_spot_generation_configs_events.created',
    updated: 'synthetic.v1.fx_spot_generation_configs_events.updated',
    deleted: 'synthetic.v1.fx_spot_generation_configs_events.deleted',
} as const;

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
import type { SyntheticFxSpotConfig } from '../domain/synthetic_fx_spot_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SyntheticFxSpotConfigKey {
    id: string;
}

export interface SyntheticFxSpotConfigWrite {
    id: string;
    name: string;
    description: string | null;
    enabled: boolean;
    auto_start: boolean;
    base_currency_code: string;
    quote_currency_code: string;
    gmm_initial_price: number;
    ticks_per_hour: number;
    process_type: string;
    price_source: string;
    vintage_source: string | null;
    vintage_date: string | null;
}

export interface SyntheticFxSpotConfigChange {
    write: SyntheticFxSpotConfigWrite;
    precondition: Precondition;
}

export interface SyntheticFxSpotConfigRemoval {
    key: SyntheticFxSpotConfigKey;
    precondition: Precondition;
}

export interface SyntheticFxSpotConfigLookup {
    key: SyntheticFxSpotConfigKey;
    synthetic_fx_spot_config: SyntheticFxSpotConfig | null;
}

export interface SyntheticFxSpotConfigEvent {
    event_id: string;
    key: SyntheticFxSpotConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SyntheticFxSpotConfigVersionKey {
    synthetic_fx_spot_config: SyntheticFxSpotConfigKey;
    version: number;
}

export interface SyntheticFxSpotConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSyntheticFxSpotConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSyntheticFxSpotConfigsResponse {
    result: Result;
    configs: SyntheticFxSpotConfig[];
    total: number;
}

export interface GetSyntheticFxSpotConfigRequest {
    key: SyntheticFxSpotConfigKey;
}

export interface GetSyntheticFxSpotConfigResponse {
    result: Result;
    synthetic_fx_spot_config: SyntheticFxSpotConfig | null;
}

export interface GetManySyntheticFxSpotConfigsRequest {
    keys: SyntheticFxSpotConfigKey[];
}

export interface GetManySyntheticFxSpotConfigsResponse {
    result: Result;
    entries: SyntheticFxSpotConfigLookup[];
}

export interface PutSyntheticFxSpotConfigRequest {
    change: SyntheticFxSpotConfigChange;
    intent: ChangeIntent;
}

export interface PutSyntheticFxSpotConfigResponse {
    result: Result;
    synthetic_fx_spot_config: SyntheticFxSpotConfig;
}

export interface PutManySyntheticFxSpotConfigsRequest {
    changes: SyntheticFxSpotConfigChange[];
    intent: ChangeIntent;
}

export interface PutManySyntheticFxSpotConfigsResponse {
    result: Result;
    configs: SyntheticFxSpotConfig[];
}

export interface DeleteSyntheticFxSpotConfigRequest {
    removal: SyntheticFxSpotConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteSyntheticFxSpotConfigResponse {
    result: Result;
}

export interface DeleteManySyntheticFxSpotConfigsRequest {
    removals: SyntheticFxSpotConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySyntheticFxSpotConfigsResponse {
    result: Result;
}

export interface ListSyntheticFxSpotConfigVersionsRequest {
    key: SyntheticFxSpotConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SyntheticFxSpotConfigVersionsFilter | null;
}

export interface ListSyntheticFxSpotConfigVersionsResponse {
    result: Result;
    versions: SyntheticFxSpotConfig[];
    total: number;
}

export interface GetSyntheticFxSpotConfigVersionRequest {
    key: SyntheticFxSpotConfigVersionKey;
}

export interface GetSyntheticFxSpotConfigVersionResponse {
    result: Result;
    version: SyntheticFxSpotConfig;
}

export const subjects = {
    list_synthetic_fx_spot_configs_request: "dq.v1.synthetic_fx_spot_configs.list",
    get_synthetic_fx_spot_config_request: "dq.v1.synthetic_fx_spot_configs.get",
    get_many_synthetic_fx_spot_configs_request: "dq.v1.synthetic_fx_spot_configs.get_many",
    put_synthetic_fx_spot_config_request: "dq.v1.synthetic_fx_spot_configs.put",
    put_many_synthetic_fx_spot_configs_request: "dq.v1.synthetic_fx_spot_configs.put_many",
    delete_synthetic_fx_spot_config_request: "dq.v1.synthetic_fx_spot_configs.delete",
    delete_many_synthetic_fx_spot_configs_request: "dq.v1.synthetic_fx_spot_configs.delete_many",
    list_synthetic_fx_spot_config_versions_request: "dq.v1.synthetic_fx_spot_configs_versions.list",
    get_synthetic_fx_spot_config_version_request: "dq.v1.synthetic_fx_spot_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_synthetic_fx_spot_configs_request: true,
    get_synthetic_fx_spot_config_request: true,
    get_many_synthetic_fx_spot_configs_request: true,
    put_synthetic_fx_spot_config_request: true,
    put_many_synthetic_fx_spot_configs_request: true,
    delete_synthetic_fx_spot_config_request: true,
    delete_many_synthetic_fx_spot_configs_request: true,
    list_synthetic_fx_spot_config_versions_request: true,
    get_synthetic_fx_spot_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.synthetic_fx_spot_configs_events.created",
    updated: "dq.v1.synthetic_fx_spot_configs_events.updated",
    deleted: "dq.v1.synthetic_fx_spot_configs_events.deleted",
} as const;

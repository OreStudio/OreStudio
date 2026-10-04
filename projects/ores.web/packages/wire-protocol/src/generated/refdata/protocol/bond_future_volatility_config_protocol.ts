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
import type { BondFutureVolatilityConfig } from '../domain/bond_future_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondFutureVolatilityConfigKey {
    id: string;
}

export interface BondFutureVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    contract_name: string;
    day_counter: string | null;
    calendar: string | null;
    yield_curve_id: string | null;
    strike_factor: number | null;
    use_only_put_call: string | null;
    prefer_out_of_the_money: string | null;
    treat_as_european: string | null;
    has_volatility_config: boolean;
}

export interface BondFutureVolatilityConfigChange {
    write: BondFutureVolatilityConfigWrite;
    precondition: Precondition;
}

export interface BondFutureVolatilityConfigRemoval {
    key: BondFutureVolatilityConfigKey;
    precondition: Precondition;
}

export interface BondFutureVolatilityConfigLookup {
    key: BondFutureVolatilityConfigKey;
    bond_future_volatility_config: BondFutureVolatilityConfig | null;
}

export interface BondFutureVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface BondFutureVolatilityConfigEvent {
    event_id: string;
    key: BondFutureVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondFutureVolatilityConfigVersionKey {
    bond_future_volatility_config: BondFutureVolatilityConfigKey;
    version: number;
}

export interface BondFutureVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondFutureVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondFutureVolatilityConfigsFilter | null;
}

export interface ListBondFutureVolatilityConfigsResponse {
    result: Result;
    bond_future_volatility_configs: BondFutureVolatilityConfig[];
    total: number;
}

export interface GetBondFutureVolatilityConfigRequest {
    key: BondFutureVolatilityConfigKey;
}

export interface GetBondFutureVolatilityConfigResponse {
    result: Result;
    bond_future_volatility_config: BondFutureVolatilityConfig | null;
}

export interface GetManyBondFutureVolatilityConfigsRequest {
    keys: BondFutureVolatilityConfigKey[];
}

export interface GetManyBondFutureVolatilityConfigsResponse {
    result: Result;
    entries: BondFutureVolatilityConfigLookup[];
}

export interface PutBondFutureVolatilityConfigRequest {
    change: BondFutureVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutBondFutureVolatilityConfigResponse {
    result: Result;
    bond_future_volatility_config: BondFutureVolatilityConfig | null;
}

export interface PutManyBondFutureVolatilityConfigsRequest {
    changes: BondFutureVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyBondFutureVolatilityConfigsResponse {
    result: Result;
    bond_future_volatility_configs: BondFutureVolatilityConfig[];
}

export interface DeleteBondFutureVolatilityConfigRequest {
    removal: BondFutureVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondFutureVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyBondFutureVolatilityConfigsRequest {
    removals: BondFutureVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondFutureVolatilityConfigsResponse {
    result: Result;
}

export interface ListBondFutureVolatilityConfigVersionsRequest {
    key: BondFutureVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondFutureVolatilityConfigVersionsFilter | null;
}

export interface ListBondFutureVolatilityConfigVersionsResponse {
    result: Result;
    versions: BondFutureVolatilityConfig[];
    total: number;
}

export interface GetBondFutureVolatilityConfigVersionRequest {
    key: BondFutureVolatilityConfigVersionKey;
}

export interface GetBondFutureVolatilityConfigVersionResponse {
    result: Result;
    version: BondFutureVolatilityConfig | null;
}

export const subjects = {
    list_bond_future_volatility_configs_request: 'refdata.v1.bond_future_volatility_configs.list',
    get_bond_future_volatility_config_request: 'refdata.v1.bond_future_volatility_configs.get',
    get_many_bond_future_volatility_configs_request:
        'refdata.v1.bond_future_volatility_configs.get_many',
    put_bond_future_volatility_config_request: 'refdata.v1.bond_future_volatility_configs.put',
    put_many_bond_future_volatility_configs_request:
        'refdata.v1.bond_future_volatility_configs.put_many',
    delete_bond_future_volatility_config_request:
        'refdata.v1.bond_future_volatility_configs.delete',
    delete_many_bond_future_volatility_configs_request:
        'refdata.v1.bond_future_volatility_configs.delete_many',
    list_bond_future_volatility_config_versions_request:
        'refdata.v1.bond_future_volatility_configs_versions.list',
    get_bond_future_volatility_config_version_request:
        'refdata.v1.bond_future_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_future_volatility_configs_request: true,
    get_bond_future_volatility_config_request: true,
    get_many_bond_future_volatility_configs_request: true,
    put_bond_future_volatility_config_request: true,
    put_many_bond_future_volatility_configs_request: true,
    delete_bond_future_volatility_config_request: true,
    delete_many_bond_future_volatility_configs_request: true,
    list_bond_future_volatility_config_versions_request: true,
    get_bond_future_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bond_future_volatility_configs_events.created',
    updated: 'refdata.v1.bond_future_volatility_configs_events.updated',
    deleted: 'refdata.v1.bond_future_volatility_configs_events.deleted',
} as const;

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
import type { CreditSimulationConfig } from '../domain/credit_simulation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationConfigKey {
    name: string;
}

export interface CreditSimulationConfigWrite {
    id: string;
    name: string;
    configuration_id: string;
    market: string;
    credit: string;
    zero_market_pnl: boolean;
    evaluation: string;
    double_default: boolean;
    seed: number;
    paths: number;
    credit_mode: string;
    loan_exposure_mode: string;
}

export interface CreditSimulationConfigChange {
    write: CreditSimulationConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationConfigRemoval {
    key: CreditSimulationConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationConfigLookup {
    key: CreditSimulationConfigKey;
    credit_simulation_config: CreditSimulationConfig | null;
}

export interface CreditSimulationConfigsFilter {
    configuration_id: string | null;
    id_one_of: string[] | null;
    configuration_id_one_of: string[] | null;
}

export interface CreditSimulationConfigEvent {
    event_id: string;
    key: CreditSimulationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationConfigVersionKey {
    credit_simulation_config: CreditSimulationConfigKey;
    version: number;
}

export interface CreditSimulationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationConfigsFilter | null;
}

export interface ListCreditSimulationConfigsResponse {
    result: Result;
    configs: CreditSimulationConfig[];
    total: number;
}

export interface GetCreditSimulationConfigRequest {
    key: CreditSimulationConfigKey;
}

export interface GetCreditSimulationConfigResponse {
    result: Result;
    credit_simulation_config: CreditSimulationConfig | null;
}

export interface GetManyCreditSimulationConfigsRequest {
    keys: CreditSimulationConfigKey[];
}

export interface GetManyCreditSimulationConfigsResponse {
    result: Result;
    entries: CreditSimulationConfigLookup[];
}

export interface PutCreditSimulationConfigRequest {
    change: CreditSimulationConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationConfigResponse {
    result: Result;
    credit_simulation_config: CreditSimulationConfig | null;
}

export interface PutManyCreditSimulationConfigsRequest {
    changes: CreditSimulationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationConfigsResponse {
    result: Result;
    configs: CreditSimulationConfig[];
}

export interface DeleteCreditSimulationConfigRequest {
    removal: CreditSimulationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationConfigsRequest {
    removals: CreditSimulationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationConfigsResponse {
    result: Result;
}

export interface ListByConfigurationIdCreditSimulationConfigsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationConfigsFilter | null;
}

export interface ListByConfigurationIdCreditSimulationConfigsResponse {
    result: Result;
    configs: CreditSimulationConfig[];
    total: number;
}

export interface ListCreditSimulationConfigVersionsRequest {
    key: CreditSimulationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationConfigVersionsFilter | null;
}

export interface ListCreditSimulationConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationConfig[];
    total: number;
}

export interface GetCreditSimulationConfigVersionRequest {
    key: CreditSimulationConfigVersionKey;
}

export interface GetCreditSimulationConfigVersionResponse {
    result: Result;
    version: CreditSimulationConfig | null;
}

export const subjects = {
    list_credit_simulation_configs_request: 'analytics.v1.credit_simulation_configs.list',
    get_credit_simulation_config_request: 'analytics.v1.credit_simulation_configs.get',
    get_many_credit_simulation_configs_request: 'analytics.v1.credit_simulation_configs.get_many',
    put_credit_simulation_config_request: 'analytics.v1.credit_simulation_configs.put',
    put_many_credit_simulation_configs_request: 'analytics.v1.credit_simulation_configs.put_many',
    delete_credit_simulation_config_request: 'analytics.v1.credit_simulation_configs.delete',
    delete_many_credit_simulation_configs_request:
        'analytics.v1.credit_simulation_configs.delete_many',
    list_by_configuration_id_credit_simulation_configs_request:
        'analytics.v1.credit_simulation_configs.list_by_configuration_id',
    list_credit_simulation_config_versions_request:
        'analytics.v1.credit_simulation_configs_versions.list',
    get_credit_simulation_config_version_request:
        'analytics.v1.credit_simulation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_configs_request: true,
    get_credit_simulation_config_request: true,
    get_many_credit_simulation_configs_request: true,
    put_credit_simulation_config_request: true,
    put_many_credit_simulation_configs_request: true,
    delete_credit_simulation_config_request: true,
    delete_many_credit_simulation_configs_request: true,
    list_by_configuration_id_credit_simulation_configs_request: true,
    list_credit_simulation_config_versions_request: true,
    get_credit_simulation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.credit_simulation_configs_events.created',
    updated: 'analytics.v1.credit_simulation_configs_events.updated',
    deleted: 'analytics.v1.credit_simulation_configs_events.deleted',
} as const;

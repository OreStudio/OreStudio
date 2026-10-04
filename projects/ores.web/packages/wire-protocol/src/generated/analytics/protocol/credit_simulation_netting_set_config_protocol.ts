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
import type { CreditSimulationNettingSetConfig } from '../domain/credit_simulation_netting_set_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationNettingSetConfigKey {
    netting_set_id: string;
}

export interface CreditSimulationNettingSetConfigWrite {
    id: string;
    credit_simulation_config_id: string;
    netting_set_id: string;
    position: number;
}

export interface CreditSimulationNettingSetConfigChange {
    write: CreditSimulationNettingSetConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationNettingSetConfigRemoval {
    key: CreditSimulationNettingSetConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationNettingSetConfigLookup {
    key: CreditSimulationNettingSetConfigKey;
    credit_simulation_netting_set_config: CreditSimulationNettingSetConfig | null;
}

export interface CreditSimulationNettingSetConfigsFilter {
    credit_simulation_config_id: string | null;
    id_one_of: string[] | null;
    credit_simulation_config_id_one_of: string[] | null;
}

export interface CreditSimulationNettingSetConfigEvent {
    event_id: string;
    key: CreditSimulationNettingSetConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationNettingSetConfigVersionKey {
    credit_simulation_netting_set_config: CreditSimulationNettingSetConfigKey;
    version: number;
}

export interface CreditSimulationNettingSetConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationNettingSetConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationNettingSetConfigsFilter | null;
}

export interface ListCreditSimulationNettingSetConfigsResponse {
    result: Result;
    netting_sets: CreditSimulationNettingSetConfig[];
    total: number;
}

export interface GetCreditSimulationNettingSetConfigRequest {
    key: CreditSimulationNettingSetConfigKey;
}

export interface GetCreditSimulationNettingSetConfigResponse {
    result: Result;
    credit_simulation_netting_set_config: CreditSimulationNettingSetConfig | null;
}

export interface GetManyCreditSimulationNettingSetConfigsRequest {
    keys: CreditSimulationNettingSetConfigKey[];
}

export interface GetManyCreditSimulationNettingSetConfigsResponse {
    result: Result;
    entries: CreditSimulationNettingSetConfigLookup[];
}

export interface PutCreditSimulationNettingSetConfigRequest {
    change: CreditSimulationNettingSetConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationNettingSetConfigResponse {
    result: Result;
    credit_simulation_netting_set_config: CreditSimulationNettingSetConfig | null;
}

export interface PutManyCreditSimulationNettingSetConfigsRequest {
    changes: CreditSimulationNettingSetConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationNettingSetConfigsResponse {
    result: Result;
    netting_sets: CreditSimulationNettingSetConfig[];
}

export interface DeleteCreditSimulationNettingSetConfigRequest {
    removal: CreditSimulationNettingSetConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationNettingSetConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationNettingSetConfigsRequest {
    removals: CreditSimulationNettingSetConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationNettingSetConfigsResponse {
    result: Result;
}

export interface ListByCreditSimulationConfigIdCreditSimulationNettingSetConfigsRequest {
    credit_simulation_config_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationNettingSetConfigsFilter | null;
}

export interface ListByCreditSimulationConfigIdCreditSimulationNettingSetConfigsResponse {
    result: Result;
    netting_sets: CreditSimulationNettingSetConfig[];
    total: number;
}

export interface ListCreditSimulationNettingSetConfigVersionsRequest {
    key: CreditSimulationNettingSetConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationNettingSetConfigVersionsFilter | null;
}

export interface ListCreditSimulationNettingSetConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationNettingSetConfig[];
    total: number;
}

export interface GetCreditSimulationNettingSetConfigVersionRequest {
    key: CreditSimulationNettingSetConfigVersionKey;
}

export interface GetCreditSimulationNettingSetConfigVersionResponse {
    result: Result;
    version: CreditSimulationNettingSetConfig | null;
}

export const subjects = {
    list_credit_simulation_netting_set_configs_request:
        'analytics.v1.credit_simulation_netting_set_configs.list',
    get_credit_simulation_netting_set_config_request:
        'analytics.v1.credit_simulation_netting_set_configs.get',
    get_many_credit_simulation_netting_set_configs_request:
        'analytics.v1.credit_simulation_netting_set_configs.get_many',
    put_credit_simulation_netting_set_config_request:
        'analytics.v1.credit_simulation_netting_set_configs.put',
    put_many_credit_simulation_netting_set_configs_request:
        'analytics.v1.credit_simulation_netting_set_configs.put_many',
    delete_credit_simulation_netting_set_config_request:
        'analytics.v1.credit_simulation_netting_set_configs.delete',
    delete_many_credit_simulation_netting_set_configs_request:
        'analytics.v1.credit_simulation_netting_set_configs.delete_many',
    list_by_credit_simulation_config_id_credit_simulation_netting_set_configs_request:
        'analytics.v1.credit_simulation_netting_set_configs.list_by_credit_simulation_config_id',
    list_credit_simulation_netting_set_config_versions_request:
        'analytics.v1.credit_simulation_netting_set_configs_versions.list',
    get_credit_simulation_netting_set_config_version_request:
        'analytics.v1.credit_simulation_netting_set_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_netting_set_configs_request: true,
    get_credit_simulation_netting_set_config_request: true,
    get_many_credit_simulation_netting_set_configs_request: true,
    put_credit_simulation_netting_set_config_request: true,
    put_many_credit_simulation_netting_set_configs_request: true,
    delete_credit_simulation_netting_set_config_request: true,
    delete_many_credit_simulation_netting_set_configs_request: true,
    list_by_credit_simulation_config_id_credit_simulation_netting_set_configs_request: true,
    list_credit_simulation_netting_set_config_versions_request: true,
    get_credit_simulation_netting_set_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.credit_simulation_netting_set_configs_events.created',
    updated: 'analytics.v1.credit_simulation_netting_set_configs_events.updated',
    deleted: 'analytics.v1.credit_simulation_netting_set_configs_events.deleted',
} as const;

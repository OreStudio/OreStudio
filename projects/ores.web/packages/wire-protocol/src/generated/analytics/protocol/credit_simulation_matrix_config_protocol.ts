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
import type { CreditSimulationMatrixConfig } from '../domain/credit_simulation_matrix_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CreditSimulationMatrixConfigKey {
    name: string;
}

export interface CreditSimulationMatrixConfigWrite {
    id: string;
    name: string;
    t0: number;
    t1: number;
}

export interface CreditSimulationMatrixConfigChange {
    write: CreditSimulationMatrixConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationMatrixConfigRemoval {
    key: CreditSimulationMatrixConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationMatrixConfigLookup {
    key: CreditSimulationMatrixConfigKey;
    credit_simulation_matrix_config: CreditSimulationMatrixConfig | null;
}

export interface CreditSimulationMatrixConfigEvent {
    event_id: string;
    key: CreditSimulationMatrixConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationMatrixConfigVersionKey {
    credit_simulation_matrix_config: CreditSimulationMatrixConfigKey;
    version: number;
}

export interface CreditSimulationMatrixConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationMatrixConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCreditSimulationMatrixConfigsResponse {
    result: Result;
    matrices: CreditSimulationMatrixConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixConfigRequest {
    key: CreditSimulationMatrixConfigKey;
}

export interface GetCreditSimulationMatrixConfigResponse {
    result: Result;
    credit_simulation_matrix_config: CreditSimulationMatrixConfig | null;
}

export interface GetManyCreditSimulationMatrixConfigsRequest {
    keys: CreditSimulationMatrixConfigKey[];
}

export interface GetManyCreditSimulationMatrixConfigsResponse {
    result: Result;
    entries: CreditSimulationMatrixConfigLookup[];
}

export interface PutCreditSimulationMatrixConfigRequest {
    change: CreditSimulationMatrixConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationMatrixConfigResponse {
    result: Result;
    credit_simulation_matrix_config: CreditSimulationMatrixConfig;
}

export interface PutManyCreditSimulationMatrixConfigsRequest {
    changes: CreditSimulationMatrixConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationMatrixConfigsResponse {
    result: Result;
    matrices: CreditSimulationMatrixConfig[];
}

export interface DeleteCreditSimulationMatrixConfigRequest {
    removal: CreditSimulationMatrixConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationMatrixConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationMatrixConfigsRequest {
    removals: CreditSimulationMatrixConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationMatrixConfigsResponse {
    result: Result;
}

export interface ListCreditSimulationMatrixConfigVersionsRequest {
    key: CreditSimulationMatrixConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixConfigVersionsFilter | null;
}

export interface ListCreditSimulationMatrixConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationMatrixConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixConfigVersionRequest {
    key: CreditSimulationMatrixConfigVersionKey;
}

export interface GetCreditSimulationMatrixConfigVersionResponse {
    result: Result;
    version: CreditSimulationMatrixConfig;
}

export const subjects = {
    list_credit_simulation_matrix_configs_request:
        'analytics.v1.credit_simulation_matrix_configs.list',
    get_credit_simulation_matrix_config_request:
        'analytics.v1.credit_simulation_matrix_configs.get',
    get_many_credit_simulation_matrix_configs_request:
        'analytics.v1.credit_simulation_matrix_configs.get_many',
    put_credit_simulation_matrix_config_request:
        'analytics.v1.credit_simulation_matrix_configs.put',
    put_many_credit_simulation_matrix_configs_request:
        'analytics.v1.credit_simulation_matrix_configs.put_many',
    delete_credit_simulation_matrix_config_request:
        'analytics.v1.credit_simulation_matrix_configs.delete',
    delete_many_credit_simulation_matrix_configs_request:
        'analytics.v1.credit_simulation_matrix_configs.delete_many',
    list_credit_simulation_matrix_config_versions_request:
        'analytics.v1.credit_simulation_matrix_configs_versions.list',
    get_credit_simulation_matrix_config_version_request:
        'analytics.v1.credit_simulation_matrix_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_matrix_configs_request: true,
    get_credit_simulation_matrix_config_request: true,
    get_many_credit_simulation_matrix_configs_request: true,
    put_credit_simulation_matrix_config_request: true,
    put_many_credit_simulation_matrix_configs_request: true,
    delete_credit_simulation_matrix_config_request: true,
    delete_many_credit_simulation_matrix_configs_request: true,
    list_credit_simulation_matrix_config_versions_request: true,
    get_credit_simulation_matrix_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.credit_simulation_matrix_configs_events.created',
    updated: 'analytics.v1.credit_simulation_matrix_configs_events.updated',
    deleted: 'analytics.v1.credit_simulation_matrix_configs_events.deleted',
} as const;

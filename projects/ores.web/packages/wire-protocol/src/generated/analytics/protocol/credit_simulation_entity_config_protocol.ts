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
import type { CreditSimulationEntityConfig } from '../domain/credit_simulation_entity_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationEntityConfigKey {
    name: string;
}

export interface CreditSimulationEntityConfigWrite {
    id: string;
    credit_simulation_config_id: string;
    name: string;
    transition_matrix_id: string;
    initial_state: number;
    factor_loadings: string;
}

export interface CreditSimulationEntityConfigChange {
    write: CreditSimulationEntityConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationEntityConfigRemoval {
    key: CreditSimulationEntityConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationEntityConfigLookup {
    key: CreditSimulationEntityConfigKey;
    credit_simulation_entity_config: CreditSimulationEntityConfig | null;
}

export interface CreditSimulationEntityConfigsFilter {
    credit_simulation_config_id: string | null;
    transition_matrix_id: string | null;
    id_one_of: string[] | null;
    credit_simulation_config_id_one_of: string[] | null;
    transition_matrix_id_one_of: string[] | null;
}

export interface CreditSimulationEntityConfigEvent {
    event_id: string;
    key: CreditSimulationEntityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationEntityConfigVersionKey {
    credit_simulation_entity_config: CreditSimulationEntityConfigKey;
    version: number;
}

export interface CreditSimulationEntityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationEntityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationEntityConfigsFilter | null;
}

export interface ListCreditSimulationEntityConfigsResponse {
    result: Result;
    entities: CreditSimulationEntityConfig[];
    total: number;
}

export interface GetCreditSimulationEntityConfigRequest {
    key: CreditSimulationEntityConfigKey;
}

export interface GetCreditSimulationEntityConfigResponse {
    result: Result;
    credit_simulation_entity_config: CreditSimulationEntityConfig | null;
}

export interface GetManyCreditSimulationEntityConfigsRequest {
    keys: CreditSimulationEntityConfigKey[];
}

export interface GetManyCreditSimulationEntityConfigsResponse {
    result: Result;
    entries: CreditSimulationEntityConfigLookup[];
}

export interface PutCreditSimulationEntityConfigRequest {
    change: CreditSimulationEntityConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationEntityConfigResponse {
    result: Result;
    credit_simulation_entity_config: CreditSimulationEntityConfig | null;
}

export interface PutManyCreditSimulationEntityConfigsRequest {
    changes: CreditSimulationEntityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationEntityConfigsResponse {
    result: Result;
    entities: CreditSimulationEntityConfig[];
}

export interface DeleteCreditSimulationEntityConfigRequest {
    removal: CreditSimulationEntityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationEntityConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationEntityConfigsRequest {
    removals: CreditSimulationEntityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationEntityConfigsResponse {
    result: Result;
}

export interface ListByCreditSimulationConfigIdCreditSimulationEntityConfigsRequest {
    credit_simulation_config_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationEntityConfigsFilter | null;
}

export interface ListByCreditSimulationConfigIdCreditSimulationEntityConfigsResponse {
    result: Result;
    entities: CreditSimulationEntityConfig[];
    total: number;
}

export interface ListByTransitionMatrixIdCreditSimulationEntityConfigsRequest {
    transition_matrix_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationEntityConfigsFilter | null;
}

export interface ListByTransitionMatrixIdCreditSimulationEntityConfigsResponse {
    result: Result;
    entities: CreditSimulationEntityConfig[];
    total: number;
}

export interface ListCreditSimulationEntityConfigVersionsRequest {
    key: CreditSimulationEntityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationEntityConfigVersionsFilter | null;
}

export interface ListCreditSimulationEntityConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationEntityConfig[];
    total: number;
}

export interface GetCreditSimulationEntityConfigVersionRequest {
    key: CreditSimulationEntityConfigVersionKey;
}

export interface GetCreditSimulationEntityConfigVersionResponse {
    result: Result;
    version: CreditSimulationEntityConfig | null;
}

export const subjects = {
    list_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.list',
    get_credit_simulation_entity_config_request:
        'analytics.v1.credit_simulation_entity_configs.get',
    get_many_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.get_many',
    put_credit_simulation_entity_config_request:
        'analytics.v1.credit_simulation_entity_configs.put',
    put_many_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.put_many',
    delete_credit_simulation_entity_config_request:
        'analytics.v1.credit_simulation_entity_configs.delete',
    delete_many_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.delete_many',
    list_by_credit_simulation_config_id_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.list_by_credit_simulation_config_id',
    list_by_transition_matrix_id_credit_simulation_entity_configs_request:
        'analytics.v1.credit_simulation_entity_configs.list_by_transition_matrix_id',
    list_credit_simulation_entity_config_versions_request:
        'analytics.v1.credit_simulation_entity_configs_versions.list',
    get_credit_simulation_entity_config_version_request:
        'analytics.v1.credit_simulation_entity_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_entity_configs_request: true,
    get_credit_simulation_entity_config_request: true,
    get_many_credit_simulation_entity_configs_request: true,
    put_credit_simulation_entity_config_request: true,
    put_many_credit_simulation_entity_configs_request: true,
    delete_credit_simulation_entity_config_request: true,
    delete_many_credit_simulation_entity_configs_request: true,
    list_by_credit_simulation_config_id_credit_simulation_entity_configs_request: true,
    list_by_transition_matrix_id_credit_simulation_entity_configs_request: true,
    list_credit_simulation_entity_config_versions_request: true,
    get_credit_simulation_entity_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.credit_simulation_entity_configs_events.created',
    updated: 'analytics.v1.credit_simulation_entity_configs_events.updated',
    deleted: 'analytics.v1.credit_simulation_entity_configs_events.deleted',
} as const;

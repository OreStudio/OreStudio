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
import type { CreditSimulationMatrixStateConfig } from '../domain/credit_simulation_matrix_state_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationMatrixStateConfigKey {
    credit_rating_code: string;
}

export interface CreditSimulationMatrixStateConfigWrite {
    id: string;
    transition_matrix_id: string;
    position: number;
    credit_rating_code: string;
}

export interface CreditSimulationMatrixStateConfigChange {
    write: CreditSimulationMatrixStateConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationMatrixStateConfigRemoval {
    key: CreditSimulationMatrixStateConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationMatrixStateConfigLookup {
    key: CreditSimulationMatrixStateConfigKey;
    credit_simulation_matrix_state_config: CreditSimulationMatrixStateConfig | null;
}

export interface CreditSimulationMatrixStateConfigsFilter {
    transition_matrix_id: string | null;
    credit_rating_code: string | null;
}

export interface CreditSimulationMatrixStateConfigEvent {
    event_id: string;
    key: CreditSimulationMatrixStateConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationMatrixStateConfigVersionKey {
    credit_simulation_matrix_state_config: CreditSimulationMatrixStateConfigKey;
    version: number;
}

export interface CreditSimulationMatrixStateConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationMatrixStateConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixStateConfigsFilter | null;
}

export interface ListCreditSimulationMatrixStateConfigsResponse {
    result: Result;
    states: CreditSimulationMatrixStateConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixStateConfigRequest {
    key: CreditSimulationMatrixStateConfigKey;
}

export interface GetCreditSimulationMatrixStateConfigResponse {
    result: Result;
    credit_simulation_matrix_state_config: CreditSimulationMatrixStateConfig | null;
}

export interface GetManyCreditSimulationMatrixStateConfigsRequest {
    keys: CreditSimulationMatrixStateConfigKey[];
}

export interface GetManyCreditSimulationMatrixStateConfigsResponse {
    result: Result;
    entries: CreditSimulationMatrixStateConfigLookup[];
}

export interface PutCreditSimulationMatrixStateConfigRequest {
    change: CreditSimulationMatrixStateConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationMatrixStateConfigResponse {
    result: Result;
    credit_simulation_matrix_state_config: CreditSimulationMatrixStateConfig;
}

export interface PutManyCreditSimulationMatrixStateConfigsRequest {
    changes: CreditSimulationMatrixStateConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationMatrixStateConfigsResponse {
    result: Result;
    states: CreditSimulationMatrixStateConfig[];
}

export interface DeleteCreditSimulationMatrixStateConfigRequest {
    removal: CreditSimulationMatrixStateConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationMatrixStateConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationMatrixStateConfigsRequest {
    removals: CreditSimulationMatrixStateConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationMatrixStateConfigsResponse {
    result: Result;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixStateConfigsRequest {
    transition_matrix_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixStateConfigsFilter | null;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixStateConfigsResponse {
    result: Result;
    states: CreditSimulationMatrixStateConfig[];
    total: number;
}

export interface ListByCreditRatingCodeCreditSimulationMatrixStateConfigsRequest {
    credit_rating_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixStateConfigsFilter | null;
}

export interface ListByCreditRatingCodeCreditSimulationMatrixStateConfigsResponse {
    result: Result;
    states: CreditSimulationMatrixStateConfig[];
    total: number;
}

export interface ListCreditSimulationMatrixStateConfigVersionsRequest {
    key: CreditSimulationMatrixStateConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixStateConfigVersionsFilter | null;
}

export interface ListCreditSimulationMatrixStateConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationMatrixStateConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixStateConfigVersionRequest {
    key: CreditSimulationMatrixStateConfigVersionKey;
}

export interface GetCreditSimulationMatrixStateConfigVersionResponse {
    result: Result;
    version: CreditSimulationMatrixStateConfig;
}

export const subjects = {
    list_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.list",
    get_credit_simulation_matrix_state_config_request: "analytics.v1.credit_simulation_matrix_state_configs.get",
    get_many_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.get_many",
    put_credit_simulation_matrix_state_config_request: "analytics.v1.credit_simulation_matrix_state_configs.put",
    put_many_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.put_many",
    delete_credit_simulation_matrix_state_config_request: "analytics.v1.credit_simulation_matrix_state_configs.delete",
    delete_many_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.delete_many",
    list_by_transition_matrix_id_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.list_by_transition_matrix_id",
    list_by_credit_rating_code_credit_simulation_matrix_state_configs_request: "analytics.v1.credit_simulation_matrix_state_configs.list_by_credit_rating_code",
    list_credit_simulation_matrix_state_config_versions_request: "analytics.v1.credit_simulation_matrix_state_configs_versions.list",
    get_credit_simulation_matrix_state_config_version_request: "analytics.v1.credit_simulation_matrix_state_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_matrix_state_configs_request: true,
    get_credit_simulation_matrix_state_config_request: true,
    get_many_credit_simulation_matrix_state_configs_request: true,
    put_credit_simulation_matrix_state_config_request: true,
    put_many_credit_simulation_matrix_state_configs_request: true,
    delete_credit_simulation_matrix_state_config_request: true,
    delete_many_credit_simulation_matrix_state_configs_request: true,
    list_by_transition_matrix_id_credit_simulation_matrix_state_configs_request: true,
    list_by_credit_rating_code_credit_simulation_matrix_state_configs_request: true,
    list_credit_simulation_matrix_state_config_versions_request: true,
    get_credit_simulation_matrix_state_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.credit_simulation_matrix_state_configs_events.created",
    updated: "analytics.v1.credit_simulation_matrix_state_configs_events.updated",
    deleted: "analytics.v1.credit_simulation_matrix_state_configs_events.deleted",
} as const;

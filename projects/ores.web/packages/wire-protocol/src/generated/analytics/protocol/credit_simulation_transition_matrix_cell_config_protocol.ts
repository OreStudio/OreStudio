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
import type { CreditSimulationTransitionMatrixCellConfig } from '../domain/credit_simulation_transition_matrix_cell_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationTransitionMatrixCellConfigKey {
    id: string;
}

export interface CreditSimulationTransitionMatrixCellConfigWrite {
    id: string;
    probability: number;
}

export interface CreditSimulationTransitionMatrixCellConfigChange {
    write: CreditSimulationTransitionMatrixCellConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationTransitionMatrixCellConfigRemoval {
    key: CreditSimulationTransitionMatrixCellConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationTransitionMatrixCellConfigLookup {
    key: CreditSimulationTransitionMatrixCellConfigKey;
    credit_simulation_transition_matrix_cell_config: CreditSimulationTransitionMatrixCellConfig | null;
}

export interface CreditSimulationTransitionMatrixCellConfigsFilter {
    transition_matrix_id: string | null;
}

export interface CreditSimulationTransitionMatrixCellConfigEvent {
    event_id: string;
    key: CreditSimulationTransitionMatrixCellConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationTransitionMatrixCellConfigVersionKey {
    credit_simulation_transition_matrix_cell_config: CreditSimulationTransitionMatrixCellConfigKey;
    version: number;
}

export interface CreditSimulationTransitionMatrixCellConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationTransitionMatrixCellConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationTransitionMatrixCellConfigsFilter | null;
}

export interface ListCreditSimulationTransitionMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationTransitionMatrixCellConfig[];
    total: number;
}

export interface GetCreditSimulationTransitionMatrixCellConfigRequest {
    key: CreditSimulationTransitionMatrixCellConfigKey;
}

export interface GetCreditSimulationTransitionMatrixCellConfigResponse {
    result: Result;
    credit_simulation_transition_matrix_cell_config: CreditSimulationTransitionMatrixCellConfig | null;
}

export interface GetManyCreditSimulationTransitionMatrixCellConfigsRequest {
    keys: CreditSimulationTransitionMatrixCellConfigKey[];
}

export interface GetManyCreditSimulationTransitionMatrixCellConfigsResponse {
    result: Result;
    entries: CreditSimulationTransitionMatrixCellConfigLookup[];
}

export interface PutCreditSimulationTransitionMatrixCellConfigRequest {
    change: CreditSimulationTransitionMatrixCellConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationTransitionMatrixCellConfigResponse {
    result: Result;
    credit_simulation_transition_matrix_cell_config: CreditSimulationTransitionMatrixCellConfig;
}

export interface PutManyCreditSimulationTransitionMatrixCellConfigsRequest {
    changes: CreditSimulationTransitionMatrixCellConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationTransitionMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationTransitionMatrixCellConfig[];
}

export interface DeleteCreditSimulationTransitionMatrixCellConfigRequest {
    removal: CreditSimulationTransitionMatrixCellConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationTransitionMatrixCellConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationTransitionMatrixCellConfigsRequest {
    removals: CreditSimulationTransitionMatrixCellConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationTransitionMatrixCellConfigsResponse {
    result: Result;
}

export interface ListByTransitionMatrixIdCreditSimulationTransitionMatrixCellConfigsRequest {
    transition_matrix_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationTransitionMatrixCellConfigsFilter | null;
}

export interface ListByTransitionMatrixIdCreditSimulationTransitionMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationTransitionMatrixCellConfig[];
    total: number;
}

export interface ListCreditSimulationTransitionMatrixCellConfigVersionsRequest {
    key: CreditSimulationTransitionMatrixCellConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationTransitionMatrixCellConfigVersionsFilter | null;
}

export interface ListCreditSimulationTransitionMatrixCellConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationTransitionMatrixCellConfig[];
    total: number;
}

export interface GetCreditSimulationTransitionMatrixCellConfigVersionRequest {
    key: CreditSimulationTransitionMatrixCellConfigVersionKey;
}

export interface GetCreditSimulationTransitionMatrixCellConfigVersionResponse {
    result: Result;
    version: CreditSimulationTransitionMatrixCellConfig;
}

export const subjects = {
    list_credit_simulation_transition_matrix_cell_configs_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.list",
    get_credit_simulation_transition_matrix_cell_config_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.get",
    get_many_credit_simulation_transition_matrix_cell_configs_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.get_many",
    put_credit_simulation_transition_matrix_cell_config_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.put",
    put_many_credit_simulation_transition_matrix_cell_configs_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.put_many",
    delete_credit_simulation_transition_matrix_cell_config_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.delete",
    delete_many_credit_simulation_transition_matrix_cell_configs_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.delete_many",
    list_by_transition_matrix_id_credit_simulation_transition_matrix_cell_configs_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs.list_by_transition_matrix_id",
    list_credit_simulation_transition_matrix_cell_config_versions_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs_versions.list",
    get_credit_simulation_transition_matrix_cell_config_version_request: "analytics.v1.credit_simulation_transition_matrix_cell_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_transition_matrix_cell_configs_request: true,
    get_credit_simulation_transition_matrix_cell_config_request: true,
    get_many_credit_simulation_transition_matrix_cell_configs_request: true,
    put_credit_simulation_transition_matrix_cell_config_request: true,
    put_many_credit_simulation_transition_matrix_cell_configs_request: true,
    delete_credit_simulation_transition_matrix_cell_config_request: true,
    delete_many_credit_simulation_transition_matrix_cell_configs_request: true,
    list_by_transition_matrix_id_credit_simulation_transition_matrix_cell_configs_request: true,
    list_credit_simulation_transition_matrix_cell_config_versions_request: true,
    get_credit_simulation_transition_matrix_cell_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.credit_simulation_transition_matrix_cell_configs_events.created",
    updated: "analytics.v1.credit_simulation_transition_matrix_cell_configs_events.updated",
    deleted: "analytics.v1.credit_simulation_transition_matrix_cell_configs_events.deleted",
} as const;

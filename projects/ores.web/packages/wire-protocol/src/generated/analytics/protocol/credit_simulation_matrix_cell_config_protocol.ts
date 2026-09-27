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
import type { CreditSimulationMatrixCellConfig } from '../domain/credit_simulation_matrix_cell_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationMatrixCellConfigKey {
    from_state: number;
}

export interface CreditSimulationMatrixCellConfigWrite {
    id: string;
    transition_matrix_id: string;
    from_state: number;
    to_state: number;
    probability: number;
}

export interface CreditSimulationMatrixCellConfigChange {
    write: CreditSimulationMatrixCellConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationMatrixCellConfigRemoval {
    key: CreditSimulationMatrixCellConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationMatrixCellConfigLookup {
    key: CreditSimulationMatrixCellConfigKey;
    credit_simulation_matrix_cell_config: CreditSimulationMatrixCellConfig | null;
}

export interface CreditSimulationMatrixCellConfigsFilter {
    transition_matrix_id: string | null;
}

export interface CreditSimulationMatrixCellConfigEvent {
    event_id: string;
    key: CreditSimulationMatrixCellConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationMatrixCellConfigVersionKey {
    credit_simulation_matrix_cell_config: CreditSimulationMatrixCellConfigKey;
    version: number;
}

export interface CreditSimulationMatrixCellConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationMatrixCellConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixCellConfigsFilter | null;
}

export interface ListCreditSimulationMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationMatrixCellConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixCellConfigRequest {
    key: CreditSimulationMatrixCellConfigKey;
}

export interface GetCreditSimulationMatrixCellConfigResponse {
    result: Result;
    credit_simulation_matrix_cell_config: CreditSimulationMatrixCellConfig | null;
}

export interface GetManyCreditSimulationMatrixCellConfigsRequest {
    keys: CreditSimulationMatrixCellConfigKey[];
}

export interface GetManyCreditSimulationMatrixCellConfigsResponse {
    result: Result;
    entries: CreditSimulationMatrixCellConfigLookup[];
}

export interface PutCreditSimulationMatrixCellConfigRequest {
    change: CreditSimulationMatrixCellConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationMatrixCellConfigResponse {
    result: Result;
    credit_simulation_matrix_cell_config: CreditSimulationMatrixCellConfig;
}

export interface PutManyCreditSimulationMatrixCellConfigsRequest {
    changes: CreditSimulationMatrixCellConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationMatrixCellConfig[];
}

export interface DeleteCreditSimulationMatrixCellConfigRequest {
    removal: CreditSimulationMatrixCellConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationMatrixCellConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationMatrixCellConfigsRequest {
    removals: CreditSimulationMatrixCellConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationMatrixCellConfigsResponse {
    result: Result;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixCellConfigsRequest {
    transition_matrix_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixCellConfigsFilter | null;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixCellConfigsResponse {
    result: Result;
    cells: CreditSimulationMatrixCellConfig[];
    total: number;
}

export interface ListCreditSimulationMatrixCellConfigVersionsRequest {
    key: CreditSimulationMatrixCellConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixCellConfigVersionsFilter | null;
}

export interface ListCreditSimulationMatrixCellConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationMatrixCellConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixCellConfigVersionRequest {
    key: CreditSimulationMatrixCellConfigVersionKey;
}

export interface GetCreditSimulationMatrixCellConfigVersionResponse {
    result: Result;
    version: CreditSimulationMatrixCellConfig;
}

export const subjects = {
    list_credit_simulation_matrix_cell_configs_request: "analytics.v1.credit_simulation_matrix_cell_configs.list",
    get_credit_simulation_matrix_cell_config_request: "analytics.v1.credit_simulation_matrix_cell_configs.get",
    get_many_credit_simulation_matrix_cell_configs_request: "analytics.v1.credit_simulation_matrix_cell_configs.get_many",
    put_credit_simulation_matrix_cell_config_request: "analytics.v1.credit_simulation_matrix_cell_configs.put",
    put_many_credit_simulation_matrix_cell_configs_request: "analytics.v1.credit_simulation_matrix_cell_configs.put_many",
    delete_credit_simulation_matrix_cell_config_request: "analytics.v1.credit_simulation_matrix_cell_configs.delete",
    delete_many_credit_simulation_matrix_cell_configs_request: "analytics.v1.credit_simulation_matrix_cell_configs.delete_many",
    list_by_transition_matrix_id_credit_simulation_matrix_cell_configs_request: "analytics.v1.credit_simulation_matrix_cell_configs.list_by_transition_matrix_id",
    list_credit_simulation_matrix_cell_config_versions_request: "analytics.v1.credit_simulation_matrix_cell_configs_versions.list",
    get_credit_simulation_matrix_cell_config_version_request: "analytics.v1.credit_simulation_matrix_cell_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_matrix_cell_configs_request: true,
    get_credit_simulation_matrix_cell_config_request: true,
    get_many_credit_simulation_matrix_cell_configs_request: true,
    put_credit_simulation_matrix_cell_config_request: true,
    put_many_credit_simulation_matrix_cell_configs_request: true,
    delete_credit_simulation_matrix_cell_config_request: true,
    delete_many_credit_simulation_matrix_cell_configs_request: true,
    list_by_transition_matrix_id_credit_simulation_matrix_cell_configs_request: true,
    list_credit_simulation_matrix_cell_config_versions_request: true,
    get_credit_simulation_matrix_cell_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.credit_simulation_matrix_cell_configs_events.created",
    updated: "analytics.v1.credit_simulation_matrix_cell_configs_events.updated",
    deleted: "analytics.v1.credit_simulation_matrix_cell_configs_events.deleted",
} as const;

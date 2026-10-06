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
import type { CreditSimulationMatrixRowConfig } from '../domain/credit_simulation_matrix_row_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditSimulationMatrixRowConfigKey {
    from_rating: string;
}

export interface CreditSimulationMatrixRowConfigWrite {
    id: string;
    transition_matrix_id: string;
    from_rating: string;
    p_aaa: number;
    p_aa: number;
    p_a: number;
    p_baa: number;
    p_ba: number;
    p_b: number;
    p_c: number;
    p_default: number;
}

export interface CreditSimulationMatrixRowConfigChange {
    write: CreditSimulationMatrixRowConfigWrite;
    precondition: Precondition;
}

export interface CreditSimulationMatrixRowConfigRemoval {
    key: CreditSimulationMatrixRowConfigKey;
    precondition: Precondition;
}

export interface CreditSimulationMatrixRowConfigLookup {
    key: CreditSimulationMatrixRowConfigKey;
    credit_simulation_matrix_row_config: CreditSimulationMatrixRowConfig | null;
}

export interface CreditSimulationMatrixRowConfigsFilter {
    transition_matrix_id: string | null;
    id_one_of: string[] | null;
    transition_matrix_id_one_of: string[] | null;
}

export interface CreditSimulationMatrixRowConfigEvent {
    event_id: string;
    key: CreditSimulationMatrixRowConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditSimulationMatrixRowConfigVersionKey {
    credit_simulation_matrix_row_config: CreditSimulationMatrixRowConfigKey;
    version: number;
}

export interface CreditSimulationMatrixRowConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditSimulationMatrixRowConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixRowConfigsFilter | null;
    as_of: string | null;
}

export interface ListCreditSimulationMatrixRowConfigsResponse {
    result: Result;
    rows: CreditSimulationMatrixRowConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixRowConfigRequest {
    key: CreditSimulationMatrixRowConfigKey;
}

export interface GetCreditSimulationMatrixRowConfigResponse {
    result: Result;
    credit_simulation_matrix_row_config: CreditSimulationMatrixRowConfig | null;
}

export interface GetManyCreditSimulationMatrixRowConfigsRequest {
    keys: CreditSimulationMatrixRowConfigKey[];
}

export interface GetManyCreditSimulationMatrixRowConfigsResponse {
    result: Result;
    entries: CreditSimulationMatrixRowConfigLookup[];
}

export interface PutCreditSimulationMatrixRowConfigRequest {
    change: CreditSimulationMatrixRowConfigChange;
    intent: ChangeIntent;
}

export interface PutCreditSimulationMatrixRowConfigResponse {
    result: Result;
    credit_simulation_matrix_row_config: CreditSimulationMatrixRowConfig | null;
}

export interface PutManyCreditSimulationMatrixRowConfigsRequest {
    changes: CreditSimulationMatrixRowConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditSimulationMatrixRowConfigsResponse {
    result: Result;
    rows: CreditSimulationMatrixRowConfig[];
}

export interface DeleteCreditSimulationMatrixRowConfigRequest {
    removal: CreditSimulationMatrixRowConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditSimulationMatrixRowConfigResponse {
    result: Result;
}

export interface DeleteManyCreditSimulationMatrixRowConfigsRequest {
    removals: CreditSimulationMatrixRowConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditSimulationMatrixRowConfigsResponse {
    result: Result;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixRowConfigsRequest {
    transition_matrix_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixRowConfigsFilter | null;
}

export interface ListByTransitionMatrixIdCreditSimulationMatrixRowConfigsResponse {
    result: Result;
    rows: CreditSimulationMatrixRowConfig[];
    total: number;
}

export interface ListCreditSimulationMatrixRowConfigVersionsRequest {
    key: CreditSimulationMatrixRowConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditSimulationMatrixRowConfigVersionsFilter | null;
}

export interface ListCreditSimulationMatrixRowConfigVersionsResponse {
    result: Result;
    versions: CreditSimulationMatrixRowConfig[];
    total: number;
}

export interface GetCreditSimulationMatrixRowConfigVersionRequest {
    key: CreditSimulationMatrixRowConfigVersionKey;
}

export interface GetCreditSimulationMatrixRowConfigVersionResponse {
    result: Result;
    version: CreditSimulationMatrixRowConfig | null;
}

export const subjects = {
    list_credit_simulation_matrix_row_configs_request:
        'analytics.v1.credit_simulation_matrix_row_configs.list',
    get_credit_simulation_matrix_row_config_request:
        'analytics.v1.credit_simulation_matrix_row_configs.get',
    get_many_credit_simulation_matrix_row_configs_request:
        'analytics.v1.credit_simulation_matrix_row_configs.get_many',
    put_credit_simulation_matrix_row_config_request:
        'analytics.v1.credit_simulation_matrix_row_configs.put',
    put_many_credit_simulation_matrix_row_configs_request:
        'analytics.v1.credit_simulation_matrix_row_configs.put_many',
    delete_credit_simulation_matrix_row_config_request:
        'analytics.v1.credit_simulation_matrix_row_configs.delete',
    delete_many_credit_simulation_matrix_row_configs_request:
        'analytics.v1.credit_simulation_matrix_row_configs.delete_many',
    list_by_transition_matrix_id_credit_simulation_matrix_row_configs_request:
        'analytics.v1.credit_simulation_matrix_row_configs.list_by_transition_matrix_id',
    list_credit_simulation_matrix_row_config_versions_request:
        'analytics.v1.credit_simulation_matrix_row_configs_versions.list',
    get_credit_simulation_matrix_row_config_version_request:
        'analytics.v1.credit_simulation_matrix_row_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_simulation_matrix_row_configs_request: true,
    get_credit_simulation_matrix_row_config_request: true,
    get_many_credit_simulation_matrix_row_configs_request: true,
    put_credit_simulation_matrix_row_config_request: true,
    put_many_credit_simulation_matrix_row_configs_request: true,
    delete_credit_simulation_matrix_row_config_request: true,
    delete_many_credit_simulation_matrix_row_configs_request: true,
    list_by_transition_matrix_id_credit_simulation_matrix_row_configs_request: true,
    list_credit_simulation_matrix_row_config_versions_request: true,
    get_credit_simulation_matrix_row_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.credit_simulation_matrix_row_configs_events.created',
    updated: 'analytics.v1.credit_simulation_matrix_row_configs_events.updated',
    deleted: 'analytics.v1.credit_simulation_matrix_row_configs_events.deleted',
} as const;

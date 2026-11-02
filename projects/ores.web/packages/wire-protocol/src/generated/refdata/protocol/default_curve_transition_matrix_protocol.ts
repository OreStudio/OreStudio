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
import type { DefaultCurveTransitionMatrix } from '../domain/default_curve_transition_matrix.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveTransitionMatrixKey {
    id: string;
}

export interface DefaultCurveTransitionMatrixWrite {
    id: string;
    default_curve_configuration_id: string;
    initial_state: string | null;
    states: string | null;
}

export interface DefaultCurveTransitionMatrixChange {
    write: DefaultCurveTransitionMatrixWrite;
    precondition: Precondition;
}

export interface DefaultCurveTransitionMatrixRemoval {
    key: DefaultCurveTransitionMatrixKey;
    precondition: Precondition;
}

export interface DefaultCurveTransitionMatrixLookup {
    key: DefaultCurveTransitionMatrixKey;
    default_curve_transition_matrix: DefaultCurveTransitionMatrix | null;
}

export interface DefaultCurveTransitionMatricesFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveTransitionMatrixEvent {
    event_id: string;
    key: DefaultCurveTransitionMatrixKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveTransitionMatrixVersionKey {
    default_curve_transition_matrix: DefaultCurveTransitionMatrixKey;
    version: number;
}

export interface DefaultCurveTransitionMatrixVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveTransitionMatricesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveTransitionMatricesFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveTransitionMatricesResponse {
    result: Result;
    transition_matrices: DefaultCurveTransitionMatrix[];
    total: number;
}

export interface GetDefaultCurveTransitionMatrixRequest {
    key: DefaultCurveTransitionMatrixKey;
}

export interface GetDefaultCurveTransitionMatrixResponse {
    result: Result;
    default_curve_transition_matrix: DefaultCurveTransitionMatrix | null;
}

export interface GetManyDefaultCurveTransitionMatricesRequest {
    keys: DefaultCurveTransitionMatrixKey[];
}

export interface GetManyDefaultCurveTransitionMatricesResponse {
    result: Result;
    entries: DefaultCurveTransitionMatrixLookup[];
}

export interface PutDefaultCurveTransitionMatrixRequest {
    change: DefaultCurveTransitionMatrixChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveTransitionMatrixResponse {
    result: Result;
    default_curve_transition_matrix: DefaultCurveTransitionMatrix | null;
}

export interface PutManyDefaultCurveTransitionMatricesRequest {
    changes: DefaultCurveTransitionMatrixChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveTransitionMatricesResponse {
    result: Result;
    transition_matrices: DefaultCurveTransitionMatrix[];
}

export interface DeleteDefaultCurveTransitionMatrixRequest {
    removal: DefaultCurveTransitionMatrixRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveTransitionMatrixResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveTransitionMatricesRequest {
    removals: DefaultCurveTransitionMatrixRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveTransitionMatricesResponse {
    result: Result;
}

export interface ListDefaultCurveTransitionMatrixVersionsRequest {
    key: DefaultCurveTransitionMatrixKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveTransitionMatrixVersionsFilter | null;
}

export interface ListDefaultCurveTransitionMatrixVersionsResponse {
    result: Result;
    versions: DefaultCurveTransitionMatrix[];
    total: number;
}

export interface GetDefaultCurveTransitionMatrixVersionRequest {
    key: DefaultCurveTransitionMatrixVersionKey;
}

export interface GetDefaultCurveTransitionMatrixVersionResponse {
    result: Result;
    version: DefaultCurveTransitionMatrix | null;
}

export const subjects = {
    list_default_curve_transition_matrices_request:
        'refdata.v1.default_curve_transition_matrices.list',
    get_default_curve_transition_matrix_request: 'refdata.v1.default_curve_transition_matrices.get',
    get_many_default_curve_transition_matrices_request:
        'refdata.v1.default_curve_transition_matrices.get_many',
    put_default_curve_transition_matrix_request: 'refdata.v1.default_curve_transition_matrices.put',
    put_many_default_curve_transition_matrices_request:
        'refdata.v1.default_curve_transition_matrices.put_many',
    delete_default_curve_transition_matrix_request:
        'refdata.v1.default_curve_transition_matrices.delete',
    delete_many_default_curve_transition_matrices_request:
        'refdata.v1.default_curve_transition_matrices.delete_many',
    list_default_curve_transition_matrix_versions_request:
        'refdata.v1.default_curve_transition_matrices_versions.list',
    get_default_curve_transition_matrix_version_request:
        'refdata.v1.default_curve_transition_matrices_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_transition_matrices_request: true,
    get_default_curve_transition_matrix_request: true,
    get_many_default_curve_transition_matrices_request: true,
    put_default_curve_transition_matrix_request: true,
    put_many_default_curve_transition_matrices_request: true,
    delete_default_curve_transition_matrix_request: true,
    delete_many_default_curve_transition_matrices_request: true,
    list_default_curve_transition_matrix_versions_request: true,
    get_default_curve_transition_matrix_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curve_transition_matrices_events.created',
    updated: 'refdata.v1.default_curve_transition_matrices_events.updated',
    deleted: 'refdata.v1.default_curve_transition_matrices_events.deleted',
} as const;

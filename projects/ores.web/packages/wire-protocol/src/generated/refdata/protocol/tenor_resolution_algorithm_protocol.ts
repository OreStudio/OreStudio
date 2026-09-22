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
import type { TenorResolutionAlgorithm } from '../domain/tenor_resolution_algorithm.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorResolutionAlgorithmKey {
    code: string;
}

export interface TenorResolutionAlgorithmWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface TenorResolutionAlgorithmChange {
    write: TenorResolutionAlgorithmWrite;
    precondition: Precondition;
}

export interface TenorResolutionAlgorithmRemoval {
    key: TenorResolutionAlgorithmKey;
    precondition: Precondition;
}

export interface TenorResolutionAlgorithmLookup {
    key: TenorResolutionAlgorithmKey;
    tenor_resolution_algorithm: TenorResolutionAlgorithm | null;
}

export interface TenorResolutionAlgorithmEvent {
    event_id: string;
    key: TenorResolutionAlgorithmKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorResolutionAlgorithmVersionKey {
    tenor_resolution_algorithm: TenorResolutionAlgorithmKey;
    version: number;
}

export interface TenorResolutionAlgorithmVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorResolutionAlgorithmsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorResolutionAlgorithmsResponse {
    result: Result;
    algorithms: TenorResolutionAlgorithm[];
    total: number;
}

export interface GetTenorResolutionAlgorithmRequest {
    key: TenorResolutionAlgorithmKey;
}

export interface GetTenorResolutionAlgorithmResponse {
    result: Result;
    tenor_resolution_algorithm: TenorResolutionAlgorithm | null;
}

export interface GetManyTenorResolutionAlgorithmsRequest {
    keys: TenorResolutionAlgorithmKey[];
}

export interface GetManyTenorResolutionAlgorithmsResponse {
    result: Result;
    entries: TenorResolutionAlgorithmLookup[];
}

export interface PutTenorResolutionAlgorithmRequest {
    change: TenorResolutionAlgorithmChange;
    intent: ChangeIntent;
}

export interface PutTenorResolutionAlgorithmResponse {
    result: Result;
    tenor_resolution_algorithm: TenorResolutionAlgorithm;
}

export interface PutManyTenorResolutionAlgorithmsRequest {
    changes: TenorResolutionAlgorithmChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorResolutionAlgorithmsResponse {
    result: Result;
    algorithms: TenorResolutionAlgorithm[];
}

export interface DeleteTenorResolutionAlgorithmRequest {
    removal: TenorResolutionAlgorithmRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorResolutionAlgorithmResponse {
    result: Result;
}

export interface DeleteManyTenorResolutionAlgorithmsRequest {
    removals: TenorResolutionAlgorithmRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorResolutionAlgorithmsResponse {
    result: Result;
}

export interface ListTenorResolutionAlgorithmVersionsRequest {
    key: TenorResolutionAlgorithmKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorResolutionAlgorithmVersionsFilter | null;
}

export interface ListTenorResolutionAlgorithmVersionsResponse {
    result: Result;
    versions: TenorResolutionAlgorithm[];
    total: number;
}

export interface GetTenorResolutionAlgorithmVersionRequest {
    key: TenorResolutionAlgorithmVersionKey;
}

export interface GetTenorResolutionAlgorithmVersionResponse {
    result: Result;
    version: TenorResolutionAlgorithm;
}

export const subjects = {
    list_tenor_resolution_algorithms_request: "refdata.v1.tenor_resolution_algorithms.list",
    get_tenor_resolution_algorithm_request: "refdata.v1.tenor_resolution_algorithms.get",
    get_many_tenor_resolution_algorithms_request: "refdata.v1.tenor_resolution_algorithms.get_many",
    put_tenor_resolution_algorithm_request: "refdata.v1.tenor_resolution_algorithms.put",
    put_many_tenor_resolution_algorithms_request: "refdata.v1.tenor_resolution_algorithms.put_many",
    delete_tenor_resolution_algorithm_request: "refdata.v1.tenor_resolution_algorithms.delete",
    delete_many_tenor_resolution_algorithms_request: "refdata.v1.tenor_resolution_algorithms.delete_many",
    list_tenor_resolution_algorithm_versions_request: "refdata.v1.tenor_resolution_algorithms_versions.list",
    get_tenor_resolution_algorithm_version_request: "refdata.v1.tenor_resolution_algorithms_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_resolution_algorithms_request: true,
    get_tenor_resolution_algorithm_request: true,
    get_many_tenor_resolution_algorithms_request: true,
    put_tenor_resolution_algorithm_request: true,
    put_many_tenor_resolution_algorithms_request: true,
    delete_tenor_resolution_algorithm_request: true,
    delete_many_tenor_resolution_algorithms_request: true,
    list_tenor_resolution_algorithm_versions_request: true,
    get_tenor_resolution_algorithm_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_resolution_algorithms_events.created",
    updated: "refdata.v1.tenor_resolution_algorithms_events.updated",
    deleted: "refdata.v1.tenor_resolution_algorithms_events.deleted",
} as const;

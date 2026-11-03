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
import type { DefaultCurveYieldReinterpretation } from '../domain/default_curve_yield_reinterpretation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveYieldReinterpretationKey {
    id: string;
}

export interface DefaultCurveYieldReinterpretationWrite {
    id: string;
    default_curve_configuration_id: string;
    reinterpreted_yield_curve: string | null;
}

export interface DefaultCurveYieldReinterpretationChange {
    write: DefaultCurveYieldReinterpretationWrite;
    precondition: Precondition;
}

export interface DefaultCurveYieldReinterpretationRemoval {
    key: DefaultCurveYieldReinterpretationKey;
    precondition: Precondition;
}

export interface DefaultCurveYieldReinterpretationLookup {
    key: DefaultCurveYieldReinterpretationKey;
    default_curve_yield_reinterpretation: DefaultCurveYieldReinterpretation | null;
}

export interface DefaultCurveYieldReinterpretationsFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveYieldReinterpretationEvent {
    event_id: string;
    key: DefaultCurveYieldReinterpretationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveYieldReinterpretationVersionKey {
    default_curve_yield_reinterpretation: DefaultCurveYieldReinterpretationKey;
    version: number;
}

export interface DefaultCurveYieldReinterpretationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveYieldReinterpretationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveYieldReinterpretationsFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveYieldReinterpretationsResponse {
    result: Result;
    yield_reinterpretations: DefaultCurveYieldReinterpretation[];
    total: number;
}

export interface GetDefaultCurveYieldReinterpretationRequest {
    key: DefaultCurveYieldReinterpretationKey;
}

export interface GetDefaultCurveYieldReinterpretationResponse {
    result: Result;
    default_curve_yield_reinterpretation: DefaultCurveYieldReinterpretation | null;
}

export interface GetManyDefaultCurveYieldReinterpretationsRequest {
    keys: DefaultCurveYieldReinterpretationKey[];
}

export interface GetManyDefaultCurveYieldReinterpretationsResponse {
    result: Result;
    entries: DefaultCurveYieldReinterpretationLookup[];
}

export interface PutDefaultCurveYieldReinterpretationRequest {
    change: DefaultCurveYieldReinterpretationChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveYieldReinterpretationResponse {
    result: Result;
    default_curve_yield_reinterpretation: DefaultCurveYieldReinterpretation | null;
}

export interface PutManyDefaultCurveYieldReinterpretationsRequest {
    changes: DefaultCurveYieldReinterpretationChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveYieldReinterpretationsResponse {
    result: Result;
    yield_reinterpretations: DefaultCurveYieldReinterpretation[];
}

export interface DeleteDefaultCurveYieldReinterpretationRequest {
    removal: DefaultCurveYieldReinterpretationRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveYieldReinterpretationResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveYieldReinterpretationsRequest {
    removals: DefaultCurveYieldReinterpretationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveYieldReinterpretationsResponse {
    result: Result;
}

export interface ListDefaultCurveYieldReinterpretationVersionsRequest {
    key: DefaultCurveYieldReinterpretationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveYieldReinterpretationVersionsFilter | null;
}

export interface ListDefaultCurveYieldReinterpretationVersionsResponse {
    result: Result;
    versions: DefaultCurveYieldReinterpretation[];
    total: number;
}

export interface GetDefaultCurveYieldReinterpretationVersionRequest {
    key: DefaultCurveYieldReinterpretationVersionKey;
}

export interface GetDefaultCurveYieldReinterpretationVersionResponse {
    result: Result;
    version: DefaultCurveYieldReinterpretation | null;
}

export const subjects = {
    list_default_curve_yield_reinterpretations_request:
        'refdata.v1.default_curve_yield_reinterpretations.list',
    get_default_curve_yield_reinterpretation_request:
        'refdata.v1.default_curve_yield_reinterpretations.get',
    get_many_default_curve_yield_reinterpretations_request:
        'refdata.v1.default_curve_yield_reinterpretations.get_many',
    put_default_curve_yield_reinterpretation_request:
        'refdata.v1.default_curve_yield_reinterpretations.put',
    put_many_default_curve_yield_reinterpretations_request:
        'refdata.v1.default_curve_yield_reinterpretations.put_many',
    delete_default_curve_yield_reinterpretation_request:
        'refdata.v1.default_curve_yield_reinterpretations.delete',
    delete_many_default_curve_yield_reinterpretations_request:
        'refdata.v1.default_curve_yield_reinterpretations.delete_many',
    list_default_curve_yield_reinterpretation_versions_request:
        'refdata.v1.default_curve_yield_reinterpretations_versions.list',
    get_default_curve_yield_reinterpretation_version_request:
        'refdata.v1.default_curve_yield_reinterpretations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_yield_reinterpretations_request: true,
    get_default_curve_yield_reinterpretation_request: true,
    get_many_default_curve_yield_reinterpretations_request: true,
    put_default_curve_yield_reinterpretation_request: true,
    put_many_default_curve_yield_reinterpretations_request: true,
    delete_default_curve_yield_reinterpretation_request: true,
    delete_many_default_curve_yield_reinterpretations_request: true,
    list_default_curve_yield_reinterpretation_versions_request: true,
    get_default_curve_yield_reinterpretation_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.default_curve_yield_reinterpretations.created',
    updated: 'refdata.v1.events.default_curve_yield_reinterpretations.updated',
    deleted: 'refdata.v1.events.default_curve_yield_reinterpretations.deleted',
} as const;

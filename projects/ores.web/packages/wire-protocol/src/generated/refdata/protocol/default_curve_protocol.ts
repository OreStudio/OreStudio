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
import type { DefaultCurve } from '../domain/default_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveKey {
    id: string;
}

export interface DefaultCurveWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
}

export interface DefaultCurveChange {
    write: DefaultCurveWrite;
    precondition: Precondition;
}

export interface DefaultCurveRemoval {
    key: DefaultCurveKey;
    precondition: Precondition;
}

export interface DefaultCurveLookup {
    key: DefaultCurveKey;
    default_curve: DefaultCurve | null;
}

export interface DefaultCurveEvent {
    event_id: string;
    key: DefaultCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveVersionKey {
    default_curve: DefaultCurveKey;
    version: number;
}

export interface DefaultCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDefaultCurvesResponse {
    result: Result;
    default_curves: DefaultCurve[];
    total: number;
}

export interface GetDefaultCurveRequest {
    key: DefaultCurveKey;
}

export interface GetDefaultCurveResponse {
    result: Result;
    default_curve: DefaultCurve | null;
}

export interface GetManyDefaultCurvesRequest {
    keys: DefaultCurveKey[];
}

export interface GetManyDefaultCurvesResponse {
    result: Result;
    entries: DefaultCurveLookup[];
}

export interface PutDefaultCurveRequest {
    change: DefaultCurveChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveResponse {
    result: Result;
    default_curve: DefaultCurve | null;
}

export interface PutManyDefaultCurvesRequest {
    changes: DefaultCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurvesResponse {
    result: Result;
    default_curves: DefaultCurve[];
}

export interface DeleteDefaultCurveRequest {
    removal: DefaultCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveResponse {
    result: Result;
}

export interface DeleteManyDefaultCurvesRequest {
    removals: DefaultCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurvesResponse {
    result: Result;
}

export interface ListDefaultCurveVersionsRequest {
    key: DefaultCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveVersionsFilter | null;
}

export interface ListDefaultCurveVersionsResponse {
    result: Result;
    versions: DefaultCurve[];
    total: number;
}

export interface GetDefaultCurveVersionRequest {
    key: DefaultCurveVersionKey;
}

export interface GetDefaultCurveVersionResponse {
    result: Result;
    version: DefaultCurve | null;
}

export const subjects = {
    list_default_curves_request: 'refdata.v1.default_curves.list',
    get_default_curve_request: 'refdata.v1.default_curves.get',
    get_many_default_curves_request: 'refdata.v1.default_curves.get_many',
    put_default_curve_request: 'refdata.v1.default_curves.put',
    put_many_default_curves_request: 'refdata.v1.default_curves.put_many',
    delete_default_curve_request: 'refdata.v1.default_curves.delete',
    delete_many_default_curves_request: 'refdata.v1.default_curves.delete_many',
    list_default_curve_versions_request: 'refdata.v1.default_curves_versions.list',
    get_default_curve_version_request: 'refdata.v1.default_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curves_request: true,
    get_default_curve_request: true,
    get_many_default_curves_request: true,
    put_default_curve_request: true,
    put_many_default_curves_request: true,
    delete_default_curve_request: true,
    delete_many_default_curves_request: true,
    list_default_curve_versions_request: true,
    get_default_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curves_events.created',
    updated: 'refdata.v1.default_curves_events.updated',
    deleted: 'refdata.v1.default_curves_events.deleted',
} as const;

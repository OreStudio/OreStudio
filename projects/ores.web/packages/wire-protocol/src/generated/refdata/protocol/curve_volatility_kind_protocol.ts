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
import type { CurveVolatilityKind } from '../domain/curve_volatility_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityKindKey {
    id: string;
}

export interface CurveVolatilityKindWrite {
    id: string;
    curve_volatility_config_id: string;
    kind_code: string;
    priority: number | null;
    quote_type: string | null;
    volatility_type: string | null;
    exercise_type: string | null;
    calendar: string | null;
}

export interface CurveVolatilityKindChange {
    write: CurveVolatilityKindWrite;
    precondition: Precondition;
}

export interface CurveVolatilityKindRemoval {
    key: CurveVolatilityKindKey;
    precondition: Precondition;
}

export interface CurveVolatilityKindLookup {
    key: CurveVolatilityKindKey;
    curve_volatility_kind: CurveVolatilityKind | null;
}

export interface CurveVolatilityKindsFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityKindEvent {
    event_id: string;
    key: CurveVolatilityKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityKindVersionKey {
    curve_volatility_kind: CurveVolatilityKindKey;
    version: number;
}

export interface CurveVolatilityKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityKindsFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityKindsResponse {
    result: Result;
    curve_volatility_kinds: CurveVolatilityKind[];
    total: number;
}

export interface GetCurveVolatilityKindRequest {
    key: CurveVolatilityKindKey;
}

export interface GetCurveVolatilityKindResponse {
    result: Result;
    curve_volatility_kind: CurveVolatilityKind | null;
}

export interface GetManyCurveVolatilityKindsRequest {
    keys: CurveVolatilityKindKey[];
}

export interface GetManyCurveVolatilityKindsResponse {
    result: Result;
    entries: CurveVolatilityKindLookup[];
}

export interface PutCurveVolatilityKindRequest {
    change: CurveVolatilityKindChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityKindResponse {
    result: Result;
    curve_volatility_kind: CurveVolatilityKind | null;
}

export interface PutManyCurveVolatilityKindsRequest {
    changes: CurveVolatilityKindChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityKindsResponse {
    result: Result;
    curve_volatility_kinds: CurveVolatilityKind[];
}

export interface DeleteCurveVolatilityKindRequest {
    removal: CurveVolatilityKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityKindResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityKindsRequest {
    removals: CurveVolatilityKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityKindsResponse {
    result: Result;
}

export interface ListCurveVolatilityKindVersionsRequest {
    key: CurveVolatilityKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityKindVersionsFilter | null;
}

export interface ListCurveVolatilityKindVersionsResponse {
    result: Result;
    versions: CurveVolatilityKind[];
    total: number;
}

export interface GetCurveVolatilityKindVersionRequest {
    key: CurveVolatilityKindVersionKey;
}

export interface GetCurveVolatilityKindVersionResponse {
    result: Result;
    version: CurveVolatilityKind | null;
}

export const subjects = {
    list_curve_volatility_kinds_request: 'refdata.v1.curve_volatility_kinds.list',
    get_curve_volatility_kind_request: 'refdata.v1.curve_volatility_kinds.get',
    get_many_curve_volatility_kinds_request: 'refdata.v1.curve_volatility_kinds.get_many',
    put_curve_volatility_kind_request: 'refdata.v1.curve_volatility_kinds.put',
    put_many_curve_volatility_kinds_request: 'refdata.v1.curve_volatility_kinds.put_many',
    delete_curve_volatility_kind_request: 'refdata.v1.curve_volatility_kinds.delete',
    delete_many_curve_volatility_kinds_request: 'refdata.v1.curve_volatility_kinds.delete_many',
    list_curve_volatility_kind_versions_request: 'refdata.v1.curve_volatility_kinds_versions.list',
    get_curve_volatility_kind_version_request: 'refdata.v1.curve_volatility_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_kinds_request: true,
    get_curve_volatility_kind_request: true,
    get_many_curve_volatility_kinds_request: true,
    put_curve_volatility_kind_request: true,
    put_many_curve_volatility_kinds_request: true,
    delete_curve_volatility_kind_request: true,
    delete_many_curve_volatility_kinds_request: true,
    list_curve_volatility_kind_versions_request: true,
    get_curve_volatility_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_kinds_events.created',
    updated: 'refdata.v1.curve_volatility_kinds_events.updated',
    deleted: 'refdata.v1.curve_volatility_kinds_events.deleted',
} as const;

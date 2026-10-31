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
import type { CurveVolatilityConstant } from '../domain/curve_volatility_constant.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityConstantKey {
    id: string;
}

export interface CurveVolatilityConstantWrite {
    id: string;
    curve_volatility_kind_id: string;
    quote: string;
}

export interface CurveVolatilityConstantChange {
    write: CurveVolatilityConstantWrite;
    precondition: Precondition;
}

export interface CurveVolatilityConstantRemoval {
    key: CurveVolatilityConstantKey;
    precondition: Precondition;
}

export interface CurveVolatilityConstantLookup {
    key: CurveVolatilityConstantKey;
    curve_volatility_constant: CurveVolatilityConstant | null;
}

export interface CurveVolatilityConstantsFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityConstantEvent {
    event_id: string;
    key: CurveVolatilityConstantKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityConstantVersionKey {
    curve_volatility_constant: CurveVolatilityConstantKey;
    version: number;
}

export interface CurveVolatilityConstantVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityConstantsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityConstantsFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityConstantsResponse {
    result: Result;
    curve_volatility_constants: CurveVolatilityConstant[];
    total: number;
}

export interface GetCurveVolatilityConstantRequest {
    key: CurveVolatilityConstantKey;
}

export interface GetCurveVolatilityConstantResponse {
    result: Result;
    curve_volatility_constant: CurveVolatilityConstant | null;
}

export interface GetManyCurveVolatilityConstantsRequest {
    keys: CurveVolatilityConstantKey[];
}

export interface GetManyCurveVolatilityConstantsResponse {
    result: Result;
    entries: CurveVolatilityConstantLookup[];
}

export interface PutCurveVolatilityConstantRequest {
    change: CurveVolatilityConstantChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityConstantResponse {
    result: Result;
    curve_volatility_constant: CurveVolatilityConstant | null;
}

export interface PutManyCurveVolatilityConstantsRequest {
    changes: CurveVolatilityConstantChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityConstantsResponse {
    result: Result;
    curve_volatility_constants: CurveVolatilityConstant[];
}

export interface DeleteCurveVolatilityConstantRequest {
    removal: CurveVolatilityConstantRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityConstantResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityConstantsRequest {
    removals: CurveVolatilityConstantRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityConstantsResponse {
    result: Result;
}

export interface ListCurveVolatilityConstantVersionsRequest {
    key: CurveVolatilityConstantKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityConstantVersionsFilter | null;
}

export interface ListCurveVolatilityConstantVersionsResponse {
    result: Result;
    versions: CurveVolatilityConstant[];
    total: number;
}

export interface GetCurveVolatilityConstantVersionRequest {
    key: CurveVolatilityConstantVersionKey;
}

export interface GetCurveVolatilityConstantVersionResponse {
    result: Result;
    version: CurveVolatilityConstant | null;
}

export const subjects = {
    list_curve_volatility_constants_request: 'refdata.v1.curve_volatility_constants.list',
    get_curve_volatility_constant_request: 'refdata.v1.curve_volatility_constants.get',
    get_many_curve_volatility_constants_request: 'refdata.v1.curve_volatility_constants.get_many',
    put_curve_volatility_constant_request: 'refdata.v1.curve_volatility_constants.put',
    put_many_curve_volatility_constants_request: 'refdata.v1.curve_volatility_constants.put_many',
    delete_curve_volatility_constant_request: 'refdata.v1.curve_volatility_constants.delete',
    delete_many_curve_volatility_constants_request:
        'refdata.v1.curve_volatility_constants.delete_many',
    list_curve_volatility_constant_versions_request:
        'refdata.v1.curve_volatility_constants_versions.list',
    get_curve_volatility_constant_version_request:
        'refdata.v1.curve_volatility_constants_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_constants_request: true,
    get_curve_volatility_constant_request: true,
    get_many_curve_volatility_constants_request: true,
    put_curve_volatility_constant_request: true,
    put_many_curve_volatility_constants_request: true,
    delete_curve_volatility_constant_request: true,
    delete_many_curve_volatility_constants_request: true,
    list_curve_volatility_constant_versions_request: true,
    get_curve_volatility_constant_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_constants_events.created',
    updated: 'refdata.v1.curve_volatility_constants_events.updated',
    deleted: 'refdata.v1.curve_volatility_constants_events.deleted',
} as const;

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
import type { CurveSecurity } from '../domain/curve_security.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSecurityKey {
    id: string;
}

export interface CurveSecurityWrite {
    id: string;
    curve_definition_id: string;
    spread_quote: string | null;
    recovery_rate_quote: string | null;
    cpr_quote: string | null;
    price_quote: string | null;
    conversion_factor: string | null;
}

export interface CurveSecurityChange {
    write: CurveSecurityWrite;
    precondition: Precondition;
}

export interface CurveSecurityRemoval {
    key: CurveSecurityKey;
    precondition: Precondition;
}

export interface CurveSecurityLookup {
    key: CurveSecurityKey;
    curve_security: CurveSecurity | null;
}

export interface CurveSecurityEvent {
    event_id: string;
    key: CurveSecurityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSecurityVersionKey {
    curve_security: CurveSecurityKey;
    version: number;
}

export interface CurveSecurityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSecuritiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveSecuritiesResponse {
    result: Result;
    securities: CurveSecurity[];
    total: number;
}

export interface GetCurveSecurityRequest {
    key: CurveSecurityKey;
}

export interface GetCurveSecurityResponse {
    result: Result;
    curve_security: CurveSecurity | null;
}

export interface GetManyCurveSecuritiesRequest {
    keys: CurveSecurityKey[];
}

export interface GetManyCurveSecuritiesResponse {
    result: Result;
    entries: CurveSecurityLookup[];
}

export interface PutCurveSecurityRequest {
    change: CurveSecurityChange;
    intent: ChangeIntent;
}

export interface PutCurveSecurityResponse {
    result: Result;
    curve_security: CurveSecurity | null;
}

export interface PutManyCurveSecuritiesRequest {
    changes: CurveSecurityChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSecuritiesResponse {
    result: Result;
    securities: CurveSecurity[];
}

export interface DeleteCurveSecurityRequest {
    removal: CurveSecurityRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSecurityResponse {
    result: Result;
}

export interface DeleteManyCurveSecuritiesRequest {
    removals: CurveSecurityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSecuritiesResponse {
    result: Result;
}

export interface ListCurveSecurityVersionsRequest {
    key: CurveSecurityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSecurityVersionsFilter | null;
}

export interface ListCurveSecurityVersionsResponse {
    result: Result;
    versions: CurveSecurity[];
    total: number;
}

export interface GetCurveSecurityVersionRequest {
    key: CurveSecurityVersionKey;
}

export interface GetCurveSecurityVersionResponse {
    result: Result;
    version: CurveSecurity | null;
}

export const subjects = {
    list_curve_securities_request: 'refdata.v1.curve_securities.list',
    get_curve_security_request: 'refdata.v1.curve_securities.get',
    get_many_curve_securities_request: 'refdata.v1.curve_securities.get_many',
    put_curve_security_request: 'refdata.v1.curve_securities.put',
    put_many_curve_securities_request: 'refdata.v1.curve_securities.put_many',
    delete_curve_security_request: 'refdata.v1.curve_securities.delete',
    delete_many_curve_securities_request: 'refdata.v1.curve_securities.delete_many',
    list_curve_security_versions_request: 'refdata.v1.curve_securities_versions.list',
    get_curve_security_version_request: 'refdata.v1.curve_securities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_securities_request: true,
    get_curve_security_request: true,
    get_many_curve_securities_request: true,
    put_curve_security_request: true,
    put_many_curve_securities_request: true,
    delete_curve_security_request: true,
    delete_many_curve_securities_request: true,
    list_curve_security_versions_request: true,
    get_curve_security_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_securities_events.created',
    updated: 'refdata.v1.curve_securities_events.updated',
    deleted: 'refdata.v1.curve_securities_events.deleted',
} as const;

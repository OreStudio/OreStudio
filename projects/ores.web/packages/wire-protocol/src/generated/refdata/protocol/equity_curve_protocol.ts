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
import type { EquityCurve } from '../domain/equity_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityCurveKey {
    id: string;
}

export interface EquityCurveWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    calendar: string | null;
    forecasting_curve: string;
    equity_type: string;
    exercise_style: string | null;
    spot_quote: string;
    has_quotes: boolean;
    day_counter: string | null;
    has_dividend_interpolation: boolean;
    dividend_interpolation_variable: string | null;
    dividend_interpolation_method: string | null;
    dividend_extrapolation: string | null;
    extrapolation: string | null;
}

export interface EquityCurveChange {
    write: EquityCurveWrite;
    precondition: Precondition;
}

export interface EquityCurveRemoval {
    key: EquityCurveKey;
    precondition: Precondition;
}

export interface EquityCurveLookup {
    key: EquityCurveKey;
    equity_curve: EquityCurve | null;
}

export interface EquityCurveEvent {
    event_id: string;
    key: EquityCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityCurveVersionKey {
    equity_curve: EquityCurveKey;
    version: number;
}

export interface EquityCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListEquityCurvesResponse {
    result: Result;
    equity_curves: EquityCurve[];
    total: number;
}

export interface GetEquityCurveRequest {
    key: EquityCurveKey;
}

export interface GetEquityCurveResponse {
    result: Result;
    equity_curve: EquityCurve | null;
}

export interface GetManyEquityCurvesRequest {
    keys: EquityCurveKey[];
}

export interface GetManyEquityCurvesResponse {
    result: Result;
    entries: EquityCurveLookup[];
}

export interface PutEquityCurveRequest {
    change: EquityCurveChange;
    intent: ChangeIntent;
}

export interface PutEquityCurveResponse {
    result: Result;
    equity_curve: EquityCurve | null;
}

export interface PutManyEquityCurvesRequest {
    changes: EquityCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityCurvesResponse {
    result: Result;
    equity_curves: EquityCurve[];
}

export interface DeleteEquityCurveRequest {
    removal: EquityCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityCurveResponse {
    result: Result;
}

export interface DeleteManyEquityCurvesRequest {
    removals: EquityCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityCurvesResponse {
    result: Result;
}

export interface ListEquityCurveVersionsRequest {
    key: EquityCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityCurveVersionsFilter | null;
}

export interface ListEquityCurveVersionsResponse {
    result: Result;
    versions: EquityCurve[];
    total: number;
}

export interface GetEquityCurveVersionRequest {
    key: EquityCurveVersionKey;
}

export interface GetEquityCurveVersionResponse {
    result: Result;
    version: EquityCurve | null;
}

export const subjects = {
    list_equity_curves_request: 'refdata.v1.equity_curves.list',
    get_equity_curve_request: 'refdata.v1.equity_curves.get',
    get_many_equity_curves_request: 'refdata.v1.equity_curves.get_many',
    put_equity_curve_request: 'refdata.v1.equity_curves.put',
    put_many_equity_curves_request: 'refdata.v1.equity_curves.put_many',
    delete_equity_curve_request: 'refdata.v1.equity_curves.delete',
    delete_many_equity_curves_request: 'refdata.v1.equity_curves.delete_many',
    list_equity_curve_versions_request: 'refdata.v1.equity_curves_versions.list',
    get_equity_curve_version_request: 'refdata.v1.equity_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_curves_request: true,
    get_equity_curve_request: true,
    get_many_equity_curves_request: true,
    put_equity_curve_request: true,
    put_many_equity_curves_request: true,
    delete_equity_curve_request: true,
    delete_many_equity_curves_request: true,
    list_equity_curve_versions_request: true,
    get_equity_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.equity_curves_events.created',
    updated: 'refdata.v1.equity_curves_events.updated',
    deleted: 'refdata.v1.equity_curves_events.deleted',
} as const;

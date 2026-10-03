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
import type { IntradayPowerCurve } from '../domain/intraday_power_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IntradayPowerCurveKey {
    id: string;
}

export interface IntradayPowerCurveWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    daily_average_price_curve: string;
    shape_quote_name: string;
    convention: string | null;
}

export interface IntradayPowerCurveChange {
    write: IntradayPowerCurveWrite;
    precondition: Precondition;
}

export interface IntradayPowerCurveRemoval {
    key: IntradayPowerCurveKey;
    precondition: Precondition;
}

export interface IntradayPowerCurveLookup {
    key: IntradayPowerCurveKey;
    intraday_power_curve: IntradayPowerCurve | null;
}

export interface IntradayPowerCurveEvent {
    event_id: string;
    key: IntradayPowerCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IntradayPowerCurveVersionKey {
    intraday_power_curve: IntradayPowerCurveKey;
    version: number;
}

export interface IntradayPowerCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIntradayPowerCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListIntradayPowerCurvesResponse {
    result: Result;
    intraday_power_curves: IntradayPowerCurve[];
    total: number;
}

export interface GetIntradayPowerCurveRequest {
    key: IntradayPowerCurveKey;
}

export interface GetIntradayPowerCurveResponse {
    result: Result;
    intraday_power_curve: IntradayPowerCurve | null;
}

export interface GetManyIntradayPowerCurvesRequest {
    keys: IntradayPowerCurveKey[];
}

export interface GetManyIntradayPowerCurvesResponse {
    result: Result;
    entries: IntradayPowerCurveLookup[];
}

export interface PutIntradayPowerCurveRequest {
    change: IntradayPowerCurveChange;
    intent: ChangeIntent;
}

export interface PutIntradayPowerCurveResponse {
    result: Result;
    intraday_power_curve: IntradayPowerCurve | null;
}

export interface PutManyIntradayPowerCurvesRequest {
    changes: IntradayPowerCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyIntradayPowerCurvesResponse {
    result: Result;
    intraday_power_curves: IntradayPowerCurve[];
}

export interface DeleteIntradayPowerCurveRequest {
    removal: IntradayPowerCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteIntradayPowerCurveResponse {
    result: Result;
}

export interface DeleteManyIntradayPowerCurvesRequest {
    removals: IntradayPowerCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIntradayPowerCurvesResponse {
    result: Result;
}

export interface ListIntradayPowerCurveVersionsRequest {
    key: IntradayPowerCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IntradayPowerCurveVersionsFilter | null;
}

export interface ListIntradayPowerCurveVersionsResponse {
    result: Result;
    versions: IntradayPowerCurve[];
    total: number;
}

export interface GetIntradayPowerCurveVersionRequest {
    key: IntradayPowerCurveVersionKey;
}

export interface GetIntradayPowerCurveVersionResponse {
    result: Result;
    version: IntradayPowerCurve | null;
}

export const subjects = {
    list_intraday_power_curves_request: 'refdata.v1.intraday_power_curves.list',
    get_intraday_power_curve_request: 'refdata.v1.intraday_power_curves.get',
    get_many_intraday_power_curves_request: 'refdata.v1.intraday_power_curves.get_many',
    put_intraday_power_curve_request: 'refdata.v1.intraday_power_curves.put',
    put_many_intraday_power_curves_request: 'refdata.v1.intraday_power_curves.put_many',
    delete_intraday_power_curve_request: 'refdata.v1.intraday_power_curves.delete',
    delete_many_intraday_power_curves_request: 'refdata.v1.intraday_power_curves.delete_many',
    list_intraday_power_curve_versions_request: 'refdata.v1.intraday_power_curves_versions.list',
    get_intraday_power_curve_version_request: 'refdata.v1.intraday_power_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_intraday_power_curves_request: true,
    get_intraday_power_curve_request: true,
    get_many_intraday_power_curves_request: true,
    put_intraday_power_curve_request: true,
    put_many_intraday_power_curves_request: true,
    delete_intraday_power_curve_request: true,
    delete_many_intraday_power_curves_request: true,
    list_intraday_power_curve_versions_request: true,
    get_intraday_power_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.intraday_power_curves_events.created',
    updated: 'refdata.v1.intraday_power_curves_events.updated',
    deleted: 'refdata.v1.intraday_power_curves_events.deleted',
} as const;

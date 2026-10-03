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
import type { CurveGlobalReport } from '../domain/curve_global_report.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveGlobalReportKey {
    id: string;
}

export interface CurveGlobalReportWrite {
    id: string;
    curve_configuration_id: string;
    family: string;
    has_report: boolean;
    report_on_delta_grid: string | null;
    report_on_moneyness_grid: string | null;
    report_on_strike_grid: string | null;
    report_on_strike_spread_grid: string | null;
    deltas: string | null;
    moneyness: string | null;
    strikes: string | null;
    strike_spreads: string | null;
    expiries: string | null;
    pillar_dates: string | null;
    underlying_tenors: string | null;
    continuation_expiry: string | null;
    position: number;
}

export interface CurveGlobalReportChange {
    write: CurveGlobalReportWrite;
    precondition: Precondition;
}

export interface CurveGlobalReportRemoval {
    key: CurveGlobalReportKey;
    precondition: Precondition;
}

export interface CurveGlobalReportLookup {
    key: CurveGlobalReportKey;
    curve_global_report: CurveGlobalReport | null;
}

export interface CurveGlobalReportEvent {
    event_id: string;
    key: CurveGlobalReportKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveGlobalReportVersionKey {
    curve_global_report: CurveGlobalReportKey;
    version: number;
}

export interface CurveGlobalReportVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveGlobalReportsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveGlobalReportsResponse {
    result: Result;
    global_reports: CurveGlobalReport[];
    total: number;
}

export interface GetCurveGlobalReportRequest {
    key: CurveGlobalReportKey;
}

export interface GetCurveGlobalReportResponse {
    result: Result;
    curve_global_report: CurveGlobalReport | null;
}

export interface GetManyCurveGlobalReportsRequest {
    keys: CurveGlobalReportKey[];
}

export interface GetManyCurveGlobalReportsResponse {
    result: Result;
    entries: CurveGlobalReportLookup[];
}

export interface PutCurveGlobalReportRequest {
    change: CurveGlobalReportChange;
    intent: ChangeIntent;
}

export interface PutCurveGlobalReportResponse {
    result: Result;
    curve_global_report: CurveGlobalReport | null;
}

export interface PutManyCurveGlobalReportsRequest {
    changes: CurveGlobalReportChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveGlobalReportsResponse {
    result: Result;
    global_reports: CurveGlobalReport[];
}

export interface DeleteCurveGlobalReportRequest {
    removal: CurveGlobalReportRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveGlobalReportResponse {
    result: Result;
}

export interface DeleteManyCurveGlobalReportsRequest {
    removals: CurveGlobalReportRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveGlobalReportsResponse {
    result: Result;
}

export interface ListCurveGlobalReportVersionsRequest {
    key: CurveGlobalReportKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveGlobalReportVersionsFilter | null;
}

export interface ListCurveGlobalReportVersionsResponse {
    result: Result;
    versions: CurveGlobalReport[];
    total: number;
}

export interface GetCurveGlobalReportVersionRequest {
    key: CurveGlobalReportVersionKey;
}

export interface GetCurveGlobalReportVersionResponse {
    result: Result;
    version: CurveGlobalReport | null;
}

export const subjects = {
    list_curve_global_reports_request: 'refdata.v1.curve_global_reports.list',
    get_curve_global_report_request: 'refdata.v1.curve_global_reports.get',
    get_many_curve_global_reports_request: 'refdata.v1.curve_global_reports.get_many',
    put_curve_global_report_request: 'refdata.v1.curve_global_reports.put',
    put_many_curve_global_reports_request: 'refdata.v1.curve_global_reports.put_many',
    delete_curve_global_report_request: 'refdata.v1.curve_global_reports.delete',
    delete_many_curve_global_reports_request: 'refdata.v1.curve_global_reports.delete_many',
    list_curve_global_report_versions_request: 'refdata.v1.curve_global_reports_versions.list',
    get_curve_global_report_version_request: 'refdata.v1.curve_global_reports_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_global_reports_request: true,
    get_curve_global_report_request: true,
    get_many_curve_global_reports_request: true,
    put_curve_global_report_request: true,
    put_many_curve_global_reports_request: true,
    delete_curve_global_report_request: true,
    delete_many_curve_global_reports_request: true,
    list_curve_global_report_versions_request: true,
    get_curve_global_report_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_global_reports_events.created',
    updated: 'refdata.v1.curve_global_reports_events.updated',
    deleted: 'refdata.v1.curve_global_reports_events.deleted',
} as const;

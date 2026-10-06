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
import type { ReportAnalytic } from '../domain/report_analytic.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportAnalyticKey {
    id: string;
}

export interface ReportAnalyticWrite {
    id: string;
    report_definition_id: string;
    analytic_type_code: string;
    display_order: number;
    active: string;
}

export interface ReportAnalyticChange {
    write: ReportAnalyticWrite;
    precondition: Precondition;
}

export interface ReportAnalyticRemoval {
    key: ReportAnalyticKey;
    precondition: Precondition;
}

export interface ReportAnalyticLookup {
    key: ReportAnalyticKey;
    report_analytic: ReportAnalytic | null;
}

export interface ReportAnalyticsFilter {
    analytic_type_code: string | null;
    id_one_of: string[] | null;
    analytic_type_code_one_of: string[] | null;
}

export interface ReportAnalyticEvent {
    event_id: string;
    key: ReportAnalyticKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportAnalyticVersionKey {
    report_analytic: ReportAnalyticKey;
    version: number;
}

export interface ReportAnalyticVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportAnalyticsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticsFilter | null;
    as_of: string | null;
}

export interface ListReportAnalyticsResponse {
    result: Result;
    analytics: ReportAnalytic[];
    total: number;
}

export interface GetReportAnalyticRequest {
    key: ReportAnalyticKey;
}

export interface GetReportAnalyticResponse {
    result: Result;
    report_analytic: ReportAnalytic | null;
}

export interface GetManyReportAnalyticsRequest {
    keys: ReportAnalyticKey[];
}

export interface GetManyReportAnalyticsResponse {
    result: Result;
    entries: ReportAnalyticLookup[];
}

export interface PutReportAnalyticRequest {
    change: ReportAnalyticChange;
    intent: ChangeIntent;
}

export interface PutReportAnalyticResponse {
    result: Result;
    report_analytic: ReportAnalytic | null;
}

export interface PutManyReportAnalyticsRequest {
    changes: ReportAnalyticChange[];
    intent: ChangeIntent;
}

export interface PutManyReportAnalyticsResponse {
    result: Result;
    analytics: ReportAnalytic[];
}

export interface DeleteReportAnalyticRequest {
    removal: ReportAnalyticRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportAnalyticResponse {
    result: Result;
}

export interface DeleteManyReportAnalyticsRequest {
    removals: ReportAnalyticRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportAnalyticsResponse {
    result: Result;
}

export interface ListByAnalyticTypeCodeReportAnalyticsRequest {
    analytic_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticsFilter | null;
}

export interface ListByAnalyticTypeCodeReportAnalyticsResponse {
    result: Result;
    analytics: ReportAnalytic[];
    total: number;
}

export interface ListReportAnalyticVersionsRequest {
    key: ReportAnalyticKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticVersionsFilter | null;
}

export interface ListReportAnalyticVersionsResponse {
    result: Result;
    versions: ReportAnalytic[];
    total: number;
}

export interface GetReportAnalyticVersionRequest {
    key: ReportAnalyticVersionKey;
}

export interface GetReportAnalyticVersionResponse {
    result: Result;
    version: ReportAnalytic | null;
}

export const subjects = {
    list_report_analytics_request: 'reporting.v1.report_analytics.list',
    get_report_analytic_request: 'reporting.v1.report_analytics.get',
    get_many_report_analytics_request: 'reporting.v1.report_analytics.get_many',
    put_report_analytic_request: 'reporting.v1.report_analytics.put',
    put_many_report_analytics_request: 'reporting.v1.report_analytics.put_many',
    delete_report_analytic_request: 'reporting.v1.report_analytics.delete',
    delete_many_report_analytics_request: 'reporting.v1.report_analytics.delete_many',
    list_by_analytic_type_code_report_analytics_request:
        'reporting.v1.report_analytics.list_by_analytic_type_code',
    list_report_analytic_versions_request: 'reporting.v1.report_analytics_versions.list',
    get_report_analytic_version_request: 'reporting.v1.report_analytics_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_analytics_request: true,
    get_report_analytic_request: true,
    get_many_report_analytics_request: true,
    put_report_analytic_request: true,
    put_many_report_analytics_request: true,
    delete_report_analytic_request: true,
    delete_many_report_analytics_request: true,
    list_by_analytic_type_code_report_analytics_request: true,
    list_report_analytic_versions_request: true,
    get_report_analytic_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_analytics_events.created',
    updated: 'reporting.v1.report_analytics_events.updated',
    deleted: 'reporting.v1.report_analytics_events.deleted',
} as const;

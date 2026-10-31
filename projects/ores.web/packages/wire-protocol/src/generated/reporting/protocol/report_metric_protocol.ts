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
import type { ReportMetric } from '../domain/report_metric.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportMetricKey {
    id: string;
}

export interface ReportMetricWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    family: string;
    subject_type: string;
    subject_id: string;
    as_of: string;
    measure: string;
    report_definition_id: string;
    currency: string;
    scenario: string;
    factor: string;
    shift: string;
    qualifiers: string;
    value: number;
    source: string;
}

export interface ReportMetricChange {
    write: ReportMetricWrite;
    precondition: Precondition;
}

export interface ReportMetricRemoval {
    key: ReportMetricKey;
    precondition: Precondition;
}

export interface ReportMetricLookup {
    key: ReportMetricKey;
    report_metric: ReportMetric | null;
}

export interface ReportMetricsFilter {
    report_instance_id: string | null;
    family: string | null;
    subject_id: string | null;
    measure: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    family_one_of: string[] | null;
    subject_id_one_of: string[] | null;
    measure_one_of: string[] | null;
}

export interface ReportMetricEvent {
    event_id: string;
    key: ReportMetricKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListReportMetricsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportMetricsFilter | null;
    as_of: string | null;
}

export interface ListReportMetricsResponse {
    result: Result;
    metrics: ReportMetric[];
    total: number;
}

export interface GetReportMetricRequest {
    key: ReportMetricKey;
}

export interface GetReportMetricResponse {
    result: Result;
    report_metric: ReportMetric | null;
}

export interface GetManyReportMetricsRequest {
    keys: ReportMetricKey[];
}

export interface GetManyReportMetricsResponse {
    result: Result;
    entries: ReportMetricLookup[];
}

export interface PutReportMetricRequest {
    change: ReportMetricChange;
    intent: ChangeIntent;
}

export interface PutReportMetricResponse {
    result: Result;
    report_metric: ReportMetric | null;
}

export interface PutManyReportMetricsRequest {
    changes: ReportMetricChange[];
    intent: ChangeIntent;
}

export interface PutManyReportMetricsResponse {
    result: Result;
    metrics: ReportMetric[];
}

export interface DeleteReportMetricRequest {
    removal: ReportMetricRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportMetricResponse {
    result: Result;
}

export interface DeleteManyReportMetricsRequest {
    removals: ReportMetricRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportMetricsResponse {
    result: Result;
}

export const subjects = {
    list_report_metrics_request: 'reporting.v1.report_metrics.list',
    get_report_metric_request: 'reporting.v1.report_metrics.get',
    get_many_report_metrics_request: 'reporting.v1.report_metrics.get_many',
    put_report_metric_request: 'reporting.v1.report_metrics.put',
    put_many_report_metrics_request: 'reporting.v1.report_metrics.put_many',
    delete_report_metric_request: 'reporting.v1.report_metrics.delete',
    delete_many_report_metrics_request: 'reporting.v1.report_metrics.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_metrics_request: true,
    get_report_metric_request: true,
    get_many_report_metrics_request: true,
    put_report_metric_request: true,
    put_many_report_metrics_request: true,
    delete_report_metric_request: true,
    delete_many_report_metrics_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_metrics_events.created',
    updated: 'reporting.v1.report_metrics_events.updated',
    deleted: 'reporting.v1.report_metrics_events.deleted',
} as const;

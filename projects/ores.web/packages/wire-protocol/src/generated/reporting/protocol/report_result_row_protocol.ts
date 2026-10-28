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
import type { ReportResultRow } from '../domain/report_result_row.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportResultRowKey {
    id: string;
}

export interface ReportResultRowWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    as_of: string;
    subject_type: string;
    subject_id: string;
    measure: string;
    currency: string;
    report_definition_id: string;
    counterparty_id: string;
    figure: number;
    source: string;
}

export interface ReportResultRowChange {
    write: ReportResultRowWrite;
    precondition: Precondition;
}

export interface ReportResultRowRemoval {
    key: ReportResultRowKey;
    precondition: Precondition;
}

export interface ReportResultRowLookup {
    key: ReportResultRowKey;
    report_result_row: ReportResultRow | null;
}

export interface ReportResultRowsFilter {
    report_instance_id: string | null;
    measure: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    measure_one_of: string[] | null;
}

export interface ReportResultRowEvent {
    event_id: string;
    key: ReportResultRowKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListReportResultRowsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportResultRowsFilter | null;
    as_of: string | null;
}

export interface ListReportResultRowsResponse {
    result: Result;
    rows: ReportResultRow[];
    total: number;
}

export interface GetReportResultRowRequest {
    key: ReportResultRowKey;
}

export interface GetReportResultRowResponse {
    result: Result;
    report_result_row: ReportResultRow | null;
}

export interface GetManyReportResultRowsRequest {
    keys: ReportResultRowKey[];
}

export interface GetManyReportResultRowsResponse {
    result: Result;
    entries: ReportResultRowLookup[];
}

export interface PutReportResultRowRequest {
    change: ReportResultRowChange;
    intent: ChangeIntent;
}

export interface PutReportResultRowResponse {
    result: Result;
    report_result_row: ReportResultRow | null;
}

export interface PutManyReportResultRowsRequest {
    changes: ReportResultRowChange[];
    intent: ChangeIntent;
}

export interface PutManyReportResultRowsResponse {
    result: Result;
    rows: ReportResultRow[];
}

export interface DeleteReportResultRowRequest {
    removal: ReportResultRowRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportResultRowResponse {
    result: Result;
}

export interface DeleteManyReportResultRowsRequest {
    removals: ReportResultRowRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportResultRowsResponse {
    result: Result;
}

export const subjects = {
    list_report_result_rows_request: 'reporting.v1.report_result_rows.list',
    get_report_result_row_request: 'reporting.v1.report_result_rows.get',
    get_many_report_result_rows_request: 'reporting.v1.report_result_rows.get_many',
    put_report_result_row_request: 'reporting.v1.report_result_rows.put',
    put_many_report_result_rows_request: 'reporting.v1.report_result_rows.put_many',
    delete_report_result_row_request: 'reporting.v1.report_result_rows.delete',
    delete_many_report_result_rows_request: 'reporting.v1.report_result_rows.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_result_rows_request: true,
    get_report_result_row_request: true,
    get_many_report_result_rows_request: true,
    put_report_result_row_request: true,
    put_many_report_result_rows_request: true,
    delete_report_result_row_request: true,
    delete_many_report_result_rows_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_result_rows_events.created',
    updated: 'reporting.v1.report_result_rows_events.updated',
    deleted: 'reporting.v1.report_result_rows_events.deleted',
} as const;

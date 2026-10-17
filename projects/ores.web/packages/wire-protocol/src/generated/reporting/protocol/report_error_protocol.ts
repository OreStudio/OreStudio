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
import type { ReportError } from '../domain/report_error.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportErrorKey {
    id: string;
}

export interface ReportErrorWrite {
    id: string;
    report_instance_id: string;
    scope_code: string;
    severity: string;
    message: string;
    subject: string | null;
    subject_id: string | null;
    detail: string;
}

export interface ReportErrorChange {
    write: ReportErrorWrite;
    precondition: Precondition;
}

export interface ReportErrorRemoval {
    key: ReportErrorKey;
    precondition: Precondition;
}

export interface ReportErrorLookup {
    key: ReportErrorKey;
    report_error: ReportError | null;
}

export interface ReportErrorsFilter {
    report_instance_id: string | null;
    severity: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    severity_one_of: string[] | null;
}

export interface ReportErrorEvent {
    event_id: string;
    key: ReportErrorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportErrorVersionKey {
    report_error: ReportErrorKey;
    version: number;
}

export interface ReportErrorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportErrorsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorsFilter | null;
    as_of: string | null;
}

export interface ListReportErrorsResponse {
    result: Result;
    errors: ReportError[];
    total: number;
}

export interface GetReportErrorRequest {
    key: ReportErrorKey;
}

export interface GetReportErrorResponse {
    result: Result;
    report_error: ReportError | null;
}

export interface GetManyReportErrorsRequest {
    keys: ReportErrorKey[];
}

export interface GetManyReportErrorsResponse {
    result: Result;
    entries: ReportErrorLookup[];
}

export interface PutReportErrorRequest {
    change: ReportErrorChange;
    intent: ChangeIntent;
}

export interface PutReportErrorResponse {
    result: Result;
    report_error: ReportError | null;
}

export interface PutManyReportErrorsRequest {
    changes: ReportErrorChange[];
    intent: ChangeIntent;
}

export interface PutManyReportErrorsResponse {
    result: Result;
    errors: ReportError[];
}

export interface DeleteReportErrorRequest {
    removal: ReportErrorRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportErrorResponse {
    result: Result;
}

export interface DeleteManyReportErrorsRequest {
    removals: ReportErrorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportErrorsResponse {
    result: Result;
}

export interface ListByReportInstanceIdReportErrorsRequest {
    report_instance_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorsFilter | null;
}

export interface ListByReportInstanceIdReportErrorsResponse {
    result: Result;
    errors: ReportError[];
    total: number;
}

export interface ListReportErrorVersionsRequest {
    key: ReportErrorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorVersionsFilter | null;
}

export interface ListReportErrorVersionsResponse {
    result: Result;
    versions: ReportError[];
    total: number;
}

export interface GetReportErrorVersionRequest {
    key: ReportErrorVersionKey;
}

export interface GetReportErrorVersionResponse {
    result: Result;
    version: ReportError | null;
}

export const subjects = {
    list_report_errors_request: 'reporting.v1.report_errors.list',
    get_report_error_request: 'reporting.v1.report_errors.get',
    get_many_report_errors_request: 'reporting.v1.report_errors.get_many',
    put_report_error_request: 'reporting.v1.report_errors.put',
    put_many_report_errors_request: 'reporting.v1.report_errors.put_many',
    delete_report_error_request: 'reporting.v1.report_errors.delete',
    delete_many_report_errors_request: 'reporting.v1.report_errors.delete_many',
    list_by_report_instance_id_report_errors_request:
        'reporting.v1.report_errors.list_by_report_instance_id',
    list_report_error_versions_request: 'reporting.v1.report_errors_versions.list',
    get_report_error_version_request: 'reporting.v1.report_errors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_errors_request: true,
    get_report_error_request: true,
    get_many_report_errors_request: true,
    put_report_error_request: true,
    put_many_report_errors_request: true,
    delete_report_error_request: true,
    delete_many_report_errors_request: true,
    list_by_report_instance_id_report_errors_request: true,
    list_report_error_versions_request: true,
    get_report_error_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_errors_events.created',
    updated: 'reporting.v1.report_errors_events.updated',
    deleted: 'reporting.v1.report_errors_events.deleted',
} as const;

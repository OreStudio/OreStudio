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
import type { ReportErrorScope } from '../domain/report_error_scope.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportErrorScopeKey {
    code: string;
}

export interface ReportErrorScopeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ReportErrorScopeChange {
    write: ReportErrorScopeWrite;
    precondition: Precondition;
}

export interface ReportErrorScopeRemoval {
    key: ReportErrorScopeKey;
    precondition: Precondition;
}

export interface ReportErrorScopeLookup {
    key: ReportErrorScopeKey;
    report_error_scope: ReportErrorScope | null;
}

export interface ReportErrorScopesFilter {
    code_one_of: string[] | null;
}

export interface ReportErrorScopeEvent {
    event_id: string;
    key: ReportErrorScopeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportErrorScopeVersionKey {
    report_error_scope: ReportErrorScopeKey;
    version: number;
}

export interface ReportErrorScopeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportErrorScopesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorScopesFilter | null;
    as_of: string | null;
}

export interface ListReportErrorScopesResponse {
    result: Result;
    scopes: ReportErrorScope[];
    total: number;
}

export interface GetReportErrorScopeRequest {
    key: ReportErrorScopeKey;
}

export interface GetReportErrorScopeResponse {
    result: Result;
    report_error_scope: ReportErrorScope | null;
}

export interface GetManyReportErrorScopesRequest {
    keys: ReportErrorScopeKey[];
}

export interface GetManyReportErrorScopesResponse {
    result: Result;
    entries: ReportErrorScopeLookup[];
}

export interface PutReportErrorScopeRequest {
    change: ReportErrorScopeChange;
    intent: ChangeIntent;
}

export interface PutReportErrorScopeResponse {
    result: Result;
    report_error_scope: ReportErrorScope | null;
}

export interface PutManyReportErrorScopesRequest {
    changes: ReportErrorScopeChange[];
    intent: ChangeIntent;
}

export interface PutManyReportErrorScopesResponse {
    result: Result;
    scopes: ReportErrorScope[];
}

export interface DeleteReportErrorScopeRequest {
    removal: ReportErrorScopeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportErrorScopeResponse {
    result: Result;
}

export interface DeleteManyReportErrorScopesRequest {
    removals: ReportErrorScopeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportErrorScopesResponse {
    result: Result;
}

export interface ListReportErrorScopeVersionsRequest {
    key: ReportErrorScopeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorScopeVersionsFilter | null;
}

export interface ListReportErrorScopeVersionsResponse {
    result: Result;
    versions: ReportErrorScope[];
    total: number;
}

export interface GetReportErrorScopeVersionRequest {
    key: ReportErrorScopeVersionKey;
}

export interface GetReportErrorScopeVersionResponse {
    result: Result;
    version: ReportErrorScope | null;
}

export const subjects = {
    list_report_error_scopes_request: 'reporting.v1.report_error_scopes.list',
    get_report_error_scope_request: 'reporting.v1.report_error_scopes.get',
    get_many_report_error_scopes_request: 'reporting.v1.report_error_scopes.get_many',
    put_report_error_scope_request: 'reporting.v1.report_error_scopes.put',
    put_many_report_error_scopes_request: 'reporting.v1.report_error_scopes.put_many',
    delete_report_error_scope_request: 'reporting.v1.report_error_scopes.delete',
    delete_many_report_error_scopes_request: 'reporting.v1.report_error_scopes.delete_many',
    list_report_error_scope_versions_request: 'reporting.v1.report_error_scopes_versions.list',
    get_report_error_scope_version_request: 'reporting.v1.report_error_scopes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_error_scopes_request: true,
    get_report_error_scope_request: true,
    get_many_report_error_scopes_request: true,
    put_report_error_scope_request: true,
    put_many_report_error_scopes_request: true,
    delete_report_error_scope_request: true,
    delete_many_report_error_scopes_request: true,
    list_report_error_scope_versions_request: true,
    get_report_error_scope_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_error_scopes_events.created',
    updated: 'reporting.v1.report_error_scopes_events.updated',
    deleted: 'reporting.v1.report_error_scopes_events.deleted',
} as const;

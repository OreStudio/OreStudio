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
import type { ReportType } from '../domain/report_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportTypeKey {
    code: string;
}

export interface ReportTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ReportTypeChange {
    write: ReportTypeWrite;
    precondition: Precondition;
}

export interface ReportTypeRemoval {
    key: ReportTypeKey;
    precondition: Precondition;
}

export interface ReportTypeLookup {
    key: ReportTypeKey;
    report_type: ReportType | null;
}

export interface ReportTypeEvent {
    event_id: string;
    key: ReportTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportTypeVersionKey {
    report_type: ReportTypeKey;
    version: number;
}

export interface ReportTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListReportTypesResponse {
    result: Result;
    types: ReportType[];
    total: number;
}

export interface GetReportTypeRequest {
    key: ReportTypeKey;
}

export interface GetReportTypeResponse {
    result: Result;
    report_type: ReportType | null;
}

export interface GetManyReportTypesRequest {
    keys: ReportTypeKey[];
}

export interface GetManyReportTypesResponse {
    result: Result;
    entries: ReportTypeLookup[];
}

export interface PutReportTypeRequest {
    change: ReportTypeChange;
    intent: ChangeIntent;
}

export interface PutReportTypeResponse {
    result: Result;
    report_type: ReportType;
}

export interface PutManyReportTypesRequest {
    changes: ReportTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTypesResponse {
    result: Result;
    types: ReportType[];
}

export interface DeleteReportTypeRequest {
    removal: ReportTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTypeResponse {
    result: Result;
}

export interface DeleteManyReportTypesRequest {
    removals: ReportTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTypesResponse {
    result: Result;
}

export interface ListReportTypeVersionsRequest {
    key: ReportTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTypeVersionsFilter | null;
}

export interface ListReportTypeVersionsResponse {
    result: Result;
    versions: ReportType[];
    total: number;
}

export interface GetReportTypeVersionRequest {
    key: ReportTypeVersionKey;
}

export interface GetReportTypeVersionResponse {
    result: Result;
    version: ReportType;
}

export const subjects = {
    list_report_types_request: "reporting.v1.report_types.list",
    get_report_type_request: "reporting.v1.report_types.get",
    get_many_report_types_request: "reporting.v1.report_types.get_many",
    put_report_type_request: "reporting.v1.report_types.put",
    put_many_report_types_request: "reporting.v1.report_types.put_many",
    delete_report_type_request: "reporting.v1.report_types.delete",
    delete_many_report_types_request: "reporting.v1.report_types.delete_many",
    list_report_type_versions_request: "reporting.v1.report_types_versions.list",
    get_report_type_version_request: "reporting.v1.report_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_types_request: true,
    get_report_type_request: true,
    get_many_report_types_request: true,
    put_report_type_request: true,
    put_many_report_types_request: true,
    delete_report_type_request: true,
    delete_many_report_types_request: true,
    list_report_type_versions_request: true,
    get_report_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "reporting.v1.report_types_events.created",
    updated: "reporting.v1.report_types_events.updated",
    deleted: "reporting.v1.report_types_events.deleted",
} as const;

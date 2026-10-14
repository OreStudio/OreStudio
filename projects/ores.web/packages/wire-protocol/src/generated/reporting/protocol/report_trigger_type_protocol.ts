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
import type { ReportTriggerType } from '../domain/report_trigger_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportTriggerTypeKey {
    code: string;
}

export interface ReportTriggerTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ReportTriggerTypeChange {
    write: ReportTriggerTypeWrite;
    precondition: Precondition;
}

export interface ReportTriggerTypeRemoval {
    key: ReportTriggerTypeKey;
    precondition: Precondition;
}

export interface ReportTriggerTypeLookup {
    key: ReportTriggerTypeKey;
    report_trigger_type: ReportTriggerType | null;
}

export interface ReportTriggerTypesFilter {
    code_one_of: string[] | null;
}

export interface ReportTriggerTypeEvent {
    event_id: string;
    key: ReportTriggerTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportTriggerTypeVersionKey {
    report_trigger_type: ReportTriggerTypeKey;
    version: number;
}

export interface ReportTriggerTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportTriggerTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTriggerTypesFilter | null;
    as_of: string | null;
}

export interface ListReportTriggerTypesResponse {
    result: Result;
    types: ReportTriggerType[];
    total: number;
}

export interface GetReportTriggerTypeRequest {
    key: ReportTriggerTypeKey;
}

export interface GetReportTriggerTypeResponse {
    result: Result;
    report_trigger_type: ReportTriggerType | null;
}

export interface GetManyReportTriggerTypesRequest {
    keys: ReportTriggerTypeKey[];
}

export interface GetManyReportTriggerTypesResponse {
    result: Result;
    entries: ReportTriggerTypeLookup[];
}

export interface PutReportTriggerTypeRequest {
    change: ReportTriggerTypeChange;
    intent: ChangeIntent;
}

export interface PutReportTriggerTypeResponse {
    result: Result;
    report_trigger_type: ReportTriggerType | null;
}

export interface PutManyReportTriggerTypesRequest {
    changes: ReportTriggerTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTriggerTypesResponse {
    result: Result;
    types: ReportTriggerType[];
}

export interface DeleteReportTriggerTypeRequest {
    removal: ReportTriggerTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTriggerTypeResponse {
    result: Result;
}

export interface DeleteManyReportTriggerTypesRequest {
    removals: ReportTriggerTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTriggerTypesResponse {
    result: Result;
}

export interface ListReportTriggerTypeVersionsRequest {
    key: ReportTriggerTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTriggerTypeVersionsFilter | null;
}

export interface ListReportTriggerTypeVersionsResponse {
    result: Result;
    versions: ReportTriggerType[];
    total: number;
}

export interface GetReportTriggerTypeVersionRequest {
    key: ReportTriggerTypeVersionKey;
}

export interface GetReportTriggerTypeVersionResponse {
    result: Result;
    version: ReportTriggerType | null;
}

export const subjects = {
    list_report_trigger_types_request: 'reporting.v1.report_trigger_types.list',
    get_report_trigger_type_request: 'reporting.v1.report_trigger_types.get',
    get_many_report_trigger_types_request: 'reporting.v1.report_trigger_types.get_many',
    put_report_trigger_type_request: 'reporting.v1.report_trigger_types.put',
    put_many_report_trigger_types_request: 'reporting.v1.report_trigger_types.put_many',
    delete_report_trigger_type_request: 'reporting.v1.report_trigger_types.delete',
    delete_many_report_trigger_types_request: 'reporting.v1.report_trigger_types.delete_many',
    list_report_trigger_type_versions_request: 'reporting.v1.report_trigger_types_versions.list',
    get_report_trigger_type_version_request: 'reporting.v1.report_trigger_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_trigger_types_request: true,
    get_report_trigger_type_request: true,
    get_many_report_trigger_types_request: true,
    put_report_trigger_type_request: true,
    put_many_report_trigger_types_request: true,
    delete_report_trigger_type_request: true,
    delete_many_report_trigger_types_request: true,
    list_report_trigger_type_versions_request: true,
    get_report_trigger_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_trigger_types_events.created',
    updated: 'reporting.v1.report_trigger_types_events.updated',
    deleted: 'reporting.v1.report_trigger_types_events.deleted',
} as const;

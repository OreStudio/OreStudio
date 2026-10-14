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
import type { ReportInstanceKind } from '../domain/report_instance_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportInstanceKindKey {
    code: string;
}

export interface ReportInstanceKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ReportInstanceKindChange {
    write: ReportInstanceKindWrite;
    precondition: Precondition;
}

export interface ReportInstanceKindRemoval {
    key: ReportInstanceKindKey;
    precondition: Precondition;
}

export interface ReportInstanceKindLookup {
    key: ReportInstanceKindKey;
    report_instance_kind: ReportInstanceKind | null;
}

export interface ReportInstanceKindsFilter {
    code_one_of: string[] | null;
}

export interface ReportInstanceKindEvent {
    event_id: string;
    key: ReportInstanceKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportInstanceKindVersionKey {
    report_instance_kind: ReportInstanceKindKey;
    version: number;
}

export interface ReportInstanceKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportInstanceKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportInstanceKindsFilter | null;
    as_of: string | null;
}

export interface ListReportInstanceKindsResponse {
    result: Result;
    types: ReportInstanceKind[];
    total: number;
}

export interface GetReportInstanceKindRequest {
    key: ReportInstanceKindKey;
}

export interface GetReportInstanceKindResponse {
    result: Result;
    report_instance_kind: ReportInstanceKind | null;
}

export interface GetManyReportInstanceKindsRequest {
    keys: ReportInstanceKindKey[];
}

export interface GetManyReportInstanceKindsResponse {
    result: Result;
    entries: ReportInstanceKindLookup[];
}

export interface PutReportInstanceKindRequest {
    change: ReportInstanceKindChange;
    intent: ChangeIntent;
}

export interface PutReportInstanceKindResponse {
    result: Result;
    report_instance_kind: ReportInstanceKind | null;
}

export interface PutManyReportInstanceKindsRequest {
    changes: ReportInstanceKindChange[];
    intent: ChangeIntent;
}

export interface PutManyReportInstanceKindsResponse {
    result: Result;
    types: ReportInstanceKind[];
}

export interface DeleteReportInstanceKindRequest {
    removal: ReportInstanceKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportInstanceKindResponse {
    result: Result;
}

export interface DeleteManyReportInstanceKindsRequest {
    removals: ReportInstanceKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportInstanceKindsResponse {
    result: Result;
}

export interface ListReportInstanceKindVersionsRequest {
    key: ReportInstanceKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportInstanceKindVersionsFilter | null;
}

export interface ListReportInstanceKindVersionsResponse {
    result: Result;
    versions: ReportInstanceKind[];
    total: number;
}

export interface GetReportInstanceKindVersionRequest {
    key: ReportInstanceKindVersionKey;
}

export interface GetReportInstanceKindVersionResponse {
    result: Result;
    version: ReportInstanceKind | null;
}

export const subjects = {
    list_report_instance_kinds_request: 'reporting.v1.report_instance_kinds.list',
    get_report_instance_kind_request: 'reporting.v1.report_instance_kinds.get',
    get_many_report_instance_kinds_request: 'reporting.v1.report_instance_kinds.get_many',
    put_report_instance_kind_request: 'reporting.v1.report_instance_kinds.put',
    put_many_report_instance_kinds_request: 'reporting.v1.report_instance_kinds.put_many',
    delete_report_instance_kind_request: 'reporting.v1.report_instance_kinds.delete',
    delete_many_report_instance_kinds_request: 'reporting.v1.report_instance_kinds.delete_many',
    list_report_instance_kind_versions_request: 'reporting.v1.report_instance_kinds_versions.list',
    get_report_instance_kind_version_request: 'reporting.v1.report_instance_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_instance_kinds_request: true,
    get_report_instance_kind_request: true,
    get_many_report_instance_kinds_request: true,
    put_report_instance_kind_request: true,
    put_many_report_instance_kinds_request: true,
    delete_report_instance_kind_request: true,
    delete_many_report_instance_kinds_request: true,
    list_report_instance_kind_versions_request: true,
    get_report_instance_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_instance_kinds_events.created',
    updated: 'reporting.v1.report_instance_kinds_events.updated',
    deleted: 'reporting.v1.report_instance_kinds_events.deleted',
} as const;

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
import type { ReportInstance } from '../domain/report_instance.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportInstanceKey {
    name: string;
}

export interface ReportInstanceWrite {
    id: string;
    name: string;
    description: string;
    definition_id: string;
    fsm_state_id: string | null;
    trigger_run_id: number;
    output_message: string;
    started_at: string | null;
    completed_at: string | null;
}

export interface ReportInstanceChange {
    write: ReportInstanceWrite;
    precondition: Precondition;
}

export interface ReportInstanceRemoval {
    key: ReportInstanceKey;
    precondition: Precondition;
}

export interface ReportInstanceLookup {
    key: ReportInstanceKey;
    report_instance: ReportInstance | null;
}

export interface ReportInstanceEvent {
    event_id: string;
    key: ReportInstanceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportInstanceVersionKey {
    report_instance: ReportInstanceKey;
    version: number;
}

export interface ReportInstanceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportInstancesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListReportInstancesResponse {
    result: Result;
    instances: ReportInstance[];
    total: number;
}

export interface GetReportInstanceRequest {
    key: ReportInstanceKey;
}

export interface GetReportInstanceResponse {
    result: Result;
    report_instance: ReportInstance | null;
}

export interface GetManyReportInstancesRequest {
    keys: ReportInstanceKey[];
}

export interface GetManyReportInstancesResponse {
    result: Result;
    entries: ReportInstanceLookup[];
}

export interface PutReportInstanceRequest {
    change: ReportInstanceChange;
    intent: ChangeIntent;
}

export interface PutReportInstanceResponse {
    result: Result;
    report_instance: ReportInstance;
}

export interface PutManyReportInstancesRequest {
    changes: ReportInstanceChange[];
    intent: ChangeIntent;
}

export interface PutManyReportInstancesResponse {
    result: Result;
    instances: ReportInstance[];
}

export interface DeleteReportInstanceRequest {
    removal: ReportInstanceRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportInstanceResponse {
    result: Result;
}

export interface DeleteManyReportInstancesRequest {
    removals: ReportInstanceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportInstancesResponse {
    result: Result;
}

export interface ListReportInstanceVersionsRequest {
    key: ReportInstanceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportInstanceVersionsFilter | null;
}

export interface ListReportInstanceVersionsResponse {
    result: Result;
    versions: ReportInstance[];
    total: number;
}

export interface GetReportInstanceVersionRequest {
    key: ReportInstanceVersionKey;
}

export interface GetReportInstanceVersionResponse {
    result: Result;
    version: ReportInstance;
}

export const subjects = {
    list_report_instances_request: "reporting.v1.report_instances.list",
    get_report_instance_request: "reporting.v1.report_instances.get",
    get_many_report_instances_request: "reporting.v1.report_instances.get_many",
    put_report_instance_request: "reporting.v1.report_instances.put",
    put_many_report_instances_request: "reporting.v1.report_instances.put_many",
    delete_report_instance_request: "reporting.v1.report_instances.delete",
    delete_many_report_instances_request: "reporting.v1.report_instances.delete_many",
    list_report_instance_versions_request: "reporting.v1.report_instances_versions.list",
    get_report_instance_version_request: "reporting.v1.report_instances_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_instances_request: true,
    get_report_instance_request: true,
    get_many_report_instances_request: true,
    put_report_instance_request: true,
    put_many_report_instances_request: true,
    delete_report_instance_request: true,
    delete_many_report_instances_request: true,
    list_report_instance_versions_request: true,
    get_report_instance_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "reporting.v1.report_instances_events.created",
    updated: "reporting.v1.report_instances_events.updated",
    deleted: "reporting.v1.report_instances_events.deleted",
} as const;

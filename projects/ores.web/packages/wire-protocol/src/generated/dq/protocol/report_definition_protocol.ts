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
import type { ReportDefinition } from '../domain/report_definition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportDefinitionKey {
    id: string;
}

export interface ReportDefinitionWrite {
    id: string;
    name: string;
    description: string | null;
    report_type: string;
    schedule_expression: string;
    concurrency_policy: string;
    display_order: number;
}

export interface ReportDefinitionChange {
    write: ReportDefinitionWrite;
    precondition: Precondition;
}

export interface ReportDefinitionRemoval {
    key: ReportDefinitionKey;
    precondition: Precondition;
}

export interface ReportDefinitionLookup {
    key: ReportDefinitionKey;
    report_definition: ReportDefinition | null;
}

export interface ReportDefinitionEvent {
    event_id: string;
    key: ReportDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportDefinitionVersionKey {
    report_definition: ReportDefinitionKey;
    version: number;
}

export interface ReportDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListReportDefinitionsResponse {
    result: Result;
    definitions: ReportDefinition[];
    total: number;
}

export interface GetReportDefinitionRequest {
    key: ReportDefinitionKey;
}

export interface GetReportDefinitionResponse {
    result: Result;
    report_definition: ReportDefinition | null;
}

export interface GetManyReportDefinitionsRequest {
    keys: ReportDefinitionKey[];
}

export interface GetManyReportDefinitionsResponse {
    result: Result;
    entries: ReportDefinitionLookup[];
}

export interface PutReportDefinitionRequest {
    change: ReportDefinitionChange;
    intent: ChangeIntent;
}

export interface PutReportDefinitionResponse {
    result: Result;
    report_definition: ReportDefinition;
}

export interface PutManyReportDefinitionsRequest {
    changes: ReportDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyReportDefinitionsResponse {
    result: Result;
    definitions: ReportDefinition[];
}

export interface DeleteReportDefinitionRequest {
    removal: ReportDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportDefinitionResponse {
    result: Result;
}

export interface DeleteManyReportDefinitionsRequest {
    removals: ReportDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportDefinitionsResponse {
    result: Result;
}

export interface ListReportDefinitionVersionsRequest {
    key: ReportDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportDefinitionVersionsFilter | null;
}

export interface ListReportDefinitionVersionsResponse {
    result: Result;
    versions: ReportDefinition[];
    total: number;
}

export interface GetReportDefinitionVersionRequest {
    key: ReportDefinitionVersionKey;
}

export interface GetReportDefinitionVersionResponse {
    result: Result;
    version: ReportDefinition;
}

export const subjects = {
    list_report_definitions_request: "dq.v1.report_definitions.list",
    get_report_definition_request: "dq.v1.report_definitions.get",
    get_many_report_definitions_request: "dq.v1.report_definitions.get_many",
    put_report_definition_request: "dq.v1.report_definitions.put",
    put_many_report_definitions_request: "dq.v1.report_definitions.put_many",
    delete_report_definition_request: "dq.v1.report_definitions.delete",
    delete_many_report_definitions_request: "dq.v1.report_definitions.delete_many",
    list_report_definition_versions_request: "dq.v1.report_definitions_versions.list",
    get_report_definition_version_request: "dq.v1.report_definitions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_definitions_request: true,
    get_report_definition_request: true,
    get_many_report_definitions_request: true,
    put_report_definition_request: true,
    put_many_report_definitions_request: true,
    delete_report_definition_request: true,
    delete_many_report_definitions_request: true,
    list_report_definition_versions_request: true,
    get_report_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.report_definitions_events.created",
    updated: "dq.v1.report_definitions_events.updated",
    deleted: "dq.v1.report_definitions_events.deleted",
} as const;

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
import type { ReportAnalyticParameter } from '../domain/report_analytic_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportAnalyticParameterKey {
    id: string;
}

export interface ReportAnalyticParameterWrite {
    id: string;
    report_analytic_id: string;
    parameter_definition_id: string;
    value: string;
    position: number;
}

export interface ReportAnalyticParameterChange {
    write: ReportAnalyticParameterWrite;
    precondition: Precondition;
}

export interface ReportAnalyticParameterRemoval {
    key: ReportAnalyticParameterKey;
    precondition: Precondition;
}

export interface ReportAnalyticParameterLookup {
    key: ReportAnalyticParameterKey;
    report_analytic_parameter: ReportAnalyticParameter | null;
}

export interface ReportAnalyticParametersFilter {
    report_analytic_id: string | null;
    parameter_definition_id: string | null;
    id_one_of: string[] | null;
    report_analytic_id_one_of: string[] | null;
    parameter_definition_id_one_of: string[] | null;
}

export interface ReportAnalyticParameterEvent {
    event_id: string;
    key: ReportAnalyticParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportAnalyticParameterVersionKey {
    report_analytic_parameter: ReportAnalyticParameterKey;
    version: number;
}

export interface ReportAnalyticParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportAnalyticParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticParametersFilter | null;
    as_of: string | null;
}

export interface ListReportAnalyticParametersResponse {
    result: Result;
    parameter_values: ReportAnalyticParameter[];
    total: number;
}

export interface GetReportAnalyticParameterRequest {
    key: ReportAnalyticParameterKey;
}

export interface GetReportAnalyticParameterResponse {
    result: Result;
    report_analytic_parameter: ReportAnalyticParameter | null;
}

export interface GetManyReportAnalyticParametersRequest {
    keys: ReportAnalyticParameterKey[];
}

export interface GetManyReportAnalyticParametersResponse {
    result: Result;
    entries: ReportAnalyticParameterLookup[];
}

export interface PutReportAnalyticParameterRequest {
    change: ReportAnalyticParameterChange;
    intent: ChangeIntent;
}

export interface PutReportAnalyticParameterResponse {
    result: Result;
    report_analytic_parameter: ReportAnalyticParameter | null;
}

export interface PutManyReportAnalyticParametersRequest {
    changes: ReportAnalyticParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyReportAnalyticParametersResponse {
    result: Result;
    parameter_values: ReportAnalyticParameter[];
}

export interface DeleteReportAnalyticParameterRequest {
    removal: ReportAnalyticParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportAnalyticParameterResponse {
    result: Result;
}

export interface DeleteManyReportAnalyticParametersRequest {
    removals: ReportAnalyticParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportAnalyticParametersResponse {
    result: Result;
}

export interface ListByReportAnalyticIdReportAnalyticParametersRequest {
    report_analytic_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticParametersFilter | null;
}

export interface ListByReportAnalyticIdReportAnalyticParametersResponse {
    result: Result;
    parameter_values: ReportAnalyticParameter[];
    total: number;
}

export interface ListByParameterDefinitionIdReportAnalyticParametersRequest {
    parameter_definition_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticParametersFilter | null;
}

export interface ListByParameterDefinitionIdReportAnalyticParametersResponse {
    result: Result;
    parameter_values: ReportAnalyticParameter[];
    total: number;
}

export interface ListReportAnalyticParameterVersionsRequest {
    key: ReportAnalyticParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAnalyticParameterVersionsFilter | null;
}

export interface ListReportAnalyticParameterVersionsResponse {
    result: Result;
    versions: ReportAnalyticParameter[];
    total: number;
}

export interface GetReportAnalyticParameterVersionRequest {
    key: ReportAnalyticParameterVersionKey;
}

export interface GetReportAnalyticParameterVersionResponse {
    result: Result;
    version: ReportAnalyticParameter | null;
}

export const subjects = {
    list_report_analytic_parameters_request: 'reporting.v1.report_analytic_parameters.list',
    get_report_analytic_parameter_request: 'reporting.v1.report_analytic_parameters.get',
    get_many_report_analytic_parameters_request: 'reporting.v1.report_analytic_parameters.get_many',
    put_report_analytic_parameter_request: 'reporting.v1.report_analytic_parameters.put',
    put_many_report_analytic_parameters_request: 'reporting.v1.report_analytic_parameters.put_many',
    delete_report_analytic_parameter_request: 'reporting.v1.report_analytic_parameters.delete',
    delete_many_report_analytic_parameters_request:
        'reporting.v1.report_analytic_parameters.delete_many',
    list_by_report_analytic_id_report_analytic_parameters_request:
        'reporting.v1.report_analytic_parameters.list_by_report_analytic_id',
    list_by_parameter_definition_id_report_analytic_parameters_request:
        'reporting.v1.report_analytic_parameters.list_by_parameter_definition_id',
    list_report_analytic_parameter_versions_request:
        'reporting.v1.report_analytic_parameters_versions.list',
    get_report_analytic_parameter_version_request:
        'reporting.v1.report_analytic_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_analytic_parameters_request: true,
    get_report_analytic_parameter_request: true,
    get_many_report_analytic_parameters_request: true,
    put_report_analytic_parameter_request: true,
    put_many_report_analytic_parameters_request: true,
    delete_report_analytic_parameter_request: true,
    delete_many_report_analytic_parameters_request: true,
    list_by_report_analytic_id_report_analytic_parameters_request: true,
    list_by_parameter_definition_id_report_analytic_parameters_request: true,
    list_report_analytic_parameter_versions_request: true,
    get_report_analytic_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_analytic_parameters_events.created',
    updated: 'reporting.v1.report_analytic_parameters_events.updated',
    deleted: 'reporting.v1.report_analytic_parameters_events.deleted',
} as const;

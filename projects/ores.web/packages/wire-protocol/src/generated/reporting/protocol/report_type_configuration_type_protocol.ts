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
import type { ReportTypeConfigurationType } from '../domain/report_type_configuration_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportTypeConfigurationTypeKey {
    report_type_code: string;
    configuration_type_code: string;
}

export interface ReportTypeConfigurationTypeWrite {
    report_type_code: string;
    configuration_type_code: string;
}

export interface ReportTypeConfigurationTypeChange {
    write: ReportTypeConfigurationTypeWrite;
    precondition: Precondition;
}

export interface ReportTypeConfigurationTypeRemoval {
    key: ReportTypeConfigurationTypeKey;
    precondition: Precondition;
}

export interface ReportTypeConfigurationTypeLookup {
    key: ReportTypeConfigurationTypeKey;
    report_type_configuration_type: ReportTypeConfigurationType | null;
}

export interface ReportTypeConfigurationTypesFilter {
    report_type_code: string | null;
    report_type_code_one_of: string[] | null;
}

export interface ListReportTypeConfigurationTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTypeConfigurationTypesFilter | null;
}

export interface ListReportTypeConfigurationTypesResponse {
    result: Result;
    report_type_configuration_types: ReportTypeConfigurationType[];
    total: number;
}

export interface GetReportTypeConfigurationTypeRequest {
    key: ReportTypeConfigurationTypeKey;
}

export interface GetReportTypeConfigurationTypeResponse {
    result: Result;
    report_type_configuration_type: ReportTypeConfigurationType | null;
}

export interface GetManyReportTypeConfigurationTypesRequest {
    keys: ReportTypeConfigurationTypeKey[];
}

export interface GetManyReportTypeConfigurationTypesResponse {
    result: Result;
    entries: ReportTypeConfigurationTypeLookup[];
}

export interface PutReportTypeConfigurationTypeRequest {
    change: ReportTypeConfigurationTypeChange;
    intent: ChangeIntent;
}

export interface PutReportTypeConfigurationTypeResponse {
    result: Result;
    report_type_configuration_type: ReportTypeConfigurationType | null;
}

export interface PutManyReportTypeConfigurationTypesRequest {
    changes: ReportTypeConfigurationTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTypeConfigurationTypesResponse {
    result: Result;
    report_type_configuration_types: ReportTypeConfigurationType[];
}

export interface DeleteReportTypeConfigurationTypeRequest {
    removal: ReportTypeConfigurationTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTypeConfigurationTypeResponse {
    result: Result;
}

export interface DeleteManyReportTypeConfigurationTypesRequest {
    removals: ReportTypeConfigurationTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTypeConfigurationTypesResponse {
    result: Result;
}

export interface ListByReportTypeCodeReportTypeConfigurationTypesRequest {
    report_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTypeConfigurationTypesFilter | null;
}

export interface ListByReportTypeCodeReportTypeConfigurationTypesResponse {
    result: Result;
    report_type_configuration_types: ReportTypeConfigurationType[];
    total: number;
}

export const subjects = {
    list_report_type_configuration_types_request:
        'reporting.v1.report_type_configuration_types.list',
    get_report_type_configuration_type_request: 'reporting.v1.report_type_configuration_types.get',
    get_many_report_type_configuration_types_request:
        'reporting.v1.report_type_configuration_types.get_many',
    put_report_type_configuration_type_request: 'reporting.v1.report_type_configuration_types.put',
    put_many_report_type_configuration_types_request:
        'reporting.v1.report_type_configuration_types.put_many',
    delete_report_type_configuration_type_request:
        'reporting.v1.report_type_configuration_types.delete',
    delete_many_report_type_configuration_types_request:
        'reporting.v1.report_type_configuration_types.delete_many',
    list_by_report_type_code_report_type_configuration_types_request:
        'reporting.v1.report_type_configuration_types.list_by_report_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_type_configuration_types_request: true,
    get_report_type_configuration_type_request: true,
    get_many_report_type_configuration_types_request: true,
    put_report_type_configuration_type_request: true,
    put_many_report_type_configuration_types_request: true,
    delete_report_type_configuration_type_request: true,
    delete_many_report_type_configuration_types_request: true,
    list_by_report_type_code_report_type_configuration_types_request: true,
} as const;

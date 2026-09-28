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
import type { ReportConfiguration } from '../domain/report_configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportConfigurationKey {
    configuration_type_code: string;
}

export interface ReportConfigurationWrite {
    id: string;
    report_definition_id: string;
    configuration_type_code: string;
    configuration_id: string;
}

export interface ReportConfigurationChange {
    write: ReportConfigurationWrite;
    precondition: Precondition;
}

export interface ReportConfigurationRemoval {
    key: ReportConfigurationKey;
    precondition: Precondition;
}

export interface ReportConfigurationLookup {
    key: ReportConfigurationKey;
    report_configuration: ReportConfiguration | null;
}

export interface ReportConfigurationsFilter {
    report_definition_id: string | null;
    configuration_type_code: string | null;
    configuration_id: string | null;
}

export interface ReportConfigurationEvent {
    event_id: string;
    key: ReportConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportConfigurationVersionKey {
    report_configuration: ReportConfigurationKey;
    version: number;
}

export interface ReportConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportConfigurationsFilter | null;
}

export interface ListReportConfigurationsResponse {
    result: Result;
    report_configurations: ReportConfiguration[];
    total: number;
}

export interface GetReportConfigurationRequest {
    key: ReportConfigurationKey;
}

export interface GetReportConfigurationResponse {
    result: Result;
    report_configuration: ReportConfiguration | null;
}

export interface GetManyReportConfigurationsRequest {
    keys: ReportConfigurationKey[];
}

export interface GetManyReportConfigurationsResponse {
    result: Result;
    entries: ReportConfigurationLookup[];
}

export interface PutReportConfigurationRequest {
    change: ReportConfigurationChange;
    intent: ChangeIntent;
}

export interface PutReportConfigurationResponse {
    result: Result;
    report_configuration: ReportConfiguration | null;
}

export interface PutManyReportConfigurationsRequest {
    changes: ReportConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyReportConfigurationsResponse {
    result: Result;
    report_configurations: ReportConfiguration[];
}

export interface DeleteReportConfigurationRequest {
    removal: ReportConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportConfigurationResponse {
    result: Result;
}

export interface DeleteManyReportConfigurationsRequest {
    removals: ReportConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportConfigurationsResponse {
    result: Result;
}

export interface ListByReportDefinitionIdReportConfigurationsRequest {
    report_definition_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportConfigurationsFilter | null;
}

export interface ListByReportDefinitionIdReportConfigurationsResponse {
    result: Result;
    report_configurations: ReportConfiguration[];
    total: number;
}

export interface ListByConfigurationTypeCodeReportConfigurationsRequest {
    configuration_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportConfigurationsFilter | null;
}

export interface ListByConfigurationTypeCodeReportConfigurationsResponse {
    result: Result;
    report_configurations: ReportConfiguration[];
    total: number;
}

export interface ListByConfigurationIdReportConfigurationsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportConfigurationsFilter | null;
}

export interface ListByConfigurationIdReportConfigurationsResponse {
    result: Result;
    report_configurations: ReportConfiguration[];
    total: number;
}

export interface ListReportConfigurationVersionsRequest {
    key: ReportConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportConfigurationVersionsFilter | null;
}

export interface ListReportConfigurationVersionsResponse {
    result: Result;
    versions: ReportConfiguration[];
    total: number;
}

export interface GetReportConfigurationVersionRequest {
    key: ReportConfigurationVersionKey;
}

export interface GetReportConfigurationVersionResponse {
    result: Result;
    version: ReportConfiguration | null;
}

export const subjects = {
    list_report_configurations_request: 'reporting.v1.report_configurations.list',
    get_report_configuration_request: 'reporting.v1.report_configurations.get',
    get_many_report_configurations_request: 'reporting.v1.report_configurations.get_many',
    put_report_configuration_request: 'reporting.v1.report_configurations.put',
    put_many_report_configurations_request: 'reporting.v1.report_configurations.put_many',
    delete_report_configuration_request: 'reporting.v1.report_configurations.delete',
    delete_many_report_configurations_request: 'reporting.v1.report_configurations.delete_many',
    list_by_report_definition_id_report_configurations_request:
        'reporting.v1.report_configurations.list_by_report_definition_id',
    list_by_configuration_type_code_report_configurations_request:
        'reporting.v1.report_configurations.list_by_configuration_type_code',
    list_by_configuration_id_report_configurations_request:
        'reporting.v1.report_configurations.list_by_configuration_id',
    list_report_configuration_versions_request: 'reporting.v1.report_configurations_versions.list',
    get_report_configuration_version_request: 'reporting.v1.report_configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_configurations_request: true,
    get_report_configuration_request: true,
    get_many_report_configurations_request: true,
    put_report_configuration_request: true,
    put_many_report_configurations_request: true,
    delete_report_configuration_request: true,
    delete_many_report_configurations_request: true,
    list_by_report_definition_id_report_configurations_request: true,
    list_by_configuration_type_code_report_configurations_request: true,
    list_by_configuration_id_report_configurations_request: true,
    list_report_configuration_versions_request: true,
    get_report_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_configurations_events.created',
    updated: 'reporting.v1.report_configurations_events.updated',
    deleted: 'reporting.v1.report_configurations_events.deleted',
} as const;

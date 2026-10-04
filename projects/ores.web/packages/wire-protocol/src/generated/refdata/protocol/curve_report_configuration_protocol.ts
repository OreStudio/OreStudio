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
import type { CurveReportConfiguration } from '../domain/curve_report_configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveReportConfigurationKey {
    id: string;
}

export interface CurveReportConfigurationWrite {
    id: string;
    curve_definition_id: string;
    report_on_delta_grid: string | null;
    report_on_moneyness_grid: string | null;
    report_on_strike_grid: string | null;
    report_on_strike_spread_grid: string | null;
    deltas: string | null;
    moneyness: string | null;
    strikes: string | null;
    strike_spreads: string | null;
    expiries: string | null;
    pillar_dates: string | null;
    underlying_tenors: string | null;
    continuation_expiry: string | null;
}

export interface CurveReportConfigurationChange {
    write: CurveReportConfigurationWrite;
    precondition: Precondition;
}

export interface CurveReportConfigurationRemoval {
    key: CurveReportConfigurationKey;
    precondition: Precondition;
}

export interface CurveReportConfigurationLookup {
    key: CurveReportConfigurationKey;
    curve_report_configuration: CurveReportConfiguration | null;
}

export interface CurveReportConfigurationsFilter {
    id_one_of: string[] | null;
}

export interface CurveReportConfigurationEvent {
    event_id: string;
    key: CurveReportConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveReportConfigurationVersionKey {
    curve_report_configuration: CurveReportConfigurationKey;
    version: number;
}

export interface CurveReportConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveReportConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveReportConfigurationsFilter | null;
}

export interface ListCurveReportConfigurationsResponse {
    result: Result;
    report_configurations: CurveReportConfiguration[];
    total: number;
}

export interface GetCurveReportConfigurationRequest {
    key: CurveReportConfigurationKey;
}

export interface GetCurveReportConfigurationResponse {
    result: Result;
    curve_report_configuration: CurveReportConfiguration | null;
}

export interface GetManyCurveReportConfigurationsRequest {
    keys: CurveReportConfigurationKey[];
}

export interface GetManyCurveReportConfigurationsResponse {
    result: Result;
    entries: CurveReportConfigurationLookup[];
}

export interface PutCurveReportConfigurationRequest {
    change: CurveReportConfigurationChange;
    intent: ChangeIntent;
}

export interface PutCurveReportConfigurationResponse {
    result: Result;
    curve_report_configuration: CurveReportConfiguration | null;
}

export interface PutManyCurveReportConfigurationsRequest {
    changes: CurveReportConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveReportConfigurationsResponse {
    result: Result;
    report_configurations: CurveReportConfiguration[];
}

export interface DeleteCurveReportConfigurationRequest {
    removal: CurveReportConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveReportConfigurationResponse {
    result: Result;
}

export interface DeleteManyCurveReportConfigurationsRequest {
    removals: CurveReportConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveReportConfigurationsResponse {
    result: Result;
}

export interface ListCurveReportConfigurationVersionsRequest {
    key: CurveReportConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveReportConfigurationVersionsFilter | null;
}

export interface ListCurveReportConfigurationVersionsResponse {
    result: Result;
    versions: CurveReportConfiguration[];
    total: number;
}

export interface GetCurveReportConfigurationVersionRequest {
    key: CurveReportConfigurationVersionKey;
}

export interface GetCurveReportConfigurationVersionResponse {
    result: Result;
    version: CurveReportConfiguration | null;
}

export const subjects = {
    list_curve_report_configurations_request: 'refdata.v1.curve_report_configurations.list',
    get_curve_report_configuration_request: 'refdata.v1.curve_report_configurations.get',
    get_many_curve_report_configurations_request: 'refdata.v1.curve_report_configurations.get_many',
    put_curve_report_configuration_request: 'refdata.v1.curve_report_configurations.put',
    put_many_curve_report_configurations_request: 'refdata.v1.curve_report_configurations.put_many',
    delete_curve_report_configuration_request: 'refdata.v1.curve_report_configurations.delete',
    delete_many_curve_report_configurations_request:
        'refdata.v1.curve_report_configurations.delete_many',
    list_curve_report_configuration_versions_request:
        'refdata.v1.curve_report_configurations_versions.list',
    get_curve_report_configuration_version_request:
        'refdata.v1.curve_report_configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_report_configurations_request: true,
    get_curve_report_configuration_request: true,
    get_many_curve_report_configurations_request: true,
    put_curve_report_configuration_request: true,
    put_many_curve_report_configurations_request: true,
    delete_curve_report_configuration_request: true,
    delete_many_curve_report_configurations_request: true,
    list_curve_report_configuration_versions_request: true,
    get_curve_report_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_report_configurations_events.created',
    updated: 'refdata.v1.curve_report_configurations_events.updated',
    deleted: 'refdata.v1.curve_report_configurations_events.deleted',
} as const;

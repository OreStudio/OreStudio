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
import type { ConfigurationAnalytic } from '../domain/configuration_analytic.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConfigurationAnalyticKey {
    id: string;
}

export interface ConfigurationAnalyticWrite {
    id: string;
    configuration_id: string;
    analytic_type_code: string;
    display_order: number;
    active: string;
}

export interface ConfigurationAnalyticChange {
    write: ConfigurationAnalyticWrite;
    precondition: Precondition;
}

export interface ConfigurationAnalyticRemoval {
    key: ConfigurationAnalyticKey;
    precondition: Precondition;
}

export interface ConfigurationAnalyticLookup {
    key: ConfigurationAnalyticKey;
    configuration_analytic: ConfigurationAnalytic | null;
}

export interface ConfigurationAnalyticsFilter {
    configuration_id: string | null;
    analytic_type_code: string | null;
    id_one_of: string[] | null;
    configuration_id_one_of: string[] | null;
    analytic_type_code_one_of: string[] | null;
}

export interface ConfigurationAnalyticEvent {
    event_id: string;
    key: ConfigurationAnalyticKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationAnalyticVersionKey {
    configuration_analytic: ConfigurationAnalyticKey;
    version: number;
}

export interface ConfigurationAnalyticVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationAnalyticsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticsFilter | null;
    as_of: string | null;
}

export interface ListConfigurationAnalyticsResponse {
    result: Result;
    analytics: ConfigurationAnalytic[];
    total: number;
}

export interface GetConfigurationAnalyticRequest {
    key: ConfigurationAnalyticKey;
}

export interface GetConfigurationAnalyticResponse {
    result: Result;
    configuration_analytic: ConfigurationAnalytic | null;
}

export interface GetManyConfigurationAnalyticsRequest {
    keys: ConfigurationAnalyticKey[];
}

export interface GetManyConfigurationAnalyticsResponse {
    result: Result;
    entries: ConfigurationAnalyticLookup[];
}

export interface PutConfigurationAnalyticRequest {
    change: ConfigurationAnalyticChange;
    intent: ChangeIntent;
}

export interface PutConfigurationAnalyticResponse {
    result: Result;
    configuration_analytic: ConfigurationAnalytic | null;
}

export interface PutManyConfigurationAnalyticsRequest {
    changes: ConfigurationAnalyticChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationAnalyticsResponse {
    result: Result;
    analytics: ConfigurationAnalytic[];
}

export interface DeleteConfigurationAnalyticRequest {
    removal: ConfigurationAnalyticRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationAnalyticResponse {
    result: Result;
}

export interface DeleteManyConfigurationAnalyticsRequest {
    removals: ConfigurationAnalyticRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationAnalyticsResponse {
    result: Result;
}

export interface ListByConfigurationIdConfigurationAnalyticsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticsFilter | null;
}

export interface ListByConfigurationIdConfigurationAnalyticsResponse {
    result: Result;
    analytics: ConfigurationAnalytic[];
    total: number;
}

export interface ListByAnalyticTypeCodeConfigurationAnalyticsRequest {
    analytic_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticsFilter | null;
}

export interface ListByAnalyticTypeCodeConfigurationAnalyticsResponse {
    result: Result;
    analytics: ConfigurationAnalytic[];
    total: number;
}

export interface ListConfigurationAnalyticVersionsRequest {
    key: ConfigurationAnalyticKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticVersionsFilter | null;
}

export interface ListConfigurationAnalyticVersionsResponse {
    result: Result;
    versions: ConfigurationAnalytic[];
    total: number;
}

export interface GetConfigurationAnalyticVersionRequest {
    key: ConfigurationAnalyticVersionKey;
}

export interface GetConfigurationAnalyticVersionResponse {
    result: Result;
    version: ConfigurationAnalytic | null;
}

export const subjects = {
    list_configuration_analytics_request: 'reporting.v1.configuration_analytics.list',
    get_configuration_analytic_request: 'reporting.v1.configuration_analytics.get',
    get_many_configuration_analytics_request: 'reporting.v1.configuration_analytics.get_many',
    put_configuration_analytic_request: 'reporting.v1.configuration_analytics.put',
    put_many_configuration_analytics_request: 'reporting.v1.configuration_analytics.put_many',
    delete_configuration_analytic_request: 'reporting.v1.configuration_analytics.delete',
    delete_many_configuration_analytics_request: 'reporting.v1.configuration_analytics.delete_many',
    list_by_configuration_id_configuration_analytics_request:
        'reporting.v1.configuration_analytics.list_by_configuration_id',
    list_by_analytic_type_code_configuration_analytics_request:
        'reporting.v1.configuration_analytics.list_by_analytic_type_code',
    list_configuration_analytic_versions_request:
        'reporting.v1.configuration_analytics_versions.list',
    get_configuration_analytic_version_request: 'reporting.v1.configuration_analytics_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configuration_analytics_request: true,
    get_configuration_analytic_request: true,
    get_many_configuration_analytics_request: true,
    put_configuration_analytic_request: true,
    put_many_configuration_analytics_request: true,
    delete_configuration_analytic_request: true,
    delete_many_configuration_analytics_request: true,
    list_by_configuration_id_configuration_analytics_request: true,
    list_by_analytic_type_code_configuration_analytics_request: true,
    list_configuration_analytic_versions_request: true,
    get_configuration_analytic_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configuration_analytics_events.created',
    updated: 'reporting.v1.configuration_analytics_events.updated',
    deleted: 'reporting.v1.configuration_analytics_events.deleted',
} as const;

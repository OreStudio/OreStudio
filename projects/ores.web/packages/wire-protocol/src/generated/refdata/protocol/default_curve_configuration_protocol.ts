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
import type { DefaultCurveConfiguration } from '../domain/default_curve_configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveConfigurationKey {
    id: string;
}

export interface DefaultCurveConfigurationWrite {
    id: string;
    curve_definition_id: string;
    is_inline: boolean;
    priority: number | null;
    default_curve_type: string | null;
    discount_curve: string | null;
    day_counter: string | null;
    recovery_rate: string | null;
    start_date: string | null;
    has_quotes: boolean;
    benchmark_curve: string | null;
    reinterpreted_yield_curve: string | null;
    source_curve: string | null;
    pillars: string | null;
    spot_lag: number | null;
    calendar: string | null;
    conventions: string | null;
    extrapolation: string | null;
    running_spread: number | null;
    index_term: string | null;
    imply_default_from_market: string | null;
    allow_negative_rates: string | null;
    price_is_upfront: string | null;
    initial_state: string | null;
    states: string | null;
    position: number;
}

export interface DefaultCurveConfigurationChange {
    write: DefaultCurveConfigurationWrite;
    precondition: Precondition;
}

export interface DefaultCurveConfigurationRemoval {
    key: DefaultCurveConfigurationKey;
    precondition: Precondition;
}

export interface DefaultCurveConfigurationLookup {
    key: DefaultCurveConfigurationKey;
    default_curve_configuration: DefaultCurveConfiguration | null;
}

export interface DefaultCurveConfigurationsFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveConfigurationEvent {
    event_id: string;
    key: DefaultCurveConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveConfigurationVersionKey {
    default_curve_configuration: DefaultCurveConfigurationKey;
    version: number;
}

export interface DefaultCurveConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveConfigurationsFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveConfigurationsResponse {
    result: Result;
    configurations: DefaultCurveConfiguration[];
    total: number;
}

export interface GetDefaultCurveConfigurationRequest {
    key: DefaultCurveConfigurationKey;
}

export interface GetDefaultCurveConfigurationResponse {
    result: Result;
    default_curve_configuration: DefaultCurveConfiguration | null;
}

export interface GetManyDefaultCurveConfigurationsRequest {
    keys: DefaultCurveConfigurationKey[];
}

export interface GetManyDefaultCurveConfigurationsResponse {
    result: Result;
    entries: DefaultCurveConfigurationLookup[];
}

export interface PutDefaultCurveConfigurationRequest {
    change: DefaultCurveConfigurationChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveConfigurationResponse {
    result: Result;
    default_curve_configuration: DefaultCurveConfiguration | null;
}

export interface PutManyDefaultCurveConfigurationsRequest {
    changes: DefaultCurveConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveConfigurationsResponse {
    result: Result;
    configurations: DefaultCurveConfiguration[];
}

export interface DeleteDefaultCurveConfigurationRequest {
    removal: DefaultCurveConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveConfigurationResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveConfigurationsRequest {
    removals: DefaultCurveConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveConfigurationsResponse {
    result: Result;
}

export interface ListDefaultCurveConfigurationVersionsRequest {
    key: DefaultCurveConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveConfigurationVersionsFilter | null;
}

export interface ListDefaultCurveConfigurationVersionsResponse {
    result: Result;
    versions: DefaultCurveConfiguration[];
    total: number;
}

export interface GetDefaultCurveConfigurationVersionRequest {
    key: DefaultCurveConfigurationVersionKey;
}

export interface GetDefaultCurveConfigurationVersionResponse {
    result: Result;
    version: DefaultCurveConfiguration | null;
}

export const subjects = {
    list_default_curve_configurations_request: 'refdata.v1.default_curve_configurations.list',
    get_default_curve_configuration_request: 'refdata.v1.default_curve_configurations.get',
    get_many_default_curve_configurations_request:
        'refdata.v1.default_curve_configurations.get_many',
    put_default_curve_configuration_request: 'refdata.v1.default_curve_configurations.put',
    put_many_default_curve_configurations_request:
        'refdata.v1.default_curve_configurations.put_many',
    delete_default_curve_configuration_request: 'refdata.v1.default_curve_configurations.delete',
    delete_many_default_curve_configurations_request:
        'refdata.v1.default_curve_configurations.delete_many',
    list_default_curve_configuration_versions_request:
        'refdata.v1.default_curve_configurations_versions.list',
    get_default_curve_configuration_version_request:
        'refdata.v1.default_curve_configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_configurations_request: true,
    get_default_curve_configuration_request: true,
    get_many_default_curve_configurations_request: true,
    put_default_curve_configuration_request: true,
    put_many_default_curve_configurations_request: true,
    delete_default_curve_configuration_request: true,
    delete_many_default_curve_configurations_request: true,
    list_default_curve_configuration_versions_request: true,
    get_default_curve_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curve_configurations_events.created',
    updated: 'refdata.v1.default_curve_configurations_events.updated',
    deleted: 'refdata.v1.default_curve_configurations_events.deleted',
} as const;

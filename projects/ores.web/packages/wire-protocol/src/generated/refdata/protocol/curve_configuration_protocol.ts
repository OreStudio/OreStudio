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
import type { CurveConfiguration } from '../domain/curve_configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveConfigurationKey {
    id: string;
}

export interface CurveConfigurationWrite {
    id: string;
    name: string;
    description: string | null;
    configuration_id: string;
}

export interface CurveConfigurationChange {
    write: CurveConfigurationWrite;
    precondition: Precondition;
}

export interface CurveConfigurationRemoval {
    key: CurveConfigurationKey;
    precondition: Precondition;
}

export interface CurveConfigurationLookup {
    key: CurveConfigurationKey;
    curve_configuration: CurveConfiguration | null;
}

export interface CurveConfigurationsFilter {
    id_one_of: string[] | null;
}

export interface CurveConfigurationEvent {
    event_id: string;
    key: CurveConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveConfigurationVersionKey {
    curve_configuration: CurveConfigurationKey;
    version: number;
}

export interface CurveConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveConfigurationsFilter | null;
    as_of: string | null;
}

export interface ListCurveConfigurationsResponse {
    result: Result;
    configurations: CurveConfiguration[];
    total: number;
}

export interface GetCurveConfigurationRequest {
    key: CurveConfigurationKey;
}

export interface GetCurveConfigurationResponse {
    result: Result;
    curve_configuration: CurveConfiguration | null;
}

export interface GetManyCurveConfigurationsRequest {
    keys: CurveConfigurationKey[];
}

export interface GetManyCurveConfigurationsResponse {
    result: Result;
    entries: CurveConfigurationLookup[];
}

export interface PutCurveConfigurationRequest {
    change: CurveConfigurationChange;
    intent: ChangeIntent;
}

export interface PutCurveConfigurationResponse {
    result: Result;
    curve_configuration: CurveConfiguration | null;
}

export interface PutManyCurveConfigurationsRequest {
    changes: CurveConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveConfigurationsResponse {
    result: Result;
    configurations: CurveConfiguration[];
}

export interface DeleteCurveConfigurationRequest {
    removal: CurveConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveConfigurationResponse {
    result: Result;
}

export interface DeleteManyCurveConfigurationsRequest {
    removals: CurveConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveConfigurationsResponse {
    result: Result;
}

export interface ListCurveConfigurationVersionsRequest {
    key: CurveConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveConfigurationVersionsFilter | null;
}

export interface ListCurveConfigurationVersionsResponse {
    result: Result;
    versions: CurveConfiguration[];
    total: number;
}

export interface GetCurveConfigurationVersionRequest {
    key: CurveConfigurationVersionKey;
}

export interface GetCurveConfigurationVersionResponse {
    result: Result;
    version: CurveConfiguration | null;
}

export const subjects = {
    list_curve_configurations_request: 'refdata.v1.curve_configurations.list',
    get_curve_configuration_request: 'refdata.v1.curve_configurations.get',
    get_many_curve_configurations_request: 'refdata.v1.curve_configurations.get_many',
    put_curve_configuration_request: 'refdata.v1.curve_configurations.put',
    put_many_curve_configurations_request: 'refdata.v1.curve_configurations.put_many',
    delete_curve_configuration_request: 'refdata.v1.curve_configurations.delete',
    delete_many_curve_configurations_request: 'refdata.v1.curve_configurations.delete_many',
    list_curve_configuration_versions_request: 'refdata.v1.curve_configurations_versions.list',
    get_curve_configuration_version_request: 'refdata.v1.curve_configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_configurations_request: true,
    get_curve_configuration_request: true,
    get_many_curve_configurations_request: true,
    put_curve_configuration_request: true,
    put_many_curve_configurations_request: true,
    delete_curve_configuration_request: true,
    delete_many_curve_configurations_request: true,
    list_curve_configuration_versions_request: true,
    get_curve_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_configurations_events.created',
    updated: 'refdata.v1.curve_configurations_events.updated',
    deleted: 'refdata.v1.curve_configurations_events.deleted',
} as const;

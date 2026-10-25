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
import type { OreParameterValueSetValue } from '../domain/ore_parameter_value_set_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreParameterValueSetValueKey {
    code: string;
}

export interface OreParameterValueSetValueWrite {
    code: string;
    value_set_code: string;
    value: string;
    label: string;
    description: string;
    display_order: number;
    is_active: boolean;
}

export interface OreParameterValueSetValueChange {
    write: OreParameterValueSetValueWrite;
    precondition: Precondition;
}

export interface OreParameterValueSetValueRemoval {
    key: OreParameterValueSetValueKey;
    precondition: Precondition;
}

export interface OreParameterValueSetValueLookup {
    key: OreParameterValueSetValueKey;
    ore_parameter_value_set_value: OreParameterValueSetValue | null;
}

export interface OreParameterValueSetValuesFilter {
    code_one_of: string[] | null;
}

export interface OreParameterValueSetValueEvent {
    event_id: string;
    key: OreParameterValueSetValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterValueSetValueVersionKey {
    ore_parameter_value_set_value: OreParameterValueSetValueKey;
    version: number;
}

export interface OreParameterValueSetValueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParameterValueSetValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetValuesFilter | null;
    as_of: string | null;
}

export interface ListOreParameterValueSetValuesResponse {
    result: Result;
    values: OreParameterValueSetValue[];
    total: number;
}

export interface GetOreParameterValueSetValueRequest {
    key: OreParameterValueSetValueKey;
}

export interface GetOreParameterValueSetValueResponse {
    result: Result;
    ore_parameter_value_set_value: OreParameterValueSetValue | null;
}

export interface GetManyOreParameterValueSetValuesRequest {
    keys: OreParameterValueSetValueKey[];
}

export interface GetManyOreParameterValueSetValuesResponse {
    result: Result;
    entries: OreParameterValueSetValueLookup[];
}

export interface PutOreParameterValueSetValueRequest {
    change: OreParameterValueSetValueChange;
    intent: ChangeIntent;
}

export interface PutOreParameterValueSetValueResponse {
    result: Result;
    ore_parameter_value_set_value: OreParameterValueSetValue | null;
}

export interface PutManyOreParameterValueSetValuesRequest {
    changes: OreParameterValueSetValueChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParameterValueSetValuesResponse {
    result: Result;
    values: OreParameterValueSetValue[];
}

export interface DeleteOreParameterValueSetValueRequest {
    removal: OreParameterValueSetValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterValueSetValueResponse {
    result: Result;
}

export interface DeleteManyOreParameterValueSetValuesRequest {
    removals: OreParameterValueSetValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParameterValueSetValuesResponse {
    result: Result;
}

export interface ListOreParameterValueSetValueVersionsRequest {
    key: OreParameterValueSetValueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetValueVersionsFilter | null;
}

export interface ListOreParameterValueSetValueVersionsResponse {
    result: Result;
    versions: OreParameterValueSetValue[];
    total: number;
}

export interface GetOreParameterValueSetValueVersionRequest {
    key: OreParameterValueSetValueVersionKey;
}

export interface GetOreParameterValueSetValueVersionResponse {
    result: Result;
    version: OreParameterValueSetValue | null;
}

export const subjects = {
    list_ore_parameter_value_set_values_request: 'reporting.v1.ore_parameter_value_set_values.list',
    get_ore_parameter_value_set_value_request: 'reporting.v1.ore_parameter_value_set_values.get',
    get_many_ore_parameter_value_set_values_request:
        'reporting.v1.ore_parameter_value_set_values.get_many',
    put_ore_parameter_value_set_value_request: 'reporting.v1.ore_parameter_value_set_values.put',
    put_many_ore_parameter_value_set_values_request:
        'reporting.v1.ore_parameter_value_set_values.put_many',
    delete_ore_parameter_value_set_value_request:
        'reporting.v1.ore_parameter_value_set_values.delete',
    delete_many_ore_parameter_value_set_values_request:
        'reporting.v1.ore_parameter_value_set_values.delete_many',
    list_ore_parameter_value_set_value_versions_request:
        'reporting.v1.ore_parameter_value_set_values_versions.list',
    get_ore_parameter_value_set_value_version_request:
        'reporting.v1.ore_parameter_value_set_values_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameter_value_set_values_request: true,
    get_ore_parameter_value_set_value_request: true,
    get_many_ore_parameter_value_set_values_request: true,
    put_ore_parameter_value_set_value_request: true,
    put_many_ore_parameter_value_set_values_request: true,
    delete_ore_parameter_value_set_value_request: true,
    delete_many_ore_parameter_value_set_values_request: true,
    list_ore_parameter_value_set_value_versions_request: true,
    get_ore_parameter_value_set_value_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameter_value_set_values_events.created',
    updated: 'reporting.v1.ore_parameter_value_set_values_events.updated',
    deleted: 'reporting.v1.ore_parameter_value_set_values_events.deleted',
} as const;

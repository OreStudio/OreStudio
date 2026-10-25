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
import type { OreParameterSettingValueSet } from '../domain/ore_parameter_setting_value_set.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreParameterSettingValueSetKey {
    code: string;
}

export interface OreParameterSettingValueSetWrite {
    code: string;
    scope: string;
    name: string;
    value_set_code: string;
}

export interface OreParameterSettingValueSetChange {
    write: OreParameterSettingValueSetWrite;
    precondition: Precondition;
}

export interface OreParameterSettingValueSetRemoval {
    key: OreParameterSettingValueSetKey;
    precondition: Precondition;
}

export interface OreParameterSettingValueSetLookup {
    key: OreParameterSettingValueSetKey;
    ore_parameter_setting_value_set: OreParameterSettingValueSet | null;
}

export interface OreParameterSettingValueSetsFilter {
    code_one_of: string[] | null;
}

export interface OreParameterSettingValueSetEvent {
    event_id: string;
    key: OreParameterSettingValueSetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterSettingValueSetVersionKey {
    ore_parameter_setting_value_set: OreParameterSettingValueSetKey;
    version: number;
}

export interface OreParameterSettingValueSetVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParameterSettingValueSetsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterSettingValueSetsFilter | null;
    as_of: string | null;
}

export interface ListOreParameterSettingValueSetsResponse {
    result: Result;
    setting_value_sets: OreParameterSettingValueSet[];
    total: number;
}

export interface GetOreParameterSettingValueSetRequest {
    key: OreParameterSettingValueSetKey;
}

export interface GetOreParameterSettingValueSetResponse {
    result: Result;
    ore_parameter_setting_value_set: OreParameterSettingValueSet | null;
}

export interface GetManyOreParameterSettingValueSetsRequest {
    keys: OreParameterSettingValueSetKey[];
}

export interface GetManyOreParameterSettingValueSetsResponse {
    result: Result;
    entries: OreParameterSettingValueSetLookup[];
}

export interface PutOreParameterSettingValueSetRequest {
    change: OreParameterSettingValueSetChange;
    intent: ChangeIntent;
}

export interface PutOreParameterSettingValueSetResponse {
    result: Result;
    ore_parameter_setting_value_set: OreParameterSettingValueSet | null;
}

export interface PutManyOreParameterSettingValueSetsRequest {
    changes: OreParameterSettingValueSetChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParameterSettingValueSetsResponse {
    result: Result;
    setting_value_sets: OreParameterSettingValueSet[];
}

export interface DeleteOreParameterSettingValueSetRequest {
    removal: OreParameterSettingValueSetRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterSettingValueSetResponse {
    result: Result;
}

export interface DeleteManyOreParameterSettingValueSetsRequest {
    removals: OreParameterSettingValueSetRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParameterSettingValueSetsResponse {
    result: Result;
}

export interface ListOreParameterSettingValueSetVersionsRequest {
    key: OreParameterSettingValueSetKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterSettingValueSetVersionsFilter | null;
}

export interface ListOreParameterSettingValueSetVersionsResponse {
    result: Result;
    versions: OreParameterSettingValueSet[];
    total: number;
}

export interface GetOreParameterSettingValueSetVersionRequest {
    key: OreParameterSettingValueSetVersionKey;
}

export interface GetOreParameterSettingValueSetVersionResponse {
    result: Result;
    version: OreParameterSettingValueSet | null;
}

export const subjects = {
    list_ore_parameter_setting_value_sets_request:
        'reporting.v1.ore_parameter_setting_value_sets.list',
    get_ore_parameter_setting_value_set_request:
        'reporting.v1.ore_parameter_setting_value_sets.get',
    get_many_ore_parameter_setting_value_sets_request:
        'reporting.v1.ore_parameter_setting_value_sets.get_many',
    put_ore_parameter_setting_value_set_request:
        'reporting.v1.ore_parameter_setting_value_sets.put',
    put_many_ore_parameter_setting_value_sets_request:
        'reporting.v1.ore_parameter_setting_value_sets.put_many',
    delete_ore_parameter_setting_value_set_request:
        'reporting.v1.ore_parameter_setting_value_sets.delete',
    delete_many_ore_parameter_setting_value_sets_request:
        'reporting.v1.ore_parameter_setting_value_sets.delete_many',
    list_ore_parameter_setting_value_set_versions_request:
        'reporting.v1.ore_parameter_setting_value_sets_versions.list',
    get_ore_parameter_setting_value_set_version_request:
        'reporting.v1.ore_parameter_setting_value_sets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameter_setting_value_sets_request: true,
    get_ore_parameter_setting_value_set_request: true,
    get_many_ore_parameter_setting_value_sets_request: true,
    put_ore_parameter_setting_value_set_request: true,
    put_many_ore_parameter_setting_value_sets_request: true,
    delete_ore_parameter_setting_value_set_request: true,
    delete_many_ore_parameter_setting_value_sets_request: true,
    list_ore_parameter_setting_value_set_versions_request: true,
    get_ore_parameter_setting_value_set_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameter_setting_value_sets_events.created',
    updated: 'reporting.v1.ore_parameter_setting_value_sets_events.updated',
    deleted: 'reporting.v1.ore_parameter_setting_value_sets_events.deleted',
} as const;

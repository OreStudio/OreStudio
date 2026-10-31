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
import type { OreParameterValueSet } from '../domain/ore_parameter_value_set.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreParameterValueSetKey {
    code: string;
}

export interface OreParameterValueSetWrite {
    code: string;
    name: string;
    description: string;
    source_kind: string;
    cardinality: string;
    catalogue_code: string;
    minimum: number | null;
    maximum: number | null;
    pattern: string;
    is_ordered: boolean;
}

export interface OreParameterValueSetChange {
    write: OreParameterValueSetWrite;
    precondition: Precondition;
}

export interface OreParameterValueSetRemoval {
    key: OreParameterValueSetKey;
    precondition: Precondition;
}

export interface OreParameterValueSetLookup {
    key: OreParameterValueSetKey;
    ore_parameter_value_set: OreParameterValueSet | null;
}

export interface OreParameterValueSetsFilter {
    code_one_of: string[] | null;
}

export interface OreParameterValueSetEvent {
    event_id: string;
    key: OreParameterValueSetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterValueSetVersionKey {
    ore_parameter_value_set: OreParameterValueSetKey;
    version: number;
}

export interface OreParameterValueSetVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParameterValueSetsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetsFilter | null;
    as_of: string | null;
}

export interface ListOreParameterValueSetsResponse {
    result: Result;
    sets: OreParameterValueSet[];
    total: number;
}

export interface GetOreParameterValueSetRequest {
    key: OreParameterValueSetKey;
}

export interface GetOreParameterValueSetResponse {
    result: Result;
    ore_parameter_value_set: OreParameterValueSet | null;
}

export interface GetManyOreParameterValueSetsRequest {
    keys: OreParameterValueSetKey[];
}

export interface GetManyOreParameterValueSetsResponse {
    result: Result;
    entries: OreParameterValueSetLookup[];
}

export interface PutOreParameterValueSetRequest {
    change: OreParameterValueSetChange;
    intent: ChangeIntent;
}

export interface PutOreParameterValueSetResponse {
    result: Result;
    ore_parameter_value_set: OreParameterValueSet | null;
}

export interface PutManyOreParameterValueSetsRequest {
    changes: OreParameterValueSetChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParameterValueSetsResponse {
    result: Result;
    sets: OreParameterValueSet[];
}

export interface DeleteOreParameterValueSetRequest {
    removal: OreParameterValueSetRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterValueSetResponse {
    result: Result;
}

export interface DeleteManyOreParameterValueSetsRequest {
    removals: OreParameterValueSetRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParameterValueSetsResponse {
    result: Result;
}

export interface ListOreParameterValueSetVersionsRequest {
    key: OreParameterValueSetKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetVersionsFilter | null;
}

export interface ListOreParameterValueSetVersionsResponse {
    result: Result;
    versions: OreParameterValueSet[];
    total: number;
}

export interface GetOreParameterValueSetVersionRequest {
    key: OreParameterValueSetVersionKey;
}

export interface GetOreParameterValueSetVersionResponse {
    result: Result;
    version: OreParameterValueSet | null;
}

export const subjects = {
    list_ore_parameter_value_sets_request: 'reporting.v1.ore_parameter_value_sets.list',
    get_ore_parameter_value_set_request: 'reporting.v1.ore_parameter_value_sets.get',
    get_many_ore_parameter_value_sets_request: 'reporting.v1.ore_parameter_value_sets.get_many',
    put_ore_parameter_value_set_request: 'reporting.v1.ore_parameter_value_sets.put',
    put_many_ore_parameter_value_sets_request: 'reporting.v1.ore_parameter_value_sets.put_many',
    delete_ore_parameter_value_set_request: 'reporting.v1.ore_parameter_value_sets.delete',
    delete_many_ore_parameter_value_sets_request:
        'reporting.v1.ore_parameter_value_sets.delete_many',
    list_ore_parameter_value_set_versions_request:
        'reporting.v1.ore_parameter_value_sets_versions.list',
    get_ore_parameter_value_set_version_request:
        'reporting.v1.ore_parameter_value_sets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameter_value_sets_request: true,
    get_ore_parameter_value_set_request: true,
    get_many_ore_parameter_value_sets_request: true,
    put_ore_parameter_value_set_request: true,
    put_many_ore_parameter_value_sets_request: true,
    delete_ore_parameter_value_set_request: true,
    delete_many_ore_parameter_value_sets_request: true,
    list_ore_parameter_value_set_versions_request: true,
    get_ore_parameter_value_set_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameter_value_sets_events.created',
    updated: 'reporting.v1.ore_parameter_value_sets_events.updated',
    deleted: 'reporting.v1.ore_parameter_value_sets_events.deleted',
} as const;

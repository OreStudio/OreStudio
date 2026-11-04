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
import type { ScriptedParameterValue } from '../domain/scripted_parameter_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ScriptedParameterValueKey {
    trade_id: string;
    parameter_sequence_number: number;
    sequence_number: number;
}

export interface ScriptedParameterValueWrite {
    trade_id: string;
    parameter_sequence_number: number;
    sequence_number: number;
    trade_activity_id: string;
    value: string;
}

export interface ScriptedParameterValueChange {
    write: ScriptedParameterValueWrite;
    precondition: Precondition;
}

export interface ScriptedParameterValueRemoval {
    key: ScriptedParameterValueKey;
    precondition: Precondition;
}

export interface ScriptedParameterValueLookup {
    key: ScriptedParameterValueKey;
    scripted_parameter_value: ScriptedParameterValue | null;
}

export interface ScriptedParameterValuesFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface ScriptedParameterValueEvent {
    event_id: string;
    key: ScriptedParameterValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptedParameterValueVersionKey {
    scripted_parameter_value: ScriptedParameterValueKey;
    version: number;
}

export interface ScriptedParameterValueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptedParameterValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParameterValuesFilter | null;
    as_of: string | null;
}

export interface ListScriptedParameterValuesResponse {
    result: Result;
    scripted_parameter_values: ScriptedParameterValue[];
    total: number;
}

export interface GetScriptedParameterValueRequest {
    key: ScriptedParameterValueKey;
}

export interface GetScriptedParameterValueResponse {
    result: Result;
    scripted_parameter_value: ScriptedParameterValue | null;
}

export interface GetManyScriptedParameterValuesRequest {
    keys: ScriptedParameterValueKey[];
}

export interface GetManyScriptedParameterValuesResponse {
    result: Result;
    entries: ScriptedParameterValueLookup[];
}

export interface PutScriptedParameterValueRequest {
    change: ScriptedParameterValueChange;
    intent: ChangeIntent;
}

export interface PutScriptedParameterValueResponse {
    result: Result;
    scripted_parameter_value: ScriptedParameterValue | null;
}

export interface PutManyScriptedParameterValuesRequest {
    changes: ScriptedParameterValueChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptedParameterValuesResponse {
    result: Result;
    scripted_parameter_values: ScriptedParameterValue[];
}

export interface DeleteScriptedParameterValueRequest {
    removal: ScriptedParameterValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptedParameterValueResponse {
    result: Result;
}

export interface DeleteManyScriptedParameterValuesRequest {
    removals: ScriptedParameterValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptedParameterValuesResponse {
    result: Result;
}

export interface ListByTradeIdScriptedParameterValuesRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParameterValuesFilter | null;
}

export interface ListByTradeIdScriptedParameterValuesResponse {
    result: Result;
    scripted_parameter_values: ScriptedParameterValue[];
    total: number;
}

export interface ListScriptedParameterValueVersionsRequest {
    key: ScriptedParameterValueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParameterValueVersionsFilter | null;
}

export interface ListScriptedParameterValueVersionsResponse {
    result: Result;
    versions: ScriptedParameterValue[];
    total: number;
}

export interface GetScriptedParameterValueVersionRequest {
    key: ScriptedParameterValueVersionKey;
}

export interface GetScriptedParameterValueVersionResponse {
    result: Result;
    version: ScriptedParameterValue | null;
}

export const subjects = {
    list_scripted_parameter_values_request: 'trading.v1.scripted_parameter_values.list',
    get_scripted_parameter_value_request: 'trading.v1.scripted_parameter_values.get',
    get_many_scripted_parameter_values_request: 'trading.v1.scripted_parameter_values.get_many',
    put_scripted_parameter_value_request: 'trading.v1.scripted_parameter_values.put',
    put_many_scripted_parameter_values_request: 'trading.v1.scripted_parameter_values.put_many',
    delete_scripted_parameter_value_request: 'trading.v1.scripted_parameter_values.delete',
    delete_many_scripted_parameter_values_request:
        'trading.v1.scripted_parameter_values.delete_many',
    list_by_trade_id_scripted_parameter_values_request:
        'trading.v1.scripted_parameter_values.list_by_trade_id',
    list_scripted_parameter_value_versions_request:
        'trading.v1.scripted_parameter_values_versions.list',
    get_scripted_parameter_value_version_request:
        'trading.v1.scripted_parameter_values_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_scripted_parameter_values_request: true,
    get_scripted_parameter_value_request: true,
    get_many_scripted_parameter_values_request: true,
    put_scripted_parameter_value_request: true,
    put_many_scripted_parameter_values_request: true,
    delete_scripted_parameter_value_request: true,
    delete_many_scripted_parameter_values_request: true,
    list_by_trade_id_scripted_parameter_values_request: true,
    list_scripted_parameter_value_versions_request: true,
    get_scripted_parameter_value_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.scripted_parameter_values_events.created',
    updated: 'trading.v1.scripted_parameter_values_events.updated',
    deleted: 'trading.v1.scripted_parameter_values_events.deleted',
} as const;

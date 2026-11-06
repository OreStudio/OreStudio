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
import type { ScriptedParameter } from '../domain/scripted_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ScriptedParameterKey {
    trade_id: string;
    sequence_number: number;
}

export interface ScriptedParameterWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    name: string;
    declared_type: string;
    shape: string;
    value: string | null;
    apply_coarsening: string | null;
    base_schedule: string | null;
    shift: string | null;
    calendar: string | null;
    convention: string | null;
    remove_first_date: string | null;
    remove_last_date: string | null;
    shift_unit: string | null;
    shift_anchor: string | null;
}

export interface ScriptedParameterChange {
    write: ScriptedParameterWrite;
    precondition: Precondition;
}

export interface ScriptedParameterRemoval {
    key: ScriptedParameterKey;
    precondition: Precondition;
}

export interface ScriptedParameterLookup {
    key: ScriptedParameterKey;
    scripted_parameter: ScriptedParameter | null;
}

export interface ScriptedParametersFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface ScriptedParameterEvent {
    event_id: string;
    key: ScriptedParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptedParameterVersionKey {
    scripted_parameter: ScriptedParameterKey;
    version: number;
}

export interface ScriptedParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptedParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParametersFilter | null;
    as_of: string | null;
}

export interface ListScriptedParametersResponse {
    result: Result;
    scripted_parameters: ScriptedParameter[];
    total: number;
}

export interface GetScriptedParameterRequest {
    key: ScriptedParameterKey;
}

export interface GetScriptedParameterResponse {
    result: Result;
    scripted_parameter: ScriptedParameter | null;
}

export interface GetManyScriptedParametersRequest {
    keys: ScriptedParameterKey[];
}

export interface GetManyScriptedParametersResponse {
    result: Result;
    entries: ScriptedParameterLookup[];
}

export interface PutScriptedParameterRequest {
    change: ScriptedParameterChange;
    intent: ChangeIntent;
}

export interface PutScriptedParameterResponse {
    result: Result;
    scripted_parameter: ScriptedParameter | null;
}

export interface PutManyScriptedParametersRequest {
    changes: ScriptedParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptedParametersResponse {
    result: Result;
    scripted_parameters: ScriptedParameter[];
}

export interface DeleteScriptedParameterRequest {
    removal: ScriptedParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptedParameterResponse {
    result: Result;
}

export interface DeleteManyScriptedParametersRequest {
    removals: ScriptedParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptedParametersResponse {
    result: Result;
}

export interface ListByTradeIdScriptedParametersRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParametersFilter | null;
}

export interface ListByTradeIdScriptedParametersResponse {
    result: Result;
    scripted_parameters: ScriptedParameter[];
    total: number;
}

export interface ListScriptedParameterVersionsRequest {
    key: ScriptedParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedParameterVersionsFilter | null;
}

export interface ListScriptedParameterVersionsResponse {
    result: Result;
    versions: ScriptedParameter[];
    total: number;
}

export interface GetScriptedParameterVersionRequest {
    key: ScriptedParameterVersionKey;
}

export interface GetScriptedParameterVersionResponse {
    result: Result;
    version: ScriptedParameter | null;
}

export const subjects = {
    list_scripted_parameters_request: 'trading.v1.scripted_parameters.list',
    get_scripted_parameter_request: 'trading.v1.scripted_parameters.get',
    get_many_scripted_parameters_request: 'trading.v1.scripted_parameters.get_many',
    put_scripted_parameter_request: 'trading.v1.scripted_parameters.put',
    put_many_scripted_parameters_request: 'trading.v1.scripted_parameters.put_many',
    delete_scripted_parameter_request: 'trading.v1.scripted_parameters.delete',
    delete_many_scripted_parameters_request: 'trading.v1.scripted_parameters.delete_many',
    list_by_trade_id_scripted_parameters_request: 'trading.v1.scripted_parameters.list_by_trade_id',
    list_scripted_parameter_versions_request: 'trading.v1.scripted_parameters_versions.list',
    get_scripted_parameter_version_request: 'trading.v1.scripted_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_scripted_parameters_request: true,
    get_scripted_parameter_request: true,
    get_many_scripted_parameters_request: true,
    put_scripted_parameter_request: true,
    put_many_scripted_parameters_request: true,
    delete_scripted_parameter_request: true,
    delete_many_scripted_parameters_request: true,
    list_by_trade_id_scripted_parameters_request: true,
    list_scripted_parameter_versions_request: true,
    get_scripted_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.scripted_parameters.created',
    updated: 'trading.v1.events.scripted_parameters.updated',
    deleted: 'trading.v1.events.scripted_parameters.deleted',
} as const;

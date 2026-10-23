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
import type { CompositeInput } from '../domain/composite_input.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CompositeInputKey {
    oresmd_uri: string;
}

export interface CompositeInputWrite {
    id: string;
    composite_id: string;
    provider_code: string;
    oresmd_uri: string;
    position: number;
}

export interface CompositeInputChange {
    write: CompositeInputWrite;
    precondition: Precondition;
}

export interface CompositeInputRemoval {
    key: CompositeInputKey;
    precondition: Precondition;
}

export interface CompositeInputLookup {
    key: CompositeInputKey;
    composite_input: CompositeInput | null;
}

export interface CompositeInputsFilter {
    composite_id: string | null;
    provider_code: string | null;
    id_one_of: string[] | null;
    composite_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
}

export interface CompositeInputEvent {
    event_id: string;
    key: CompositeInputKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CompositeInputVersionKey {
    composite_input: CompositeInputKey;
    version: number;
}

export interface CompositeInputVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCompositeInputsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeInputsFilter | null;
    as_of: string | null;
}

export interface ListCompositeInputsResponse {
    result: Result;
    composite_inputs: CompositeInput[];
    total: number;
}

export interface GetCompositeInputRequest {
    key: CompositeInputKey;
}

export interface GetCompositeInputResponse {
    result: Result;
    composite_input: CompositeInput | null;
}

export interface GetManyCompositeInputsRequest {
    keys: CompositeInputKey[];
}

export interface GetManyCompositeInputsResponse {
    result: Result;
    entries: CompositeInputLookup[];
}

export interface PutCompositeInputRequest {
    change: CompositeInputChange;
    intent: ChangeIntent;
}

export interface PutCompositeInputResponse {
    result: Result;
    composite_input: CompositeInput | null;
}

export interface PutManyCompositeInputsRequest {
    changes: CompositeInputChange[];
    intent: ChangeIntent;
}

export interface PutManyCompositeInputsResponse {
    result: Result;
    composite_inputs: CompositeInput[];
}

export interface DeleteCompositeInputRequest {
    removal: CompositeInputRemoval;
    intent: ChangeIntent;
}

export interface DeleteCompositeInputResponse {
    result: Result;
}

export interface DeleteManyCompositeInputsRequest {
    removals: CompositeInputRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCompositeInputsResponse {
    result: Result;
}

export interface ListCompositeInputVersionsRequest {
    key: CompositeInputKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeInputVersionsFilter | null;
}

export interface ListCompositeInputVersionsResponse {
    result: Result;
    versions: CompositeInput[];
    total: number;
}

export interface GetCompositeInputVersionRequest {
    key: CompositeInputVersionKey;
}

export interface GetCompositeInputVersionResponse {
    result: Result;
    version: CompositeInput | null;
}

export const subjects = {
    list_composite_inputs_request: 'marketdata.v1.composite_inputs.list',
    get_composite_input_request: 'marketdata.v1.composite_inputs.get',
    get_many_composite_inputs_request: 'marketdata.v1.composite_inputs.get_many',
    put_composite_input_request: 'marketdata.v1.composite_inputs.put',
    put_many_composite_inputs_request: 'marketdata.v1.composite_inputs.put_many',
    delete_composite_input_request: 'marketdata.v1.composite_inputs.delete',
    delete_many_composite_inputs_request: 'marketdata.v1.composite_inputs.delete_many',
    list_composite_input_versions_request: 'marketdata.v1.composite_inputs_versions.list',
    get_composite_input_version_request: 'marketdata.v1.composite_inputs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_composite_inputs_request: true,
    get_composite_input_request: true,
    get_many_composite_inputs_request: true,
    put_composite_input_request: true,
    put_many_composite_inputs_request: true,
    delete_composite_input_request: true,
    delete_many_composite_inputs_request: true,
    list_composite_input_versions_request: true,
    get_composite_input_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.composite_inputs_events.created',
    updated: 'marketdata.v1.composite_inputs_events.updated',
    deleted: 'marketdata.v1.composite_inputs_events.deleted',
} as const;

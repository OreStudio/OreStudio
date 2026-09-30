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
import type { CurveDefinition } from '../domain/curve_definition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveDefinitionKey {
    curve_id: string;
}

export interface CurveDefinitionWrite {
    id: string;
    section_code: string;
    curve_id: string;
    description: string | null;
    currency: string | null;
    day_counter: string | null;
    interpolation_method: string | null;
    interpolation_variable: string | null;
    extrapolation: string | null;
    tolerance: string | null;
    extras: string | null;
    position: number;
}

export interface CurveDefinitionChange {
    write: CurveDefinitionWrite;
    precondition: Precondition;
}

export interface CurveDefinitionRemoval {
    key: CurveDefinitionKey;
    precondition: Precondition;
}

export interface CurveDefinitionLookup {
    key: CurveDefinitionKey;
    curve_definition: CurveDefinition | null;
}

export interface CurveDefinitionEvent {
    event_id: string;
    key: CurveDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveDefinitionVersionKey {
    curve_definition: CurveDefinitionKey;
    version: number;
}

export interface CurveDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveDefinitionsResponse {
    result: Result;
    curves: CurveDefinition[];
    total: number;
}

export interface GetCurveDefinitionRequest {
    key: CurveDefinitionKey;
}

export interface GetCurveDefinitionResponse {
    result: Result;
    curve_definition: CurveDefinition | null;
}

export interface GetManyCurveDefinitionsRequest {
    keys: CurveDefinitionKey[];
}

export interface GetManyCurveDefinitionsResponse {
    result: Result;
    entries: CurveDefinitionLookup[];
}

export interface PutCurveDefinitionRequest {
    change: CurveDefinitionChange;
    intent: ChangeIntent;
}

export interface PutCurveDefinitionResponse {
    result: Result;
    curve_definition: CurveDefinition | null;
}

export interface PutManyCurveDefinitionsRequest {
    changes: CurveDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveDefinitionsResponse {
    result: Result;
    curves: CurveDefinition[];
}

export interface DeleteCurveDefinitionRequest {
    removal: CurveDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveDefinitionResponse {
    result: Result;
}

export interface DeleteManyCurveDefinitionsRequest {
    removals: CurveDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveDefinitionsResponse {
    result: Result;
}

export interface ListCurveDefinitionVersionsRequest {
    key: CurveDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveDefinitionVersionsFilter | null;
}

export interface ListCurveDefinitionVersionsResponse {
    result: Result;
    versions: CurveDefinition[];
    total: number;
}

export interface GetCurveDefinitionVersionRequest {
    key: CurveDefinitionVersionKey;
}

export interface GetCurveDefinitionVersionResponse {
    result: Result;
    version: CurveDefinition | null;
}

export const subjects = {
    list_curve_definitions_request: 'refdata.v1.curve_definitions.list',
    get_curve_definition_request: 'refdata.v1.curve_definitions.get',
    get_many_curve_definitions_request: 'refdata.v1.curve_definitions.get_many',
    put_curve_definition_request: 'refdata.v1.curve_definitions.put',
    put_many_curve_definitions_request: 'refdata.v1.curve_definitions.put_many',
    delete_curve_definition_request: 'refdata.v1.curve_definitions.delete',
    delete_many_curve_definitions_request: 'refdata.v1.curve_definitions.delete_many',
    list_curve_definition_versions_request: 'refdata.v1.curve_definitions_versions.list',
    get_curve_definition_version_request: 'refdata.v1.curve_definitions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_definitions_request: true,
    get_curve_definition_request: true,
    get_many_curve_definitions_request: true,
    put_curve_definition_request: true,
    put_many_curve_definitions_request: true,
    delete_curve_definition_request: true,
    delete_many_curve_definitions_request: true,
    list_curve_definition_versions_request: true,
    get_curve_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_definitions_events.created',
    updated: 'refdata.v1.curve_definitions_events.updated',
    deleted: 'refdata.v1.curve_definitions_events.deleted',
} as const;

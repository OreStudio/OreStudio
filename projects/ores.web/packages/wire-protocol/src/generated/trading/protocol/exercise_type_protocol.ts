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
import type { ExerciseType } from '../domain/exercise_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ExerciseTypeKey {
    code: string;
}

export interface ExerciseTypeWrite {
    code: string;
    description: string;
}

export interface ExerciseTypeChange {
    write: ExerciseTypeWrite;
    precondition: Precondition;
}

export interface ExerciseTypeRemoval {
    key: ExerciseTypeKey;
    precondition: Precondition;
}

export interface ExerciseTypeLookup {
    key: ExerciseTypeKey;
    exercise_type: ExerciseType | null;
}

export interface ExerciseTypesFilter {
    code_one_of: string[] | null;
}

export interface ExerciseTypeEvent {
    event_id: string;
    key: ExerciseTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ExerciseTypeVersionKey {
    exercise_type: ExerciseTypeKey;
    version: number;
}

export interface ExerciseTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListExerciseTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ExerciseTypesFilter | null;
}

export interface ListExerciseTypesResponse {
    result: Result;
    exercise_types: ExerciseType[];
    total: number;
}

export interface GetExerciseTypeRequest {
    key: ExerciseTypeKey;
}

export interface GetExerciseTypeResponse {
    result: Result;
    exercise_type: ExerciseType | null;
}

export interface GetManyExerciseTypesRequest {
    keys: ExerciseTypeKey[];
}

export interface GetManyExerciseTypesResponse {
    result: Result;
    entries: ExerciseTypeLookup[];
}

export interface PutExerciseTypeRequest {
    change: ExerciseTypeChange;
    intent: ChangeIntent;
}

export interface PutExerciseTypeResponse {
    result: Result;
    exercise_type: ExerciseType | null;
}

export interface PutManyExerciseTypesRequest {
    changes: ExerciseTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyExerciseTypesResponse {
    result: Result;
    exercise_types: ExerciseType[];
}

export interface DeleteExerciseTypeRequest {
    removal: ExerciseTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteExerciseTypeResponse {
    result: Result;
}

export interface DeleteManyExerciseTypesRequest {
    removals: ExerciseTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyExerciseTypesResponse {
    result: Result;
}

export interface ListExerciseTypeVersionsRequest {
    key: ExerciseTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ExerciseTypeVersionsFilter | null;
}

export interface ListExerciseTypeVersionsResponse {
    result: Result;
    versions: ExerciseType[];
    total: number;
}

export interface GetExerciseTypeVersionRequest {
    key: ExerciseTypeVersionKey;
}

export interface GetExerciseTypeVersionResponse {
    result: Result;
    version: ExerciseType | null;
}

export const subjects = {
    list_exercise_types_request: 'trading.v1.exercise_types.list',
    get_exercise_type_request: 'trading.v1.exercise_types.get',
    get_many_exercise_types_request: 'trading.v1.exercise_types.get_many',
    put_exercise_type_request: 'trading.v1.exercise_types.put',
    put_many_exercise_types_request: 'trading.v1.exercise_types.put_many',
    delete_exercise_type_request: 'trading.v1.exercise_types.delete',
    delete_many_exercise_types_request: 'trading.v1.exercise_types.delete_many',
    list_exercise_type_versions_request: 'trading.v1.exercise_types_versions.list',
    get_exercise_type_version_request: 'trading.v1.exercise_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_exercise_types_request: true,
    get_exercise_type_request: true,
    get_many_exercise_types_request: true,
    put_exercise_type_request: true,
    put_many_exercise_types_request: true,
    delete_exercise_type_request: true,
    delete_many_exercise_types_request: true,
    list_exercise_type_versions_request: true,
    get_exercise_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.exercise_types_events.created',
    updated: 'trading.v1.exercise_types_events.updated',
    deleted: 'trading.v1.exercise_types_events.deleted',
} as const;

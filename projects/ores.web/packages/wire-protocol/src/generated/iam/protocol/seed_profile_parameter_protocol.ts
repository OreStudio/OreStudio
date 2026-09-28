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
import type { SeedProfileParameter } from '../domain/seed_profile_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SeedProfileParameterKey {
    id: string;
}

export interface SeedProfileParameterWrite {
    id: string;
    seed_profile_id: string;
    name: string;
    label: string;
    data_type: string;
    choices_json: string;
    default_value: string;
    is_required: boolean;
    description: string;
    display_order: number;
}

export interface SeedProfileParameterChange {
    write: SeedProfileParameterWrite;
    precondition: Precondition;
}

export interface SeedProfileParameterRemoval {
    key: SeedProfileParameterKey;
    precondition: Precondition;
}

export interface SeedProfileParameterLookup {
    key: SeedProfileParameterKey;
    seed_profile_parameter: SeedProfileParameter | null;
}

export interface SeedProfileParametersFilter {
    seed_profile_id: string | null;
}

export interface SeedProfileParameterEvent {
    event_id: string;
    key: SeedProfileParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeedProfileParameterVersionKey {
    seed_profile_parameter: SeedProfileParameterKey;
    version: number;
}

export interface SeedProfileParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeedProfileParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileParametersFilter | null;
}

export interface ListSeedProfileParametersResponse {
    result: Result;
    seed_profile_parameters: SeedProfileParameter[];
    total: number;
}

export interface GetSeedProfileParameterRequest {
    key: SeedProfileParameterKey;
}

export interface GetSeedProfileParameterResponse {
    result: Result;
    seed_profile_parameter: SeedProfileParameter | null;
}

export interface GetManySeedProfileParametersRequest {
    keys: SeedProfileParameterKey[];
}

export interface GetManySeedProfileParametersResponse {
    result: Result;
    entries: SeedProfileParameterLookup[];
}

export interface PutSeedProfileParameterRequest {
    change: SeedProfileParameterChange;
    intent: ChangeIntent;
}

export interface PutSeedProfileParameterResponse {
    result: Result;
    seed_profile_parameter: SeedProfileParameter;
}

export interface PutManySeedProfileParametersRequest {
    changes: SeedProfileParameterChange[];
    intent: ChangeIntent;
}

export interface PutManySeedProfileParametersResponse {
    result: Result;
    seed_profile_parameters: SeedProfileParameter[];
}

export interface DeleteSeedProfileParameterRequest {
    removal: SeedProfileParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeedProfileParameterResponse {
    result: Result;
}

export interface DeleteManySeedProfileParametersRequest {
    removals: SeedProfileParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeedProfileParametersResponse {
    result: Result;
}

export interface ListBySeedProfileIdSeedProfileParametersRequest {
    seed_profile_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileParametersFilter | null;
}

export interface ListBySeedProfileIdSeedProfileParametersResponse {
    result: Result;
    seed_profile_parameters: SeedProfileParameter[];
    total: number;
}

export interface ListSeedProfileParameterVersionsRequest {
    key: SeedProfileParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileParameterVersionsFilter | null;
}

export interface ListSeedProfileParameterVersionsResponse {
    result: Result;
    versions: SeedProfileParameter[];
    total: number;
}

export interface GetSeedProfileParameterVersionRequest {
    key: SeedProfileParameterVersionKey;
}

export interface GetSeedProfileParameterVersionResponse {
    result: Result;
    version: SeedProfileParameter;
}

export const subjects = {
    list_seed_profile_parameters_request: 'iam.v1.seed_profile_parameters.list',
    get_seed_profile_parameter_request: 'iam.v1.seed_profile_parameters.get',
    get_many_seed_profile_parameters_request: 'iam.v1.seed_profile_parameters.get_many',
    put_seed_profile_parameter_request: 'iam.v1.seed_profile_parameters.put',
    put_many_seed_profile_parameters_request: 'iam.v1.seed_profile_parameters.put_many',
    delete_seed_profile_parameter_request: 'iam.v1.seed_profile_parameters.delete',
    delete_many_seed_profile_parameters_request: 'iam.v1.seed_profile_parameters.delete_many',
    list_by_seed_profile_id_seed_profile_parameters_request:
        'iam.v1.seed_profile_parameters.list_by_seed_profile_id',
    list_seed_profile_parameter_versions_request: 'iam.v1.seed_profile_parameters_versions.list',
    get_seed_profile_parameter_version_request: 'iam.v1.seed_profile_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_seed_profile_parameters_request: true,
    get_seed_profile_parameter_request: true,
    get_many_seed_profile_parameters_request: true,
    put_seed_profile_parameter_request: true,
    put_many_seed_profile_parameters_request: true,
    delete_seed_profile_parameter_request: true,
    delete_many_seed_profile_parameters_request: true,
    list_by_seed_profile_id_seed_profile_parameters_request: true,
    list_seed_profile_parameter_versions_request: true,
    get_seed_profile_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.seed_profile_parameters_events.created',
    updated: 'iam.v1.seed_profile_parameters_events.updated',
    deleted: 'iam.v1.seed_profile_parameters_events.deleted',
} as const;

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
import type { SeedProfileStep } from '../domain/seed_profile_step.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SeedProfileStepKey {
    id: string;
}

export interface SeedProfileStepWrite {
    id: string;
    seed_profile_id: string;
    step_kind: string;
    arguments_json: string;
    display_order: number;
}

export interface SeedProfileStepChange {
    write: SeedProfileStepWrite;
    precondition: Precondition;
}

export interface SeedProfileStepRemoval {
    key: SeedProfileStepKey;
    precondition: Precondition;
}

export interface SeedProfileStepLookup {
    key: SeedProfileStepKey;
    seed_profile_step: SeedProfileStep | null;
}

export interface SeedProfileStepsFilter {
    seed_profile_id: string | null;
    id_one_of: string[] | null;
    seed_profile_id_one_of: string[] | null;
}

export interface SeedProfileStepEvent {
    event_id: string;
    key: SeedProfileStepKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeedProfileStepVersionKey {
    seed_profile_step: SeedProfileStepKey;
    version: number;
}

export interface SeedProfileStepVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeedProfileStepsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileStepsFilter | null;
}

export interface ListSeedProfileStepsResponse {
    result: Result;
    seed_profile_steps: SeedProfileStep[];
    total: number;
}

export interface GetSeedProfileStepRequest {
    key: SeedProfileStepKey;
}

export interface GetSeedProfileStepResponse {
    result: Result;
    seed_profile_step: SeedProfileStep | null;
}

export interface GetManySeedProfileStepsRequest {
    keys: SeedProfileStepKey[];
}

export interface GetManySeedProfileStepsResponse {
    result: Result;
    entries: SeedProfileStepLookup[];
}

export interface PutSeedProfileStepRequest {
    change: SeedProfileStepChange;
    intent: ChangeIntent;
}

export interface PutSeedProfileStepResponse {
    result: Result;
    seed_profile_step: SeedProfileStep | null;
}

export interface PutManySeedProfileStepsRequest {
    changes: SeedProfileStepChange[];
    intent: ChangeIntent;
}

export interface PutManySeedProfileStepsResponse {
    result: Result;
    seed_profile_steps: SeedProfileStep[];
}

export interface DeleteSeedProfileStepRequest {
    removal: SeedProfileStepRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeedProfileStepResponse {
    result: Result;
}

export interface DeleteManySeedProfileStepsRequest {
    removals: SeedProfileStepRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeedProfileStepsResponse {
    result: Result;
}

export interface ListBySeedProfileIdSeedProfileStepsRequest {
    seed_profile_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileStepsFilter | null;
}

export interface ListBySeedProfileIdSeedProfileStepsResponse {
    result: Result;
    seed_profile_steps: SeedProfileStep[];
    total: number;
}

export interface ListSeedProfileStepVersionsRequest {
    key: SeedProfileStepKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileStepVersionsFilter | null;
}

export interface ListSeedProfileStepVersionsResponse {
    result: Result;
    versions: SeedProfileStep[];
    total: number;
}

export interface GetSeedProfileStepVersionRequest {
    key: SeedProfileStepVersionKey;
}

export interface GetSeedProfileStepVersionResponse {
    result: Result;
    version: SeedProfileStep | null;
}

export const subjects = {
    list_seed_profile_steps_request: 'iam.v1.seed_profile_steps.list',
    get_seed_profile_step_request: 'iam.v1.seed_profile_steps.get',
    get_many_seed_profile_steps_request: 'iam.v1.seed_profile_steps.get_many',
    put_seed_profile_step_request: 'iam.v1.seed_profile_steps.put',
    put_many_seed_profile_steps_request: 'iam.v1.seed_profile_steps.put_many',
    delete_seed_profile_step_request: 'iam.v1.seed_profile_steps.delete',
    delete_many_seed_profile_steps_request: 'iam.v1.seed_profile_steps.delete_many',
    list_by_seed_profile_id_seed_profile_steps_request:
        'iam.v1.seed_profile_steps.list_by_seed_profile_id',
    list_seed_profile_step_versions_request: 'iam.v1.seed_profile_steps_versions.list',
    get_seed_profile_step_version_request: 'iam.v1.seed_profile_steps_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_seed_profile_steps_request: true,
    get_seed_profile_step_request: true,
    get_many_seed_profile_steps_request: true,
    put_seed_profile_step_request: true,
    put_many_seed_profile_steps_request: true,
    delete_seed_profile_step_request: true,
    delete_many_seed_profile_steps_request: true,
    list_by_seed_profile_id_seed_profile_steps_request: true,
    list_seed_profile_step_versions_request: true,
    get_seed_profile_step_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.seed_profile_steps_events.created',
    updated: 'iam.v1.seed_profile_steps_events.updated',
    deleted: 'iam.v1.seed_profile_steps_events.deleted',
} as const;

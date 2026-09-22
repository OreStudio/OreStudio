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
import type { DepositConvention } from '../domain/deposit_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DepositConventionKey {
    id: string;
}

export interface DepositConventionWrite {
    id: string;
    index_based: boolean;
    index: string | null;
    calendar: string | null;
    convention: string | null;
    end_of_month: boolean | null;
    day_count_fraction: string | null;
    settlement_days: number | null;
}

export interface DepositConventionChange {
    write: DepositConventionWrite;
    precondition: Precondition;
}

export interface DepositConventionRemoval {
    key: DepositConventionKey;
    precondition: Precondition;
}

export interface DepositConventionLookup {
    key: DepositConventionKey;
    deposit_convention: DepositConvention | null;
}

export interface DepositConventionEvent {
    event_id: string;
    key: DepositConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DepositConventionVersionKey {
    deposit_convention: DepositConventionKey;
    version: number;
}

export interface DepositConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDepositConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDepositConventionsResponse {
    result: Result;
    deposit_conventions: DepositConvention[];
    total: number;
}

export interface GetDepositConventionRequest {
    key: DepositConventionKey;
}

export interface GetDepositConventionResponse {
    result: Result;
    deposit_convention: DepositConvention | null;
}

export interface GetManyDepositConventionsRequest {
    keys: DepositConventionKey[];
}

export interface GetManyDepositConventionsResponse {
    result: Result;
    entries: DepositConventionLookup[];
}

export interface PutDepositConventionRequest {
    change: DepositConventionChange;
    intent: ChangeIntent;
}

export interface PutDepositConventionResponse {
    result: Result;
    deposit_convention: DepositConvention;
}

export interface PutManyDepositConventionsRequest {
    changes: DepositConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyDepositConventionsResponse {
    result: Result;
    deposit_conventions: DepositConvention[];
}

export interface DeleteDepositConventionRequest {
    removal: DepositConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteDepositConventionResponse {
    result: Result;
}

export interface DeleteManyDepositConventionsRequest {
    removals: DepositConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDepositConventionsResponse {
    result: Result;
}

export interface ListDepositConventionVersionsRequest {
    key: DepositConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DepositConventionVersionsFilter | null;
}

export interface ListDepositConventionVersionsResponse {
    result: Result;
    versions: DepositConvention[];
    total: number;
}

export interface GetDepositConventionVersionRequest {
    key: DepositConventionVersionKey;
}

export interface GetDepositConventionVersionResponse {
    result: Result;
    version: DepositConvention;
}

export const subjects = {
    list_deposit_conventions_request: "refdata.v1.deposit_conventions.list",
    get_deposit_convention_request: "refdata.v1.deposit_conventions.get",
    get_many_deposit_conventions_request: "refdata.v1.deposit_conventions.get_many",
    put_deposit_convention_request: "refdata.v1.deposit_conventions.put",
    put_many_deposit_conventions_request: "refdata.v1.deposit_conventions.put_many",
    delete_deposit_convention_request: "refdata.v1.deposit_conventions.delete",
    delete_many_deposit_conventions_request: "refdata.v1.deposit_conventions.delete_many",
    list_deposit_convention_versions_request: "refdata.v1.deposit_conventions_versions.list",
    get_deposit_convention_version_request: "refdata.v1.deposit_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_deposit_conventions_request: true,
    get_deposit_convention_request: true,
    get_many_deposit_conventions_request: true,
    put_deposit_convention_request: true,
    put_many_deposit_conventions_request: true,
    delete_deposit_convention_request: true,
    delete_many_deposit_conventions_request: true,
    list_deposit_convention_versions_request: true,
    get_deposit_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.deposit_conventions_events.created",
    updated: "refdata.v1.deposit_conventions_events.updated",
    deleted: "refdata.v1.deposit_conventions_events.deleted",
} as const;

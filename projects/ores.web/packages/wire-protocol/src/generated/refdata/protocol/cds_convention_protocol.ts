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
import type { CdsConvention } from '../domain/cds_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CdsConventionKey {
    id: string;
}

export interface CdsConventionWrite {
    id: string;
    settlement_days: number;
    calendar: string;
    frequency: string;
    payment_convention: string;
    rule: string;
    day_count_fraction: string;
    settles_accrual: boolean;
    pays_at_default_time: boolean;
    upfront_settlement_days: number | null;
    last_period_day_count_fraction: string | null;
}

export interface CdsConventionChange {
    write: CdsConventionWrite;
    precondition: Precondition;
}

export interface CdsConventionRemoval {
    key: CdsConventionKey;
    precondition: Precondition;
}

export interface CdsConventionLookup {
    key: CdsConventionKey;
    cds_convention: CdsConvention | null;
}

export interface CdsConventionEvent {
    event_id: string;
    key: CdsConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CdsConventionVersionKey {
    cds_convention: CdsConventionKey;
    version: number;
}

export interface CdsConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCdsConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCdsConventionsResponse {
    result: Result;
    cds_conventions: CdsConvention[];
    total: number;
}

export interface GetCdsConventionRequest {
    key: CdsConventionKey;
}

export interface GetCdsConventionResponse {
    result: Result;
    cds_convention: CdsConvention | null;
}

export interface GetManyCdsConventionsRequest {
    keys: CdsConventionKey[];
}

export interface GetManyCdsConventionsResponse {
    result: Result;
    entries: CdsConventionLookup[];
}

export interface PutCdsConventionRequest {
    change: CdsConventionChange;
    intent: ChangeIntent;
}

export interface PutCdsConventionResponse {
    result: Result;
    cds_convention: CdsConvention;
}

export interface PutManyCdsConventionsRequest {
    changes: CdsConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCdsConventionsResponse {
    result: Result;
    cds_conventions: CdsConvention[];
}

export interface DeleteCdsConventionRequest {
    removal: CdsConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCdsConventionResponse {
    result: Result;
}

export interface DeleteManyCdsConventionsRequest {
    removals: CdsConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCdsConventionsResponse {
    result: Result;
}

export interface ListCdsConventionVersionsRequest {
    key: CdsConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CdsConventionVersionsFilter | null;
}

export interface ListCdsConventionVersionsResponse {
    result: Result;
    versions: CdsConvention[];
    total: number;
}

export interface GetCdsConventionVersionRequest {
    key: CdsConventionVersionKey;
}

export interface GetCdsConventionVersionResponse {
    result: Result;
    version: CdsConvention;
}

export const subjects = {
    list_cds_conventions_request: "refdata.v1.cds_conventions.list",
    get_cds_convention_request: "refdata.v1.cds_conventions.get",
    get_many_cds_conventions_request: "refdata.v1.cds_conventions.get_many",
    put_cds_convention_request: "refdata.v1.cds_conventions.put",
    put_many_cds_conventions_request: "refdata.v1.cds_conventions.put_many",
    delete_cds_convention_request: "refdata.v1.cds_conventions.delete",
    delete_many_cds_conventions_request: "refdata.v1.cds_conventions.delete_many",
    list_cds_convention_versions_request: "refdata.v1.cds_conventions_versions.list",
    get_cds_convention_version_request: "refdata.v1.cds_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cds_conventions_request: true,
    get_cds_convention_request: true,
    get_many_cds_conventions_request: true,
    put_cds_convention_request: true,
    put_many_cds_conventions_request: true,
    delete_cds_convention_request: true,
    delete_many_cds_conventions_request: true,
    list_cds_convention_versions_request: true,
    get_cds_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.cds_conventions_events.created",
    updated: "refdata.v1.cds_conventions_events.updated",
    deleted: "refdata.v1.cds_conventions_events.deleted",
} as const;

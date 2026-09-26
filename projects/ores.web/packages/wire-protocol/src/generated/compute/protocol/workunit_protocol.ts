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
import type { Workunit } from '../domain/workunit.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface WorkunitKey {
    id: string;
}

export interface WorkunitWrite {
    id: string;
    batch_id: string;
    app_version_id: string;
    input_uri: string;
    config_uri: string;
    priority: number;
    target_redundancy: number;
    canonical_result_id: string;
}

export interface WorkunitChange {
    write: WorkunitWrite;
    precondition: Precondition;
}

export interface WorkunitRemoval {
    key: WorkunitKey;
    precondition: Precondition;
}

export interface WorkunitLookup {
    key: WorkunitKey;
    workunit: Workunit | null;
}

export interface WorkunitsFilter {
    batch_id: string | null;
}

export interface WorkunitEvent {
    event_id: string;
    key: WorkunitKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkunitVersionKey {
    workunit: WorkunitKey;
    version: number;
}

export interface WorkunitVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkunitsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkunitsFilter | null;
}

export interface ListWorkunitsResponse {
    result: Result;
    workunits: Workunit[];
    total: number;
}

export interface GetWorkunitRequest {
    key: WorkunitKey;
}

export interface GetWorkunitResponse {
    result: Result;
    workunit: Workunit | null;
}

export interface GetManyWorkunitsRequest {
    keys: WorkunitKey[];
}

export interface GetManyWorkunitsResponse {
    result: Result;
    entries: WorkunitLookup[];
}

export interface PutWorkunitRequest {
    change: WorkunitChange;
    intent: ChangeIntent;
}

export interface PutWorkunitResponse {
    result: Result;
    workunit: Workunit;
}

export interface PutManyWorkunitsRequest {
    changes: WorkunitChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkunitsResponse {
    result: Result;
    workunits: Workunit[];
}

export interface DeleteWorkunitRequest {
    removal: WorkunitRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkunitResponse {
    result: Result;
}

export interface DeleteManyWorkunitsRequest {
    removals: WorkunitRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkunitsResponse {
    result: Result;
}

export interface ListByBatchIdWorkunitsRequest {
    batch_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkunitsFilter | null;
}

export interface ListByBatchIdWorkunitsResponse {
    result: Result;
    workunits: Workunit[];
    total: number;
}

export interface ListWorkunitVersionsRequest {
    key: WorkunitKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkunitVersionsFilter | null;
}

export interface ListWorkunitVersionsResponse {
    result: Result;
    versions: Workunit[];
    total: number;
}

export interface GetWorkunitVersionRequest {
    key: WorkunitVersionKey;
}

export interface GetWorkunitVersionResponse {
    result: Result;
    version: Workunit;
}

export const subjects = {
    list_workunits_request: "compute.v1.workunits.list",
    get_workunit_request: "compute.v1.workunits.get",
    get_many_workunits_request: "compute.v1.workunits.get_many",
    put_workunit_request: "compute.v1.workunits.put",
    put_many_workunits_request: "compute.v1.workunits.put_many",
    delete_workunit_request: "compute.v1.workunits.delete",
    delete_many_workunits_request: "compute.v1.workunits.delete_many",
    list_by_batch_id_workunits_request: "compute.v1.workunits.list_by_batch_id",
    list_workunit_versions_request: "compute.v1.workunits_versions.list",
    get_workunit_version_request: "compute.v1.workunits_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workunits_request: true,
    get_workunit_request: true,
    get_many_workunits_request: true,
    put_workunit_request: true,
    put_many_workunits_request: true,
    delete_workunit_request: true,
    delete_many_workunits_request: true,
    list_by_batch_id_workunits_request: true,
    list_workunit_versions_request: true,
    get_workunit_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.workunits_events.created",
    updated: "compute.v1.workunits_events.updated",
    deleted: "compute.v1.workunits_events.deleted",
} as const;

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
import type { PartyStatus } from '../domain/party_status.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyStatusKey {
    code: string;
}

export interface PartyStatusWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface PartyStatusChange {
    write: PartyStatusWrite;
    precondition: Precondition;
}

export interface PartyStatusRemoval {
    key: PartyStatusKey;
    precondition: Precondition;
}

export interface PartyStatusLookup {
    key: PartyStatusKey;
    party_status: PartyStatus | null;
}

export interface PartyStatusEvent {
    event_id: string;
    key: PartyStatusKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyStatusVersionKey {
    party_status: PartyStatusKey;
    version: number;
}

export interface PartyStatusVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyStatusesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPartyStatusesResponse {
    result: Result;
    statuses: PartyStatus[];
    total: number;
}

export interface GetPartyStatusRequest {
    key: PartyStatusKey;
}

export interface GetPartyStatusResponse {
    result: Result;
    party_status: PartyStatus | null;
}

export interface GetManyPartyStatusesRequest {
    keys: PartyStatusKey[];
}

export interface GetManyPartyStatusesResponse {
    result: Result;
    entries: PartyStatusLookup[];
}

export interface PutPartyStatusRequest {
    change: PartyStatusChange;
    intent: ChangeIntent;
}

export interface PutPartyStatusResponse {
    result: Result;
    party_status: PartyStatus;
}

export interface PutManyPartyStatusesRequest {
    changes: PartyStatusChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyStatusesResponse {
    result: Result;
    statuses: PartyStatus[];
}

export interface DeletePartyStatusRequest {
    removal: PartyStatusRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyStatusResponse {
    result: Result;
}

export interface DeleteManyPartyStatusesRequest {
    removals: PartyStatusRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyStatusesResponse {
    result: Result;
}

export interface ListPartyStatusVersionsRequest {
    key: PartyStatusKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyStatusVersionsFilter | null;
}

export interface ListPartyStatusVersionsResponse {
    result: Result;
    versions: PartyStatus[];
    total: number;
}

export interface GetPartyStatusVersionRequest {
    key: PartyStatusVersionKey;
}

export interface GetPartyStatusVersionResponse {
    result: Result;
    version: PartyStatus;
}

export const subjects = {
    list_party_statuses_request: "refdata.v1.party_statuses.list",
    get_party_status_request: "refdata.v1.party_statuses.get",
    get_many_party_statuses_request: "refdata.v1.party_statuses.get_many",
    put_party_status_request: "refdata.v1.party_statuses.put",
    put_many_party_statuses_request: "refdata.v1.party_statuses.put_many",
    delete_party_status_request: "refdata.v1.party_statuses.delete",
    delete_many_party_statuses_request: "refdata.v1.party_statuses.delete_many",
    list_party_status_versions_request: "refdata.v1.party_statuses_versions.list",
    get_party_status_version_request: "refdata.v1.party_statuses_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_statuses_request: true,
    get_party_status_request: true,
    get_many_party_statuses_request: true,
    put_party_status_request: true,
    put_many_party_statuses_request: true,
    delete_party_status_request: true,
    delete_many_party_statuses_request: true,
    list_party_status_versions_request: true,
    get_party_status_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.party_statuses_events.created",
    updated: "refdata.v1.party_statuses_events.updated",
    deleted: "refdata.v1.party_statuses_events.deleted",
} as const;

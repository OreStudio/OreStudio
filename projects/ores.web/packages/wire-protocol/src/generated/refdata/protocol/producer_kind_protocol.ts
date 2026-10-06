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
import type { ProducerKind } from '../domain/producer_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ProducerKindKey {
    code: string;
}

export interface ProducerKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ProducerKindChange {
    write: ProducerKindWrite;
    precondition: Precondition;
}

export interface ProducerKindRemoval {
    key: ProducerKindKey;
    precondition: Precondition;
}

export interface ProducerKindLookup {
    key: ProducerKindKey;
    producer_kind: ProducerKind | null;
}

export interface ProducerKindsFilter {
    code_one_of: string[] | null;
}

export interface ProducerKindEvent {
    event_id: string;
    key: ProducerKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ProducerKindVersionKey {
    producer_kind: ProducerKindKey;
    version: number;
}

export interface ProducerKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListProducerKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ProducerKindsFilter | null;
    as_of: string | null;
}

export interface ListProducerKindsResponse {
    result: Result;
    kinds: ProducerKind[];
    total: number;
}

export interface GetProducerKindRequest {
    key: ProducerKindKey;
}

export interface GetProducerKindResponse {
    result: Result;
    producer_kind: ProducerKind | null;
}

export interface GetManyProducerKindsRequest {
    keys: ProducerKindKey[];
}

export interface GetManyProducerKindsResponse {
    result: Result;
    entries: ProducerKindLookup[];
}

export interface PutProducerKindRequest {
    change: ProducerKindChange;
    intent: ChangeIntent;
}

export interface PutProducerKindResponse {
    result: Result;
    producer_kind: ProducerKind | null;
}

export interface PutManyProducerKindsRequest {
    changes: ProducerKindChange[];
    intent: ChangeIntent;
}

export interface PutManyProducerKindsResponse {
    result: Result;
    kinds: ProducerKind[];
}

export interface DeleteProducerKindRequest {
    removal: ProducerKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteProducerKindResponse {
    result: Result;
}

export interface DeleteManyProducerKindsRequest {
    removals: ProducerKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyProducerKindsResponse {
    result: Result;
}

export interface ListProducerKindVersionsRequest {
    key: ProducerKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ProducerKindVersionsFilter | null;
}

export interface ListProducerKindVersionsResponse {
    result: Result;
    versions: ProducerKind[];
    total: number;
}

export interface GetProducerKindVersionRequest {
    key: ProducerKindVersionKey;
}

export interface GetProducerKindVersionResponse {
    result: Result;
    version: ProducerKind | null;
}

export const subjects = {
    list_producer_kinds_request: 'refdata.v1.producer_kinds.list',
    get_producer_kind_request: 'refdata.v1.producer_kinds.get',
    get_many_producer_kinds_request: 'refdata.v1.producer_kinds.get_many',
    put_producer_kind_request: 'refdata.v1.producer_kinds.put',
    put_many_producer_kinds_request: 'refdata.v1.producer_kinds.put_many',
    delete_producer_kind_request: 'refdata.v1.producer_kinds.delete',
    delete_many_producer_kinds_request: 'refdata.v1.producer_kinds.delete_many',
    list_producer_kind_versions_request: 'refdata.v1.producer_kinds_versions.list',
    get_producer_kind_version_request: 'refdata.v1.producer_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_producer_kinds_request: true,
    get_producer_kind_request: true,
    get_many_producer_kinds_request: true,
    put_producer_kind_request: true,
    put_many_producer_kinds_request: true,
    delete_producer_kind_request: true,
    delete_many_producer_kinds_request: true,
    list_producer_kind_versions_request: true,
    get_producer_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.producer_kinds_events.created',
    updated: 'refdata.v1.producer_kinds_events.updated',
    deleted: 'refdata.v1.producer_kinds_events.deleted',
} as const;

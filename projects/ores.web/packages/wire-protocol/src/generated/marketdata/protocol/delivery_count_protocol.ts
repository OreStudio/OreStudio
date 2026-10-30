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
import type { DeliveryCount } from '../domain/delivery_count.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DeliveryCountKey {
    oresmd_uri: string;
}

export interface DeliveryCountWrite {
    id: string;
    party_id: string;
    provider_code: string;
    oresmd_uri: string;
    period_start: string;
    flushed_at: string;
    feed_id: string;
    deliveries: number;
}

export interface DeliveryCountChange {
    write: DeliveryCountWrite;
    precondition: Precondition;
}

export interface DeliveryCountRemoval {
    key: DeliveryCountKey;
    precondition: Precondition;
}

export interface DeliveryCountLookup {
    key: DeliveryCountKey;
    delivery_count: DeliveryCount | null;
}

export interface DeliveryCountsFilter {
    party_id: string | null;
    provider_code: string | null;
    feed_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
    feed_id_one_of: string[] | null;
}

export interface DeliveryCountEvent {
    event_id: string;
    key: DeliveryCountKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DeliveryCountVersionKey {
    delivery_count: DeliveryCountKey;
    version: number;
}

export interface DeliveryCountVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDeliveryCountsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DeliveryCountsFilter | null;
    as_of: string | null;
}

export interface ListDeliveryCountsResponse {
    result: Result;
    delivery_counts: DeliveryCount[];
    total: number;
}

export interface GetDeliveryCountRequest {
    key: DeliveryCountKey;
}

export interface GetDeliveryCountResponse {
    result: Result;
    delivery_count: DeliveryCount | null;
}

export interface GetManyDeliveryCountsRequest {
    keys: DeliveryCountKey[];
}

export interface GetManyDeliveryCountsResponse {
    result: Result;
    entries: DeliveryCountLookup[];
}

export interface PutDeliveryCountRequest {
    change: DeliveryCountChange;
    intent: ChangeIntent;
}

export interface PutDeliveryCountResponse {
    result: Result;
    delivery_count: DeliveryCount | null;
}

export interface PutManyDeliveryCountsRequest {
    changes: DeliveryCountChange[];
    intent: ChangeIntent;
}

export interface PutManyDeliveryCountsResponse {
    result: Result;
    delivery_counts: DeliveryCount[];
}

export interface DeleteDeliveryCountRequest {
    removal: DeliveryCountRemoval;
    intent: ChangeIntent;
}

export interface DeleteDeliveryCountResponse {
    result: Result;
}

export interface DeleteManyDeliveryCountsRequest {
    removals: DeliveryCountRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDeliveryCountsResponse {
    result: Result;
}

export interface ListDeliveryCountVersionsRequest {
    key: DeliveryCountKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DeliveryCountVersionsFilter | null;
}

export interface ListDeliveryCountVersionsResponse {
    result: Result;
    versions: DeliveryCount[];
    total: number;
}

export interface GetDeliveryCountVersionRequest {
    key: DeliveryCountVersionKey;
}

export interface GetDeliveryCountVersionResponse {
    result: Result;
    version: DeliveryCount | null;
}

export const subjects = {
    list_delivery_counts_request: 'marketdata.v1.delivery_counts.list',
    get_delivery_count_request: 'marketdata.v1.delivery_counts.get',
    get_many_delivery_counts_request: 'marketdata.v1.delivery_counts.get_many',
    put_delivery_count_request: 'marketdata.v1.delivery_counts.put',
    put_many_delivery_counts_request: 'marketdata.v1.delivery_counts.put_many',
    delete_delivery_count_request: 'marketdata.v1.delivery_counts.delete',
    delete_many_delivery_counts_request: 'marketdata.v1.delivery_counts.delete_many',
    list_delivery_count_versions_request: 'marketdata.v1.delivery_counts_versions.list',
    get_delivery_count_version_request: 'marketdata.v1.delivery_counts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_delivery_counts_request: true,
    get_delivery_count_request: true,
    get_many_delivery_counts_request: true,
    put_delivery_count_request: true,
    put_many_delivery_counts_request: true,
    delete_delivery_count_request: true,
    delete_many_delivery_counts_request: true,
    list_delivery_count_versions_request: true,
    get_delivery_count_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.delivery_counts_events.created',
    updated: 'marketdata.v1.delivery_counts_events.updated',
    deleted: 'marketdata.v1.delivery_counts_events.deleted',
} as const;

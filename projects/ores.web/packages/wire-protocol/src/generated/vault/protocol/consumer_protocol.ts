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
import type { Consumer } from '../domain/consumer.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConsumerKey {
    name: string;
}

export interface ConsumerWrite {
    id: string;
    name: string;
    public_key: string;
    is_enabled: boolean;
    description: string;
}

export interface ConsumerChange {
    write: ConsumerWrite;
    precondition: Precondition;
}

export interface ConsumerRemoval {
    key: ConsumerKey;
    precondition: Precondition;
}

export interface ConsumerLookup {
    key: ConsumerKey;
    consumer: Consumer | null;
}

export interface ConsumersFilter {
    id_one_of: string[] | null;
    search: string | null;
}

export interface ConsumerEvent {
    event_id: string;
    key: ConsumerKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConsumerVersionKey {
    consumer: ConsumerKey;
    version: number;
}

export interface ConsumerVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConsumersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConsumersFilter | null;
    as_of: string | null;
}

export interface ListConsumersResponse {
    result: Result;
    consumers: Consumer[];
    total: number;
}

export interface GetConsumerRequest {
    key: ConsumerKey;
}

export interface GetConsumerResponse {
    result: Result;
    consumer: Consumer | null;
}

export interface GetManyConsumersRequest {
    keys: ConsumerKey[];
}

export interface GetManyConsumersResponse {
    result: Result;
    entries: ConsumerLookup[];
}

export interface PutConsumerRequest {
    change: ConsumerChange;
    intent: ChangeIntent;
}

export interface PutConsumerResponse {
    result: Result;
    consumer: Consumer | null;
}

export interface PutManyConsumersRequest {
    changes: ConsumerChange[];
    intent: ChangeIntent;
}

export interface PutManyConsumersResponse {
    result: Result;
    consumers: Consumer[];
}

export interface DeleteConsumerRequest {
    removal: ConsumerRemoval;
    intent: ChangeIntent;
}

export interface DeleteConsumerResponse {
    result: Result;
}

export interface DeleteManyConsumersRequest {
    removals: ConsumerRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConsumersResponse {
    result: Result;
}

export interface ListConsumerVersionsRequest {
    key: ConsumerKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConsumerVersionsFilter | null;
}

export interface ListConsumerVersionsResponse {
    result: Result;
    versions: Consumer[];
    total: number;
}

export interface GetConsumerVersionRequest {
    key: ConsumerVersionKey;
}

export interface GetConsumerVersionResponse {
    result: Result;
    version: Consumer | null;
}

export const subjects = {
    list_consumers_request: 'vault.v1.consumers.list',
    get_consumer_request: 'vault.v1.consumers.get',
    get_many_consumers_request: 'vault.v1.consumers.get_many',
    put_consumer_request: 'vault.v1.consumers.put',
    put_many_consumers_request: 'vault.v1.consumers.put_many',
    delete_consumer_request: 'vault.v1.consumers.delete',
    delete_many_consumers_request: 'vault.v1.consumers.delete_many',
    list_consumer_versions_request: 'vault.v1.consumers_versions.list',
    get_consumer_version_request: 'vault.v1.consumers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_consumers_request: true,
    get_consumer_request: true,
    get_many_consumers_request: true,
    put_consumer_request: true,
    put_many_consumers_request: true,
    delete_consumer_request: true,
    delete_many_consumers_request: true,
    list_consumer_versions_request: true,
    get_consumer_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'vault.v1.events.consumers.created',
    updated: 'vault.v1.events.consumers.updated',
    deleted: 'vault.v1.events.consumers.deleted',
} as const;

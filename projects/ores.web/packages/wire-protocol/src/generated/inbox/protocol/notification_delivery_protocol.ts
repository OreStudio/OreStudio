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
import type { NotificationDelivery } from '../domain/notification_delivery.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NotificationDeliveryKey {
    id: string;
}

export interface NotificationDeliveryWrite {
    id: string;
    notification_id: string;
    account_id: string;
    channel_code: string;
    attempted_at: string;
    outcome: string;
    failure_reason: string | null;
}

export interface NotificationDeliveryChange {
    write: NotificationDeliveryWrite;
    precondition: Precondition;
}

export interface NotificationDeliveryRemoval {
    key: NotificationDeliveryKey;
    precondition: Precondition;
}

export interface NotificationDeliveryLookup {
    key: NotificationDeliveryKey;
    notification_delivery: NotificationDelivery | null;
}

export interface NotificationDeliveriesFilter {
    notification_id: string | null;
    id_one_of: string[] | null;
    notification_id_one_of: string[] | null;
}

export interface NotificationDeliveryEvent {
    event_id: string;
    key: NotificationDeliveryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NotificationDeliveryVersionKey {
    notification_delivery: NotificationDeliveryKey;
    version: number;
}

export interface NotificationDeliveryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNotificationDeliveriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationDeliveriesFilter | null;
}

export interface ListNotificationDeliveriesResponse {
    result: Result;
    deliveries: NotificationDelivery[];
    total: number;
}

export interface GetNotificationDeliveryRequest {
    key: NotificationDeliveryKey;
}

export interface GetNotificationDeliveryResponse {
    result: Result;
    notification_delivery: NotificationDelivery | null;
}

export interface GetManyNotificationDeliveriesRequest {
    keys: NotificationDeliveryKey[];
}

export interface GetManyNotificationDeliveriesResponse {
    result: Result;
    entries: NotificationDeliveryLookup[];
}

export interface PutNotificationDeliveryRequest {
    change: NotificationDeliveryChange;
    intent: ChangeIntent;
}

export interface PutNotificationDeliveryResponse {
    result: Result;
    notification_delivery: NotificationDelivery | null;
}

export interface PutManyNotificationDeliveriesRequest {
    changes: NotificationDeliveryChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationDeliveriesResponse {
    result: Result;
    deliveries: NotificationDelivery[];
}

export interface DeleteNotificationDeliveryRequest {
    removal: NotificationDeliveryRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationDeliveryResponse {
    result: Result;
}

export interface DeleteManyNotificationDeliveriesRequest {
    removals: NotificationDeliveryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationDeliveriesResponse {
    result: Result;
}

export interface ListByNotificationIdNotificationDeliveriesRequest {
    notification_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationDeliveriesFilter | null;
}

export interface ListByNotificationIdNotificationDeliveriesResponse {
    result: Result;
    deliveries: NotificationDelivery[];
    total: number;
}

export interface ListNotificationDeliveryVersionsRequest {
    key: NotificationDeliveryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationDeliveryVersionsFilter | null;
}

export interface ListNotificationDeliveryVersionsResponse {
    result: Result;
    versions: NotificationDelivery[];
    total: number;
}

export interface GetNotificationDeliveryVersionRequest {
    key: NotificationDeliveryVersionKey;
}

export interface GetNotificationDeliveryVersionResponse {
    result: Result;
    version: NotificationDelivery | null;
}

export const subjects = {
    list_notification_deliveries_request: 'inbox.v1.notification_deliveries.list',
    get_notification_delivery_request: 'inbox.v1.notification_deliveries.get',
    get_many_notification_deliveries_request: 'inbox.v1.notification_deliveries.get_many',
    put_notification_delivery_request: 'inbox.v1.notification_deliveries.put',
    put_many_notification_deliveries_request: 'inbox.v1.notification_deliveries.put_many',
    delete_notification_delivery_request: 'inbox.v1.notification_deliveries.delete',
    delete_many_notification_deliveries_request: 'inbox.v1.notification_deliveries.delete_many',
    list_by_notification_id_notification_deliveries_request:
        'inbox.v1.notification_deliveries.list_by_notification_id',
    list_notification_delivery_versions_request: 'inbox.v1.notification_deliveries_versions.list',
    get_notification_delivery_version_request: 'inbox.v1.notification_deliveries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_deliveries_request: true,
    get_notification_delivery_request: true,
    get_many_notification_deliveries_request: true,
    put_notification_delivery_request: true,
    put_many_notification_deliveries_request: true,
    delete_notification_delivery_request: true,
    delete_many_notification_deliveries_request: true,
    list_by_notification_id_notification_deliveries_request: true,
    list_notification_delivery_versions_request: true,
    get_notification_delivery_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.notification_deliveries_events.created',
    updated: 'inbox.v1.notification_deliveries_events.updated',
    deleted: 'inbox.v1.notification_deliveries_events.deleted',
} as const;

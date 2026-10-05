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
import type { Notification } from '../domain/notification.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NotificationKey {
    id: string;
}

export interface NotificationWrite {
    id: string;
    kind_code: string;
    raised_by: string;
    raised_at: string;
    link_route: string;
    link_id: string | null;
    audience_permission_code: string | null;
}

export interface NotificationChange {
    write: NotificationWrite;
    precondition: Precondition;
}

export interface NotificationRemoval {
    key: NotificationKey;
    precondition: Precondition;
}

export interface NotificationLookup {
    key: NotificationKey;
    notification: Notification | null;
}

export interface NotificationsFilter {
    kind_code: string | null;
    id_one_of: string[] | null;
    kind_code_one_of: string[] | null;
}

export interface NotificationEvent {
    event_id: string;
    key: NotificationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NotificationVersionKey {
    notification: NotificationKey;
    version: number;
}

export interface NotificationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNotificationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationsFilter | null;
}

export interface ListNotificationsResponse {
    result: Result;
    notifications: Notification[];
    total: number;
}

export interface GetNotificationRequest {
    key: NotificationKey;
}

export interface GetNotificationResponse {
    result: Result;
    notification: Notification | null;
}

export interface GetManyNotificationsRequest {
    keys: NotificationKey[];
}

export interface GetManyNotificationsResponse {
    result: Result;
    entries: NotificationLookup[];
}

export interface PutNotificationRequest {
    change: NotificationChange;
    intent: ChangeIntent;
}

export interface PutNotificationResponse {
    result: Result;
    notification: Notification | null;
}

export interface PutManyNotificationsRequest {
    changes: NotificationChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationsResponse {
    result: Result;
    notifications: Notification[];
}

export interface DeleteNotificationRequest {
    removal: NotificationRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationResponse {
    result: Result;
}

export interface DeleteManyNotificationsRequest {
    removals: NotificationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationsResponse {
    result: Result;
}

export interface ListByKindCodeNotificationsRequest {
    kind_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationsFilter | null;
}

export interface ListByKindCodeNotificationsResponse {
    result: Result;
    notifications: Notification[];
    total: number;
}

export interface ListNotificationVersionsRequest {
    key: NotificationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationVersionsFilter | null;
}

export interface ListNotificationVersionsResponse {
    result: Result;
    versions: Notification[];
    total: number;
}

export interface GetNotificationVersionRequest {
    key: NotificationVersionKey;
}

export interface GetNotificationVersionResponse {
    result: Result;
    version: Notification | null;
}

export const subjects = {
    list_notifications_request: 'inbox.v1.notifications.list',
    get_notification_request: 'inbox.v1.notifications.get',
    get_many_notifications_request: 'inbox.v1.notifications.get_many',
    put_notification_request: 'inbox.v1.notifications.put',
    put_many_notifications_request: 'inbox.v1.notifications.put_many',
    delete_notification_request: 'inbox.v1.notifications.delete',
    delete_many_notifications_request: 'inbox.v1.notifications.delete_many',
    list_by_kind_code_notifications_request: 'inbox.v1.notifications.list_by_kind_code',
    list_notification_versions_request: 'inbox.v1.notifications_versions.list',
    get_notification_version_request: 'inbox.v1.notifications_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notifications_request: true,
    get_notification_request: true,
    get_many_notifications_request: true,
    put_notification_request: true,
    put_many_notifications_request: true,
    delete_notification_request: true,
    delete_many_notifications_request: true,
    list_by_kind_code_notifications_request: true,
    list_notification_versions_request: true,
    get_notification_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.notifications_events.created',
    updated: 'inbox.v1.notifications_events.updated',
    deleted: 'inbox.v1.notifications_events.deleted',
} as const;

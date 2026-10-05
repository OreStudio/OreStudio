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
import type { NotificationChannel } from '../domain/notification_channel.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NotificationChannelKey {
    code: string;
}

export interface NotificationChannelWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface NotificationChannelChange {
    write: NotificationChannelWrite;
    precondition: Precondition;
}

export interface NotificationChannelRemoval {
    key: NotificationChannelKey;
    precondition: Precondition;
}

export interface NotificationChannelLookup {
    key: NotificationChannelKey;
    notification_channel: NotificationChannel | null;
}

export interface NotificationChannelsFilter {
    code_one_of: string[] | null;
}

export interface NotificationChannelEvent {
    event_id: string;
    key: NotificationChannelKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NotificationChannelVersionKey {
    notification_channel: NotificationChannelKey;
    version: number;
}

export interface NotificationChannelVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNotificationChannelsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationChannelsFilter | null;
}

export interface ListNotificationChannelsResponse {
    result: Result;
    channels: NotificationChannel[];
    total: number;
}

export interface GetNotificationChannelRequest {
    key: NotificationChannelKey;
}

export interface GetNotificationChannelResponse {
    result: Result;
    notification_channel: NotificationChannel | null;
}

export interface GetManyNotificationChannelsRequest {
    keys: NotificationChannelKey[];
}

export interface GetManyNotificationChannelsResponse {
    result: Result;
    entries: NotificationChannelLookup[];
}

export interface PutNotificationChannelRequest {
    change: NotificationChannelChange;
    intent: ChangeIntent;
}

export interface PutNotificationChannelResponse {
    result: Result;
    notification_channel: NotificationChannel | null;
}

export interface PutManyNotificationChannelsRequest {
    changes: NotificationChannelChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationChannelsResponse {
    result: Result;
    channels: NotificationChannel[];
}

export interface DeleteNotificationChannelRequest {
    removal: NotificationChannelRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationChannelResponse {
    result: Result;
}

export interface DeleteManyNotificationChannelsRequest {
    removals: NotificationChannelRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationChannelsResponse {
    result: Result;
}

export interface ListNotificationChannelVersionsRequest {
    key: NotificationChannelKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationChannelVersionsFilter | null;
}

export interface ListNotificationChannelVersionsResponse {
    result: Result;
    versions: NotificationChannel[];
    total: number;
}

export interface GetNotificationChannelVersionRequest {
    key: NotificationChannelVersionKey;
}

export interface GetNotificationChannelVersionResponse {
    result: Result;
    version: NotificationChannel | null;
}

export const subjects = {
    list_notification_channels_request: 'inbox.v1.notification_channels.list',
    get_notification_channel_request: 'inbox.v1.notification_channels.get',
    get_many_notification_channels_request: 'inbox.v1.notification_channels.get_many',
    put_notification_channel_request: 'inbox.v1.notification_channels.put',
    put_many_notification_channels_request: 'inbox.v1.notification_channels.put_many',
    delete_notification_channel_request: 'inbox.v1.notification_channels.delete',
    delete_many_notification_channels_request: 'inbox.v1.notification_channels.delete_many',
    list_notification_channel_versions_request: 'inbox.v1.notification_channels_versions.list',
    get_notification_channel_version_request: 'inbox.v1.notification_channels_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_channels_request: true,
    get_notification_channel_request: true,
    get_many_notification_channels_request: true,
    put_notification_channel_request: true,
    put_many_notification_channels_request: true,
    delete_notification_channel_request: true,
    delete_many_notification_channels_request: true,
    list_notification_channel_versions_request: true,
    get_notification_channel_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.notification_channels_events.created',
    updated: 'inbox.v1.notification_channels_events.updated',
    deleted: 'inbox.v1.notification_channels_events.deleted',
} as const;

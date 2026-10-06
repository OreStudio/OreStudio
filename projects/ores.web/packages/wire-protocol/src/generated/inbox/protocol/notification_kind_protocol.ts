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
import type { NotificationKind } from '../domain/notification_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NotificationKindKey {
    code: string;
}

export interface NotificationKindWrite {
    code: string;
    name: string;
    description: string;
    message_key: string;
    mail_optional: boolean;
    display_order: number;
}

export interface NotificationKindChange {
    write: NotificationKindWrite;
    precondition: Precondition;
}

export interface NotificationKindRemoval {
    key: NotificationKindKey;
    precondition: Precondition;
}

export interface NotificationKindLookup {
    key: NotificationKindKey;
    notification_kind: NotificationKind | null;
}

export interface NotificationKindsFilter {
    code_one_of: string[] | null;
}

export interface NotificationKindEvent {
    event_id: string;
    key: NotificationKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NotificationKindVersionKey {
    notification_kind: NotificationKindKey;
    version: number;
}

export interface NotificationKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNotificationKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationKindsFilter | null;
    as_of: string | null;
}

export interface ListNotificationKindsResponse {
    result: Result;
    notification_kinds: NotificationKind[];
    total: number;
}

export interface GetNotificationKindRequest {
    key: NotificationKindKey;
}

export interface GetNotificationKindResponse {
    result: Result;
    notification_kind: NotificationKind | null;
}

export interface GetManyNotificationKindsRequest {
    keys: NotificationKindKey[];
}

export interface GetManyNotificationKindsResponse {
    result: Result;
    entries: NotificationKindLookup[];
}

export interface PutNotificationKindRequest {
    change: NotificationKindChange;
    intent: ChangeIntent;
}

export interface PutNotificationKindResponse {
    result: Result;
    notification_kind: NotificationKind | null;
}

export interface PutManyNotificationKindsRequest {
    changes: NotificationKindChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationKindsResponse {
    result: Result;
    notification_kinds: NotificationKind[];
}

export interface DeleteNotificationKindRequest {
    removal: NotificationKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationKindResponse {
    result: Result;
}

export interface DeleteManyNotificationKindsRequest {
    removals: NotificationKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationKindsResponse {
    result: Result;
}

export interface ListNotificationKindVersionsRequest {
    key: NotificationKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationKindVersionsFilter | null;
}

export interface ListNotificationKindVersionsResponse {
    result: Result;
    versions: NotificationKind[];
    total: number;
}

export interface GetNotificationKindVersionRequest {
    key: NotificationKindVersionKey;
}

export interface GetNotificationKindVersionResponse {
    result: Result;
    version: NotificationKind | null;
}

export const subjects = {
    list_notification_kinds_request: 'inbox.v1.notification_kinds.list',
    get_notification_kind_request: 'inbox.v1.notification_kinds.get',
    get_many_notification_kinds_request: 'inbox.v1.notification_kinds.get_many',
    put_notification_kind_request: 'inbox.v1.notification_kinds.put',
    put_many_notification_kinds_request: 'inbox.v1.notification_kinds.put_many',
    delete_notification_kind_request: 'inbox.v1.notification_kinds.delete',
    delete_many_notification_kinds_request: 'inbox.v1.notification_kinds.delete_many',
    list_notification_kind_versions_request: 'inbox.v1.notification_kinds_versions.list',
    get_notification_kind_version_request: 'inbox.v1.notification_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_kinds_request: true,
    get_notification_kind_request: true,
    get_many_notification_kinds_request: true,
    put_notification_kind_request: true,
    put_many_notification_kinds_request: true,
    delete_notification_kind_request: true,
    delete_many_notification_kinds_request: true,
    list_notification_kind_versions_request: true,
    get_notification_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.notification_kinds_events.created',
    updated: 'inbox.v1.notification_kinds_events.updated',
    deleted: 'inbox.v1.notification_kinds_events.deleted',
} as const;

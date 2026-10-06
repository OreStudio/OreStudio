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
import type { NotificationPreference } from '../domain/notification_preference.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NotificationPreferenceKey {
    account_id: string;
    kind_code: string;
    channel_code: string;
}

export interface NotificationPreferenceWrite {
    account_id: string;
    kind_code: string;
    channel_code: string;
    enabled: boolean;
}

export interface NotificationPreferenceChange {
    write: NotificationPreferenceWrite;
    precondition: Precondition;
}

export interface NotificationPreferenceRemoval {
    key: NotificationPreferenceKey;
    precondition: Precondition;
}

export interface NotificationPreferenceLookup {
    key: NotificationPreferenceKey;
    notification_preference: NotificationPreference | null;
}

export interface NotificationPreferencesFilter {
    account_id: string | null;
    account_id_one_of: string[] | null;
}

export interface NotificationPreferenceEvent {
    event_id: string;
    key: NotificationPreferenceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NotificationPreferenceVersionKey {
    notification_preference: NotificationPreferenceKey;
    version: number;
}

export interface NotificationPreferenceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNotificationPreferencesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationPreferencesFilter | null;
    as_of: string | null;
}

export interface ListNotificationPreferencesResponse {
    result: Result;
    preferences: NotificationPreference[];
    total: number;
}

export interface GetNotificationPreferenceRequest {
    key: NotificationPreferenceKey;
}

export interface GetNotificationPreferenceResponse {
    result: Result;
    notification_preference: NotificationPreference | null;
}

export interface GetManyNotificationPreferencesRequest {
    keys: NotificationPreferenceKey[];
}

export interface GetManyNotificationPreferencesResponse {
    result: Result;
    entries: NotificationPreferenceLookup[];
}

export interface PutNotificationPreferenceRequest {
    change: NotificationPreferenceChange;
    intent: ChangeIntent;
}

export interface PutNotificationPreferenceResponse {
    result: Result;
    notification_preference: NotificationPreference | null;
}

export interface PutManyNotificationPreferencesRequest {
    changes: NotificationPreferenceChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationPreferencesResponse {
    result: Result;
    preferences: NotificationPreference[];
}

export interface DeleteNotificationPreferenceRequest {
    removal: NotificationPreferenceRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationPreferenceResponse {
    result: Result;
}

export interface DeleteManyNotificationPreferencesRequest {
    removals: NotificationPreferenceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationPreferencesResponse {
    result: Result;
}

export interface ListByAccountIdNotificationPreferencesRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationPreferencesFilter | null;
}

export interface ListByAccountIdNotificationPreferencesResponse {
    result: Result;
    preferences: NotificationPreference[];
    total: number;
}

export interface ListNotificationPreferenceVersionsRequest {
    key: NotificationPreferenceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationPreferenceVersionsFilter | null;
}

export interface ListNotificationPreferenceVersionsResponse {
    result: Result;
    versions: NotificationPreference[];
    total: number;
}

export interface GetNotificationPreferenceVersionRequest {
    key: NotificationPreferenceVersionKey;
}

export interface GetNotificationPreferenceVersionResponse {
    result: Result;
    version: NotificationPreference | null;
}

export const subjects = {
    list_notification_preferences_request: 'inbox.v1.notification_preferences.list',
    get_notification_preference_request: 'inbox.v1.notification_preferences.get',
    get_many_notification_preferences_request: 'inbox.v1.notification_preferences.get_many',
    put_notification_preference_request: 'inbox.v1.notification_preferences.put',
    put_many_notification_preferences_request: 'inbox.v1.notification_preferences.put_many',
    delete_notification_preference_request: 'inbox.v1.notification_preferences.delete',
    delete_many_notification_preferences_request: 'inbox.v1.notification_preferences.delete_many',
    list_by_account_id_notification_preferences_request:
        'inbox.v1.notification_preferences.list_by_account_id',
    list_notification_preference_versions_request:
        'inbox.v1.notification_preferences_versions.list',
    get_notification_preference_version_request: 'inbox.v1.notification_preferences_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_preferences_request: true,
    get_notification_preference_request: true,
    get_many_notification_preferences_request: true,
    put_notification_preference_request: true,
    put_many_notification_preferences_request: true,
    delete_notification_preference_request: true,
    delete_many_notification_preferences_request: true,
    list_by_account_id_notification_preferences_request: true,
    list_notification_preference_versions_request: true,
    get_notification_preference_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.notification_preferences_events.created',
    updated: 'inbox.v1.notification_preferences_events.updated',
    deleted: 'inbox.v1.notification_preferences_events.deleted',
} as const;

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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief One value a notification's message names.
 */
export interface NotificationArgumentValue {
    name: string;
    value: string;
}

/**
 * @brief One notification as its recipient sees it: what happened, where it
 * is dealt with, the values its message names, and the recipient's own read
 * state.
 */
export interface InboxNotification {
    id: string;
    kind_code: string;
    /**
     * @brief The message the screen renders in the reader's language.
     */
    message_key: string;
    /**
     * @brief The username of the account that raised it.
     */
    raised_by: string;
    raised_at: string;
    link_route: string;
    /**
     * @brief The thing on the linked screen, or empty when the screen is the
     * whole of the link.
     */
    link_id: string;
    arguments: NotificationArgumentValue[];
    /**
     * @brief When the recipient read it, or empty while it is unread.
     */
    read_at: string;
}

/**
 * @brief Tells people that something happened.
 *
 * The audience is the named accounts, the holders of a permission in the
 * tenant, or both. The raiser is the signed-in account, a person or a
 * service. Raising never fails the activity that asks for it: a caller
 * logs an unsuccessful outcome and carries on.
 */
export interface RaiseNotificationRequest {
    kind_code: string;
    link_route: string;
    link_id: string;
    arguments: NotificationArgumentValue[];
    /**
     * @brief The accounts told by name, as UUID strings.
     */
    account_ids: string[];
    /**
     * @brief The permission whose holders are told, or empty for none.
     */
    audience_permission_code: string;
}

export interface RaiseNotificationResponse {
    result: Result;
    notification_id: string;
    /**
     * @brief How many people the notification reached.
     */
    recipient_count: number;
}

/**
 * @brief Reads the signed-in person's notifications that they have not
 * cleared, newest first.
 */
export interface ListMyNotificationsRequest {
    unread_only: boolean;
    offset: number;
    limit: number;
}

export interface ListMyNotificationsResponse {
    result: Result;
    notifications: InboxNotification[];
    total: number;
}

/**
 * @brief Reads how many of the signed-in person's notifications are unread,
 * for the bell on every screen.
 */
export interface CountUnreadNotificationsRequest {}

export interface CountUnreadNotificationsResponse {
    result: Result;
    unread: number;
}

/**
 * @brief Marks some or all of the signed-in person's notifications read.
 */
export interface MarkNotificationsReadRequest {
    /**
     * @brief The notifications to mark, or empty for every unread one.
     */
    notification_ids: string[];
}

export interface MarkNotificationsReadResponse {
    result: Result;
    marked: number;
}

/**
 * @brief Removes notifications from the signed-in person's list. A cleared
 * notification is also read.
 */
export interface ClearNotificationsRequest {
    /**
     * @brief The notifications to clear, or empty for every read one.
     */
    notification_ids: string[];
}

export interface ClearNotificationsResponse {
    result: Result;
    cleared: number;
}

export const subjects = {
    raise_notification_request: 'inbox.v1.ops.raise_notification',
    list_my_notifications_request: 'inbox.v1.ops.list_my_notifications',
    count_unread_notifications_request: 'inbox.v1.ops.count_unread_notifications',
    mark_notifications_read_request: 'inbox.v1.ops.mark_notifications_read',
    clear_notifications_request: 'inbox.v1.ops.clear_notifications',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    raise_notification_request: true,
    list_my_notifications_request: true,
    count_unread_notifications_request: true,
    mark_notifications_read_request: true,
    clear_notifications_request: true,
} as const;

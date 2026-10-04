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
import type { NotificationRecipient } from '../domain/notification_recipient.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NotificationRecipientKey {
    notification_id: string;
    account_id: string;
}

export interface NotificationRecipientWrite {
    notification_id: string;
    account_id: string;
    read_at: string | null;
    cleared_at: string | null;
}

export interface NotificationRecipientChange {
    write: NotificationRecipientWrite;
    precondition: Precondition;
}

export interface NotificationRecipientRemoval {
    key: NotificationRecipientKey;
    precondition: Precondition;
}

export interface NotificationRecipientLookup {
    key: NotificationRecipientKey;
    notification_recipient: NotificationRecipient | null;
}

export interface NotificationRecipientsFilter {
    account_id: string | null;
    account_id_one_of: string[] | null;
}

export interface ListNotificationRecipientsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationRecipientsFilter | null;
}

export interface ListNotificationRecipientsResponse {
    result: Result;
    notification_recipients: NotificationRecipient[];
    total: number;
}

export interface GetNotificationRecipientRequest {
    key: NotificationRecipientKey;
}

export interface GetNotificationRecipientResponse {
    result: Result;
    notification_recipient: NotificationRecipient | null;
}

export interface GetManyNotificationRecipientsRequest {
    keys: NotificationRecipientKey[];
}

export interface GetManyNotificationRecipientsResponse {
    result: Result;
    entries: NotificationRecipientLookup[];
}

export interface PutNotificationRecipientRequest {
    change: NotificationRecipientChange;
    intent: ChangeIntent;
}

export interface PutNotificationRecipientResponse {
    result: Result;
    notification_recipient: NotificationRecipient | null;
}

export interface PutManyNotificationRecipientsRequest {
    changes: NotificationRecipientChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationRecipientsResponse {
    result: Result;
    notification_recipients: NotificationRecipient[];
}

export interface DeleteNotificationRecipientRequest {
    removal: NotificationRecipientRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationRecipientResponse {
    result: Result;
}

export interface DeleteManyNotificationRecipientsRequest {
    removals: NotificationRecipientRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationRecipientsResponse {
    result: Result;
}

export interface ListByAccountIdNotificationRecipientsRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationRecipientsFilter | null;
}

export interface ListByAccountIdNotificationRecipientsResponse {
    result: Result;
    notification_recipients: NotificationRecipient[];
    total: number;
}

export const subjects = {
    list_notification_recipients_request: 'inbox.v1.notification_recipients.list',
    get_notification_recipient_request: 'inbox.v1.notification_recipients.get',
    get_many_notification_recipients_request: 'inbox.v1.notification_recipients.get_many',
    put_notification_recipient_request: 'inbox.v1.notification_recipients.put',
    put_many_notification_recipients_request: 'inbox.v1.notification_recipients.put_many',
    delete_notification_recipient_request: 'inbox.v1.notification_recipients.delete',
    delete_many_notification_recipients_request: 'inbox.v1.notification_recipients.delete_many',
    list_by_account_id_notification_recipients_request:
        'inbox.v1.notification_recipients.list_by_account_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_recipients_request: true,
    get_notification_recipient_request: true,
    get_many_notification_recipients_request: true,
    put_notification_recipient_request: true,
    put_many_notification_recipients_request: true,
    delete_notification_recipient_request: true,
    delete_many_notification_recipients_request: true,
    list_by_account_id_notification_recipients_request: true,
} as const;

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
import type { NotificationArgument } from '../domain/notification_argument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NotificationArgumentKey {
    notification_id: string;
    name: string;
}

export interface NotificationArgumentWrite {
    notification_id: string;
    name: string;
    value: string;
}

export interface NotificationArgumentChange {
    write: NotificationArgumentWrite;
    precondition: Precondition;
}

export interface NotificationArgumentRemoval {
    key: NotificationArgumentKey;
    precondition: Precondition;
}

export interface NotificationArgumentLookup {
    key: NotificationArgumentKey;
    notification_argument: NotificationArgument | null;
}

export interface NotificationArgumentsFilter {
    notification_id: string | null;
    notification_id_one_of: string[] | null;
}

export interface ListNotificationArgumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationArgumentsFilter | null;
}

export interface ListNotificationArgumentsResponse {
    result: Result;
    notification_arguments: NotificationArgument[];
    total: number;
}

export interface GetNotificationArgumentRequest {
    key: NotificationArgumentKey;
}

export interface GetNotificationArgumentResponse {
    result: Result;
    notification_argument: NotificationArgument | null;
}

export interface GetManyNotificationArgumentsRequest {
    keys: NotificationArgumentKey[];
}

export interface GetManyNotificationArgumentsResponse {
    result: Result;
    entries: NotificationArgumentLookup[];
}

export interface PutNotificationArgumentRequest {
    change: NotificationArgumentChange;
    intent: ChangeIntent;
}

export interface PutNotificationArgumentResponse {
    result: Result;
    notification_argument: NotificationArgument | null;
}

export interface PutManyNotificationArgumentsRequest {
    changes: NotificationArgumentChange[];
    intent: ChangeIntent;
}

export interface PutManyNotificationArgumentsResponse {
    result: Result;
    notification_arguments: NotificationArgument[];
}

export interface DeleteNotificationArgumentRequest {
    removal: NotificationArgumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteNotificationArgumentResponse {
    result: Result;
}

export interface DeleteManyNotificationArgumentsRequest {
    removals: NotificationArgumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNotificationArgumentsResponse {
    result: Result;
}

export interface ListByNotificationIdNotificationArgumentsRequest {
    notification_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NotificationArgumentsFilter | null;
}

export interface ListByNotificationIdNotificationArgumentsResponse {
    result: Result;
    notification_arguments: NotificationArgument[];
    total: number;
}

export const subjects = {
    list_notification_arguments_request: 'inbox.v1.notification_arguments.list',
    get_notification_argument_request: 'inbox.v1.notification_arguments.get',
    get_many_notification_arguments_request: 'inbox.v1.notification_arguments.get_many',
    put_notification_argument_request: 'inbox.v1.notification_arguments.put',
    put_many_notification_arguments_request: 'inbox.v1.notification_arguments.put_many',
    delete_notification_argument_request: 'inbox.v1.notification_arguments.delete',
    delete_many_notification_arguments_request: 'inbox.v1.notification_arguments.delete_many',
    list_by_notification_id_notification_arguments_request:
        'inbox.v1.notification_arguments.list_by_notification_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_notification_arguments_request: true,
    get_notification_argument_request: true,
    get_many_notification_arguments_request: true,
    put_notification_argument_request: true,
    put_many_notification_arguments_request: true,
    delete_notification_argument_request: true,
    delete_many_notification_arguments_request: true,
    list_by_notification_id_notification_arguments_request: true,
} as const;

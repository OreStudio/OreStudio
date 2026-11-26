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
import type { SecretUse } from '../domain/secret_use.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SecretUseKey {
    id: string;
}

export interface SecretUseWrite {
    id: string;
    name: string;
    subject: string;
    outcome: string;
    key_version: number;
    used_at: string;
}

export interface SecretUseChange {
    write: SecretUseWrite;
    precondition: Precondition;
}

export interface SecretUseRemoval {
    key: SecretUseKey;
    precondition: Precondition;
}

export interface SecretUseLookup {
    key: SecretUseKey;
    secret_use: SecretUse | null;
}

export interface SecretUsesFilter {
    id_one_of: string[] | null;
}

export interface SecretUseEvent {
    event_id: string;
    key: SecretUseKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListSecretUsesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SecretUsesFilter | null;
}

export interface ListSecretUsesResponse {
    result: Result;
    secret_uses: SecretUse[];
    total: number;
}

export interface GetSecretUseRequest {
    key: SecretUseKey;
}

export interface GetSecretUseResponse {
    result: Result;
    secret_use: SecretUse | null;
}

export interface GetManySecretUsesRequest {
    keys: SecretUseKey[];
}

export interface GetManySecretUsesResponse {
    result: Result;
    entries: SecretUseLookup[];
}

export interface PutSecretUseRequest {
    change: SecretUseChange;
    intent: ChangeIntent;
}

export interface PutSecretUseResponse {
    result: Result;
    secret_use: SecretUse | null;
}

export interface PutManySecretUsesRequest {
    changes: SecretUseChange[];
    intent: ChangeIntent;
}

export interface PutManySecretUsesResponse {
    result: Result;
    secret_uses: SecretUse[];
}

export interface DeleteSecretUseRequest {
    removal: SecretUseRemoval;
    intent: ChangeIntent;
}

export interface DeleteSecretUseResponse {
    result: Result;
}

export interface DeleteManySecretUsesRequest {
    removals: SecretUseRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySecretUsesResponse {
    result: Result;
}

export const subjects = {
    list_secret_uses_request: 'vault.v1.secret_uses.list',
    get_secret_use_request: 'vault.v1.secret_uses.get',
    get_many_secret_uses_request: 'vault.v1.secret_uses.get_many',
    put_secret_use_request: 'vault.v1.secret_uses.put',
    put_many_secret_uses_request: 'vault.v1.secret_uses.put_many',
    delete_secret_use_request: 'vault.v1.secret_uses.delete',
    delete_many_secret_uses_request: 'vault.v1.secret_uses.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_secret_uses_request: true,
    get_secret_use_request: true,
    get_many_secret_uses_request: true,
    put_secret_use_request: true,
    put_many_secret_uses_request: true,
    delete_secret_use_request: true,
    delete_many_secret_uses_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'vault.v1.events.secret_uses.created',
    updated: 'vault.v1.events.secret_uses.updated',
    deleted: 'vault.v1.events.secret_uses.deleted',
} as const;

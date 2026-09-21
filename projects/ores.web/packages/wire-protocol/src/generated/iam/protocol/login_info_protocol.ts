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
import type { LoginInfo } from '../domain/login_info.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LoginInfoKey {
    account_id: string;
}

export interface LoginInfoWrite {
    account_id: string;
    last_ip: string;
    last_attempt_ip: string;
    failed_logins: number;
    locked: boolean;
    last_login: string;
    online: boolean;
    password_reset_required: boolean;
}

export interface LoginInfoChange {
    write: LoginInfoWrite;
    precondition: Precondition;
}

export interface LoginInfoRemoval {
    key: LoginInfoKey;
    precondition: Precondition;
}

export interface LoginInfoLookup {
    key: LoginInfoKey;
    login_info: LoginInfo | null;
}

export interface ListLoginInfoRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLoginInfoResponse {
    result: Result;
    login_info: LoginInfo[];
    total: number;
}

export interface GetLoginInfoRequest {
    key: LoginInfoKey;
}

export interface GetLoginInfoResponse {
    result: Result;
    login_info: LoginInfo | null;
}

export interface GetManyLoginInfoRequest {
    keys: LoginInfoKey[];
}

export interface GetManyLoginInfoResponse {
    result: Result;
    entries: LoginInfoLookup[];
}

export interface PutLoginInfoRequest {
    change: LoginInfoChange;
    intent: ChangeIntent;
}

export interface PutLoginInfoResponse {
    result: Result;
    login_info: LoginInfo;
}

export interface PutManyLoginInfoRequest {
    changes: LoginInfoChange[];
    intent: ChangeIntent;
}

export interface PutManyLoginInfoResponse {
    result: Result;
    login_info: LoginInfo[];
}

export interface DeleteLoginInfoRequest {
    removal: LoginInfoRemoval;
    intent: ChangeIntent;
}

export interface DeleteLoginInfoResponse {
    result: Result;
}

export interface DeleteManyLoginInfoRequest {
    removals: LoginInfoRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLoginInfoResponse {
    result: Result;
}

export const subjects = {
    list_login_info_request: "iam.v1.login_info.list",
    get_login_info_request: "iam.v1.login_info.get",
    get_many_login_info_request: "iam.v1.login_info.get_many",
    put_login_info_request: "iam.v1.login_info.put",
    put_many_login_info_request: "iam.v1.login_info.put_many",
    delete_login_info_request: "iam.v1.login_info.delete",
    delete_many_login_info_request: "iam.v1.login_info.delete_many",
} as const;

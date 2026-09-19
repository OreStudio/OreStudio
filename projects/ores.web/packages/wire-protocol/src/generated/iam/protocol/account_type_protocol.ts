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
import type { AccountType } from '../domain/account_type.js';

export interface GetAccountTypesRequest {
    offset: number;
    limit: number;
}

export interface GetAccountTypesResponse {
    types: AccountType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAccountTypeRequest {
    data: AccountType;
}

export interface SaveAccountTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteAccountTypeRequest {
    types: string[];
}

export interface DeleteAccountTypeResponse {
    success: boolean;
    message: string;
}

export interface GetAccountTypeHistoryRequest {
    type: string;
}

export interface GetAccountTypeHistoryResponse {
    history: AccountType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_account_types_request: "iam.v1.account_types.list",
    save_account_type_request: "iam.v1.account_types.save",
    delete_account_type_request: "iam.v1.account_types.delete",
    get_account_type_history_request: "iam.v1.account_types.history",
} as const;

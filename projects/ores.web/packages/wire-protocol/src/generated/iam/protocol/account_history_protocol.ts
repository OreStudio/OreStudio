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
import type { Account } from '../domain/account.js';

/**
 * @brief One version of an account, with the metadata a history view
 * renders beside it.
 */
export interface AccountVersion {
    /**
     * @brief The account data at this version.
     */
    data: Account;
    /**
     * @brief Version number (1-based, higher is newer).
     */
    version_number: number;
    /**
     * @brief Username of the person who recorded this version in the system.
     */
    modified_by: string;
    /**
     * @brief Timestamp when this version was recorded in the system.
     */
    recorded_at: string;
    /**
     * @brief Summary of changes made in this version.
     *
     * Examples: "Created account", "Modified 2 fields", "Updated email".
     */
    change_summary: string;
}

export interface AccountVersionHistory {
    versions: AccountVersion[];
}

export interface GetAccountHistoryRequest {
    username: string;
}

export interface GetAccountHistoryResponse {
    success: boolean;
    message: string;
    history: AccountVersionHistory;
}

export const subjects = {
    get_account_history_request: "iam.v1.accounts.history",
} as const;

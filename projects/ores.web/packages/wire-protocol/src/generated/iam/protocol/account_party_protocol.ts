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
import type { AccountParty } from '../domain/account_party.js';

export interface AccountPartyKey {
    account_id: string;
    party_id: string;
}

export interface GetAccountPartiesRequest {
    offset: number;
    limit: number;
}

export interface GetAccountPartiesResponse {
    account_parties: AccountParty[];
    total_available_count: number;
}

export interface GetAccountPartiesByAccountRequest {
    account_id: string;
}

export interface GetAccountPartiesByAccountResponse {
    account_parties: AccountParty[];
}

export interface SaveAccountPartyRequest {
    account_parties: AccountParty[];
}

export interface SaveAccountPartyResponse {
    success: boolean;
    message: string;
}

export interface DeleteAccountPartyRequest {
    keys: AccountPartyKey[];
}

export interface DeleteAccountPartyResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    get_account_parties_request: "iam.v1.account-parties.list",
    get_account_parties_by_account_request: "iam.v1.account-parties.by-account",
    save_account_party_request: "iam.v1.account-parties.save",
    delete_account_party_request: "iam.v1.account-parties.delete",
} as const;

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

export interface GetAccountPartiesRequest {
    offset: number;
    limit: number;
}

export interface GetAccountPartiesResponse {
    account_parties: AccountParty[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetAccountPartiesByAccountRequest {
    account_id: string;
    offset: number;
    limit: number;
}

export interface GetAccountPartiesByAccountResponse {
    account_parties: AccountPartyView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAccountPartyRequest {
    account_parties: AccountParty[];
}

export interface SaveAccountPartyResponse {
    success: boolean;
    message: string;
}

export interface DeleteAccountPartyRequest {
    account_ids: string[];
    party_ids: string[];
}

export interface DeleteAccountPartyResponse {
    success: boolean;
    message: string;
}

export interface ReplaceAccountPartiesByAccountRequest {
    account_id: string;
    account_parties: AccountParty[];
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface ReplaceAccountPartiesByAccountResponse {
    success: boolean;
    message: string;
}

export interface CountAccountPartiesByAccountRequest {
    account_id: string;
}

export interface CountAccountPartiesByAccountResponse {
    total_available_count: number;
}

export interface CountAccountPartiesByPartyRequest {
    party_id: string;
}

export interface CountAccountPartiesByPartyResponse {
    total_available_count: number;
}

export interface AccountPartyView {
    account_party: AccountParty;
}

export const subjects = {
    get_account_parties_request: "iam.v1.account_parties.list",
    get_account_parties_by_account_request: "iam.v1.account_parties.list_by_account_id",
    save_account_party_request: "iam.v1.account_parties.save",
    delete_account_party_request: "iam.v1.account_parties.delete",
    replace_account_parties_by_account_request: "iam.v1.account_parties.replace_by_account_id",
    count_account_parties_by_account_request: "iam.v1.account_parties.count_by_account_id",
    count_account_parties_by_party_request: "iam.v1.account_parties.count_by_party_id",
} as const;

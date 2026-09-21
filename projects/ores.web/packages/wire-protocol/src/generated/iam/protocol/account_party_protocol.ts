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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AccountPartyKey {
    account_id: string;
    party_id: string;
}

export interface AccountPartyWrite {
    account_id: string;
    party_id: string;
}

export interface AccountPartyChange {
    write: AccountPartyWrite;
    precondition: Precondition;
}

export interface AccountPartyRemoval {
    key: AccountPartyKey;
    precondition: Precondition;
}

export interface AccountPartyLookup {
    key: AccountPartyKey;
    account_party: AccountParty | null;
}

export interface AccountPartiesFilter {
    account_id: string | null;
}

export interface ListAccountPartiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AccountPartiesFilter | null;
}

export interface ListAccountPartiesResponse {
    result: Result;
    account_parties: AccountParty[];
    total: number;
}

export interface GetAccountPartyRequest {
    key: AccountPartyKey;
}

export interface GetAccountPartyResponse {
    result: Result;
    account_party: AccountParty | null;
}

export interface GetManyAccountPartiesRequest {
    keys: AccountPartyKey[];
}

export interface GetManyAccountPartiesResponse {
    result: Result;
    entries: AccountPartyLookup[];
}

export interface PutAccountPartyRequest {
    change: AccountPartyChange;
    intent: ChangeIntent;
}

export interface PutAccountPartyResponse {
    result: Result;
    account_party: AccountParty;
}

export interface PutManyAccountPartiesRequest {
    changes: AccountPartyChange[];
    intent: ChangeIntent;
}

export interface PutManyAccountPartiesResponse {
    result: Result;
    account_parties: AccountParty[];
}

export interface DeleteAccountPartyRequest {
    removal: AccountPartyRemoval;
    intent: ChangeIntent;
}

export interface DeleteAccountPartyResponse {
    result: Result;
}

export interface DeleteManyAccountPartiesRequest {
    removals: AccountPartyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAccountPartiesResponse {
    result: Result;
}

export interface ListByAccountIdAccountPartiesRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountPartiesFilter | null;
}

export interface ListByAccountIdAccountPartiesResponse {
    result: Result;
    account_parties: AccountParty[];
    total: number;
}

export const subjects = {
    list_account_parties_request: "iam.v1.account_parties.list",
    get_account_party_request: "iam.v1.account_parties.get",
    get_many_account_parties_request: "iam.v1.account_parties.get_many",
    put_account_party_request: "iam.v1.account_parties.put",
    put_many_account_parties_request: "iam.v1.account_parties.put_many",
    delete_account_party_request: "iam.v1.account_parties.delete",
    delete_many_account_parties_request: "iam.v1.account_parties.delete_many",
    list_by_account_id_account_parties_request: "iam.v1.account_parties.list_by_account_id",
} as const;

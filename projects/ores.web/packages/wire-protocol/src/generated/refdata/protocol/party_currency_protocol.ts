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
import type { PartyCurrency } from '../domain/party_currency.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyCurrencyKey {
    party_id: string;
    currency_iso_code: string;
}

export interface PartyCurrencyWrite {
    party_id: string;
    currency_iso_code: string;
}

export interface PartyCurrencyChange {
    write: PartyCurrencyWrite;
    precondition: Precondition;
}

export interface PartyCurrencyRemoval {
    key: PartyCurrencyKey;
    precondition: Precondition;
}

export interface PartyCurrencyLookup {
    key: PartyCurrencyKey;
    party_currency: PartyCurrency | null;
}

export interface PartyCurrenciesFilter {
    party_id: string | null;
}

export interface PartyCurrencyEvent {
    event_id: string;
    key: PartyCurrencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPartyCurrenciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCurrenciesFilter | null;
}

export interface ListPartyCurrenciesResponse {
    result: Result;
    party_currencies: PartyCurrency[];
    total: number;
}

export interface GetPartyCurrencyRequest {
    key: PartyCurrencyKey;
}

export interface GetPartyCurrencyResponse {
    result: Result;
    party_currency: PartyCurrency | null;
}

export interface GetManyPartyCurrenciesRequest {
    keys: PartyCurrencyKey[];
}

export interface GetManyPartyCurrenciesResponse {
    result: Result;
    entries: PartyCurrencyLookup[];
}

export interface PutPartyCurrencyRequest {
    change: PartyCurrencyChange;
    intent: ChangeIntent;
}

export interface PutPartyCurrencyResponse {
    result: Result;
    party_currency: PartyCurrency;
}

export interface PutManyPartyCurrenciesRequest {
    changes: PartyCurrencyChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyCurrenciesResponse {
    result: Result;
    party_currencies: PartyCurrency[];
}

export interface DeletePartyCurrencyRequest {
    removal: PartyCurrencyRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyCurrencyResponse {
    result: Result;
}

export interface DeleteManyPartyCurrenciesRequest {
    removals: PartyCurrencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyCurrenciesResponse {
    result: Result;
}

export interface ListByPartyIdPartyCurrenciesRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCurrenciesFilter | null;
}

export interface ListByPartyIdPartyCurrenciesResponse {
    result: Result;
    party_currencies: PartyCurrency[];
    total: number;
}

export const subjects = {
    list_party_currencies_request: "refdata.v1.party_currencies.list",
    get_party_currency_request: "refdata.v1.party_currencies.get",
    get_many_party_currencies_request: "refdata.v1.party_currencies.get_many",
    put_party_currency_request: "refdata.v1.party_currencies.put",
    put_many_party_currencies_request: "refdata.v1.party_currencies.put_many",
    delete_party_currency_request: "refdata.v1.party_currencies.delete",
    delete_many_party_currencies_request: "refdata.v1.party_currencies.delete_many",
    list_by_party_id_party_currencies_request: "refdata.v1.party_currencies.list_by_party_id",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_currencies_request: true,
    get_party_currency_request: true,
    get_many_party_currencies_request: true,
    put_party_currency_request: true,
    put_many_party_currencies_request: true,
    delete_party_currency_request: true,
    delete_many_party_currencies_request: true,
    list_by_party_id_party_currencies_request: true,
} as const;

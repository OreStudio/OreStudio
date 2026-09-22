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
import type { CurrencyPair } from '../domain/currency_pair.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyPairKey {
    pair_code: string;
}

export interface CurrencyPairWrite {
    pair_code: string;
    base_currency: string;
    quote_currency: string;
    classification: string;
}

export interface CurrencyPairChange {
    write: CurrencyPairWrite;
    precondition: Precondition;
}

export interface CurrencyPairRemoval {
    key: CurrencyPairKey;
    precondition: Precondition;
}

export interface CurrencyPairLookup {
    key: CurrencyPairKey;
    currency_pair: CurrencyPair | null;
}

export interface CurrencyPairEvent {
    event_id: string;
    key: CurrencyPairKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyPairVersionKey {
    currency_pair: CurrencyPairKey;
    version: number;
}

export interface CurrencyPairVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrencyPairsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurrencyPairsResponse {
    result: Result;
    pairs: CurrencyPair[];
    total: number;
}

export interface GetCurrencyPairRequest {
    key: CurrencyPairKey;
}

export interface GetCurrencyPairResponse {
    result: Result;
    currency_pair: CurrencyPair | null;
}

export interface GetManyCurrencyPairsRequest {
    keys: CurrencyPairKey[];
}

export interface GetManyCurrencyPairsResponse {
    result: Result;
    entries: CurrencyPairLookup[];
}

export interface PutCurrencyPairRequest {
    change: CurrencyPairChange;
    intent: ChangeIntent;
}

export interface PutCurrencyPairResponse {
    result: Result;
    currency_pair: CurrencyPair;
}

export interface PutManyCurrencyPairsRequest {
    changes: CurrencyPairChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyPairsResponse {
    result: Result;
    pairs: CurrencyPair[];
}

export interface DeleteCurrencyPairRequest {
    removal: CurrencyPairRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyPairResponse {
    result: Result;
}

export interface DeleteManyCurrencyPairsRequest {
    removals: CurrencyPairRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyPairsResponse {
    result: Result;
}

export interface ListCurrencyPairVersionsRequest {
    key: CurrencyPairKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyPairVersionsFilter | null;
}

export interface ListCurrencyPairVersionsResponse {
    result: Result;
    versions: CurrencyPair[];
    total: number;
}

export interface GetCurrencyPairVersionRequest {
    key: CurrencyPairVersionKey;
}

export interface GetCurrencyPairVersionResponse {
    result: Result;
    version: CurrencyPair;
}

export const subjects = {
    list_currency_pairs_request: "refdata.v1.currency_pairs.list",
    get_currency_pair_request: "refdata.v1.currency_pairs.get",
    get_many_currency_pairs_request: "refdata.v1.currency_pairs.get_many",
    put_currency_pair_request: "refdata.v1.currency_pairs.put",
    put_many_currency_pairs_request: "refdata.v1.currency_pairs.put_many",
    delete_currency_pair_request: "refdata.v1.currency_pairs.delete",
    delete_many_currency_pairs_request: "refdata.v1.currency_pairs.delete_many",
    list_currency_pair_versions_request: "refdata.v1.currency_pairs_versions.list",
    get_currency_pair_version_request: "refdata.v1.currency_pairs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_pairs_request: true,
    get_currency_pair_request: true,
    get_many_currency_pairs_request: true,
    put_currency_pair_request: true,
    put_many_currency_pairs_request: true,
    delete_currency_pair_request: true,
    delete_many_currency_pairs_request: true,
    list_currency_pair_versions_request: true,
    get_currency_pair_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currency_pairs_events.created",
    updated: "refdata.v1.currency_pairs_events.updated",
    deleted: "refdata.v1.currency_pairs_events.deleted",
} as const;

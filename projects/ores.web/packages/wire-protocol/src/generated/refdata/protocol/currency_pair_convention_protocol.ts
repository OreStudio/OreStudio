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
import type { CurrencyPairConvention } from '../domain/currency_pair_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyPairConventionKey {
    pair_code: string;
}

export interface CurrencyPairConventionWrite {
    pair_code: string;
    pip_factor: number;
    tick_size: number;
    decimal_places: number;
    business_day_convention: string | null;
    spot_relative: boolean | null;
    end_of_month: boolean | null;
}

export interface CurrencyPairConventionChange {
    write: CurrencyPairConventionWrite;
    precondition: Precondition;
}

export interface CurrencyPairConventionRemoval {
    key: CurrencyPairConventionKey;
    precondition: Precondition;
}

export interface CurrencyPairConventionLookup {
    key: CurrencyPairConventionKey;
    currency_pair_convention: CurrencyPairConvention | null;
}

export interface CurrencyPairConventionEvent {
    event_id: string;
    key: CurrencyPairConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyPairConventionVersionKey {
    currency_pair_convention: CurrencyPairConventionKey;
    version: number;
}

export interface CurrencyPairConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrencyPairConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurrencyPairConventionsResponse {
    result: Result;
    conventions: CurrencyPairConvention[];
    total: number;
}

export interface GetCurrencyPairConventionRequest {
    key: CurrencyPairConventionKey;
}

export interface GetCurrencyPairConventionResponse {
    result: Result;
    currency_pair_convention: CurrencyPairConvention | null;
}

export interface GetManyCurrencyPairConventionsRequest {
    keys: CurrencyPairConventionKey[];
}

export interface GetManyCurrencyPairConventionsResponse {
    result: Result;
    entries: CurrencyPairConventionLookup[];
}

export interface PutCurrencyPairConventionRequest {
    change: CurrencyPairConventionChange;
    intent: ChangeIntent;
}

export interface PutCurrencyPairConventionResponse {
    result: Result;
    currency_pair_convention: CurrencyPairConvention;
}

export interface PutManyCurrencyPairConventionsRequest {
    changes: CurrencyPairConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyPairConventionsResponse {
    result: Result;
    conventions: CurrencyPairConvention[];
}

export interface DeleteCurrencyPairConventionRequest {
    removal: CurrencyPairConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyPairConventionResponse {
    result: Result;
}

export interface DeleteManyCurrencyPairConventionsRequest {
    removals: CurrencyPairConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyPairConventionsResponse {
    result: Result;
}

export interface ListCurrencyPairConventionVersionsRequest {
    key: CurrencyPairConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyPairConventionVersionsFilter | null;
}

export interface ListCurrencyPairConventionVersionsResponse {
    result: Result;
    versions: CurrencyPairConvention[];
    total: number;
}

export interface GetCurrencyPairConventionVersionRequest {
    key: CurrencyPairConventionVersionKey;
}

export interface GetCurrencyPairConventionVersionResponse {
    result: Result;
    version: CurrencyPairConvention;
}

export const subjects = {
    list_currency_pair_conventions_request: "refdata.v1.currency_pair_conventions.list",
    get_currency_pair_convention_request: "refdata.v1.currency_pair_conventions.get",
    get_many_currency_pair_conventions_request: "refdata.v1.currency_pair_conventions.get_many",
    put_currency_pair_convention_request: "refdata.v1.currency_pair_conventions.put",
    put_many_currency_pair_conventions_request: "refdata.v1.currency_pair_conventions.put_many",
    delete_currency_pair_convention_request: "refdata.v1.currency_pair_conventions.delete",
    delete_many_currency_pair_conventions_request: "refdata.v1.currency_pair_conventions.delete_many",
    list_currency_pair_convention_versions_request: "refdata.v1.currency_pair_conventions_versions.list",
    get_currency_pair_convention_version_request: "refdata.v1.currency_pair_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_pair_conventions_request: true,
    get_currency_pair_convention_request: true,
    get_many_currency_pair_conventions_request: true,
    put_currency_pair_convention_request: true,
    put_many_currency_pair_conventions_request: true,
    delete_currency_pair_convention_request: true,
    delete_many_currency_pair_conventions_request: true,
    list_currency_pair_convention_versions_request: true,
    get_currency_pair_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currency_pair_conventions_events.created",
    updated: "refdata.v1.currency_pair_conventions_events.updated",
    deleted: "refdata.v1.currency_pair_conventions_events.deleted",
} as const;

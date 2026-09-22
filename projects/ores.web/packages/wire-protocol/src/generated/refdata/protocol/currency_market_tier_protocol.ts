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
import type { CurrencyMarketTier } from '../domain/currency_market_tier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyMarketTierKey {
    code: string;
}

export interface CurrencyMarketTierWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CurrencyMarketTierChange {
    write: CurrencyMarketTierWrite;
    precondition: Precondition;
}

export interface CurrencyMarketTierRemoval {
    key: CurrencyMarketTierKey;
    precondition: Precondition;
}

export interface CurrencyMarketTierLookup {
    key: CurrencyMarketTierKey;
    currency_market_tier: CurrencyMarketTier | null;
}

export interface CurrencyMarketTierEvent {
    event_id: string;
    key: CurrencyMarketTierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyMarketTierVersionKey {
    currency_market_tier: CurrencyMarketTierKey;
    version: number;
}

export interface CurrencyMarketTierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrencyMarketTiersRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurrencyMarketTiersResponse {
    result: Result;
    types: CurrencyMarketTier[];
    total: number;
}

export interface GetCurrencyMarketTierRequest {
    key: CurrencyMarketTierKey;
}

export interface GetCurrencyMarketTierResponse {
    result: Result;
    currency_market_tier: CurrencyMarketTier | null;
}

export interface GetManyCurrencyMarketTiersRequest {
    keys: CurrencyMarketTierKey[];
}

export interface GetManyCurrencyMarketTiersResponse {
    result: Result;
    entries: CurrencyMarketTierLookup[];
}

export interface PutCurrencyMarketTierRequest {
    change: CurrencyMarketTierChange;
    intent: ChangeIntent;
}

export interface PutCurrencyMarketTierResponse {
    result: Result;
    currency_market_tier: CurrencyMarketTier;
}

export interface PutManyCurrencyMarketTiersRequest {
    changes: CurrencyMarketTierChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyMarketTiersResponse {
    result: Result;
    types: CurrencyMarketTier[];
}

export interface DeleteCurrencyMarketTierRequest {
    removal: CurrencyMarketTierRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyMarketTierResponse {
    result: Result;
}

export interface DeleteManyCurrencyMarketTiersRequest {
    removals: CurrencyMarketTierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyMarketTiersResponse {
    result: Result;
}

export interface ListCurrencyMarketTierVersionsRequest {
    key: CurrencyMarketTierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyMarketTierVersionsFilter | null;
}

export interface ListCurrencyMarketTierVersionsResponse {
    result: Result;
    versions: CurrencyMarketTier[];
    total: number;
}

export interface GetCurrencyMarketTierVersionRequest {
    key: CurrencyMarketTierVersionKey;
}

export interface GetCurrencyMarketTierVersionResponse {
    result: Result;
    version: CurrencyMarketTier;
}

export const subjects = {
    list_currency_market_tiers_request: "refdata.v1.currency_market_tiers.list",
    get_currency_market_tier_request: "refdata.v1.currency_market_tiers.get",
    get_many_currency_market_tiers_request: "refdata.v1.currency_market_tiers.get_many",
    put_currency_market_tier_request: "refdata.v1.currency_market_tiers.put",
    put_many_currency_market_tiers_request: "refdata.v1.currency_market_tiers.put_many",
    delete_currency_market_tier_request: "refdata.v1.currency_market_tiers.delete",
    delete_many_currency_market_tiers_request: "refdata.v1.currency_market_tiers.delete_many",
    list_currency_market_tier_versions_request: "refdata.v1.currency_market_tiers_versions.list",
    get_currency_market_tier_version_request: "refdata.v1.currency_market_tiers_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_market_tiers_request: true,
    get_currency_market_tier_request: true,
    get_many_currency_market_tiers_request: true,
    put_currency_market_tier_request: true,
    put_many_currency_market_tiers_request: true,
    delete_currency_market_tier_request: true,
    delete_many_currency_market_tiers_request: true,
    list_currency_market_tier_versions_request: true,
    get_currency_market_tier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currency_market_tiers_events.created",
    updated: "refdata.v1.currency_market_tiers_events.updated",
    deleted: "refdata.v1.currency_market_tiers_events.deleted",
} as const;

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
import type { FxSpotFixingItem } from '../domain/fx_spot_fixing_item.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxSpotFixingItemKey {
    id: string;
}

export interface FxSpotFixingItemWrite {
    id: string;
    fx_spot_config_id: string;
    rank: number;
    item: string;
    provider_code: string;
    policy: string;
    window_days: number;
}

export interface FxSpotFixingItemChange {
    write: FxSpotFixingItemWrite;
    precondition: Precondition;
}

export interface FxSpotFixingItemRemoval {
    key: FxSpotFixingItemKey;
    precondition: Precondition;
}

export interface FxSpotFixingItemLookup {
    key: FxSpotFixingItemKey;
    fx_spot_fixing_item: FxSpotFixingItem | null;
}

export interface FxSpotFixingItemsFilter {
    fx_spot_config_id: string | null;
    id_one_of: string[] | null;
    fx_spot_config_id_one_of: string[] | null;
}

export interface FxSpotFixingItemEvent {
    event_id: string;
    key: FxSpotFixingItemKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxSpotFixingItemVersionKey {
    fx_spot_fixing_item: FxSpotFixingItemKey;
    version: number;
}

export interface FxSpotFixingItemVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxSpotFixingItemsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxSpotFixingItemsFilter | null;
    as_of: string | null;
}

export interface ListFxSpotFixingItemsResponse {
    result: Result;
    fx_spot_fixing_items: FxSpotFixingItem[];
    total: number;
}

export interface GetFxSpotFixingItemRequest {
    key: FxSpotFixingItemKey;
}

export interface GetFxSpotFixingItemResponse {
    result: Result;
    fx_spot_fixing_item: FxSpotFixingItem | null;
}

export interface GetManyFxSpotFixingItemsRequest {
    keys: FxSpotFixingItemKey[];
}

export interface GetManyFxSpotFixingItemsResponse {
    result: Result;
    entries: FxSpotFixingItemLookup[];
}

export interface PutFxSpotFixingItemRequest {
    change: FxSpotFixingItemChange;
    intent: ChangeIntent;
}

export interface PutFxSpotFixingItemResponse {
    result: Result;
    fx_spot_fixing_item: FxSpotFixingItem | null;
}

export interface PutManyFxSpotFixingItemsRequest {
    changes: FxSpotFixingItemChange[];
    intent: ChangeIntent;
}

export interface PutManyFxSpotFixingItemsResponse {
    result: Result;
    fx_spot_fixing_items: FxSpotFixingItem[];
}

export interface DeleteFxSpotFixingItemRequest {
    removal: FxSpotFixingItemRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxSpotFixingItemResponse {
    result: Result;
}

export interface DeleteManyFxSpotFixingItemsRequest {
    removals: FxSpotFixingItemRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxSpotFixingItemsResponse {
    result: Result;
}

export interface ListFxSpotFixingItemVersionsRequest {
    key: FxSpotFixingItemKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxSpotFixingItemVersionsFilter | null;
}

export interface ListFxSpotFixingItemVersionsResponse {
    result: Result;
    versions: FxSpotFixingItem[];
    total: number;
}

export interface GetFxSpotFixingItemVersionRequest {
    key: FxSpotFixingItemVersionKey;
}

export interface GetFxSpotFixingItemVersionResponse {
    result: Result;
    version: FxSpotFixingItem | null;
}

export const subjects = {
    list_fx_spot_fixing_items_request: 'synthetic.v1.fx_spot_fixing_items.list',
    get_fx_spot_fixing_item_request: 'synthetic.v1.fx_spot_fixing_items.get',
    get_many_fx_spot_fixing_items_request: 'synthetic.v1.fx_spot_fixing_items.get_many',
    put_fx_spot_fixing_item_request: 'synthetic.v1.fx_spot_fixing_items.put',
    put_many_fx_spot_fixing_items_request: 'synthetic.v1.fx_spot_fixing_items.put_many',
    delete_fx_spot_fixing_item_request: 'synthetic.v1.fx_spot_fixing_items.delete',
    delete_many_fx_spot_fixing_items_request: 'synthetic.v1.fx_spot_fixing_items.delete_many',
    list_fx_spot_fixing_item_versions_request: 'synthetic.v1.fx_spot_fixing_items_versions.list',
    get_fx_spot_fixing_item_version_request: 'synthetic.v1.fx_spot_fixing_items_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_spot_fixing_items_request: true,
    get_fx_spot_fixing_item_request: true,
    get_many_fx_spot_fixing_items_request: true,
    put_fx_spot_fixing_item_request: true,
    put_many_fx_spot_fixing_items_request: true,
    delete_fx_spot_fixing_item_request: true,
    delete_many_fx_spot_fixing_items_request: true,
    list_fx_spot_fixing_item_versions_request: true,
    get_fx_spot_fixing_item_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.fx_spot_fixing_items_events.created',
    updated: 'synthetic.v1.fx_spot_fixing_items_events.updated',
    deleted: 'synthetic.v1.fx_spot_fixing_items_events.deleted',
} as const;

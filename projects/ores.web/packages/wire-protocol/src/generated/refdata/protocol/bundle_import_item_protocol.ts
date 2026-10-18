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
import type { BundleImportItem } from '../domain/bundle_import_item.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface BundleImportItemKey {
    id: string;
}

export interface BundleImportItemWrite {
    id: string;
    import_id: string;
    family: string;
    sequence: number;
    payload: string;
    checksum: string | null;
}

export interface BundleImportItemChange {
    write: BundleImportItemWrite;
    precondition: Precondition;
}

export interface BundleImportItemRemoval {
    key: BundleImportItemKey;
    precondition: Precondition;
}

export interface BundleImportItemLookup {
    key: BundleImportItemKey;
    bundle_import_item: BundleImportItem | null;
}

export interface BundleImportItemsFilter {
    import_id: string | null;
    id_one_of: string[] | null;
    import_id_one_of: string[] | null;
}

export interface BundleImportItemEvent {
    event_id: string;
    key: BundleImportItemKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BundleImportItemVersionKey {
    bundle_import_item: BundleImportItemKey;
    version: number;
}

export interface BundleImportItemVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBundleImportItemsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BundleImportItemsFilter | null;
    as_of: string | null;
}

export interface ListBundleImportItemsResponse {
    result: Result;
    bundle_import_items: BundleImportItem[];
    total: number;
}

export interface GetBundleImportItemRequest {
    key: BundleImportItemKey;
}

export interface GetBundleImportItemResponse {
    result: Result;
    bundle_import_item: BundleImportItem | null;
}

export interface GetManyBundleImportItemsRequest {
    keys: BundleImportItemKey[];
}

export interface GetManyBundleImportItemsResponse {
    result: Result;
    entries: BundleImportItemLookup[];
}

export interface PutBundleImportItemRequest {
    change: BundleImportItemChange;
    intent: ChangeIntent;
}

export interface PutBundleImportItemResponse {
    result: Result;
    bundle_import_item: BundleImportItem | null;
}

export interface PutManyBundleImportItemsRequest {
    changes: BundleImportItemChange[];
    intent: ChangeIntent;
}

export interface PutManyBundleImportItemsResponse {
    result: Result;
    bundle_import_items: BundleImportItem[];
}

export interface DeleteBundleImportItemRequest {
    removal: BundleImportItemRemoval;
    intent: ChangeIntent;
}

export interface DeleteBundleImportItemResponse {
    result: Result;
}

export interface DeleteManyBundleImportItemsRequest {
    removals: BundleImportItemRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBundleImportItemsResponse {
    result: Result;
}

export interface ListByImportIdBundleImportItemsRequest {
    import_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BundleImportItemsFilter | null;
}

export interface ListByImportIdBundleImportItemsResponse {
    result: Result;
    bundle_import_items: BundleImportItem[];
    total: number;
}

export interface ListBundleImportItemVersionsRequest {
    key: BundleImportItemKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BundleImportItemVersionsFilter | null;
}

export interface ListBundleImportItemVersionsResponse {
    result: Result;
    versions: BundleImportItem[];
    total: number;
}

export interface GetBundleImportItemVersionRequest {
    key: BundleImportItemVersionKey;
}

export interface GetBundleImportItemVersionResponse {
    result: Result;
    version: BundleImportItem | null;
}

export const subjects = {
    list_bundle_import_items_request: 'refdata.v1.bundle_import_items.list',
    get_bundle_import_item_request: 'refdata.v1.bundle_import_items.get',
    get_many_bundle_import_items_request: 'refdata.v1.bundle_import_items.get_many',
    put_bundle_import_item_request: 'refdata.v1.bundle_import_items.put',
    put_many_bundle_import_items_request: 'refdata.v1.bundle_import_items.put_many',
    delete_bundle_import_item_request: 'refdata.v1.bundle_import_items.delete',
    delete_many_bundle_import_items_request: 'refdata.v1.bundle_import_items.delete_many',
    list_by_import_id_bundle_import_items_request:
        'refdata.v1.bundle_import_items.list_by_import_id',
    list_bundle_import_item_versions_request: 'refdata.v1.bundle_import_items_versions.list',
    get_bundle_import_item_version_request: 'refdata.v1.bundle_import_items_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bundle_import_items_request: true,
    get_bundle_import_item_request: true,
    get_many_bundle_import_items_request: true,
    put_bundle_import_item_request: true,
    put_many_bundle_import_items_request: true,
    delete_bundle_import_item_request: true,
    delete_many_bundle_import_items_request: true,
    list_by_import_id_bundle_import_items_request: true,
    list_bundle_import_item_versions_request: true,
    get_bundle_import_item_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bundle_import_items_events.created',
    updated: 'refdata.v1.bundle_import_items_events.updated',
    deleted: 'refdata.v1.bundle_import_items_events.deleted',
} as const;

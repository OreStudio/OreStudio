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
import type { ConventionsImportItem } from '../domain/conventions_import_item.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConventionsImportItemKey {
    id: string;
}

export interface ConventionsImportItemWrite {
    id: string;
    import_id: string;
    family: string;
    sequence: number;
    payload: string;
    checksum: string | null;
}

export interface ConventionsImportItemChange {
    write: ConventionsImportItemWrite;
    precondition: Precondition;
}

export interface ConventionsImportItemRemoval {
    key: ConventionsImportItemKey;
    precondition: Precondition;
}

export interface ConventionsImportItemLookup {
    key: ConventionsImportItemKey;
    conventions_import_item: ConventionsImportItem | null;
}

export interface ConventionsImportItemsFilter {
    import_id: string | null;
    id_one_of: string[] | null;
    import_id_one_of: string[] | null;
}

export interface ConventionsImportItemEvent {
    event_id: string;
    key: ConventionsImportItemKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConventionsImportItemVersionKey {
    conventions_import_item: ConventionsImportItemKey;
    version: number;
}

export interface ConventionsImportItemVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConventionsImportItemsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionsImportItemsFilter | null;
    as_of: string | null;
}

export interface ListConventionsImportItemsResponse {
    result: Result;
    conventions_import_items: ConventionsImportItem[];
    total: number;
}

export interface GetConventionsImportItemRequest {
    key: ConventionsImportItemKey;
}

export interface GetConventionsImportItemResponse {
    result: Result;
    conventions_import_item: ConventionsImportItem | null;
}

export interface GetManyConventionsImportItemsRequest {
    keys: ConventionsImportItemKey[];
}

export interface GetManyConventionsImportItemsResponse {
    result: Result;
    entries: ConventionsImportItemLookup[];
}

export interface PutConventionsImportItemRequest {
    change: ConventionsImportItemChange;
    intent: ChangeIntent;
}

export interface PutConventionsImportItemResponse {
    result: Result;
    conventions_import_item: ConventionsImportItem | null;
}

export interface PutManyConventionsImportItemsRequest {
    changes: ConventionsImportItemChange[];
    intent: ChangeIntent;
}

export interface PutManyConventionsImportItemsResponse {
    result: Result;
    conventions_import_items: ConventionsImportItem[];
}

export interface DeleteConventionsImportItemRequest {
    removal: ConventionsImportItemRemoval;
    intent: ChangeIntent;
}

export interface DeleteConventionsImportItemResponse {
    result: Result;
}

export interface DeleteManyConventionsImportItemsRequest {
    removals: ConventionsImportItemRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConventionsImportItemsResponse {
    result: Result;
}

export interface ListByImportIdConventionsImportItemsRequest {
    import_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionsImportItemsFilter | null;
}

export interface ListByImportIdConventionsImportItemsResponse {
    result: Result;
    conventions_import_items: ConventionsImportItem[];
    total: number;
}

export interface ListConventionsImportItemVersionsRequest {
    key: ConventionsImportItemKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionsImportItemVersionsFilter | null;
}

export interface ListConventionsImportItemVersionsResponse {
    result: Result;
    versions: ConventionsImportItem[];
    total: number;
}

export interface GetConventionsImportItemVersionRequest {
    key: ConventionsImportItemVersionKey;
}

export interface GetConventionsImportItemVersionResponse {
    result: Result;
    version: ConventionsImportItem | null;
}

export const subjects = {
    list_conventions_import_items_request: 'refdata.v1.conventions_import_items.list',
    get_conventions_import_item_request: 'refdata.v1.conventions_import_items.get',
    get_many_conventions_import_items_request: 'refdata.v1.conventions_import_items.get_many',
    put_conventions_import_item_request: 'refdata.v1.conventions_import_items.put',
    put_many_conventions_import_items_request: 'refdata.v1.conventions_import_items.put_many',
    delete_conventions_import_item_request: 'refdata.v1.conventions_import_items.delete',
    delete_many_conventions_import_items_request: 'refdata.v1.conventions_import_items.delete_many',
    list_by_import_id_conventions_import_items_request:
        'refdata.v1.conventions_import_items.list_by_import_id',
    list_conventions_import_item_versions_request:
        'refdata.v1.conventions_import_items_versions.list',
    get_conventions_import_item_version_request: 'refdata.v1.conventions_import_items_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_conventions_import_items_request: true,
    get_conventions_import_item_request: true,
    get_many_conventions_import_items_request: true,
    put_conventions_import_item_request: true,
    put_many_conventions_import_items_request: true,
    delete_conventions_import_item_request: true,
    delete_many_conventions_import_items_request: true,
    list_by_import_id_conventions_import_items_request: true,
    list_conventions_import_item_versions_request: true,
    get_conventions_import_item_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.conventions_import_items_events.created',
    updated: 'refdata.v1.conventions_import_items_events.updated',
    deleted: 'refdata.v1.conventions_import_items_events.deleted',
} as const;

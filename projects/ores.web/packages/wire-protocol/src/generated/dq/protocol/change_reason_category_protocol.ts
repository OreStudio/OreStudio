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
import type { ChangeReasonCategory } from '../domain/change_reason_category.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ChangeReasonCategoryKey {
    code: string;
}

export interface ChangeReasonCategoryWrite {
    code: string;
    description: string;
}

export interface ChangeReasonCategoryChange {
    write: ChangeReasonCategoryWrite;
    precondition: Precondition;
}

export interface ChangeReasonCategoryRemoval {
    key: ChangeReasonCategoryKey;
    precondition: Precondition;
}

export interface ChangeReasonCategoryLookup {
    key: ChangeReasonCategoryKey;
    change_reason_category: ChangeReasonCategory | null;
}

export interface ChangeReasonCategoryEvent {
    event_id: string;
    key: ChangeReasonCategoryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ChangeReasonCategoryVersionKey {
    change_reason_category: ChangeReasonCategoryKey;
    version: number;
}

export interface ChangeReasonCategoryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListChangeReasonCategoriesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListChangeReasonCategoriesResponse {
    result: Result;
    categories: ChangeReasonCategory[];
    total: number;
}

export interface GetChangeReasonCategoryRequest {
    key: ChangeReasonCategoryKey;
}

export interface GetChangeReasonCategoryResponse {
    result: Result;
    change_reason_category: ChangeReasonCategory | null;
}

export interface GetManyChangeReasonCategoriesRequest {
    keys: ChangeReasonCategoryKey[];
}

export interface GetManyChangeReasonCategoriesResponse {
    result: Result;
    entries: ChangeReasonCategoryLookup[];
}

export interface PutChangeReasonCategoryRequest {
    change: ChangeReasonCategoryChange;
    intent: ChangeIntent;
}

export interface PutChangeReasonCategoryResponse {
    result: Result;
    change_reason_category: ChangeReasonCategory;
}

export interface PutManyChangeReasonCategoriesRequest {
    changes: ChangeReasonCategoryChange[];
    intent: ChangeIntent;
}

export interface PutManyChangeReasonCategoriesResponse {
    result: Result;
    categories: ChangeReasonCategory[];
}

export interface DeleteChangeReasonCategoryRequest {
    removal: ChangeReasonCategoryRemoval;
    intent: ChangeIntent;
}

export interface DeleteChangeReasonCategoryResponse {
    result: Result;
}

export interface DeleteManyChangeReasonCategoriesRequest {
    removals: ChangeReasonCategoryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyChangeReasonCategoriesResponse {
    result: Result;
}

export interface ListChangeReasonCategoryVersionsRequest {
    key: ChangeReasonCategoryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ChangeReasonCategoryVersionsFilter | null;
}

export interface ListChangeReasonCategoryVersionsResponse {
    result: Result;
    versions: ChangeReasonCategory[];
    total: number;
}

export interface GetChangeReasonCategoryVersionRequest {
    key: ChangeReasonCategoryVersionKey;
}

export interface GetChangeReasonCategoryVersionResponse {
    result: Result;
    version: ChangeReasonCategory;
}

export const subjects = {
    list_change_reason_categories_request: "dq.v1.change_reason_categories.list",
    get_change_reason_category_request: "dq.v1.change_reason_categories.get",
    get_many_change_reason_categories_request: "dq.v1.change_reason_categories.get_many",
    put_change_reason_category_request: "dq.v1.change_reason_categories.put",
    put_many_change_reason_categories_request: "dq.v1.change_reason_categories.put_many",
    delete_change_reason_category_request: "dq.v1.change_reason_categories.delete",
    delete_many_change_reason_categories_request: "dq.v1.change_reason_categories.delete_many",
    list_change_reason_category_versions_request: "dq.v1.change_reason_categories_versions.list",
    get_change_reason_category_version_request: "dq.v1.change_reason_categories_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_change_reason_categories_request: true,
    get_change_reason_category_request: true,
    get_many_change_reason_categories_request: true,
    put_change_reason_category_request: true,
    put_many_change_reason_categories_request: true,
    delete_change_reason_category_request: true,
    delete_many_change_reason_categories_request: true,
    list_change_reason_category_versions_request: true,
    get_change_reason_category_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.change_reason_categories_events.created",
    updated: "dq.v1.change_reason_categories_events.updated",
    deleted: "dq.v1.change_reason_categories_events.deleted",
} as const;

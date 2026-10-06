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
import type { ActivityCategory } from '../domain/activity_category.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ActivityCategoryKey {
    code: string;
}

export interface ActivityCategoryWrite {
    code: string;
    description: string;
}

export interface ActivityCategoryChange {
    write: ActivityCategoryWrite;
    precondition: Precondition;
}

export interface ActivityCategoryRemoval {
    key: ActivityCategoryKey;
    precondition: Precondition;
}

export interface ActivityCategoryLookup {
    key: ActivityCategoryKey;
    activity_category: ActivityCategory | null;
}

export interface ActivityCategoriesFilter {
    code_one_of: string[] | null;
}

export interface ActivityCategoryEvent {
    event_id: string;
    key: ActivityCategoryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ActivityCategoryVersionKey {
    activity_category: ActivityCategoryKey;
    version: number;
}

export interface ActivityCategoryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListActivityCategoriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ActivityCategoriesFilter | null;
    as_of: string | null;
}

export interface ListActivityCategoriesResponse {
    result: Result;
    activity_categories: ActivityCategory[];
    total: number;
}

export interface GetActivityCategoryRequest {
    key: ActivityCategoryKey;
}

export interface GetActivityCategoryResponse {
    result: Result;
    activity_category: ActivityCategory | null;
}

export interface GetManyActivityCategoriesRequest {
    keys: ActivityCategoryKey[];
}

export interface GetManyActivityCategoriesResponse {
    result: Result;
    entries: ActivityCategoryLookup[];
}

export interface PutActivityCategoryRequest {
    change: ActivityCategoryChange;
    intent: ChangeIntent;
}

export interface PutActivityCategoryResponse {
    result: Result;
    activity_category: ActivityCategory | null;
}

export interface PutManyActivityCategoriesRequest {
    changes: ActivityCategoryChange[];
    intent: ChangeIntent;
}

export interface PutManyActivityCategoriesResponse {
    result: Result;
    activity_categories: ActivityCategory[];
}

export interface DeleteActivityCategoryRequest {
    removal: ActivityCategoryRemoval;
    intent: ChangeIntent;
}

export interface DeleteActivityCategoryResponse {
    result: Result;
}

export interface DeleteManyActivityCategoriesRequest {
    removals: ActivityCategoryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyActivityCategoriesResponse {
    result: Result;
}

export interface ListActivityCategoryVersionsRequest {
    key: ActivityCategoryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ActivityCategoryVersionsFilter | null;
}

export interface ListActivityCategoryVersionsResponse {
    result: Result;
    versions: ActivityCategory[];
    total: number;
}

export interface GetActivityCategoryVersionRequest {
    key: ActivityCategoryVersionKey;
}

export interface GetActivityCategoryVersionResponse {
    result: Result;
    version: ActivityCategory | null;
}

export const subjects = {
    list_activity_categories_request: 'trading.v1.activity_categories.list',
    get_activity_category_request: 'trading.v1.activity_categories.get',
    get_many_activity_categories_request: 'trading.v1.activity_categories.get_many',
    put_activity_category_request: 'trading.v1.activity_categories.put',
    put_many_activity_categories_request: 'trading.v1.activity_categories.put_many',
    delete_activity_category_request: 'trading.v1.activity_categories.delete',
    delete_many_activity_categories_request: 'trading.v1.activity_categories.delete_many',
    list_activity_category_versions_request: 'trading.v1.activity_categories_versions.list',
    get_activity_category_version_request: 'trading.v1.activity_categories_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_activity_categories_request: true,
    get_activity_category_request: true,
    get_many_activity_categories_request: true,
    put_activity_category_request: true,
    put_many_activity_categories_request: true,
    delete_activity_category_request: true,
    delete_many_activity_categories_request: true,
    list_activity_category_versions_request: true,
    get_activity_category_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.activity_categories_events.created',
    updated: 'trading.v1.activity_categories_events.updated',
    deleted: 'trading.v1.activity_categories_events.deleted',
} as const;

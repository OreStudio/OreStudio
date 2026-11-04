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
import type { OreFileCategory } from '../domain/ore_file_category.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreFileCategoryKey {
    code: string;
}

export interface OreFileCategoryWrite {
    code: string;
    name: string;
    origin: string;
    description: string;
    display_order: number;
}

export interface OreFileCategoryChange {
    write: OreFileCategoryWrite;
    precondition: Precondition;
}

export interface OreFileCategoryRemoval {
    key: OreFileCategoryKey;
    precondition: Precondition;
}

export interface OreFileCategoryLookup {
    key: OreFileCategoryKey;
    ore_file_category: OreFileCategory | null;
}

export interface OreFileCategoriesFilter {
    code_one_of: string[] | null;
}

export interface OreFileCategoryEvent {
    event_id: string;
    key: OreFileCategoryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreFileCategoryVersionKey {
    ore_file_category: OreFileCategoryKey;
    version: number;
}

export interface OreFileCategoryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreFileCategoriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreFileCategoriesFilter | null;
    as_of: string | null;
}

export interface ListOreFileCategoriesResponse {
    result: Result;
    categories: OreFileCategory[];
    total: number;
}

export interface GetOreFileCategoryRequest {
    key: OreFileCategoryKey;
}

export interface GetOreFileCategoryResponse {
    result: Result;
    ore_file_category: OreFileCategory | null;
}

export interface GetManyOreFileCategoriesRequest {
    keys: OreFileCategoryKey[];
}

export interface GetManyOreFileCategoriesResponse {
    result: Result;
    entries: OreFileCategoryLookup[];
}

export interface PutOreFileCategoryRequest {
    change: OreFileCategoryChange;
    intent: ChangeIntent;
}

export interface PutOreFileCategoryResponse {
    result: Result;
    ore_file_category: OreFileCategory | null;
}

export interface PutManyOreFileCategoriesRequest {
    changes: OreFileCategoryChange[];
    intent: ChangeIntent;
}

export interface PutManyOreFileCategoriesResponse {
    result: Result;
    categories: OreFileCategory[];
}

export interface DeleteOreFileCategoryRequest {
    removal: OreFileCategoryRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreFileCategoryResponse {
    result: Result;
}

export interface DeleteManyOreFileCategoriesRequest {
    removals: OreFileCategoryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreFileCategoriesResponse {
    result: Result;
}

export interface ListOreFileCategoryVersionsRequest {
    key: OreFileCategoryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFileCategoryVersionsFilter | null;
}

export interface ListOreFileCategoryVersionsResponse {
    result: Result;
    versions: OreFileCategory[];
    total: number;
}

export interface GetOreFileCategoryVersionRequest {
    key: OreFileCategoryVersionKey;
}

export interface GetOreFileCategoryVersionResponse {
    result: Result;
    version: OreFileCategory | null;
}

export const subjects = {
    list_ore_file_categories_request: 'reporting.v1.ore_file_categories.list',
    get_ore_file_category_request: 'reporting.v1.ore_file_categories.get',
    get_many_ore_file_categories_request: 'reporting.v1.ore_file_categories.get_many',
    put_ore_file_category_request: 'reporting.v1.ore_file_categories.put',
    put_many_ore_file_categories_request: 'reporting.v1.ore_file_categories.put_many',
    delete_ore_file_category_request: 'reporting.v1.ore_file_categories.delete',
    delete_many_ore_file_categories_request: 'reporting.v1.ore_file_categories.delete_many',
    list_ore_file_category_versions_request: 'reporting.v1.ore_file_categories_versions.list',
    get_ore_file_category_version_request: 'reporting.v1.ore_file_categories_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_file_categories_request: true,
    get_ore_file_category_request: true,
    get_many_ore_file_categories_request: true,
    put_ore_file_category_request: true,
    put_many_ore_file_categories_request: true,
    delete_ore_file_category_request: true,
    delete_many_ore_file_categories_request: true,
    list_ore_file_category_versions_request: true,
    get_ore_file_category_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.ore_file_categories.created',
    updated: 'reporting.v1.events.ore_file_categories.updated',
    deleted: 'reporting.v1.events.ore_file_categories.deleted',
} as const;

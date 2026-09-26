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
import type { Tag } from '../domain/tag.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TagKey {
    name: string;
}

export interface TagWrite {
    id: string;
    name: string;
    description: string;
}

export interface TagChange {
    write: TagWrite;
    precondition: Precondition;
}

export interface TagRemoval {
    key: TagKey;
    precondition: Precondition;
}

export interface TagLookup {
    key: TagKey;
    tag: Tag | null;
}

export interface TagEvent {
    event_id: string;
    key: TagKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TagVersionKey {
    tag: TagKey;
    version: number;
}

export interface TagVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTagsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTagsResponse {
    result: Result;
    tags: Tag[];
    total: number;
}

export interface GetTagRequest {
    key: TagKey;
}

export interface GetTagResponse {
    result: Result;
    tag: Tag | null;
}

export interface GetManyTagsRequest {
    keys: TagKey[];
}

export interface GetManyTagsResponse {
    result: Result;
    entries: TagLookup[];
}

export interface PutTagRequest {
    change: TagChange;
    intent: ChangeIntent;
}

export interface PutTagResponse {
    result: Result;
    tag: Tag;
}

export interface PutManyTagsRequest {
    changes: TagChange[];
    intent: ChangeIntent;
}

export interface PutManyTagsResponse {
    result: Result;
    tags: Tag[];
}

export interface DeleteTagRequest {
    removal: TagRemoval;
    intent: ChangeIntent;
}

export interface DeleteTagResponse {
    result: Result;
}

export interface DeleteManyTagsRequest {
    removals: TagRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTagsResponse {
    result: Result;
}

export interface ListTagVersionsRequest {
    key: TagKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TagVersionsFilter | null;
}

export interface ListTagVersionsResponse {
    result: Result;
    versions: Tag[];
    total: number;
}

export interface GetTagVersionRequest {
    key: TagVersionKey;
}

export interface GetTagVersionResponse {
    result: Result;
    version: Tag;
}

export const subjects = {
    list_tags_request: "assets.v1.tags.list",
    get_tag_request: "assets.v1.tags.get",
    get_many_tags_request: "assets.v1.tags.get_many",
    put_tag_request: "assets.v1.tags.put",
    put_many_tags_request: "assets.v1.tags.put_many",
    delete_tag_request: "assets.v1.tags.delete",
    delete_many_tags_request: "assets.v1.tags.delete_many",
    list_tag_versions_request: "assets.v1.tags_versions.list",
    get_tag_version_request: "assets.v1.tags_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tags_request: true,
    get_tag_request: true,
    get_many_tags_request: true,
    put_tag_request: true,
    put_many_tags_request: true,
    delete_tag_request: true,
    delete_many_tags_request: true,
    list_tag_versions_request: true,
    get_tag_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "assets.v1.tags_events.created",
    updated: "assets.v1.tags_events.updated",
    deleted: "assets.v1.tags_events.deleted",
} as const;

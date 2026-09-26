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
import type { ImageTag } from '../domain/image_tag.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ImageTagKey {
    image_id: string;
    tag_id: string;
}

export interface ImageTagWrite {
    image_id: string;
    tag_id: string;
    assigned_by: string;
    assigned_at: string;
}

export interface ImageTagChange {
    write: ImageTagWrite;
    precondition: Precondition;
}

export interface ImageTagRemoval {
    key: ImageTagKey;
    precondition: Precondition;
}

export interface ImageTagLookup {
    key: ImageTagKey;
    image_tag: ImageTag | null;
}

export interface ImageTagsFilter {
    image_id: string | null;
}

export interface ImageTagEvent {
    event_id: string;
    key: ImageTagKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListImageTagsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ImageTagsFilter | null;
}

export interface ListImageTagsResponse {
    result: Result;
    image_tags: ImageTag[];
    total: number;
}

export interface GetImageTagRequest {
    key: ImageTagKey;
}

export interface GetImageTagResponse {
    result: Result;
    image_tag: ImageTag | null;
}

export interface GetManyImageTagsRequest {
    keys: ImageTagKey[];
}

export interface GetManyImageTagsResponse {
    result: Result;
    entries: ImageTagLookup[];
}

export interface PutImageTagRequest {
    change: ImageTagChange;
    intent: ChangeIntent;
}

export interface PutImageTagResponse {
    result: Result;
    image_tag: ImageTag;
}

export interface PutManyImageTagsRequest {
    changes: ImageTagChange[];
    intent: ChangeIntent;
}

export interface PutManyImageTagsResponse {
    result: Result;
    image_tags: ImageTag[];
}

export interface DeleteImageTagRequest {
    removal: ImageTagRemoval;
    intent: ChangeIntent;
}

export interface DeleteImageTagResponse {
    result: Result;
}

export interface DeleteManyImageTagsRequest {
    removals: ImageTagRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyImageTagsResponse {
    result: Result;
}

export interface ListByImageIdImageTagsRequest {
    image_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ImageTagsFilter | null;
}

export interface ListByImageIdImageTagsResponse {
    result: Result;
    image_tags: ImageTag[];
    total: number;
}

export const subjects = {
    list_image_tags_request: "assets.v1.image_tags.list",
    get_image_tag_request: "assets.v1.image_tags.get",
    get_many_image_tags_request: "assets.v1.image_tags.get_many",
    put_image_tag_request: "assets.v1.image_tags.put",
    put_many_image_tags_request: "assets.v1.image_tags.put_many",
    delete_image_tag_request: "assets.v1.image_tags.delete",
    delete_many_image_tags_request: "assets.v1.image_tags.delete_many",
    list_by_image_id_image_tags_request: "assets.v1.image_tags.list_by_image_id",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_image_tags_request: true,
    get_image_tag_request: true,
    get_many_image_tags_request: true,
    put_image_tag_request: true,
    put_many_image_tags_request: true,
    delete_image_tag_request: true,
    delete_many_image_tags_request: true,
    list_by_image_id_image_tags_request: true,
} as const;

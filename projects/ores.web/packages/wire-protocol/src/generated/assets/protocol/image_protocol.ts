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
import type { Image } from '../domain/image.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ImageKey {
    code: string;
}

export interface ImageWrite {
    id: string;
    code: string;
    description: string;
    mime_type: string;
    data: string;
}

export interface ImageChange {
    write: ImageWrite;
    precondition: Precondition;
}

export interface ImageRemoval {
    key: ImageKey;
    precondition: Precondition;
}

export interface ImageLookup {
    key: ImageKey;
    image: Image | null;
}

export interface ImageEvent {
    event_id: string;
    key: ImageKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ImageVersionKey {
    image: ImageKey;
    version: number;
}

export interface ImageVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListImagesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListImagesResponse {
    result: Result;
    images: Image[];
    total: number;
}

export interface GetImageRequest {
    key: ImageKey;
}

export interface GetImageResponse {
    result: Result;
    image: Image | null;
}

export interface GetManyImagesRequest {
    keys: ImageKey[];
}

export interface GetManyImagesResponse {
    result: Result;
    entries: ImageLookup[];
}

export interface PutImageRequest {
    change: ImageChange;
    intent: ChangeIntent;
}

export interface PutImageResponse {
    result: Result;
    image: Image;
}

export interface PutManyImagesRequest {
    changes: ImageChange[];
    intent: ChangeIntent;
}

export interface PutManyImagesResponse {
    result: Result;
    images: Image[];
}

export interface DeleteImageRequest {
    removal: ImageRemoval;
    intent: ChangeIntent;
}

export interface DeleteImageResponse {
    result: Result;
}

export interface DeleteManyImagesRequest {
    removals: ImageRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyImagesResponse {
    result: Result;
}

export interface ListImageVersionsRequest {
    key: ImageKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ImageVersionsFilter | null;
}

export interface ListImageVersionsResponse {
    result: Result;
    versions: Image[];
    total: number;
}

export interface GetImageVersionRequest {
    key: ImageVersionKey;
}

export interface GetImageVersionResponse {
    result: Result;
    version: Image;
}

export const subjects = {
    list_images_request: "assets.v1.images.list",
    get_image_request: "assets.v1.images.get",
    get_many_images_request: "assets.v1.images.get_many",
    put_image_request: "assets.v1.images.put",
    put_many_images_request: "assets.v1.images.put_many",
    delete_image_request: "assets.v1.images.delete",
    delete_many_images_request: "assets.v1.images.delete_many",
    list_image_versions_request: "assets.v1.images_versions.list",
    get_image_version_request: "assets.v1.images_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_images_request: true,
    get_image_request: true,
    get_many_images_request: true,
    put_image_request: true,
    put_many_images_request: true,
    delete_image_request: true,
    delete_many_images_request: true,
    list_image_versions_request: true,
    get_image_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "assets.v1.images_events.created",
    updated: "assets.v1.images_events.updated",
    deleted: "assets.v1.images_events.deleted",
} as const;

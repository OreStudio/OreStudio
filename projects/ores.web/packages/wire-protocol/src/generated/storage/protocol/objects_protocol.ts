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
/**
 * @brief Writes one object, replacing whatever was at the key.
 *
 * A put carries the whole object: there is no append and no partial write, so
 * an object is either the old value or the new one. The content travels in the
 * message only when it is small enough to; the bulk path is HTTP, and the two
 * leave the same object at the same bucket and key.
 */
export interface PutObjectsRequest {
    /**
     * @brief The allocation the object lives in. Named by the caller.
     */
    bucket: string;
    /**
     * @brief The object's name within the bucket. Opaque: storage never parses it.
     */
    key: string;
    /**
     * @brief The object's bytes.
     */
    content: string;
    /**
     * @brief Empty for raw bytes, or the encoding the caller applied.
     *
     * A caller may compress before it puts, and it must say so here, because
     * nothing on the wire marks an object as compressed.
     */
    content_encoding: string;
}

/**
 * @brief The outcome of the write.
 */
export interface PutObjectsResponse {
    success: boolean;
    message: string;
    /**
     * @brief How many bytes the store now holds under the key.
     */
    size_bytes: number;
}

/**
 * @brief Reads one object's metadata, and its value when asked for.
 *
 * The same operation answers existence: a key that is not there comes back as
 * a reply that says so, which is why the surface needs no separate verb for it.
 */
export interface GetObjectsRequest {
    bucket: string;
    key: string;
    /**
     * @brief Whether the reply should carry the object's bytes.
     */
    include_content: boolean;
}

/**
 * @brief The object, or the news that it is not there.
 */
export interface GetObjectsResponse {
    success: boolean;
    message: string;
    /**
     * @brief False when the bucket holds no object at the key.
     */
    found: boolean;
    size_bytes: number;
    /**
     * @brief The object's bytes, present only when the request asked for them.
     */
    content: string;
}

/**
 * @brief Removes one object.
 */
export interface DeleteObjectsRequest {
    bucket: string;
    key: string;
}

/**
 * @brief The outcome of the removal.
 */
export interface DeleteObjectsResponse {
    success: boolean;
    message: string;
    /**
     * @brief False when there was nothing at the key to remove.
     */
    removed: boolean;
}

/**
 * @brief Asks for a page of the objects in one bucket.
 *
 * The prefix is a filter over keys, not a directory: storage treats a key as
 * opaque, so a caller that wants everything under a prefix asks for it by
 * prefix and never by path.
 */
export interface ListObjectsRequest {
    bucket: string;
    /**
     * @brief Only keys that start with this are returned. Empty means every key.
     */
    prefix: string;
    offset: number;
    limit: number;
}

/**
 * @brief One object in a listing, without its value.
 */
export interface ObjectSummary {
    key: string;
    size_bytes: number;
}

/**
 * @brief The page of objects, with the total the caller is paging through.
 */
export interface ListObjectsResponse {
    success: boolean;
    message: string;
    objects: ObjectSummary[];
    total_available_count: number;
}

export const subjects = {
    put_objects_request: "storage.v1.objects.put",
    get_objects_request: "storage.v1.objects.get",
    delete_objects_request: "storage.v1.objects.delete",
    list_objects_request: "storage.v1.objects.list",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    put_objects_request: true,
    get_objects_request: true,
    delete_objects_request: true,
    list_objects_request: true,
} as const;

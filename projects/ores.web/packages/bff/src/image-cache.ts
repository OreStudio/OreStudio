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
 *
 */

import type { ImageContent } from '@ores/wire-protocol';

/**
 * The images read inside a tenant, kept so the browser can fetch them by URL.
 *
 * A page of a tenant's people or parties is read inside the tenant once, and
 * the pictures and flags it names are read in that same visit. The browser then
 * asks for each image by its own URL, and the answer comes from here rather
 * than from a second visit per image. An image's identifier is its identity
 * and its bytes never change, so an entry is never stale; the cache is bounded
 * by size, and the image used longest ago leaves first.
 */
export interface ImageCache {
    get(tenantId: string, imageId: string): ImageContent | undefined;
    /** The identifiers named that the cache does not hold. */
    missing(tenantId: string, imageIds: readonly string[]): string[];
    put(tenantId: string, image: ImageContent): void;
}

export function createImageCache(maxBytes: number): ImageCache {
    const entries = new Map<string, ImageContent>();
    let totalBytes = 0;
    const keyOf = (tenantId: string, imageId: string) => `${tenantId}/${imageId}`;

    return {
        get(tenantId, imageId) {
            const key = keyOf(tenantId, imageId);
            const image = entries.get(key);
            if (image !== undefined) {
                entries.delete(key);
                entries.set(key, image);
            }
            return image;
        },
        missing(tenantId, imageIds) {
            return [...new Set(imageIds)].filter((id) => !entries.has(keyOf(tenantId, id)));
        },
        put(tenantId, image) {
            const key = keyOf(tenantId, image.imageId);
            const previous = entries.get(key);
            if (previous !== undefined) {
                entries.delete(key);
                totalBytes -= previous.bytes.length;
            }
            if (image.bytes.length > maxBytes) {
                return;
            }
            entries.set(key, image);
            totalBytes += image.bytes.length;
            for (const [oldest, evicted] of entries) {
                if (totalBytes <= maxBytes) break;
                entries.delete(oldest);
                totalBytes -= evicted.bytes.length;
            }
        },
    };
}

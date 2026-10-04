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

import { describe, expect, it } from 'vitest';
import { createImageCache } from './image-cache.js';

function image(imageId: string, size: number) {
    return { imageId, mimeType: 'image/jpeg', bytes: Buffer.alloc(size) };
}

describe('the tenant image cache', () => {
    it('keeps each tenant images apart', () => {
        const cache = createImageCache(100);
        cache.put('acme', image('a', 10));

        expect(cache.get('acme', 'a')?.bytes.length).toBe(10);
        expect(cache.get('globex', 'a')).toBeUndefined();
        expect(cache.missing('globex', ['a', 'a'])).toEqual(['a']);
        expect(cache.missing('acme', ['a', 'b'])).toEqual(['b']);
    });

    it('lets the image used longest ago go first when it is full', () => {
        const cache = createImageCache(30);
        cache.put('acme', image('a', 10));
        cache.put('acme', image('b', 10));
        cache.put('acme', image('c', 10));
        cache.get('acme', 'a');
        cache.put('acme', image('d', 10));

        expect(cache.missing('acme', ['a', 'b', 'c', 'd'])).toEqual(['b']);
    });

    it('keeps no image larger than the whole cache', () => {
        const cache = createImageCache(30);
        cache.put('acme', image('a', 10));
        cache.put('acme', image('huge', 31));

        expect(cache.missing('acme', ['a', 'huge'])).toEqual(['huge']);
    });
});

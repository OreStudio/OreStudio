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
import { describe, expect, it } from 'vitest';
import { bundleCacheControl } from './server.js';

describe('how long a browser may keep a file of the bundle', () => {
    it('never keeps the entry point, so a reload runs the build the server holds', () => {
        expect(bundleCacheControl('/srv/web/dist/index.html')).toBe('no-store');
    });

    it('keeps a hashed asset for good, because a new build names a new file', () => {
        expect(bundleCacheControl('/srv/web/dist/assets/index-BaE7V_8Q.js')).toBe(
            'public, max-age=31536000, immutable',
        );
    });
});

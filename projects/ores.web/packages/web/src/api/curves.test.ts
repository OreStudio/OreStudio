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

import { afterEach, describe, expect, it, vi } from 'vitest';
import { curves } from './curves.js';

/**
 * The series reads, and the id shape they accept.
 *
 * The store writes ids whose version and variant nibbles are not the RFC's, so
 * a screen that parsed them with `z.uuid()` failed the whole read on a request
 * error. These tests answer the BFF's series route with such an id and assert
 * the read parses it, which is what the blueprint and published-curve screens
 * need in order to draw.
 */

/** A series id as the store wrote it: the version and variant nibbles are not RFC's. */
const STORE_ID = 'a1eec62d-a758-f647-de09-d7f2c25d9196';

function serve(body: unknown): void {
    vi.stubGlobal('fetch', async () => new Response(JSON.stringify(body), { status: 200 }));
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('a series named by the id the store wrote', () => {
    it('parses the read of named series, and maps each id to its URI', async () => {
        serve({ series: [{ id: STORE_ID, uri: 'oresmd://ir/USD?type=series&curve_id=USD-SOFR' }] });

        const uris = await curves.seriesUris([STORE_ID]);

        expect(uris.get(STORE_ID)).toBe('oresmd://ir/USD?type=series&curve_id=USD-SOFR');
    });

    it('parses the list a person chooses a blueprint’s output series from', async () => {
        serve({ series: [{ id: STORE_ID, uri: 'oresmd://fx/EUR?type=series' }] });

        const held = await curves.heldSeries();

        expect([...held.entries()]).toEqual([[STORE_ID, 'oresmd://fx/EUR?type=series']]);
    });

    it('still refuses an answer whose id is not a uuid at all', async () => {
        serve({ series: [{ id: 'not-an-id', uri: 'oresmd://ir/USD' }] });

        await expect(curves.heldSeries()).rejects.toThrow();
    });
});

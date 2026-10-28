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
import { z } from 'zod';
import { isStoreUuid, storeUuidSchema } from './ids.js';

/**
 * The identifier shape the store writes.
 *
 * A market series id can be any 8-4-4-4-12 hexadecimal string: the generator
 * that wrote the demo's series did not set the RFC version and variant bits, so
 * a strict reader refuses rows the store really holds and the screen that parses
 * the read fails on a request error.
 */
describe('a row identifier as the store writes one', () => {
    /** An id the store wrote, whose version and variant nibbles are not RFC's. */
    const STORE_ID = 'a1eec62d-a758-f647-de09-d7f2c25d9196';

    it('accepts an id whose version and variant nibbles are not the RFC’s', () => {
        expect(storeUuidSchema.parse(STORE_ID)).toBe(STORE_ID);
        expect(isStoreUuid(STORE_ID)).toBe(true);
    });

    it('accepts the canonical spelling too, so one schema reads every row', () => {
        const canonical = '00000000-0000-4000-8000-000000000001';
        expect(storeUuidSchema.parse(canonical)).toBe(canonical);
    });

    it('is stricter than the store needs and looser than z.uuid, which is the point', () => {
        expect(z.uuid().safeParse(STORE_ID).success).toBe(false);
        expect(storeUuidSchema.safeParse(STORE_ID).success).toBe(true);
    });

    it('trims the space a form or a shell may leave around the value', () => {
        expect(storeUuidSchema.parse(` ${STORE_ID} `)).toBe(STORE_ID);
    });

    it('refuses a value that is not a uuid at all', () => {
        expect(storeUuidSchema.safeParse('oresmd://ir/USD').success).toBe(false);
        expect(storeUuidSchema.safeParse('').success).toBe(false);
        expect(isStoreUuid('00000000000040008000000000000001')).toBe(false);
    });
});

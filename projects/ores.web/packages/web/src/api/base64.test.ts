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
import { base64Of } from './base64.js';

describe('bytes as base64', () => {
    it('agrees with Node on small and empty input', () => {
        expect(base64Of(new Uint8Array())).toBe('');
        expect(base64Of(Uint8Array.from([0, 255, 128]))).toBe(
            Buffer.from([0, 255, 128]).toString('base64'),
        );
    });

    it('agrees with Node on every length near a piece boundary, and on a large file', () => {
        for (const length of [0x7fff, 0x8000, 0x8001, 0x10000, 300_000]) {
            const bytes = Uint8Array.from({ length }, (_, i) => (i * 31) % 256);
            expect(base64Of(bytes)).toBe(Buffer.from(bytes).toString('base64'));
        }
    });
});

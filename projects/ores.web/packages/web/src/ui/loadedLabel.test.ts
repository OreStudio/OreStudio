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
import { loadedIso } from './loadedLabel.js';

describe('when the data on screen was loaded', () => {
    it("is an ISO 8601 timestamp with the reader's offset", () => {
        const at = new Date(2026, 9, 11, 2, 13, 10).getTime();
        expect(loadedIso(at)).toMatch(/^2026-10-11T02:13:10[+-]\d{2}:\d{2}$/);
    });

    it('names the same instant as the moment it was made from', () => {
        const at = new Date(2026, 9, 11, 2, 13, 10).getTime();
        expect(Date.parse(loadedIso(at))).toBe(at);
    });

    it('pads every part, so the text sorts and parses', () => {
        const at = new Date(2026, 0, 2, 3, 4, 5).getTime();
        expect(loadedIso(at)).toMatch(/^2026-01-02T03:04:05/);
    });
});

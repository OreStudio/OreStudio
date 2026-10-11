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
import { qaImageUrl } from './qa.js';

describe('the address of an image beside a scenario', () => {
    it('names the scenario and the file', () => {
        expect(qaImageUrl('ABC', 'shot.png')).toBe('/api/qa/scenarios/ABC/files/shot.png');
    });

    it('encodes a name, so a slash or a dot-dot cannot leave the folder', () => {
        expect(qaImageUrl('ABC', '../x.png')).toBe('/api/qa/scenarios/ABC/files/..%2Fx.png');
        expect(qaImageUrl('A/B', 'a b.png')).toBe('/api/qa/scenarios/A%2FB/files/a%20b.png');
    });
});

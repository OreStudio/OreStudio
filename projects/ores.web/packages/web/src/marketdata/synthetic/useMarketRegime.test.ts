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
import { regimeLabel, stateName } from './useMarketRegime.js';

/** A catalogue that answers with the last part of the key, so the test reads the choice of word. */
const t = (key: string): string => key.split('.').at(-1) ?? key;

describe('the market state as a screen writes it', () => {
    it('names a calm market', () => {
        expect(stateName('calm', t)).toBe('calm');
    });

    it('names an active market', () => {
        expect(stateName('active', t)).toBe('active');
    });

    it('writes the state and the multiplier together', () => {
        expect(regimeLabel({ state: 'calm', multiplier: 0.5, pinned: false }, t)).toBe('calm ×0.5');
    });

    it('writes a pinned active market as the state it is, because the switch says who holds it', () => {
        expect(regimeLabel({ state: 'active', multiplier: 2, pinned: true }, t)).toBe('active ×2');
    });
});

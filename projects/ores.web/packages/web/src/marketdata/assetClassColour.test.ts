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
import { assetClassColour, NAMED_ASSET_CLASSES, UNKNOWN_ASSET_CLASS } from './assetClassColour.js';

describe('the asset class colour scheme', () => {
    it('names every class the reference data carries', () => {
        expect([...NAMED_ASSET_CLASSES].sort()).toEqual([
            'bond',
            'commodity',
            'credit',
            'equity',
            'fx',
            'inflation',
            'interest_rates',
        ]);
    });

    it('gives every class a colour of its own', () => {
        const dots = NAMED_ASSET_CLASSES.map((code) => assetClassColour(code).dot);
        expect(new Set(dots).size).toBe(dots.length);
    });

    it('draws a class in the token named for it', () => {
        expect(assetClassColour('fx')).toEqual({
            dot: 'bg-class-fx',
            text: 'text-class-fx',
        });
    });

    it('draws a class the scheme does not name in the accent', () => {
        expect(assetClassColour('weather')).toEqual(UNKNOWN_ASSET_CLASS);
    });
});

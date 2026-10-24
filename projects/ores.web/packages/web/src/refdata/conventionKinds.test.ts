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
import { CONVENTION_FAMILY_ENTRIES } from '@ores/wire-protocol/generated/refdata/domain/convention_family_registry';
import { ASSET_CLASSES, assetClassOf, isShared } from './conventionKinds.js';

/** The screen's own key for a registry family. */
const kebab = (key: string): string => key.replaceAll('_', '-');

/** The registry families the screen lists alone: the rest stand inside the tenor basis swap. */
const ABSORBED = new Set([
    'convention',
    'tenor_basis_swap_pay_receive',
    'tenor_basis_swap_long_short',
]);

describe('the convention kinds', () => {
    it('give every registry family an asset class, so no kind lands under Other', () => {
        const missing = CONVENTION_FAMILY_ENTRIES.filter((entry) => !ABSORBED.has(entry.key))
            .map((entry) => kebab(entry.key))
            .filter((key) => assetClassOf(key) === undefined);

        expect(missing).toEqual([]);
    });

    it('classify the kinds the screen adds to the registry', () => {
        for (const key of ['tenor-basis-swap', 'tenor', 'currency-pair']) {
            expect(assetClassOf(key), key).toBeDefined();
        }
    });

    it('draw only classes the product model has', () => {
        for (const key of [
            'swap',
            'cds',
            'fx-option',
            'inflation-swap',
            'commodity-forward',
            'bond-yield',
        ]) {
            expect(ASSET_CLASSES).toContain(assetClassOf(key));
        }
    });

    it('hold the four tenant kinds apart from the party kinds', () => {
        expect(['currency-pair', 'ibor-index', 'overnight-index', 'tenor'].every(isShared)).toBe(
            true,
        );
        expect(isShared('swap')).toBe(false);
    });
});

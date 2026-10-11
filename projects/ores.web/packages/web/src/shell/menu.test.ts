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
import { MENU } from './areas.js';
import { AREA_CARDS, FOLDERS, cardOf, type CardArea, type CardId } from './cards.js';
import { PATHS } from './paths.js';

const AREA_PATH: Record<CardArea, string> = {
    refdata: PATHS.refdata,
    organisation: PATHS.organisation,
    marketdata: '/market-data',
    operations: PATHS.operations,
    tenants: '/tenants',
};

/**
 * The menu is one list, in the order the Menu structure page gives: Home,
 * Reference data, Organisation, Market data, Requests, Operations and Tenants.
 * Trades has its place between Market data and Requests when it has screens.
 */
describe('the menu', () => {
    const paths = MENU.map((entry) => entry.to);

    it('keeps the order of the Menu structure page', () => {
        const order = [
            '/',
            '/refdata',
            '/organisation',
            '/market-data',
            '/requests',
            '/operations',
        ];
        expect(paths.filter((path) => order.includes(path))).toEqual(order);
        expect(paths.indexOf('/tenants')).toBeGreaterThan(paths.indexOf('/operations'));
    });

    it('is drawn on Home as one folder card for each entry but Home, in the same order', () => {
        const homed = paths.filter((path) => FOLDERS.some((folder) => folder.to === path));

        expect(homed).toEqual(paths.filter((path) => path !== '/'));
        expect(FOLDERS.map((folder) => folder.to)).toEqual(homed);
    });

    it('asks of an area the reads its cards ask, where the menu names any', () => {
        const mismatches = Object.entries(AREA_CARDS).flatMap(([area, ids]) => {
            const entry = MENU.find((item) => item.to === AREA_PATH[area as CardArea]);
            if (entry?.permission === undefined) {
                return [];
            }
            const ofMenu = [entry.permission].flat().sort();
            const ofCards = [
                ...new Set(
                    (ids as readonly CardId[]).flatMap((id) =>
                        [cardOf(id).permission ?? []].flat(),
                    ),
                ),
            ].sort();
            return JSON.stringify(ofMenu) === JSON.stringify(ofCards)
                ? []
                : [
                      `${area}: the menu asks ${ofMenu.join(', ')} but the cards ask ${ofCards.join(', ')}`,
                  ];
        });

        expect(mismatches).toEqual([]);
    });

    it('lists no address twice', () => {
        expect(new Set(paths).size).toBe(paths.length);
    });
});

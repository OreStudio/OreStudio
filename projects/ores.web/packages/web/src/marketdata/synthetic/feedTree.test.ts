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
import { feedPageOf, folderChain } from './feedTree.js';
import { providersOf } from './providers.js';
import type { FeedFolder } from '../../api/client.js';

function collection(id: string, name: string, provider: string): FeedFolder {
    return {
        id,
        parent_id: 'root',
        name,
        kind: 'collection',
        provider_code: provider,
        fx_feeds: 0,
        ir_feeds: 0,
        has_children: true,
    };
}

const bloomberg = collection('b', 'Bloomberg', 'bloomberg');
const samples = collection('s', '2016 ORE Samples', 'ore');
const more = collection('m', 'Bloomberg Extra', 'bloomberg');

describe('the providers of the collections', () => {
    it('groups the collections by provider code, in code order', () => {
        const entries = providersOf([samples, bloomberg, more]);
        expect(entries.map((entry) => entry.code)).toEqual(['bloomberg', 'ore']);
        expect(entries[0]?.collections.map((one) => one.name)).toEqual([
            'Bloomberg',
            'Bloomberg Extra',
        ]);
    });
});

describe('the trail of a feed', () => {
    it('lists the folders from a feed up to its collection, outermost first', () => {
        const rows = [
            { id: 'r', parent_id: '', name: 'Synthetic', kind: 'root' },
            { id: 'b', parent_id: 'r', name: 'Bloomberg', kind: 'collection' },
            { id: 'a', parent_id: 'b', name: 'FX', kind: 'asset_class' },
        ];
        expect(folderChain(rows, 'a').map((row) => row.name)).toEqual(['Bloomberg', 'FX']);
        expect(folderChain(rows, 'missing')).toEqual([]);
    });

    it('opens a feed on the page of its kind', () => {
        expect(feedPageOf({ kind: 'fx_spot', id: 'x/1' })).toBe('/market-data/synthetic/fx/x%2F1');
        expect(feedPageOf({ kind: 'ir_curve', id: 'y' })).toBe('/market-data/synthetic/ir/y');
    });
});

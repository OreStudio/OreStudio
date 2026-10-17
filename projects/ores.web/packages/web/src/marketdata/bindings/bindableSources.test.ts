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

import { describe, expect, it, vi } from 'vitest';

vi.mock('../../api/bindings.js', () => ({
    bindings: {
        sources: vi.fn(async () => [
            {
                source_name: 'synthetic.bloomberg.usdcad',
                oresmd_uri: 'oresmd://fx/USD?ccy=CAD',
                label: '',
                provider_code: 'bloomberg',
                asset_class_code: 'fx',
                enabled: true,
                running: false,
                held_by: null,
            },
            {
                source_name: 'synthetic.bloomberg.eurusd',
                oresmd_uri: 'oresmd://fx/EUR?ccy=USD',
                label: 'EUR/USD spot rate',
                provider_code: 'bloomberg',
                asset_class_code: 'fx',
                enabled: true,
                running: false,
                held_by: 'synthetic.bloomberg.eurusd.sandbox',
            },
        ]),
    },
}));

import type { BindableSource } from '../../api/bindings.js';
import { BINDABLE, bindableSourcesStore } from './bindableSourcesStore.js';
import { heldBy } from './bindingView.js';

const sources: readonly BindableSource[] = [
    {
        source_name: 'a.free',
        oresmd_uri: 'oresmd://fx/A',
        label: 'A spot rate',
        provider_code: 'bloomberg',
        asset_class_code: 'fx',
        enabled: true,
        running: false,
        held_by: null,
    },
    {
        source_name: 'a.held',
        oresmd_uri: 'oresmd://fx/B',
        label: 'B spot rate',
        provider_code: 'bloomberg',
        asset_class_code: 'fx',
        enabled: true,
        running: false,
        held_by: 'other.running',
    },
];

describe('the sources a party may bind, as the picker reads them', () => {
    it('gives one row per source, labelled, with the series it would write', async () => {
        const rows = await bindableSourcesStore.records(BINDABLE);

        expect(rows.map((row) => row['source_name'])).toEqual([
            'synthetic.bloomberg.usdcad',
            'synthetic.bloomberg.eurusd',
        ]);
        expect(rows.map((row) => row['label'])).toEqual([
            'synthetic.bloomberg.usdcad',
            'EUR/USD spot rate',
        ]);
        expect(rows.map((row) => row['series'])).toEqual([
            'oresmd://fx/USD?ccy=CAD',
            'oresmd://fx/EUR?ccy=USD',
        ]);
    });

    it('answers for its own resource only', async () => {
        expect(await bindableSourcesStore.records('currencies')).toEqual([]);
    });

    it('refuses to write, because the picker only chooses a source', () => {
        expect(() => bindableSourcesStore.remove(BINDABLE, {} as never)).toThrow(/read from/);
    });
});

describe('the running feed that holds a series', () => {
    it('names the feed that holds the series a source would write', () => {
        expect(heldBy(sources, 'a.held')).toBe('other.running');
    });

    it('says nothing when no feed holds it, or the source is not offered', () => {
        expect(heldBy(sources, 'a.free')).toBeUndefined();
        expect(heldBy(sources, 'not.offered')).toBeUndefined();
    });

    it('says nothing while the sources are not yet read', () => {
        expect(heldBy(undefined, 'a.held')).toBeUndefined();
    });
});

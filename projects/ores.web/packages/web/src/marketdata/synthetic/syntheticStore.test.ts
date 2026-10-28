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

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

/**
 * A record screen reads its lists from the store it sits in, and the store
 * decides which service answers. A tab of the Synthetic screen that reads
 * records outside the synthetic store asks the reference data service for them,
 * which has no such resource. A static render never fetches, so the other tests
 * cannot see it; this one reads the source.
 */
function source(name: string): string {
    return readFileSync(fileURLToPath(new URL(`./${name}`, import.meta.url)), 'utf8');
}

describe('the screens of Synthetic', () => {
    it('SyntheticScreen holds every tab inside the synthetic store', () => {
        const text = source('SyntheticScreen.tsx');
        expect(text).toContain('<RecordStoreProvider store={syntheticStore}>');
        // Every tab is a pane of the one record drawn inside the provider.
        expect(text).toContain('<Pane />');
        expect(text).toContain('dashboard: DashboardTab');
        expect(text).toContain('provider: ProviderTab');
        expect(text).toContain('regime: MarketTab');
    });

    for (const name of ['DashboardTab.tsx', 'ProviderTab.tsx']) {
        it(`${name} reads records and leaves the store to the screen that holds it`, () => {
            const text = source(name);
            expect(text).toMatch(/useRecords\(|useProviderFacts\(/);
            expect(text).not.toContain('RecordStoreProvider');
        });
    }

    it('a feed page reads its records from the synthetic store', () => {
        const text = source('FeedScreens.tsx');
        expect(text).toMatch(/useRecords\(|useRecordSource\(/);
        expect(text).toContain('<RecordStoreProvider store={syntheticStore}>');
    });
});

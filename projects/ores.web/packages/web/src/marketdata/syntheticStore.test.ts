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
 * decides which service answers. A screen of the Synthetic folder that reads
 * records without the synthetic store asks the reference data service for them,
 * which has no such resource. A static render never fetches, so the other tests
 * cannot see it; this one reads the source.
 */
const SCREENS = ['FeedOverviewPage.tsx', 'FeedTreePage.tsx', 'FeedScreens.tsx'];

describe('the screens of the Synthetic folder', () => {
    for (const name of SCREENS) {
        it(`${name} reads its records from the synthetic store`, () => {
            const source = readFileSync(
                fileURLToPath(new URL(`./${name}`, import.meta.url)),
                'utf8',
            );
            expect(source).toMatch(/useRecords\(|useRecordSource\(/);
            expect(source).toContain('<RecordStoreProvider store={syntheticStore}>');
        });
    }
});

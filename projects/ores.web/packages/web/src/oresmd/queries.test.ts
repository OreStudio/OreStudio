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

import { afterEach, describe, expect, it, vi } from 'vitest';
import { ORESMD_MAX_BATCH } from '@ores/contracts';
import { api } from '../api/client.js';
import { oresmd } from '../api/oresmd.js';
import { refdataStore, syntheticStore } from '../refdata/store.js';
import { chunksOf, oresmdQueries, parseManyKey } from './queries.js';

afterEach(() => {
    vi.restoreAllMocks();
});

describe('the suggestions of the editor', () => {
    it('read the currencies from the refdata store and never from the store of the screen', async () => {
        const refdata = vi.spyOn(api, 'records').mockResolvedValue([]);
        const synthetic = vi.spyOn(api, 'syntheticRecords').mockResolvedValue([]);
        const options = oresmdQueries.currencies();
        await (options.queryFn as () => Promise<unknown>)();
        expect(refdata).toHaveBeenCalledWith('currencies', undefined);
        expect(synthetic).not.toHaveBeenCalled();
        expect(refdataStore.name).toBe('refdata');
        expect(syntheticStore.name).not.toBe(refdataStore.name);
    });

    it('read the day counters from the classification list', async () => {
        const rows = vi.spyOn(api, 'classificationRows').mockResolvedValue([]);
        await (oresmdQueries.dayCounters().queryFn as () => Promise<unknown>)();
        expect(rows).toHaveBeenCalledWith('day-counter');
    });

    it('are quiet, so a missing list raises no banner, and have keys of their own', () => {
        for (const options of [oresmdQueries.currencies(), oresmdQueries.dayCounters()]) {
            expect(options.meta).toEqual({ quiet: true });
        }
        expect(oresmdQueries.currencies().queryKey).not.toEqual(['records', 'currencies']);
    });
});

describe('the parts of many URLs', () => {
    it('asks about the whole set in one call, and about no URI on its own', async () => {
        const many = vi.spyOn(oresmd, 'parseMany').mockResolvedValue([]);
        const one = vi.spyOn(oresmd, 'parse');
        const options = oresmdQueries.parseMany(['a', 'b', 'a', '']);
        await (options.queryFn as () => Promise<unknown>)();
        expect(many).toHaveBeenCalledTimes(1);
        expect(many).toHaveBeenCalledWith(['a', 'b']);
        expect(one).not.toHaveBeenCalled();
    });

    it('splits a set larger than one call carries into a call a set, and keeps the order asked', async () => {
        const many = vi.spyOn(oresmd, 'parseMany').mockResolvedValue([]);
        const uris = Array.from(
            { length: ORESMD_MAX_BATCH + 1 },
            (_, index) => `u${String(index).padStart(4, '0')}`,
        );
        const options = oresmdQueries.parseMany(uris);
        await (options.queryFn as () => Promise<unknown>)();
        expect(many).toHaveBeenCalledTimes(2);
        expect(many.mock.calls.map(([chunk]) => chunk.length)).toEqual([ORESMD_MAX_BATCH, 1]);
        expect(many.mock.calls[1]).toEqual([['u0200']]);
    });

    it('asks nothing for a set with no URI in it, and reads a URI named twice once', () => {
        expect(oresmdQueries.parseMany(['', '']).enabled).toBe(false);
        expect(chunksOf(['', 'a', 'a'])).toEqual([['a']]);
    });

    it('names the read after the set, so the same set is one answer', () => {
        expect(parseManyKey(['b', 'a', 'b'])).toEqual(parseManyKey(['a', 'b']));
        expect(parseManyKey(['a'])).not.toEqual(parseManyKey(['b']));
    });
});

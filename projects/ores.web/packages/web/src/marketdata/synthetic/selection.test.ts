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
import {
    MOST_NAMED,
    NOTHING,
    allPicked,
    everything,
    isPicked,
    pickedCount,
    selectionRequest,
    sendable,
    setPicked,
} from './selection.js';

describe('the feeds a person picked', () => {
    it('starts with none picked', () => {
        expect(isPicked(NOTHING, 'a')).toBe(false);
        expect(pickedCount(NOTHING, 100)).toBe(0);
    });

    it('picks and drops feeds one by one', () => {
        const some = setPicked(NOTHING, ['a', 'b'], true);
        expect(isPicked(some, 'a')).toBe(true);
        expect(pickedCount(some, 100)).toBe(2);
        const fewer = setPicked(some, ['a'], false);
        expect(isPicked(fewer, 'a')).toBe(false);
        expect(pickedCount(fewer, 100)).toBe(1);
        expect(NOTHING).toEqual({ mode: 'ids', ids: new Set() });
    });

    it('knows when a whole page is picked', () => {
        const page = ['a', 'b', 'c'];
        expect(allPicked(NOTHING, page)).toBe(false);
        expect(allPicked(setPicked(NOTHING, page, true), page)).toBe(true);
        expect(allPicked(everything, [])).toBe(false);
    });

    it('counts every feed the filter matches, less those left out', () => {
        expect(pickedCount(everything, 2850)).toBe(2850);
        const less = setPicked(everything, ['a', 'b'], false);
        expect(less.mode).toBe('all');
        expect(pickedCount(less, 2850)).toBe(2848);
        expect(isPicked(less, 'a')).toBe(false);
        expect(isPicked(less, 'z')).toBe(true);
        expect(isPicked(setPicked(less, ['a'], true), 'a')).toBe(true);
    });

    it('sends the ids of a picked set and nothing of the filter', () => {
        const some = setPicked(NOTHING, ['a', 'b'], true);
        expect(selectionRequest(some, { provider: 'bloomberg', state: 'stopped' })).toEqual({
            feedIds: ['a', 'b'],
        });
    });

    it('sends the filter and the left-out ids of an all-matching set, never its feeds', () => {
        const less = setPicked(everything, ['a'], false);
        expect(selectionRequest(less, { provider: 'bloomberg', state: 'stopped' })).toEqual({
            provider: 'bloomberg',
            state: 'stopped',
            excludedIds: ['a'],
        });
    });

    it('refuses a picked list longer than a request may name', () => {
        const many = setPicked(
            NOTHING,
            Array.from({ length: MOST_NAMED + 1 }, (_, i) => String(i)),
            true,
        );
        expect(sendable(many)).toBe(false);
        expect(sendable(everything)).toBe(true);
    });
});

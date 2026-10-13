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
import { feedBindingActivityListSchema, hasWritten } from './feedBindings.js';

describe('the activity of a feed binding', () => {
    it('reads what the server sends, a binding that has written and one that has not', () => {
        const list = feedBindingActivityListSchema.parse({
            activities: [
                {
                    source_name: 'synthetic.bloomberg.audusd',
                    series_id: '5d0f4a1e-9c2b-4a6e-8f31-0a7b1c2d3e4f',
                    last_tick_at: '2026-10-12 23:50:26Z',
                },
                { source_name: 'synthetic.bloomberg.nzdusd', series_id: '', last_tick_at: '' },
            ],
        });
        expect(list.activities.map(hasWritten)).toEqual([true, false]);
    });

    it('refuses an entry with no source name', () => {
        expect(
            feedBindingActivityListSchema.safeParse({
                activities: [{ series_id: '', last_tick_at: '' }],
            }).success,
        ).toBe(false);
    });
});

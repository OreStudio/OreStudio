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
import { SERIES_WINDOWS, type SeriesWindow } from '@ores/contracts';
import { observedSince } from './series-window.js';

/** An instant late in 2026, so a window that walks back a year stays in range. */
const NOW = Date.parse('2026-10-18T12:00:00Z');

describe('the series window rule', () => {
    it('names one earliest instant per window, counted back from the clock it is given', () => {
        const expected: Readonly<Record<SeriesWindow, string | null>> = {
            hour: '2026-10-18 11:00:00.000000Z',
            day: '2026-10-17 12:00:00.000000Z',
            month: '2026-09-18 12:00:00.000000Z',
            year: '2025-10-18 12:00:00.000000Z',
            all: null,
        };
        for (const window of SERIES_WINDOWS) {
            expect(observedSince(window, NOW), window).toBe(expected[window]);
        }
    });
});

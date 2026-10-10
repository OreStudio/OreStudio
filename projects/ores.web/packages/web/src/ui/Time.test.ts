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
import { formatDateTime, isZeroTimestamp } from './Time.js';

/**
 * A moment a server states is drawn with its time, and not only its date.
 *
 * The screens used to truncate a timestamp to its first ten characters, so
 * every record made on one day drew the same "asked 2026-10-08" whatever hour
 * it was made at. Two requests made hours apart are the case the list is
 * sorted by, so the truncation removed the ordering the reader was looking at.
 */
describe('formatDateTime', () => {
    it('draws a moment with its time, not only its date', () => {
        const drawn = formatDateTime('2026-10-08 13:45:00Z', 'en');

        expect(drawn).not.toBe('2026-10-08');
        expect(drawn).toMatch(/\d{1,2}:\d{2}/);
    });

    it('reads the space-separated form a server sends', () => {
        // A timestamp arrives as "2026-10-08 13:45:00Z". Not every JavaScript
        // engine reads that form, so the separator is what makes it a moment
        // rather than an unreadable string.
        expect(formatDateTime('2026-10-08 13:45:00Z', 'en')).not.toBe('2026-10-08 13:45:00Z');
    });

    it('draws a moment in the reader’s language', () => {
        const inEnglish = formatDateTime('2026-10-08 13:45:00Z', 'en');
        const inFrench = formatDateTime('2026-10-08 13:45:00Z', 'fr');

        expect(inFrench).not.toBe(inEnglish);
    });

    it('returns a value it cannot read as it arrived', () => {
        // Showing the raw value is better than showing a wrong one.
        expect(formatDateTime('', 'en')).toBe('');
        expect(formatDateTime('not a moment', 'en')).toBe('not a moment');
    });
});

describe('isZeroTimestamp', () => {
    it('states the epoch and an empty value as no moment at all', () => {
        expect(isZeroTimestamp('1970-01-01 00:00:00Z')).toBe(true);
        expect(isZeroTimestamp('')).toBe(true);
    });

    it('states a real moment as a moment', () => {
        expect(isZeroTimestamp('2026-09-29 21:03:00Z')).toBe(false);
    });
});

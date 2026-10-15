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
import { QUIET_AFTER_SECONDS, activityByName, secondsSince, statusOf } from './bindingView.js';

const NOW = Date.parse('2026-10-13T10:00:00Z');
const recent = '2026-10-13 09:59:50Z';
const old = '2026-10-13 09:00:00Z';

const synthetic = { kind: 'SYNTHETIC', enabled: true, running: true, lastTickAt: recent, now: NOW };

describe('secondsSince', () => {
    it('reads the wire time and counts from it', () => {
        expect(secondsSince(recent, NOW)).toBe(10);
    });

    it('has no answer for no time or a time that is not one', () => {
        expect(secondsSince('', NOW)).toBeUndefined();
        expect(secondsSince('yesterday', NOW)).toBeUndefined();
    });

    it('never counts a time in the future as negative', () => {
        expect(secondsSince('2026-10-13 10:00:30Z', NOW)).toBe(0);
    });
});

describe('statusOf', () => {
    it('says running for an enabled binding that ticks lately while its feed runs', () => {
        expect(statusOf(synthetic)).toBe('running');
    });

    it('says stopped for an enabled binding whose feed is stopped but ticked lately', () => {
        expect(statusOf({ ...synthetic, running: false })).toBe('stopped');
    });

    it('says quiet when the source has not ticked lately, even while its feed runs', () => {
        expect(statusOf({ ...synthetic, lastTickAt: old })).toBe('quiet');
        expect(QUIET_AFTER_SECONDS).toBe(600);
    });

    it('says quiet when the source has never ticked', () => {
        expect(statusOf({ ...synthetic, lastTickAt: '' })).toBe('quiet');
    });

    it('says disabled for a binding that is not enabled, before it says anything about ticks', () => {
        expect(statusOf({ ...synthetic, enabled: false, lastTickAt: old })).toBe('disabled');
    });

    it('says unsupported for a binding of a real feed, whatever else is true', () => {
        expect(statusOf({ ...synthetic, kind: 'VENDOR', enabled: false })).toBe('unsupported');
    });

    it('says nothing while the running feeds are not known', () => {
        expect(statusOf({ ...synthetic, running: undefined })).toBeUndefined();
    });
});

describe('activityByName', () => {
    it('keys each activity by its source name', () => {
        const map = activityByName([
            { source_name: 'a', series_id: 's1', last_tick_at: recent },
            { source_name: 'b', series_id: '', last_tick_at: '' },
        ]);
        expect(map.get('a')?.series_id).toBe('s1');
        expect(map.get('b')?.series_id).toBe('');
        expect(map.get('c')).toBeUndefined();
    });
});

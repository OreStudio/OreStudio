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
import type { CapacitySample } from '@ores/wire-protocol/browser';
import { daysOfSpaceLeft, freeSpacePoints, latestOf, verdictOf } from './capacity.js';

/**
 * The capacity panel's arithmetic: the verdict on the free space, and the time it
 * lasts at the rate the database grows.
 *
 * Every case is a series of measurements the host could have taken, because the
 * panel's job is to state the figures and never to assume them: a window with one
 * measurement states no rate, and a disk that could not be read states no verdict.
 */

const GiB = 1024 * 1024 * 1024;

function sample(overrides: Partial<CapacitySample> = {}): CapacitySample {
    return {
        sampled_at: '2026-11-28 01:00:00.000000Z',
        database_name: 'ores_dev_solid_dirac',
        host_name: 'Wiener',
        measured_path: '/',
        database_bytes: 1800 * 1024 * 1024,
        disk_free_bytes: 500 * GiB,
        disk_total_bytes: 900 * GiB,
        note: '',
        ...overrides,
    };
}

describe('the verdict on the free space', () => {
    it('reads a disk with room to spare as fine', () => {
        expect(verdictOf([sample()])).toBe('ok');
    });

    it('warns before the disk is nearly full, and alarms when it is', () => {
        expect(
            verdictOf([sample({ disk_free_bytes: 200 * GiB, disk_total_bytes: 1000 * GiB })]),
        ).toBe('ok');
        expect(
            verdictOf([sample({ disk_free_bytes: 150 * GiB, disk_total_bytes: 1000 * GiB })]),
        ).toBe('warn');
        expect(
            verdictOf([sample({ disk_free_bytes: 50 * GiB, disk_total_bytes: 1000 * GiB })]),
        ).toBe('alarm');
    });

    it('states no verdict for a disk it could not read', () => {
        expect(verdictOf([sample({ disk_free_bytes: null, note: 'permission denied' })])).toBe(
            'unknown',
        );
        expect(verdictOf([sample({ disk_total_bytes: null })])).toBe('unknown');
        expect(verdictOf([])).toBe('unknown');
    });

    it('reads the newest measurement, not the oldest', () => {
        expect(latestOf([sample(), sample({ database_bytes: 42 })])?.database_bytes).toBe(42);
        expect(latestOf([])).toBeNull();
    });
});

describe('the time the free space lasts', () => {
    it('states the days at the rate the free space fell over the window', () => {
        /* 20 GiB over two days, 480 GiB left: it lasts 48 more days. */
        const series = [
            sample({ sampled_at: '2026-11-26 00:00:00.000000Z', disk_free_bytes: 500 * GiB }),
            sample({ sampled_at: '2026-11-27 00:00:00.000000Z', disk_free_bytes: 490 * GiB }),
            sample({ sampled_at: '2026-11-28 00:00:00.000000Z', disk_free_bytes: 480 * GiB }),
        ];
        expect(daysOfSpaceLeft(series)).toBeCloseTo(48, 5);
    });

    it('states nothing when the window cannot state a rate', () => {
        expect(daysOfSpaceLeft([])).toBeNull();
        expect(daysOfSpaceLeft([sample()])).toBeNull();
        expect(
            daysOfSpaceLeft([
                sample({ sampled_at: '2026-11-28 00:00:00.000000Z', disk_free_bytes: 500 * GiB }),
                sample({ sampled_at: '2026-11-28 00:00:00.000000Z', disk_free_bytes: 490 * GiB }),
            ]),
        ).toBeNull();
    });

    it('states nothing when the free space is not falling', () => {
        const series = [
            sample({ sampled_at: '2026-11-27 00:00:00.000000Z', disk_free_bytes: 480 * GiB }),
            sample({ sampled_at: '2026-11-28 00:00:00.000000Z', disk_free_bytes: 490 * GiB }),
        ];
        expect(daysOfSpaceLeft(series)).toBeNull();
    });

    it('measures between the ends that were readable, not across an unreadable one', () => {
        const series = [
            sample({ sampled_at: '2026-11-26 00:00:00.000000Z', disk_free_bytes: null }),
            sample({ sampled_at: '2026-11-27 00:00:00.000000Z', disk_free_bytes: 500 * GiB }),
            sample({ sampled_at: '2026-11-28 00:00:00.000000Z', disk_free_bytes: 490 * GiB }),
        ];
        expect(daysOfSpaceLeft(series)).toBeCloseTo(49, 5);
        expect(freeSpacePoints(series)).toEqual([500 * GiB, 490 * GiB]);
    });
});

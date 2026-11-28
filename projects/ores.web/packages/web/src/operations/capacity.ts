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

/**
 * What the capacity panel reads out of the installation's own series.
 *
 * The panel answers one question — how long the free space lasts at the rate the
 * database grows — and every step of that answer is arithmetic over the read,
 * kept here so a test can state the figures and read the verdict.
 */

import type { CapacitySample } from '@ores/wire-protocol/browser';

/** The share of the filesystem below which the panel warns, and below which it alarms. */
export const FREE_SPACE_WARN_SHARE = 0.2;
export const FREE_SPACE_ALARM_SHARE = 0.1;

/** How the disk reads: room to spare, filling, or nearly full. */
export type CapacityVerdict = 'ok' | 'warn' | 'alarm' | 'unknown';

/** The newest measurement the series holds, or null before the first sample. */
export function latestOf(samples: readonly CapacitySample[]): CapacitySample | null {
    return samples.length === 0 ? null : (samples[samples.length - 1] ?? null);
}

/**
 * The verdict on the free space, from the share of the filesystem it holds.
 *
 * A measurement with no free space states no verdict, because the panel cannot
 * tell a full disk from a disk it could not read; the sample's note says which.
 */
export function verdictOf(samples: readonly CapacitySample[]): CapacityVerdict {
    const latest = latestOf(samples);
    if (latest === null || latest.disk_free_bytes === null) {
        return 'unknown';
    }
    const total = latest.disk_total_bytes;
    if (total === null || total <= 0) {
        return 'unknown';
    }
    const share = latest.disk_free_bytes / total;
    if (share < FREE_SPACE_ALARM_SHARE) {
        return 'alarm';
    }
    return share < FREE_SPACE_WARN_SHARE ? 'warn' : 'ok';
}

/**
 * How many days the free space lasts at the rate the free space is falling.
 *
 * The rate comes from the first and the last measurement of the window, so a
 * short horizon answers about the last day and a long one about the last week.
 * It answers nothing when the window holds fewer than two measurements, when the
 * free space is not falling, and when the disk was not readable at either end: a
 * number invented from one point is worse than no number.
 */
export function daysOfSpaceLeft(samples: readonly CapacitySample[]): number | null {
    const measured = samples.filter(
        (sample): sample is CapacitySample & { readonly disk_free_bytes: number } =>
            sample.disk_free_bytes !== null,
    );
    if (measured.length < 2) {
        return null;
    }
    const first = measured[0]!;
    const last = measured[measured.length - 1]!;
    const from = Date.parse(first.sampled_at.replace(' ', 'T'));
    const to = Date.parse(last.sampled_at.replace(' ', 'T'));
    const days = (to - from) / 86_400_000;
    if (!Number.isFinite(days) || days <= 0) {
        return null;
    }
    const perDay = (last.disk_free_bytes - first.disk_free_bytes) / days;
    if (perDay >= 0) {
        return null;
    }
    return last.disk_free_bytes / -perDay;
}

/** The free space of every measurement that has one, oldest first, for the trend. */
export function freeSpacePoints(samples: readonly CapacitySample[]): readonly number[] {
    const points: number[] = [];
    for (const sample of samples) {
        if (sample.disk_free_bytes !== null) {
            points.push(sample.disk_free_bytes);
        }
    }
    return points;
}

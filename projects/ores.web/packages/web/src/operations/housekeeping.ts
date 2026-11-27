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
 * What the housekeeping screens read a store as, from
 * doc/knowledge/journeys/operations/journey_see_what_the_system_keeps.org.
 *
 * The server decides whether a store is bounded and which mechanism leaves it
 * so; this module only names the answer in the words a person reads and puts the
 * stores in the order the list exists to show. Nothing here re-derives boundedness
 * from a policy's presence, because a policy whose action never runs reads as a
 * bound and is the one case the screen exists to expose.
 */

import type { StoreBound, StoreSample } from '@ores/wire-protocol/browser';

/**
 * How many of a store's measurements the page plots.
 *
 * The sampler writes one point an hour, so a week is 168 points: enough to see a
 * run that happened, a policy that drops and the store that regrows into it, and
 * few enough that the line stays a shape rather than a smear.
 */
export const STORE_SAMPLES_FOR_PLOT = 168;

/** How a store reads at a glance: bounded by something, by its own keying, or not at all. */
export type BoundVerdict = 'bounded' | 'keyed' | 'unbounded';

/**
 * Why a store is not bounded, in the order the screen states them.
 *
 * `notBoundable` is the store no period can bound — the calendar dates, whose
 * bound is a delete by range — and it is not a fault: it is a decision the
 * platform made and has not built a mechanism for.
 */
export type UnboundedReason = 'off' | 'notApplied' | 'failed' | 'noPolicy' | 'notBoundable';

export interface StoreVerdict {
    readonly verdict: BoundVerdict;
    readonly reason: UnboundedReason | null;
}

/**
 * The verdict a store carries, and the reason when the news is bad.
 *
 * `bounded_now` is the read's answer and is never second-guessed here: a policy
 * that is off, one whose action the sweep does not perform and one whose last run
 * failed all leave the store kept for ever, and the read says so. The reason is
 * read from the mechanism and the last run, in that order, because a policy that
 * is off is not also a failure.
 */
export function verdictOf(store: StoreBound): StoreVerdict {
    if (store.bound_kind === 'keying') {
        return { verdict: 'keyed', reason: null };
    }
    if (store.bound.bounded_now) {
        return { verdict: 'bounded', reason: null };
    }
    if (store.bound_kind === 'none') {
        return { verdict: 'unbounded', reason: 'notBoundable' };
    }
    if (store.bound.policy_code === null) {
        return { verdict: 'unbounded', reason: 'noPolicy' };
    }
    if (!store.bound.policy_enabled || store.bound.mechanism === 'policy_off') {
        return { verdict: 'unbounded', reason: 'off' };
    }
    if (store.bound.mechanism === 'storage_lifecycle' || store.bound.mechanism === 'node_release') {
        return { verdict: 'unbounded', reason: 'notApplied' };
    }
    if (store.last_run.outcome === 'failed') {
        return { verdict: 'unbounded', reason: 'failed' };
    }
    /*
     * The read said nothing bounds this store and every cause above is absent,
     * so the run failed in a way the outcome does not name. Reporting the failure
     * is the honest answer: the alternative is to call a store bounded because
     * nothing said otherwise.
     */
    return { verdict: 'unbounded', reason: 'failed' };
}

/** The counts and the total the list states before it states any row. */
export interface StoreFigures {
    readonly stores: number;
    readonly bounded: number;
    readonly unbounded: number;
    readonly unmeasured: number;
    readonly bytes: number;
}

/**
 * What the list says at its head: how many stores, how many are bound, the
 * number that has to reach zero, and the ground they occupy.
 *
 * A store nothing measures counts as unbounded, because it is: a store whose
 * size is unknown may be growing with nothing stopping it, and counting it as
 * bound would be an assumption in the wrong direction.
 */
export function summarise(stores: readonly StoreBound[]): StoreFigures {
    let bounded = 0;
    let unmeasured = 0;
    let bytes = 0;
    for (const store of stores) {
        const { verdict } = verdictOf(store);
        if (verdict === 'bounded' || verdict === 'keyed') {
            bounded += 1;
        }
        if (store.size.sampled_at === null) {
            unmeasured += 1;
        }
        bytes += store.size.size_bytes;
    }
    return {
        stores: stores.length,
        bounded,
        unbounded: stores.length - bounded,
        unmeasured,
        bytes,
    };
}

/** Which stores the list shows, and in what order. */
export type StoreFilter = 'all' | 'unbounded';
export type StoreOrder = 'attention' | 'size' | 'name';

/**
 * The stores the list shows, in the order the person asked for.
 *
 * `attention` is the list's own order: the unbounded stores first, because they
 * are the work, and then the largest, because the work is also the space. It is
 * the order the summary states the page in, and the others are there for when the
 * question is only "what is big" or "where is a store I know the name of".
 */
export function arrange(
    stores: readonly StoreBound[],
    filter: StoreFilter,
    order: StoreOrder,
): readonly StoreBound[] {
    const shown =
        filter === 'unbounded'
            ? stores.filter((store) => verdictOf(store).verdict === 'unbounded')
            : stores;
    const byName = (left: StoreBound, right: StoreBound): number =>
        left.display_name.localeCompare(right.display_name);
    const sorted = [...shown];
    switch (order) {
        case 'size':
            sorted.sort(
                (left, right) =>
                    right.size.size_bytes - left.size.size_bytes || byName(left, right),
            );
            break;
        case 'name':
            sorted.sort(byName);
            break;
        case 'attention':
            sorted.sort((left, right) => {
                const leftBound = verdictOf(left).verdict === 'unbounded' ? 0 : 1;
                const rightBound = verdictOf(right).verdict === 'unbounded' ? 0 : 1;
                return (
                    leftBound - rightBound ||
                    right.size.size_bytes - left.size.size_bytes ||
                    byName(left, right)
                );
            });
            break;
    }
    return sorted;
}

const UNITS = ['B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB'] as const;

/**
 * A store's size as a person reads it.
 *
 * The figure is stated in the largest unit that keeps it above one, because a
 * store is read to be compared with another and 1.9 GiB beside 283 MiB says
 * which is larger faster than 2040109465 does.
 */
export function formatSize(bytes: number): string {
    if (!Number.isFinite(bytes) || bytes <= 0) {
        return '0 B';
    }
    let value = bytes;
    let unit = 0;
    while (value >= 1024 && unit < UNITS.length - 1) {
        value /= 1024;
        unit += 1;
    }
    return unit === 0 ? `${Math.round(value)} B` : `${value.toFixed(1)} ${UNITS[unit]}`;
}

/**
 * What a store's count counts.
 *
 * The size is always bytes and the count is the store's own unit — rows of a
 * relation, objects under a prefix, files in a folder — so a count without its
 * unit states nothing.
 */
export type MeasureUnit = 'relationRows' | 'objects' | 'files' | 'unknown';

export function measureUnit(measureKind: string | null): MeasureUnit {
    switch (measureKind) {
        case 'relation_rows':
            return 'relationRows';
        case 'objects':
            return 'objects';
        case 'files':
            return 'files';
        default:
            return 'unknown';
    }
}

/** One store's movement between the first and the last measurement of its series. */
export interface StoreTrend {
    readonly from: number;
    readonly to: number;
    readonly delta: number;
    /** How many days the two measurements are apart, as a fraction. */
    readonly days: number;
    /** The change a week, or null when the two points are the same instant. */
    readonly perWeek: number | null;
}

/**
 * The movement of one store's series, or nothing when it has fewer than two
 * points.
 *
 * The rate is stated a week at a time because a week is the span a capacity
 * question is asked over; it is null when both measurements are the same
 * instant, so a screen cannot divide by zero and report an infinite growth.
 */
export function trendOf(samples: readonly StoreSample[]): StoreTrend | null {
    const measured = samples.filter(
        (sample): sample is StoreSample & { readonly sampled_at: string } =>
            sample.sampled_at !== null,
    );
    if (measured.length < 2) {
        return null;
    }
    const first = measured[0]!;
    const last = measured[measured.length - 1]!;
    const from = Date.parse(first.sampled_at.replace(' ', 'T'));
    const to = Date.parse(last.sampled_at.replace(' ', 'T'));
    const days = (to - from) / 86_400_000;
    const delta = last.size_bytes - first.size_bytes;
    return {
        from: first.size_bytes,
        to: last.size_bytes,
        delta,
        days,
        perWeek: days > 0 ? (delta / days) * 7 : null,
    };
}

/** The sizes of one store's series, oldest first, for the plot. */
export function plotPoints(samples: readonly StoreSample[]): readonly number[] {
    return samples.map((sample) => sample.size_bytes);
}

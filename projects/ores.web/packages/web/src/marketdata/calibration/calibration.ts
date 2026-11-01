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
 */
import type { CalibratedFeed } from '../../api/client.js';

/** The asset classes whose feeds calibrate from fixings today. */
export const CALIBRATED_CLASSES = ['fx'] as const;

/** How a feed resolved: from a fixing, on its fixed level, or refused by the policy. */
export type FeedOutcome = 'fixing' | 'fixed' | 'refused';

export function outcomeOf(feed: CalibratedFeed): FeedOutcome {
    if (feed.refused) {
        return 'refused';
    }
    return feed.start.from_fixing ? 'fixing' : 'fixed';
}

/** A feed states an exception to its class policy: its own items or its own window. */
export function overrides(feed: CalibratedFeed): boolean {
    return feed.explicit_items || feed.window_days_override != null;
}

/** A feed a person should look at: it took no fixing, or it overrides the policy. */
export function needsAttention(feed: CalibratedFeed): boolean {
    return outcomeOf(feed) !== 'fixing' || overrides(feed);
}

export const FILTERS = ['all', 'fixing', 'kept', 'attention', 'overridden'] as const;
export type CalibrationFilter = (typeof FILTERS)[number];

export function filterOf(value: string | null): CalibrationFilter {
    return FILTERS.find((f) => f === value) ?? 'all';
}

export function visibleFeeds(
    feeds: readonly CalibratedFeed[],
    filter: CalibrationFilter,
): readonly CalibratedFeed[] {
    if (filter === 'fixing') {
        return feeds.filter((feed) => feed.start.from_fixing);
    }
    if (filter === 'kept') {
        return feeds.filter((feed) => !feed.start.from_fixing && !feed.refused);
    }
    if (filter === 'attention') {
        return feeds.filter(needsAttention);
    }
    if (filter === 'overridden') {
        return feeds.filter(overrides);
    }
    return feeds;
}

/** The provider order with @p code moved by @p delta places, kept inside the list. */
export function moveProvider(
    order: readonly string[],
    code: string,
    delta: number,
): readonly string[] {
    const from = order.indexOf(code);
    const to = from + delta;
    if (from < 0 || to < 0 || to >= order.length) {
        return order;
    }
    const out = [...order];
    out.splice(from, 1);
    out.splice(to, 0, code);
    return out;
}

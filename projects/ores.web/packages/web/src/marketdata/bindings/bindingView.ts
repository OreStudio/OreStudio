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

import type { FeedBindingActivity } from '@ores/contracts';
import type { BindableSource } from '../../api/bindings.js';

/** The kind of a binding that names a generated producer. */
export const SYNTHETIC = 'SYNTHETIC';

/**
 * How long an enabled binding may go without a tick before the screen calls it quiet.
 * Nothing states how long a feed may be quiet yet, so the threshold is provisional.
 */
export const QUIET_AFTER_SECONDS = 600;

/** What a binding is doing, as the status column says it. */
export type BindingStatus = 'unsupported' | 'disabled' | 'quiet' | 'running' | 'stopped';

/** Whether a binding names a generated producer. Any other kind is read only here. */
export function isSynthetic(kind: string): boolean {
    return kind === SYNTHETIC;
}

/** The seconds since a time written as YYYY-MM-DD HH:MM:SSZ, or nothing when there is no time. */
export function secondsSince(at: string, now: number): number | undefined {
    if (at === '') {
        return undefined;
    }
    const then = Date.parse(at.replace(' ', 'T'));
    return Number.isNaN(then) ? undefined : Math.max(0, Math.round((now - then) / 1000));
}

/**
 * The status of a binding. A binding of a real feed is not supported. A binding that
 * is not enabled stores nothing. An enabled binding whose source has written nothing,
 * or nothing lately, is quiet whether its feed runs or not, so a silent source cannot
 * hide behind a running one. Otherwise it is running or stopped as its feed is. While
 * the running feeds are not yet known, nothing is said.
 */
export function statusOf(input: {
    readonly kind: string;
    readonly enabled: boolean;
    readonly running: boolean | undefined;
    readonly lastTickAt: string;
    readonly now: number;
}): BindingStatus | undefined {
    if (!isSynthetic(input.kind)) {
        return 'unsupported';
    }
    if (!input.enabled) {
        return 'disabled';
    }
    if (input.running === undefined) {
        return undefined;
    }
    const age = secondsSince(input.lastTickAt, input.now);
    if (age === undefined || age > QUIET_AFTER_SECONDS) {
        return 'quiet';
    }
    return input.running ? 'running' : 'stopped';
}

/** The activity of each binding by its source name. */
export function activityByName(
    activities: readonly FeedBindingActivity[],
): ReadonlyMap<string, FeedBindingActivity> {
    return new Map(activities.map((activity) => [activity.source_name, activity]));
}

/**
 * The running feed that already holds the series the named source would write, or
 * nothing when no feed holds it.
 *
 * A series holds one feed's points at a time, so a source whose series is held cannot
 * be bound while that feed runs. The answer is the feed controller's, through the
 * sources read; nothing here decides it.
 */
export function heldBy(
    sources: readonly BindableSource[] | undefined,
    sourceName: string,
): string | undefined {
    return sources?.find((source) => source.source_name === sourceName)?.held_by ?? undefined;
}

/**
 * Whether Revert is offered on a version of a binding.
 *
 * The offering is a statement about a write, so it needs a person who may write a
 * binding and a binding that is not a vendor's, which records a real feed the screen
 * does not control. The newest version has nothing to revert to, and a version whose
 * series a running feed holds cannot be written back without taking a series another
 * feed is publishing.
 */
export function revertOffer(input: {
    readonly kind: string;
    readonly mayWrite: boolean;
    readonly version: number;
    readonly newest: number;
    readonly heldBy: string | undefined;
}): boolean {
    return (
        input.mayWrite &&
        isSynthetic(input.kind) &&
        input.version < input.newest &&
        input.heldBy === undefined
    );
}

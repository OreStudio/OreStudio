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
 * What the resolve screen asks, and what its one answer can be.
 *
 * The screen has four answers to give and one reply to read. The reply is an
 * answer whether the server accepted the question or declined it, so a refusal
 * is not an exception; only a failed request is. This module holds that
 * distinction as data, with no React in it, so it can be tested on its own.
 */

import type { TenorResolution } from '../api/tenors.js';

/**
 * The answers the person gives: which tenor, under which convention, on which
 * calendar, counted from when, and from which spot date when the rule needs one.
 */
export interface ResolveQuery {
    readonly conventionCode: string;
    readonly tenorCode: string;
    readonly calendarCode: string;
    readonly horizon: string;
    readonly spot: string;
}

/**
 * Whether the answers are enough to ask the question.
 *
 * A convention that measures from spot cannot answer without one, and the
 * engine refuses rather than assume the pair's spot lag, so the page asks
 * before the request instead of after the refusal.
 */
export function canResolve(query: ResolveQuery, needsSpot: boolean): boolean {
    return (
        query.conventionCode !== '' &&
        query.tenorCode !== '' &&
        query.calendarCode !== '' &&
        query.horizon.trim() !== '' &&
        (!needsSpot || query.spot.trim() !== '')
    );
}

export type ResolveAnswer =
    | { readonly kind: 'idle' }
    | { readonly kind: 'asking' }
    | { readonly kind: 'answered'; readonly resolution: TenorResolution }
    | { readonly kind: 'refused'; readonly resolution: TenorResolution }
    | { readonly kind: 'unreachable'; readonly message: string };

/**
 * The three dates a resolution reports, each empty when it did not reach it.
 *
 * The server fills them as it walks the rule, so a refusal can carry an anchor
 * and no adjusted date. Each stands on its own; a page draws the ones it got.
 */
export interface ReachedDates {
    readonly anchor: string;
    readonly beforeRoll: string;
    readonly adjusted: string;
}

/** The words for the fields a refusal can name, so no wire name reaches a person. */
const FIELD_LABEL_KEYS: Readonly<Record<string, string>> = {
    calendar_code: 'refdata.tenors.calendar',
    tenor_code: 'refdata.tenors.tenor',
    convention_code: 'refdata.tenors.convention',
    spot: 'refdata.tenors.spot',
    tenor_anchor: 'refdata.tenors.anchor',
    resolution_algorithm: 'refdata.tenors.algorithm',
    schedule_code: 'refdata.tenors.schedule',
    tenor_convention_resolutions: 'refdata.tenors.tenor',
};

/** The label key for a field a refusal names, or nothing when it names a field we do not draw. */
export function fieldLabelKey(field: string): string | undefined {
    return FIELD_LABEL_KEYS[field];
}

export function answerOf(
    resolution: TenorResolution | undefined,
    error: Error | null,
    asking: boolean,
): ResolveAnswer {
    if (error !== null) {
        return { kind: 'unreachable', message: error.message };
    }
    if (asking) {
        return { kind: 'asking' };
    }
    if (resolution === undefined) {
        return { kind: 'idle' };
    }
    return resolution.result.outcome === 'ok'
        ? { kind: 'answered', resolution }
        : { kind: 'refused', resolution };
}

/**
 * The dates the resolution reached, or nothing when it reached none.
 *
 * A refusal reports empty dates rather than a sentinel, and the server is the
 * only walk of the rule there is: a date it did not report is a date the screen
 * must not invent.
 */
export function reachedDates(resolution: TenorResolution): ReachedDates | undefined {
    const dates = {
        anchor: resolution.anchorDate,
        beforeRoll: resolution.dateBeforeRoll,
        adjusted: resolution.adjustedDate,
    };
    return dates.anchor === '' && dates.beforeRoll === '' && dates.adjusted === ''
        ? undefined
        : dates;
}

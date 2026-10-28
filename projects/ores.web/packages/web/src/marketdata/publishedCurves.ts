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

import type { PublishedCurvePoint, PublishedCurveSet } from '../api/client.js';

/**
 * The reading a published set takes, from
 * doc/prototypes/market_data/published-curves.org.
 *
 * A published set is a row a person reads: the curve, the party, the as-of the
 * set was built at, the account that applied the approval, the instant it moved
 * and the state it is in. A candidate is a set nobody has approved; an ended set
 * is one that was in use and is not any more. This module holds the readings the
 * screens make of those rows — which sets the overlay draws, how a comparison
 * marks each pillar, what a value implies — and no rendering, so each reading is
 * asserted without a browser.
 */

/** The most sets the overlay draws, as the IR feed's ticks panel caps its history. */
export const MAX_OVERLAY_SETS = 20;

/** How many sets the overlay draws before a person asks for another number. */
export const DEFAULT_OVERLAY_SETS = 5;

/** The most baselines the comparison offers: the set in use and two earlier as-ofs. */
export const BASELINE_CHOICES = 3;

/** The four states one publish record reads as. */
export type SetState = 'published' | 'superseded' | 'withdrawn' | 'candidate';

/**
 * The state the record is in.
 *
 * The server states it, so this is a reading of the answer and not a second
 * derivation: a fact the screen worked out again could disagree with the record.
 */
export function stateOf(set: PublishedCurveSet): SetState {
    switch (set.state_code) {
        case 'published':
        case 'superseded':
        case 'withdrawn':
            return set.state_code;
        default:
            return 'candidate';
    }
}

/** Whether a person has applied an approval and the set moved. */
export function isPublished(set: PublishedCurveSet): boolean {
    return set.published_at !== null;
}

/** Whether the set left use, by a withdrawal or by a successor. */
export function hasEnded(set: PublishedCurveSet): boolean {
    const state = stateOf(set);
    return state === 'withdrawn' || state === 'superseded';
}

/** The published sets, oldest first, which is the order the overlay draws them in. */
export function publishedSetsOf(sets: readonly PublishedCurveSet[]): readonly PublishedCurveSet[] {
    return sets
        .filter(isPublished)
        .slice()
        .sort((left, right) => ((left.published_at ?? '') < (right.published_at ?? '') ? -1 : 1));
}

/**
 * The record the page is about: the newest set of the curve and party.
 *
 * A curve whose newest set has ended still has a record, and that record is the
 * newest one: the list's ended-set filter is how a person reaches it.
 */
export function newestPublished(sets: readonly PublishedCurveSet[]): PublishedCurveSet | undefined {
    return publishedSetsOf(sets).at(-1);
}

/** The set a desk prices from, which is the newest one that has not ended. */
export function setInUse(sets: readonly PublishedCurveSet[]): PublishedCurveSet | undefined {
    return publishedSetsOf(sets)
        .filter((set) => stateOf(set) === 'published')
        .at(-1);
}

/** The sets nobody has approved, newest as-of first: what a comparison may be against. */
export function candidatesOf(sets: readonly PublishedCurveSet[]): readonly PublishedCurveSet[] {
    return sets
        .filter((set) => stateOf(set) === 'candidate')
        .slice()
        .sort((left, right) => (left.as_of < right.as_of ? 1 : -1));
}

/**
 * The instants the comparison's selector offers: each published set's as-of,
 * newest first, and no more than three.
 *
 * Two sets published at one as-of hold the same pillars, so the selector names
 * each instant once: a choice is a baseline and not a record.
 */
export function baselineChoicesOf(sets: readonly PublishedCurveSet[]): readonly string[] {
    const instants: string[] = [];
    for (const set of publishedSetsOf(sets).slice().reverse()) {
        if (!instants.includes(set.as_of)) instants.push(set.as_of);
        if (instants.length === BASELINE_CHOICES) break;
    }
    return instants;
}

/**
 * The baseline the comparison draws: the set at the chosen as-of, or the newest
 * published set when nobody has chosen.
 *
 * Where two sets share an as-of, the newer one is the baseline, because that is
 * the one whose values the series holds.
 */
export function baselineAt(
    sets: readonly PublishedCurveSet[],
    asOf?: string,
): PublishedCurveSet | undefined {
    const published = publishedSetsOf(sets);
    if (asOf === undefined || asOf === '') return published.at(-1);
    const matching = published.filter((set) => set.as_of === asOf);
    return matching.at(-1) ?? published.at(-1);
}

/** A number the value's text states, or undefined when it states none. */
export function numberOf(text: string): number | undefined {
    if (text.trim() === '') return undefined;
    const value = Number(text);
    return Number.isFinite(value) ? value : undefined;
}

/** How long a tenor is, in years, or undefined when the label is not a tenor. */
export function yearsOf(tenor: string): number | undefined {
    const label = tenor.trim();
    if (label === '') return undefined;
    // An overnight, tomorrow-next or spot-next tenor is a day, near enough for a
    // rate a reader compares by eye.
    if (label === 'ON' || label === 'TN' || label === 'SN') return 1 / 365;

    let years = 0;
    let rest = label;
    const unitYears: Readonly<Record<string, number>> = {
        D: 1 / 365,
        W: 1 / 52,
        M: 1 / 12,
        Y: 1,
    };
    while (rest !== '') {
        const match = /^(\d+)([DWMY])/.exec(rest);
        if (match === null) return undefined;
        const amount = Number(match[1]);
        const unit = unitYears[match[2] ?? ''];
        if (unit === undefined || !Number.isFinite(amount)) return undefined;
        years += amount * unit;
        rest = rest.slice(match[0].length);
    }
    return years === 0 ? undefined : years;
}

/** What one published point states: the rate and the discount factor, where both follow. */
export interface Quoted {
    readonly rate: number | undefined;
    readonly discount: number | undefined;
}

/**
 * The rate and the discount factor one point states.
 *
 * The stored value denotes one of the two, and the tenor gives the time the
 * other is derived over: a discount factor states the rate that reproduces it,
 * and a rate states the factor it compounds to. A tenor that is not a period
 * leaves the derived side empty, because a number with no time behind it would
 * be an invention and not the record.
 */
export function quoted(point: PublishedCurvePoint): Quoted {
    const value = numberOf(point.value);
    const years = yearsOf(point.pillar);
    if (value === undefined) return { rate: undefined, discount: undefined };

    if (point.value_kind === 'discount') {
        const rate = years === undefined || value <= 0 ? undefined : -Math.log(value) / years;
        return { rate, discount: value };
    }
    if (point.value_kind === 'rate') {
        const discount = years === undefined ? undefined : Math.exp(-value * years);
        return { rate: value, discount };
    }
    return { rate: undefined, discount: undefined };
}

/** How one pillar of a comparison reads. */
export type MarkKind = 'changed' | 'added' | 'dropped' | 'unchanged';

/** One line of the comparison's table: a pillar, the two values and the change. */
export interface CompareRow {
    readonly pillar: string;
    readonly mark: MarkKind;
    readonly published: number | undefined;
    readonly candidate: number | undefined;
    readonly publishedText: string;
    readonly candidateText: string;
    /** The change in the value's own unit, signed, or undefined when a side is absent. */
    readonly delta: number | undefined;
    /** What the change is stated in: basis points for a rate, the unit itself otherwise. */
    readonly unit: 'bp' | '';
}

/**
 * The points of two sets compared, one line for each pillar either holds.
 *
 * A pillar only one set holds is drawn and not hidden: a comparison that hides a
 * removal is not a comparison. The pillars are ordered by the baseline's own and
 * then by the candidate's additions, so a new pillar stands where it was added
 * rather than at the end.
 */
export function comparePoints(
    published: readonly PublishedCurvePoint[],
    candidate: readonly PublishedCurvePoint[],
): readonly CompareRow[] {
    const publishedOf = new Map(published.map((point) => [point.pillar, point]));
    const candidateOf = new Map(candidate.map((point) => [point.pillar, point]));
    const pillars: string[] = [];
    for (const point of published) if (!pillars.includes(point.pillar)) pillars.push(point.pillar);
    for (const point of candidate) if (!pillars.includes(point.pillar)) pillars.push(point.pillar);

    return pillars.map((pillar) => {
        const left = publishedOf.get(pillar);
        const right = candidateOf.get(pillar);
        const publishedValue = left === undefined ? undefined : numberOf(left.value);
        const candidateValue = right === undefined ? undefined : numberOf(right.value);
        const unit: 'bp' | '' = (right ?? left)?.value_kind === 'rate' ? 'bp' : '';
        const delta =
            publishedValue === undefined || candidateValue === undefined
                ? undefined
                : candidateValue - publishedValue;
        const mark: MarkKind =
            left === undefined
                ? 'added'
                : right === undefined
                  ? 'dropped'
                  : delta === undefined || delta === 0
                    ? 'unchanged'
                    : 'changed';
        return {
            pillar,
            mark,
            published: publishedValue,
            candidate: candidateValue,
            publishedText: left?.value ?? '',
            candidateText: right?.value ?? '',
            delta,
            unit,
        };
    });
}

/** How many lines of each mark the comparison holds. */
export interface Tally {
    readonly changed: number;
    readonly added: number;
    readonly dropped: number;
    readonly unchanged: number;
}

export function tallyOf(rows: readonly CompareRow[]): Tally {
    const tally = { changed: 0, added: 0, dropped: 0, unchanged: 0 };
    for (const row of rows) tally[row.mark] += 1;
    return tally;
}

/**
 * Whether anything changed between the two sets.
 *
 * Nothing changed means the candidate matches the published set point for
 * point, so there is nothing to raise and the published set stays the one in use.
 */
export function somethingChanged(rows: readonly CompareRow[]): boolean {
    const { changed, added, dropped } = tallyOf(rows);
    return changed + added + dropped > 0;
}

/**
 * The pillars a chart draws, and each series' values over them.
 *
 * The pillars are the first set's, and then any the later sets add; a set that
 * holds no pillar is a gap in its line and never a zero, because a missing point
 * and a point at zero are different facts.
 */
export function alignOnPillars(
    sets: readonly { readonly points: readonly PublishedCurvePoint[] }[],
): { readonly pillars: readonly string[]; readonly values: readonly (number | null)[][] } {
    const pillars: string[] = [];
    for (const set of sets)
        for (const point of set.points)
            if (!pillars.includes(point.pillar)) pillars.push(point.pillar);

    const values = sets.map((set) => {
        const byPillar = new Map(set.points.map((point) => [point.pillar, numberOf(point.value)]));
        return pillars.map((pillar) => byPillar.get(pillar) ?? null);
    });
    return { pillars, values };
}

/**
 * The sentence the points tab says about how many sets it drew.
 *
 * The prototype states four cases and the fourth is the common one, so the four
 * are named here and not worked out again in the drawing.
 */
export function overlayCaption(
    asked: number,
    publishedCount: number,
    drawn: number,
): { readonly key: string; readonly values: Readonly<Record<string, string>> } {
    if (publishedCount <= 1) return { key: 'marketdata.published.points.one', values: {} };
    if (publishedCount < asked)
        return {
            key: 'marketdata.published.points.fewer',
            values: { asked: String(asked), published: String(publishedCount) },
        };
    if (drawn >= asked)
        return { key: 'marketdata.published.points.newest', values: { count: String(drawn) } };
    return {
        key: 'marketdata.published.points.newest',
        values: { count: String(drawn) },
    };
}

/** The change a comparison states, in the words the row carries. */
export function changeText(row: CompareRow): string {
    if (row.mark === 'added') return 'marketdata.published.compare.newPillar';
    if (row.mark === 'dropped') return 'marketdata.published.compare.droppedPillar';
    if (row.mark === 'unchanged') return '';
    if (row.delta === undefined) return '';
    return row.unit === 'bp' ? 'marketdata.published.compare.basisPoints' : '';
}

/** A signed change, in basis points when the value is a rate. */
export function deltaText(row: CompareRow): string {
    if (row.delta === undefined || row.mark !== 'changed') return '';
    const shown = row.unit === 'bp' ? row.delta * 10000 : row.delta;
    const sign = shown > 0 ? '+' : '';
    return `${sign}${shown.toFixed(row.unit === 'bp' ? 1 : 6)}${row.unit === 'bp' ? ' bp' : ''}`;
}

/** The instant a set last moved, which is the line a record page ends on. */
export function lastChangedAt(set: PublishedCurveSet): string | null {
    return set.withdrawn_at ?? set.superseded_at ?? set.published_at ?? null;
}

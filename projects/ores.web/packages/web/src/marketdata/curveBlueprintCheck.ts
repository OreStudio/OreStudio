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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import type { RecordRow } from '../api/client.js';
import { show } from '../refdata/records.js';

/**
 * The readings the blueprint's Check tab makes of a recipe, of the points the
 * server builds, and of the shape those points imply.
 *
 * The judgements are the screen's own: no operation validates a recipe and
 * `marketdata.v1.ops.compute_curve` answers with points and nothing about them,
 * so every answer here is derived from what the screen read and says that it is
 * derived. Nothing here renders, so each reading is asserted without a browser.
 */

/** The interpolation methods the bootstrap understands, in the order the form offers them. */
export const INTERPOLATION_METHODS = [
    'LOG_LINEAR_DISCOUNT',
    'FLAT_FORWARD_THEN_LOG_LINEAR',
    'CUBIC_SPLINE',
] as const;

/** The two roles a blueprint plays in its family. */
export const CURVE_FAMILY_ROLES = ['FUNDING', 'PROJECTION'] as const;

/** The three instrument roles one pillar bootstraps as. */
export const PILLAR_ROLES = ['DEPOSIT', 'FRA', 'SWAP'] as const;

/** The two-segment method, whose split tenor is a knot of its own. */
export const TWO_SEGMENT_METHOD = 'FLAT_FORWARD_THEN_LOG_LINEAR';

/** The tenor a deposit starts at, because a deposit starts today or at spot. */
export const SPOT = 'SPOT';

/** The months a period tenor stands for; a tenor of another shape stands for none. */
function monthsOf(code: string): number | null {
    const match = /^([0-9]+)([DWMY])$/.exec(code.trim().toUpperCase());
    if (match === null) {
        return null;
    }
    const count = Number.parseInt(match[1] ?? '', 10);
    const unit = match[2];
    if (!Number.isInteger(count) || count <= 0) {
        return null;
    }
    switch (unit) {
        case 'D':
            return count / 30;
        case 'W':
            return count / 4;
        case 'M':
            return count;
        default:
            return count * 12;
    }
}

/**
 * Where a tenor falls on the ladder, in months.
 *
 * Spot is the front of the ladder and a period tenor ranks by its duration. A
 * special tenor, such as the FOMC meeting codes, stands for a schedule-resolved
 * date the tenor table does not carry a duration for: it ranks after spot and
 * before every period tenor, which is right for a meeting-dated short end that
 * hands over to a swap grid, and is the one case this reading does not settle.
 */
export function tenorRank(code: string): number {
    const tenor = code.trim().toUpperCase();
    if (tenor === SPOT) {
        return 0;
    }
    const months = monthsOf(tenor);
    if (months !== null) {
        return 1000 + months;
    }
    const special = /^([0-9]+)[A-Z]$/.exec(tenor);
    if (special !== null) {
        return 1 + Number.parseInt(special[1] ?? '0', 10);
    }
    return 999;
}

/** The maturity order of two tenors: the shorter first, and equal tenors by their text. */
export function compareTenors(left: string, right: string): number {
    const rank = tenorRank(left) - tenorRank(right);
    return rank !== 0 ? rank : show(left).localeCompare(show(right));
}

/** The pillars in maturity order, which is the order the panel and the sequence show. */
export function inMaturityOrder(pillars: readonly RecordRow[]): readonly RecordRow[] {
    return [...pillars].sort((left, right) =>
        compareTenors(show(left['end_tenor_code']), show(right['end_tenor_code'])),
    );
}

/** Which act a failed check names as the way to fix the recipe. */
export type Remedy = 'pillars' | 'edit' | 'add' | 'funding';

/** One judgement of the recipe, and the act that answers it when it fails. */
export interface BlueprintCheck {
    readonly id: string;
    readonly passed: boolean;
    readonly remedy: Remedy;
}

/**
 * Whether the split tenor closes the ladder the way the method says it does.
 *
 * A single-segment method splits nowhere, so the value is the last pillar's end
 * tenor. The two-segment method splits inside the ladder, at or after the first
 * pillar's end and at or before the last, so both segments hold points and the
 * split may be the knot the first segment closes on.
 */
export function splitTenorPasses(
    method: string,
    splitTenor: string,
    pillars: readonly RecordRow[],
): boolean {
    const ordered = inMaturityOrder(pillars);
    const first = ordered[0];
    const last = ordered.at(-1);
    if (first === undefined || last === undefined) {
        return false;
    }
    const split = tenorRank(splitTenor);
    if (method === TWO_SEGMENT_METHOD) {
        return (
            split >= tenorRank(show(first['end_tenor_code'])) &&
            split <= tenorRank(show(last['end_tenor_code']))
        );
    }
    return splitTenor === show(last['end_tenor_code']);
}

/** What a check reads: the recipe, its pillars in maturity order, and the blueprint it names. */
export interface BlueprintRecipe {
    readonly config: RecordRow;
    readonly pillars: readonly RecordRow[];
    /** The blueprint a projection names as its discount curve, once read. */
    readonly discount: RecordRow | undefined;
}

/**
 * The six checks the recipe itself must pass, in the order the tab lists them.
 *
 * Each is answered from what the screen read. A check whose answer needs a
 * blueprint the reader cannot see is answered as failed, because an unproven
 * reference is not a passing one, and the failure says which read is missing.
 */
export function checkRecipe(recipe: BlueprintRecipe): readonly BlueprintCheck[] {
    const { config, pillars, discount } = recipe;
    const ordered = inMaturityOrder(pillars);
    const role = show(config['curve_family_role']);
    const projection = role === 'PROJECTION';
    const named = show(config['discount_curve_config_id']);
    const self = show(config['id']);
    const namedIsFunding =
        discount !== undefined && show(discount['curve_family_role']) === 'FUNDING';
    return [
        {
            id: 'one_pillar',
            passed: pillars.length > 0,
            remedy: 'add',
        },
        {
            id: 'maturity_order',
            passed: ordered.every((pillar, index) => {
                const before = ordered[index - 1];
                return (
                    before === undefined ||
                    tenorRank(show(pillar['end_tenor_code'])) >
                        tenorRank(show(before['end_tenor_code']))
                );
            }),
            remedy: 'pillars',
        },
        {
            id: 'deposit_from_spot',
            passed: ordered.every(
                (pillar) =>
                    show(pillar['curve_role_code']) !== 'DEPOSIT' ||
                    show(pillar['start_tenor_code']) === SPOT,
            ),
            remedy: 'pillars',
        },
        {
            id: 'split_tenor',
            passed: splitTenorPasses(
                show(config['interpolation_method']),
                show(config['split_tenor_code']),
                pillars,
            ),
            remedy: 'edit',
        },
        {
            id: 'projection_funding',
            passed:
                !projection ||
                (named !== '' &&
                    named !== self &&
                    namedIsFunding &&
                    discount !== undefined &&
                    show(discount['currency_code']) === show(config['currency_code'])),
            remedy: 'funding',
        },
        {
            id: 'funding_not_projection',
            passed:
                !projection ||
                (discount !== undefined && show(discount['curve_family_role']) !== 'PROJECTION'),
            remedy: 'funding',
        },
    ];
}

/** One point the server built: its pillar, the date it matures, and the factor it implies. */
export interface CurvePoint {
    readonly pillar: string;
    readonly date: string;
    readonly discountFactor: number;
    /** The instantaneous forward to the next point; the last point has none. */
    readonly forward: number | null;
    /** The days from this point to the next, which the forward is quoted over. */
    readonly daysToNext: number | null;
}

/** One point of the server's answer: the pillar it answers and the factor at its date. */
export interface ComputedPoint {
    readonly pointId: string;
    readonly date: string;
    readonly discountFactor: number;
}

/** The days between two ISO dates, or null when either does not parse. */
function daysBetween(from: string, to: string): number | null {
    const start = Date.parse(`${from}T00:00:00Z`);
    const end = Date.parse(`${to}T00:00:00Z`);
    if (Number.isNaN(start) || Number.isNaN(end)) {
        return null;
    }
    return Math.round((end - start) / 86_400_000);
}

/**
 * The points in maturity order, each with the forward the two neighbouring
 * discount factors imply.
 *
 * The forward is the screen's own arithmetic: it is the rate that carries the
 * earlier factor to the later one over the days between them, on an actual/365
 * basis. A pillar the server answered no point for keeps its place with no
 * factor, so the gap shows where the market had nothing rather than closing up.
 */
export function buildPoints(
    pillars: readonly RecordRow[],
    computed: readonly ComputedPoint[],
): readonly CurvePoint[] {
    const points = new Map(computed.map((point) => [point.pointId, point]));
    const answered = inMaturityOrder(pillars)
        .map((pillar) => {
            const pillarCode = show(pillar['end_tenor_code']);
            return { pillar: pillarCode, point: points.get(pillarCode) };
        })
        .filter(
            (entry): entry is { pillar: string; point: ComputedPoint } => entry.point !== undefined,
        );
    return answered.map((entry, index) => {
        const next = answered[index + 1];
        const days = next === undefined ? null : daysBetween(entry.point.date, next.point.date);
        const forward =
            next === undefined || days === null || days <= 0
                ? null
                : (entry.point.discountFactor / next.point.discountFactor - 1) / (days / 365);
        return {
            pillar: entry.pillar,
            date: entry.point.date,
            discountFactor: entry.point.discountFactor,
            forward,
            daysToNext: days,
        };
    });
}

/** The pillars the server answered no point for, in maturity order. */
export function unfilledPillars(
    pillars: readonly RecordRow[],
    computed: readonly ComputedPoint[],
): readonly string[] {
    const answered = new Set(computed.map((point) => point.pointId));
    return inMaturityOrder(pillars)
        .map((pillar) => show(pillar['end_tenor_code']))
        .filter((pillar) => !answered.has(pillar));
}

/** What the screen read off the points, said as a sentence and not as the server's verdict. */
export interface Finding {
    readonly kind: 'negative_forward' | 'jump' | 'gap';
    readonly pillar: string;
    /** The size of the effect, for a reader who wants the number beside the words. */
    readonly value: number | null;
}

/** How far one forward may move from the one before it before the move is called a jump. */
export const JUMP_SHARE = 0.5;

/**
 * The findings the points support: a forward below zero, a forward that moves
 * more than half its predecessor, and a pillar the server built no point for.
 */
export function findingsOf(
    points: readonly CurvePoint[],
    unfilled: readonly string[],
): readonly Finding[] {
    const found: Finding[] = [];
    points.forEach((point, index) => {
        if (point.forward !== null && point.forward < 0) {
            found.push({ kind: 'negative_forward', pillar: point.pillar, value: point.forward });
        }
        const before = points[index - 1];
        if (
            point.forward !== null &&
            before !== undefined &&
            before.forward !== null &&
            before.forward !== 0 &&
            Math.abs(point.forward - before.forward) > Math.abs(before.forward) * JUMP_SHARE
        ) {
            found.push({ kind: 'jump', pillar: point.pillar, value: point.forward });
        }
    });
    for (const pillar of unfilled) {
        found.push({ kind: 'gap', pillar, value: null });
    }
    return found;
}

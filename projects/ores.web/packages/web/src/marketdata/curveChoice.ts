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

/**
 * Which curves the curve analytics offers, and which two it may draw together.
 *
 * A curve is read within its currency: a projection beside the overnight curve
 * it discounts off, or two tenors of one index. Two currencies are compared only
 * overnight against overnight, because that pair says one thing, the rate
 * differential between two economies; a term curve of one currency beside the
 * overnight of another says nothing either curve means.
 */

const FUNDING = 'FUNDING';

/**
 * One curve configuration, as the pickers and the pair need it.
 *
 * A name alone does not say what a curve is: USD-SOFR-FOMC and USD-LIBOR-1M are
 * two names in a list until the role and the index tenor beside them say which
 * one is the overnight and which one is a tenor of LIBOR projected off it.
 */
export interface CurveOption {
    readonly id: string;
    readonly name: string;
    readonly currency: string;
    readonly role: string;
    readonly indexTenor: string;
    /** The funding curve a projection names as the one it discounts off. */
    readonly discountId: string;
}

export function isFunding(curve: CurveOption | undefined): boolean {
    return curve?.role === FUNDING;
}

/** Every currency a curve is held for, in code order. */
export function currenciesOf(options: readonly CurveOption[]): readonly string[] {
    return [...new Set(options.map((option) => option.currency))].sort((a, b) =>
        a.localeCompare(b),
    );
}

/** The curves of one currency: its funding curves first, then the rest, each by name. */
export function curvesOf(
    options: readonly CurveOption[],
    currency: string,
): readonly CurveOption[] {
    return options
        .filter((option) => option.currency === currency)
        .sort(
            (a, b) => Number(!isFunding(a)) - Number(!isFunding(b)) || a.name.localeCompare(b.name),
        );
}

/** The curves that may be drawn beside the chosen one, in the order the picker offers them. */
export interface BesideChoices {
    /** The chosen curve's own currency: the curve it discounts off first, then the rest. */
    readonly same: readonly CurveOption[];
    /** The funding curves of the other currencies, offered only beside a funding curve. */
    readonly other: readonly CurveOption[];
}

export function besideChoices(
    options: readonly CurveOption[],
    chosen: CurveOption | undefined,
    across: boolean,
): BesideChoices {
    if (chosen === undefined) return { same: [], other: [] };
    const same = curvesOf(options, chosen.currency)
        .filter((option) => option.id !== chosen.id)
        .sort((a, b) => Number(a.id !== chosen.discountId) - Number(b.id !== chosen.discountId));
    const other =
        across && isFunding(chosen)
            ? options
                  .filter((option) => option.currency !== chosen.currency && isFunding(option))
                  .sort(
                      (a, b) =>
                          a.currency.localeCompare(b.currency) || a.name.localeCompare(b.name),
                  )
            : [];
    return { same, other };
}

/** Whether a curve is another currency's funding curve that may stand beside the chosen one. */
export function comparesAcross(
    chosen: CurveOption | undefined,
    beside: CurveOption | undefined,
): boolean {
    return (
        chosen !== undefined &&
        beside !== undefined &&
        isFunding(chosen) &&
        isFunding(beside) &&
        chosen.currency !== beside.currency
    );
}

/** A funding curve and a projection curve drawn together, or undefined for any other pair. */
export function pairOf(
    chosen: CurveOption | undefined,
    beside: CurveOption | undefined,
): { readonly funding: CurveOption; readonly projection: CurveOption } | undefined {
    if (chosen === undefined || beside === undefined) return undefined;
    if (chosen.role === FUNDING && beside.role === 'PROJECTION') {
        return { funding: chosen, projection: beside };
    }
    if (chosen.role === 'PROJECTION' && beside.role === FUNDING) {
        return { funding: beside, projection: chosen };
    }
    return undefined;
}

/** Two currencies' overnight curves drawn together, or undefined for any other pair. */
export function differentialOf(
    chosen: CurveOption | undefined,
    beside: CurveOption | undefined,
): { readonly left: CurveOption; readonly right: CurveOption } | undefined {
    if (chosen === undefined || beside === undefined || !comparesAcross(chosen, beside)) {
        return undefined;
    }
    return { left: chosen, right: beside };
}

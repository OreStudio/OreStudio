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

import type { OfficialFixings, OfficialProvider, OfficialSeries } from '../api/client.js';

/** How a provider gives a pair: as it publishes it, the other way round, or through its pivot. */
export type PairRoute = 'direct' | 'inverse' | 'cross';

/** The series a provider's fixings of a pair are made from, and how. */
export interface PairPlan {
    readonly provider: OfficialProvider;
    readonly route: PairRoute;
    /** The fixing of the pivot in the quote currency; absent when the quote is the pivot. */
    readonly toQuote: OfficialSeries | undefined;
    /** The fixing of the pivot in the base currency; absent when the base is the pivot. */
    readonly toBase: OfficialSeries | undefined;
}

/** A provider's pivot: the base of its fixings, which is the same for all of them. */
export function pivotOf(provider: OfficialProvider): string | undefined {
    return provider.series[0]?.base;
}

/**
 * How @p provider gives @p base / @p quote, or undefined when it quotes neither leg. A
 * provider whose pivot is the base gives the pair as it publishes it; one whose pivot is the
 * quote gives its inverse; any other gives it as a cross of two of its own fixings, which
 * the ECB's terms count as a change and so it is shown as derived.
 */
export function planPair(
    provider: OfficialProvider,
    base: string,
    quote: string,
): PairPlan | undefined {
    const pivot = pivotOf(provider);
    if (pivot === undefined || base === quote) {
        return undefined;
    }
    const leg = (currency: string): OfficialSeries | undefined =>
        provider.series.find((s) => s.quote === currency);
    if (pivot === base) {
        const toQuote = leg(quote);
        return toQuote === undefined
            ? undefined
            : { provider, route: 'direct', toQuote, toBase: undefined };
    }
    if (pivot === quote) {
        const toBase = leg(base);
        return toBase === undefined
            ? undefined
            : { provider, route: 'inverse', toQuote: undefined, toBase };
    }
    const toQuote = leg(quote);
    const toBase = leg(base);
    return toQuote === undefined || toBase === undefined
        ? undefined
        : { provider, route: 'cross', toQuote, toBase };
}

/** The series the plans need read. */
export function seriesOf(plans: readonly PairPlan[]): readonly string[] {
    const ids = new Set<string>();
    for (const plan of plans) {
        if (plan.toQuote !== undefined) ids.add(plan.toQuote.id);
        if (plan.toBase !== undefined) ids.add(plan.toBase.id);
    }
    return [...ids];
}

/**
 * The pair by date as @p plan gives it from @p fixings: the units of the quote that one
 * unit of the base buys. A date that lacks a leg has no value.
 */
export function pairValues(plan: PairPlan, fixings: OfficialFixings): ReadonlyMap<string, number> {
    const values = (series: OfficialSeries | undefined): ReadonlyMap<string, number> =>
        new Map((series === undefined ? [] : (fixings[series.id] ?? [])).map((f) => [f.date, Number(f.value)]));
    const toQuote = values(plan.toQuote);
    const toBase = values(plan.toBase);
    const out = new Map<string, number>();
    const dates = plan.route === 'inverse' ? toBase.keys() : toQuote.keys();
    for (const date of dates) {
        const q = plan.route === 'inverse' ? 1 : toQuote.get(date);
        const b = plan.route === 'direct' ? 1 : toBase.get(date);
        if (q !== undefined && b !== undefined && b !== 0 && Number.isFinite(q / b)) {
            out.set(date, q / b);
        }
    }
    return out;
}

/** The currencies any official provider quotes, its pivots included, sorted. */
export function currenciesOf(providers: readonly OfficialProvider[]): readonly string[] {
    const all = new Set<string>();
    for (const provider of providers) {
        for (const series of provider.series) {
            all.add(series.base);
            all.add(series.quote);
        }
    }
    return [...all].sort();
}

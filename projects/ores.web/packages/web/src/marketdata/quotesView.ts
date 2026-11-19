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

import type { Quote, QuoteFeed } from '../api/quotes.js';

/**
 * What the quotes view shows, as functions of the quotes it holds, so the screen's
 * test drives the same choices the page makes.
 */

/** The source the rail picks: every vendor, or one feed. */
export const ALL_VENDORS = '*';

/** The key of a feed: its provider and its id. */
export function feedKey(feed: { readonly provider: string; readonly feed: string }): string {
    return `${feed.provider}|${feed.feed}`;
}

/** The key of a quote: its feed and its datum. */
function quoteKey(q: Quote): string {
    return `${feedKey(q)}|${q.uri}`;
}

/** The quotes held, with the board's newer quotes put in place of the older ones. */
export function merged(
    held: ReadonlyMap<string, Quote>,
    newer: readonly Quote[],
): Map<string, Quote> {
    const out = new Map(held);
    for (const q of newer) out.set(quoteKey(q), q);
    return out;
}

/** What the person narrows the quotes by. */
export interface QuoteFilter {
    readonly source: string;
    readonly assetClass: string;
    readonly instrumentType: string;
    readonly find: string;
}

/**
 * The quotes the table shows, the rows of one datum together: by series, by point,
 * then by provider.
 */
export function shownQuotes(
    held: ReadonlyMap<string, Quote>,
    feeds: readonly QuoteFeed[],
    filter: QuoteFilter,
): Quote[] {
    const allowed = new Set(feeds.map(feedKey));
    const find = filter.find.trim().toLowerCase();
    return [...held.values()]
        .filter((q) => allowed.has(feedKey(q)))
        .filter((q) => filter.source === ALL_VENDORS || feedKey(q) === filter.source)
        .filter((q) => filter.assetClass === '' || q.asset_class === filter.assetClass)
        .filter((q) => filter.instrumentType === '' || q.instrument_type === filter.instrumentType)
        .filter((q) => find === '' || `${q.series} ${q.point}`.toLowerCase().includes(find))
        .sort(
            (a, b) =>
                a.asset_class.localeCompare(b.asset_class) ||
                a.instrument_type.localeCompare(b.instrument_type) ||
                a.series.localeCompare(b.series) ||
                a.point.localeCompare(b.point, undefined, { numeric: true }) ||
                a.provider.localeCompare(b.provider),
        );
}

/** The instrument types the held quotes of an asset class carry, for the type filter. */
export function instrumentTypes(held: ReadonlyMap<string, Quote>, assetClass: string): string[] {
    return [
        ...new Set(
            [...held.values()]
                .filter((q) => assetClass === '' || q.asset_class === assetClass)
                .map((q) => q.instrument_type),
        ),
    ].sort();
}

/** One series of the table and the quotes of its points. */
export interface SeriesGroup {
    readonly key: string;
    readonly quotes: readonly Quote[];
    /** The newest of its quotes, which the closed row shows. */
    readonly newest: Quote;
}

/** The quotes grouped by series, in the order the table shows them. */
export function bySeries(rows: readonly Quote[]): SeriesGroup[] {
    const groups = new Map<string, Quote[]>();
    for (const q of rows) {
        const key = `${q.instrument_type}|${q.series_uri}`;
        const list = groups.get(key) ?? [];
        list.push(q);
        groups.set(key, list);
    }
    return [...groups].map(([key, quotes]) => ({
        key,
        quotes,
        newest: quotes.reduce((a, b) => (a.at >= b.at ? a : b)),
    }));
}

/** Every vendor's quote of one datum, the mid of them and the spread between them. */
export interface AllQuotes {
    readonly quotes: readonly Quote[];
    readonly mid: number | null;
    readonly spread: number | null;
}

/** Every quote the session may watch of the datum @p uri. */
export function allQuotesOf(
    held: ReadonlyMap<string, Quote>,
    feeds: readonly QuoteFeed[],
    uri: string,
): AllQuotes {
    const allowed = new Set(feeds.map(feedKey));
    const quotes = [...held.values()]
        .filter((q) => q.uri === uri && allowed.has(feedKey(q)))
        .sort((a, b) => a.provider.localeCompare(b.provider));
    const values = quotes.map((q) => Number(q.value)).filter((v) => Number.isFinite(v));
    if (values.length === 0) return { quotes, mid: null, spread: null };
    return {
        quotes,
        mid: values.reduce((a, b) => a + b, 0) / values.length,
        spread: Math.max(...values) - Math.min(...values),
    };
}

/**
 * The milliseconds of an instant as the server writes it, which separates the date
 * from the time with a space rather than the T a browser's parser requires.
 */
export function instantOf(at: string): number {
    return Date.parse(at.replace(' ', 'T'));
}

/** Whether a time is older than the view calls stale. */
export function isStale(at: string | null, now: number, limitMs = 60_000): boolean {
    return at === null || now - instantOf(at) > limitMs;
}

/** The change from a quote's previous value, or zero when either is not a number. */
export function changeOf(q: Quote): number {
    const value = Number(q.value);
    const previous = Number(q.previous);
    return Number.isFinite(value) && Number.isFinite(previous) ? value - previous : 0;
}

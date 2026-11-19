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
 * What the quotes view shows, as functions of the quotes it holds, so the tests
 * drive the same choices the page makes.
 *
 * The unit of the screen is the series, not the vendor's quote: a vendor is a
 * column, and a series is drawn in its own shape. A series with no coordinate
 * is one quote, such as an FX spot; with one it is a curve, by tenor; with two
 * it is a surface, tenor down and the second coordinate across. The boards are
 * the series of one asset class and one shape.
 */

/** A vendor the session may watch: one feed, its provider's rank and its last tick. */
export interface Vendor {
    readonly key: string;
    readonly provider: string;
    readonly feed: string;
    readonly source: string;
    /** The lower, the more trusted: a quote is measured against the highest-ranked vendor. */
    readonly precedence: number;
    readonly lastAt: string | null;
}

/** The key of a feed: its provider and its id. */
export function feedKey(feed: { readonly provider: string; readonly feed: string }): string {
    return `${feed.provider}|${feed.feed}`;
}

/** The vendors of the feeds, in the order the providers are trusted. */
export function vendorsOf(feeds: readonly QuoteFeed[]): Vendor[] {
    return feeds
        .map((f) => ({
            key: feedKey(f),
            provider: f.provider,
            feed: f.feed,
            source: f.source,
            precedence: f.precedence,
            lastAt: f.last_at,
        }))
        .sort((a, b) => a.precedence - b.precedence || a.provider.localeCompare(b.provider));
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

/** What a series is, which decides its unit, its change and how far apart two vendors may be. */
export type Kind = 'spot' | 'rate' | 'vol' | 'plain';

/** How the values of a type are read. A type not named here is shown as it is. */
const KINDS: Readonly<Record<string, Kind>> = {
    fx_spot: 'spot',
    fx_option: 'vol',
};

export function kindOf(assetClass: string, instrumentType: string): Kind {
    const named = KINDS[instrumentType];
    if (named !== undefined) return named;
    if (assetClass === 'interest_rates' && !/option|swaption|cap|floor/.test(instrumentType))
        return 'rate';
    return 'plain';
}

/** The shape of a series, by how many coordinates its points have. */
export type Shape = 'quote' | 'curve' | 'surface' | 'datums';

function shapeOf(coordinates: number): Shape {
    return coordinates === 0
        ? 'quote'
        : coordinates === 1
          ? 'curve'
          : coordinates === 2
            ? 'surface'
            : 'datums';
}

/**
 * The coordinates of a quote. The server writes the point as the values of the
 * datum's coordinate fields in the order its schema lists them, joined by a
 * space, and none of those values holds a space.
 */
export function coordinatesOf(q: Quote): string[] {
    return q.point === '' ? [] : q.point.split(' ');
}

const UNIT_DAYS: Readonly<Record<string, number>> = { D: 1, W: 7, M: 30.4375, Y: 365.25 };
const NAMED_DAYS: Readonly<Record<string, number>> = { ON: 1, TN: 2, SN: 3 };

/** The days a tenor stands for, or nothing for a coordinate that is not a tenor. */
export function tenorDays(text: string): number | undefined {
    const named = NAMED_DAYS[text.toUpperCase()];
    if (named !== undefined) return named;
    const m = /^(\d+)([DWMY])$/i.exec(text);
    return m === null ? undefined : Number(m[1]) * (UNIT_DAYS[m[2]!.toUpperCase()] ?? 0);
}

/** Two coordinates in tenor order when both are tenors, else in natural text order. */
export function byTenor(a: string, b: string): number {
    const da = tenorDays(a);
    const db = tenorDays(b);
    if (da !== undefined && db !== undefined) return da - db;
    if (da !== undefined) return -1;
    if (db !== undefined) return 1;
    return a.localeCompare(b, undefined, { numeric: true });
}

/** The order an FX vol surface's quotes are read in, across. */
const STRATEGIES = ['ATM', '25RR', '25BF', '10RR', '10BF'];

/** Two columns of a surface: its strategies in the desk's order, then the rest. */
export function byColumn(a: string, b: string): number {
    const ia = STRATEGIES.indexOf(a);
    const ib = STRATEGIES.indexOf(b);
    if (ia >= 0 && ib >= 0) return ia - ib;
    if (ia >= 0) return -1;
    if (ib >= 0) return 1;
    return byTenor(a, b);
}

/** One point of a series and each vendor's quote of it. */
export interface Point {
    readonly uri: string;
    readonly coordinates: readonly string[];
    readonly label: string;
    /** By vendor key. */
    readonly quotes: ReadonlyMap<string, Quote>;
}

/** One series: its points, in order, and what its values are. */
export interface Series {
    readonly key: string;
    readonly name: string;
    readonly assetClass: string;
    readonly instrumentType: string;
    readonly kind: Kind;
    readonly shape: Shape;
    readonly points: readonly Point[];
}

/** The series of one asset class and one shape: one tab of the screen. */
export interface Board {
    readonly key: string;
    readonly assetClass: string;
    readonly shape: Shape;
    readonly series: readonly Series[];
}

/** A pair's name as a desk writes it: EUR/USD, not EUR USD. */
function nameOf(q: Quote): string {
    const words = q.series.split(' ');
    return q.asset_class === 'fx' && words.length === 2 ? words.join('/') : q.series;
}

const SHAPE_ORDER: readonly Shape[] = ['quote', 'curve', 'surface', 'datums'];

/** The boards of every quote of the vendors, each series with its points in order. */
export function boardsOf(held: ReadonlyMap<string, Quote>, vendors: readonly Vendor[]): Board[] {
    const watched = new Set(vendors.map((v) => v.key));
    const series = new Map<string, { first: Quote; points: Map<string, Map<string, Quote>> }>();
    for (const q of held.values()) {
        const vendor = feedKey(q);
        if (!watched.has(vendor)) continue;
        const one = series.get(q.series_uri) ?? { first: q, points: new Map() };
        const point = one.points.get(q.uri) ?? new Map<string, Quote>();
        point.set(vendor, q);
        one.points.set(q.uri, point);
        series.set(q.series_uri, one);
    }
    const boards = new Map<string, Series[]>();
    for (const [key, { first, points }] of series) {
        const listed = [...points].map(([uri, quotes]): Point => {
            const any = quotes.values().next().value as Quote;
            const coordinates = coordinatesOf(any);
            return { uri, coordinates, label: any.point, quotes };
        });
        const dimensions = Math.max(0, ...listed.map((p) => p.coordinates.length));
        listed.sort((a, b) => {
            for (let i = 0; i < Math.min(a.coordinates.length, b.coordinates.length); i++) {
                const c =
                    i === 1
                        ? byColumn(a.coordinates[i]!, b.coordinates[i]!)
                        : byTenor(a.coordinates[i]!, b.coordinates[i]!);
                if (c !== 0) return c;
            }
            return a.coordinates.length - b.coordinates.length;
        });
        const shape = shapeOf(dimensions);
        const one: Series = {
            key,
            name: nameOf(first),
            assetClass: first.asset_class,
            instrumentType: first.instrument_type,
            kind: kindOf(first.asset_class, first.instrument_type),
            shape,
            points: listed,
        };
        const boardKey = `${first.asset_class}.${shape}`;
        boards.set(boardKey, [...(boards.get(boardKey) ?? []), one]);
    }
    return [...boards]
        .map(([key, list]) => {
            const [assetClass = '', shape = 'quote'] = key.split('.') as [string, Shape];
            return { key, assetClass, shape, series: list };
        })
        .sort(
            (a, b) =>
                a.assetClass.localeCompare(b.assetClass) ||
                SHAPE_ORDER.indexOf(a.shape) - SHAPE_ORDER.indexOf(b.shape),
        );
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

/** The newest tick of a series from the vendors shown. */
export function newestOf(s: Series, shown: ReadonlySet<string>): number {
    let newest = 0;
    for (const p of s.points)
        for (const [vendor, q] of p.quotes)
            if (shown.has(vendor)) newest = Math.max(newest, instantOf(q.at));
    return newest;
}

/** The order the series of a board are listed in. */
export type Order = 'live' | 'name';

/** The series of a board a person asked for, in the order asked. */
export function shownSeries(
    board: Board,
    shown: ReadonlySet<string>,
    find: string,
    order: Order,
    now: number,
): Series[] {
    const text = find.trim().toLowerCase().replaceAll('/', '').replaceAll(' ', '');
    const matches = (s: Series): boolean =>
        text === '' ||
        `${s.name}${s.instrumentType}`
            .toLowerCase()
            .replaceAll('/', '')
            .replaceAll(' ', '')
            .includes(text);
    const listed = board.series
        .filter((s) => s.points.some((p) => [...p.quotes.keys()].some((v) => shown.has(v))))
        .filter(matches);
    const live = (s: Series): boolean => now - newestOf(s, shown) <= 60_000;
    return listed.sort(
        (a, b) =>
            (order === 'live' ? Number(live(b)) - Number(live(a)) : 0) ||
            a.name.localeCompare(b.name),
    );
}

/** How far apart two vendors may be before one is called an outlier, by kind. */
const LIMIT: Readonly<Record<Kind, { readonly absolute?: number; readonly relative?: number }>> = {
    spot: { relative: 0.005 },
    rate: { absolute: 0.0025 },
    vol: { absolute: 0.004 },
    plain: { relative: 0.01 },
};

export function isOutlier(kind: Kind, value: number, reference: number | null): boolean {
    if (reference === null) return false;
    const limit = LIMIT[kind];
    if (limit.absolute !== undefined) return Math.abs(value - reference) > limit.absolute;
    return (
        reference !== 0 && Math.abs(value - reference) / Math.abs(reference) > (limit.relative ?? 0)
    );
}

function median(xs: readonly number[]): number {
    const s = [...xs].sort((a, b) => a - b);
    const m = s.length >> 1;
    return s.length % 2 === 1 ? s[m]! : (s[m - 1]! + s[m]!) / 2;
}

/** A point's agreement: what each quote is measured against, the mid and the spread. */
export interface Agreement {
    /** The median of three or more live vendors, or else the highest-ranked live vendor's value. */
    readonly reference: number | null;
    /** The mean of the live vendors that are not outliers. */
    readonly mid: number | null;
    /** The widest gap between two live vendors that are not outliers. */
    readonly spread: number | null;
    /** The live vendors the mid is made of. */
    readonly count: number;
}

/**
 * The agreement of the live, shown vendors on a point. A vendor whose last quote
 * is stale is left out, as is one the person left out. With fewer than three, a
 * median says nothing about which vendor is wrong, so the providers' rank decides.
 */
export function agreementOf(
    kind: Kind,
    point: Point,
    vendors: readonly Vendor[],
    shown: ReadonlySet<string>,
    now: number,
    leaveOut?: string,
): Agreement {
    const live = vendors
        .filter((v) => v.key !== leaveOut && shown.has(v.key))
        .flatMap((v) => {
            const q = point.quotes.get(v.key);
            const value = q === undefined ? NaN : Number(q.value);
            return q !== undefined && !isStale(q.at, now) && Number.isFinite(value) ? [value] : [];
        });
    if (live.length === 0) return { reference: null, mid: null, spread: null, count: 0 };
    const reference = live.length >= 3 ? median(live) : live[0]!;
    const kept = live.filter((x) => !isOutlier(kind, x, reference));
    return {
        reference,
        mid: kept.reduce((a, b) => a + b, 0) / kept.length,
        spread: Math.max(...kept) - Math.min(...kept),
        count: kept.length,
    };
}

/** The change from a quote's previous value, or zero when either is not a number. */
export function changeOf(q: Quote): number {
    const value = Number(q.value);
    const previous = Number(q.previous);
    return Number.isFinite(value) && Number.isFinite(previous) ? value - previous : 0;
}

/** The decimals of an FX price: five for most pairs, fewer as the price grows. */
function spotDecimals(value: number): number {
    const size = Math.abs(value);
    return size >= 100 ? 3 : size >= 10 ? 4 : 5;
}

const PLAIN = new Intl.NumberFormat('en-GB', { maximumSignificantDigits: 7, useGrouping: false });

/** A value in its kind's unit: a pair's decimals, a rate in %, a vol in vol points. */
export function valueText(kind: Kind, value: number | null): string {
    if (value === null || !Number.isFinite(value)) return '—';
    if (kind === 'spot') return value.toFixed(spotDecimals(value));
    if (kind === 'rate') return `${(value * 100).toFixed(3)}%`;
    if (kind === 'vol') return (value * 100).toFixed(2);
    return PLAIN.format(value);
}

/** A difference in its kind's unit: pips, basis points or vol points. */
export interface Difference {
    readonly amount: number;
    readonly unit: 'pip' | 'bp' | 'vol' | '';
    readonly decimals: number;
}

/** A difference between two values of a series, at the size of @p at, in its kind's unit. */
export function differenceOf(kind: Kind, delta: number, at: number): Difference {
    if (kind === 'spot')
        return { amount: delta / 10 ** (1 - spotDecimals(at)), unit: 'pip', decimals: 1 };
    if (kind === 'rate') return { amount: delta * 10_000, unit: 'bp', decimals: 1 };
    if (kind === 'vol') return { amount: delta * 100, unit: 'vol', decimals: 2 };
    return { amount: delta, unit: '', decimals: 6 };
}

/** A difference as text, signed, and nothing but zero when it rounds to zero. */
export function differenceText(d: Difference): string {
    const shown = d.amount.toFixed(d.decimals);
    if (Number(shown) === 0) return '0';
    return `${d.amount > 0 ? '+' : ''}${d.unit === '' ? PLAIN.format(d.amount) : shown}`;
}

/** The consensus of a comparison: the mid of the vendors other than the one compared. */
export const CONSENSUS = '*';

/** One cell of a comparison: the two values and A − B. */
export interface Compared {
    readonly a: number | null;
    readonly b: number | null;
    readonly difference: number | null;
}

/** The value a source gives a point: a vendor's live quote, or the consensus without the other side. */
export function sourceValue(
    kind: Kind,
    point: Point,
    source: string,
    other: string,
    vendors: readonly Vendor[],
    shown: ReadonlySet<string>,
    now: number,
): number | null {
    if (source === CONSENSUS)
        return agreementOf(
            kind,
            point,
            vendors,
            shown,
            now,
            other === CONSENSUS ? undefined : other,
        ).mid;
    const q = point.quotes.get(source);
    const value = q === undefined ? NaN : Number(q.value);
    return Number.isFinite(value) ? value : null;
}

/** Two sources of a point, side by side. */
export function compare(
    kind: Kind,
    point: Point,
    a: string,
    b: string,
    vendors: readonly Vendor[],
    shown: ReadonlySet<string>,
    now: number,
): Compared {
    const va = sourceValue(kind, point, a, b, vendors, shown, now);
    const vb = sourceValue(kind, point, b, a, vendors, shown, now);
    return { a: va, b: vb, difference: va === null || vb === null ? null : va - vb };
}

/** A cell of a grid: the point, and the series it belongs to. */
export interface Cell {
    readonly series: Series;
    readonly point: Point;
}

/**
 * Points laid out as a grid, tenor down. A surface's columns are its second
 * coordinate; a currency's columns are its curves. Either way a grid is read the
 * same: as the mid, one vendor, the spread, or two sources compared.
 */
export interface Grid {
    readonly key: string;
    readonly name: string;
    readonly kind: Kind;
    readonly rows: readonly string[];
    readonly columns: readonly string[];
    readonly at: (row: string, column: string) => Cell | undefined;
    readonly series: readonly Series[];
}

/** A surface as a grid: its first coordinate down, its second across. */
export function gridOf(s: Series): Grid {
    const rows = [...new Set(s.points.map((p) => p.coordinates[0] ?? ''))].sort(byTenor);
    const columns = [...new Set(s.points.map((p) => p.coordinates[1] ?? ''))].sort(byColumn);
    const index = new Map(
        s.points.map((p) => [`${p.coordinates[0]}|${p.coordinates[1]}`, { series: s, point: p }]),
    );
    return {
        key: s.key,
        name: s.name,
        kind: s.kind,
        rows,
        columns,
        at: (row, column) => index.get(`${row}|${column}`),
        series: [s],
    };
}

/** The currency a curve is of: the first word of its name, which is its subject. */
export function currencyOf(s: Series): string {
    return s.name.split(' ')[0] ?? s.name;
}

/** The curves of a board as one grid for each currency: tenor down, each curve across. */
export function currencyGrids(curves: readonly Series[]): Grid[] {
    const by = new Map<string, Series[]>();
    for (const s of curves) by.set(currencyOf(s), [...(by.get(currencyOf(s)) ?? []), s]);
    return [...by]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([currency, list]) => {
            const sorted = [...list].sort((a, b) =>
                a.name.localeCompare(b.name, undefined, { numeric: true }),
            );
            const rows = [
                ...new Set(sorted.flatMap((s) => s.points.map((p) => p.coordinates[0] ?? ''))),
            ].sort(byTenor);
            const index = new Map(
                sorted.flatMap((s) =>
                    s.points.map(
                        (p) => [`${p.coordinates[0]}|${s.name}`, { series: s, point: p }] as const,
                    ),
                ),
            );
            return {
                key: currency,
                name: currency,
                kind: sorted[0]?.kind ?? 'plain',
                rows,
                columns: sorted.map((s) => s.name),
                at: (row: string, column: string) => index.get(`${row}|${column}`),
                series: sorted,
            };
        });
}

/** One tick of a vendor's history: when, and the value. */
export interface Tick {
    readonly at: number;
    readonly value: number;
}

/**
 * Each vendor's ticks on one time axis, so vendors that tick at different rates
 * line up: @p slots instants from @p from to @p to, each holding a vendor's last
 * value at or before it, and a gap before its first tick.
 */
export function onTimeAxis(
    ticks: readonly (readonly Tick[])[],
    from: number,
    to: number,
    slots: number,
): { readonly xs: number[]; readonly lines: (number | null)[][] } {
    const xs = Array.from({ length: slots }, (_, i) => from + ((to - from) * i) / (slots - 1));
    const lines = ticks.map((one) => {
        const sorted = [...one].sort((a, b) => a.at - b.at);
        let next = 0;
        let last: number | null = null;
        return xs.map((x) => {
            while (next < sorted.length && sorted[next]!.at <= x) last = sorted[next++]!.value;
            return last;
        });
    });
    return { xs, lines };
}

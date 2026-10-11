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

/** The key of one record: each column of the entity's declared key, as text. */
export type RecordKey = Readonly<Record<string, string>>;

/**
 * Whether a change may concern a record.
 *
 * A change that names no records, because there were many or the event could not
 * name its own, may concern any record. That is the safe direction: a screen
 * that is told "something changed" and finds nothing new has lost one click, and
 * a screen that is told nothing has lost the news.
 *
 * A change that names records concerns a record only when one of them has every
 * column the record's key states.
 */
export function mayConcern(keys: readonly RecordKey[] | undefined, record: RecordKey): boolean {
    if (keys === undefined) return true;
    return keys.some((key) =>
        Object.entries(record).every(([column, value]) => key[column] === value),
    );
}

/** The most records a screen keeps by name; past it, it only knows that something changed. */
const MAX_HEARD = 100;

/**
 * What a screen has heard since it last loaded: the records the changes named,
 * or that it cannot tell which, because a change named none.
 */
export type Heard = { readonly keys: readonly RecordKey[] } | 'unknown';

/** Nothing heard yet. */
export const NOTHING_HEARD: Heard = { keys: [] };

const sameKey = (a: RecordKey, b: RecordKey): boolean => {
    const columns = Object.keys(a);
    return columns.length === Object.keys(b).length && columns.every((c) => a[c] === b[c]);
};

/** Adds the records one change named to what was heard. A change that named none leaves it unknown. */
export function hear(before: Heard, keys: readonly RecordKey[] | undefined): Heard {
    if (before === 'unknown' || keys === undefined) return 'unknown';
    const merged = [...before.keys];
    for (const key of keys) {
        if (!merged.some((known) => sameKey(known, key))) merged.push(key);
    }
    return merged.length > MAX_HEARD ? 'unknown' : { keys: merged };
}

/** Whether a key names a record: it has every column the record's key states. */
const names = (key: RecordKey, record: RecordKey): boolean =>
    Object.entries(record).every(([column, value]) => key[column] === value);

/**
 * Where what was heard falls on the page on screen.
 *
 * `rows` are the positions of the rows a change named. `elsewhere` is true when
 * something changed that the page cannot point at: a change named a record that
 * is not on the page, the changes were too many to name, or the list cannot say
 * which record a row is.
 */
export function whereChanged<Row>(
    heard: Heard,
    rows: readonly Row[],
    recordOf: ((row: Row) => RecordKey) | undefined,
): { readonly rows: ReadonlySet<number>; readonly elsewhere: boolean } {
    if (heard === 'unknown') return { rows: new Set(), elsewhere: true };
    if (heard.keys.length === 0) return { rows: new Set(), elsewhere: false };
    if (recordOf === undefined) return { rows: new Set(), elsewhere: true };
    const records = rows.map(recordOf);
    const named = new Set<number>();
    records.forEach((record, index) => {
        if (heard.keys.some((key) => names(key, record))) named.add(index);
    });
    const elsewhere = heard.keys.some((key) => !records.some((record) => names(key, record)));
    return { rows: named, elsewhere };
}

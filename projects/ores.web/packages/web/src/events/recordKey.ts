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

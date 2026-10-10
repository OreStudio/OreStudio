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

import { useEffect, useRef, useState } from 'react';

/** How long a changed row stays marked: three pulses and a moment to be seen. */
const MARK_MS = 4000;

/**
 * The rows whose content differs from the same page read before.
 *
 * A row is changed when its key was not there or its content is not what it was.
 * Nothing is compared with a clock: the server's time and the browser's differ,
 * and a comparison across them marks rows that did not change. A page read for
 * the first time has nothing to be read against and marks nothing.
 */
export function changedKeys(
    before: ReadonlyMap<string, string> | undefined,
    after: ReadonlyMap<string, string>,
): ReadonlySet<string> {
    const changed = new Set<string>();
    if (before === undefined) return changed;
    for (const [key, print] of after) {
        if (before.get(key) !== print) changed.add(key);
    }
    return changed;
}

/**
 * Marks the rows that changed when a page is read again.
 *
 * It looks at a page only when its data arrives and the page is the one it saw
 * last, so moving to another page, searching or sorting marks nothing. The mark
 * goes after a few seconds; a list that never reloads by itself never marks
 * anything, because the reload is the reader's.
 */
export function useChangedRows<Row>(
    page: string,
    rows: readonly Row[],
    keyOf: (row: Row, index: number) => string,
    settled: { readonly dataUpdatedAt: number; readonly isPlaceholder: boolean },
): ReadonlySet<string> {
    const seen = useRef<{ page: string; prints: ReadonlyMap<string, string> } | undefined>(
        undefined,
    );
    const [marked, setMarked] = useState<ReadonlySet<string>>(new Set());
    const latest = useRef({ rows, keyOf });
    latest.current = { rows, keyOf };

    useEffect(() => {
        if (settled.isPlaceholder || settled.dataUpdatedAt === 0) return undefined;
        const prints = new Map(
            latest.current.rows.map((row, index) => [
                latest.current.keyOf(row, index),
                JSON.stringify(row),
            ]),
        );
        const before = seen.current?.page === page ? seen.current.prints : undefined;
        seen.current = { page, prints };
        const changed = changedKeys(before, prints);
        if (changed.size === 0) return undefined;
        setMarked(changed);
        const timer = window.setTimeout(() => {
            setMarked(new Set());
        }, MARK_MS);
        return () => window.clearTimeout(timer);
    }, [page, settled.dataUpdatedAt, settled.isPlaceholder]);

    return marked;
}

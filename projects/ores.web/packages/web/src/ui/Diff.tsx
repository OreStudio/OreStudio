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

import type { ReactNode } from 'react';

/**
 * What changed inside one value, marked rather than restated.
 *
 * A record's history and a request's story both answer "what changed", and both
 * should answer it the same way: the value as it was, in red, and the value as
 * it is, in green, with only the characters that moved picked out. The widget
 * lives here so that the record screens and the story screen cannot drift into
 * two ways of drawing one change.
 */

/**
 * A changed value split into its common prefix, its changed middle and its
 * common suffix, so the middle can be marked on the old line and on the new.
 */
export function splitValue(
    before: string,
    after: string,
): {
    readonly old: readonly [string, string, string];
    readonly new: readonly [string, string, string];
} {
    let prefix = 0;
    while (prefix < before.length && prefix < after.length && before[prefix] === after[prefix]) {
        prefix += 1;
    }
    let suffix = 0;
    while (
        suffix < before.length - prefix &&
        suffix < after.length - prefix &&
        before[before.length - 1 - suffix] === after[after.length - 1 - suffix]
    ) {
        suffix += 1;
    }
    const parts = (text: string): readonly [string, string, string] => [
        text.slice(0, prefix),
        text.slice(prefix, text.length - suffix),
        text.slice(text.length - suffix),
    ];
    return { old: parts(before), new: parts(after) };
}

/** One line of a difference: the sign, the value, and what moved inside it. */
function DiffLine({
    sign,
    parts,
    tone,
}: {
    readonly sign: string;
    readonly parts: readonly [string, string, string];
    readonly tone: 'old' | 'new';
}): ReactNode {
    const line = tone === 'old' ? 'bg-down/15' : 'bg-up/15';
    const mark = tone === 'old' ? 'bg-down/45' : 'bg-up/45';
    return (
        <div className={`grid grid-cols-[1.25rem_1fr] rounded px-2 py-0.5 ${line}`}>
            <span aria-hidden className="text-ink-faint select-none">
                {sign}
            </span>
            <span>
                {parts[0]}
                {parts[1] !== '' && (
                    <mark className={`rounded-sm text-inherit ${mark}`}>{parts[1]}</mark>
                )}
                {parts[2]}
            </span>
        </div>
    );
}

/**
 * A value as it was and as it is.
 *
 * A value that did not change is the caller's to draw as plain text: this
 * widget answers a change, and showing both lines for a value that stayed the
 * same would say something happened where nothing did.
 */
export function DiffLines({
    before,
    after,
}: {
    readonly before: string;
    readonly after: string;
}): ReactNode {
    const parts = splitValue(before, after);
    return (
        <div className="grid gap-px">
            <DiffLine sign="−" parts={parts.old} tone="old" />
            <DiffLine sign="+" parts={parts.new} tone="new" />
        </div>
    );
}

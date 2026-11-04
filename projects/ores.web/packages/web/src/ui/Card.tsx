/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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

import type { ReactNode } from 'react';

/** A titled card, as every block of a feed or calibration page is drawn. */
export function Card({
    title,
    aside,
    children,
}: {
    readonly title: string;
    readonly aside?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="card space-y-3 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="font-medium">{title}</h2>
                {aside}
            </header>
            {children}
        </section>
    );
}

/** One figure: a small caption, the value, and a line beneath it. */
export function MeasureTile({
    label,
    value,
    sub,
}: {
    readonly label: string;
    readonly value: string;
    readonly sub?: string;
}): ReactNode {
    return (
        <div className="rounded-chip border border-line-subtle bg-surface-base px-2.5 py-2">
            <span className="block font-mono text-[0.65rem] font-semibold uppercase tracking-wide text-ink-faint">
                {label}
            </span>
            <span className="font-mono font-semibold tabular-nums">{value}</span>
            {sub !== undefined && <span className="block text-xs text-ink-faint">{sub}</span>}
        </div>
    );
}

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

import type { ReactNode } from 'react';
import { Link } from 'react-router';

/**
 * One figure in a box: the number large, its label small beneath.
 *
 * The dashboard states its facts as tiles, so each reads as a separate fact
 * instead of one run of text. A tile with an address leads to the screen that
 * explains the figure.
 */
export type MetricTone = 'good' | 'warn' | 'bad' | 'neutral';

const TONE_CLASS: Readonly<Record<MetricTone, string>> = {
    good: 'text-up',
    warn: 'text-warn',
    bad: 'text-down',
    neutral: 'text-ink',
};

export function Metric({
    value,
    label,
    tone = 'neutral',
    to,
    note,
}: {
    readonly value: string;
    readonly label: string;
    readonly tone?: MetricTone;
    readonly to?: string;
    readonly note?: string;
}): ReactNode {
    const body = (
        <>
            <span className={`text-2xl font-semibold tabular-nums ${TONE_CLASS[tone]}`}>
                {value}
            </span>
            <span className="text-xs text-ink-muted">{label}</span>
            {note !== undefined && <span className="text-xs text-warn">{note}</span>}
        </>
    );
    const box = 'grid content-start gap-1 rounded-lg border border-line-subtle bg-surface-base p-3';

    return to === undefined ? (
        <div className={box}>{body}</div>
    ) : (
        <Link to={to} className={`${box} transition-colors hover:border-accent-line`}>
            {body}
        </Link>
    );
}

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

/*
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * The parts the five operations screens share: the strip that moves between
 * them, and the panel that records what each screen cannot show yet.
 */

import type { ReactNode } from 'react';
import { cx } from '../ui/Primitives.js';

interface ScreenLink {
    readonly path: string;
    readonly name: string;
}

const SCREENS: readonly ScreenLink[] = [
    { path: '/prototype', name: 'Index' },
    { path: '/prototype/services', name: 'Services' },
    { path: '/prototype/grid', name: 'Compute grid' },
    { path: '/prototype/bus', name: 'Message bus' },
    { path: '/prototype/logs', name: 'Telemetry logs' },
    { path: '/prototype/versions', name: 'Versions' },
];

export function OperationsNav({ pathname }: { readonly pathname: string }): ReactNode {
    return (
        <nav className="flex flex-wrap gap-1 border-b border-line pb-2">
            {SCREENS.map((screen) => (
                <a
                    key={screen.path}
                    href={screen.path}
                    className={cx(
                        'rounded px-3 py-1 text-sm',
                        screen.path === pathname
                            ? 'bg-surface-overlay text-ink'
                            : 'text-ink-muted hover:text-ink',
                    )}
                >
                    {screen.name}
                </a>
            ))}
        </nav>
    );
}

export interface ScreenGap {
    readonly title: string;
    readonly body: string;
}

export function GapPanel({ gaps }: { readonly gaps: readonly ScreenGap[] }): ReactNode {
    return (
        <section className="card space-y-3 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Not on this screen yet</h2>
                <span className="text-xs text-ink-faint">
                    each gap names the journey that records it
                </span>
            </header>
            <dl className="space-y-3 text-sm">
                {gaps.map((gap) => (
                    <div key={gap.title} className="space-y-1">
                        <dt className="font-medium text-ink">{gap.title}</dt>
                        <dd className="text-ink-muted">{gap.body}</dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

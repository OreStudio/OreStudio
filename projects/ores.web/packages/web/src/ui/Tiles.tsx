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
import { Link } from 'react-router';
import { Icon, type IconName } from './Icon.js';

/**
 * A grid of tiles, each a place a person can go.
 *
 * The shape is the landing-page review's: a glyph in a tinted square, the name,
 * a line about it, and the rule across the foot. A tile with no address is a
 * place that is not built yet, so it wears the future hue and its foot carries
 * the words that say so.
 */
export interface Tile {
    readonly title: string;
    readonly body: string;
    readonly to?: string;
    /**
     * The glyph the place is known by, from the icon reference. A tile without
     * one is a tile nobody has given a face yet, which is a gap and not a
     * meaning.
     */
    readonly icon?: IconName;
}

/**
 * The mark a tile leads with.
 *
 * A place that can be gone to wears the accent, and the square fills with it
 * when the pointer arrives. A place that cannot yet wears the future hue, so
 * the grid says which is which before a word of it is read.
 */
function Mark({ name, future }: { readonly name: IconName; readonly future: boolean }): ReactNode {
    return (
        <span
            className={
                future
                    ? 'flex h-9 w-9 items-center justify-center rounded-chip border border-roadmap-line bg-roadmap-dim text-roadmap'
                    : 'flex h-9 w-9 items-center justify-center rounded-chip border border-accent-line bg-accent-dim text-accent transition-colors group-hover:bg-accent group-hover:text-ink-inverse'
            }
        >
            <Icon name={name} />
        </span>
    );
}

export function Tiles({
    tiles,
    later,
}: {
    readonly tiles: readonly Tile[];
    readonly later?: string;
}): ReactNode {
    return (
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
            {tiles.map((tile) =>
                tile.to === undefined ? (
                    <div key={tile.title} className="card flex flex-col p-4">
                        <div className="flex items-start">
                            {tile.icon !== undefined && <Mark name={tile.icon} future />}
                        </div>
                        <span className="mt-3 text-sm font-semibold text-ink">{tile.title}</span>
                        <span className="mt-1 text-sm text-ink-muted">{tile.body}</span>
                        {later !== undefined && (
                            <div className="mt-4 border-t border-line pt-3">
                                <span className="text-micro tracking-label text-roadmap uppercase">
                                    {later}
                                </span>
                            </div>
                        )}
                    </div>
                ) : (
                    <Link
                        key={tile.title}
                        to={tile.to}
                        className="card group flex flex-col p-4 transition-[border-color,box-shadow] hover:border-accent-line hover:shadow-glow focus-visible:outline-accent"
                    >
                        <div className="flex items-start">
                            {tile.icon !== undefined && <Mark name={tile.icon} future={false} />}
                        </div>
                        <span className="mt-3 text-sm font-semibold text-ink transition-colors group-hover:text-accent-bright">
                            {tile.title}
                        </span>
                        <span className="mt-1 text-sm text-ink-muted">{tile.body}</span>
                    </Link>
                ),
            )}
        </div>
    );
}

/**
 * A labelled run of tiles, under the rule the review drew.
 *
 * The heading is the review's: a micro-label, a glyph, and a rule beneath it.
 * The rule is what separates one run from the next, so a screen with two of
 * them reads as two and not as one grid with a stray line through it.
 */
export function TilesSection({
    title,
    icon,
    children,
}: {
    readonly title: string;
    readonly icon?: IconName;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section>
            <h2 className="mb-3 flex items-center gap-2 border-b border-line pb-2 text-micro font-semibold tracking-label text-ink-muted uppercase">
                {icon !== undefined && <Icon name={icon} size={16} />}
                {title}
            </h2>
            {children}
        </section>
    );
}

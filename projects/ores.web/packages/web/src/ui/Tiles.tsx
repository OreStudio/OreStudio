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

/**
 * A grid of tiles, each a place a person can go. A tile with no address is a
 * place that is not built yet; it is drawn dimmed, with the words that say so.
 */
export interface Tile {
    readonly title: string;
    readonly body: string;
    readonly to?: string;
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
                    <div
                        key={tile.title}
                        className="card grid content-start gap-1.5 p-4 opacity-70"
                    >
                        <span className="text-sm font-semibold text-ink">{tile.title}</span>
                        <span className="text-sm text-ink-muted">{tile.body}</span>
                        {later !== undefined && (
                            <span className="mt-1 justify-self-start rounded-full border border-line px-2 py-0.5 text-[11px] text-ink-faint">
                                {later}
                            </span>
                        )}
                    </div>
                ) : (
                    <Link
                        key={tile.title}
                        to={tile.to}
                        className="card grid content-start gap-1.5 p-4 hover:border-line-strong focus-visible:outline-accent"
                    >
                        <span className="text-sm font-semibold text-ink">{tile.title}</span>
                        <span className="text-sm text-ink-muted">{tile.body}</span>
                    </Link>
                ),
            )}
        </div>
    );
}

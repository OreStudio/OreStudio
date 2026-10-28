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
import type { FeedRaster } from '../../../api/client.js';
import { cx } from '../../../ui/Primitives.js';
import { feedPageOf } from '../feedTree.js';

/** The page of every feed the feed list knows, by source name. */
export type FeedPages = ReadonlyMap<string, string>;

export function feedPages(raster: FeedRaster | undefined): FeedPages {
    return new Map(
        (raster?.feeds ?? [])
            .filter((feed) => feed.id !== '')
            .map((feed) => [feed.sourceName, feedPageOf(feed)] as const),
    );
}

/**
 * A feed's name that opens the feed's page, or its name alone when the feed list does not know
 * it yet, such as a feed that has just been added.
 */
export function FeedLink({
    sourceName,
    pages,
    tab,
    title,
    children,
    className,
}: {
    readonly sourceName: string;
    readonly pages: FeedPages;
    /** The tab of the feed's page to open, such as `arrivals`. */
    readonly tab?: string;
    /** What a reader sees on hover, such as the numbers behind a law. */
    readonly title?: string;
    readonly children?: ReactNode;
    readonly className?: string;
}): ReactNode {
    const page = pages.get(sourceName);
    const name = children ?? sourceName;
    if (page === undefined) {
        return (
            <span className={className} title={title}>
                {name}
            </span>
        );
    }
    return (
        <Link
            to={tab === undefined ? page : `${page}?tab=${tab}`}
            title={title}
            onClick={(event) => event.stopPropagation()}
            className={cx(
                'underline decoration-ink-faint decoration-dotted underline-offset-2 hover:text-accent hover:decoration-solid',
                className,
            )}
        >
            {name}
        </Link>
    );
}

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
import { useTranslation } from '../i18n/Provider.js';
import { Button } from './Primitives.js';

/**
 * Which rows a list shows, and the way to the pages either side.
 *
 * The total is the server's, so it counts every row that matches, not the
 * rows on this page. The sentence that says so is the caller's, because it
 * names what the list holds.
 */
export function Pager({
    offset,
    shown,
    total,
    pageSize,
    showing,
    onMove,
}: {
    readonly offset: number;
    readonly shown: number;
    readonly total: number;
    readonly pageSize: number;
    readonly showing: string;
    readonly onMove: (offset: number) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="mt-4 flex flex-wrap items-center justify-between gap-3 text-sm text-ink-muted">
            <span>{showing}</span>
            <span className="flex gap-2">
                <Button
                    size="sm"
                    disabled={offset === 0}
                    onClick={() => onMove(Math.max(0, offset - pageSize))}
                >
                    {t('entity.previous')}
                </Button>
                <Button
                    size="sm"
                    disabled={offset + shown >= total}
                    onClick={() => onMove(offset + pageSize)}
                >
                    {t('entity.next')}
                </Button>
            </span>
        </div>
    );
}

/** The first and last row numbers a page shows, counting from one. */
export function pageBounds(offset: number, shown: number): { first: number; last: number } {
    return { first: shown === 0 ? 0 : offset + 1, last: offset + shown };
}

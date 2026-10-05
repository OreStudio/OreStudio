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
import { Button, Select } from './Primitives.js';

/** The page sizes a list offers, as the record screen standard sets them. */
export const PAGE_SIZES = [15, 25, 50, 100, 200, 500] as const;

/** The page size every paged list starts with: about one screen of rows. */
export const DEFAULT_PAGE_SIZE = 15;

/** Load all is offered only up to this total, so it never reads an unbounded list. */
export const LOAD_ALL_LIMIT = 1000;

/**
 * Which rows a list shows, and the way to the other pages.
 *
 * The total is the server's, so it counts every row that matches, not the
 * rows on this page. The sentence that says so is the caller's, because it
 * names what the list holds. A list that passes `onPageSize` also offers the
 * page sizes, and Load all when the total is small enough to read whole.
 */
export function Pager({
    offset,
    shown,
    total,
    pageSize,
    showing,
    onMove,
    onPageSize,
}: {
    readonly offset: number;
    readonly shown: number;
    readonly total: number;
    readonly pageSize: number;
    readonly showing: string;
    readonly onMove: (offset: number) => void;
    readonly onPageSize?: (pageSize: number) => void;
}): ReactNode {
    const { t } = useTranslation();
    const lastOffset = Math.max(0, Math.floor((total - 1) / pageSize) * pageSize);
    const atStart = offset === 0;
    const atEnd = offset + shown >= total;
    return (
        <div className="mt-4 flex flex-wrap items-center justify-between gap-3 text-sm text-ink-muted">
            <span>{showing}</span>
            <span className="flex flex-wrap items-center gap-2">
                {onPageSize !== undefined && (
                    <Button size="sm" disabled={atStart} onClick={() => onMove(0)}>
                        {t('entity.first')}
                    </Button>
                )}
                <Button
                    size="sm"
                    disabled={atStart}
                    onClick={() => onMove(Math.max(0, offset - pageSize))}
                >
                    {t('entity.previous')}
                </Button>
                <Button size="sm" disabled={atEnd} onClick={() => onMove(offset + pageSize)}>
                    {t('entity.next')}
                </Button>
                {onPageSize !== undefined && (
                    <>
                        <Button size="sm" disabled={atEnd} onClick={() => onMove(lastOffset)}>
                            {t('entity.last')}
                        </Button>
                        <label className="flex items-center gap-2 whitespace-nowrap">
                            <span>{t('entity.pageSize')}</span>
                            <Select
                                className="w-24"
                                value={String(pageSize)}
                                onChange={(event) => onPageSize(Number(event.target.value))}
                            >
                                {(PAGE_SIZES as readonly number[]).includes(pageSize) ? null : (
                                    <option value={String(pageSize)}>{pageSize}</option>
                                )}
                                {PAGE_SIZES.map((size) => (
                                    <option key={size} value={String(size)}>
                                        {size}
                                    </option>
                                ))}
                            </Select>
                        </label>
                        {total <= LOAD_ALL_LIMIT && total > pageSize && (
                            <Button size="sm" variant="ghost" onClick={() => onPageSize(total)}>
                                {t('entity.loadAll')}
                            </Button>
                        )}
                    </>
                )}
            </span>
        </div>
    );
}

/** The first and last row numbers a page shows, counting from one. */
export function pageBounds(offset: number, shown: number): { first: number; last: number } {
    return { first: shown === 0 ? 0 : offset + 1, last: offset + shown };
}

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
import { useSearchParams } from 'react-router';

/**
 * The address parameters of a tab: none for the first, which is the page's
 * own address, and only `tab` for the others. Every other parameter belongs
 * to the tab left, so it is dropped.
 */
export function tabAddress(tabs: readonly string[], tab: string): Readonly<Record<string, string>> {
    return tab === tabs[0] ? {} : { tab };
}

/**
 * The tabs of a page, held in the address so a link reopens the same tab. The
 * first tab is the page's own address; the others add `?tab=`. A tab change
 * drops the other parameters, which belong to the tab left, such as a list's
 * page. A tab change replaces the history entry, so Back leaves the
 * page rather than walking back through its tabs. Every page with tabs draws
 * them through this, so they look and behave the same everywhere.
 */
export function useTabs({
    label,
    tabs,
    titleOf,
}: {
    readonly label: string;
    readonly tabs: readonly string[];
    readonly titleOf: (tab: string) => string;
}): { readonly tab: string; readonly bar: ReactNode } {
    const [search, setSearch] = useSearchParams();
    const requested = search.get('tab');
    const tab = tabs.find((candidate) => candidate === requested) ?? tabs[0] ?? '';
    const bar = (
        <div role="tablist" aria-label={label} className="flex gap-1 border-b border-line">
            {tabs.map((candidate) => (
                <button
                    key={candidate}
                    type="button"
                    role="tab"
                    aria-selected={tab === candidate}
                    className={
                        tab === candidate
                            ? 'border-b-2 border-accent px-3 py-2 text-sm text-ink'
                            : 'border-b-2 border-transparent px-3 py-2 text-sm text-ink-muted hover:text-ink'
                    }
                    onClick={() => setSearch(tabAddress(tabs, candidate), { replace: true })}
                >
                    {titleOf(candidate)}
                </button>
            ))}
        </div>
    );
    return { tab, bar };
}

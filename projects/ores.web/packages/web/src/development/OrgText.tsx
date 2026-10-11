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

import { useMemo, type MouseEvent, type ReactNode } from 'react';
import { renderOrgHtml } from '@ores/org/render';

/**
 * Org text drawn as HTML.
 *
 * The renderer shows raw HTML in the source as text and removes any link or
 * attribute that could run script, so its output is safe to place in the page.
 * The classes give the plain markup the look of the rest of the screen.
 *
 * An id link in the text points at a route of the app. A click on one goes to
 * `onDocLink` with the id, so the app navigates without a page load. Any other
 * link behaves as the browser makes it. Pass `docHref` with `onDocLink`: the
 * handler gives the click, and `docHref` gives the link an address, so a
 * modified click and a copied link still reach the doc.
 */
export function OrgText({
    source,
    docHref,
    onDocLink,
}: {
    readonly source: string;
    /** The route of a doc, by its id. Without it an id link points at a fragment. */
    readonly docHref?: (id: string) => string;
    readonly onDocLink?: (id: string) => void;
}): ReactNode {
    const html = useMemo(
        () => renderOrgHtml(source, docHref === undefined ? {} : { idHref: docHref }),
        [source, docHref],
    );
    const follow = (event: MouseEvent<HTMLDivElement>): void => {
        if (onDocLink === undefined || event.defaultPrevented) return;
        if (event.metaKey || event.ctrlKey || event.shiftKey || event.button !== 0) return;
        const link = (event.target as HTMLElement).closest('a[data-org-id]');
        const id = link?.getAttribute('data-org-id');
        if (id === null || id === undefined) return;
        event.preventDefault();
        onDocLink(id);
    };
    return (
        <div
            className={
                'space-y-2 text-sm text-ink [&_a]:text-accent [&_code]:rounded [&_code]:bg-surface-overlay ' +
                '[&_code]:px-1 [&_code]:font-mono [&_code]:text-xs [&_h1]:text-lg [&_h1]:font-semibold ' +
                '[&_h2]:text-base [&_h2]:font-semibold [&_h3]:font-medium [&_li]:ml-5 [&_ol]:list-decimal ' +
                '[&_pre]:overflow-x-auto [&_pre]:rounded-md [&_pre]:bg-surface-overlay [&_pre]:p-3 ' +
                '[&_pre]:font-mono [&_pre]:text-xs [&_table]:w-full [&_td]:border [&_td]:border-line ' +
                '[&_td]:px-2 [&_td]:py-1 [&_th]:border [&_th]:border-line [&_th]:px-2 [&_th]:py-1 ' +
                '[&_ul]:list-disc'
            }
            onClick={follow}
            dangerouslySetInnerHTML={{ __html: html }}
        />
    );
}

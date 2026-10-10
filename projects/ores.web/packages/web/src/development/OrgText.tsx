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

import { useMemo, type ReactNode } from 'react';
import { renderOrgHtml } from '@ores/org/render';

/**
 * Org text drawn as HTML.
 *
 * The renderer shows raw HTML in the source as text and removes any link or
 * attribute that could run script, so its output is safe to place in the page.
 * The classes give the plain markup the look of the rest of the screen.
 */
export function OrgText({ lines }: { readonly lines: readonly string[] }): ReactNode {
    const html = useMemo(() => renderOrgHtml(lines.join('\n')), [lines]);
    return (
        <div
            className={
                'space-y-2 text-sm text-ink [&_a]:text-accent [&_code]:rounded [&_code]:bg-surface-overlay ' +
                '[&_code]:px-1 [&_code]:font-mono [&_code]:text-xs [&_li]:ml-5 [&_ol]:list-decimal ' +
                '[&_pre]:overflow-x-auto [&_pre]:rounded-md [&_pre]:bg-surface-overlay [&_pre]:p-3 ' +
                '[&_pre]:font-mono [&_pre]:text-xs [&_ul]:list-disc'
            }
            dangerouslySetInnerHTML={{ __html: html }}
        />
    );
}

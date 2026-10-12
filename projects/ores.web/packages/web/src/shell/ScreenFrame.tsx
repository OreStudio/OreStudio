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
import { PageHeader } from '../ui/Primitives.js';

/**
 * A screen that is waiting, or that failed to read, in its own frame.
 *
 * The breadcrumb and the header stay while the content loads or fails, so a
 * person always sees where they are and can go back. The entity screen
 * specification puts "Loading…" in the place of the content, and not in place
 * of the screen.
 */
export function ScreenFrame({
    trail,
    title,
    description,
    children,
}: {
    /** The breadcrumb the screen draws once it has loaded. */
    readonly trail: ReactNode;
    readonly title: string;
    readonly description: string;
    /** What stands in place of the content: the loading line or the error. */
    readonly children: ReactNode;
}): ReactNode {
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader title={title} description={description} />
            </div>
            {children}
        </div>
    );
}

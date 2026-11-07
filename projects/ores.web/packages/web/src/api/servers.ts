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

/**
 * Which server answers a screen while the port is in progress.
 *
 * The browser talks to one origin and asks for relative paths. The dev server, and
 * the proxy in front of a deployment, sends =/api= to the BFF and =/cpp= to the C++
 * server, so moving a screen is a prefix change and not a second origin. An empty
 * prefix is the BFF: a screen that has not moved asks for the path it always did.
 *
 * The screen is named by the module that calls the API, not by its page title, so a
 * module shared by two screens is either moved for both or for neither.
 */

/** The prefixes a screen's requests can go through, in this deployment. */
export type serverPrefix = '' | '/cpp';

/** Where one screen's requests go. */
export function baseForScreen(screen: string, moved: readonly string[]): serverPrefix {
    return moved.includes(screen) ? '/cpp' : '';
}

/**
 * The screens this build has moved, read from =VITE_ORE_CPP_SCREENS=.
 *
 * A comma-separated list, so the flag is one variable whatever the number of screens
 * on the new server. Empty means none, which is the deployment the browser has today.
 */
export const screensOnCpp: readonly string[] = (
    (import.meta.env['VITE_ORE_CPP_SCREENS'] as string | undefined) ?? ''
)
    .split(',')
    .map((name) => name.trim())
    .filter((name) => name.length > 0);

/** The prefix for a screen, from the flag this build was made with. */
export function baseFor(screen: string): serverPrefix {
    return baseForScreen(screen, screensOnCpp);
}

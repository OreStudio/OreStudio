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

import { originOf, type page } from '../api/servers.js';

/**
 * Where the API reader is, from the screen that offers it.
 *
 * The reader is a page of the server that composes the API, so it is named by that
 * server's origin and not by a path relative to this one: the BFF serves this bundle
 * today and is being retired, and asking it for a reader would ask a server that has
 * none. It is a page of its own, so the palette it is read in travels in the query
 * rather than being inherited from the screen that opened it.
 */
export function apiReaderUrl(path: string, at: page | undefined, theme: string): string {
    const base = at === undefined ? path : `${originOf('cpp', __ORE_SERVER_PORTS__, at)}${path}`;
    return `${base}?theme=${encodeURIComponent(theme)}`;
}

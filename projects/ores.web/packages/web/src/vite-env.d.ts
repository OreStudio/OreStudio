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

/// <reference types="vite/client" />

/** SVG imports resolve to their URL. */
declare module '*.svg' {
    const url: string;
    export default url;
}

/** PNG imports resolve to their URL. */
declare module '*.png' {
    const url: string;
    export default url;
}

/**
 * The build this bundle came from, stamped in by `vite.config.ts` at build
 * time: the release and the commit, not a value the server could answer with.
 *
 * The versions screen states the three parts of the stamp on their own, so
 * they travel beside the single line the footer shows.
 */
declare const __BUILD_VERSION__: string;
declare const __BUILD_RELEASE__: string;
declare const __BUILD_COMMIT__: string;
declare const __BUILD_DIRTY__: boolean;

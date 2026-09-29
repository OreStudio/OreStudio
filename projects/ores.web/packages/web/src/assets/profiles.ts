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
 * The artwork a starting point carries.
 *
 * A file is named after the profile's own code, so the screen asks for the
 * artwork of the profile it is drawing rather than deciding which profile
 * deserves one: a profile with no file here is drawn without a logo, and a
 * profile that wants one is a file dropped in beside these.
 *
 * The deployment holds the same artwork as an image record, which is where a
 * logo belongs once a profile can name one; the bundle carries a copy so the
 * cards need no read and no session to draw themselves.
 */

const FILES = import.meta.glob<string>('./profiles/*.png', {
    eager: true,
    query: '?url',
    import: 'default',
});

const BY_CODE = new Map<string, string>(
    Object.entries(FILES).map(([path, url]) => [
        path.slice(path.lastIndexOf('/') + 1, path.lastIndexOf('.')),
        url,
    ]),
);

/** The artwork for a starting point, or nothing when it has none. */
export function profileLogo(code: string): string | undefined {
    return BY_CODE.get(code);
}

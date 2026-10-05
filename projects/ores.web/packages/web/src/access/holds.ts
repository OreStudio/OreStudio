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

import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';

/**
 * Whether the signed-in person holds a permission, from the roles they hold.
 * A screen asks this only to decide what it offers; the server checks every
 * call again. Everything (`*`) and an area's wildcard (`iam::*`) count.
 */
export function useHolds(): (code: string) => boolean {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const codes = new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes));
    return (code) =>
        codes.has('*') || codes.has(`${code.split('::')[0] ?? ''}::*`) || codes.has(code);
}

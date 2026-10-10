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
import { meets, type Needs, type Permission } from './permissions.js';

/** Whether a set of permission codes holds one, counting everything and an area's wildcard. */
export function holdsFrom(codes: ReadonlySet<string>): (code: string) => boolean {
    return (code) =>
        codes.has('*') || codes.has(`${code.split('::')[0] ?? ''}::*`) || codes.has(code);
}

/**
 * What the signed-in person may do, and whether that is known yet.
 *
 * Until the roles have been read nothing is held, so a screen that decided
 * anyway would decide wrongly. `ready` is false until they are known, and a
 * screen waits for it instead of guessing.
 */
export function usePermissions(): {
    readonly ready: boolean;
    readonly can: (needs: Needs) => boolean;
} {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const codes = new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes));
    const holds = holdsFrom(codes);
    return { ready: !access.isPending, can: (needs) => meets(holds, needs) };
}

/**
 * Whether the signed-in person holds a permission, from the roles they hold.
 * A screen asks this only to decide what it offers; the server checks every
 * call again. Prefer {@link usePermissions} with a declared {@link Needs}.
 */
export function useHolds(): (code: Permission) => boolean {
    return useHeld();
}

/**
 * Like {@link useHolds}, for a caller that holds codes written as data, such as a
 * menu entry, and so cannot name them from {@link Permission}.
 */
export function useHeld(): (code: string) => boolean {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    return holdsFrom(new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes)));
}
